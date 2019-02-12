#
# Employee Attrition Analysis and Recommendations
#
# Assumptions
# Data Set File   - Following files are available in the working directory
#                   general_data.csv
#                   employee_survey_data.csv
#                   manager_survey_data.csv
#                   in_time.csv
#                   out_time.csv
#


#--------------------------------------------------------------------------------------------------#

#
# Load required libraries
#
library(tidyr)
library(lubridate)
library(dplyr)
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(prediction)
library(ROCR)
library(GGally)



#
# Load & view the Data Set
#
general_data          <- read.csv("general_data.csv", stringsAsFactors = T)
survey_data           <- read.csv("employee_survey_data.csv", stringsAsFactors = T, na.strings = NA)
manager_survey_data   <- read.csv("manager_survey_data.csv", stringsAsFactors = T, na.strings = NA)
in_time               <- read.csv("in_time.csv", stringsAsFactors = F)
out_time              <- read.csv("out_time.csv", stringsAsFactors = F)

View(general_data)
View(survey_data)
View(manager_survey_data)
View(in_time)
View(out_time)


#
# Understaning the structure of the data
#
str(general_data)
str(survey_data)
str(manager_survey_data)
str(in_time)
str(out_time)


#--------------------------------------------------------------------------------------------------#

#
# Data Cleaning and Manipulation
#


# Update column names in in_time and out_time to merge with other data frames
colnames(in_time)[colnames(in_time) == "X"] <- "EmployeeID"
colnames(in_time) <- gsub("X", "", colnames(in_time))

colnames(out_time)[colnames(out_time) == "X"] <- "EmployeeID"
colnames(out_time) <- gsub("X","", colnames(out_time))


# check if employeeID is a primary key or not
setdiff(survey_data$EmployeeID,general_data$EmployeeID)
setdiff(survey_data$EmployeeID,manager_survey_data$EmployeeID)
setdiff(survey_data$EmployeeID,in_time$EmployeeID)
setdiff(survey_data$EmployeeID,out_time$EmployeeID)
# Result - 0 -> employeeID is identical across all the data sets


#
# in_time and out_time data manipulation

# Collect in and out time in one data frame
in_time_gathered  <- gather(in_time, key="Date", value="in_time", -EmployeeID, na.rm=T)
out_time_gathered <- gather(out_time, key="Date", value="out_time", -EmployeeID, na.rm=T)
in_out_time       <- merge(in_time_gathered, out_time_gathered, by = c("EmployeeID","Date"))

# Calculate the working hours by each employee
in_out_time$in_time_dt         <- ymd_hms(in_out_time$in_time)
in_out_time$out_time_dt        <- ymd_hms(in_out_time$out_time)
in_out_time$duration_day       <- in_out_time$in_time_dt %--% in_out_time$out_time_dt
in_out_time$duration_day_num   <- as.duration(in_out_time$duration_day)
in_out_time$duration_day_hours <- as.double(in_out_time$duration_day_num / 60 / 60)
str(in_out_time$duration_day_hours)

# Calculate Avg working hours per employee

in_out_means <- in_out_time %>%
  group_by(EmployeeID) %>%
  summarise(StdHours = 8, AvgWorkHrs = mean(duration_day_hours))


#
# Merge all the data frames
emp_data <- merge(general_data, in_out_means, by = "EmployeeID")
emp_data <- merge(emp_data, survey_data, by = "EmployeeID")
emp_data <- merge(emp_data, manager_survey_data, by = "EmployeeID")
str(emp_data)


#
# Data Selection - Remove columns with same value
# Some columns have same value for all records/rows. Therefore nothing can be inferred from these
# columns. Such columns can be removed from final data. 
# Eliminate columns with same value for all records
# Ref - https://stackoverflow.com/questions/30544282/how-to-remove-columns-with-same-value-in-r
emp_data <- emp_data[vapply(emp_data, function(x) length(unique(x)) > 1, logical(1L))]


#
# Convert to correct data type
emp_data$Education <- as.factor(emp_data$Education)
emp_data$JobLevel <- as.factor(emp_data$JobLevel)
emp_data$StockOptionLevel <- as.factor(emp_data$StockOptionLevel)
emp_data$EnvironmentSatisfaction <- as.factor(emp_data$EnvironmentSatisfaction)
emp_data$JobSatisfaction <- as.factor(emp_data$JobSatisfaction)
emp_data$WorkLifeBalance <- as.factor(emp_data$WorkLifeBalance)
emp_data$JobInvolvement <- as.factor(emp_data$JobInvolvement)
emp_data$PerformanceRating <- as.factor(emp_data$PerformanceRating)

emp_data$EmployeeID <- as.numeric(emp_data$EmployeeID)
emp_data$Age <- as.numeric(emp_data$Age)
emp_data$DistanceFromHome <- as.numeric(emp_data$DistanceFromHome)
emp_data$MonthlyIncome <- as.numeric(emp_data$MonthlyIncome)
emp_data$NumCompaniesWorked <- as.numeric(emp_data$NumCompaniesWorked)
emp_data$PercentSalaryHike <- as.numeric(emp_data$PercentSalaryHike)
emp_data$TotalWorkingYears <- as.numeric(emp_data$TotalWorkingYears)
emp_data$TrainingTimesLastYear <- as.numeric(emp_data$TrainingTimesLastYear)
emp_data$YearsAtCompany <- as.numeric(emp_data$YearsAtCompany)
emp_data$YearsSinceLastPromotion <- as.numeric(emp_data$YearsSinceLastPromotion)
emp_data$YearsWithCurrManager <- as.numeric(emp_data$YearsWithCurrManager)


#
# Missing values check
sapply(emp_data, function(x) sum(is.na(x)))

# Percentage of missing values
round(colMeans(is.na(emp_data)),4)

# Since % of missing values is low, we can omit records with NA
emp_data <-na.omit(emp_data)

#
# Check for duplicated data
sum(duplicated(emp_data))

# Removing all data not required to reduced memory consumption
rm(general_data, in_out_means, in_out_time, in_time, in_time_gathered, manager_survey_data)
rm(out_time, out_time_gathered, survey_data)


#--------------------------------------------------------------------------------------------------#


# 
# Univariate Analysis - Analyse how numeric and factor variables affect attrition rate
#


#
# Factor variables vs Attrition

# BusinessTravel
ggplot(emp_data, aes(x=BusinessTravel,fill=Attrition)) + geom_bar(position = position_fill()) + 
        labs(y="Proportion")
# Frequent travel affects attrition 


# Department
ggplot(emp_data, aes(x=Department,fill=Attrition)) + geom_bar(position = position_fill()) + 
        labs(y="Proportion")
# HR dept has a higer attrition rate


# Education
ggplot(emp_data, aes(x=Education,fill=Attrition)) + geom_bar(position = position_fill()) + 
        labs(y="Proportion")
#no major trend uncovered

# EducationField
ggplot(emp_data, aes(x=EducationField,fill=Attrition)) + geom_bar(position = position_fill()) + 
        labs(y="Proportion")
#associates educated in HR seem to have much higher attrition rates

# Gender
ggplot(emp_data, aes(x=Gender,fill=Attrition)) + geom_bar(position = position_fill()) + 
        labs(y="Proportion")
#no major trend uncovered

# JobLevel
ggplot(emp_data, aes(x=JobLevel,fill=Attrition)) + geom_bar(position = position_fill()) + 
        labs(y="Proportion")
#no major trend uncovered

# JobRole
ggplot(emp_data, aes(x=JobRole,fill=Attrition)) + geom_bar(position = position_fill()) + 
        labs(y="Proportion")
#research director role seems to have the highest attrition, followed by research scientists.

# MaritalStatus
ggplot(emp_data, aes(x=MaritalStatus,fill=Attrition)) + geom_bar(position = position_fill()) + 
        labs(y="Proportion")
#single associates indicate higher chances of attrition

# StockOptionLevel
ggplot(emp_data, aes(x=StockOptionLevel,fill=Attrition)) + geom_bar(position = position_fill()) + 
        labs(y="Proportion")
#no major trend uncovered

# EnvironmentSatisfaction
ggplot(emp_data, aes(x=EnvironmentSatisfaction,fill=Attrition)) + 
        geom_bar(position = position_fill()) + labs(y="Proportion")
#Employees with low satisfaction seem to have higher attrition rate

# JobSatisfaction
ggplot(emp_data, aes(x=JobSatisfaction,fill=Attrition)) + geom_bar(position = position_fill()) + 
        labs(y="Proportion")
#Employees with low satisfaction seem to have higher attrition rate

# WorkLifeBalance
ggplot(emp_data, aes(x=WorkLifeBalance,fill=Attrition)) + geom_bar(position = position_fill()) + 
        labs(y="Proportion")
#Employees with low work life balance seem to have highest attrition rate.

# JobInvolvement
ggplot(emp_data, aes(x=JobInvolvement,fill=Attrition)) + geom_bar(position = position_fill()) + 
        labs(y="Proportion")
#both low and very high involvement indicates higher attrition

# PerformanceRating
ggplot(emp_data, aes(x=PerformanceRating,fill=Attrition)) + geom_bar(position = position_fill()) + 
        labs(y="Proportion")
#no major trend uncovered

#
# Numeric variables vs Attrition
# 

# Age
ggplot(emp_data, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot()
# Attrition rate is more in younger employees


# DistanceFromHome
ggplot(emp_data, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot()
# Seems like DistanceFromHome is not affecting Attrition rate


# MonthlyIncome
ggplot(emp_data, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot()
# Lower income does effect attrition


# NumCompaniesWorked
ggplot(emp_data, aes(x=Attrition,y=NumCompaniesWorked, fill=Attrition))+ geom_boxplot()
# Job hoppers are more likely to quit


# PercentSalaryHike
ggplot(emp_data, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot()
# Same pattern for Yes and No. 


# TotalWorkingYears
ggplot(emp_data, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot()
# Less experienced employees are more likely to leave


# TrainingTimesLastYear
ggplot(emp_data, aes(x=Attrition,y=TrainingTimesLastYear, fill=Attrition))+ geom_boxplot()
# Same pattern for Yes and No


# YearsAtCompany
ggplot(emp_data, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot()
# Less years in company affects attrition rate


# YearsSinceLastPromotion
ggplot(emp_data, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot()


# YearsWithCurrManager
ggplot(emp_data, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot()
# Manager affects the attrition rate


# AvgWorkHrs
ggplot(emp_data, aes(x=Attrition,y=AvgWorkHrs, fill=Attrition))+ geom_boxplot()
# More avg hours, more attrition seen


# Above plots gives a picture on which factors are important in predicting attrition rate.
# Factors identified above like Age, Experience, Training, Avg Work hours etc should be part of
# final model we design


#--------------------------------------------------------------------------------------------------#

# 
# Data preparation for modelling
#

# EmployeeID is not required for modelling. Eliminate EmployeeID
emp_data$EmployeeID <- NULL


#
# Split emp_data into numeric and factor variables
# Numeric data will have to be scaled
# Dummy variables will have to be created for factor variables
num_var <- lapply(emp_data, class) == "numeric"
factor_var <- lapply(emp_data, class) == "factor"
emp_data_num <- emp_data[, num_var]
emp_data_factor <- emp_data[, factor_var]

# Scale numeric variables
emp_data_num_stand<- data.frame(sapply(emp_data_num, function(x) scale(x)))

# Create dummy variables for factor variables
dummy_var <- data.frame(sapply(emp_data_factor, 
                      function(x) data.frame(model.matrix(~x-1,data =emp_data_factor))[,-1]))

# Checking attrition rate of employees
sum(dummy_var$Attrition)/nrow(dummy_var)
#16.16%

#
# Create final data for analysis
emp_data_final <- cbind(emp_data_num_stand, dummy_var)

# Deleting unnecessary data
rm(dummy_var, emp_data_factor, emp_data_num, emp_data_num_stand)
rm(factor_var, num_var)


#--------------------------------------------------------------------------------------------------#


# 
# Test and training data set creation
#

set.seed(100)

# Split data train:test @ 70:30 ratio
indices = sample.split(emp_data_final$Attrition, SplitRatio = 0.7)

# Create training data set
train_data = emp_data_final[indices,]

# Create testing data set
test_data = emp_data_final[!(indices),]


#--------------------------------------------------------------------------------------------------#

# 
# Model building - Logistic Regression
#


#
# Initial model - model_01
model_01 = glm(Attrition ~ ., data = train_data, family = "binomial")
summary(model_01)


#
# Run stepAIC
model_02<- stepAIC(model_01, direction="both")
summary(model_02)
vif(model_02)


#
# Removing multicollinearity through VIF and p value check
#

# Based on model_02
# Remove EducationField.xLife.Sciences
model_03 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Education.x5 + EducationField.xMarketing + 
                  EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                data = train_data)

summary(model_03)
vif(model_03)


# Remove EducationField.xMarketing
model_04 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Education.x5 + EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
              data = train_data)
summary(model_04)
vif(model_04)


# Remove EducationField.xMedical
model_05 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Education.x5 + EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                data = train_data)
summary(model_05)
vif(model_05)


# Remove EducationField.xOther
model_06 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Education.x5 + EducationField.xTechnical.Degree + 
                  JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                data = train_data)
summary(model_06)
vif(model_06)


# Remove EducationField.xTechnical.Degree
model_07 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Education.x5 + JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                data = train_data)
summary(model_07)
vif(model_07)


# Remove JobLevel.x5
model_08 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Education.x5 + JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                data = train_data)
summary(model_08)
vif(model_08)


# Remove MaritalStatus.xMarried
model_09 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Education.x5 + JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                data = train_data)
summary(model_09)
vif(model_09)


# Remove Education.x5
model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                data = train_data)
summary(model_10)
vif(model_10)


# Remove JobRole.xHuman.Resources
model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobLevel.x2 + JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                data = train_data)
summary(model_11)
vif(model_11)


# Remove StockOptionLevel.x1
model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobLevel.x2 + JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                data = train_data)
summary(model_12)
vif(model_12)


# Remove JobRole.xManager
model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                data = train_data)
summary(model_13)
vif(model_13)


# Remove JobLevel.x2
model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                data = train_data)
summary(model_14)
vif(model_14)


# Remove JobInvolvement.x3
model_15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4, family = "binomial", 
                data = train_data)
summary(model_15)
vif(model_15)


# Remove JobRole.xSales.Executive
model_16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4, family = "binomial", 
                data = train_data) 
summary(model_16)
vif(model_16)


# Remove JobRole.xResearch.Director
model_17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4, family = "binomial", 
                data = train_data)  
summary(model_17)
vif(model_17)

# Remove BusinessTravel.xTravel_Rarely
model_18 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkHrs + BusinessTravel.xTravel_Frequently + 
                  JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4, family = "binomial", 
                data = train_data) 
summary(model_18)
vif(model_18)


# 
# In model_18 all the variables are found to be significant. 
# Hence this will be considered as the final model
final_model <- model_18


#--------------------------------------------------------------------------------------------------#


# 
# Model evaluation
#

test_pred = predict(final_model, type = "response", newdata = test_data)
summary(test_pred)

# With 0.5 cut off value, test the model
test_pred_attrition <- factor(ifelse(test_pred >= 0.5, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test_data$Attrition==1,"Yes","No"))

test_conf <- caret::confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

#
# Results
#
# Confusion Matrix and Statistics
#
# Reference
# Prediction   No  Yes
# No  1044  147
# Yes   37   62
# 
# Accuracy : 0.8574         
# 95% CI : (0.8371, 0.876)
# No Information Rate : 0.838          
# P-Value [Acc > NIR] : 0.03051        
# 
# Kappa : 0.3331         
# Mcnemar's Test P-Value : 9.313e-16      
# 
# Sensitivity : 0.29665        
# Specificity : 0.96577        
# Pos Pred Value : 0.62626        
# Neg Pred Value : 0.87657        
# Prevalence : 0.16202        
# Detection Rate : 0.04806        
# Detection Prevalence : 0.07674        
# Balanced Accuracy : 0.63121        
# 
# 'Positive' Class : Yes    

# From on above results, Accuracy and Specificity is good for chosen cutoff values of 0.5
# But Sensitivity is low (0.29665)
# Hence we need to find an optimal cut off value which will give good and acceptable
# accuracy, sensitivity and specificity values


#Finding optimal cut_off value
#function to find optimal cut_off value
cutoff_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- caret::confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(cutoff,sens, spec, acc))) 
  colnames(out) <- c("cutoff","sensitivity", "specificity", "accuracy")
  return(out)
}
cutoff_data = seq(.1,.8,length=700)
cutoff_matrix = matrix(0,700,4)
colnames(cutoff_matrix)<- c("cutoff","sensitivity", "specificity", "accuracy")
for(i in 1:700){
  cutoff_matrix[i,] = round(cutoff_fn(cutoff_data[i]),3)
} 
cutoff_matrix_df<- as.data.frame(cutoff_matrix)
View(cutoff_matrix_df)

#plot of cut_off value , specificity,sensitivity and accuracy

ggplot(cutoff_matrix_df, aes(cutoff)) +  geom_line(aes(y = specificity,color="specificity")) +
  geom_line(aes(y = sensitivity,color="sensitivity")) + geom_line(aes(y = accuracy,color="accuracy")) + 
  labs(x="cutoff", y="values")

#Based on the cutoff_matrix, we find 0.184 as the optimal cutoff value for the matrix.


# With 0.184 cut off value
test_pred_attrition <- factor(ifelse(test_pred >= 0.184, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test_data$Attrition==1,"Yes","No"))

test_conf <- caret::confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

# Confusion Matrix and Statistics
# 
# Reference
# Prediction  No Yes
# No  827  49
# Yes 254 160
# 
# Accuracy : 0.7651        
# 95% CI : (0.741, 0.788)
# No Information Rate : 0.838         
# P-Value [Acc > NIR] : 1             
# 
# Kappa : 0.3802        
# Mcnemar's Test P-Value : <2e-16        
# 
# Sensitivity : 0.7656        
# Specificity : 0.7650        
# Pos Pred Value : 0.3865        
# Neg Pred Value : 0.9441        
# Prevalence : 0.1620        
# Detection Rate : 0.1240        
# Detection Prevalence : 0.3209        
# Balanced Accuracy : 0.7653        
# 
# 'Positive' Class : Yes     



## final values

#accuracy: 77%
#sensitivity: 77%
#specificity: 77%


# KS -statistic - Test Data
test_pred_attrition <- ifelse(test_pred_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

#on testing  data
pred_object_test<- prediction(test_pred_attrition, test_actual_attrition)
performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
#KS statistic = 53.1%

# Lift & Gain chart 

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)

View(Attrition_decile)

plot_grid(ggplot(Attrition_decile,aes(x=Attrition_decile$bucket,y=Attrition_decile$Gain, color=""))+geom_line()+geom_point(),
          ggplot(Attrition_decile,aes(x=Attrition_decile$bucket,y=Attrition_decile$Cumlift))+geom_line()+geom_point(), 
          align = "h",ncol = 1)


if(!require("InformationValue",character.only = TRUE))
{ 
  install.packages("InformationValue")
}

library(InformationValue)
ks_plot(test_actual_attrition, test_pred_attrition) # Gain chart plot




