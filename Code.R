# Project HR
# Anastasiia Sviridova 466520, Jaume Fuente Ponce k-15773

setwd("C:/Users/User/Documents/2 semester/Adv Econometrics/Project")
getwd()
Sys.setenv(LANG = "en")
options(scipen = 5)

# Libraries and Packages

install.packages("pscl")
install.packages("ucminf")
install.packages("ordinal")
install.packages("reshape")
install.packages("generalhoslem")
install.packages("oglmx")
install.packages("brant")
install.packages("aod")
install.packages("dplyr")
install.packages("tidyr")
install.packages("broom.mixed")
install.packages("MASS")
install.packages("devtools")
install.packages("rms")
install.packages("statmod")
install.packages("speedglm")
install.packages("data.table")
install.packages("pROC")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("car")
install.packages("caret")
install.packages("stargazer")
install.packages("gridExtra")
install.packages("lmtest")

library("sandwich")
library("zoo")
library("lmtest")
library("MASS")
library("pscl")
library("LogisticDx")
library("ucminf")
library("ordinal")
library("reshape")
library("generalhoslem")
library("oglmx")
library("aod")
library("brant")
library("dplyr")
library("tidyr")
library("broom.mixed")
library("devtools")
library("caret")
library("ggplot2")
library("corrplot")
library("MASS")
library("car")
library("stargazer") 
library("gridExtra")
library("lmtest")

# a) Estimation ordered probit and ordered logit, selection of the covariates

hr = read.csv2(file="_HR_.csv", header=TRUE, sep=",")
View(hr)

str(hr)
summary(hr) # no missing values

# Remove the variables from the dataset
hr <- hr %>% select(-Over18)
hr <- hr %>% select(-EmployeeCount)
hr <- hr %>% select(-StandardHours)
hr <- hr %>% select(-EmployeeNumber)
hr <- na.omit(hr)
str(hr)

# Plot Over18
plot_over18 <- ggplot(hr, aes(x = factor(Over18))) +
  geom_bar() +
  labs(title = "Distribution", x = "Over18", y = "Count")

# Plot EmployeeCount
plot_employee_count <- ggplot(hr, aes(x = factor(EmployeeCount))) +
  geom_bar() +
  labs(x = "EmployeeCount", y = "Count")

# Plot StandardHours
plot_standard_hours <- ggplot(hr, aes(x = factor(StandardHours))) +
  geom_bar() +
  labs(x = "StandardHours", y = "Count")

# Arrange the three plots in one space
grid.arrange(plot_over18, plot_employee_count, plot_standard_hours, ncol = 1)

# Encoding the categorical variables

hr$Gender[hr$Gender=="Female"] = 1
hr$Gender[hr$Gender=="Male"] = 0

hr$Female = 0
hr$Female[hr$Gender=="1"] = 1
hr$Male = 0
hr$Male[hr$Gender=="0"] = 1

hr$Attrition[hr$Attrition=="Yes"] = 1
hr$Attrition[hr$Attrition=="No"] = 0

hr$OverTime[hr$OverTime=="Yes"] = 1
hr$OverTime[hr$OverTime=="No"] = 0

hr$BusinessTravel[hr$BusinessTravel=="Non-Travel"] = 0
hr$BusinessTravel[hr$BusinessTravel=="Travel_Rarely"] = 1
hr$BusinessTravel[hr$BusinessTravel=="Travel_Frequently"] = 2

hr$NonTravel = 0
hr$NonTravel[hr$BusinessTravel=="0"] = 1
hr$Travel_Rarely = 0
hr$Travel_Rarely[hr$BusinessTravel=="1"] = 1
hr$Travel_Frequently = 0
hr$Travel_Frequently[hr$BusinessTravel=="2"] = 1

# Department
hr$Department_HumanResources = 0
hr$Department_HumanResources[hr$Department=="Human Resources"] = 1
hr$Department_ResearcsDevelopment = 0
hr$Department_ResearcsDevelopment[hr$Department=="Research & Development"] = 1
hr$Department_Sales = 0
hr$Department_Sales[hr$Department=="Sales"] = 1

# JobRole
hr$JobRole_HealthcareRep = 0
hr$JobRole_HealthcareRep[hr$JobRole=="Healthcare Representative"] = 1
hr$JobRole_HumanResources = 0
hr$JobRole_HumanResources[hr$JobRole=="Human Resources"] = 1
hr$JobRole_LaboratoryTech = 0
hr$JobRole_LaboratoryTech[hr$JobRole=="Laboratory Technician"] = 1
hr$JobRole_Manager = 0
hr$JobRole_Manager[hr$JobRole=="Manager"] = 1
hr$JobRole_ManufacturingDir = 0
hr$JobRole_ManufacturingDir[hr$JobRole=="Manufacturing Director"] = 1
hr$JobRole_ResearchDir = 0
hr$JobRole_ResearchDir[hr$JobRole=="Research Director"] = 1
hr$JobRole_ResearchSci = 0
hr$JobRole_ResearchSci[hr$JobRole=="Research Scientist"] = 1
hr$JobRole_SalesExec = 0
hr$JobRole_SalesExec[hr$JobRole=="Sales Executive"] = 1
hr$JobRole_SalesRep = 0
hr$JobRole_SalesRep[hr$JobRole=="Sales Representative"] = 1

# MaritalStatus 
hr$MaritalStatus_Divorced = 0
hr$MaritalStatus_Divorced[hr$MaritalStatus=="Divorced"] = 1
hr$MaritalStatus_Married = 0
hr$MaritalStatus_Married[hr$MaritalStatus=="Married"] = 1
hr$MaritalStatus_Single = 0
hr$MaritalStatus_Single[hr$MaritalStatus=="Single"] = 1

# Preparation variables

hr <- hr %>%
  mutate(across(where(is.character), as.factor))

hr$JobSatisfaction <- factor(hr$JobSatisfaction, ordered = TRUE)

hr$Education <- as.numeric(hr$Education)
hr$EnvironmentSatisfaction <- as.numeric(hr$EnvironmentSatisfaction)
hr$JobInvolvement <- as.numeric(hr$JobInvolvement)
hr$JobLevel <- as.numeric(hr$JobLevel)
hr$PerformanceRating <- as.numeric(hr$PerformanceRating)
hr$RelationshipSatisfaction <- as.numeric(hr$RelationshipSatisfaction)
hr$StockOptionLevel <- as.numeric(hr$StockOptionLevel)
hr$TrainingTimesLastYear <- as.numeric(hr$TrainingTimesLastYear)
hr$WorkLifeBalance <- as.numeric(hr$WorkLifeBalance)

# Standardizing function
standardize <- function(x) {
  return ((x - mean(x)) / sd(x))
}

# Apply standardization to numerical variables
hr$Age <- standardize(hr$Age)
hr$MonthlyIncome <- standardize(hr$MonthlyIncome)
hr$MonthlyRate <- standardize(hr$MonthlyRate)
hr$DailyRate <- standardize(hr$DailyRate)
hr$HourlyRate <- standardize(hr$HourlyRate)

hr$DistanceFromHome <- standardize(hr$DistanceFromHome)
hr$NumCompaniesWorked <- standardize(hr$NumCompaniesWorked)
hr$PercentSalaryHike <- standardize(hr$PercentSalaryHike)
hr$TotalWorkingYears <- standardize(hr$TotalWorkingYears)

hr$YearsAtCompany <- standardize(hr$YearsAtCompany)
hr$YearsInCurrentRole <- standardize(hr$YearsInCurrentRole)
hr$YearsSinceLastPromotion <- standardize(hr$YearsSinceLastPromotion)
hr$YearsWithCurrManager <- standardize(hr$YearsWithCurrManager)

hr$Education <- standardize(hr$Education)
hr$EnvironmentSatisfaction <- standardize(hr$EnvironmentSatisfaction)
hr$JobInvolvement  <- standardize(hr$JobInvolvement )
hr$JobLevel  <- standardize(hr$JobLevel)
hr$PerformanceRating  <- standardize(hr$PerformanceRating)
hr$RelationshipSatisfaction <- standardize(hr$RelationshipSatisfaction)
hr$StockOptionLevel  <- standardize(hr$StockOptionLevel)
hr$TrainingTimesLastYear  <- standardize(hr$TrainingTimesLastYear)
hr$WorkLifeBalance  <- standardize(hr$WorkLifeBalance)

# EDA part

# Dependent variable - Job satisfaction
# Display the count of observations for each unique value of JobSatisfaction
observations_per_value <- table(hr$JobSatisfaction)
print(observations_per_value)

# Create a bar chart for JobSatisfaction
ggplot(data = hr, aes(x = as.factor(JobSatisfaction))) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Job Satisfaction Distribution",
       x = "Job Satisfaction",
       y = "Frequency") +
  theme_minimal()

# Create a boxplot for JobSatisfaction
ggplot(data = hr, aes(x = "", y = JobSatisfaction)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot of Job Satisfaction",
       x = "",
       y = "Job Satisfaction") +
  theme_minimal()



#independent variables (significants)

# Create a bar plot for Attrition
ggplot(hr, aes(x = as.factor(Attrition))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Histogram of Attrition",
       x = "Attrition",
       y = "Count") +
  theme_minimal() +
  scale_x_discrete(labels = c("No", "Yes"))


# Create a density plot for HourlyRate
ggplot(hr, aes(x = HourlyRate)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Density Plot of Hourly Rate",
       x = "Hourly Rate",
       y = "Density") +
  theme_minimal()


# Create a histogram for NumCompaniesWorked
ggplot(hr, aes(x = NumCompaniesWorked)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 20, stat = "count") +
  labs(title = "Histogram of NumCompaniesWorked",
       x = "Number of Companies Worked",
       y = "Count") +
  theme_minimal()


# Create a bar plot for Marital_Status_single
ggplot(hr, aes(x = as.factor(MaritalStatus))) +
  geom_bar(fill = "green", color = "black") +
  labs(title = "Bar Plot of Marital Status",
       x = "MaritalStatus",
       y = "Count"
  ) +
  theme_minimal()


# Create a pie chart for OverTime
ggplot(hr, aes(x = "", fill = OverTime)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Pie Chart of OverTime",
       fill = "OverTime") +
  theme_void() +
  theme(legend.position = "right")


# Create a density plot for YearsWithCurrManager
ggplot(hr, aes(x = YearsWithCurrManager)) +
  geom_density(fill = "orange", color = "black") +
  labs(title = "Density Plot of Years With Current Manager",
       x = "Years With Current Manager",
       y = "Density") +
  theme_minimal()


# Fit the ordered logit model
ologit_model <- polr(as.factor(JobSatisfaction) ~ Age + as.factor(Attrition) + as.factor(BusinessTravel) +
                       DailyRate + as.factor(Department) + DistanceFromHome + Education +
                      EnvironmentSatisfaction + as.factor(Gender) + HourlyRate +
                       JobInvolvement + JobLevel + as.factor(JobRole) +
                       as.factor(MaritalStatus) + MonthlyIncome + MonthlyRate + NumCompaniesWorked +
                       as.factor(OverTime) + PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                       WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion +
                       YearsWithCurrManager, data = hr, method = "logistic")

# Summary of the ordered logit model
summary(ologit_model)  #AIC: 4033.301
coeftest(ologit_model)

# Fit the ordered probit model
oprobit_model <- polr(as.factor(JobSatisfaction) ~ Age + as.factor(Attrition) + as.factor(BusinessTravel) +
                        DailyRate + as.factor(Department) + DistanceFromHome + Education +
                         EnvironmentSatisfaction + as.factor(Gender) + HourlyRate +
                        JobInvolvement + JobLevel + as.factor(JobRole) +
                        as.factor(MaritalStatus) + MonthlyIncome + MonthlyRate + NumCompaniesWorked +
                        as.factor(OverTime) + PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                        WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion +
                        YearsWithCurrManager, data = hr, method = "probit")

# Summary of the ordered logit model
summary(oprobit_model)  #AIC: 4033.415
coeftest(oprobit_model)


#b) let's do the general to specific approach

#general model
ologit1 = ologit.reg(as.numeric(JobSatisfaction)~Age+as.factor(Attrition)+as.factor(BusinessTravel)+
                       DailyRate + as.factor(Department) + DistanceFromHome + Education+
                        EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                       JobInvolvement+ JobLevel+ as.factor(JobRole)+
                       as.factor(MaritalStatus)+ MonthlyIncome+ MonthlyRate+ NumCompaniesWorked+
                       as.factor(OverTime)+ PercentSalaryHike+ PerformanceRating+ RelationshipSatisfaction+
                       StockOptionLevel+ TotalWorkingYears+ TrainingTimesLastYear+
                       WorkLifeBalance+ YearsAtCompany+ YearsInCurrentRole+ YearsSinceLastPromotion+
                       YearsWithCurrManager, data=hr)
summary(ologit1)

# test whether all insignificant variables all jointly insignificant
ologit1_r = ologit.reg(as.numeric(JobSatisfaction)~as.factor(Attrition)+ HourlyRate+
                         MaritalStatus_Single+as.factor(OverTime), data=hr)

summary(ologit1_r)

lrtest(ologit1, ologit1_r)
# all insignificant variables are jointly insignificant

#let's drop "the most insignificant" variable from ologit1 - MonthlyRate

#Step 2
#let's do the estimation without the variables dropped

ologit2 = ologit.reg(as.numeric(JobSatisfaction)~Age+as.factor(Attrition)+as.factor(BusinessTravel)+
                       DailyRate + as.factor(Department) + DistanceFromHome + Education+
                        EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                       JobInvolvement+ JobLevel+ as.factor(JobRole)+
                       as.factor(MaritalStatus)+ MonthlyIncome+ NumCompaniesWorked+
                       as.factor(OverTime)+ PercentSalaryHike+ PerformanceRating+ RelationshipSatisfaction+
                       StockOptionLevel+ TotalWorkingYears+ TrainingTimesLastYear+
                       WorkLifeBalance+ YearsAtCompany+ YearsInCurrentRole+ YearsSinceLastPromotion+
                       YearsWithCurrManager, data=hr)
summary(ologit2)

#let's drop "the most insignificant" variable from ologit2- as.factor(Department)Sales

#Step 3
# let's do the estimation without the variables dropped

ologit3 = ologit.reg(as.numeric(JobSatisfaction)~Age+as.factor(Attrition)+as.factor(BusinessTravel)+
                       DailyRate + Department_ResearcsDevelopment+Department_HumanResources + DistanceFromHome + Education+
                        EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                       JobInvolvement+ JobLevel+ as.factor(JobRole)+
                       as.factor(MaritalStatus)+ MonthlyIncome+ NumCompaniesWorked+
                       as.factor(OverTime)+ PercentSalaryHike+ PerformanceRating+ RelationshipSatisfaction+
                       StockOptionLevel+ TotalWorkingYears+ TrainingTimesLastYear+
                       WorkLifeBalance+ YearsAtCompany+ YearsInCurrentRole+ YearsSinceLastPromotion+
                       YearsWithCurrManager, data=hr)
summary(ologit3)

# and test joint hypothesis: beta_MonthlyRate=beta_(Department)Sales=0
lrtest(ologit1, ologit3)
# we cannot reject the null that
# beta_MonthlyRate=beta_(Department)Sales=0, so (Department)Sales might be dropped from model

#let's drop "the most insignificant" variable from ologit3- Department_HumanResources


#Step 4
#let's do the estimation without the variables dropped

ologit4 = ologit.reg(as.numeric(JobSatisfaction)~Age+as.factor(Attrition)+as.factor(BusinessTravel)+
                       DailyRate + Department_ResearcsDevelopment + DistanceFromHome + Education+
                        EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                       JobInvolvement+ JobLevel+ as.factor(JobRole)+
                       as.factor(MaritalStatus)+ MonthlyIncome+ NumCompaniesWorked+
                       as.factor(OverTime)+ PercentSalaryHike+ PerformanceRating+ RelationshipSatisfaction+
                       StockOptionLevel+ TotalWorkingYears+ TrainingTimesLastYear+
                       WorkLifeBalance+ YearsAtCompany+ YearsInCurrentRole+ YearsSinceLastPromotion+
                       YearsWithCurrManager, data=hr)
summary(ologit4)

# and test joint hypothesis: beta_MonthlyRate=beta_(Department)Sales=Department_HumanResources=0
lrtest(ologit1, ologit4)
# we cannot reject the null that
# beta_MonthlyRate=beta_(Department)Sales=Department_HumanResources=0, so Department_HumanResources might be dropped from model

#let's drop "the most insignificant" variable from ologit4- as.factor(JobRole)Research Scientist


#Step 5
#let's do the estimation without the variables dropped

ologit5 = ologit.reg(as.numeric(JobSatisfaction)~Age+as.factor(Attrition)+as.factor(BusinessTravel)+
                       DailyRate + Department_ResearcsDevelopment + DistanceFromHome + Education+
                       EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                       JobInvolvement+ JobLevel+ JobRole_HealthcareRep+JobRole_HumanResources+
                       JobRole_LaboratoryTech+ JobRole_Manager+ JobRole_ManufacturingDir+ 
                       JobRole_ResearchDir+ JobRole_SalesExec+ JobRole_SalesRep+
                       as.factor(MaritalStatus)+ MonthlyIncome+ NumCompaniesWorked+
                       as.factor(OverTime)+ PercentSalaryHike+ PerformanceRating+ RelationshipSatisfaction+
                       StockOptionLevel+ TotalWorkingYears+ TrainingTimesLastYear+
                       WorkLifeBalance+ YearsAtCompany+ YearsInCurrentRole+ YearsSinceLastPromotion+
                       YearsWithCurrManager, data=hr)
summary(ologit5)

# and test joint hypothesis: beta_MonthlyRate=beta_(Department)Sales=Department_HumanResources
# =(JobRole)Research Scientist=0
lrtest(ologit1, ologit5)
# we cannot reject the null that
# beta_MonthlyRate=beta_(Department)Sales=Department_HumanResources
#=(JobRole)Research Scientist=0, so (JobRole)Research Scientist might be dropped from model

#let's drop "the most insignificant" variable from ologit5- JobRole_Manager

#Step 6
#let's do the estimation without the variables dropped

ologit6 = ologit.reg(as.numeric(JobSatisfaction)~Age+as.factor(Attrition)+as.factor(BusinessTravel)+
                       DailyRate + Department_ResearcsDevelopment + DistanceFromHome + Education+
                       EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                       JobInvolvement+ JobLevel+ JobRole_HealthcareRep+JobRole_HumanResources+
                       JobRole_LaboratoryTech+ JobRole_ManufacturingDir+ 
                       JobRole_ResearchDir+ JobRole_SalesExec+ JobRole_SalesRep+
                       as.factor(MaritalStatus)+ MonthlyIncome+ NumCompaniesWorked+
                       as.factor(OverTime)+ PercentSalaryHike+ PerformanceRating+ RelationshipSatisfaction+
                       StockOptionLevel+ TotalWorkingYears+ TrainingTimesLastYear+
                       WorkLifeBalance+ YearsAtCompany+ YearsInCurrentRole+ YearsSinceLastPromotion+
                       YearsWithCurrManager, data=hr)
summary(ologit6)

lrtest(ologit1, ologit6)

#let's drop "the most insignificant" variable from ologit6- JobRole_ResearchDir


#Step 7
#Let's do the estimation without the variables droped

ologit7 = ologit.reg(as.numeric(JobSatisfaction)~Age+as.factor(Attrition)+as.factor(BusinessTravel)+
                       DailyRate + Department_ResearcsDevelopment + DistanceFromHome + Education+
                       EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                       JobInvolvement+ JobLevel+ JobRole_HealthcareRep+JobRole_HumanResources+
                       JobRole_LaboratoryTech+ JobRole_ManufacturingDir+ 
                        JobRole_SalesExec+ JobRole_SalesRep+
                       as.factor(MaritalStatus)+ MonthlyIncome+ NumCompaniesWorked+
                       as.factor(OverTime)+ PercentSalaryHike+ PerformanceRating+ RelationshipSatisfaction+
                       StockOptionLevel+ TotalWorkingYears+ TrainingTimesLastYear+
                       WorkLifeBalance+ YearsAtCompany+ YearsInCurrentRole+ YearsSinceLastPromotion+
                       YearsWithCurrManager, data=hr)
summary(ologit7)

lrtest(ologit1, ologit7)

#let's drop "the most insignificant" variable from ologit7 -YearsSinceLastPromotion


#Step 8
#Let's do the estimation without the variables droped

ologit8 = ologit.reg(as.numeric(JobSatisfaction)~Age+as.factor(Attrition)+as.factor(BusinessTravel)+
                       DailyRate + Department_ResearcsDevelopment + DistanceFromHome + Education+
                       EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                       JobInvolvement+ JobLevel+ JobRole_HealthcareRep+JobRole_HumanResources+
                       JobRole_LaboratoryTech+ JobRole_ManufacturingDir+ 
                       JobRole_SalesExec+ JobRole_SalesRep+
                       as.factor(MaritalStatus)+ MonthlyIncome+ NumCompaniesWorked+
                       as.factor(OverTime)+ PercentSalaryHike+ PerformanceRating+ RelationshipSatisfaction+
                       StockOptionLevel+ TotalWorkingYears+ TrainingTimesLastYear+
                       WorkLifeBalance+ YearsAtCompany+ YearsInCurrentRole+
                       YearsWithCurrManager, data=hr)
summary(ologit8)

lrtest(ologit1, ologit8)

#let's drop "the most insignificant" variable from ologit8 - JobRole_HealthcareRep


#Step 9
#Let's do the estimation without the variables droped

ologit9 = ologit.reg(as.numeric(JobSatisfaction)~Age+as.factor(Attrition)+as.factor(BusinessTravel)+
                       DailyRate + Department_ResearcsDevelopment + DistanceFromHome + Education+
                       EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                       JobInvolvement+ JobLevel+JobRole_HumanResources+
                       JobRole_LaboratoryTech+ JobRole_ManufacturingDir+ 
                       JobRole_SalesExec+ JobRole_SalesRep+
                       as.factor(MaritalStatus)+ MonthlyIncome+ NumCompaniesWorked+
                       as.factor(OverTime)+ PercentSalaryHike+ PerformanceRating+ RelationshipSatisfaction+
                       StockOptionLevel+ TotalWorkingYears+ TrainingTimesLastYear+
                       WorkLifeBalance+ YearsAtCompany+ YearsInCurrentRole+
                       YearsWithCurrManager, data=hr)
summary(ologit9)

lrtest(ologit1, ologit9)
#let's drop "the most insignificant" variable from ologit9 - as.factor(BusinessTravel)1



#Step 10
#Let's do the estimation without the variables droped

ologit10 = ologit.reg(as.numeric(JobSatisfaction)~Age+as.factor(Attrition)+Travel_Frequently
                      + NonTravel+
                        DailyRate + Department_ResearcsDevelopment + DistanceFromHome + Education+
                        EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                        JobInvolvement+ JobLevel+JobRole_HumanResources+
                        JobRole_LaboratoryTech+ JobRole_ManufacturingDir+ 
                        JobRole_SalesExec+ JobRole_SalesRep+
                        as.factor(MaritalStatus)+ MonthlyIncome+ NumCompaniesWorked+
                        as.factor(OverTime)+ PercentSalaryHike+ PerformanceRating+ RelationshipSatisfaction+
                        StockOptionLevel+ TotalWorkingYears+ TrainingTimesLastYear+
                        WorkLifeBalance+ YearsAtCompany+ YearsInCurrentRole+
                        YearsWithCurrManager, data=hr)
summary(ologit10)

lrtest(ologit1, ologit10)
#let's drop "the most insignificant" variable from ologit10- NonTravel

str(hr)
#Step 11
#Let's do the estimation without the variables droped

ologit11 = ologit.reg(as.numeric(JobSatisfaction)~Age+as.factor(Attrition)+Travel_Frequently
                      + DailyRate + Department_ResearcsDevelopment + DistanceFromHome + Education+
                        EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                        JobInvolvement+ JobLevel+JobRole_HumanResources+
                        JobRole_LaboratoryTech+ JobRole_ManufacturingDir+ 
                        JobRole_SalesExec+ JobRole_SalesRep+
                        as.factor(MaritalStatus)+ MonthlyIncome+ NumCompaniesWorked+
                        as.factor(OverTime)+ PercentSalaryHike+ PerformanceRating+ RelationshipSatisfaction+
                        StockOptionLevel+ TotalWorkingYears+ TrainingTimesLastYear+
                        WorkLifeBalance+ YearsAtCompany+ YearsInCurrentRole+
                        YearsWithCurrManager, data=hr)
summary(ologit11)

lrtest(ologit1, ologit11)
#let's drop "the most insignificant" variable from ologit11 - DistanceFromHome


#Step 12
#Let's do the estimation without the variables droped

ologit12 = ologit.reg(as.numeric(JobSatisfaction)~Age+as.factor(Attrition)+Travel_Frequently
                      + DailyRate + Department_ResearcsDevelopment + Education+
                        EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                        JobInvolvement+ JobLevel+JobRole_HumanResources+
                        JobRole_LaboratoryTech+ JobRole_ManufacturingDir+ 
                        JobRole_SalesExec+ JobRole_SalesRep+
                        as.factor(MaritalStatus)+ MonthlyIncome+ NumCompaniesWorked+
                        as.factor(OverTime)+ PercentSalaryHike+ PerformanceRating+ RelationshipSatisfaction+
                        StockOptionLevel+ TotalWorkingYears+ TrainingTimesLastYear+
                        WorkLifeBalance+ YearsAtCompany+ YearsInCurrentRole+
                        YearsWithCurrManager, data=hr)
summary(ologit12)

lrtest(ologit1, ologit12)
#let's drop "the most insignificant" variable from ologit12- Education


#Step 13
#Let's do the estimation without the variables droped

ologit13 = ologit.reg(as.numeric(JobSatisfaction)~Age+as.factor(Attrition)+Travel_Frequently
                      + DailyRate + Department_ResearcsDevelopment +
                        EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                        JobInvolvement+ JobLevel+JobRole_HumanResources+
                        JobRole_LaboratoryTech+ JobRole_ManufacturingDir+ 
                        JobRole_SalesExec+ JobRole_SalesRep+
                        as.factor(MaritalStatus)+ MonthlyIncome+ NumCompaniesWorked+
                        as.factor(OverTime)+ PercentSalaryHike+ PerformanceRating+ RelationshipSatisfaction+
                        StockOptionLevel+ TotalWorkingYears+ TrainingTimesLastYear+
                        WorkLifeBalance+ YearsAtCompany+ YearsInCurrentRole+
                        YearsWithCurrManager, data=hr)
summary(ologit13)

lrtest(ologit1, ologit13)

#let's drop "the most insignificant" variable from ologit13- JobRole_LaboratoryTech



#Step 14
#Let's do the estimation without the variables droped

ologit14 = ologit.reg(as.numeric(JobSatisfaction)~Age+as.factor(Attrition)+Travel_Frequently
                      + DailyRate + Department_ResearcsDevelopment +
                        EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                        JobInvolvement+ JobLevel+JobRole_HumanResources+
                         JobRole_ManufacturingDir+ 
                        JobRole_SalesExec+ JobRole_SalesRep+
                        as.factor(MaritalStatus)+ MonthlyIncome+ NumCompaniesWorked+
                        as.factor(OverTime)+ PercentSalaryHike+ PerformanceRating+ RelationshipSatisfaction+
                        StockOptionLevel+ TotalWorkingYears+ TrainingTimesLastYear+
                        WorkLifeBalance+ YearsAtCompany+ YearsInCurrentRole+
                        YearsWithCurrManager, data=hr)
summary(ologit14)

lrtest(ologit1, ologit14)
#let's drop "the most insignificant" variable from ologit14- JobRole_SalesExec


#Step 15
#Let's do the estimation without the variables droped

ologit15 = ologit.reg(as.numeric(JobSatisfaction)~Age+as.factor(Attrition)+Travel_Frequently
                      + DailyRate + Department_ResearcsDevelopment +
                        EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                        JobInvolvement+ JobLevel+JobRole_HumanResources+
                        JobRole_ManufacturingDir+ 
                        JobRole_SalesRep+
                        as.factor(MaritalStatus)+ MonthlyIncome+ NumCompaniesWorked+
                        as.factor(OverTime)+ PercentSalaryHike+ PerformanceRating+ RelationshipSatisfaction+
                        StockOptionLevel+ TotalWorkingYears+ TrainingTimesLastYear+
                        WorkLifeBalance+ YearsAtCompany+ YearsInCurrentRole+
                        YearsWithCurrManager, data=hr)
summary(ologit15)

lrtest(ologit1, ologit15)
#let's drop "the most insignificant" variable from ologit15- MonthlyIncome


#Step 16
#Let's do the estimation without the variables droped

ologit16 = ologit.reg(as.numeric(JobSatisfaction)~Age+as.factor(Attrition)+Travel_Frequently
                      + DailyRate + Department_ResearcsDevelopment +
                        EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                        JobInvolvement+ JobLevel+JobRole_HumanResources+
                        JobRole_ManufacturingDir+ 
                        JobRole_SalesRep+
                        as.factor(MaritalStatus)+ NumCompaniesWorked+
                        as.factor(OverTime)+ PercentSalaryHike+ PerformanceRating+ RelationshipSatisfaction+
                        StockOptionLevel+ TotalWorkingYears+ TrainingTimesLastYear+
                        WorkLifeBalance+ YearsAtCompany+ YearsInCurrentRole+
                        YearsWithCurrManager, data=hr)
summary(ologit16)

lrtest(ologit1, ologit16)
#let's drop "the most insignificant" variable from ologit16- JobLevel


# step 17
#Let's do the estimation without the variables droped

ologit17 = ologit.reg(as.numeric(JobSatisfaction)~Age+as.factor(Attrition)+Travel_Frequently
                      + DailyRate + Department_ResearcsDevelopment +
                        EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                        JobInvolvement+JobRole_HumanResources+
                        JobRole_ManufacturingDir+ 
                        JobRole_SalesRep+
                        as.factor(MaritalStatus)+ NumCompaniesWorked+
                        as.factor(OverTime)+ PercentSalaryHike+ PerformanceRating+ RelationshipSatisfaction+
                        StockOptionLevel+ TotalWorkingYears+ TrainingTimesLastYear+
                        WorkLifeBalance+ YearsAtCompany+ YearsInCurrentRole+
                        YearsWithCurrManager, data=hr)
summary(ologit17)

lrtest(ologit1, ologit17)
#let's drop "the most insignificant" variable from ologit17- YearsInCurrentRole



# step 18
#Let's do the estimation without the variables droped

ologit18 = ologit.reg(as.numeric(JobSatisfaction)~Age+as.factor(Attrition)+Travel_Frequently
                      + DailyRate + Department_ResearcsDevelopment +
                        EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                        JobInvolvement+JobRole_HumanResources+
                        JobRole_ManufacturingDir+ 
                        JobRole_SalesRep+
                        as.factor(MaritalStatus)+ NumCompaniesWorked+
                        as.factor(OverTime)+ PercentSalaryHike+ PerformanceRating+ RelationshipSatisfaction+
                        StockOptionLevel+ TotalWorkingYears+ TrainingTimesLastYear+
                        WorkLifeBalance+ YearsAtCompany+YearsWithCurrManager, data=hr)
summary(ologit18)

lrtest(ologit1, ologit18)
#let's drop "the most insignificant" variable from ologit18- Age



# step 19
#Let's do the estimation without the variables droped

ologit19 = ologit.reg(as.numeric(JobSatisfaction)~as.factor(Attrition)+Travel_Frequently
                      + DailyRate + Department_ResearcsDevelopment +
                        EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                        JobInvolvement+JobRole_HumanResources+
                        JobRole_ManufacturingDir+ 
                        JobRole_SalesRep+
                        as.factor(MaritalStatus)+ NumCompaniesWorked+
                        as.factor(OverTime)+ PercentSalaryHike+ PerformanceRating+ RelationshipSatisfaction+
                        StockOptionLevel+ TotalWorkingYears+ TrainingTimesLastYear+
                        WorkLifeBalance+ YearsAtCompany+YearsWithCurrManager, data=hr)
summary(ologit19)

lrtest(ologit1, ologit19)
#let's drop "the most insignificant" variable from ologit19- JobRole_SalesRep



# step 20
#Let's do the estimation without the variables droped

ologit20 = ologit.reg(as.numeric(JobSatisfaction)~as.factor(Attrition)+Travel_Frequently
                      + DailyRate + Department_ResearcsDevelopment +
                        EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                        JobInvolvement+JobRole_HumanResources+
                        JobRole_ManufacturingDir+ 
                        as.factor(MaritalStatus)+ NumCompaniesWorked+
                        as.factor(OverTime)+ PercentSalaryHike+ PerformanceRating+ RelationshipSatisfaction+
                        StockOptionLevel+ TotalWorkingYears+ TrainingTimesLastYear+
                        WorkLifeBalance+ YearsAtCompany+YearsWithCurrManager, data=hr)
summary(ologit20)

lrtest(ologit1, ologit20)
#let's drop "the most insignificant" variable from ologit20- PerformanceRating 


# step 21
#Let's do the estimation without the variables droped

ologit21 = ologit.reg(as.numeric(JobSatisfaction)~as.factor(Attrition)+Travel_Frequently
                      + DailyRate + Department_ResearcsDevelopment +
                        EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                        JobInvolvement+JobRole_HumanResources+
                        JobRole_ManufacturingDir+ 
                        as.factor(MaritalStatus)+ NumCompaniesWorked+
                        as.factor(OverTime)+ PercentSalaryHike+ RelationshipSatisfaction+
                        StockOptionLevel+ TotalWorkingYears+ TrainingTimesLastYear+
                        WorkLifeBalance+ YearsAtCompany+YearsWithCurrManager, data=hr)
summary(ologit21)

lrtest(ologit1, ologit21)
#let's drop "the most insignificant" variable from ologit21- TrainingTimesLastYear


# step 22
#Let's do the estimation without the variables droped

ologit22 = ologit.reg(as.numeric(JobSatisfaction)~as.factor(Attrition)+Travel_Frequently
                      + DailyRate + Department_ResearcsDevelopment +
                        EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                        JobInvolvement+JobRole_HumanResources+
                        JobRole_ManufacturingDir+ 
                        as.factor(MaritalStatus)+ NumCompaniesWorked+
                        as.factor(OverTime)+ PercentSalaryHike+ RelationshipSatisfaction+
                        StockOptionLevel+ TotalWorkingYears+
                        WorkLifeBalance+ YearsAtCompany+YearsWithCurrManager, data=hr)
summary(ologit22)

lrtest(ologit1, ologit22)
#let's drop "the most insignificant" variable from ologit22- JobRole_ManufacturingDir



# step 23
#Let's do the estimation without the variables droped

ologit23 = ologit.reg(as.numeric(JobSatisfaction)~as.factor(Attrition)+Travel_Frequently
                      + DailyRate + Department_ResearcsDevelopment +
                        EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                        JobInvolvement+JobRole_HumanResources+
                        as.factor(MaritalStatus)+ NumCompaniesWorked+
                        as.factor(OverTime)+ PercentSalaryHike+ RelationshipSatisfaction+
                        StockOptionLevel+ TotalWorkingYears+
                        WorkLifeBalance+ YearsAtCompany+YearsWithCurrManager, data=hr)
summary(ologit23)

lrtest(ologit1, ologit23)
#let's drop "the most insignificant" variable from ologit23- PercentSalaryHike


#step24
#Let's do the estimation without the variables droped

ologit24 = ologit.reg(as.numeric(JobSatisfaction)~as.factor(Attrition)+Travel_Frequently
                      + DailyRate + Department_ResearcsDevelopment +
                        EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                        JobInvolvement+JobRole_HumanResources+
                        as.factor(MaritalStatus)+ NumCompaniesWorked+
                        as.factor(OverTime)+ RelationshipSatisfaction+
                        StockOptionLevel+ TotalWorkingYears+
                        WorkLifeBalance+ YearsAtCompany+YearsWithCurrManager, data=hr)
summary(ologit24)

lrtest(ologit1, ologit24)
#let's drop "the most insignificant" variable from ologit24- TotalWorkingYears


#step 25

ologit25 = ologit.reg(as.numeric(JobSatisfaction)~as.factor(Attrition)+Travel_Frequently
                      + DailyRate + Department_ResearcsDevelopment +
                        EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                        JobInvolvement+JobRole_HumanResources+
                        as.factor(MaritalStatus)+ NumCompaniesWorked+
                        as.factor(OverTime)+ RelationshipSatisfaction+
                        StockOptionLevel+
                        WorkLifeBalance+ YearsAtCompany+YearsWithCurrManager, data=hr)
summary(ologit25)

lrtest(ologit1, ologit25)
#let's drop "the most insignificant" variable from ologit24- YearsAtCompany

#step 26

ologit26 = ologit.reg(as.numeric(JobSatisfaction)~as.factor(Attrition)+Travel_Frequently
                      + DailyRate + Department_ResearcsDevelopment +
                        EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                        JobInvolvement+JobRole_HumanResources+
                        as.factor(MaritalStatus)+ NumCompaniesWorked+
                        as.factor(OverTime)+ RelationshipSatisfaction+
                        StockOptionLevel+
                        WorkLifeBalance+YearsWithCurrManager, data=hr)
summary(ologit26)

lrtest(ologit1, ologit26)
#let's drop "the most insignificant" variable from ologit24- RelationshipSatisfaction

#step 27

ologit27 = ologit.reg(as.numeric(JobSatisfaction)~as.factor(Attrition)+Travel_Frequently
                      + DailyRate + Department_ResearcsDevelopment +
                        EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                        JobInvolvement+JobRole_HumanResources+
                        as.factor(MaritalStatus)+ NumCompaniesWorked+
                        as.factor(OverTime)+StockOptionLevel+
                        WorkLifeBalance+YearsWithCurrManager, data=hr)
summary(ologit27)

lrtest(ologit1, ologit27)
#let's drop "the most insignificant" variable from ologit24- Department_ResearcsDevelopment

#step 28

ologit28 =  ologit.reg(as.numeric(JobSatisfaction)~as.factor(Attrition)+Travel_Frequently
                       + DailyRate +EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                         JobInvolvement+JobRole_HumanResources+
                         as.factor(MaritalStatus)+ NumCompaniesWorked+
                         as.factor(OverTime)+StockOptionLevel+
                         WorkLifeBalance+YearsWithCurrManager, data=hr)
summary(ologit28)

lrtest(ologit1, ologit28)
#let's drop "the most insignificant" variable from ologit24- JobRole_HumanResources

#step 29

ologit29 = ologit.reg(as.numeric(JobSatisfaction)~as.factor(Attrition)+Travel_Frequently
                      + DailyRate +EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                        JobInvolvement+as.factor(MaritalStatus)+ NumCompaniesWorked+
                        as.factor(OverTime)+StockOptionLevel+
                        WorkLifeBalance+YearsWithCurrManager, data=hr)
summary(ologit29)

lrtest(ologit1, ologit29)
#let's drop "the most insignificant" variable from ologit24- DailyRate

#step 30

ologit30 = ologit.reg(as.numeric(JobSatisfaction)~as.factor(Attrition)+Travel_Frequently
                      + EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                        JobInvolvement+as.factor(MaritalStatus)+ NumCompaniesWorked+
                        as.factor(OverTime)+StockOptionLevel+
                        WorkLifeBalance+YearsWithCurrManager, data=hr)
summary(ologit30)

lrtest(ologit1, ologit30)
#let's drop "the most insignificant" variable from ologit24- as.factor(MaritalStatus)Married
str(hr)
#step 31

ologit31 = ologit.reg(as.numeric(JobSatisfaction)~as.factor(Attrition)+Travel_Frequently
                      + EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                        JobInvolvement+MaritalStatus_Single+ MaritalStatus_Divorced+ NumCompaniesWorked+
                        as.factor(OverTime)+StockOptionLevel+
                        WorkLifeBalance+YearsWithCurrManager, data=hr)
summary(ologit31)

lrtest(ologit1, ologit31)
#let's drop "the most insignificant" variable from ologit24- MaritalStatus_Divorced

#step 32

ologit32 = ologit.reg(as.numeric(JobSatisfaction)~as.factor(Attrition)+Travel_Frequently
                      + EnvironmentSatisfaction+ as.factor(Gender) + HourlyRate+
                        JobInvolvement+MaritalStatus_Single+ NumCompaniesWorked+
                        as.factor(OverTime)+StockOptionLevel+
                        WorkLifeBalance+YearsWithCurrManager, data=hr)
summary(ologit32)

lrtest(ologit1, ologit32)
#let's drop "the most insignificant" variable from ologit24- EnvironmentSatisfaction

#step 33

ologit33 = ologit.reg(as.numeric(JobSatisfaction)~as.factor(Attrition)+Travel_Frequently
                      + as.factor(Gender) + HourlyRate+
                        JobInvolvement+MaritalStatus_Single+ NumCompaniesWorked+
                        as.factor(OverTime)+StockOptionLevel+
                        WorkLifeBalance+YearsWithCurrManager, data=hr)
summary(ologit33)

lrtest(ologit1, ologit33)
#let's drop "the most insignificant" variable from ologit24- JobInvolvement

#step 34

ologit34 = ologit.reg(as.numeric(JobSatisfaction)~as.factor(Attrition)+Travel_Frequently
                      + as.factor(Gender) + HourlyRate+
                        MaritalStatus_Single+ NumCompaniesWorked+
                        as.factor(OverTime)+StockOptionLevel+
                        WorkLifeBalance+YearsWithCurrManager, data=hr)
summary(ologit34)

lrtest(ologit1, ologit34)
#let's drop "the most insignificant" variable from ologit24- WorkLifeBalance

#step 35

ologit35 = ologit.reg(as.numeric(JobSatisfaction)~as.factor(Attrition)+Travel_Frequently
                      + as.factor(Gender) + HourlyRate+
                        MaritalStatus_Single+ NumCompaniesWorked+
                        as.factor(OverTime)+StockOptionLevel+
                        YearsWithCurrManager, data=hr)
summary(ologit35)

lrtest(ologit1, ologit35)
#let's drop "the most insignificant" variable from ologit24- StockOptionLevel

#step 36

ologit36 = ologit.reg(as.numeric(JobSatisfaction)~as.factor(Attrition)+Travel_Frequently
                      + as.factor(Gender) + HourlyRate+
                        MaritalStatus_Single+ NumCompaniesWorked+
                        as.factor(OverTime)+
                        YearsWithCurrManager, data=hr)
summary(ologit36)

lrtest(ologit1, ologit36)
#let's drop "the most insignificant" variable from ologit24- as.factor(Gender)1

#step 37

ologit37 = ologit.reg(as.numeric(JobSatisfaction)~as.factor(Attrition)+Travel_Frequently
                      + Male + HourlyRate+
                        MaritalStatus_Single+ NumCompaniesWorked+
                        as.factor(OverTime)+
                        YearsWithCurrManager, data=hr)
summary(ologit37)

lrtest(ologit1, ologit37)
#let's drop "the most insignificant" variable from ologit24- Male

#step 38

ologit38 = ologit.reg(as.numeric(JobSatisfaction)~as.factor(Attrition)+Travel_Frequently
                      + HourlyRate+
                        MaritalStatus_Single+ NumCompaniesWorked+
                        as.factor(OverTime)+
                        YearsWithCurrManager, data=hr)
summary(ologit38) #AIC: 3978.513

lrtest(ologit1, ologit38)
#let's drop "the most insignificant" variable from ologit24- Travel_Frequently

#step 39

ologit39 = ologit.reg(as.numeric(JobSatisfaction)~as.factor(Attrition)+ HourlyRate+
                        MaritalStatus_Single+ NumCompaniesWorked+
                        as.factor(OverTime)+
                        YearsWithCurrManager, data=hr)
summary(ologit39) #AIC: 3979.946

lrtest(ologit1, ologit39)



#c) Interactions in models

# H1: Employees who travel frequently have higher job satisfaction than those who do not.
# H1: Male employees have higher job satisfaction than female employees.
inter_model_log1 <- ologit.reg(as.numeric(JobSatisfaction) ~ Attrition+
                          HourlyRate+
                          NumCompaniesWorked+MaritalStatus_Single+
                          OverTime+
                          YearsWithCurrManager+Travel_Frequently*Male, data = hr)
summary(inter_model_log1) #AIC: 3980.151

inter_model_log2 <- ologit.reg(as.numeric(JobSatisfaction) ~ Attrition+
                                 HourlyRate+
                                 NumCompaniesWorked+MaritalStatus_Single+
                                 OverTime+
                                 YearsWithCurrManager+Travel_Frequently*Female, data = hr)
summary(inter_model_log2) #AIC: 3980.151

# Hypothesis (H7): The effect of being married on job satisfaction is moderated by overtime.
# Null Hypothesis (H7_0): There is no interaction effect between being married and working overtime on job satisfaction.
# Alternative Hypothesis (H7_A): There is an interaction effect between being married and working overtime on job satisfaction.
inter_model_log3 <- ologit.reg(as.numeric(JobSatisfaction) ~ Attrition+
                                 HourlyRate+
                                 NumCompaniesWorked+
                                 YearsWithCurrManager+as.factor(MaritalStatus)*as.factor(OverTime), data = hr)
summary(inter_model_log3) # AIC: 3981.703

# Hypothesis (H9): The effect of years at the company on job satisfaction is moderated by gender.
# Null Hypothesis (H9_0): There is no interaction effect between years at the company and gender on job satisfaction.
# Alternative Hypothesis (H9_A): There is an interaction effect between years at the company and gender on job satisfaction.
inter_model_log4 <- ologit.reg(as.numeric(JobSatisfaction) ~ Attrition+
                                 HourlyRate+
                                 NumCompaniesWorked+MaritalStatus_Single+
                                 OverTime+
                                 YearsWithCurrManager+YearsAtCompany*as.factor(Gender), data = hr)
summary(inter_model_log4) # AIC: 3978.556

# d) Quality table

# Fit the Logit model
logit_model <- polr(as.factor(JobSatisfaction) ~ Age + as.factor(Attrition) + as.factor(BusinessTravel) +
                      DailyRate + as.factor(Department) + DistanceFromHome + Education +
                       EnvironmentSatisfaction + as.factor(Gender) + HourlyRate +
                      JobInvolvement + JobLevel + as.factor(JobRole) +
                      as.factor(MaritalStatus) + MonthlyIncome + MonthlyRate + NumCompaniesWorked +
                      as.factor(OverTime) + PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                      WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion +
                      YearsWithCurrManager, data = hr, method = "logistic")

summary(logit_model)
coeftest(logit_model)

# Fit the Probit model
probit_model <- polr(as.factor(JobSatisfaction) ~ Age + as.factor(Attrition) + as.factor(BusinessTravel) +
                       DailyRate + as.factor(Department) + DistanceFromHome + Education +
                       EnvironmentSatisfaction + as.factor(Gender) + HourlyRate +
                       JobInvolvement + JobLevel + as.factor(JobRole) +
                       as.factor(MaritalStatus) + MonthlyIncome + MonthlyRate + NumCompaniesWorked +
                       as.factor(OverTime) + PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                       WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion +
                       YearsWithCurrManager, data = hr, method = "probit")

summary(probit_model)
coeftest(probit_model)

# Fit the final specific model
final_model_log <- polr(as.factor(JobSatisfaction) ~ Attrition+
                          HourlyRate+
                          NumCompaniesWorked+MaritalStatus_Single+
                          OverTime+
                          YearsWithCurrManager, data = hr, method = "logistic")
summary(final_model_log) #AIC: 3979.946
coeftest(final_model_log)

final_model_prob <- polr(as.factor(JobSatisfaction) ~ Attrition+
                           HourlyRate+
                           NumCompaniesWorked+MaritalStatus_Single+
                           OverTime+
                           YearsWithCurrManager, data = hr, method = "probit")
summary(final_model_prob) #AIC: 3979.62
coeftest(final_model_prob)

# Intermediate model

inter_model <- ologit.reg(as.numeric(JobSatisfaction) ~ Attrition+
                            HourlyRate+
                            NumCompaniesWorked+MaritalStatus_Single+
                            OverTime+
                            YearsWithCurrManager+YearsAtCompany*as.factor(Gender), data = hr)
summary(inter_model) # AIC: 3978.556

# Create a summary table using stargazer
stargazer(logit_model, probit_model, final_model_log, final_model_prob, type = "text", 
          column.labels = c("General", "General", "Final", "Final"),
          dep.var.labels = "JobSatisfaction",
          out = "model_comparison.txt")

# AIC and BIC for both final models (logit and probit)
logit_aic <- AIC(final_model_log)
probit_aic <- AIC(final_model_prob)
logit_bic <- BIC(final_model_log)
probit_bic <- BIC(final_model_prob)

print(c(logit_aic, probit_aic))
print(c(logit_bic, probit_bic))

# Both AIC and BIC values are slightly 
#lower for the ordered probit model compared to the ordered logit model.


# h) Performing the Hosmer-Lemeshow test, the Lipsitz, and the Pulkstenis-Robinson tests

# Probit
# goodness-of-fit tests
logitgof(hr$JobSatisfaction, fitted(final_model_prob), g = 10, ord = TRUE)
pulkrob.chisq(final_model_prob, c("Attrition"))
#lipsitz.test(final_model_prob)

# Summary of the model
summary(final_model_prob)

# Predicted probabilities from the model
predicted_probs <- predict(final_model_prob, type = "probs")

# Create a numeric response variable for the JobSatisfaction categories
response_numeric <- as.numeric(hr$JobSatisfaction)

# Divide observations into deciles based on predicted probabilities
n_groups <- 10
group <- cut(predicted_probs[, 1], breaks = n_groups, labels = FALSE)

# Create a table of observed and expected frequencies for each decile
observed <- table(group, response_numeric)
expected <- matrix(0, nrow = n_groups, ncol = nlevels(as.factor(response_numeric)))

for (i in 1:n_groups) {
  for (j in 1:nlevels(as.factor(response_numeric))) {
    expected[i, j] <- sum(predicted_probs[group == i, j])
  }
}

# Calculate the Lipsitz statistic
lipsitz_stat <- sum((observed - expected)^2 / expected)

# Degrees of freedom for the chi-square distribution
df <- (n_groups - 2) * (nlevels(as.factor(response_numeric)) - 1)

# Calculate the p-value
p_value <- 1 - pchisq(lipsitz_stat, df)

# Print results
cat("Lipsitz Test:\n")
cat("Test statistic:", lipsitz_stat, "\n")
cat("Degrees of freedom:", df, "\n")
cat("p-value:", p_value, "\n")

# Logit

# goodness-of-fit tests
logitgof(hr$JobSatisfaction, fitted(final_model_log), g = 10, ord = TRUE)
pulkrob.chisq(final_model_log, c("Attrition"))
#lipsitz.test(final_model_log)

# Summary of the model
summary(final_model_log)

# Predicted probabilities from the model
predicted_logs <- predict(final_model_log, type = "probs")

# Create a numeric response variable for the JobSatisfaction categories
response_numeric <- as.numeric(hr$JobSatisfaction)

# Check the structure of predicted_probs to ensure it has the correct format
if (!is.matrix(predicted_logs) || ncol(predicted_logs) != nlevels(as.factor(hr$JobSatisfaction))) {
  stop("The predicted probabilities do not have the expected format.")
}

# Divide observations into deciles based on the first column of predicted probabilities
n_groups <- 10
group <- cut(predicted_probs[, 1], breaks = n_groups, labels = FALSE)

# Create a table of observed frequencies for each decile
observed <- table(group, response_numeric)

# Create a matrix for expected frequencies
expected <- matrix(0, nrow = n_groups, ncol = nlevels(as.factor(response_numeric)))

for (i in 1:n_groups) {
  for (j in 1:nlevels(as.factor(response_numeric))) {
    expected[i, j] <- sum(predicted_logs[group == i, j])
  }
}

# Ensure no expected frequency is zero to avoid division by zero errors
if (any(expected == 0)) {
  warning("Some expected frequencies are zero, which may lead to inaccurate results.")
}

# Calculate the Lipsitz statistic
lipsitz_stat <- sum((observed - expected)^2 / expected)

# Degrees of freedom for the chi-square distribution
df <- (n_groups - 2) * (nlevels(as.factor(response_numeric)) - 1)

# Calculate the p-value
p_value <- 1 - pchisq(lipsitz_stat, df)

# Print results
cat("Lipsitz Test:\n")
cat("Test statistic:", lipsitz_stat, "\n")
cat("Degrees of freedom:", df, "\n")
cat("p-value:", p_value, "\n")



# i) Proportional odds assumption

# Verification of the proportional odds assumption using the Brant test
brant_test <- brant(final_model_log)
print(brant_test)

#H0: the assumption is ok (our model is ok)


# e) Marginal effects for the final model (from the general-to specific approach);

ologit = ologit.reg(as.numeric(JobSatisfaction)~as.factor(Attrition)+
                      HourlyRate+ NumCompaniesWorked+
                      MaritalStatus_Single+
                      as.factor(OverTime)+
                      YearsWithCurrManager, data = hr)
summary(ologit)

# marginal effects
options(scipen=999)
margins.oglmx(ologit, atmeans = TRUE)

# marginal effects for a user-defined characteristics
source("ome.R")
# this function works with polr models
model= polr(as.factor(JobSatisfaction)~Attrition+
              + HourlyRate+
              NumCompaniesWorked+ MaritalStatus_Single+
              OverTime+
              YearsWithCurrManager, data = hr, method="logistic")

summary(model)

hr$Attrition<- as.numeric(hr$Attrition)
hr$OverTime<- as.numeric(hr$OverTime)
hr$MaritalStatus_Single<- as.numeric(hr$MaritalStatus_Single)

x = c(mean(hr$Attrition), mean(hr$HourlyRate), mean(hr$NumCompaniesWorked),
      mean(hr$MaritalStatus_Single),mean(hr$OverTime), mean(hr$YearsWithCurrManager))
ome(model, x)

x = c(0, 30, 0, 0, 0, 2)
ome(model, x)

x = c(1,100,9,1,1,17)
ome(model, x)


# f) R2 statistics (R2 McKelvey-Zavoina, count R2, and adjusted count R2;

final_model_log <- polr(as.factor(JobSatisfaction) ~ Attrition+
                           HourlyRate+
                           NumCompaniesWorked+MaritalStatus_Single+
                           OverTime+
                           YearsWithCurrManager, data = hr, method = "logistic")
summary(final_model_log) #AIC: 3979.62
coeftest(final_model_log)

# Function to calculate McKelvey-Zavoina R2
mckelvey_zavoina_r2 <- function(model) {
  predicted_probs <- predict(model, type = "probs")
  predicted_vals <- apply(predicted_probs, 1, which.max)
  mean_val <- mean(predicted_vals)
  residual_variance <- mean((predicted_vals - mean_val)^2)
  total_variance <- residual_variance + pi^2 / 3
  r2_mz <- residual_variance / total_variance
  return(r2_mz)
}

# Calculate McKelvey-Zavoina R2
r2_mz <- mckelvey_zavoina_r2(final_model_log)
r2_mz

# Calculate Count R2
predicted_classes <- predict(final_model_log, type = "class")
correct_predictions <- sum(predicted_classes == hr$JobSatisfaction)
total_observations <- length(hr$JobSatisfaction)
count_r2 <- correct_predictions / total_observations
count_r2

# Calculate Adjusted Count R2
most_frequent_class <- which.max(table(hr$JobSatisfaction))
correct_by_chance <- sum(hr$JobSatisfaction == most_frequent_class) / total_observations
adjusted_count_r2 <- (count_r2 - correct_by_chance) / (1 - correct_by_chance)
adjusted_count_r2



