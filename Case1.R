# Case 1
################################################################################

# Install dplyr to removing rows 
install.packages("dplyr")
library(dplyr)

# Moments package for skewness and kurtosis 
install.packages('moments')
library(moments)

# Corr Plot package for visual correlation matrix 

install.packages('corrplot')
library(corrplot)

################################################################################

# Read data set in to Atlas_data
Atlas_data <- read.csv(file.choose(), header = TRUE)

# View Atlas_data
View(Atlas_data)


#Boxplot to look for outliers 
boxplot(Atlas_data$Bonus.Index ~ Atlas_data$Type)

# Checking for outliers using IQR
IQR <- IQR(Atlas_data$Bonus.Index)
IQR

# Confidence interval for the bonus_index mean 
# 95% confidence interval for the bonus index

t.test(Atlas_data$Bonus.Index, conf.level = 0.95)$conf.int
# The mean bonus_index in the population is between 0.49 and 0.55 with 95% confidence

####################################################################################

# Subset Atlas_data in to only job type full time
Atlas_data_fulltime <- subset(Atlas_data, select=c(Type, Bonus.Index))
Atlas_data_fulltime <- Atlas_data_fulltime[Atlas_data_fulltime$Type=="Full Time",]
View(Atlas_data_fulltime)

# Checking for outliers using IQR for full time only
IQR_full_time <- IQR(Atlas_data_fulltime$Bonus.Index)
IQR_full_time




# Subset Atlas_data in to only job type Contract
Atlas_data_Contract <- subset(Atlas_data, select=c(Type, Bonus.Index))
Atlas_data_Contract <- Atlas_data_Contract[Atlas_data_Contract$Type=="Contract",]
View(Atlas_data_Contract)

# Checking for outliers using IQR for Contract only 
IQR_Contract <- IQR(Atlas_data_Contract$Bonus.Index)
IQR_Contract



# Subset Atlas_data in to only job type Temporary
Atlas_data_Temporary <- subset(Atlas_data, select=c(Type, Bonus.Index))
Atlas_data_Temporary <- Atlas_data_Temporary[Atlas_data_Temporary$Type=="Temporary",]
View(Atlas_data_Temporary)

# Checking for outliers using IQR for Contract only 
IQR_Temporary <- IQR(Atlas_data_Temporary$Bonus.Index)
IQR_Temporary



#####################################################################################################################
# LOOKING FOR OUTLIERS AND IF THERE ARE REPLACE IT WITH THE MEDIAN (THIS IS DONE FOR EACH SUBSET OF JOB TYPE)
######################################################################################################################

########################
# FULL TIME DATA
########################

# Get the Third quartile of Bonus Index for full time type
q3_fulltime <- quantile(Atlas_data_fulltime$Bonus.Index, 0.75)
q3_fulltime

# Get the First quartile of Bonus Index for full time type
q1_fulltime <- quantile(Atlas_data_fulltime$Bonus.Index, 0.25)
q1_fulltime

# Upper whisker (q3 + 1.5 * IQR) for full time
upper_bound_fulltime <- q3_fulltime + 1.5 * IQR_full_time
upper_bound_fulltime


# lower whisker (q1 - 1.5 * IQR) for full time 
lower_bound_fulltime <- q1_fulltime - 1.5 * IQR_full_time
lower_bound_fulltime

# Identifying the outlier rows in full time
outlier_rows_fulltime <- subset(Atlas_data_fulltime, (Atlas_data_fulltime$Bonus.Index < lower_bound_fulltime) | 
                                   (Atlas_data_fulltime$Bonus.Index  > upper_bound_fulltime))

# Viewing outlier rows in full time
View(outlier_rows_fulltime)


# Get the median of full time bonus index
median_TC_fulltime <- median(Atlas_data_fulltime2$Bonus.Index)
median_TC_fulltime

#replace outliers in full time with the median of the full time bonus index
Atlas_data_fulltime[(Atlas_data_fulltime$Bonus.Index < lower_bound_fulltime) | (Atlas_data_fulltime$Bonus.Index  > upper_bound_fulltime), ]$Bonus.Index <- median_TC_fulltime
View(Atlas_data_fulltime)



##############################
# CONTRACT DATA
#############################

# Get the third quartile of Bonus Index for Contract type
q3_contract <- quantile(Atlas_data_Contract$Bonus.Index, 0.75)
q3_contract

# First quantile of Bonus Index for Contract type
q1_contract <- quantile(Atlas_data_Contract$Bonus.Index, 0.25)
q1_contract

# Upper whisker (q3 + 1.5 * IQR) for Contract
upper_bound_contract <- q3_contract + 1.5 * IQR_Contract
upper_bound_contract

# lower whisker (q1 - 1.5 * IQR) for contract
lower_bound_contract <- q1_contract - 1.5 * IQR_Contract
lower_bound_contract


# Identifying the outlier rows in Contract
outlier_rows_contract <- subset(Atlas_data_Contract, (Atlas_data_Contract$Bonus.Index < lower_bound_contract) | 
                                  (Atlas_data_Contract$Bonus.Index  > upper_bound_contract))
View(outlier_rows_contract)

######## Since there are no ouliers then there is no rows to update with the median.######### 


################################
# TEMPORARY DATA
###############################

# Get the third quartile of Bonus Index for Temporary type
q3_temporary <- quantile(Atlas_data_Temporary$Bonus.Index, 0.75)
q3_temporary

# Get the First quartile of Bonus Index for Temporary type
q1_temporary <- quantile(Atlas_data_Temporary$Bonus.Index, 0.25)
q1_temporary

# Upper whisker (q3 + 1.5 * IQR) for Temporary
upper_bound_temporary <- q3_temporary + 1.5 * IQR_Temporary
upper_bound_temporary


# Lower whisker (q1 - 1.5 * IQR) for Temporary
lower_bound_temporary <- q1_temporary - 1.5 * IQR_Temporary
lower_bound_temporary


# Identifying the outlier rows in Temporary

outlier_rows_temporary <- subset(Atlas_data_Temporary, (Atlas_data_Temporary$Bonus.Index < lower_bound_temporary) | 
                                  (Atlas_data_Temporary$Bonus.Index  > upper_bound_temporary))
View(outlier_rows_temporary)

##### Since there are no outliers then there is no rows to update with the median. ###################



#######################################################################################################################
###### ANALYSIS CHARTS FOR EACH SUBSET SET OF JOB TYPES
#########################################################################################################################

# Histogram for each job type

hist(Atlas_data_fulltime$Bonus.Index, main = "Bonus Index for Full Time ", xlab = "Bonus Index", col = 'blue')

hist(Atlas_data_Contract$Bonus.Index, main = "Bonus Index for Contract ", xlab = "Bonus Index", col = 'blue')

hist(Atlas_data_Temporary$Bonus.Index, main = "Bonus Index for Temporary ", xlab = "Bonus Index", col = 'blue')


################################################################################################################################


# Computing skewness for each job type

skew_FT <- skewness(Atlas_data_fulltime$Bonus.Index)
skew_FT

skew_CT <- skewness(Atlas_data_Contract$Bonus.Index)
skew_CT

skew_TP <- skewness(Atlas_data_Temporary$Bonus.Index)
skew_TP

# Moderates skews 0.5 to 1



# Computing Kurtosis for each job type

kurtosis_FT <- kurtosis(Atlas_data_fulltime$Bonus.Index)
kurtosis_FT

kurtosis_CT <- kurtosis(Atlas_data_Contract$Bonus.Index)
kurtosis_CT

kurtosis_TP <- kurtosis(Atlas_data_Temporary$Bonus.Index)
kurtosis_TP

# (-3 to 3 ) is OK for normal distribution. Greater than 4 or less than -4 indicates many outliers on the right or left side. 

###################################################################################################################################

# Hypotheses testing on each job type 

t.test(Atlas_data_fulltime$Bonus.Index, mu = 0.58, prob = 0.95)$p.value
# Pvalue = 0.00007698599 

t.test(Atlas_data_Contract$Bonus.Index, mu = 0.7, prob = 0.95)$p.value
# Pvalue = 0.00000000000005173668

t.test(Atlas_data_Temporary$Bonus.Index, mu = 0.77, prob = 0.95)$p.value
# Pvalue = 0.0000000000001185271


#####################################################################################################################################

### Notes: Look for correlation between age, bonus index, and job type.

##############################
# CONTRACT DATA
#############################
#Subsetting data first
Atlas_data_Contract2 <- subset(Atlas_data, select=c(Type, Bonus.Index, Age))
Atlas_data_Contract2 <- Atlas_data_Contract2[Atlas_data_Contract2$Type=="Contract",]
View((Atlas_data_Contract2))

#Mean Age for all Contract Employees 
mean_age_Contract <- mean(Atlas_data_Contract2$Age)
mean_age_Contract
# 48.19337

# Mean Bonus index for all Contract Employees
mean_bonus_index_Contract <- mean(Atlas_data_Contract2$Bonus.Index)
mean_bonus_index_Contract
# 0.4946409

#Scatter plot and linear regression to show correlation between age and bonus index for all contract employees
plot(Atlas_data_Contract2$Age, Atlas_data_Contract2$Bonus.Index,main = "Correlation between Age and Bonus index for all Contract Employees",
     xlab= 'Age', ylab = 'Bonus Index', col = 'blue', pch = 16)
lines(lowess(Atlas_data_Contract2$Age, Atlas_data_Contract2$Bonus.Index), col = 3, lwd = 3)

# New subsetting to show how many employees were violated within the contract job type
Atlas_data_Contract3 <- Atlas_data_Contract2[Atlas_data_Contract2$Bonus.Index < 0.7,]
View(Atlas_data_Contract3)
#120 over 181 were violated, 66.3%

# The mean bonus index of the employees violated within the contract job type
mean_bonus_index_violated_Contract <- mean(Atlas_data_Contract3$Bonus.Index)
mean_bonus_index_violated_Contract
#0.29 mean index

# The mean Age of the employees violated within the contract job type
mean_age_violated_Contract <- mean(Atlas_data_Contract3$Age)
mean_age_violated_Contract
#48.555

#Scatter plot and linear regression to show correlation between age and bonus index for violated Contract Employees
plot(Atlas_data_Contract3$Age, Atlas_data_Contract3$Bonus.Index,main = "Correlation between Age and Bonus index for Violated Contract Employees",
     xlab= 'Age', ylab = 'Bonus Index', col = 'blue', pch = 16)
lines(lowess(Atlas_data_Contract3$Age, Atlas_data_Contract3$Bonus.Index), col = 3, lwd = 3)


#############################
# FULL TIME DATA
#############################
#Subsetting data first
Atlas_data_fulltime2 <- subset(Atlas_data, select=c(Type, Bonus.Index, Age))
Atlas_data_fulltime2 <- Atlas_data_fulltime2[Atlas_data_fulltime2$Type=="Full Time",]
View((Atlas_data_fulltime2))

#Mean Age for all fulltime Employees 
mean_age_full <- mean(Atlas_data_fulltime2$Age)
mean_age_full
# 41.30924

# Mean Bonus index for all Full time Employees
mean_bonus_index_full <- mean(Atlas_data_fulltime2$Bonus.Index)
mean_bonus_index_full
# 0.4532773

#Scatter plot and linear regression to show correlation between age and bonus index for all full time employees
plot(Atlas_data_fulltime2$Age, Atlas_data_fulltime2$Bonus.Index,main = "Correlation between Age and Bonus index for all Full Time Employees",
     xlab= 'Age', ylab = 'Bonus Index', col = 'blue', pch = 16)
lines(lowess(Atlas_data_fulltime2$Age, Atlas_data_fulltime2$Bonus.Index), col = 3, lwd = 3)

# New subsetting to show how many employees were violated within the full time job type
Atlas_data_fulltime3 <- Atlas_data_fulltime2[Atlas_data_fulltime2$Bonus.Index < 0.58,]
View(Atlas_data_fulltime3)
#82 over 119 were violated. 68.9% 

# The mean bonus index of the employees violated within the full time job type
mean_bonus_index_violated_full <- mean(Atlas_data_fulltime3$Bonus.Index)
mean_bonus_index_violated_full
# 0.2596341

# The mean Age of the employees violated within the full time job type
mean_age_violated_full <- mean(Atlas_data_fulltime3$Age)
mean_age_violated_full
# 42.40976

#Scatter plot and linear regression to show correlation between age and bonus index for violated full time Employees
plot(Atlas_data_fulltime3$Age, Atlas_data_fulltime3$Bonus.Index, main = "Correlation between Age and Bonus index for Violated Full Time Employees",
     xlab= 'Age', ylab = 'Bonus Index', col = 'blue', pch = 16)
lines(lowess(Atlas_data_fulltime3$Age, Atlas_data_fulltime3$Bonus.Index), col = 3, lwd = 3)


################################
# TEMPORARY DATA
###############################
#Subsetting data first
Atlas_data_Temporary2 <- subset(Atlas_data, select=c(Type, Bonus.Index, Age))
Atlas_data_Temporary2 <- Atlas_data_Temporary2[Atlas_data_Temporary2$Type=="Temporary",]
View((Atlas_data_Temporary2))

#Mean Age for all Temporary Employees 
mean_age_Temp <- mean(Atlas_data_Temporary2$Age)
mean_age_Temp
# 42.49025

# Mean Bonus index for all Temporary Employees
mean_bonus_index_temp <- mean(Atlas_data_Temporary2$Bonus.Index)
mean_bonus_index_temp
# 0.6009

#Scatter plot and linear regression to show correlation between age and bonus index for all Temporary employees
plot(Atlas_data_Temporary2$Age, Atlas_data_Temporary2$Bonus.Index, main = "Correlation between Age and Bonus index for all Temporary Employees",
     xlab= 'Age', ylab = 'Bonus Index', col = 'blue', pch = 16)
lines(lowess(Atlas_data_Temporary2$Age, Atlas_data_Temporary2$Bonus.Index), col = 3, lwd = 3)

# New subsetting to show how many employees were violated within the Temporary job type
Atlas_data_Temporary3 <- Atlas_data_Temporary2[Atlas_data_Temporary2$Bonus.Index < 0.77,]
View(Atlas_data_Temporary3)
# 146 over 200 were violated. 73%

# The mean bonus index of the employees violated within the Temporary job type
mean_bonus_index_violated_temp <- mean(Atlas_data_Temporary3$Bonus.Index)
mean_bonus_index_violated_temp
# 0.4487671

# The mean Age of the employees violated within the Temporary job type
mean_age_violated_temp <- mean(Atlas_data_Temporary3$Age)
mean_age_violated_temp
# 43.5226

#Scatter plot and linear regression to show correlation between age and bonus index for violated Temporary Employees
plot(Atlas_data_Temporary3$Age, Atlas_data_Temporary3$Bonus.Index, main = "Correlation between Age and Bonus index for Violated Temporary Employees",
     xlab= 'Age', ylab = 'Bonus Index', col = 'blue', pch = 16)
lines(lowess(Atlas_data_Temporary3$Age, Atlas_data_Temporary3$Bonus.Index), col = 3, lwd = 3)



#################################################################################
# Full time correlation Matrix
# Subset new data 
Atlas_data_fulltime4 <- Atlas_data[Atlas_data$Type=="Full Time",]
View(Atlas_data_fulltime4)

# Create vector of columns to drop  
drop_columns <- c("ID", "Type")

# Drop columns to show only numerical data
num_mydata_full <- Atlas_data_fulltime4  %>% 
  select(-one_of(drop_columns))

# View new data
View(num_mydata_full)

# visual correlation matrix - need corrplot package 
corrplot(cor(num_mydata_full), diag = FALSE)

# Scatter plot and linear regression of numerical data for full time focusing on how tenure affects bonus index
plot(num_mydata_full$Tenure, num_mydata_full$Bonus.Index,main = "Correlation between Tenure and Bonus index for all Full Time Employees",
     xlab= 'Tenure', ylab = 'Bonus Index',  col = 'blue', pch = 16)  
lines(lowess(num_mydata_full$Tenure, num_mydata_full$Bonus.Index), col = 3, lwd = 3)



# Contract Correlation Matrix 
# Subset new data 
Atlas_data_Contract4 <- Atlas_data[Atlas_data$Type=="Contract",]
View(Atlas_data_Contract4)

# Create vector of columns to drop 
drop_columns <- c("ID", "Type")

# Drop columns to show only numerical data
num_mydata_contract <- Atlas_data_Contract4  %>% 
  select(-one_of(drop_columns))

# View new data
View(num_mydata_contract)

# visual correlation matrix - need corrplot package 
corrplot(cor(num_mydata_contract), diag = FALSE)

# Scatter plot and linear regression of numerical data for Contract focusing on how tenure affects bonus index
plot(num_mydata_contract$Tenure, num_mydata_contract$Bonus.Index, main = "Correlation between Tenure and Bonus index for all Contract Employees",
     xlab= 'Tenure', ylab = 'Bonus Index', col = 'blue', pch = 16)
lines(lowess(num_mydata_contract$Tenure, num_mydata_contract$Bonus.Index), col = 3, lwd = 3)



# Temporary Correlation matrix 
# Subset new data 
Atlas_data_Temporary4 <- Atlas_data[Atlas_data$Type=="Temporary",]
View(Atlas_data_Temporary4)

# Create vector of columns to drop  
drop_columns <- c("ID", "Type")

# Drop columns to show only numerical data
num_mydata_temp <- Atlas_data_Temporary4  %>% 
  select(-one_of(drop_columns))

# View new data
View(num_mydata)

# visual correlation matrix - need corrplot package 
corrplot(cor(num_mydata_temp), diag = FALSE)

# Scatter plot and linear regression of numerical data for Temporary focusing on how tenure affects bonus index
plot(num_mydata_temp$Tenure, num_mydata_temp$Bonus.Index,main = "Correlation between Tenure and Bonus index for all Temporary Employees",
     xlab= 'Tenure', ylab = 'Bonus Index', col = 'blue', pch = 16)
lines(lowess(num_mydata_temp$Tenure, num_mydata_temp$Bonus.Index), col = 3, lwd = 3)



#########################################################################################################################

###############################################################################

### Checking the pay for each type ###
#CONTRACT
CT_Profit <- 20000 + (((0.7 - mean_bonus_index_violated_Contract)/ 0.7)*100)*50000
CT_Profit
# Choosing contracts would produce an average pay of $2,912,262


## FULL TIME
FT_Profit <- 20000 + (((0.58 - mean_bonus_index_violated_full)/ 0.58)*100)*50000
FT_Profit
# Choosing Full Time would produce an average pay of $2,781,775


## TEMPORARY
TP_Profit <- 20000 + (((0.77 - mean_bonus_index_violated_temp)/ 0.77)*100)*50000
TP_Profit
# Choosing temporary would produce an average pay of $2,105,928


 

