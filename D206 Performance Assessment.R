library(tidyverse)
library(visdat)
library(dlookr)

# load and profile data
data <- read.csv('performance review/data/medical/medical_raw_data.csv')
str(data)

## Detection and Treatment of Duplicates
# use duplicated() to return duplicate rows as TRUE
anyDuplicated(data)

### Detection and Treatment of Missing Values
# Detect if missing values exist
colSums(is.na(data))
# Visualize missing values
vis_miss(data) # 2.4% of the values are missing

### Treatment of Treating Missing Data ---
# A: Examine Distribution
hist(data$Children) # right skewed- positive
hist(data$Age) # uniform
hist(data$Income) # right skewed- positive
hist(data$Anxiety) # right-skewed
hist(data$Initial_days) # bimodal
hist(data$Overweight) # bimodal
table(data$Soft_drink) # categorical

# B: Perform Imputation for Missing Data
# The Children distribution is right-skewed, so I will impute using the median
data$Children[is.na(data$Children)] <- median(data$Children, na.rm = TRUE)

# The Age distribution is uniform, so I will impute using the mean
data$Age[is.na(data$Age)] <- mean(data$Age, na.rm = TRUE)

# The income distribution is right-skewed, so I will impute using the median
data$Income[is.na(data$Income)] <- median(data$Income, na.rm = TRUE)

# The Anxiety distribution is right-skewed, so I will impute using the median
data$Anxiety[is.na(data$Anxiety)] <- median(data$Anxiety, na.rm = TRUE)

# The Initial_days distribution is bimodal, so I will impute using the median
data$Initial_days[is.na(data$Initial_days)] <- median(data$Initial_days, na.rm = TRUE)

# The Overweight distribution is bimodal, so I will impute using the median
data$Overweight[is.na(data$Overweight)] <- median(data$Overweight, na.rm = TRUE)

# The Soft_drink distribution is categorical, so I will impute using the mode
data$Soft_drink[is.na(data$Soft_drink)] <- (names(which.max(table(data$Soft_drink))))


# C: Verification
colSums(is.na(data))
sum(is.na(data))
hist(data$Children) # right skewed- positive
hist(data$Age) # uniform
hist(data$Income) # right skewed- positive
hist(data$Anxiety) # right-skewed
hist(data$Initial_days) # bimodal
hist(data$Overweight) # bimodal
table(data$Soft_drink) # categorical
# The age histogram after imputation looked weird, so I'm going to check
summary(data$Age) # mean and median are the same


## Detecting and Treating Outliers for all numerical data

# Use box plots to visualize outliers based on original data (not Z-score)
boxplot(data[, c('Population', 'Children', 'Age', 'Income', 'VitD_levels', 'Doc_visits', 'Full_meals_eaten', 'VitD_supp', 'Initial_days', 'TotalCharge', 'Additional_charges')], main = "Boxplots of Numeric Untreated Numeric Varaibles")
# From the visualization it appears Population, Income, TotalCharge, and Additional_charges have outliers, but I will zoom in to check Children, VitD_levels, Full_meals_eaten, and VitD_supp
boxplot(data[, c('Children', 'VitD_levels', 'Full_meals_eaten', 'VitD_supp')])
# These also have potential outliers.

# Calculate Z-Score and store results in a new variable "Income_z", etc., to detect outliers
data$Income_z <- scale(data$Income)
data$VitD_levels_z <- scale(data$VitD_levels)
data$Population_z <- scale(data$Population)
data$Children_z <- scale(data$Children)
data$VitD_supp_z <- scale(data$VitD_supp)
data$TotalCharge_z <- scale(data$TotalCharge)
data$Additional_charges_z <- scale(data$Additional_charges)

boxplot(data[, c('Income_z', 'VitD_levels_z', 'Population_z', 'Children_z', 'VitD_supp', 'TotalCharge', 'Additional_charges')])


#  Display calculated Z-scores
head(data$Population_z)
head(data$Children_z)
head(data$Income_z)
head(data$VitD_levels_z)
head(data$VitD_supp_z)
head(data$TotalCharge_z)
head(data$Additional_charges_z)

# Use visualizations to examine outliers based on Z-score
boxplot(data[, c('Population_z', 'Children_z', 'Income_z', 'VitD_levels_z', 'Full_meals_eaten_z', 'VitD_supp_z', 'TotalCharge_z', 'Additional_charges_z')], main = "Boxplots of Numeric Treated Numeric Varaibles")



# From the dlookr package, double check you found all variables with outliers and what percent of the data are outliers. If outliers are 5% or less than of all data points, I will completely omit the outliers.  If Over 5% of points are outliers, I will impute the outliers.
find_outliers(data, rate = TRUE)
# Columns with outliers 10 11 12 16 20 24 44 45
# 10 is "Lat" - will ignore 
# 11 is "Lng" - will ignore
# 12 is "Population" - 8.77% of data are outliers, will impute
# 16 is "Children" - 4.57% of data are outliers, will omit
# 20 is "Income" - 7.02% of data are outliers, will impute
# 24 is "VitD_levels" - 5.34% of data are outliers, will impute
# 44 is "TotalCharge"  - 4.66% of data are outliers, will omit
# 45 is "Additional_charges" - 4.24% of data are outliers, will omit

# Now, replace the outliers in Population and Income with NA (because over 5%. code adapted from[@https://www.digitalocean.com/community/tutorials/outlier-analysis-in-r]
for (x in c('Population', 'Income', 'VitD_levels')) {
  value = data[,x][data[,x] %in% boxplot.stats(data[,x])$out]
  data[,x][data[,x] %in% value] = NA
}
#Check
sum(is.na(data$Population)) # 855 missing values
sum(is.na(data$Income)) # 702 missing values
sum(is.na(data$VitD_levels)) # 534 missing values
# Determine distribution shape to decide imputation method
hist(data$Population) # skewed, impute with median
hist(data$Income) # normal, impute with mean
hist(data$VitD_levels) # normal, will impute with mean

# Impute
data$Population[is.na(data$Population)] <- median(data$Population, na.rm = T)
data$Income[is.na(data$Income)] <- mean(data$Income, na.rm = T)
data$VitD_levels[is.na(data$VitD_levels)] <- mean(data$VitD_levels, na.rm = T)

boxplot(data[, c('Population', 'Income', 'VitD_levels')])
# Check
sum(is.na(data))

# Check that Age only contains full numbers
unique(data$Age) # Has a decimal, will need to round
data$Age <- round(data$Age, digits = 0)
unique(data$Age) # check

## Re-express Categorical Variables
# Ordinal Encoding
unique(data$Education) # Find unique categories

# [1] "Some College, Less than 1 Year"          
# [2] "Some College, 1 or More Years, No Degree"
# [3] "GED or Alternative Credential"           
# [4] "Regular High School Diploma"             
# [5] "Bachelor's Degree"                       
# [6] "Master's Degree"                         
# [7] "Nursery School to 8th Grade"             
# [8] "9th Grade to 12th Grade, No Diploma"     
# [9] "Doctorate Degree"                        
# [10] "Associate's Degree"                      
# [11] "Professional School Degree"              
# [12] "No Schooling Completed"  


data$Education <- recode_factor(data$Education
                         , "Some College, Less than 1 Year" = 13
                         ,"Some College, 1 or More Years, No Degree" = 14
                         ,"GED or Alternative Credential" = 12
                         , "Regular High School Diploma" = 12
                         , "Bachelor's Degree" = 16
                         , "Master's Degree" = 18
                         , "Nursery School to 8th Grade" = 8
                         , "9th Grade to 12th Grade, No Diploma" = 9
                         , "Doctorate Degree" = 24
                         , "Associate's Degree" = 15
                         , "Professional School Degree" = 18
                         , "No Schooling Completed" = 0
                         )

unique(data$Complication_risk)
data$Complication_risk <- recode_factor(data$Complication_risk
                                 , "Low" = 1
                                 , "Medium" = 2
                                 ,"High" = 3)
# Categorical Encoding
unique(data$Area)
data$Area <- recode(data$Area
                    , "Suburban" = 1
                    , "Urban" = 2
                    , "Rural" = 3)

unique(data$Employment)
data$Employment <- recode(data$Employment
                          , "Unemployed" = 1
                          , "Student" = 2
                          , "Part Time" = 3
                          , "Full Time" = 4
                          , "Retired" = 5)

unique(data$Marital)
data$Marital <- recode(data$Marital
                       , "Divorced" = 1
                       , "Married" = 2
                       , "Widowed" = 3
                       , "Never Married" = 4
                       , "Separated" = 5)

data$Gender <- recode(data$Gender
                      , "Male" = 1
                      , "Female" = 2
                      , "Prefer not to answer" = 3)

data$ReAdmis <- recode(data$ReAdmis
                       , "No" = 0
                       , "Yes" = 1)

unique(data$Initial_admin)
data$Initial_admin <- recode(data$Initial_admin
                             , "Emergency Admission" = 1
                             , "Elective Admission" = 2
                             , "Observation Admission" = 3)

unique(data$HighBlood)
data$HighBlood <- recode(data$HighBlood
                          , "Yes" = 1
                          , "No" = 0)

unique(data$Stroke)
data$Stroke <- recode(data$Stroke
                      , "Yes" = 1
                      , "No" = 0)

 
unique(data$Arthritis)
data$Arthritis <- recode(data$Arthritis
                         , "Yes" = 1
                         , "No" = 0)

unique(data$Diabetes)
data$Diabetes <- recode(data$Diabetes
                        , "Yes" = 1
                        , "No" = 0)

unique(data$Hyperlipidemia)
data$Hyperlipidemia <- recode(data$Hyperlipidemia
                              , "Yes" = 1
                              , "No" = 0)

unique(data$BackPain)
data$BackPain <- recode(data$BackPain
                         , "Yes" = 1
                         , "No" = 0)

unique(data$Allergic_rhinitis)
data$Allergic_rhinitis <- recode(data$Allergic_rhinitis
                                 , "Yes" = 1
                                 , "No" = 0)

unique(data$Reflux_esophagitis)
data$Reflux_esophagitis <- recode(data$Reflux_esophagitis
                                  , "Yes" = 1
                                  , "No" = 0)

unique(data$Asthma)
data$Asthma <- recode(data$Asthma
                      , "Yes" = 1
                      , "No" = 0)

unique(data$Services)
data$Services <- recode(data$Services
                        , "Blood Work" = 1
                        , "Intravenous" = 2
                        , "CT Scan" = 3
                        , "MRI" = 4)

unique(data$Soft_drink)
data$Soft_drink <- recode(data$Soft_drink
                          , "Yes" = 1
                          , "No" = 2)

sum(is.na(data))# Check
viz_miss(data)
write.csv(data, "performance review/clean_data.csv")
