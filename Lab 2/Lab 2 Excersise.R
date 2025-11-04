# #1.Accessing Various File Types
#-------------------------------------------------------------------------------
# 1. Write R code to:
# Load a CSV file (dataset_Facebook.csv) into a dataframe.
# Load an Excel file (LungCap_Dataset.xls) into a dataframe.
# Load a text file separated by commas.
#-------------------------------------------------------------------------------
#Load dataset facebook csv
facebook_data <- read.csv("C:/Users/abhis/OneDrive/Documents/TY sem 5/R programing/datasets_lab1/dataset_Facebook.csv",sep = ";",
                          header = TRUE, stringsAsFactors = FALSE)
#Load excel file
library(readxl)
lungcap_data <- read_excel("C:/Users/abhis/OneDrive/Documents/TY sem 5/R programing/datasets_lab1/LungCap_Dataset.xls")

#load another file
titanic_data <- read.csv("C:/Users/abhis/OneDrive/Documents/TY sem 5/R programing/datasets_lab1/train_titanic.csv",
                         header = TRUE, stringsAsFactors = FALSE)
#-------------------------------------------------------------------------------
# 2.Display the first 10 rows of each dataset using head().
#-------------------------------------------------------------------------------
head(facebook_data, 10)
head(lungcap_data, 10)
head(titanic_data, 10)
#-------------------------------------------------------------------------------
# 3.Check the data type of each column in the Titanic dataset.
#-------------------------------------------------------------------------------
titanic_df <- read.csv("C:/Users/abhis/OneDrive/Documents/TY sem 5/R programing/datasets_lab1/train_titanic.csv")
str(titanic_df)
#-------------------------------------------------------------------------------
# 4.Save the Titanic dataset into a new CSV file after filtering only passengers who survived.
#-------------------------------------------------------------------------------
survived_df <- subset(titanic_df, Survived == 1)
write.csv(survived_df, "titanic_survived.csv", row.names=FALSE)
#-------------------------------------------------------------------------------
# #2.Data Selection & Manipulation
#-------------------------------------------------------------------------------
# A.From the Titanic dataset:
#-------------------------------------------------------------------------------
# Select columns
selected_titanic <- titanic_df[, c("Name", "Sex", "Age", "Survived")]
# Passengers older than 50
older_passengers <- subset(selected_titanic, Age > 50)
# Count survivors by Pclass
table(titanic_df$Survived, titanic_df$Pclass)
#-------------------------------------------------------------------------------
# B.From the Facebook dataset:
#-------------------------------------------------------------------------------
# Find the post with the maximum likes
max_likes_post <- facebook_data[which.max(facebook_data$like), ]
# Calculate the average shares per post
mean_shares <- mean(facebook_data$share, na.rm = TRUE)
# Create an Engagement column
facebook_data$Engagement <- facebook_data$like + facebook_data$comment + facebook_data$share
#-------------------------------------------------------------------------------
# c.Using the Lung Capacity dataset:
#-------------------------------------------------------------------------------
#Select children below age 12.
#-------------------------------------------------------------------------------
head(lungcap_data)
children <- lungcap_data[lungcap_data$`Age( years)` < 12, ]
print(children)
#-------------------------------------------------------------------------------
# Group by Gender and calculate average Lung Capacity
#-------------------------------------------------------------------------------
library(dplyr)

avg_lungcap_by_gender <- lungcap_data %>%
  group_by(Gender) %>%
  summarise(mean_LungCap = mean(`LungCap(cc)`, na.rm = TRUE))

print(avg_lungcap_by_gender)
#-------------------------------------------------------------------------------
# # 3.Data Manipulation (Using dplyr/base R)
#-------------------------------------------------------------------------------
# a) Rename the columns of Titanic dataset to lowercase.
#-------------------------------------------------------------------------------
colnames(titanic_data) <- tolower(colnames(titanic_data))
#-------------------------------------------------------------------------------
# b) Sort the Titanic dataset by Age in descending order.
#-------------------------------------------------------------------------------
sorted_titanic <- titanic_data[order(-titanic_data$age), ]
#-------------------------------------------------------------------------------
# c) Create AgeGroup column
#-------------------------------------------------------------------------------
titanic_data$agegroup <- cut(titanic_data$age, breaks = c(-Inf, 12, 18, 59, Inf),
                             labels = c("Child", "Teen", "Adult", "Senior"))
#-------------------------------------------------------------------------------
# d) Mean Fare by Pclass and Survived
#-------------------------------------------------------------------------------
aggregate(fare ~ pclass + survived, data = titanic_data, FUN = mean, na.rm = TRUE)
#-------------------------------------------------------------------------------
# e) Facebook: Average likes by post type
#-------------------------------------------------------------------------------
facebook_data %>%
  group_by(Type) %>%
  summarise(avg_likes = mean(like, na.rm = TRUE))
#-------------------------------------------------------------------------------
# # 4.Handling Missing Values
#-------------------------------------------------------------------------------
# a)Identify the columns with missing values in the Titanic dataset.
#-------------------------------------------------------------------------------
colnames(titanic_data)[colSums(is.na(titanic_data)) > 0]
#-------------------------------------------------------------------------------
# b)Replace missing Age values with the median Age.
#------------=------------------------------------------------------------------
titanic_data$age[is.na(titanic_data$age)] <- median(titanic_data$age, na.rm = TRUE)
#-------------------------------------------------------------------------------
# C)Drop rows where Embarked is missing.
#-------------------------------------------------------------------------------
titanic_data <- titanic_data[!is.na(titanic_data$embarked), ]
#-------------------------------------------------------------------------------
# D) For Lung Capacity dataset, fill missing values of LungCap with the mean LungCap.
#-------------------------------------------------------------------------------
# Calculate the mean of LungCap(cc), ignoring NA values
mean_lungcap <- mean(lungcap_data$`LungCap(cc)`, na.rm = TRUE)

# Replace NA values with the mean for LungCap(cc)
lungcap_data$`LungCap(cc)`[is.na(lungcap_data$`LungCap(cc)`)] <- mean_lungcap

# Print to confirm replacement
print(head(lungcap_data$`LungCap(cc)`))
#-------------------------------------------------------------------------------
# #5. Exploratory Data Analysis (EDA)
#-------------------------------------------------------------------------------
# A) Draw a histogram of Age (Titanic dataset).
#-------------------------------------------------------------------------------
hist(titanic_data$age, main = "Age Distribution", xlab = "Age") # Pop-up window in R
#-------------------------------------------------------------------------------
# B) Create a bar chart of Pclass vs. count of passengers.
#-------------------------------------------------------------------------------
barplot(table(titanic_data$pclass), main = "Count by Passenger Class", xlab = "Class", ylab = "Count")
#-------------------------------------------------------------------------------
# C) Plot a boxplot of LungCap by Gender.
#-------------------------------------------------------------------------------
boxplot(`LungCap(cc)` ~ Gender, data = lungcap_data, main = "LungCap by Gender")
#-------------------------------------------------------------------------------
# D) In Facebook dataset:
#-------------------------------------------------------------------------------
plot(facebook_data$like, facebook_data$comment, xlab = "Likes", ylab = "Comments", main = "Likes vs Comments Scatter")
hist(facebook_data$share, main = "Shares Distribution", xlab = "Shares")
#-------------------------------------------------------------------------------
# E)For Titanic dataset, use a pie chart to show the proportion of survivors vs non-survivors.
#-------------------------------------------------------------------------------
pie(table(titanic_data$survived), labels = c("Did Not Survive", "Survived"), main = "Survival Proportion")
#-------------------------------------------------------------------------------
# # 6.Detecting and Handling Outliers
#-------------------------------------------------------------------------------
# A) Using the Titanic dataset
#Create a boxplot of Fare and visually identify potential outliers.
boxplot(titanic_data$fare, main = "Fare Boxplot")
#Create a boxplot of Age separated by Survived (use boxplot(Age ~ Survived, data=Titanic) in R).
boxplot(titanic_data$age ~ titanic_data$survived, main = "Age by Survival")
# B)Using the Facebook dataset:
# Facebook: Likes boxplot
boxplot(facebook_data$like, main = "Likes Boxplot")
# Facebook: Combined Likes, Shares, Comments boxplot
boxplot(facebook_data[, c("like","share","comment")], main = "Likes, Shares, Comments")
#-------------------------------------------------------------------------------
# #7. IQR Method
# C)# Titanic Fare IQR
Q1 <- quantile(titanic_data$fare, 0.25, na.rm = TRUE)
Q3 <- quantile(titanic_data$fare, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1
lower <- Q1 - 1.5 * IQR_val
upper <- Q3 + 1.5 * IQR_val
outliers_fare_count <- sum(titanic_data$fare < lower | titanic_data$fare > upper, na.rm=TRUE)
print(paste("Number of Fare Outliers:", outliers_fare_count))

# Titanic Age IQR
Q1_age <- quantile(titanic_data$age, 0.25, na.rm = TRUE)
Q3_age <- quantile(titanic_data$age, 0.75, na.rm = TRUE)
IQR_age <- Q3_age - Q1_age
lower_age <- Q1_age - 1.5 * IQR_age
upper_age <- Q3_age + 1.5 * IQR_age
outliers_age_count <- sum(titanic_data$age < lower_age | titanic_data$age > upper_age, na.rm=TRUE)
print(paste("Number of Age Outliers:", outliers_age_count))

# Capping outliers (replace by nearest boundary)
titanic_data$fare <- ifelse(titanic_data$fare < lower, lower, ifelse(titanic_data$fare > upper, upper, titanic_data$fare))
titanic_data$age <- ifelse(titanic_data$age < lower_age, lower_age, ifelse(titanic_data$age > upper_age, upper_age, titanic_data$age))
print("Capped Fare and Age Outliers. First few values:")
print(head(titanic_data[, c("fare", "age")], 10))

# LungCap outlier detection and removal using correct column name with backticks
Q1_lc <- quantile(lungcap_data$`LungCap(cc)`, 0.25, na.rm = TRUE)
Q3_lc <- quantile(lungcap_data$`LungCap(cc)`, 0.75, na.rm = TRUE)
IQR_lc <- Q3_lc - Q1_lc
lower_lc <- Q1_lc - 1.5 * IQR_lc
upper_lc <- Q3_lc + 1.5 * IQR_lc
is_lc_outlier <- lungcap_data$`LungCap(cc)` < lower_lc | lungcap_data$`LungCap(cc)` > upper_lc
avg_before <- mean(lungcap_data$`LungCap(cc)`, na.rm = TRUE)
lungcap_no_outliers <- lungcap_data[!is_lc_outlier, ]
avg_after <- mean(lungcap_no_outliers$`LungCap(cc)`, na.rm = TRUE)
print(paste("Average Lung Capacity before outlier removal:", avg_before))
print(paste("Average Lung Capacity after outlier removal:", avg_after))

