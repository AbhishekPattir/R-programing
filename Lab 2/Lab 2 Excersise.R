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
