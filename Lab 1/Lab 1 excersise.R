#-------------------------------------------------------------------------------
# 1. name of charecter
#-------------------------------------------------------------------------------
name <-"Abhishek Pattir"
#NUMERIC TYPE
age <-20
#LOGICAL
isTRUE(age<22)
#-------------------------------------------------------------------------------
# 2.creating a vector
#-------------------------------------------------------------------------------
v<- c(1,2,3)
#-------------------------------------------------------------------------------
# 3.increment with 5
#-------------------------------------------------------------------------------
s<-seq(5,50,5)
print(s)
#-------------------------------------------------------------------------------
# 4.store 5 fruits in charecter vector and display 2nd and 4th fruit
#-------------------------------------------------------------------------------
fruits<-c("apple","grape","pineapple","orangee","pear")
fruits[c(2,4)]
#-------------------------------------------------------------------------------
# 5.Create a numeric vector of 10 random numbers between 1 and 100, then find:
#------------------------------------------------------------------------------
set.seed(1)
nums <- sample(1:100,10)
nums
#max
max(nums)
#min
min(nums)
#mean
mean(nums)
#-------------------------------------------------------------------------------
#6.Create a data frame with columns: Name, Age, Marks. Enter at least 5 records.
#-------------------------------------------------------------------------------
df<-data.frame(
  name<-c("n1","n2","n3","n4","n5"),
  age<-c(12,13,14,16,15),
  marks<-c(10,15,14,16,17)
 )
df
summary(df)
colnames(df)
#-------------------------------------------------------------------------------
# 7.Write code to sort the data frame by Marks in descending order.
#-------------------------------------------------------------------------------
dfs<-order(df$marks,decreasing = TRUE)
dfs
#-------------------------------------------------------------------------------
##2. operations in r
#-------------------------------------------------------------------------------
10 + 5
10 - 5
10 * 5
10 / 5
10 %% 3
10 %/% 3
#-------------------------------------------------------------------------------
# 2.COMPARE
#-------------------------------------------------------------------------------
15>10
7==7
#-------------------------------------------------------------------------------
# 3. OPERATIONS IN VECTOR
#-------------------------------------------------------------------------------
a <- c(2, 4, 6, 8)
b <- c(1, 3, 5, 7)
add<-a+b
print(add)
sub<-a-b
print(sub)
multy<-a*b
print(multy)
#-------------------------------------------------------------------------------
# 4.Use logical operators to check:
#-------------------------------------------------------------------------------
#Which elements of a are greater than 5?
a>5
#Which elements of b are less than or equal to 4?
b<=4
#-------------------------------------------------------------------------------
# 5.Use %in% to check if the number 5 exists in vector a.
#-------------------------------------------------------------------------------
5 %in% a
#------------------------------------------------------------------------------- 
# 6. Given x <- c(TRUE, FALSE, TRUE, FALSE) and y <- c(TRUE, TRUE, FALSE, FALSE), apply:
#-------------------------------------------------------------------------------
x <- c(TRUE, FALSE, TRUE, FALSE)
y <- c(TRUE, TRUE, FALSE, FALSE)
#&
x&y
#|
x|y
#!
!x
#-------------------------------------------------------------------------------
# # 3.Loops in R
#-------------------------------------------------------------------------------
# 1.Write a for loop to print numbers from 1 to 10.
#-------------------------------------------------------------------------------
for (i in 1:10) {
  print(i)
}
#-------------------------------------------------------------------------------
# 2.Write a while loop to sum numbers from 1 to 100.
#-------------------------------------------------------------------------------
sum <- 0
i <- 1

while (i <= 100) {
  sum <- sum + i
  i <- i + 1
}

print(sum)
#-------------------------------------------------------------------------------
# 3.Write a loop to print only even numbers between 1 and 50.
#-------------------------------------------------------------------------------
for (i in 1:50) {
  if (i %% 2 == 0) {
    print(i)
  }
}
#-------------------------------------------------------------------------------
# 4.Write a loop to print the multiplication table of 7.
#-------------------------------------------------------------------------------
for (i in 1:10) {
  cat("7 x", i, "=", 7 * i, "\n")
}
#-------------------------------------------------------------------------------
# 5.Create a loop to calculate the factorial of a given number n.
#-------------------------------------------------------------------------------
n <- 5    
factorial <- 1

for (i in 1:n) {
  factorial <- factorial * i
}

print(factorial) 
#-------------------------------------------------------------------------------
# 6.Write a nested loop to print a star pattern:
#-------------------------------------------------------------------------------
rows <- 4

for (i in 1:rows) {
  for (j in 1:i) {
    cat("*")
  }
  cat("\n")
}
#-------------------------------------------------------------------------------
# #4. CONDITIONALS IN R
#-------------------------------------------------------------------------------
# 1.Write an if statement to check if a number is positive or negative.
#-------------------------------------------------------------------------------
number <- 10
if (number > 0) {
  print("Number is positive")
} else if (number==0){
  print("Number is zero")  
} else {
  print("Number is non-positive (negative)")
}
#-------------------------------------------------------------------------------
# 2.Write an if-else statement to check if a given number is even or odd.
#-------------------------------------------------------------------------------
number <- 10
if (number%%2== 0) {
  print("Number is even")
} else {
  print("Number is odd")
}
#-------------------------------------------------------------------------------
# 3.Write a program to check if a given year is a leap year.
#-------------------------------------------------------------------------------
# Function to check for a leap year
is_leap_year <- function(year) {
  if ((year %% 4 == 0 && year %% 100 != 0) || year %% 400 == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Input year
input_year <- 2025

# Check if it's a leap year
if (is_leap_year(input_year)) {
  print(paste(input_year, "is a leap year."))
} else {
  print(paste(input_year, "is not a leap year."))
}
#-------------------------------------------------------------------------------
# 4.Take a numeric input for marks and print:
#-------------------------------------------------------------------------------
check_pass <- function(marks) {
  if (marks >= 40) {
    return("Pass")
  } else {
    return("Fail")
  }
}
input_marks <- 35
print(check_pass(input_marks))
#-------------------------------------------------------------------------------
# 5.sing nested if-else, assign grades:
#-------------------------------------------------------------------------------
marks <- 82

if (marks >= 90) {
  print("Grade: A")
} else if (marks >= 75) {
  print("Grade: B")
} else if (marks >= 60) {
  print("Grade: C")
} else {
  print("Fail")
}
#-------------------------------------------------------------------------------
# #5.Functions in R
#-------------------------------------------------------------------------------
# 1.Write a function add_numbers(a, b) to return the sum of two numbers.
#-------------------------------------------------------------------------------
add_numbers <- function(a, b) {
  return(a + b)
}

# Example:
add_numbers(5, 3)
#-------------------------------------------------------------------------------
# 2.Write a function square(n) to return the square of a number.
#-------------------------------------------------------------------------------
square <- function(n) {
  return(n * n)
}

# Example:
square(6)
#-------------------------------------------------------------------------------
# 3.Write a function to calculate the factorial of a number using recursion.
#-------------------------------------------------------------------------------
factorial_recursive <- function(n) {
  if (n == 0) {
    return(1)
  } else {
    return(n * factorial_recursive(n - 1))
  }
}

# Example:
factorial_recursive(5)
#-------------------------------------------------------------------------------
# 4.Write a function to check if a number is prime.
#-------------------------------------------------------------------------------
is_prime <- function(n) {
  if (n <= 1) {
    return(FALSE)
  }
  for (i in 2:(n - 1)) {
    if (n %% i == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

# Example:
is_prime(7)  
#-------------------------------------------------------------------------------
# 5.Write a function that takes a vector and returns:
#-------------------------------------------------------------------------------
vector_stats <- function(v) {
  mean_val <- mean(v)
  median_val <- median(v)
  sd_val <- sd(v)
  
  return(list(mean = mean_val, median = median_val, sd = sd_val))
}

# Example:
vector_stats(c(10, 20, 30, 40, 50))
#-------------------------------------------------------------------------------
# 6.Write a function that accepts a data frame and a column name, and returns the top 5 highest values in that column.
#-------------------------------------------------------------------------------
top_5_values <- function(df, column_name) {
  return(head(df[order(-df[[column_name]]), column_name], 5))
}

# Example:
data <- data.frame(name = c("A", "B", "C", "D", "E", "F"),
                   marks = c(75, 88, 45, 92, 67, 81))

top_5_values(data, "marks")

#-------------------------------------------------------------------------------
## 6.Data Analysis – Adult Census Dataset
#-------------------------------------------------------------------------------
# 1.Load the Adult dataset into R.
#-------------------------------------------------------------------------------
cols <- c("age","workclass","fnlwgt","education","education_num","martial_status","occupation","relationship","race","Sex","capital_gain","capital_loss","hours_per_week","native_country","income")

adult<- read.csv("C:/Users/abhis/OneDrive/Documents/TY sem 5/R programing/datasets_lab1/adult-data.txt",
                 header =FALSE, col.names=cols,
                 strip.white=TRUE, na.strings="?",
                 stringsAsFactors=FALSE)
#-------------------------------------------------------------------------------
# 2.display 10 rows
#-------------------------------------------------------------------------------
View(head(adult,10))
print(head(adult,10))
#-------------------------------------------------------------------------------
# 3.Find the structure of the dataset.
#-------------------------------------------------------------------------------
str(adult)
#-------------------------------------------------------------------------------
# 4.Find the average age of all individuals.
#-------------------------------------------------------------------------------
mean(adult$age, na.rm = TRUE)
#-------------------------------------------------------------------------------
# 5.Count how many individuals earn >50K and how many earn <=50K.
#-------------------------------------------------------------------------------
table(adult$income)
#-------------------------------------------------------------------------------
# 6.Find the most common occupation.
#-------------------------------------------------------------------------------
names(sort(table(adult$occupation), decreasing = TRUE))[1]
#-------------------------------------------------------------------------------
# 7.Calculate the average hours-per-week for people earning >50K vs <=50K.
#-------------------------------------------------------------------------------
tapply(adult$hours_per_week, adult$income, mean, na.rm = TRUE)
#-------------------------------------------------------------------------------
# 8.Create a bar chart showing the distribution of education levels. 
#-------------------------------------------------------------------------------
# Count education levels
edu_counts <- table(adult$education)

# Simple bar chart
barplot(edu_counts,
        main = "Distribution of Education Levels",
        xlab = "Education Level",
        ylab = "Count")
#-------------------------------------------------------------------------------
# 9.Find which native country has the highest percentage of people earning >50K.
#-------------------------------------------------------------------------------
adult$income <- as.character(adult$income)


total <- table(adult$native_country)


high_income <- tapply(adult$income == ">50K", adult$native_country, sum)


percentage <- (high_income / total) * 100


country_max <- names(which.max(percentage))
percent_max <- max(percentage, na.rm = TRUE)

cat("Country with highest % of >50K earners:", country_max, "(", round(percent_max, 2), "% )\n")
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# #7.Data Analysis – IPL Dataset (batting_bowling_ipl_bat.csv)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 1.Load the IPL dataset into R.
#-------------------------------------------------------------------------------
ipl <- read.csv("C:/Users/abhis/OneDrive/Documents/TY sem 5/R programing/datasets_lab1/batting_bowling_ipl_bat.csv",
                header = TRUE, strip.white = TRUE, stringsAsFactors = FALSE, na.strings = c("-", "NA", "?"))


ipl <- ipl[!is.na(ipl$Name) & ipl$Name != "", ]

numeric_cols <- setdiff(names(ipl), "Name")   # all except Name

for (col in numeric_cols) {
  ipl[[col]] <- suppressWarnings(as.numeric(ipl[[col]]))
}

#-------------------------------------------------------------------------------
# 2.First 10 rows
#-------------------------------------------------------------------------------
head(ipl, 10)
#-------------------------------------------------------------------------------
# 3.Top 5 players with highest runs
#-------------------------------------------------------------------------------
total_runs <- tapply(ipl$Runs, ipl$Name, sum, na.rm = TRUE)
top5_runs <- sort(total_runs, decreasing = TRUE)[1:5]
top5_runs
#-------------------------------------------------------------------------------
# 4.Player with highest batting average
#-------------------------------------------------------------------------------
avg_runs <- tapply(ipl$Ave, ipl$Name, mean, na.rm = TRUE)
best_avg_player <- names(which.max(avg_runs))
best_avg_value  <- max(avg_runs, na.rm = TRUE)
cat("Player with highest batting average:", best_avg_player, "(", best_avg_value, ")\n")
#-------------------------------------------------------------------------------
# 5.Bar chart of top 10 players by strike rate
#-------------------------------------------------------------------------------
sr <- tapply(ipl$SR, ipl$Name, mean, na.rm = TRUE)
top10_sr <- sort(sr, decreasing = TRUE)[1:10]

barplot(top10_sr,
        main = "Top 10 Players by Strike Rate",
        ylab = "Strike Rate")
#-------------------------------------------------------------------------------
# 6.Correlation between Fours hit and Runs scored
#-------------------------------------------------------------------------------
ipl_clean <- ipl[!is.na(ipl$Name) & ipl$Name != "", ]

ipl_clean$Runs  <- suppressWarnings(as.numeric(ipl_clean$Runs))
ipl_clean$Fours <- suppressWarnings(as.numeric(ipl_clean$Fours))

fours <- tapply(ipl_clean$Fours, ipl_clean$Name, sum, na.rm = TRUE)
runs  <- tapply(ipl_clean$Runs,  ipl_clean$Name, sum, na.rm = TRUE)

common_players <- intersect(names(fours), names(runs))
fours <- fours[common_players]
runs  <- runs[common_players]

correlation <- cor(fours, runs, use = "complete.obs")
cat("Correlation between Fours hit and Runs scored:", correlation, "\n")

