data(iris)

names(iris)

min_val <- min(iris$Sepal.Length, na.rm = TRUE)
max_val <- max(iris$Sepal.Length, na.rm = TRUE)

cat("Min Sepal.Length:", min_val, "\n")
cat("Max Sepal.Length:", max_val, "\n")
