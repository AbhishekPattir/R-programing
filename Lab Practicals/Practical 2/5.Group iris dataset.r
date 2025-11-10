data(iris)

aggregate(Sepal.Width ~ Species, data = iris, FUN = mean)

library(dplyr)

iris %>%
  group_by(Species) %>%
  summarise(avg_sepal_width = mean(Sepal.Width, na.rm = TRUE))