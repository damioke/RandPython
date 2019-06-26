
# Set seed to make your analysis reproducible
setwd("C:/Users/test/projects-jupyter/RandPython")

# set seed
set.seed(1)
set.seed(111)


#install.packages("readr") # data parsing
#install.packages("purrr") # data mapping
#install.packages("dplyr") # Data analysis
#install.packages("GGally") #  extends 'ggplot2' by adding several functions
#install.packages("cluster") # cluster analysis
#install.packages("randomForest") # random forest algorithm

library(readr)
library(purrr)
library(dplyr)
library(GGally)
library(cluster)
library(randomForest)

# Importing a CSV
nba <- read_csv("nba_2013.csv")

# Finding the number of rows
dim(nba)

# Looking at the first row of the data
head(nba, 1)

# Find the average of each statistic
nba %>%
  select_if(is.numeric) %>%
  map_dbl(mean, na.rm = TRUE)

# Make pairwise scatterplots
nba %>%
  select(ast, fg, trb) %>%
  ggpairs()

# Make clusters of the players => showIing which players are most similar.
isGoodCol <- function(col){
  sum(is.na(col)) == 0 && is.numeric(col)
}
goodCols <- sapply(nba, isGoodCol)
clusters <- kmeans(nba[,goodCols], centers=5)
labels <- clusters$cluster

# Plot players by cluster
nba2d <- prcomp(nba[,goodCols], center=TRUE)
twoColumns <- nba2d$x[,1:2]
clusplot(twoColumns, labels)


# Split into training and testing sets
trainRowCount <- floor(0.8 * nrow(nba))
trainIndex <- sample(1:nrow(nba), trainRowCount)
train <- nba[trainIndex,]
test <- nba[-trainIndex,]

# Univariate linear regression: Let's say we want to predict number of assists per player from field goals made per player.
fit <- lm(ast ~ fg, data=train)
predictions <- predict(fit, test)

# Calculate summary statistics for the model
summary(fit)

# Fit a random forest model
predictorColumns <- c("age", "mp", "fg", "trb", "stl", "blk")
rf <- randomForest(train[predictorColumns], train$ast, ntree=100)
predictions <- predict(rf, test[predictorColumns])

# Calculate error: Now that we've fit two models, let's calculate error. We'll use MSE.
mean((test$ast - predictions)^2)