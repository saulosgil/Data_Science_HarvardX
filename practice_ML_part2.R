# packages ------------------------------------------------------------------------------------
library(caret)

# data
data(iris)
glimpse(iris)


iris <- iris[-which(iris$Species=='setosa'),]
glimpse(iris)

# outcome
y <- iris$Species

# Q7 ------------------------------------------------------------------------------------------

# First let us create an even split of the data into train and test partitions using
# createDataPartition() from the caret package.

test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)

set.seed(76)

# line of code
test <- iris[test_index,]

train <- iris[-test_index,]

# Q8 ------------------------------------------------------------------------------------------
# Next we will figure out the singular feature in the dataset that yields the greatest overall
# accuracy when predicting species. You can use the code from the introduction and from Q7 to start
# your analysis.

# Using only the train iris dataset, for each feature, perform a simple search to find the cutoff
# that produces the highest accuracy, predicting virginica if greater than the cutoff and versicolor
# otherwise. Use the seq function over the range of each feature by intervals of 0.1 for this search.

# Which feature produces the highest accuracy?
foo <- function(x){
  rangedValues <- seq(range(x)[1], range(x)[2], by=0.1)
  sapply(rangedValues, function(i){
    y_hat <- ifelse(x > i, 'virginica', 'versicolor')
    mean(y_hat==train$Species)
  })
}

predictions <- apply(train[,-5], 2, foo)

sapply(predictions, max)

# Q9 ------------------------------------------------------------------------------------------
# For the feature selected in Q8, use the smart cutoff value from the training data to calculate
# overall accuracy in the test data.

# What is the overall accuracy?

predictions <- foo(train[,4])

rangedValues <- seq(range(train[,4])[1], range(train[,4])[2], by=0.1)

cutoffs <-rangedValues[which(predictions==max(predictions))]

y_hat <- ifelse(test[,4]>cutoffs[1], 'virginica', 'versicolor')

mean(y_hat==test$Species)

# Q10------------------------------------------------------------------------------------------
# Notice that we had an overall accuracy greater than 90% in the training data, but the overall
# accuracy was lower in the test data. This can happen often if we overtrain. In fact, it could be
# the case that a single feature is not the best choice. For example, a combination of features might
# be optimal. Using a single feature and optimizing the cutoff as we did on our training data can
# lead to overfitting.

# To consider which other features could be helpful to add, we can repeat the analysis from Q8.

# Which feature produces the second highest accuracy?

foo <- function(x) {
  rangedValues <- seq(range(x)[1], range(x)[2], by = 0.1)
  sapply(rangedValues, function(i) {
    y_hat <- ifelse(x > i, 'virginica', 'versicolor')
    mean(y_hat == test$Species)
  })
}

predictions <- apply(test[, -5], 2, foo)

sapply(predictions, max)

# Q11 -----------------------------------------------------------------------------------------
# Now we will perform some exploratory data analysis on the data.

plot(iris, pch=21, bg=iris$Species)

# Notice that Petal.Length and Petal.Width in combination could potentially be more information than
# either feature alone.

# Optimize the the cutoffs for Petal.Length and Petal.Width separately in the train dataset by using
# the seq function with increments of 0.1. Then, report the overall accuracy when applied to the test
# dataset by creating a rule that predicts virginica if Petal.Length is greater than the length cutoff
# AND Petal.Width is greater than the width cutoff, and versicolor otherwise.

# What is the overall accuracy for the test data now?

set.seed(2, sample.kind="Rounding") # if using R 3.6 or later

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

# training and test dataset
test <- iris[test_index,]

train <- iris[-test_index,]

petalLengthRange <-
  seq(range(train$Petal.Length)[1],
      range(train$Petal.Length)[2], by = 0.1)

petalWidthRange <-
  seq(range(train$Petal.Width)[1],
      range(train$Petal.Width)[2], by = 0.1)

# Algorithms
length_predictions <- sapply(petalLengthRange, function(i) {
  y_hat <- ifelse(train$Petal.Length > i, 'virginica', 'versicolor')
  mean(y_hat == train$Species)
})

length_cutoff <-
  petalLengthRange[which.max(length_predictions)] # 4.6

width_predictions <- sapply(petalWidthRange, function(i) {
  y_hat <- ifelse(train$Petal.Width > i, 'virginica', 'versicolor')
  mean(y_hat == train$Species)
})

width_cutoff <- petalWidthRange[which.max(width_predictions)] # 1.5

y_hat <-
  ifelse(
    test$Petal.Length > length_cutoff |
      test$Petal.Width > width_cutoff,
    'virginica',
    'versicolor'
  )

mean(y_hat == test$Species)



























