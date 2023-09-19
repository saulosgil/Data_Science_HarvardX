# packages ------------------------------------------------------------------------------------
library(tidyverse)
library(caret)

# data ----------------------------------------------------------------------------------------
set.seed(1, sample.kind = "Rounding")

n <- 100

Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)

dat <-
  MASS::mvrnorm(n = 100,
                mu = c(69, 69),
                Sigma = Sigma) |>
  data.frame() |>
  setNames(c("x", "y"))

summary(dat)

# Q1 ------------------------------------------------------------------------------------------
# We will build 100 linear models using the data above and calculate the mean and standard
# deviation of the combined models. First, set the seed to 1 again. Then, within a replicate
# () loop, (1) partition the dataset into test and training sets with p = 0.5 and using dat$y
# to generate your indices, (2) train a linear model predicting y from x, (3) generate
# predictions on the test set, and (4) calculate the RMSE of that model. Then, report the
# mean and standard deviation (SD) of the RMSEs from all 100 models.

# Report all answers to at least 3 significant digits.

set.seed(1, sample.kind="Rounding")

rmse <- replicate(100, {
  # separete in train and test dataset
  test_index <-
    createDataPartition(dat$y, times = 1,
                        p = 0.5,
                        list = FALSE)
  # train dataset
  train_set <- dat |>
    slice(-test_index)

  # test dataset
  test_set <- dat |>
    slice(test_index)

  # model
  fit <- lm(y ~ x, data = train_set)

  # predictions
  y_hat <- predict(fit, newdata = test_set)

  #rmse
  sqrt(mean((y_hat-test_set$y)^2))
})

# whats is the mean?
mean(rmse)

# whats is the SD?
sd(rmse)

# Q2 ------------------------------------------------------------------------------------------
# Now we will repeat the exercise above but using larger datasets. Write a function that takes
# a size n, then (1) builds a dataset using the code provided at the top of Q1 but with n
# observations instead of 100 and without the set.seed(1), (2) runs the replicate() loop that
# you wrote to answer Q1, which builds 100 linear models and returns a vector of RMSEs,
# and (3) calculates the mean and standard deviation of the 100 RMSEs.
#
# Set the seed to 1 and then use sapply() or map() to apply your new function to
# n <- c(100, 500, 1000, 5000, 10000).
#
# Note: You only need to set the seed once before running your function; do not set a seed
# within your function. Also be sure to use sapply() or map() as you will get different
# answers running the simulations individually due to setting the seed.

set.seed(1, sample.kind="Rounding")

# sample size
n <- c(100, 500, 1000, 5000, 10000)

res <- sapply(n, function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = n,
                       mu = c(69, 69),
                       Sigma = Sigma) |>
    data.frame() |>
    setNames(c("x", "y"))
  # RMSE function
  rmse <- replicate(100, {
    # separete in train and test dataset
    test_index <-
      createDataPartition(dat$y, times = 1,
                          p = 0.5,
                          list = FALSE)
    # train dataset
    train_set <- dat |>
      slice(-test_index)

    # test dataset
    test_set <- dat |>
      slice(test_index)

    # model
    fit <- lm(y ~ x, data = train_set)

    # predictions
    y_hat <- predict(fit, newdata = test_set)

    #rmse
    sqrt(mean((y_hat-test_set$y)^2))
  })
  c(avg = mean(rmse),
    sd = sd(rmse))
})

res

# Q3 ------------------------------------------------------------------------------------------
# What happens to the RMSE as the size of the dataset becomes larger?

# On average, the RMSE does not change much as n gets larger, but the variability of the
# RMSE decreases.
set.seed(1, sample.kind = "Rounding")

n <- 100

Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)

dat <-
  MASS::mvrnorm(n = 100,
                mu = c(69, 69),
                Sigma = Sigma) |>
  data.frame() |>
  setNames(c("x", "y"))

set.seed(1, sample.kind="Rounding")

# function to replicate RMSE
rmse <- replicate(100, {
  # separete in train and test dataset
  test_index <-
    createDataPartition(dat$y, times = 1,
                        p = 0.5,
                        list = FALSE)
  # train dataset
  train_set <- dat |>
    slice(-test_index)

  # test dataset
  test_set <- dat |>
    slice(test_index)

  # model
  fit <- lm(y ~ x, data = train_set)

  # predictions
  y_hat <- predict(fit, newdata = test_set)

  #rmse
  sqrt(mean((y_hat-test_set$y)^2))
})

# whats is the mean?
mean(rmse)

# whats is the SD?
sd(rmse)

# Q6 ------------------------------------------------------------------------------------------
# Create a data set using the following code.
set.seed(1, sample.kind = "Rounding")

Sigma <- 9*matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)

dat <-
  MASS::mvrnorm(n = 100,
                mu = c(0, 0, 0),
                Sigma = Sigma) |>
  data.frame() |>
  setNames(c("y", "x_1", "x_2"))

# Note that y is correlated with both x_1 and x_2 but the two predictors are independent of each other
cor(dat)

# Set the seed to 1, then use the caret package to partition into test and training sets with
# p = 0.5. Compare the RMSE when using just x_1, just x_2 and both x_1 and x_2.
# Train a single linear model for each (not 100 like in the previous questions).

set.seed(1, sample.kind = "Rounding")

# separating dataset
test_index <-
  createDataPartition(dat$y, times = 1,
                      p = 0.5,
                      list = FALSE)
# train dataset
train_set <- dat |>
  slice(-test_index)

# test dataset
test_set <- dat |>
  slice(test_index)

# model with x_1
fit <- lm(y ~ x_1, data = train_set) # model
y_hat <- predict(fit, newdata = test_set) # predictions
sqrt(mean((y_hat-test_set$y)^2)) #rmse

# model with x_2
fit <- lm(y ~ x_2, data = train_set) # model
y_hat <- predict(fit, newdata = test_set) # predictions
sqrt(mean((y_hat-test_set$y)^2)) # rmse

# model with x_1 and x_2
fit <- lm(y ~ x_1 + x_2, data = train_set) # model
y_hat <- predict(fit, newdata = test_set) # predictions
sqrt(mean((y_hat-test_set$y)^2)) # rmse

# Which of the three models performs the best (has the lowest RMSE)?
# lm(y ~ x_1 + x_2, data = train_set

# Q8 ------------------------------------------------------------------------------------------
# Repeat the exercise from Q6 but now create an example in which x_1 and x_2 are highly correlated.

set.seed(1, sample.kind = "Rounding")

Sigma <- 9*matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)

dat <-
  MASS::mvrnorm(n = 100,
                mu = c(0, 0, 0),
                Sigma = Sigma) |>
  data.frame() |>
  setNames(c("y", "x_1", "x_2"))

# separating dataset
test_index <-
  createDataPartition(dat$y, times = 1,
                      p = 0.5,
                      list = FALSE)
# train dataset
train_set <- dat |>
  slice(-test_index)

# test dataset
test_set <- dat |>
  slice(test_index)

# model with x_1
fit <- lm(y ~ x_1, data = train_set) # model
y_hat <- predict(fit, newdata = test_set) # predictions
sqrt(mean((y_hat-test_set$y)^2)) #rmse

# model with x_2
fit <- lm(y ~ x_2, data = train_set) # model
y_hat <- predict(fit, newdata = test_set) # predictions
sqrt(mean((y_hat-test_set$y)^2)) # rmse

# model with x_1 and x_2
fit <- lm(y ~ x_1 + x_2, data = train_set) # model
y_hat <- predict(fit, newdata = test_set) # predictions
sqrt(mean((y_hat-test_set$y)^2)) # rmse



