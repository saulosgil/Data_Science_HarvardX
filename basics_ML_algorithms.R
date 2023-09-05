# packages ------------------------------------------------------------------------------------
library(tidyverse)
library(caret)
library(dslabs)

# data ----------------------------------------------------------------------------------------
data(heights)

# define the outcome and predictors
y <- heights$sex
x <- heights$height

# generate training and test sets
set.seed(2007)

test_index <- createDataPartition(y, times = 1,
                                  p = 0.5,
                                  list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

# guess the outcome
y_hat <- sample(c("Male", "Female"),
                length(test_index),
                replace = TRUE) |>
  factor(levels = levels(test_set$sex))

# compute accuracy
mean(y_hat == test_set$sex)

# compare heights in males and females in our data set
heights |>
  group_by(sex) |>
  summarize(mean = mean(height),
            SD = sd(height))

# now try predicting "male" if the height is within 2 SD of the average male
y_hat <- ifelse(x > 62, "Male", "Female") |>
  factor(levels = levels(test_set$sex))

mean(y == y_hat)

# examine the accuracy of 10 cutoffs
cutoff <- seq(61, 70)

accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") |>
    factor(levels = levels(test_set$sex))

  mean(y_hat == train_set$sex)
})

data.frame(cutoff, accuracy) |>
  ggplot(aes(cutoff, accuracy)) +
  geom_point() +
  geom_line()

max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") |>
  factor(levels = levels(test_set$sex))

y_hat <- factor(y_hat)

mean(y_hat == test_set$sex)

# Confusion matrix ----------------------------------------------------------------------------
# tabulate each combination of prediction and actual value
table(predicted = y_hat,
      actual = test_set$sex)

test_set |>
  mutate(y_hat = y_hat) |>
  group_by(sex) |>
  summarize(accuracy = mean(y_hat == sex))

prev <- mean(y == "Male")

caret::confusionMatrix(data = y_hat, reference = test_set$sex)

# Sensitivity and Specificity -----------------------------------------------------------------
# get the metrics from Confusion matrix

cm <- caret::confusionMatrix(data = y_hat, reference = test_set$sex)

# access specific metrics
cm$overall["Accuracy"]

cm$byClass[c("Sensitivity","Specificity", "Prevalence")]

# Optimization --------------------------------------------------------------------------------

# For optimization purposes, sometimes it is more useful to have a one number summary than
# studying both specificity and sensitivity. One preferred metric is balanced accuracy.
# Because specificity and sensitivity are rates, it is more appropriate to compute the harmonic
# average. In fact, the F1-score, a widely used one-number summary, is the harmonic average of
# precision and recall.

# Depending on the context, some type of errors are more costly than others. The F1-score can be
# adapted to weigh specificity and sensitivity differently.

# You can compute the F1-score using the F_meas() function in the caret package.

# maximize F-score
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") |>
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

data.frame(cutoff, F_1) |>
  ggplot(aes(cutoff, F_1)) +
  geom_point() +
  geom_line()

max(F_1)

best_cutoff_2 <- cutoff[which.max(F_1)]
best_cutoff_2

y_hat <- ifelse(test_set$height > best_cutoff_2, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))

mean(y_hat == test_set$sex)

sensitivity(data = y_hat, reference = test_set$sex)

specificity(data = y_hat, reference = test_set$sex)

# ROC and precision-recall curves -------------------------------------------------------------
p <- 0.9

n <- length(test_index)

y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) |>
  factor(levels = levels(test_set$sex))

mean(y_hat == test_set$sex)

# ROC curve
probs <- seq(0, 1, length.out = 10)

guessing <- map_df(probs, function(p){
  y_hat <-
    sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) |>
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

qplot(FPR, TPR, data = guessing, xlab = "1 - Specificity", ylab = "Sensitivity")

cutoffs <- c(50, seq(60, 75), 80)

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>%
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

# plot both curves together
bind_rows(guessing, height_cutoff) |>
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

# Package for labeled curve
library(ggrepel)

map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") |>
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       cutoff = x,
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) |>
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)

# plot precision against recall
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index),
                  replace = TRUE, prob=c(p, 1-p)) |>
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") |>
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

bind_rows(guessing, height_cutoff) |>
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()

guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE,
                  prob=c(p, 1-p)) |>
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") |>
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

bind_rows(guessing, height_cutoff) |>
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()
