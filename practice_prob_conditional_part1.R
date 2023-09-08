# Q1 ------------------------------------------------------------------------------------------
# In a previous module, we covered Bayes' theorem and the Bayesian paradigm.
# Conditional probabilities are a fundamental part of this previous covered rule.

# P(A|B) = P(B|A)*P(A) / P(B)

# We first review a simple example to go over conditional probabilities.

# Assume a patient comes into the doctor’s office to test whether they have a particular disease.

# The test is positive 85% of the time when tested on a patient with the disease (high sensitivity):
## P(test +| disease) = 0.85

# The test is negative 90% of the time when tested on a healthy patient (high specificity):
## P(test -| disease) = 0.85

# The disease is prevalent in about 2% of the community:
## P(disease) = 0.02

# Using Bayes' theorem, ""calculate the probability that you have the disease if the test is
# positive"". Enter your answer as a percentage or decimal (eg "50%" or "0.50") to at least the
# hundredths place

# set.seed(1) # if using R 3.5 or earlier

set.seed(1, sample.kind = "Rounding")

disease <- sample(c(0, 1),
                  size = 1e6,
                  replace = TRUE,
                  prob = c(0.98, 0.02))

test <- rep(NA, 1e6)

test[disease == 0] <-
  sample(
    c(0, 1),
    size = sum(disease == 0),
    replace = TRUE,
    prob = c(0.90, 0.10)
  )

test[disease == 1] <-
  sample(
    c(0, 1),
    size = sum(disease == 1),
    replace = TRUE,
    prob = c(0.15, 0.85)
  )

# Q2 ------------------------------------------------------------------------------------------
# What is the probability that a test is positive?

mean(test)

## [1] 0.114509 ou 11%

# Q3 ------------------------------------------------------------------------------------------
#What is the probability that an individual has the disease if the test is negative?

mean(disease[test==0])

## [1] 0.003461356 ou 0.03%

# Q4 ------------------------------------------------------------------------------------------
# What is the probability that you have the disease if the test is positive? Remember: calculate
# the conditional probability the disease is positive assuming a positive test.

mean(disease[test==1]==1)

## [1] 0.1471762 ou 14%

# Q5 ------------------------------------------------------------------------------------------
# Compare the prevalence of disease in people who test positive to the overall prevalence of
# disease.

# If a patient’s test is positive, by how many times does that increase their risk of having
# the disease? First calculate the probability of having the disease given a positive test,
# then divide by the probability of having the disease.

mean(disease[test==1]==1)/mean(disease==1)

## [1] 7.389106


