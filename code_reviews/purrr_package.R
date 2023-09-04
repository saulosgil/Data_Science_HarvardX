# The purrr package ---------------------------------------------------------------------------

# We learned about the sapply function, which permitted us to apply the same function to each
# element of a vector. We constructed a function and used sapply to compute the sum of the first n
# integers for several values of n like this:

compute_s_n <- function(n){
  x <- 1:n
  sum(x)
  }

n <- 1:25

s_n <- sapply(n, compute_s_n)

# This type of operation, applying the same function or procedure to elements of an object, is quite
# common in data analysis. The purrr package includes functions similar to sapply but that better
# interact with other tidyverse functions. The main advantage is that we can better control the
# output type of functions. In contrast, sapply can return several different object types;
# for example, we might expect a numeric result from a line of code, but sapply might convert our
# result to character under some circumstances. purrr functions will never do this: they will return
# objects of a specified type or return an error if this is not possible.

# The first purrr function we will learn is map, which works very similar to sapply but always,
# without exception, returns a list:

library(purrr)

s_n <- map(n, compute_s_n)

class(s_n)

# Result is [1] "list"

# If we want a numeric vector, we can instead use map_dbl which always returns a vector of numeric
# values.

s_n <- map_dbl(n, compute_s_n)

class(s_n)

# Result is [1] "numeric"

# This produces the same results as the sapply call shown in the script vectorization_functionals.

# The purrr package provides much more functionality not covered here. For more details you can
# consult this online resource.
