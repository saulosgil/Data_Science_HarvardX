# Vectorization and functionals ---------------------------------------------------------------
# Using sapply of R Base to apply a functions in each element of a vector

# SINTAXE: sapply(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)

# Each element of x is passed on to the function sqrt and the result is returned. These results are
# concatenated. In this case, the result is a vector of the same length as the original x.

# Example

x <- 1:10

sapply(x, sqrt)



