library(nloptr)

# set up a stepsize
alpha <- 0.003
# set up a number of iteration
iter <- 500
# define the gradient of f(x) = x^4 - 3*x^3 + 2
gradient <- function(x) return((4*x^3) - (9*x^2))
# randomly initialize a value to x
set.seed(100)
x <- floor(runif(1)*10)
# create a vector to contain all xs for all steps
x.All <- vector("numeric",iter)
