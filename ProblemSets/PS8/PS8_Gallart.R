library(nloptr)

# 4a set the seed
set.seed(100)

# 4b create X matrix
N <- 100000
K <- 10
sigma <- 0.5
X <- matrix(rnorm(N*K, mean = 0, sd = sigma), nrow = N, ncol = K)
X[,1] <- 1


# 4c create eps vector
eps <- rnorm(N,0, sigma^2)

# 4d create beta vector
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

# 4e create Y vector
Y <- X %*% beta + eps

# ----------------------------------------------
# 5 compute OLS estimate of beta
# ----------------------------------------------
XtX <- t(X) %*% X
XtY <- t(X) %*% Y
beta_hat_ols <- solve(XtX) %*% XtY
beta_hat_ols #the values of this estimate are very close to the true values of beta, off by approx 0.01



# ----------------------------------------------
# 6 compute beta_hat_ols using Gradient Descent
# ----------------------------------------------
alpha <- 0.0000003 # step size
maxiter <- 500000 # iterations

## objective function
objfun <- function(beta,Y,X) {
  return ( sum((Y-X%*%beta)^2) )
}

## define the gradient of our objective function
gradient <- function(beta,Y,X) {
  return (as.vector(-2*t(X)%*%(Y-X%*%beta)))
}

gradient(beta, Y, X)

## gradient descent method to find the minimum
iter  <- 1
beta0 <- 0*beta
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta0 <- beta
  betaa <- beta0 - alpha*gradient(beta0,Y,X)
  beta.All[,iter] <- betaa
  if (iter%%10000==0) {
    print(betaa)
  }
  iter <- iter+1
}

betaa # this is the beta_hat_ols from gradient descent

# ----------------------------------------------
# 7a compute beta hat ols using nloptr's L-BFGS alg
# ----------------------------------------------

# uses the same objfun and gradiemt function  as 6

## initial values
beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients

## Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)

## Optimize!
result <- nloptr(x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,Y=Y,X=X)
print(result)




# ----------------------------------------------
# 7b now with Nelder-Mead
# ----------------------------------------------

objfunNM  <- function(theta,Y,X) {
  # need to slice our parameter vector into beta and sigma components
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  # write objective function as *negative* log likelihood (since NLOPT minimizes)
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((Y-X%*%beta)/sig)^2) ) 
  return (loglike)
}

## initial values
theta0 <- runif(dim(X)[2]+1) #start at uniform random numbers equal to number of coefficients
theta0 <- append(as.vector(summary(lm(Y~X))$coefficients[,1]),runif(1))

## Algorithm parameters
optionsNM <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e4)

## Optimize!
resultNM <- nloptr( x0=theta0,eval_f=objfunNM,opts=optionsNM,Y=Y,X=X)
print(resultNM)


# 7c- do answers differ?
result
resultNM
## Yes answers differ, only by around 0.0000x on average...overall it seems that Nelder-Mead has more decimal places

# ----------------------------------------------
# 8 Now compute beta hat MLE using nloptr's L-BFGS alg 
# ----------------------------------------------

# use same objfunNM as in 7b

gradient <- function (theta ,Y,X) {
  grad <- as.vector(rep (0, length (theta )))
  beta <- theta[1:(length ( theta) -1)]
  sig <- theta [ length (theta )]
  grad [1:( length ( theta) -1)] <- -t(X)%*%(Y - X%*%beta )/(sig ^2)
  grad[ length (theta )] <- dim (X)[1] /sig - crossprod (Y-X%*%beta )/(sig^3)
  return ( grad )
}

# same initial values as 7b

optionsMLE <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e4)

## Optimize!
resultMLE <- nloptr( x0=theta0,eval_f=objfunNM,eval_grad_f=gradient,opts=optionsMLE, Y=Y,X=X)
print(resultMLE)

#----------------------------------------------
# 9 compoute beta hat ols easy way using lm() --- tell lm not to include the constant
#----------------------------------------------
ols_estim <- lm(Y~X -1)
library(modelsummary)
modelsummary(ols_estim,output="latex")

