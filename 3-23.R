set.seed(100)
N <- 100000
K<- 10
sigma <- 0.5
#fill x matrix with random numbers that come from this distribution
X <- matrix(rnorm(n*K, mean=0, sd=sigma), N, K) 

# make first column have 1s to have an intercept
X[,1] <- 1
eps <- rnorm(N, mean=0, sd=0.5)
betaTrue <- as.vector(runif(K))
Y <- X%*%betaTrue +eps


# OLS estimates
estimates <- lm(Y~X -1)
print(summary(estimates))

# compare results
print(data.frame(est= coef(estimates, true = betaTrue))