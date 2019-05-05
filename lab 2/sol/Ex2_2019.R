# Solution Exercise 2
# Author: Lionel Fillatre

n <- 1000  # number of samples to compute ecdf
m <- 0.3 # theoretical mean
sigma <- 1.4 # theroetical standard-deviation
epsilon <- 0.05 # epsilon in the inequality

a <- m - 3*sigma # relevant range [a,b] of the normal distribution 
b <- m + 3*sigma
N <- 500 # number of x values to compare ecdf with cdf
t <- seq(a, b, by = (b-a)/N) # sampling of ecdf and cdf to approximate sup errors

T <- 2000 # number of loops to estimate P(D>epsilon)
vD <- numeric(T) # initialization

# main loop of T iterations to estimate the inequality probability
for ( i in 1:T) { 
  X <- rnorm(n, mean = m, sd = sigma) # random samples generation
  Fn <- ecdf(X) # compute the ecdf
  Fnt <-  Fn(t) # values of the ecdf at points t
  F <- pnorm(t, mean = m, sd = sigma) # cdf computation
  D <- max(abs(Fnt-F)) # error of approximation
  vD[i] <- D # store the error
}

r <- sum(vD>epsilon)/T # estimation of the error probability
bound <- 2*exp(-2*n*epsilon^2) # bound of the error

# print an informative message
cat("Error rate = ", r, " and inequality bound = ", bound, "for epsilon = ", epsilon, "and n = ",n, "\n")

# plot (ecdf) to illustrate the result
plot(t,Fnt, type = "l", col = "black", main ="ecdf and cdf")
# second plot (cdf)
par(new = TRUE)
plot(t,F,type = "l", col = "red")

mhat <- mean(X) # empirical mean
sdhat <- sd(X) # empirical standard-deviation

# print an informative message
cat("Empirical mean = ", mhat, " and theoretical mean = ", m, "\n")

F <- pnorm(t, mean = mhat, sd = sigma)
Fhat <- pnorm(t, mean = mhat, sd = sdhat)
Dhat <- max(abs(Fhat-F))
cat("Maximum gap between estimated pdf and theoretical one = ", Dhat, "\n")

