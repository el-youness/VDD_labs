# Solution Exercise 4
# Author: Lionel Fillatre

n <- 100000 # number of samples
D <- matrix(runif(n),1,n) # uniform samples
U <- c(1,3,5,7) # values of the discrete random variable
bounds <- c(0.1,0.4,0.8,1) # increasing sort of cdf
ind <- apply(D,2,function(r) min(which(r<bounds))) # index of the interval of each value of D
X <- U[ind] # realizations of the discrete random variable

hist(X,
     freq=FALSE,
     breaks=seq(0,8,by=1),
     main="Normalized histogram of the discrete rv", 
     xlab="values",
     xlim = c(0, 7),
     ylim = c(0, 0.5)
     )
