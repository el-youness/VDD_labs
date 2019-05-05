# Solution Exercise 2
# Lecture: Data Valorization
# Author: Lionel Fillatre

# 1 - Sample generation
M <- 1000
n <- 4
mu <- 3
rate <- 1/mu # be careful with the definition of the exponential pdf and its rate!
x <- matrix(rexp(M*n,rate),n,M) # random samples

# 2 - Maximum likelihood estimates
x.muhat <-  apply(x,2,mean) # compute the estimates

# 3 - Empirical pdf
nbbins <- 20
x.down = 0; x.up = mu*log(4)+2*log(3)*mu; # the x bounds are chosen to cover a significant interval
y <- density(x.muhat)
plot(y,
     xlim= c(x.down,x.up),
     main='Kernel density estimate')

# 4 - Behavior of the empirical pdf
nSeq <- c(4,10,50,100,500,1000) # some arbitrary values to see the empirical pdf
par(mfrow = c(3, 2))  # 3 rows and 2 columns
for (i in 1:length(nSeq)) {
  n <- nSeq[i]
  x <- matrix(rexp(M*n,rate),n,M) # random samples
  x.muhat <-  apply(x,2,mean) # compute the estimates
  y <- density(x.muhat) # Kernel density estimate
  plot(y, 
       xlim= c(x.down,x.up),
       main=paste("Kernel Density estimate with n=", n)
       )
}

# 5 - Study of the bias and variance
x.muhat.bias <-  mean(x.muhat)-mu 

bias=rep(0,length(nSeq))
varMuhat=rep(0,length(nSeq))
for (i in 1:length(nSeq)) {
  n <- nSeq[i]
  x <- matrix(rexp(M*n,rate),n,M) # random samples
  x.muhat <-  apply(x,2,mean) # compute the estimates
  x.muhat.bias <-  mean(x.muhat)-mu # compute the bias
  x.muhat.var <- var(x.muhat) # compute the variance
  bias[i] <- x.muhat.bias # save the bias
  varMuhat[i] <- var(x.muhat) # save the variance
}

CRbound <- mu**2/nSeq # Cramer-Rao bound

par(mfrow = c(2, 1)) # plot the results on the same figure

plot(nSeq,bias,type= 'l',
     xlab = "Number of samples", ylab = "Bias",
     main='Bias as a function of the number of samples')


matplot(nSeq, cbind(varMuhat,CRbound),type="l",col=1:2,lty=1:2,lwd=2,
        xlab = "Number of samples", ylab = "Variance",
        main='Variance as a function of the number of samples')
legend("center", c("Estimate", "CR bound"),col = 1:2, lty = 1:2 )

