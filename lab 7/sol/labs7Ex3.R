# Author: Lionel Fillatre
# Lecture: Data Valorization

# Question 1:
# It is a trivial recall.

# Quesion 2:
# This is a theoretical question.
# You have just to calculate the likelihood ratio test.
# A short calculation shows that the test is:
# Decide p_0 is Y <= h
# Decide p_1 if Y>h

# Question 3:
# The distribution is a binomial distribution.

# Question 4:
n <- 10            
k <- 1000 # number of samples
p0 <- 0.6 

# Generation of the samples
x = matrix(rbinom(n*k,1,p0),n,k) 
x.mean = apply(x,2,sum)
std <- sqrt(n*p0*(1-p0))
mu <- n*p0

par(mfrow=c(2,2))
# The empirical distribution of the sum of Bernoulli variables (just to recall how to plot an histogram)
x.down = 0; x.up = n; y.up = 0.3
hist(x.mean,
     xlab="Number of success",
     ylab="Empirical pdf",
     prob= T,
     xlim= c(x.down,x.up),ylim= c(0,y.up),
     breaks=n,
     main= 'Sampling distribution of Bernoulli sum')

# The theoretical binomial pdf (just to recall the pdf)
thpdf <- dbinom(0:n,size=n,prob=p0)
barplot(thpdf,
        xlab="Number of success",
        ylab="Pdf",
        ylim= c(0,y.up),
        names.arg=0:n,
        main=sprintf(paste('bin. dist. ',n,p0,sep=':')))

# The theoretical binomial cdf
thcdf <- pbinom(0:n,size=n,prob=p0)
plot(thcdf, 
     xlab="Number of success", 
     ylab="Cdf",
     main=sprintf(paste('bin. dist. ',n,p0,sep=':')))

# It is straightforward to plot the pdf for p=0.2 and some other values of n!

# Question 5:
# As shown in the lecture, the threshold depends on the inverse
# of the cdf.


# Question 6:
# The cdf is not a continuous function!
# Its inverse is not continuous.
# Hence, it is not possible to achieve any value of alpha.

# Question 7:
# The possible thresholds are -1,0,1,2,...,n
# Other thresholds will lead to the same set of error probabilities.
# For each threshold, we compute the error probabilities.
# The following vector contains all the possible values of type I errors
errI <- 1-pbinom(-1:n,size=n,prob=0.5)
errII <- pbinom(-1:n,size=n,prob=0.8)

par(mfrow=c(2,2))

plot(-1:n,errI, type="b", pch=1, col="red", 
     xlab="Number of success", ylab="Probability",main="Decision error")
lines(-1:n,errII,pch=18, col="blue", type="b", lty=2)
legend("left", legend=c("Type I error", "Type II error"),
       col=c("red", "blue"), lty=1:2, cex=0.8,
       title="Line types", text.font=4, bg='lightblue')

# Question 8:
k <- 30 # number of samples
# We choose a very small number of samples to see 
# the gap between the estimates and the true value.
# With 1000 samples, the gap is almost invisible.
x0 = matrix(rbinom(n*k,1,0.5),n,k) 
x0.mean = apply(x0,2,sum)
x1 = matrix(rbinom(n*k,1,0.8),n,k) 
x1.mean = apply(x1,2,sum)

# Question 9:
# We compare the errors for all the possible thresholds.
Fn0 <- ecdf(x0.mean)
errIhat <- 1-Fn0(-1:n)  
Fn1 <- ecdf(x1.mean)
errIIhat <- Fn1(-1:n)  

plot(-1:n,errIhat, type="b", pch=1, col="green", 
     xlab="Number of success", ylab="Probability",main="Decision error")
lines(-1:n,errIIhat,pch=18, col="orange", type="b", lty=2)
legend("left", legend=c("Type I error estimate", "Type II error estimate"),
       col=c("green", "orange"), lty=1:2, cex=0.8,
       title="Line types", text.font=4, bg='lightblue')

# Question 10:
# We compare the errors for all the possible thresholds.
plot(-1:n,errI, type="b", pch=1, col="red", 
     xlab="Number of success", ylab="Probability",main="Decision error")
lines(-1:n,errIhat,pch=1, col="green", type="b", lty=1)
lines(-1:n,errII,pch=18, col="blue", type="b", lty=2)
lines(-1:n,errIIhat,pch=18, col="orange", type="b", lty=2)
legend("left", legend=c("Type I error", "Type I error estimate",
                        "Type II error", "Type II error estimate"),
       col=c("red", "green","blue", "orange"), lty=c(1,1,2,2), cex=0.8,
       title="Line types", text.font=4, bg='lightblue')

