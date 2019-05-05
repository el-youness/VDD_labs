# Author: Lionel Fillatre
# Lecture: Data Valorization


#Sigmoid function
sigmoid <- function(z)
{
  g <- 1/(1+exp(-z))
  return(g)
}

# Replicate a matrix
repmat = function(X,m,n)
{
  mx = dim(X)[1]
  nx = dim(X)[2]
  matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T)
}

# set the seed of the ranom number generator
# The random values will be the same if you run several times the code with the same parameters
set.seed(10123) 

n <- 10000 # number of samples
beta0 <- 0.3
beta1 <- 1.7
# Generate the normal samples
X0 <- matrix(rnorm(n, mean = 0.5, sd = 1.2),nrow=n,ncol=1)
X1 <- matrix(rnorm(n, mean = 1.1, sd = 1.2),nrow=n,ncol=1)

# Generate the labels
eta0 <-beta0 + beta1*X0
eta1 <-beta0 + beta1*X1
P0 <- matrix(1/(1+exp(-eta0)),nrow=n,ncol=1)
P1 <- matrix(1/(1+exp(-eta1)),nrow=n,ncol=1)
Y0 <- matrix(rbinom(n,1,P0),nrow=n,ncol=1)     # bernoulli response variable
Y1 <- matrix(rbinom(n,1,P1),nrow=n,ncol=1)     # bernoulli response variable

# concatenate the data
Y10 <- rbind(Y0,Y1)
X10 <- rbind(X0,X1)

# optional: feed the data to glm (just to test the glm function and the generated samples)
df <- data.frame(y=Y0,x=X0)
glm( y~x,data=df,family="binomial")

# Algorithm by yourself: Gradient ascent
nbIteration <- 300 # number of iterations
theta <- matrix(numeric(2), nrow=2) # Initialize the parameters
a <- rep(1,2*n) # vector of 1's
H <- matrix(c(a,X10), nrow = 2*n, ncol = 2) # regression model

step <- 0.01 # step of the gradient ascent
y <- Y10
theta_i <- theta # initialization of the loop
J <- matrix(0, nbIteration, 1) # to save the cost function
matG <- matrix(0, nbIteration, 1) # to save the gradient norm
matTheta <- matrix(0, 2, nbIteration) # to save the estimates
for(i in 1:nbIteration)
{
  etaHat_i <- H %*% theta_i
  pHat_i <- 1/(1+exp(-etaHat_i))
  res <- y - pHat_i
  matRes <- repmat(res,1,2) # residuals with matrix form
  Gx <- matRes*H # element-wise product: gradient for each observation
  G <- matrix(colSums(Gx),nrow=2,ncol=1) # G is the full gradient
  Gnorm <- sqrt(G[1]**2+G[2]**2) # norm of the gradient
  matG[i] <- Gnorm # save the norm
  theta_i <- theta_i + step * G / Gnorm  # new estimate
  matTheta[,i] <- theta_i # save the estimate
  
  J_i<-  sum( y*log(pHat_i)+(1-y)*log(1-pHat_i) ) # Cost function
  J[i] <- J_i
}

print(theta_i) # print the last estimate

# Plot the results
par( mfrow = c( 2, 2 ) )
plot(J, xlab="iteration", ylab="J")
plot(matG, xlab="iteration", ylab="Norm of gradient")
plot(matTheta[1,], xlab="iteration", ylab="theta1")
plot(matTheta[2,], xlab="iteration", ylab="theta2")


# Estimate Pr(Y=0) andPr(Y=1)
print(table(Y0)/n) # estimate Pr(Y=0) and Pr(Y=1) for m=m0
print(table(Y1)/n) # estimate Pr(Y=0) and Pr(Y=1) for m=m1
