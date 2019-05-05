n = 30
k = 1000
mu = 5; sigma = 2; sdclt = sigma/sqrt(n)

x = matrix(rnorm(n*k,mu,sigma),n,k) #generate a multi
x.mean = apply(x,2,mean)
x.down = mu - 4*sdclt; x.up = mu + 4*sdclt; y.up = 1.5
hist(x.mean,prob= T,xlim= c(x.down,x.up),ylim= c(0,y.up),main= "Samplingdistribution of the sample mean. Normal case")
par(new= T)
x = seq(x.down,x.up,0.01)
y = dnorm(x,mu,sdclt)
plot(x,y,type= 'l',xlim= c(x.down,x.up),ylim= c(0,y.up))

#quest4: The more n increaseas the more the varriance decreases. x SUIT une loi Normale(mu, sigma²/n)