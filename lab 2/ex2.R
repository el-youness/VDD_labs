require(graphics)
m=0.3; sigma=1.4
x <- rnorm(100, m, sigma)
Fn <- ecdf(x)

#2.2
plot(Fn, col="blue")

#2.3
a = m - 3*sigma
b = m + 3*sigma
N = 500
t = seq(a, b, by = (b-a)/N)

Fnt = Fn(t)
F = pnorm(t, mean=m, sd=sigma)
plot(t, Fnt, type="l", col="blue", main='ecdf and cdf')
par(new = TRUE)
plot(t, F, type="l", col="red")