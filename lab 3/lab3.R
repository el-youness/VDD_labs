data(iris)
mytable <- table(iris$Species)
# paste(multiple R objects, sep=" ") so we put the name and the number 
print(mytable)
lbls <- paste(names(mytable), "\n", 50, sep="")
pie(mytable, labels = lbls, 
    main="Pie Chart of Species\n (with sample sizes)")

# Histogram
par(mfrow=c(2,2))
hist(iris$Petal.Length, main="Petal Length", col="blue1", breaks=20, border = "pink")
hist(iris$Petal.Width, main="Petal Width", col="blue1", breaks=4, border = "pink")
hist(iris$Sepal.Length, main="Sepal Length", col="blue1", breaks=3, border = "pink")
hist(iris$Sepal.Width, main="Sepal Width", col="blue1", breaks=10, border = "pink")


#ECDF
require(graphics)
par(mfrow=c(2,1))

FnPL<- ecdf(iris$Petal.Length)
FnSW <- ecdf(iris$Sepal.Width)
plot(FnPL, col="blue",  main='ecdf1', panel.first =grid())
plot(FnSW, col="red",  main='ecdf2', panel.first =grid())

