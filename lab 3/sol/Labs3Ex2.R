# Solution Exercise 2
# Author: Lionel Fillatre

# 1- load data
data("iris")

# 2- Scatter plot
pairs(iris[1:4],cex = 1.5,pch = 21,bg = "light blue")

# 3- Correlogram matrix
library(corrgram)
corrgram(iris, 
         lower.panel=panel.pts, 
         upper.panel=panel.conf,
         diag.panel=panel.density)

# 4- Correlation matrix
library(corrplot)
iris_matrix <- as.matrix(iris[,1:4])
cm1 <- cor(iris_matrix)
corrplot(cm1, method="ellipse")

# 5- Parallel coordinates plot
library(MASS)
parcoord(iris[,1:4], col=iris$Species)

# 6- hexbinplot function to plot 2D histogram
library(hexbin)
df=iris[,1:4]
hexbinplot(iris$Petal.Length~iris$Petal.Width, data=df)#, colramp=rf)

# 7- star plot
stars(iris[1:4], 
      len = 0.8, 
      key.loc = c(20, 1),
      main = "iris", 
      full = TRUE) 