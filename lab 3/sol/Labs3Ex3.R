# Solution Exercise 3
# Author: Lionel Fillatre

# 1- load data
data("mtcars")

# 2- Heatmap
mat <- as.matrix(mtcars)
heatmap(mat, 
        Colv=F, #scale='none',
        scale = "column")

# 3- Scatter plot in 3D with fitted surface
library("car")
scatter3d(mtcars$mpg~mtcars$wt+mtcars$disp, #|Species, 
          data=iris, 
          fit="linear", 
          residuals=TRUE, 
          parallel=FALSE, 
          bg="black", 
          axis.scales=TRUE, 
          grid=TRUE, 
          ellipsoid=FALSE)