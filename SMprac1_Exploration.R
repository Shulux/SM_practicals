#### Exploration
# install.packages("remotes")
# remotes::install_github("tmaturi/sm2data")

library(sm2data)
data(scallops)

head(scallops)
names(scallops) # note y = log(tcatch)
dim(scallops)
summary(scallops)

## Exercise 1
# mean and variance estimates of scallops long lat
scallops_long_lat <- scallops[,c("long", "lat")]

m <- colMeans(scallops_long_lat)
m # mean estimate

Sigma <- var(scallops_long_lat)
Sigma # variance estimate

eigen(Sigma) # positive definite > 0


## Exercise 2
# scatterplot of long and lat data

plot(scallops$long, scallops$lat) # scatterplot of long and lat
points(m[1], m[2], col="red", pch="+") # adding mean to the plot


## Exercise 3
# histograms of tcatch and log(tcatch)

hist(scallops$tcatch)
hist(log(scallops$tcatch))
# as we can see, log(tcatch) is more suitable as it seems to be normally distributed


## Exercise 4
# matrix of bivariate normal density values

# long goes from -73.70 to -71.77
# lat goes from 38.6 to 40.92, which we can use to cover the samples
x <- seq(-74, -71, length=31)
y <- seq(38, 41, length=31)

# create a density matrix padded with 0s
dens <- matrix(0,31,31)

# using the pdf formula (1.5), in short X ~ N(m, Sigma) 
for (i in 1:31){
  for (j in 1:31){
    dens[i,j]<- 1/(2*pi*sqrt(det(Sigma)))*exp(-1/2* c(x[i]-m[1], y[j]-m[2])%*%solve(Sigma)%*%c(x[i]-m[1], y[j]-m[2])) 
  }
}

# That is, we create two vectors (x and y) of length 31 and
# evaluate the bivariate density function at each pair.
# Note: the value 31 is arbitrary to create a smooth contour
# plot; try a smaller value, e.g. 15 or 20 and see what happens.


## Exercise 5
# use contour() to create a contour plot of the fitted density and points() in order to add the original data to it

par(mfrow=c(1,1))
contour(x, y, dens, xlab="long", ylab="lat")
points(scallops$long, scallops$lat)

# The BVN distribution seems not to be perfectly adequate as it cannot account for (and gets biased by)
# the small branch in the North-West.
# Relative to the fitted distribution, some points in the North-West and the South-East appear to be outliers,
# but due to the reason above one has to be careful with this interpretation.


