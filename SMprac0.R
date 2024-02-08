#read.table("file.dat", header = TRUE)
data(cement, package="MASS")

# gives the first six rows of the `cement` data frame.
head(cement)

# gives the names of the variables included in the data frame.
names(cement)

# gives the dimension of the  data frame.
dim(cement)

# extract the y column from the cement data set
cement$y

# write cement data to a file "test.dat" 
write.table(cement, file= "test.dat", quote=FALSE)



## Vectors and Matrices

# we can create vectors
a <- c(1, 2, 7)
b <- c(a, 2, 4, 6, 0, 1, 7)

# we can create matrices
A <- matrix(b, ncol=3, byrow=TRUE)
# here, `byrow=TRUE` means that the matrix is filled row by row

# we can select an element
a[3]
# here: 7

# we can select an elemnt in a matrix by [i,j]
A[3,2]
# here: 1

# we can select multiple rows
A[1:2, ]
# which is done by [rows, columns]
# here we did row 1 to 2, with all columns

# appending a vector to a matrix:
Aa <- cbind(A, a)
Aa
# we can see that the vector is labelled in the matrix

# transpose, inverse, determinant, trace:
t(A) #transpose
solve(A) #inverse
det(A) #determinant
sum(diag(A)) #trace

# eigenvalues and eigenvectors
eigen(A)

# matrix multiplication
B <- matrix(1:12, ncol=3, nrow=4)
BA <-  B %*% A
BA


## Ordering

# ordered/sorted values of b
b
sort(b)

# order statistics of b, tells us where the smallest value is
order(b)

# other version for ordered/sorted values of b
b[order(b)]


## Statistical operations
a
mean(a) # mean of vector a
sd(a) # standard deviation of vector a
range(a) # range of vector a

A
colMeans(A) # means of the columns of matrix A
rowMeans(A) # means of the rows of matrix A

hist(a) # histogram of distribution of vector a
hist(A) # histogram of distribution of elements in matrix A

Z <- Aa
Z
var(Z) # variance of a data matrix Z
cor(Z) # correlation matrix of a data matrix Z

qnorm(0.99, 0, 1) # 99% quantile for a normal distribution mean=0, sd=1
qt(0.95, df=18) # 95% quantile for a t distribution with 18 degrees of freedom

D <- cement # dataframe D with variables x1, y
lm1 <- lm(y ~ x1, data=D) # linear model of the type y = a + b*x + error
plot(D$x1, D$y); abline(lm1) # scatterplot of the regression line obtained

n <- 50
a <- 0
b <- 1
m <- 0
s <- 1
runif(n, min=a, max=b) # n random samples from uniform on [a,b]
rnorm(n, mean=m, sd=s) # n random samples from normal distribution with mean=m, sd=s


## Basic Programming

# if then
a <- 4
if (a==0){ stop("invalid divisor")} else {10/a}

# for loop
for (i in 1:10){ cat('This is loop', i, '\n') }

# functions
max1 <- function(a, b=1) {
  result <-  max(a, b)
  return(result)
}

max1(0.5)
max1(0.5, 0)

# apply function onto matrices
A

apply(A, 1, sum) # applied a sum function across the rows of A

apply(A, 2, mean) # applied the mean function across the columns of A, = colMeans(A)
colMeans(A)


## Plotting and data visualisation
x <-  cement$x1
y <-  cement$y

plot(x, y) # 2D scatterplot of y vs x

points(10, 100) # adds additional points to an existing plot

i <-  1
j <- 13
segments(x[i], y[i], x[j], y[j]) # draws a straight line from (xi,yi) to (xj, yj)

pairs(D) # gives a matrix plot (pairplot in seaborn) of all 2D combinations of variables in D

library(lattice)
z <-  cement$x2
cloud(z ~ x + y) # gives a 3D scatterplot of z vs x and y.

# persp(x, y, S) # gives a 3D surface plot where x and y are vectors of length n, S is an nxn matrix containing S(x,y)
p <- 1:10
q <- 1:10
S <- matrix(1:100, 10, 10)
persp(p, q, S)


contour(p, q, S) # works the same as persp but gives a contour plot instead
