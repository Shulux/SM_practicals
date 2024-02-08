#### Mahalonobis distances
# install.packages("remotes") 
# remotes::install_github("tmaturi/sm2data") 

library(sm2data)
data(engine)

head(engine)
names(engine)
dim(engine)
summary(engine)

?engine


## Exercise 6
# calculate m estimate and the sample variance matrix estimate of the full data set

M <- colMeans(engine) # means of each column m1, m2, m3
S <- var(engine) # sample variance, if we wanted pop variance we multiply by (n-1)/n

round(M, digits=2)
round(S, digits=2)


## Exercise 7
# pairs plot of the data, then scatter plot of CO and HC, find outliers

pairs(engine)

plot(engine$CO, engine$HC)
identify(engine$CO, engine$HC)

plot(engine$CO, engine$NOX)
identify(engine$CO, engine$NOX)

plot(engine$HC, engine$NOX)
identify(engine$HC, engine$NOX)

# 34 35 39 are consistently outliers in every 2D plane, so they are outliers overall


## Exercise 8
# compute squared mahalanobis distances to the mean

d_squared <- mahalanobis(engine, center=M, cov=S)
d_squared ## mahalanobis function value

d_squared[5] ## 5th case number

mahalanobis_squared_5 <- t(as.numeric(engine[5,])-M)%*%solve(S)%*%(as.numeric(engine[5,])-M)
mahalanobis_squared_5 ## mathematical formula value of the 5th case number

as.numeric(mahalanobis_squared_5 - d_squared[5]) ## equal values


## Exercise 9
# hypothesis testing for outliers using mahalanobis distance section 1.3.2

# H0: x is not an outlier
# H1: x is an outlier
# significance level alpha

alpha1 <- 0.050
alpha2 <- 0.025

?qchisq

d_squared ## our test statistics

p <- 3 # degrees of freedom = 3

chi_alpha1 <- qchisq((1-alpha1), df=p) # 0.05 significance level
chi_alpha2 <- qchisq((1-alpha2), df=p) # 0.025 significance level

d_squared[d_squared > chi_alpha1]
# 30 34 35 39

d_squared[d_squared > chi_alpha2]
# 34 35 39

which(d_squared > chi_alpha1)
# 30 34 35 39

which(d_squared > chi_alpha2)
# 34 35 39


## Exercise 10
# what assumptions did we make for Exercise 9? check this assumption

# the outlier detection process assumes multivariate normality
# we can test for this using 1.3.1

n <- dim(engine)[1]

# step 1: sort the values of d
sorted_d <-  sort(d_squared)
sorted_d

# step 2: compute quantiles of chi distribution
q <- rep(0,n)
for (i in 1:n){
  q[i] <- qchisq(p=((i-0.5)/n), df=p) # 1.31 formula step 2
}

# step 3: plot sorted d vertical against q horizontal
# step 4: compare to straight line y=x
plot(q, sorted_d); abline(0, 1)


## Exercise 11
# repeat Exercise 9 using the Bonferroni correction


alpha1 <- 0.050
alpha2 <- 0.025

?qchisq

d_squared ## our test statistics

n <- 46
p <- 3 # degrees of freedom = 3

chi_alpha1_bonferroni <- qchisq((1-alpha1/n), df=p) # 0.05 significance level
chi_alpha2_bonferroni <- qchisq((1-alpha2/n), df=p) # 0.025 significance level

d_squared[d_squared > chi_alpha1_bonferroni]
# named numeric(0)

d_squared[d_squared > chi_alpha2_bonferroni]
# named numeric(0)

which(d_squared > chi_alpha1_bonferroni)
# named numeric(0)

which(d_squared > chi_alpha2_bonferroni)
# named numeric(0)

