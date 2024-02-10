# install.packages("remotes") #you need to do this once
# remotes::install_github("tmaturi/sm2data") #you need to do this once

library(sm2data)
data(engine)
?engine

head(engine)
names(engine)
dim(engine)
summary(engine)
str(engine)

n <- 46
p <- 3

# Suppose we want to study the relation between nitrogen monoxide and hydrocarbons and carbon monoxide.
# consider NOX = b1 + b2CO +b3HC + error


## Exercise 12
# construct design matrix X, response matrix Y

X <- as.matrix(cbind(rep(1,n), engine[, c("CO", "HC")]))
X

Y <- engine[,"NOX"]
Y


## Exercise 13
# solve the normal equations, that is, compute XTXinvXTY to obtain the estimated parameters

XT <- t(X)
XTXinv <- solve(XT%*%X)

beta_hat <- XTXinv%*%XT%*%Y
beta_hat


## Exercise 14
# fit the lm above using R function, lm, with NOX as response and CO and HC as predictors.
# compare results to exercise 13.

engine.lm <- lm(NOX ~ CO + HC, data=engine)
engine.lm

summary(engine.lm)
summary(engine.lm)$coefficients

beta_hat_usinglm <- c(summary(engine.lm)$coefficients[1], summary(engine.lm)$coefficients[2], summary(engine.lm)$coefficients[3]) 
beta_hat_usinglm  ### identical

## Exercise 15
# using the fitted linear model from exercise 14, shown here:
# save the vector of parameters into beta, and the sd into s
# produce the estimated variance matrix of bhat into Sigma
# extract SE(b2), SE(b3)

bhat <- as.vector(engine.lm$coefficients)
bhat

s <- summary(engine.lm)$sigma
s

Sigma <- s^2*summary(engine.lm)$cov.unscaled #using 2.32 equation 2.14
Sigma

standard_error_1 <- s*sqrt(summary(engine.lm)$cov.unscaled[1,1])
standard_error_2 <- s*sqrt(summary(engine.lm)$cov.unscaled[2,2])
standard_error_3 <- s*sqrt(summary(engine.lm)$cov.unscaled[3,3])
se_bhat <- c(standard_error_1, standard_error_2, standard_error_3)

# equivalent to 
s*sqrt(diag(solve(t(X)%*%X)))
s*sqrt(diag(XTXinv))

summary(engine.lm) ### identical


## Exericse 16
# produce 95% confidence intervals beta_hat_j +- t_n-p_0.025 * SE(beta_hat_j) for j=2,3
# hint: use qt(0.975, df) where df = n-p
# check for correctness using confint function

n
p
alpha <-  0.05

bhat
t_quantile <- qt((1-alpha/2), df=n-p)
se_bhat

beta_confidence_intervals <- matrix(c(bhat - (t_quantile * se_bhat), bhat + (t_quantile * se_bhat)), ncol=2, nrow=3)
beta_confidence_intervals

confint(engine.lm, parm=1, level=0.95) ### identical
confint(engine.lm, parm=2, level=0.95) ### identical
confint(engine.lm, parm=3, level=0.95) ### identical


## Exercise 17
# carry out hypothesis test H0: b2=0, vs H1: b2=/=0
# a) consider p-value in summary
# b) compute test statistic, then compare with critical value
# c) consider previously computed confidence interval

# a)
summary(engine.lm)$coefficients
# here: p-value for b2 is 4.070093e-04
# very small -> evidence for rejecting H0
# 0.000407 < 0.025 so reject H0

# b)
test_statistic_b2 <- abs((bhat[2]-0)/(standard_error_2))
critical_value <- qt((1-alpha/2), df=n-p) # same as before
test_statistic_b2
critical_value
# here: T = 3.833677 > 2.016692 = critical value
# so we have evidence to reject H0 at significance level alpha = 0.05

# c)
beta_confidence_intervals[2,]
# here: confidence interval states [-0.1354..., -0.04207...] and the null hypothesis states b2=0
# the confidence interval does not contain b2=0
# so we have evidence to reject H0.

# in conclusion we have evidence to reject H0: b2=0


## Exercise 18
# carry out hypothesis test H0: b3=1, vs H1: b3=/=1
# a) compute test statistic, then compare with critical value
# b) consider the confidence interval

# a)
test_statistic_b3 <- abs((bhat[3]-1)/(standard_error_3))
test_statistic_b3
critical_value
# here: T = 0.1501 < 2.01 = critical value
# so we do not have evidence to reject H0

# b)
beta_confidence_intervals[3,]
# here: confidence interval is [-0.568.., 2.351..], b3=1 null hypothesis
# the confidence interval contains b3=1
# so we do not have evidence to reject H0.

# in conclusion we do not have sufficient evidence to reject H0: b3=1


## Exercise 19
# we know sampling distribution of s^2 is given by 
# s^2 ~ sd^2 * chisquared_(n-p degrees of freedom)/(n-p)
# we are able to devise tests for sd^2.
# assume H0: sd^2 <= sd^2_0 vs H1: sd^2 > sd^2_0
# define test statistic: V = s^2/sd^2_0 * (n-p)
# H0 rejected iff V > chisquared_(n-p)_alpha
# write a function test.s which takes a fitted model lmobject and the value sigma0
# produces the corresponding p-value as output
# apply the function onto sigma0=0.3
# draw a conclusion at significant level 5%

test.s <- function(lmobject, sigma0=1) {
  s <- summary(lmobject)$sigma
  df <- summary(lmobject)$df[2]
  
  V <- (s^2)/(sigma0^2)*(df)
  p_value <- 1 -pchisq(V, df)
  
  return(p_value)
}


s
sigma0 <- 0.3
summary(engine.lm)

test.s(engine.lm, 0.3)
# here: p-value = 0.04300668
# p-value < 0.05
# we reject H0
# so conclude sigma > 0.3


## Exercise 20
# from a light-duty engine 12.2g CO and 0.4g HC
# use the fitted model to predict the NO
# obtain 95% confidence interval for the expected emission
# obtain 95% prediction interval for the actual emission

predict(engine.lm, newdata=data.frame("CO"=12.2,"HC"=0.4)) # 0.8171029
predict(engine.lm, newdata=data.frame("CO"=12.2,"HC"=0.4), interval= "confidence") # [0.3947, 1.239]
predict(engine.lm, newdata=data.frame("CO"=12.2,"HC"=0.4), interval= "prediction") # [-0.01373, 1.6479]

##### Alternatively, manually calculate:
x0 <- c(1, 12.2, 0.4)
y0hat <- as.numeric(x0 %*% engine.lm$coef)
y0hat # prediction

# confidence interval
y0hat + c(-1, 1)*qt(0.975, 43)*s*sqrt(as.numeric(x0 %*% summary(engine.lm)$cov.unscaled %*% x0))

# prediction interval
y0hat + c(-1, 1)*qt(0.975, 43)*s*sqrt(as.numeric(1 + x0 %*% summary(engine.lm)$cov.unscaled %*% x0))
