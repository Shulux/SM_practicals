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

