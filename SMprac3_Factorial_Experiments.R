### Practical 3 Factorial Experiments

library(sm2data)
data("missile")

names(missile)
summary(missile)
dim(missile)
str(missile)

## 2 factors, 36 observations, 3x3 factorial design
## 2 factors, 3 levels each, 12 replicates for each level

# reorder the levels of temp factor
missile$Temperature <-  factor(missile$Temperature, levels = c("Low", "Medium", "High"))

str(missile) # low is now the reference category


## Exercise 25
# which experiment are we considering?
# name some characteristics in terms of design

# we have a 3x3 factorial design,
# 2 factors, 36 observations, 3 levels per factor
# 12 replicates per level
# complete and balanced.

## Exercise 26
# fit the constrained model to the data

missile.lm.interaction.constrained <- lm(Battery.Life ~
                               Temperature
                             + Material
                             + Temperature:Material,
                             data = missile)

summary(missile.lm.interaction.constrained)
# material 1, temperature low,
# any combination of them

# tau(i=1, A) = tau(i=1, B) = tau(11, AB) = tau(1j, AB) = tau(j1, AB) = 0


## Exercise 27
# produce a plot for a two-way interaction
# using mean values for the response
# compare the plot with the param estimates
# for the model with interaction
# write some considerations

interaction.plot(missile$Temperature,
                 missile$Material,
                 missile$Battery.Life,
                 fun = mean)

missile.lm.interaction.constrained$coefficients


## Exercise 28
# using the model with interaction,
# calculate 99% confidence intervals
# for the expected life of batteries,
# corresponding 99% prediction intervals
# for the life of a single battery

# a) made with material 1, low temp
# b) made with material 2, low temp
# c) made with material 1, high temp
# d) do this without function predict() for one of them

n <- 36
p <- 9
alpha <- 0.01

XTXinv <- summary(missile.lm.interaction.constrained)$cov.unscaled
sigma <- summary(missile.lm.interaction.constrained)$sigma
betahat <- as.matrix(missile.lm.interaction.constrained$coefficients)


# a)

x0 <- as.matrix(c(1, c(rep(0,8))))
x0

y_fitted <- t(x0) %*% betahat
y_fitted

CI.expected.a <- y_fitted +
                c(-1, 1) *
                qt(alpha/2, df = n - p, lower.tail = FALSE) *
                sigma *
                sqrt(t(x0) %*% XTXinv %*% x0)

PI.single.a <- y_fitted +
              c(-1, 1) *
              qt(alpha/2, df = n - p, lower.tail = FALSE) *
              sigma *
              sqrt(1 + t(x0) %*% XTXinv %*% x0)

a <- c(CI.expected.a, PI.single.a)
a


# a, b, c)

predict(missile.lm.interaction.constrained,
        newdata = data.frame(
          Temperature = c("Low", "Low", "High"),
          Material = c("1", "2", "1")),
        interval = "confidence",
        level = 0.99)

predict(missile.lm.interaction.constrained,
        newdata = data.frame(
          Temperature = c("Low", "Low", "High"),
          Material = c("1", "2", "1")),
        interval = "prediction",
        level = 0.99)
