### Practical 3 Factor Coding

### Insects data set
library(sm2data)
data("InsectSprays")

## Exercise 2.1
# coding of spray factor
# fit a linear model
# display the model summary and thee design matrix
# what spray type is the reference category?

summary(InsectSprays)
dim(InsectSprays)
names(InsectSprays)

insects <- data.frame(InsectSprays)
insects$spray = as.factor(insects$spray)
summary(insects)
str(insects)

insects.lm <- lm(count ~ spray, data = insects)
summary(insects.lm)

X.design <- model.matrix(insects.lm)
X.design

# reference category must be sprayA

## Exercise 22
# assume now that sprayF is the reference category
# repeat the lm fit using new spray2

insects$spray2 <- relevel(insects$spray, ref = "F")

names(insects)

insects.lm2 <- lm(count ~ spray2, data = insects)
summary(insects.lm2)

X.design2 <- model.matrix(insects.lm2)
X.design2

# reference category is now spray2F


## Exercise 23
# use predict() to find the predicted values for sprayC
# using each of the two models
# produce 95% prediction intervals

predict(insects.lm,
        newdata = data.frame(spray = "C"),
        interval = "prediction")

predict(insects.lm2,
        newdata = data.frame(spray2 = "C"),
        interval = "prediction")


# we have predicted value of 2.08333 for both models
# with the same prediction intervals
# meaning the reference category is arbitrary


## Exercise 24
#### NONE EXAMINABLE
# check the coding type
# change to effect coding
# change back

options("contrasts") # default coding is dummy "treatment" coding

options(contrasts = c("contr.sum", constrasts = TRUE))

options("contrasts") # default coding is dummy "treatment" coding

insects.lm3 <- lm(count ~ spray, data = insects)
summary(insects.lm3)

# we have all 5 sprays shown, the reference category is for any spray

model.matrix(insects.lm3)

predict(insects.lm3, newdata = data.frame(spray = "C"), interval = "prediction")

# same prediction interval


options(contrasts = c("contr.treatment", contrasts = TRUE))
options("contrasts")
