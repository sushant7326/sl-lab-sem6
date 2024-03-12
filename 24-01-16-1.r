# Linear Regression using R

x <- rnorm(100, 5, 1)
y <- 100 + 2 * x + rnorm(100, 2, 0.5)

m1 <- lm(y ~ x)
summary(m1)

anova(m1)

plot(x, y)
abline(lm(y ~ x), col = "red")

residuals(m1)
qqnorm(residuals(m1))

yhat <- predict(m1)
resid.calc <- y - yhat

plot(yhat, residuals(m1), xlab = "Predicted y", ylab = "Residuals")

xnew <- data.frame(x = rnorm(25, 5, 1))
xnew
ynew <- predict(m1, newdata = xnew)
ynew

install.packages("car")
library(car)
data("mtcars")
head(mtcars)

library(dplyr)
mt1 <- mtcars %>% select(mpg, disp, hp, drat, wt, qsec)
View(mt1)

pairs(mt1)
ctm1 <- cor(mt1)

install.packages("corrplot")
heatmap(cor(mt1))
library(corrplot)
corrplot(ctm1)

m2 <- lm(mpg ~ ., data = mt1)
summary(m2)
vif(m2)

m3 <- lm(mpg ~ hp + drat + wt + qsec, data = mt1)
summary(m3)
vif(m3)

m3 <- lm(mpg ~ . - disp - hp, data = mt1)
summary(m3)
vif(m3)

# analysis of variance
anova(m4)
