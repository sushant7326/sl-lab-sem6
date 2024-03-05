library(MASS)
data(Boston)
head(Boston)
summary(Boston)
attach(Boston)

#make a plot of medv Vs age
plot(age, medv, main = "age vs medv", las = 1)

plot(age, medv, main = "Polynomial Regression", las = 2)

#Linear Model
model1 <- lm(medv ~ age)
summary(model1)

abline(model1, lwd=3, col = "red")

#polynomial regression model with degree 2
model2 <- lm(medv ~ age + I(age^2))
summary(model2)

lines(smooth.spline(age, predict(model2)), col = 'blue', lwd = 3)

#Improvement Check
anova(model1, model2)

#Trying with age^3
model3 <- lm(medv ~ poly(age, degree = 3, raw = T))
summary(model3)
lines(smooth.spline(age, predict(model3)), col = 'green', lwd = 3, lty = 4)

legend(46, 15, legend = c("model1 : linear", "model2 : poly x^2", "model3 : poly x^3"), col = c("red", "blue", "green"),
       lty = c(1,1, 3), lwd = 3, bty = "n", cex = 0.9)
#Improvement check
anova(model2, model3)