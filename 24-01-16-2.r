library(MASS)
library(ggplot2)
library(car)
library(dplyr)
library(ISLR)

# Assignment 2 -> Airquality, dependent variable -> Ozone
# Run regression, do everything taught, make best model

data(Boston)
summary(Boston)
head(Boston)
View(Boston)

# Univariate Normality using QQ Plot
qqnorm(Boston$lstat)
qqline(Boston$lstat)

# Correlation Matrix
cor(Boston)

# Linear Regression
plot(
  Boston$lstat,
  Boston$medv
)
model <- lm(
  medv ~ lstat,
  data = Boston
)
abline(mode, col = "red")

summary(model)

# Multiple Linear Regression
# Model
model <- lm(
  mdev ~ .,
  data = Boston
)
summary(model)
# Variance Influence Factor = 1/(1-R^2)
vif(model)

# Anova Table
anova(model)

# New Model
model <- lm(
  mdev ~ . - rad - tax,
  data = Boston
)
summary(model)

# Confidence Interval
confint(
  model,
  level = 0.95
)

# Prediction Interval
model <- lm(
  medv ~ lstat,
  data = Boston
)
new_pt <- data.frame(lstat = c(4, 5, 6))
predict(
  model,
  newdata = new_pt
)
predict(
  model,
  newdata = new_pt,
  interval = "predict",
  level = 0.95
)

# Residual, Fitted Value and SSE
residuals(model)
fitted(model)
deviance(model)

# Normal Probability Plot for Residuals
lm.std <- rstandard(model)
qqnorm(
  lm.std,
  ylab = "Standardized Residuals",
  xlab = "Nornal Scores",
  main = "Normal Probability Plot for Residuals"
)
qqline(lm.std)


# Backward Stepwise Regression
model <- lm(
  mdev ~ .,
  data = Boston
)
summary(model)
model1 <- stepAIC(
  model,
  direction = "backward",
  trace = FALSE
)
summary(model1)

# Cooks Distance
model <- lm(
  medv ~ .,
  data = Boston
)
dist <- cooks.distance(model)
cooks.distance(model)[which.max(cooks.distance(model))]
dist

influential <- dist[(dist > 3 * mean(dist))]
names_of_influential <- names(influential)
names_of_influential
length(names_of_influential)

outliers <- Boston[names_of_influential, ]
outliers

data_wo_out <- Boston %>% anti_join(outliers)
data_wo_out

# Model wothout outlier
model <- lm(mdec ~ ., data = data_wo_out)
summary(model)
