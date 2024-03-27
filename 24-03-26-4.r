install.packages('gbm')
library(gbm)
library(MASS)
library(MLmetrics)

# Regression
# Boston
data(Boston)
dataset <- as.data.frame(Boston)
dataset

# Splitting the dataset into the Training set and Test set
sample <- sample(c(TRUE, FALSE),
  nrow(dataset),
  replace = TRUE,
  prob = c(0.7, 0.3)
)
train <- dataset[sample, ]
test <- dataset[!sample, ]
# train
# test

# Training the Model
boost.boston <- gbm(
  medv ~ .,
  data = train,
  distribution = "gaussian",
  n.trees = 5000,
  interaction.depth = 4,
)
boost.boston
summary(boost.boston)

# Partial Importance Plot
par(mfrow = c(1, 2))
plot(
  boost.boston,
  i = "rm"
)
plot(
  boost.boston,
  i = "lstat"
)

# Metrics
ypred <- predict(
  boost.boston,
  test
)
ypred
MAE(ypred, test$medv)
MSE(ypred, test$medv)
RMSE(ypred, test$medv)
MAPE(ypred, test$medv)

# Training the Model with shrinkage = 0.2
