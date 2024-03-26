install.packages("randomForest")
library(randomForest)
library(MLmetrics)
library(MASS)
library(catdata)

# Regression
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

# Training the Model with mtry = 13
rf.boston <- randomForest(
  medv ~ .,
  data = train,
  mtry = 13,
  importance = TRUE
)
rf.boston

# Plot
ypred = predict(rf.boston, test)
plot(ypred, test$medv)
abline(0, 1)
mean((ypred - test$medv)^2)

# Metrics
MAE(ypred, test$medv)
MSE(ypred, test$medv)
RMSE(ypred, test$medv)
MAPE(ypred, test$medv)
