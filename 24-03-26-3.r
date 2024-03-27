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
ypred <- predict(rf.boston, test)
plot(ypred, test$medv)
abline(0, 1)
mean((ypred - test$medv)^2)

# Metrics
MAE(ypred, test$medv)
MSE(ypred, test$medv)
RMSE(ypred, test$medv)
MAPE(ypred, test$medv)


# Training the Model with ntree = 25
rf.boston <- randomForest(
  medv ~ .,
  data = train,
  mtry = 13,
  ntree = 25,
  importance = TRUE
)
rf.boston

# Plot
ypred <- predict(rf.boston, test)
plot(ypred, test$medv)
abline(0, 1)
mean((ypred - test$medv)^2)

# Metrics
MAE(ypred, test$medv)
MSE(ypred, test$medv)
RMSE(ypred, test$medv)
MAPE(ypred, test$medv)

# Variable Importance
importance(rf.boston)
varImpPlot(rf.boston)


# Training the Model with mtry = 6



### Classification

# Heart Data
data(heart)
heart_data <- as.data.frame(heart)
head(heart_data)

# Splitting the dataset into the Training set and Test set
sample <- sample(c(TRUE, FALSE),
  nrow(heart_data),
  replace = TRUE,
  prob = c(0.7, 0.3)
)
train <- heart_data[sample, ]
test <- heart_data[!sample, ]
# train
# test

# Training the Model
rf.heart <- randomForest(
  factor(y),
  data = train,
)
summary(rf.heart)
rf.heart

# Confusion Matrix
ypred <- predict(
  rf.heart,
  test,
  type = "class"
)
ypred
table(
  predict = ypred,
  truth = test$y
)

# Metrics
ConfusionMatrix(ypred, test$y)
Accuracy(ypred, test$y)
Precision(ypred, test$y)
Recall(ypred, test$y)
F1_Score(ypred, test$y)