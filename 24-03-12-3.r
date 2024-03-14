# Regression
library(e1071)
library(MASS)
dataset <- as.data.frame(Boston)
dataset

library(caTools)
split <- sample.split(dataset$medv, SplitRatio = 0.75)
trainset <- subset(dataset, split == TRUE)
testset <- subset(dataset, split == FALSE)
trainset
testset

# Model Building
svm.fit <- svm(medv ~ ., data = trainset, kernel = "linear", cost = 10, scale = FALSE)
summary(svm.fit)

# R^2 Value on trainset
ypred_train <- predict(svm.fit, trainset)
R2_Score(ypred_train, trainset$medv)

# Metrics
ypred <- predict(svm.fit, testset)
MAE(ypred, testset$medv)
MSE(ypred, testset$medv)
RMSE(ypred, testset$medv)
MAPE(ypred, testset$medv)

# Model after tuning
set.seed(1)
tune.out <- tune(svm, medv ~ ., data = trainset, kernel = "linear")
summary(tune.out)
bestmod <- tune.out$best.model
summary(bestmod)

# R^2 Value on trainset
ypred_train <- predict(bestmod, trainset)
R2_Score(ypred_train, trainset$medv)

# Metrics
ypred <- predict(bestmod, testset)
MAE(ypred, testset$medv)
MSE(ypred, testset$medv)
RMSE(ypred, testset$medv)
MAPE(ypred, testset$medv)
