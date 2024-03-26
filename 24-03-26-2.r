# Regression
library(MASS)
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
tree.boston <- rpart(
  medv ~ .,
  data = train,
  method = "anova"
)
summary(tree.boston)
tree.boston
rpart.plot(tree.boston)

# Metrics
ypred <- predict(tree.boston, test)
ypred
MAE(ypred, test$medv)
MSE(ypred, test$medv)
RMSE(ypred, test$medv)
MAPE(ypred, test$medv)

# Pruning
plotcp(tree.boston)
tree.boston$cptable
index <- which.min(tree.boston$cptable[, "xerror"])
cpopt <- tree.boston$cptable[index, "CP"]
cpopt
opttree.boston <- prune(tree.boston, cp = cpopt)
rpart.plot(opttree.boston)

# Metrics
ypred1 <- predict(opttree.boston, test)
ypred1
MAE(ypred1, test$medv)
MSE(ypred1, test$medv)
RMSE(ypred1, test$medv)
MAPE(ypred1, test$medv)