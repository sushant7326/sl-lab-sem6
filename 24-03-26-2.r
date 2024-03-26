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