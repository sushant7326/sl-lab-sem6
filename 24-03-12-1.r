library(e1071)
library(MLmetrics)

# Training Data Generation
set.seed(1)
x <- matrix(rnorm(20 * 2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x
y
x[y == 1, ] <- x[y == 1, ] + 1
plot(x, col = (3 - y))
dat <- data.frame(x = x, y = as.factor(y))
dat

# Test Data Generation
xtest <- matrix(rnorm(20 * 2), ncol = 2)
ytest <- sample(c(-1, 1), 20, rep = TRUE)
xtest
ytest
xtest[ytest == 1, ] <- xtest[ytest == 1, ] + 1
testdat <- data.frame(x = xtest, y = as.factor(ytest))
testdat

# Model Building
svmfit1 <- svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)
plot(svmfit1, dat)
summary(svmfit1)

# Support Vectors
svmfit1$index

# Confusion Matrix
ypred1 <- predict(svmfit1, testdat)
ypred1
table(predict = ypred1, truth = testdat$y)

# Metrics
ConfusionMatrix(ypred1, testdat$y)
Accuracy(ypred1, testdat$y)
Precision(ypred1, testdat$y)
Recall(ypred1, testdat$y)
F1_Score(ypred1, testdat$y)


# Model Building with Radial Kernel
svmfit1_again <- svm(y ~ ., data = dat, kernel = "radial", cost = 10, scale = FALSE)
plot(svmfit1_again, dat)
summary(svmfit1_again)

# Support Vectors
svmfit1_again$index

# Confusion Matrix
ypred1_again <- predict(svmfit1_again, testdat)
table(predict = ypred1_again, truth = testdat$y)

# Metrics
ConfusionMatrix(ypred1_again, testdat$y)
Accuracy(ypred1_again, testdat$y)
Precision(ypred1_again, testdat$y)
Recall(ypred1_again, testdat$y)
F1_Score(ypred1_again, testdat$y)


# Model Building with lower cost
svmfit2 <- svm(y ~ ., data = dat, kernel = "linear", cost = 0.1, scale = FALSE)
plot(svmfit2, dat)
summary(svmfit2)

# Support Vectors
svmfit2$index

# Confusion Matrix
ypred2 <- predict(svmfit2, testdat)
table(predict = ypred2, truth = testdat$y)

# Metrics
ConfusionMatrix(ypred2, testdat$y)
Accuracy(ypred2, testdat$y)
Precision(ypred2, testdat$y)
Recall(ypred2, testdat$y)
F1_Score(ypred2, testdat$y)


# Model after tuning
set.seed(1)
tune.out <- tune(svm, y ~ ., data = dat, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
bestmod <- tune.out$best.model
summary(bestmod)

# Support Vectors
bestmod$index

# Confusion Matrix
ypred3 <- predict(bestmod, testdat)
table(predict = ypred3, truth = testdat$y)

# Metrics
ConfusionMatrix(ypred3, testdat$y)
Accuracy(ypred3, testdat$y)
Precision(ypred3, testdat$y)
Recall(ypred3, testdat$y)
F1_Score(ypred3, testdat$y)
