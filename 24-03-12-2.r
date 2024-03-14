library(e1071)
dataset <- as.data.frame(iris)
dataset

library(caTools)
split <- sample.split(dataset$Species, SplitRatio = 0.75)
trainset <- subset(dataset, split == TRUE)
testset <- subset(dataset, split == FALSE)
trainset
testset

svm.fit <- svm(Species ~ ., data = trainset, kernel = "linear", cost = 10, scale = FALSE)
summary(svm.fit)

# Confusion Matrix
ypred <- predict(svm.fit, testset)
ypred
table(predict = ypred, truth = testset$Species)

# Plot the model
plot(svm.fit, data = trainset, Petal.Width ~ Petal.Length)

# <etrixs
ConfusionMatrix(ypred, testset$Species)
Accuracy(ypred, testset$Species)
Precision(ypred, testset$Species)
Recall(ypred, testset$Species)
F1_Score(ypred, testset$Species)
