install.packages("rpart")
install.packages("rpart.plot")
library(catdata)
library(rpart)
library(rpart.plot)
library(MLmetrics)

## CLASSIFICATION

# Heartddata
data(heart)
heart_data <- as.data.frame(heart)
heart_data

# Splitting the dataset into the Training set and Test set
sample <- sample(c(TRUE, FALSE),
    nrow(heart_data),
    replace = TRUE,
    prob = c(0.7, 0.3)
)
train <- heart_data[sample, ]
test <- heart_data[!sample, ]

# Training the Model
tree.heart <- rpart(
    y ~ sbp + tobacco +
        ldl + adiposity + factor(famhist) +
        typea + obesity + alcohol + age,
    data = train, method = "class"
)

summary(tree.heart)
tree.heart
rpart.plot(tree.heart)

# Confusion Matrix
ypred <- predict(tree.heart, test, type = "class")
ypred
table(predict = ypred, truth = test$y)

# Metrics
ConfusionMatrix(ypred, test$y)
Accuracy(ypred, test$y)
Precision(ypred, test$y)
Recall(ypred, test$y)
F1_Score(ypred, test$y)

# Pruning
plotcp(tree.heart)
tree.heart$cptable
index <- which.min(tree.heart$cptable[, "xerror"])
cpopt <- tree.heart$cptable[index, "CP"]
cpopt
opttree.heart <- prune(tree.heart, cp = cpopt)
rpart.plot(opttree.heart)

# Confusion Matrix
ypred <- predict(opttree.heart, test, type = "class")
ypred
table(predict = ypred, truth = test$y)

# Metrics
ConfusionMatrix(ypred, test$y)
Accuracy(ypred, test$y)
Precision(ypred, test$y)
Recall(ypred, test$y)
F1_Score(ypred, test$y)