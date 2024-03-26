library(ISLR2)
library(boot)
library(dplyr)
library(MASS)
library(ggplot2)

install.packages("ISLR")
library(ISLR)

head(Default)
dim(Default)

# Randomly divide into training and test data
trn <- sample(dim(Default)[1], 8000)
trn
default_train <- Default[trn, ]
default_test <- Default[-trn, ]
default_test <- default_test[, -1]
head(default_test)
dim(default_test)


# Fit logistic regression model
glm.logit <- glm(
  default ~ balance + income,
  data = default_train,
  family = binomial(link = "logit")
)
glm.probit <- glm(
  default ~ balance + income,
  data = default_train,
  family = binomial(link = "probit")
)
glm.logit1 <- glm(
  default ~ balance,
  data = default_train,
  family = binomial(link = "logit")
)
summary(glm.logit)
summary(glm.probit)
summary(glm.logit1)

L1 <- logLik(glm.logit)
L1

L0 <- logLik(glm.logit1)
L0
dev <- 2 * (L1 - L0)
dev
qchisq(0.95, 1)
# income+balance is significant than only balance



glm.fit <- glm(
  default ~ student + balance + income,
  data = default_train,
  family = binomial(link = "logit")
)
summary(glm.fit)

glm.fit1 <- glm(
  default ~ student + balance,
  data = default_train,
  family = binomial(link = "logit")
)
summary(glm.fit1)

# Predict using Test Data Set
pred <- predict(glm.fit1, default_test, type = "response")
pred[1:5]
# Classify the prediction in default = "Yes" or "No"
pred_class <- ifelse(pred >= 0.5, "Yes", "No")
pred_class

# Create Confusion Matrix
cm <- table(Obs = Default[-trn, ]$default,
            Pred = pred_class)
TP <- cm[2, 2]
TP
FP <- cm[1, 2]
FP
TN <- cm[1, 1]
FN <- cm[2, 1]
sens <- TP / (TP + FN)
sens

spec <- TN / (TN + FP)
spec

f1 <- 2 * TP / (2 * TP + FP + FN)
f1
# Test Accuracy
mean(pred_class == Default[-trn, ]$default)

contrasts(Default$default)

?predict.glm
?cv.glm

# Number of values in the training data set with yes and no value
table(default_train$default)


# ggplot power
boxplot(Default$balance ~ Default$default)
boxplot(Default$income ~ Default$default)
