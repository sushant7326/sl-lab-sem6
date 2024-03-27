install.packages("catdata")
library(catdata)
library(ggplot2)

data(heart)
head(heart)
summary(heart)
dim(heart)

heart
?heart

heart_data <- as.data.frame(heart)
ggplot(
  heart_data,
  aes(x = factor(y),
      y = tobacco,
      fill = factor(y))
) + geom_boxplot()
ggplot(
  heart_data,
  aes(x = factor(y),
      y = ldl,
      fill = factor(y))
) + geom_boxplot()
ggplot(
  heart_data,
  aes(x = factor(y),
      y = sbp,
      fill = factor(y))
) + geom_boxplot()
ggplot(
  heart_data,
  aes(x = factor(y),
      y = famhist,
      fill = factor(y))
) + geom_boxplot()
ggplot(
  heart_data,
  aes(x = factor(y),
      y = alcohol,
      fill = factor(y))
) + geom_boxplot()
ggplot(
  heart_data,
  aes(x = factor(y),
      y = typea,
      fill = factor(y))
) + geom_boxplot()
ggplot(
  heart_data,
  aes(x = factor(y),
      y = obesity,
      fill = factor(y))
) + geom_boxplot()
ggplot(
  heart_data,
  aes(x = factor(y),
      y = age,
      fill = factor(y))
) + geom_boxplot()

# Histogram
hist(
  heart_data$age,
  main = "Age Distribution",
  xlab = "Age"
)

# Randomly divide into training and test data
trn <- sample(dim(heart_data)[1], 370)
trn
heart_train <- heart_data[trn, ]
heart_test <- heart_data[-trn, ]
heart_test <- heart_test[, -1]
head(heart_test)
dim(heart_test)

head(heart_train)
glm.logit <- glm(
  y ~ sbp + tobacco + ldl + adiposity + famhist +
    typea + obesity + alcohol + age,
  data = heart_train,
  family = binomial(link = "logit")
)
summary(glm.logit)

glm.logit1 <- glm(
  y ~ tobacco + ldl + famhist + typea + age,
  data = heart_train,
  family = binomial(link = "logit")
)
summary(glm.logit1)

L1 <- logLik(glm.logit)
L1

L0 <- logLik(glm.logit1)
L0
dev <- 2 * (L1 - L0)
dev
qchisq(0.95, 1)
# Difference is not significant, logit1 is good enough

# Predict using test data set
pred <- predict(
  glm.logit1,
  heart_test,
  type = "response"
)
pred[1:5]
# Classify the prediction in default = "Yes" or "No"
pred_class <- ifelse(pred >= 0.5, "1", "0")
pred_class
cm <- table(
  Obs = heart_data[-trn, ]$y,
  Pred = pred_class
)
cm
TP <- cm[2, 2]
TP
FP <- cm[1, 2]
FP
TN <- cm[1, 1]
TN
FN <- cm[2, 1]
FN
sens <- TP / (TP + FN)
sens
spec <- TN / (TN + FP)
spec
f1 <- 2 * TP / (2 * TP + FP + FN)
f1

# Test Accuracy
mean(pred_class == heart_data[-trn, ]$y)
