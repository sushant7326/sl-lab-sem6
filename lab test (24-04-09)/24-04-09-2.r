## LAB TEST
## Sushant Singh
## 21IM30025

# Importing libraries
library(tidyverse)
library(dplyr)
library(caret)
library(corrplot)
library(car)
library(MLmetrics)
library(e1071)
library(caTools)
library(glmnet)
library(rpart)
library(rpart.plot)

# Importing data
data <- read_csv("/home/sushant-singh/Comding/R/dataset/collected_data.csv", show_col_types = FALSE)

# Converting into lakhs of rupees
data <- data %>%
  mutate(`Annual Income (lakhs)` = ifelse(`Annual Family Income (in INR)` > 100.0, `Annual Family Income (in INR)`/100000, `Annual Family Income (in INR)`))

# Map educational data to numeric
edu_levels <- c("12th", "Diploma or Equivalent", "Graduate", "Post Graduate", "Doctoral Degree", "Less than High School")
edu_values <- c(12, 14, 15, 17, 25, 10)

data <- data %>%
  mutate(`Highest Education` = dplyr::recode(`Highest level of education`, !!!setNames(edu_values, edu_levels)),
         `Father's Education` = dplyr::recode(`Father's education level`, !!!setNames(edu_values, edu_levels)),
         `Mother's Education` = dplyr::recode(`Mother's education level`, !!!setNames(edu_values, edu_levels)))

# Preliminary data processing and removing outliers
cols_to_drop <- c('Timestamp', 'State', 'Institute name', 'Field of study', "Father's employment", "Mother's employment")
data = data[, !(names(data) %in% cols_to_drop)]

data <- data %>%
  mutate(Gender = as.integer(factor(Gender)),
         `Family Type` = as.integer(factor(`Family type`)),
         `Area Type` = as.integer(factor(`Area type`)),
         `Housing Situation` = as.integer(factor(`Housing situation`)),
         `Chosen Envelope` = as.integer(factor(`Chosen Envelope`)))

# Identify and remove outliers
model_multiple <- lm(`Willingness to pay (in INR)` ~ ., data = data)
dist <- cooks.distance(model_multiple)
cooks_threshold <- 3 * mean(dist)
influential <- which(dist > cooks_threshold)
data_wo_out <- data[-influential, ]

# Exploratory data analysis
cor_matrix <- cor(data_wo_out)
corrplot(cor_matrix)
heatmap(cor_matrix)

# Significant factors affecting Willingness to pay
model_multiple <- lm(`Willingness to pay (in INR)` ~ ., data = data_wo_out)
summary(model_multiple)

# Diagnostics
plot(model_multiple$fitted.values, model_multiple$residuals)
vif(model_multiple)
anova(model_multiple)

# Encoding WTP to High or Low
data_wo_out <- data_wo_out %>%
  mutate(`WTP Category` = ifelse(`Willingness to pay (in INR)` >= 1000, 'High', 'Low'))

data_wo_out$`WTP Category` <- as.factor(data_wo_out$`WTP Category`)

# Data Splitting
split <- sample.split(data_wo_out$`WTP Category`, SplitRatio = 0.8)
train <- subset(data_wo_out, split == TRUE)
test <- subset(data_wo_out, split == FALSE)

# SVM
svmfit <- svm(`WTP Category`~., data = train, kernel = 'linear', cost = 10, scale = FALSE)
pred_svm <- predict(svmfit, test)
print(ConfusionMatrix(pred_svm, test$`WTP Category`))
print(Accuracy(pred_svm, test$`WTP Category`))

# Logistic Regression
logreg <- glm(`WTP Category`~., data = train, family = binomial(link = 'logit'))
pred_lr <- predict(logreg, test, type = 'response')
pred_lr_class <- ifelse(pred_lr > 0.5, 'High', 'Low')
print(ConfusionMatrix(pred_lr_class, test$`WTP Category`))
print(Accuracy(pred_lr_class, test$`WTP Category`))

# Decision Tree
tree.data <- rpart(`WTP Category` ~ ., data = train, method = 'class')
ypred_dt <- predict(tree.data, test, type = 'class')
print(ConfusionMatrix(ypred_dt, test$`WTP Category`))
print(Accuracy(ypred_dt, test$`WTP Category`))