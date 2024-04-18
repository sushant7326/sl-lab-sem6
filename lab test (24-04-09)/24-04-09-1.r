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
# Divide Annual income by 100000, if it's greater than 100 (?)
data <- data %>%
  mutate(`Annual family income` = ifelse(`Annual family income` > 100.0, `Annual family income`/100000,`Annual family income`))

# Map educational data to numeric
# Highest level of education
# Mother's education level
# Father's education level
edu <- unique(data$`Highest level of education`)
f_edu <- unique(data$`Father's education level`)
m_edu <- unique(data$`Mother's education level`)

edu_new <- c(15,12,0,17)
edu_new_f <- c(17,15,12,25,14,10,10)
edu_new_m <- c(15,15,17,12,10,10,17,14,25,15,17)

for (i in 1:4){
  data$`Highest level of education` <- replace(data$`Highest level of education`, data$`Highest level of education` == edu[i] , edu_new[i])
}

for (i in 1:11){
  data$`Mother's education level` <- replace(data$`Mother's education level`, data$`Mother's education level` == m_edu[i] , edu_new_m[i])
}

for (i in 1:7){
  data$`Father's education level` <- replace(data$`Father's education level`, data$`Father's education level` == f_edu[i] , edu_new_f[i])
}

data$`Mother's education level` = as.integer(data$`Mother's education level`)
data$`Father's education level` = as.integer(data$`Father's education level`)
data$`Highest level of education` = as.integer(data$`Highest level of education`)

# Preliminary data processing on the dataset and removing outliers
# Droping unnecesary columns
drop_cols <- c('Timestamp', 'State', 'Institute name', 'Field of study', "Father's employment", "Mother's employment")
data = data[,!(names(data) %in% drop_cols)]

# Converting categorical to numerical data
data$Gender = as.integer(factor(data$Gender))
data$`Family type` = as.integer(factor(data$`Family type`))
data$`Area type` = as.integer(factor(data$`Area type`))
data$`Housing situation` = as.integer(factor(data$`Housing situation`))
data$`Chosen Envelope` = as.integer(factor(data$`Chosen Envelope`))

# Making a linreg model with all variables
model_multiple <- lm(data$`Willingness to pay (in INR)` ~ ., data = data)

# Cook's Distance
dist <- cooks.distance(model_multiple)
cooks_max <- which.max(dist)
cooks_max_value <- dist[cooks_max]
cooks_threshold <- 3 * mean(dist)

influential <- dist[dist > cooks_threshold]
names_of_influential <- names(influential)
length(names_of_influential)

outliers <- data[names_of_influential,]
outliers

data_wo_out <- data %>% anti_join(outliers)
data <- data_wo_out

# Perform exploratory data analysis to depict how willingness to pay is associated with the other variables?
# Correlation
cor(data)
corrplot(cor(data))
# Heatmap
heatmap(cor(data))
S
# Significant factors affecting someone’s Willingness to pay (Y)
# MLR on data without outliers
model_multiple <- lm(data$`Willingness to pay (in INR)` ~ ., data = data)
summary(model_multiple)

# Checking Model Accuracy
# NPP Plot
plot(model_multiple$fitted.values, model_multiple$residuals)

# VIF Test
vif(model_multiple)

# ANOVA Test
anova(model_multiple)

# Model not a good fit

# Encoding WTP to High or Low
data <- data %>%
  mutate(`Willingness to pay (in INR)` = ifelse(`Willingness to pay (in INR)` >= 1000, 'High', 'Low'))

data$`Willingness to pay (in INR)` <- as.factor(data$`Willingness to pay (in INR)`)

# Data Preparing
# Random Data Splitting
split <- sample.split(data$`Willingness to pay (in INR)`, SplitRatio = 0.8)

train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)

# SVM
svmfit <- svm(`Willingness to pay (in INR)`~., data = train, kernel = 'linear', cost = 10, scale = FALSE)

pred <- predict(svmfit, test)

ConfusionMatrix(pred, test$`Willingness to pay (in INR)`)

Accuracy(pred, test$`Willingness to pay (in INR)`)

# LogReg
logreg <- glm(`Willingness to pay (in INR)`~., data = train, family = binomial(link = 'logit'))

pred <- predict(svmfit, test)

ConfusionMatrix(pred, test$`Willingness to pay (in INR)`)

Accuracy(pred, test$`Willingness to pay (in INR)`)

# DT
# Training the model
tree.data <- rpart(`Willingness to pay (in INR)` ~ ., data = train, method = 'class')
rpart.plot(tree.data)

ypred <- predict(tree.data, test, type = 'class')

# Metrics
ConfusionMatrix(ypred, test$`Willingness to pay (in INR)`)
Accuracy(ypred, test$`Willingness to pay (in INR)`)
