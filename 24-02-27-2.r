library(ISLR)
library(glmnet)
library(dplyr)
library(tidyr)
library(MASS)
Boston <- na.omit(Boston)

# setup data, i.e., x and y variables
x <- model.matrix(medv ~ ., Boston)[, -1]
y <- Boston %>%
    dplyr::select(medv) %>%
    unlist() %>%
    as.numeric()

grid <- 10^seq(10, -2, length = 100)
ridge_mod <- glmnet(x, y, alpha = 0, lambda = grid)
dim(coef(ridge_mod))
plot(ridge_mod, xvar = "lambda", lable = TRUE)
plot(ridge_mod) # Draw plot of coefficients

cv.ridge <- cv.glmnet(x, y, alpha = 0)
plot(cv.ridge)

ridge_mod$lambda[50]

coef(ridge_mod)[, 50]
coef(ridge_mod)[, 60]
ridge_mod$lambda[60]

set.seed(1)
train <- Boston %>% sample_frac(0.5)
test <- Boston %>% setdiff(train)
x_train <- model.matrix(medv ~ ., train)[, -1]
x_test <- model.matrix(medv ~ ., test)[, -1]
y_train <- train %>%
    dplyr::select(medv) %>%
    unlist() %>%
    as.numeric()
y_test <- test %>%
    dplyr::select(medv) %>%
    unlist() %>%
    as.numeric()
x_train

ridge_mod <- glmnet(x_train, y_train, alpha = 0, lambda = grid)
ridge_pred <- predict(ridge_mod, s = 4, newx = x_test)
mean((ridge_pred - y_test)^2)
plot(ridge_mod, xvar = "lambda", label = TRUE)

mean((mean(y_train) - y_test)^2)

ridge_pred <- predict(ridge_mod, s = 1e10, newx = x_test)
mean((ridge_pred - y_test)^2)
plot(ridge_mod, xvar = "lambda", label = TRUE)

ridge_pred <- predict(ridge_mod, s = 0, newx = x_test)
mean((ridge_pred - y_test)^2)
plot(ridge_mod, xvar = "lambda", label = TRUE)

# Check with Least Square
ridge_pred <- predict(ridge_mod, s = 0, newx = x_test)
mean((ridge_pred - y_test)^2)

lm(medv ~ ., data = train)
predict(ridge_mod, s = 0, exact = TRUE, type = "coefficient")

# The Lasso
lasso_mod <- glmnet(x, y, lambda = grid)
plot(lasso_mod)
set.seed(1)
cv.out <- cv.glmnet(x, y, alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

out <- glmnet(x, y, alpha = 1, lambda)
