library(ISLR)
library(dplyr)
library(ggplot2)
library(splines)

head(Wage)
dim(Wage)
# Get min/max values of age using the range() function
agelims <- Wage %>%
  select(age) %>%
  range()
agelims

head(Wage)

# Generate a sequence of age values spanning the range
age_grid <- seq(from = min(agelims), to = max(agelims))
age_grid

# Fit a regression spline using basis functions
fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)
summary(fit)

# The bs() function generates the entire matrix of basis functions
# Predict the value of the generated ages, returning the standard error using se = TRUE

pred <- predict(fit, newdata = list(age = age_grid), se = TRUE)

# Compute error bands (2*SE)
se_bands <- with(pred, cbind("upper" = fit + 2 * se.fit, "lower" = fit - 2 * se.fit))

# Plot the spline and error bands
ggplot() +
  geom_point(data = Wage, aes(x = age, y = wage)) +
  geom_line(aes(x = age_grid, y = pred$fit), color = "blue") +
  geom_ribbon(aes(x = age_grid, ymin = se_bands[, "lower"], ymax = se_bands[, "upper"]), alpha = 0.3) +
  xlim(agelims)

# Specifying knots directly : 6 basis functions
with(Wage, dim(bs(age, knots = c(25, 40, 60))))
# Specify desired degrees of freedom, select knots automatically still 6 dimensions
with(Wage, dim(bs(age, df = 6)))
with(Wage, attr(bs(age, df = 6), "knots"))

# Natural splines
fit2 <- lm(wage ~ ns(age, df = 4), data = Wage)
fit2
pred2 <- predict(fit2, newdata = list(age = age_grid), se = TRUE)

# Compute error bands (2*SE)
se_bands2 <- with(pred2, cbind("upper" = fit + 2 * se.fit, "lower" = fit - 2 * se.fit))

# Plot the spline and error bands
ggplot() +
  geom_point(data = Wage, aes(x = age, y = wage)) +
  geom_line(aes(x = age_grid, y = pred2$fit), color = "blue") +
  geom_ribbon(aes(x = age_grid, ymin = se_bands2[, "lower"], ymax = se_bands2[, "upper"]), alpha = 0.3) +
  xlim(agelims)

# Fit 2 smoothing splines
fit_smooth <- with(Wage, smooth.spline(age, wage, df = 16))
fit_smooth_cv <- with(Wage, smooth.spline(age, wage, cv = TRUE))

# Plot the smoothing splines
ggplot() +
  geom_point(data = Wage, aes(x = age, y = wage)) +
  geom_line(aes(x = fit_smooth$x, y = fit_smooth$y), color = "16 degrees of freedom") +
  geom_line(aes(x = fit_smooth_cv$x, y = fit_smooth_cv$y), color = "6.8 degrees of freedom") +
  theme(legend.position = "bottom") +
  labs(title = "Smoothing Splines", colour = "")

# GAM
library(gam)
gam1 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)
summary(gam1)

# s() function used to incicate we like to use a smoothing splines
par(mfrow = c(1, 3))
plot(gam1, se = TRUE, col = "blue")
