# LungCapData2
lcd2 <- read.table(
    file =
        "/home/sushant-singh/Comding/R/dataset/LungCapData2.csv",
    sep = ",", header = T
)
head(lcd2)
summary(lcd2)

# Make a plot of LungCap vs Height
attach(lcd2)
plot(Height, LungCap, main = "Height vs LungCap", las = 1)
plot(Height, LungCap, main = "Polynomial Regression", las = 2)

# Now, lets fit linear regresiion
model1 <- lm(LungCap ~ Height)
summary(model1)

# See how attach function is basically used, on detaching the dataset we need to explicitly mention the dataset every time

# and add the line to the plot and make it thick and red
abline(model1, lwd = 3, col = "red")

# Fitting a polynomial regression model with degree 2
model2 <- lm(LungCap ~ Height + I(Height^2))
summary(model2)

lines(smooth.spline(Height, predict(model2)), col = "blue", lwd = 3)

# Different ways to fit a polynomial regression model
HeightSq <- Height^2
model2again <- lm(LungCap ~ Height + HeightSq)
summary(model2again)

# or use poly command
model2returns <- lm(LungCap ~ poly(Height, degree = 2, raw = T))
summary(model2returns)

# Test if the model including Height^2 is significantly better thant the one with just Height
anova(model1, model2)

# Trying with Height^3
model3 <- lm(LungCap ~ poly(Height, degree = 3, raw = T))
summary(model3)
lines(smooth)
lines(smooth.spline(Height, predict(model3)), col = "green", lwd = 3, lty = 3)

legend(46, 15, legend = c("model1: linear", "model2:poly x^2", "model3:poly x^3"), col = c("red", "blue", "green"), lty = c(1, 2, 3), lwd = 3, bty = "n", cex = 0.9)
# Improvement check
anova(model2, model3)
