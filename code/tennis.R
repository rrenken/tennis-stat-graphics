
# --------------------------------------------------
# LOAD NECESSARY PACKAGES
# --------------------------------------------------

library(ggplot2)
library(GGally)
library(car)


# --------------------------------------------------
# READ IN DATA
# --------------------------------------------------

data <- read.csv("data/atp-tennis.csv")

View(data)
str(data)
dim(data)



# --------------------------------------------------
# CLEAN DATA
# --------------------------------------------------

tennis <- data

# make surface factor
tennis$Surface <- factor(tennis$Surface, levels = c("Clay", "Grass", "Hard"))\

# drop column with player names
tennis <- tennis[, -1]

# rename columns
colnames(tennis) <- c("surface", "returnpercent", "doublefaultpercent", "winpercent")

View(tennis)
str(tennis)



# --------------------------------------------------
# EDA
# --------------------------------------------------


# Summary statistics
summary(tennis)

# Plot distribution of match wins by surface
ggplot(tennis, aes(x = surface, fill = surface)) +
  geom_bar() +
  labs(title = "Number of Matches on Each Surface", x = "Surface", y = "Count")

# Plot side by side boxplots of match wins by surface
ggplot(tennis, aes(x = surface, y = winpercent, fill = surface)) +
  geom_boxplot() +
  labs(title = "Win % by Surface", x = "Surface", y = "Win %")


# Pairs plot
ggpairs(tennis, 
        aes(colour = surface),
        upper = list(continuous = wrap("cor", size = 7)),
        lower = list(continuous = wrap("points", size = 3, alpha = 0.5)),
        diag = list(continuous = wrap("densityDiag", alpha = 0.7)),
        progress = TRUE) +
        theme_bw() +
        scale_color_brewer(palette = "Set1") +
        scale_fill_brewer(palette = "Set1")


# --------------------------------------------------
# LINEAR REGRESSION MODELING
# --------------------------------------------------


# Fit linear regression model
model <- lm(winpercent ~ returnpercent, data = tennis)
fitted <- predict(model)
residuals <- residuals(model)

# plot the scatter plot with the regression line
ggplot(tennis, aes(x = returnpercent, y = winpercent)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Win % vs. Return Points Won %", x = "Return Points Won %", y = "Win %")

summary(model)

# Plot residuals vs. fitted values
ggplot(tennis, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals")

# QQ plot
qqPlot(model, main = "QQ Plot")

# shapiro wilks test
shapiro.test(residuals)
