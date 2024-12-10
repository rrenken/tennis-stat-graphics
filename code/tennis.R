
# --------------------------------------------------
# LOAD NECESSARY PACKAGES
# --------------------------------------------------

library(ggplot2)
library(ggthemes)
library(GGally)
library(car)


# --------------------------------------------------
# READ IN DATA
# --------------------------------------------------
getwd()
data <- read.csv("BGSU - Phd Data Science/2024 Fall/Graphical Statistics/tennis-stat-graphics/data/atp-tennis.csv")

#View(data)
str(data)
dim(data)



# --------------------------------------------------
# CLEAN DATA
# --------------------------------------------------

tennis <- data

# make surface factor
tennis$Surface <- factor(tennis$Surface, levels = c("Clay", "Grass", "Hard"))

# drop column with player names
tennis <- tennis[, -1]

# rename columns
colnames(tennis) <- c("surface", "returnpercent", "doublefaultpercent", "winpercent")

#View(tennis)
str(tennis)



# --------------------------------------------------
# EDA
# --------------------------------------------------


# Summary statistics
summary(tennis)

# Plot distribution of match wins by surface
ggplot(tennis, aes(x = surface, fill = surface)) +
  theme_base() + theme(plot.background=element_rect(fill="#F0F0F0", colour = "white", 
                                                    linetype="solid"), 
                       legend.position = "none") +
  geom_bar() +
  labs(title = "Number of Records on Each Surface", x = "Surface", y = "Count")

# Plot side by side boxplots of match wins by surface
ggplot(tennis, aes(x = surface, y = winpercent, fill = surface)) +
  theme_base() + theme(plot.background=element_rect(fill="#F0F0F0", colour = "white", 
                                                    linetype="solid"),
                       legend.position = "none") +
  geom_boxplot() +
  labs(title = "Win % by Surface", x = "Surface", y = "Win %")


# Pairs plot
ggpairs(tennis, 
        aes(colour = surface),
        upper = list(continuous = wrap("cor", size = 6)),
        lower = list(continuous = wrap("points", size = 3, alpha = 0.5)),
        diag = list(continuous = wrap("densityDiag", alpha = 0.7)),
        progress = TRUE) +
        theme_bw() +
  theme(
    axis.text = element_text(size = 10),  # Smaller axis text
    strip.text = element_text(size = 12),  # Smaller facet labels
    legend.position = "bottom"  # Move legend to bottom
  ) +
        scale_color_manual(values = c("red", "darkgreen", "dodgerblue")) +
        scale_fill_manual(values = c("red", "darkgreen", "dodgerblue"))


# --------------------------------------------------
# LINEAR REGRESSION MODELING
# --------------------------------------------------


########## Return Points Won % ##########

# Fit linear regression model
model <- lm(winpercent ~ returnpercent, data = tennis)
fitted <- predict(model)
residuals <- residuals(model)

# plot the scatter plot with the regression line
ggplot(tennis, aes(x = returnpercent, y = winpercent)) +
  theme_base() + theme(plot.background=element_rect(fill="#F0F0F0", colour = "white", 
                                                    linetype="solid")) +
  geom_point(size = 4, alpha = 0.75) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 2) +
  labs(title = "Win % vs. Return Points Won %", x = "Return Points Won %", y = "Win %")

summary(model)

# Plot residuals vs. fitted values
ggplot(tennis, aes(x = fitted, y = residuals)) +
  theme_base() + theme(plot.background=element_rect(fill="#F0F0F0", colour = "white", 
                                                    linetype="solid")) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 2, alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "blue", linewidth = 2) +
  labs(title = "Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals")

# QQ plot
qqPlot(model, main = "QQ Plot", envelope = FALSE)

ggplot(tennis, aes(sample = winpercent)) +
  theme_base() + theme(plot.background=element_rect(fill="#F0F0F0", colour = "white", 
                                                    linetype="solid")) +
  geom_qq(col = "blue", size = 3) +
  geom_qq_line(col = "red", size = 1) +
  labs(title = "Normal Q-Q Plot", x="Standard Normal Quantiles", y="Win %")

# shapiro wilks test
shapiro.test(residuals)




########## Double Fault % ##########

# Fit linear regression model
model_fault <- lm(winpercent ~ doublefaultpercent, data = tennis)
fitted_fault <- predict(model_fault)
residuals_fault <- residuals(model_fault)

# plot the scatter plot with the regression line
ggplot(tennis, aes(x = doublefaultpercent, y = winpercent)) +
  theme_base() + theme(plot.background=element_rect(fill="#F0F0F0", colour = "white", 
                                                    linetype="solid")) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Win % vs. Double Fault %", x = "Double Fault %", y = "Win %")

summary(model_fault)

# Plot residuals vs. fitted values
ggplot(tennis, aes(x = fitted_fault, y = residuals_fault)) +
  theme_base() + theme(plot.background=element_rect(fill="#F0F0F0", colour = "white", 
                                                    linetype="solid")) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals")

# QQ plot
qqPlot(model_fault, main = "QQ Plot")

# shapiro wilks test
shapiro.test(residuals_fault)





########## Double Fault % ##########
str(tennis)
# Fit linear regression model
model_full <- lm(winpercent ~ returnpercent + surface, data = tennis)
fitted_full <- predict(model_full)
residuals_full <- residuals(model_full)


summary(model_full)

# Plot residuals vs. fitted values
ggplot(tennis, aes(x = fitted_full, y = residuals_full)) +
  theme_base() + theme(plot.background=element_rect(fill="#F0F0F0", colour = "white", 
                                                    linetype="solid")) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 2, alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "blue", linewidth = 2) +
  labs(title = "Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals")

# QQ plot
qqPlot(model_full, main = "QQ Plot", envelope = FALSE)

# shapiro wilks test
shapiro.test(residuals_full)


library(car)
vif(model_full)
# Cook's Dist

cooks<-cooks.distance(model_full)
cooksf<-pf(cooks,2,203)

plot(tennis$winpercent,cooksf,xlim=c(0,1),
     ylim=c(0.0,.05),cex=2,
     xlab="Urea",
     ylab="F-Dist'n Percentile of Cook's Distance",
     main = "Cook's Distance Plot")


# Influence Plots

influencePlot(model_full,
              xlim=c(0,0.04),
              ylim=c(-5,5),
              id=FALSE)
title("Influence Plot with Cook's Distance", outer = TRUE)
