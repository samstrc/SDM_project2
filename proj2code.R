# (1) Load data from github into a df
link <- "https://raw.githubusercontent.com/samstrc/SDM_project2/refs/heads/main/remote_work_regenerated.csv"
df <- read.csv(link)

# (2) Exploratory Data Analysis --------------------------------------
# Correlation table before we model
numeric_df <- df[sapply(df, is.numeric)]
round(cor(numeric_df, use = "pairwise.complete.obs"), 3)

# DISTRIBUTIONS FOR ALL MODEL VARIABLES
# 1. Productivity Index
hist(df$productivity_index,
     main = "Distribution of Productivity Index",
     xlab = "Productivity Index",
     col = "#99B2FF", border = "#003399")

# 2. Gender (categorical)
barplot(table(df$gender),
        main = "Gender Distribution",
        xlab = "Gender",
        col = "#99B2FF", border = "#003399")

# 3. Performance Rating
hist(df$performance_rating,
     main = "Distribution of Performance Rating",
     xlab = "Performance Rating",
     col = "#99B2FF", border = "#003399")

# 4. Sleep Hours
hist(df$sleep_hours_avg,
     main = "Distribution of Average Sleep Hours",
     xlab = "Sleep Hours (Avg)",
     col = "#99B2FF", border = "#003399")

# 5. Work-Life Balance Ratio
hist(df$work_life_balance_ratio,
     main = "Distribution of Work-Life Balance Ratio",
     xlab = "WLB Ratio",
     col = "#99B2FF", border = "#003399")

# 6. Salary
hist(df$salary,
     main = "Distribution of Salary",
     xlab = "Salary",
     col = "#99B2FF", border = "#003399")

# 7. High Turnover (0/1) (Quit or not, but professionally worded)
barplot(table(df$is_high_turnover),
        main = "High Turnover Counts",
        xlab = "0 = Low, 1 = High",
        col = "#99B2FF", border = "#003399")

# 8. Turnover risk (0 to 1) (Company-modeled chance of quitting)
hist(df$turnover_risk,
     main = "Turnover Risk",
     xlab = "Probability of turnover",
     col = "#99B2FF", border = "#003399")

# (4) ONE-WAY ANOVA: Productivity Score by Gender -----------------------
# Calculate group means and SDs
tapply(df$productivity_index, df$gender, mean)
tapply(df$productivity_index, df$gender, sd)

# Fit ANOVA model
df$gender <- as.factor(df$gender) # Make sure gender is a factor
anova_gender <- aov(productivity_index ~ gender, data = df)
summary(anova_gender)

# Effect size (Eta Squared, manual formula)
anova_tbl <- summary(anova_gender)[[1]]
SS_between <- anova_tbl["gender", "Sum Sq"]
SS_total   <- sum(anova_tbl[,"Sum Sq"])
eta_sq <- SS_between / SS_total
eta_sq  # print eta-squared

# Homogeneity of variance (Bartlett's Test)
bartlett.test(productivity_index ~ gender, data = df)

# Diagnostic Plots
par(mfrow = c(1, 2)) # Set plot to 2 figures

# Residuals vs Fitted
plot(fitted(anova_gender), residuals(anova_gender),
     pch = 19, col = "#3366CC",
     xlab = "Fitted Values", ylab = "Residuals",
     main = "ANOVA: Residuals vs Fitted")
abline(h = 0, lty = 2)

# Normal Q-Q plot of residuals
qqnorm(residuals(anova_gender),
       pch = 19, col = "#3366CC",
       main = "ANOVA: Normal Q-Q Plot")
qqline(residuals(anova_gender), col = "#003399", lwd = 2)

par(mfrow = c(1, 1))

# Boxplot by gender
boxplot(productivity_index ~ gender, data = df,
        col = "#99B2FF", border = "#003399",
        main = "Productivity Index by Gender",
        ylab = "Productivity Index")

# Tukey HSD to see exactly which differs
tukey_results <- TukeyHSD(anova_gender)
tukey_results

# Tukey plot
plot(tukey_results, col = "#3366CC")

# (5) SIMPLE LINEAR REGRESSION (just 1 predictor) ------------------------------------------------
# Response: productivity_index
# Predictor: performance_rating

# "Do people who get better job reviews have more professional output?"
lin_simple <- lm(productivity_index ~ performance_rating, data = df)
summary(lin_simple)

# Shapiro–Wilk test for normality of residuals...this matters, not always the normality of variables
shapiro.test(residuals(lin_simple)) # 0.21 means residuals are normal, small p would mean the opposite

# Scatterplot with fitted line (clean blue colors)
plot(df$performance_rating, df$productivity_index,
     xlab = "Performance Rating",
     ylab = "Productivity Index",
     main = "Productivity vs Performance Rating",
     pch = 19, col = "#3366CC")
abline(lin_simple, col = "#003399", lwd = 2)

# Check for influential points on model using cook's distance 
plot(lin_simple, which = 5) # Leverage scale is super small, red line is straight. This is good. 

par(mfrow = c(1, 3)) # Prepare to put 3 figures on one plot

# Residuals vs Fitted 
plot(lin_simple$fitted.values, residuals(lin_simple),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Simple Model:\nResiduals vs Fitted",
     pch = 19, col = "#3366CC")
abline(h = 0, lty = 2)

# QQ Plot of residuals
qqnorm(residuals(lin_simple),
       main = "Simple Model:\nNormal Q-Q Plot",
       pch = 20, col = "#3366CC", cex = 0.7)
qqline(residuals(lin_simple), col = "#003399", lwd = 2)

# Histogram of residuals
hist(residuals(lin_simple),
     breaks = 20,
     main = "Simple Model:\nHistogram of Residuals",
     xlab = "Residuals",
     col = "#99B2FF", border = "#003399")

par(mfrow = c(1, 1)) # Reset num of figures per image

# (6) MULTIPLE LINEAR REGRESSION (2 predictors + interaction, one version without interaction)-------------------------------------
lin_multi_int <- lm(
  productivity_index ~ performance_rating * sleep_hours_avg, # Includes interaction
  data = df
)

summary(lin_multi_int)

lin_multi <- lm(
  productivity_index ~ performance_rating + sleep_hours_avg,
  data = df
)

summary(lin_multi)

# ANOVA: simple vs. multi model + no inter. vs. multi model + inter.
anova(lin_simple, lin_multi, lin_multi_int)

# Correlation between predictors (multicollinearity check)
cor(df$performance_rating, df$sleep_hours_avg)

# Cook's contours plot
plot(lin_multi_int, which = 5)  

# Normality test on residuals + interaction
shapiro.test(residuals(lin_multi_int)) # pass

# Normality test on residuals (no interaction)
shapiro.test(residuals(lin_multi)) # pass

## Plot 3d model, no interaction 
## 3D regression plane
# Create a grid of predictor values
perf_seq  <- seq(min(df$performance_rating), max(df$performance_rating), length.out = 40)
sleep_seq <- seq(min(df$sleep_hours_avg),     max(df$sleep_hours_avg),     length.out = 40)

grid <- expand.grid(
  performance_rating = perf_seq,
  sleep_hours_avg    = sleep_seq
)

# Predicted values for the grid
z_pred <- matrix(
  predict(lin_multi, newdata = grid),
  nrow = length(perf_seq),
  ncol = length(sleep_seq)
)

# Draw the regression plane
p <- persp(
  x = perf_seq,
  y = sleep_seq,
  z = z_pred,
  theta = 40, phi = 25, expand = 0.6,
  col = "#99B2FF", shade = 0.45,
  xlab = "Performance Rating",
  ylab = "Sleep Hours",
  zlab = "Predicted Productivity",
  ticktype = "detailed",
  main = "3D Regression Plane\nProductivity ~ Performance + Sleep"
)

# Add the actual data points in 3D
pts <- trans3d(
  x = df$performance_rating,
  y = df$sleep_hours_avg,
  z = df$productivity_index,
  pmat = p
)

points(
  pts,
  pch = 19, cex = 1.1,
  col = rgb(0.1, 0.2, 0.9, 0.7)
)

par(mfrow = c(1, 3))

# Residuals vs Fitted
plot(lin_multi$fitted.values, residuals(lin_multi),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Multiple Model:\nResiduals vs Fitted",
     pch = 19, col = "#3366CC")
abline(h = 0, lty = 2)

# QQ Plot of residuals
qqnorm(residuals(lin_multi),
       main = "Multiple Model:\nNormal Q-Q Plot",
       pch = 20, col = "#3366CC", cex = 0.7)
qqline(residuals(lin_multi), col = "#003399", lwd = 2)

# Histogram of residuals
hist(residuals(lin_multi),
     breaks = 20,
     main = "Multiple Model:\nHistogram of Residuals",
     xlab = "Residuals",
     col = "#99B2FF", border = "#003399")

par(mfrow = c(1, 1)) # Reset figure

# (7) LOGISTIC REGRESSION (2 predictors + interaction) ------------------------------
# Work-Life Balance Ratio and Salary predicting turnover

# Full model: includes interaction
log_full <- glm(
  is_high_turnover ~ work_life_balance_ratio * salary,
  data = df,
  family = binomial
)
summary(log_full) # Colinearity affects this model, structured multicolinearity interaction worsened the affect, breaking down the model

wlbalance_sal_cor = cor(df$work_life_balance_ratio, df$salary)
wlbalance_sal_cor # Slight present, would have to be 95%+ to consider dropping a variable

# Reduced model: main effects only, no interaction, **Best model! :)
log_reduced <- glm(
  is_high_turnover ~ work_life_balance_ratio + salary,
  data = df,
  family = binomial
)
summary(log_reduced)

# Super reduced model: one predictor
log_super_reduced <- glm(
     is_high_turnover ~ work_life_balance_ratio,
     data = df,
     family = binomial
)
summary(log_super_reduced)

# Likelihood ratio test. **Tests one model vs the next, only 2 at once. Small p val implies The larger model fits significantly better than the smaller one.
anova(log_super_reduced, log_reduced, log_full, test = "Chisq")


# AIC comparison
# Akaike Information Criterion
# It measures: How well the model fits the data, how complex the model is
# Lower AIC = better
AIC(log_super_reduced, log_reduced, log_full)

# Cooks distance check
plot(log_full, which = 5)
plot(log_reduced, which = 5)
plot(log_super_reduced, which = 5)

# Exploratory plots
plot(df$work_life_balance_ratio, df$turnover_risk,
     xlab = "Work-Life Balance Ratio",
     ylab = "Turnover Risk (Probability)",
     main = "Turnover Risk vs Work-Life Balance Ratio",
     pch = 19, col = "#3366CC")

plot(df$salary, df$turnover_risk,
     xlab = "Salary",
     ylab = "Turnover Risk (Probability)",
     main = "Turnover Risk vs Salary",
     pch = 19, col = "#3366CC")

# Fitted curve: vary work-life balance, hold salary constant
salary_fixed <- mean(df$salary, na.rm = TRUE)

wlb_seq <- seq(min(df$work_life_balance_ratio, na.rm = TRUE),
               max(df$work_life_balance_ratio, na.rm = TRUE),
               length.out = 200)

pred_data <- data.frame(
  work_life_balance_ratio = wlb_seq,
  salary = salary_fixed
)

pred_probs <- predict(log_reduced, newdata = pred_data, type = "response") # Get predictions from model with lowest AIC

plot(wlb_seq, pred_probs,
     type = "l",
     lwd = 3,
     col = "#3366CC",
     xlab = "Work-Life Balance Ratio",
     ylab = "Predicted Probability of High Turnover",
     main = "Turnover Probability vs Work-Life Balance Ratio\nSalary Held Constant")

points(df$work_life_balance_ratio, df$turnover_risk,
       pch = 19, col = rgb(0, 0, 0.6, 0.25))

