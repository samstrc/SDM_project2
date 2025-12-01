# Load data from github
link <- "https://raw.githubusercontent.com/samstrc/SDM_project2/refs/heads/main/remote_work_final.csv"
df <- read.csv(link)

df$is_high_turnover <- factor(df$is_high_turnover, # Treat 0 and 1 as factors since glm in R expects binary outcome to be a factor
                              levels = c(0, 1),
                              labels = c("Low", "High"))


# SIMPLE LINEAR REGRESSION (just 1 predictor)
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


# MULTIPLE LINEAR REGRESSION (2 predictors + interaction)----------------
lin_multi <- lm(
  productivity_index ~ performance_rating * sleep_hours_avg, # Considers both & their interaction
  data = df
)
summary(lin_multi)

# ANOVA: simple vs multi model
anova(lin_simple, lin_multi) # Barely shows that the simpler model is just as good

# Shapiro–Wilk test
shapiro.test(residuals(lin_multi))

lin_sleep_only <- lm(productivity_index ~ sleep_hours_avg, data = df)

# Scatterplot for sleep hours (clean blue colors)
plot(df$sleep_hours_avg, df$productivity_index,
     xlab = "Average Sleep Hours",
     ylab = "Productivity Index",
     main = "Productivity vs Sleep Hours",
     pch = 19, col = "#3366CC")
abline(lin_sleep_only, col = "#003399", lwd = 2)


# RESIDUAL DIAGNOSTICS: BOTH LINEAR MODELS--------------

##### SIMPLE MODEL #####

par(mfrow = c(1, 3))

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

par(mfrow = c(1, 1))


##### MULTIPLE MODEL #####

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

par(mfrow = c(1, 1))


# LOGISTIC REGRESSION (2 predictors + interaction)---------------------
log_full <- glm( # Look at AIC & statistically significant predictors, test other variables
  is_high_turnover ~ work_life_balance_ratio * sleep_hours_avg,
  data = df,
  family = binomial
)
summary(log_full)

log_reduced <- glm(
  is_high_turnover ~ work_life_balance_ratio + sleep_hours_avg,
  data = df,
  family = binomial
)

anova(log_reduced, log_full, test = "Chisq")


# LOGISTIC PLOTS 

plot(df$work_life_balance_ratio, df$turnover_risk,
     xlab = "Work-Life Balance Ratio",
     ylab = "Turnover Risk (Probability)",
     main = "Turnover Risk vs Work-Life Balance Ratio",
     pch = 19, col = "#3366CC")

plot(df$sleep_hours_avg, df$turnover_risk,
     xlab = "Average Sleep Hours",
     ylab = "Turnover Risk (Probability)",
     main = "Turnover Risk vs Sleep Hours",
     pch = 19, col = "#3366CC")


# LOGISTIC REGRESSION FITTED CURVE (Option A)
# Predicted probability vs Work-Life Balance Ratio, Sleep fixed at its mean

sleep_fixed <- mean(df$sleep_hours_avg, na.rm = TRUE)

# Sequence of WLB values for smooth curve
wlb_seq <- seq(min(df$work_life_balance_ratio, na.rm = TRUE),
               max(df$work_life_balance_ratio, na.rm = TRUE),
               length.out = 200)

pred_data <- data.frame(
  work_life_balance_ratio = wlb_seq,
  sleep_hours_avg = sleep_fixed
)

# Predicted probabilities from the logistic regression model
pred_probs <- predict(log_full, newdata = pred_data, type = "response")

# Plot fitted curve with optional actual points
plot(wlb_seq, pred_probs,
     type = "l",
     lwd = 3,
     col = "#3366CC",
     xlab = "Work-Life Balance Ratio",
     ylab = "Predicted Probability of High Turnover",
     main = "Logistic Regression Fitted Curve\nSleep Held Constant at Mean")

points(df$work_life_balance_ratio, df$turnover_risk,
       pch = 19, col = rgb(0, 0, 0.6, 0.25))