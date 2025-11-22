link = "https://raw.githubusercontent.com/samstrc/SDM_project2/refs/heads/main/remote_work.csv"
df <- read.csv(link)
# Select only numeric columns
numeric_df <- df[, sapply(df, is.numeric)]

# Print the new data frame pair plot
# pairs(numeric_df, pch = 19, col = 'orange', lower.panel = panel.smooth)

M <- cor(numeric_df, use = "complete.obs", method = "pearson")
round(M, 2)

## CREATE BINARY TURNOVER VARIABLE

median_turnover <- median(df$turnover_risk, na.rm = TRUE)

df$high_turnover <- ifelse(df$turnover_risk > median_turnover, 1, 0)
df$high_turnover <- factor(df$high_turnover, levels = c(0, 1))

## LINEAR REGRESSION
## productivity_index ~ sleep_hours + work_hours_per_day + distractions_per_hour

lin_full <- lm(
    productivity_index ~ sleep_hours +
        work_hours_per_day +
        distractions_per_hour,
    data = df
)

summary(lin_full)

# Reduced linear model (nested)
lin_reduced <- lm(productivity_index ~ sleep_hours, data = df)

# ANOVA comparison (nested linear models)
anova(lin_reduced, lin_full)

## VISUALIZATIONS FOR LINEAR MODEL
par(mfrow = c(1, 3))

plot(
    df$sleep_hours,
    df$productivity_index,
    xlab = "Sleep Hours",
    ylab = "Productivity Index",
    main = "Productivity vs Sleep"
)

plot(
    df$work_hours_per_day,
    df$productivity_index,
    xlab = "Work Hours per Day",
    ylab = "Productivity Index",
    main = "Productivity vs Work Hours"
)

plot(
    df$distractions_per_hour,
    df$productivity_index,
    xlab = "Distractions per Hour",
    ylab = "Productivity Index",
    main = "Productivity vs Distractions"
)

par(mfrow = c(1, 1))

## LOGISTIC REGRESSION (using GLM)
## high_turnover ~ sleep_hours + distractions_per_hour

log_full <- glm(
    high_turnover ~ sleep_hours +
        distractions_per_hour,
    data = df,
    family = binomial
)

summary(log_full)

# Reduced logistic model
log_reduced <- glm(high_turnover ~ sleep_hours, data = df, family = binomial)

# ANOVA comparison (likelihood ratio test)
anova(log_reduced, log_full, test = "Chisq")

## VISUALIZATIONS FOR LOGISTIC PREDICTORS

par(mfrow = c(1, 2))

plot(
    df$sleep_hours,
    df$turnover_risk,
    xlab = "Sleep Hours",
    ylab = "Turnover Risk",
    main = "Turnover Risk vs Sleep"
)

plot(
    df$distractions_per_hour,
    df$turnover_risk,
    xlab = "Distractions per Hour",
    ylab = "Turnover Risk",
    main = "Turnover Risk vs Distractions"
)

par(mfrow = c(1, 1))
