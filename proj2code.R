df = read.csv("synthetic_health_wearables_240.csv")

# Select only numeric columns
numeric_df <- df[, sapply(df, is.numeric)]

# Print the new data frame
print(numeric_df)


pairs(numeric_df, pch=19, col='orange', lower.panel=panel.smooth)