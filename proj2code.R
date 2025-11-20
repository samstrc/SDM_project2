link = "remote_work.csv"
df <- read.csv(link)
# Select only numeric columns
numeric_df <- df[, sapply(df, is.numeric)]

# Print the new data frame
pairs(numeric_df, pch=19, col='orange', lower.panel=panel.smooth)

M <- cor(numeric_df, use = "complete.obs", method = "pearson")
round(M, 2)
