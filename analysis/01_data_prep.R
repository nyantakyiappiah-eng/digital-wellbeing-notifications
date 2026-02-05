# 01_data_prep.R - Data loading & cleaning
# digital-wellbeing-notifications analysis pipeline

# Load data (download from Kaggle first)
df <- read.csv("data/mental_health_digital_behavior_data.csv")

# Verify structure (your original checks)
cat("Dataset: N =", nrow(df), "observations,", ncol(df), "variables\n")
cat("No missing data:", all(colSums(is.na(df)) == 0), "\n")
cat("Key variables:\n")
str(df)

# Save cleaned data
dir.create("data", showWarnings = FALSE)
write.csv(df, "data/clean_data.csv", row.names = FALSE)

cat("✅ Data preparation complete\n")
