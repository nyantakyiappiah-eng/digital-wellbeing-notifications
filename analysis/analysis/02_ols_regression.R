# 02_ols_regression.R - Main OLS analysis (Table 1)
# digital-wellbeing-notifications pipeline

# Load data
df <- read.csv("data/mental_health_digital_behavior_data.csv")

# Main OLS model (your original model1)
model1 <- lm(digital_wellbeing_score ~ daily_screen_time_min + 
                               social_media_time_min + 
                               sleep_hours + 
                               notification_count + 
                               num_app_switches,
             data = df)

# Results table (your Table 1)
results_table <- data.frame(
  Predictor = c("Daily Screen Time (min)", "Social Media Time (min)", 
                "Sleep Hours", "Notification Count", "App Switches",
                "R²", "Adj R²", "F(df)", "N"),
  Beta = c(round(coef(model1)[2:6], 3), 
           round(summary(model1)$r.squared, 3),
           round(summary(model1)$adj.r.squared, 3),
           paste0(round(summary(model1)$fstatistic[1], 0), "(5,494)"),
           nrow(df))
)

# Export Table 1
dir.create("tables", showWarnings = FALSE)
write.csv(results_table, "tables/table1_ols.csv", row.names = FALSE)

cat("✅ OLS regression complete\n")
cat("R² =", round(summary(model1)$r.squared, 3), "\n")
print(results_table)
