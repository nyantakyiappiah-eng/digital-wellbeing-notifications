# 03_bootstrap.R - Bootstrap confidence intervals (Table 4)
# digital-wellbeing-notifications pipeline

# Load data
df <- read.csv("data/mental_health_digital_behavior_data.csv")

# Bootstrap (your exact 1000-rep analysis)
set.seed(123)
n_boot <- 1000
boot_betas <- matrix(NA, n_boot, 5)

for(i in 1:n_boot){
  boot_sample <- sample(1:nrow(df), replace=TRUE)
  boot_df <- df[boot_sample, ]
  
  boot_model <- lm(digital_wellbeing_score ~ daily_screen_time_min + 
                                     social_media_time_min + 
                                     sleep_hours + 
                                     notification_count + 
                                     num_app_switches,
                   data = boot_df)
  
  boot_betas[i,] <- coef(boot_model)[2:6]
}

# Bootstrap CI table (your results)
boot_table <- data.frame(
  Predictor = c("Sleep Hours", "Notifications", "Social Media", 
                "Screen Time", "App Switches"),
  Lower_CI = round(quantile(boot_betas[,3], 0.025), 3),
  Upper_CI = round(quantile(boot_betas[,3], 0.975), 3),
  stringsAsFactors = FALSE
)

# Export Table 4
write.csv(boot_table, "tables/table4_bootstrap.csv", row.names = FALSE)

cat("✅ Bootstrap complete (N=1000)\n")
print(boot_table)
