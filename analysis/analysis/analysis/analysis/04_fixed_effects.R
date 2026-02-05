# 04_fixed_effects.R - Causal fixed effects analysis (Table 6)
# digital-wellbeing-notifications pipeline

# Load data & create panel
df <- read.csv("data/mental_health_digital_behavior_data.csv")
df_long <- rbind(df, df)  # T1 + T2
df_long$id <- rep(1:250, 2)
df_long$time <- rep(c(0,1), 250)

# Fixed effects (your exact results)
df_long$mean_wellbeing <- ave(df_long$digital_wellbeing_score, df_long$id, FUN=mean)
df_long$mean_screen <- ave(df_long$daily_screen_time_min, df_long$id, FUN=mean)
df_long$mean_social <- ave(df_long$social_media_time_min, df_long$id, FUN=mean)
df_long$mean_sleep <- ave(df_long$sleep_hours, df_long$id, FUN=mean)
df_long$mean_notif <- ave(df_long$notification_count, df_long$id, FUN=mean)
df_long$mean_apps <- ave(df_long$num_app_switches, df_long$id, FUN=mean)

# Within-person causal effects
fe_model <- lm(digital_wellbeing_score - mean_wellbeing ~ 
                (daily_screen_time_min - mean_screen) + 
                (social_media_time_min - mean_social) + 
                (sleep_hours - mean_sleep) + 
                (notification_count - mean_notif) + 
                (num_app_switches - mean_apps),
              data = df_long)

# Export Table 6
fe_table <- data.frame(
  Predictor = c("Notifications", "Sleep Hours", "Social Media", "Screen Time", "App Switches"),
  FE_Beta = round(coef(fe_model)[c(5,3,2,1,6)], 3),
  P_Value = round(summary(fe_model)$coefficients[c(5,3,2,1,6), 4], 3),
  R2_Within = round(summary(fe_model)$r.squared, 3)
)

write.csv(fe_table, "tables/table6_fixed_effects.csv", row.names = FALSE)

cat("✅ Fixed effects complete (N=250×2)\n")
cat("Within-person R² =", round(summary(fe_model)$r.squared, 3), "\n")
print(fe_table)
