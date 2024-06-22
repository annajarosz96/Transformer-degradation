library(MASS)
library(ggplot2)
library(readxl)
library(gridExtra)

# Read the data from the Excel file
data <- read_excel("Dataset.xlsx")

# Combine the extracted data into a single data frame
df <- data.frame(
  time = data[[2]],  # Assuming time is in the 2nd column
  voltage_phase_1 = data[[3]],
  voltage_phase_2 = data[[4]],
  voltage_phase_3 = data[[5]],
  current_phase_1 = data[[6]],
  current_phase_2 = data[[7]],
  current_phase_3 = data[[8]],
  active_power_phase_1 = data[[12]],
  active_power_phase_2 = data[[13]],
  active_power_phase_3 = data[[14]]
)

# Create a new variable for power decrease based on voltage increase
df$power_decrease <- (df$active_power_phase_1 + df$active_power_phase_2 + df$active_power_phase_3) / (df$voltage_phase_1 + df$voltage_phase_2 + df$voltage_phase_3)

# Fit linear regression models for each phase
linear_model_phase_1 <- lm(power_decrease ~ voltage_phase_1, data = df)
linear_model_phase_2 <- lm(power_decrease ~ voltage_phase_2, data = df)
linear_model_phase_3 <- lm(power_decrease ~ voltage_phase_3, data = df)

# Obtain the summary of the models
print(summary(linear_model_phase_1))
print(summary(linear_model_phase_2))
print(summary(linear_model_phase_3))

print(aic_lm_phase_1 <- AIC(linear_model_phase_1))
print(bic_lm_phase_1 <- BIC(linear_model_phase_1))
print(goodness_of_fit_lm_phase_1 <- summary(linear_model_phase_1)$r.squared)

print(aic_lm_phase_2 <- AIC(linear_model_phase_2))
print(bic_lm_phase_2 <- BIC(linear_model_phase_2))
print(goodness_of_fit_lm_phase_2 <- summary(linear_model_phase_2)$r.squared)

print(aic_lm_phase_3 <- AIC(linear_model_phase_3))
print(bic_lm_phase_3 <- BIC(linear_model_phase_3))
print(goodness_of_fit_lm_phase_3 <- summary(linear_model_phase_3)$r.squared)

# Plot the relationship between voltage and power decrease for each phase using linear regression
plot_phase_1 <- ggplot(df, aes(x = voltage_phase_1, y = power_decrease)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Voltage Phase 1", y = "Power Decrease") +
  theme_minimal() +
  annotate("text", x = Inf, y = -Inf, hjust = 1, label = paste("Time Range:", min(df$time), "-", max(df$time)))

plot_phase_2 <- ggplot(df, aes(x = voltage_phase_2, y = power_decrease)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Voltage Phase 2", y = "Power Decrease") +
  theme_minimal() +
  annotate("text", x = Inf, y = -Inf, hjust = 1, label = paste("Time Range:", min(df$time), "-", max(df$time)))

plot_phase_3 <- ggplot(df, aes(x = voltage_phase_3, y = power_decrease)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Voltage Phase 3", y = "Power Decrease") +
  theme_minimal() +
  annotate("text", x = Inf, y = -Inf, hjust = 1, label = paste("Time Range:", min(df$time), "-", max(df$time)))

# Show the regression plots side by side
print(grid.arrange(plot_phase_1, plot_phase_2, plot_phase_3, ncol = 3))