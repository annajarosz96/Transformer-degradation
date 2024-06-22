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

# Fit Poisson regression models for each phase
poisson_model_phase_1 <- glm(power_decrease ~ voltage_phase_1, data = df, family = poisson())
poisson_model_phase_2 <- glm(power_decrease ~ voltage_phase_2, data = df, family = poisson())
poisson_model_phase_3 <- glm(power_decrease ~ voltage_phase_3, data = df, family = poisson())

# Obtain the summary of the models
print(summary(poisson_model_phase_1))
print(summary(poisson_model_phase_2))
print(summary(poisson_model_phase_3))

# Fit Poisson regression models for each phase
poisson_model_phase_1 <- glm(power_decrease ~ voltage_phase_1, data = df, family = poisson())
poisson_model_phase_2 <- glm(power_decrease ~ voltage_phase_2, data = df, family = poisson())
poisson_model_phase_3 <- glm(power_decrease ~ voltage_phase_3, data = df, family = poisson())

# Function to calculate summary statistics
calculate_summary_statistics <- function(model) {
  coefficients <- coef(model)
  summary_stats <- c(min(coefficients), quantile(coefficients, probs = c(0.25, 0.5, 0.75)), max(coefficients))
  return(summary_stats)
}

# Obtain the summary statistics for each phase
summary_stats_phase_1 <- calculate_summary_statistics(poisson_model_phase_1)
summary_stats_phase_2 <- calculate_summary_statistics(poisson_model_phase_2)
summary_stats_phase_3 <- calculate_summary_statistics(poisson_model_phase_3)



# Print the summary statistics for each phase
print("Summary Statistics for Phase 1:")
print(summary_stats_phase_1)
print("Summary Statistics for Phase 2:")
print(summary_stats_phase_2)
print("Summary Statistics for Phase 3:")
print(summary_stats_phase_3)

# Fit Poisson regression models for each phase
poisson_model_phase_1 <- glm(power_decrease ~ voltage_phase_1, data = df, family = poisson())
poisson_model_phase_2 <- glm(power_decrease ~ voltage_phase_2, data = df, family = poisson())
poisson_model_phase_3 <- glm(power_decrease ~ voltage_phase_3, data = df, family = poisson())

# Obtain the AIC, BIC, and goodness-of-fit for the models
print(goodness_of_fit_lm_phase_1 <- summary(poisson_model_phase_1)$deviance)
print(goodness_of_fit_lm_phase_2 <- summary(poisson_model_phase_2)$deviance)
print(goodness_of_fit_lm_phase_3 <- summary(poisson_model_phase_3)$deviance)

# Obtain the log-likelihood, number of parameters, and number of data points (n) for each phase
log_likelihood_phase_1 <- logLik(poisson_model_phase_1)
num_params_phase_1 <- length(coef(poisson_model_phase_1))
n_phase_1 <- nrow(df)

log_likelihood_phase_2 <- logLik(poisson_model_phase_2)
num_params_phase_2 <- length(coef(poisson_model_phase_2))
n_phase_2 <- nrow(df)

log_likelihood_phase_3 <- logLik(poisson_model_phase_3)
num_params_phase_3 <- length(coef(poisson_model_phase_3))
n_phase_3 <- nrow(df)

# Calculate AIC and BIC for each phase
print(aic_phase_1 <- 2 * num_params_phase_1 - 2 * log_likelihood_phase_1)
print(bic_phase_1 <- num_params_phase_1 * log(n_phase_1) - 2 * log_likelihood_phase_1)

print(aic_phase_2 <- 2 * num_params_phase_2 - 2 * log_likelihood_phase_2)
print(bic_phase_2 <- num_params_phase_2 * log(n_phase_2) - 2 * log_likelihood_phase_2)

print(aic_phase_3 <- 2 * num_params_phase_3 - 2 * log_likelihood_phase_3)
print(bic_phase_3 <- num_params_phase_3 * log(n_phase_3) - 2 * log_likelihood_phase_3)



# Plot the relationship between voltage and power decrease for each phase
plot_phase_1 <- ggplot(df, aes(x = voltage_phase_1, y = power_decrease)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = poisson()), se = FALSE) +
  labs(x = "Voltage Phase 1", y = "Power Decrease") +
  theme_minimal() +
  annotate("text", x = Inf, y = -Inf, hjust = 1, label = paste("Time Range:", min(df$time), "-", max(df$time)))

plot_phase_2 <- ggplot(df, aes(x = voltage_phase_2, y = power_decrease)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = poisson()), se = FALSE) +
  labs(x = "Voltage Phase 2", y = "Power Decrease") +
  theme_minimal() +
  annotate("text", x = Inf, y = -Inf, hjust = 1, label = paste("Time Range:", min(df$time), "-", max(df$time)))

plot_phase_3 <- ggplot(df, aes(x = voltage_phase_3, y = power_decrease)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = poisson()), se = FALSE) +
  labs(x = "Voltage Phase 3", y = "Power Decrease") +
  theme_minimal() +
  annotate("text", x = Inf, y = -Inf, hjust = 1, label = paste("Time Range:", min(df$time), "-", max(df$time)))

# Show the regression plots side by side
print(grid.arrange(plot_phase_1, plot_phase_2, plot_phase_3, ncol = 3))


