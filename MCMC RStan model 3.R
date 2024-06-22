User profile photo
propose corrected working code in r with usage of brms for obtain mcmc model

# Load the required packages
library(ggplot2)
library(readxl)
library(gridExtra)
library(rstanarm)

# Read the data from the Excel file
data <- read_excel("Dataset.xlsx")

# Combine the extracted data into a single data frame
df <- data.frame(
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
df$power_decrease <- df$current_phase_1 * df$voltage_phase_1 + df$current_phase_2 * df$voltage_phase_2 + df$current_phase_3 * df$voltage_phase_3

# Rescale the power decrease variable to have a maximum value of 0.5
df$power_decrease <- df$power_decrease / max(df$power_decrease) * 0.5

# Fit Bayesian regression models for each phase
bayes_phase_1 <- stan_glm(power_decrease ~ voltage_phase_1 + I(voltage_phase_1^2), data = df, family = gaussian())
bayes_phase_2 <- stan_glm(power_decrease ~ voltage_phase_2 + I(voltage_phase_2^2), data = df, family = gaussian())
bayes_phase_3 <- stan_glm(power_decrease ~ voltage_phase_3 + I(voltage_phase_3^2), data = df, family = gaussian())

# Obtain the summary of the models
print(summary_bayes_phase_1 <- summary(bayes_phase_1))
print(summary_bayes_phase_2 <- summary(bayes_phase_2))
print(summary_bayes_phase_3 <- summary(bayes_phase_3))

# Plot the relationship between voltage and power decrease for each phase
plot_phase_1 <- ggplot(df, aes(x = voltage_phase_1, y = power_decrease)) +
  geom_point() +
  geom_smooth(method = "stan_glm", formula = y ~ x + I(x^2), method.args = list(family = gaussian()), se = FALSE) +
  labs(x = "Voltage Phase 1", y = "Power Decrease (Rescaled)") +
  theme_minimal()

plot_phase_2 <- ggplot(df, aes(x = voltage_phase_2, y = power_decrease)) +
  geom_point() +
  geom_smooth(method = "stan_glm", formula = y ~ x + I(x^2), method.args = list(family = gaussian()), se = FALSE) +
  labs(x = "Voltage Phase 2", y = "Power Decrease (Rescaled)") +
  theme_minimal()

plot_phase_3 <- ggplot(df, aes(x = voltage_phase_3, y = power_decrease)) +
  geom_point() +
  geom_smooth(method = "stan_glm", formula = y ~ x + I(x^2), method.args = list(family = gaussian()), se = FALSE) +
  labs(x = "Voltage Phase 3", y = "Power Decrease (Rescaled)") +
  theme_minimal()

# Show the rescaled Bayesian regression plots side by side
print(grid.arrange(plot_phase_1, plot_phase_2, plot_phase_3, ncol = 3))