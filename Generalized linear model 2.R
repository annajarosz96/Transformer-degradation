# Load required libraries
library(MASS)
library(ggplot2)
library(readxl)
library(gridExtra)

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

# Scale the power decrease to the range 0.0-0.4
df$power_decrease <- (df$current_phase_1 * df$voltage_phase_1 + df$current_phase_2 * df$voltage_phase_2 + df$current_phase_3 * df$voltage_phase_3) / max(df$current_phase_1 * df$voltage_phase_1 + df$current_phase_2 * df$voltage_phase_2 + df$current_phase_3 * df$voltage_phase_3) * 0.4

# Fit glms for each phase
glm_phase_1 <- glm(power_decrease ~ voltage_phase_1 + I(voltage_phase_1^2), data = df, family = gaussian(link = "identity"))
glm_phase_2 <- glm(power_decrease ~ voltage_phase_2 + I(voltage_phase_2^2), data = df, family = gaussian(link = "identity"))
glm_phase_3 <- glm(power_decrease ~ voltage_phase_3 + I(voltage_phase_3^2), data = df, family = gaussian(link = "identity"))

# Obtain the summary of the models
summary_phase_1 <- summary(glm_phase_1)
summary_phase_2 <- summary(glm_phase_2)
summary_phase_3 <- summary(glm_phase_3)

# Extract coefficients and quantities
coefficients_phase_1 <- summary_phase_1$coefficients
coefficients_phase_2 <- summary_phase_2$coefficients
coefficients_phase_3 <- summary_phase_3$coefficients

# Calculate Min, 1Q, Median, 3Q, and Max for coefficients
quantiles_phase_1 <- quantile(coefficients_phase_1[, 1], probs = c(0, 0.25, 0.5, 0.75, 1))
quantiles_phase_2 <- quantile(coefficients_phase_2[, 1], probs = c(0, 0.25, 0.5, 0.75, 1))
quantiles_phase_3 <- quantile(coefficients_phase_3[, 1], probs = c(0, 0.25, 0.5, 0.75, 1))

# Calculate AIC and BIC for each phase
aic_phase_1 <- AIC(glm_phase_1)
aic_phase_2 <- AIC(glm_phase_2)
aic_phase_3 <- AIC(glm_phase_3)

bic_phase_1 <- BIC(glm_phase_1)
bic_phase_2 <- BIC(glm_phase_2)
bic_phase_3 <- BIC(glm_phase_3)

# Calculate the goodness of fit (deviance) for each phase
goodness_of_fit_phase_1 <- glm_phase_1$deviance
goodness_of_fit_phase_2 <- glm_phase_2$deviance
goodness_of_fit_phase_3 <- glm_phase_3$deviance

# Plot the relationship between voltage and power decrease for each phase
plot_phase_1 <- ggplot(df, aes(x = voltage_phase_1, y = power_decrease)) +
  geom_point() +
  geom_smooth(method = "glm", formula = y ~ x + I(x^2), method.args = list(family = gaussian(link = "identity")), se = FALSE) +
  labs(x = "Voltage Phase 1", y = "Power Decrease") +
  theme_minimal()

plot_phase_2 <- ggplot(df, aes(x = voltage_phase_2, y = power_decrease)) +
  geom_point() +
  geom_smooth(method = "glm", formula = y ~ x + I(x^2), method.args = list(family = gaussian(link = "identity")), se = FALSE) +
  labs(x = "Voltage Phase 2", y = "Power Decrease") +
  theme_minimal()

plot_phase_3 <- ggplot(df, aes(x = voltage_phase_3, y = power_decrease)) +
  geom_point() + 
  geom_smooth(method = "glm", formula = y ~ x + I(x^2), method.args = list(family = gaussian(link = "identity")), se = FALSE) +
  labs(x = "Voltage Phase 3", y = "Power Decrease") +
  theme_minimal()

# Show the glm plots and the coefficient, quantity, and criteria results
print(grid.arrange(plot_phase_1, plot_phase_2, plot_phase_3, ncol = 3))
