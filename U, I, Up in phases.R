# Load required libraries
library(readxl)
library(ggplot2)
library(gridExtra)

# Replace "Dataset.xlsx" with the actual file path
data <- read_excel("Dataset.xlsx")

# Combine the extracted data into a single data frame
df <- data.frame(
  voltage_phase_1 = data[[3]],
  voltage_phase_2 = data[[4]],
  voltage_phase_3 = data[[5]],
  current_phase_1 = data[[6]],
  current_phase_2 = data[[7]],
  current_phase_3 = data[[8]],
  multiphase_voltage_1_2 = data[[9]],
  multiphase_voltage_2_3 = data[[10]],
  multiphase_voltage_3_1 = data[[11]]
)

# Create individual plots for each phase with bold, smaller, and centered titles
plot_phase_1 <- ggplot(df, aes(x = 1:nrow(df))) +
  geom_line(aes(y = voltage_phase_1), color = "black", linetype = "solid") + # Black: Voltage
  geom_line(aes(y = multiphase_voltage_1_2), color = "darkblue", linetype = "solid") + # Dark Blue: Multiphase Voltage
  geom_line(aes(y = current_phase_1), color = "blue", linetype = "solid") + # Blue: Current
  labs(
    title = "Phase 1",
    x = "Measure",
    y = "Values"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = -1),  # Bold, smaller, and centered title
    legend.position = "none"  # Remove legend
  )

plot_phase_2 <- ggplot(df, aes(x = 1:nrow(df))) +
  geom_line(aes(y = voltage_phase_2), color = "black", linetype = "solid") + # Black: Voltage
  geom_line(aes(y = multiphase_voltage_2_3), color = "darkblue", linetype = "solid") + # Dark Blue: Multiphase Voltage
  geom_line(aes(y = current_phase_2), color = "blue", linetype = "solid") + # Blue: Current
  labs(
    title = "Phase 2",
    x = "Measure",
    y = "Values"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = -1),  # Bold, smaller, and centered title
    legend.position = "none"  # Remove legend
  )

plot_phase_3 <- ggplot(df, aes(x = 1:nrow(df))) +
  geom_line(aes(y = voltage_phase_3), color = "black", linetype = "solid") + # Black: Voltage
  geom_line(aes(y = multiphase_voltage_3_1), color = "darkblue", linetype = "solid") + # Dark Blue: Multiphase Voltage
  geom_line(aes(y = current_phase_3), color = "blue", linetype = "solid") + # Blue: Current
  labs(
    title = "Phase 3",
    x = "Measure",
    y = "Values"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = -1),  # Bold, smaller, and centered title
    legend.position = "none"  # Remove legend
  )

# Arrange the plots into three columns
grid.arrange(plot_phase_1, plot_phase_2, plot_phase_3, ncol = 3)