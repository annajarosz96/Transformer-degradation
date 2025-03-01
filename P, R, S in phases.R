


# Load required libraries
library(readxl)
library(ggplot2)
library(gridExtra)

# Replace "Dataset.xlsx" with the actual file path
data <- read_excel("Dataset.xlsx")

# Combine the extracted data into a single data frame
df <- data.frame(
  active_power_phase_1 = data[[3]],
  reactive_power_phase_1 = data[[4]],
  apparent_power_phase_1 = data[[5]],
  active_power_phase_2 = data[[6]],
  reactive_power_phase_2 = data[[7]],
  apparent_power_phase_2 = data[[8]],
  active_power_phase_3 = data[[9]],
  reactive_power_phase_3 = data[[10]],
  apparent_power_phase_3 = data[[11]]
)

# Plot for Active Power (Row 1)
plot_active_power <- ggplot(df, aes(x = 1:nrow(df))) +
  geom_line(aes(y = active_power_phase_1, color = "Phase 1")) +
  geom_line(aes(y = active_power_phase_2, color = "Phase 2")) +
  geom_line(aes(y = active_power_phase_3, color = "Phase 3")) +
  labs(
    title = "Active Power",
    x = "Measure",
    y = "Power (W)"
  ) +
  scale_color_manual(values = c("Phase 1" = "black", "Phase 2" = "blue", "Phase 3" = "darkblue")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    legend.title = element_blank()
  )

# Plot for Reactive Power (Row 2)
plot_reactive_power <- ggplot(df, aes(x = 1:nrow(df))) +
  geom_line(aes(y = reactive_power_phase_1, color = "Phase 1")) +
  geom_line(aes(y = reactive_power_phase_2, color = "Phase 2")) +
  geom_line(aes(y = reactive_power_phase_3, color = "Phase 3")) +
  labs(
    title = "Reactive Power",
    x = "Measure",
    y = "Power (VAR)"
  ) +
  scale_color_manual(values = c("Phase 1" = "black", "Phase 2" = "darkblue", "Phase 3" = "blue")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    legend.title = element_blank()
  )

# Plot for Apparent Power (Row 3)
plot_apparent_power <- ggplot(df, aes(x = 1:nrow(df))) +
  geom_line(aes(y = apparent_power_phase_1, color = "Phase 1")) +
  geom_line(aes(y = apparent_power_phase_2, color = "Phase 2")) +
  geom_line(aes(y = apparent_power_phase_3, color = "Phase 3")) +
  labs(
    title = "Apparent Power",
    x = "Measure",
    y = "Power (VA)"
  ) +
  scale_color_manual(values = c("Phase 1" = "black", "Phase 2" = "darkblue", "Phase 3" = "blue")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    legend.title = element_blank()
  )

# Arrange the plots into three rows
grid.arrange(plot_active_power, plot_reactive_power, plot_apparent_power, ncol = 1)