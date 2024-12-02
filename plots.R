# Create a data frame for the three scenarios
result <- data.frame(
  Scenario = c(rep("No Gamma", 4), rep("Gamma = 0.2", 6), rep("Gamma = 0.5", 6)),
  Parameter = c("b0", "b1", "a", "ph", 
                "b0", "b1", "a", "ph", "ga0", "ga1", 
                "b0", "b1", "a", "ph", "ga0", "ga1"),
  Bias = c(0.001, 0.002, -0.00003, -0.003, 
           0.007, -0.002, -0.001, -0.002, 0.001, 0.002,
           0.17, -0.118, 0.01, 0.333, 0.04, 0.019),
  MSE = c(0.014, 0.005, 0.002, 0.002, 
          0.005, 0.005, 0.0003, 0.001, 0.001, 0.002,
          0.12, 0.125, 0.001, 0.527, 0.009, 0.017),
  CP = c(0.91, 0.97, 0.95, 0.94,
         0.94, 0.91, 0.96, 0.94, 0.94, 0.94,
         0.67, 0.71, 0.84, 0.66, 0.88, 0.93)
)

# Reshape the data for plotting
library(tidyr)
result_df <- result %>% 
  pivot_longer(cols = c(Bias, MSE, CP), names_to = "Metric", values_to = "Value")


library(ggplot2)
result_df %>%
  ggplot(aes(x = Parameter, y = Value, group = Scenario, color = Scenario)) +
  geom_line(aes(linetype = Scenario)) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(title = "Metrics by Parameter and Scenario", y = "Value", x = "Parameter") +
  theme_minimal()


result_df %>%
  ggplot(aes(x = Parameter, y = Value, group = Metric, color = Metric)) +
  geom_line() +
  facet_grid(Metric ~ Scenario, scales = "free_y") +
  labs(title = "Faceted Metrics by Scenario", y = "Value", x = "Parameter") +
  theme_minimal()