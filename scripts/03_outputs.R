# ==============================================================================
# Script: 03-outputs.R
# Purpose: Generate predicted values and plot interaction effects for the report
# Author: Dilnaz Zhumagaliyeva
# ==============================================================================

library(tidyverse)

load("data/regression_models.RData")
analysis_data <- readRDS("data/cleaned_data.rds")

# Holding numeric controls at their mean and categorical controls at their mode
grid_data <- expand.grid(
  Education_Level = c("High School", "Bachelor's", "Master's", "Doctorate"),
  Gender = c("Male", "Female"),
  Potential_Experience = mean(analysis_data$Potential_Experience, na.rm = TRUE),
  Potential_Experience_Sq = mean(analysis_data$Potential_Experience_Sq, na.rm = TRUE),
  Location = "Urban",
  Employment_Status = "Full-time",
  Marital_Status = "Married",
  Household_Size = mean(analysis_data$Household_Size, na.rm = TRUE)
)

# Calculating predicted log income and standard errors
predictions <- predict(model_interaction, newdata = grid_data, se.fit = TRUE)

grid_data <- grid_data %>%
  mutate(
    fit_log = predictions$fit,
    se = predictions$se.fit,
    # Строим 95% доверительные интервалы (Confidence Intervals)
    ci_lower = fit_log - 1.96 * se,
    ci_upper = fit_log + 1.96 * se
  )

# Converting Education_Level to a factor with the correct order for plotting
grid_data$Education_Level <- factor(grid_data$Education_Level, 
                                    levels = c("High School", "Bachelor's", "Master's", "Doctorate"))

# Plotting marginal effects using ggplot2
interaction_plot <- ggplot(grid_data, aes(x = Education_Level, y = fit_log, group = Gender, color = Gender)) +
  geom_line(position = position_dodge(0.2), linewidth = 1) +
  geom_point(position = position_dodge(0.2), size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(0.2)) +
  labs(
    title = "Predicted Log Household Income by Education and Gender",
    subtitle = "All other covariates held at their mean/mode values",
    x = "Highest Educational Attainment",
    y = "Predicted Log(Income)",
    color = "Gender"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "bottom"
  )

if(!dir.exists("figures")) dir.create("figures")
ggsave("figures/interaction_effects.png", plot = interaction_plot, width = 7, height = 5, dpi = 300)