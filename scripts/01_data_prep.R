# ==============================================================================
# Script: 01-data-prep.R
# Purpose: Load dataset, clean variables, handle multicollinearity and scale DV
# Author: Dilnaz Zhumagaliyeva
# ==============================================================================

library(tidyverse)

raw_data <- read_csv("data/data.csv")

cleaned_data <- raw_data %>%
  mutate(
    Education_Level = factor(Education_Level, 
                             levels = c("High School", "Bachelor's", "Master's", "Doctorate")),
    Gender = factor(Gender, levels = c("Male", "Female")),
    Location = factor(Location),
    Employment_Status = factor(Employment_Status),
    Marital_Status = factor(Marital_Status)
  ) %>%
  # Assigning years of education to calculate potential experience (per feedback)
  mutate(
    Years_of_Schooling = case_when(
      Education_Level == "High School" ~ 12,
      Education_Level == "Bachelor's"  ~ 16,
      Education_Level == "Master's"     ~ 18,
      Education_Level == "Doctorate"    ~ 21,
      TRUE ~ 12
    )
  ) %>%
  # Creating Potential Experience to replace perfectly collinear Work_Experience
  mutate(
    Potential_Experience = Age - Years_of_Schooling - 6,
    # Handling potential negative values due to synthetic data constraints:
    Potential_Experience = ifelse(Potential_Experience < 0, 0, Potential_Experience),
    Potential_Experience_Sq = Potential_Experience^2
  ) %>%
  mutate(
    log_income = log(Income)
  ) %>%
  # Removing anomalous infinite values if Income was 0 (just in case)
  filter(!is.infinite(log_income) & !is.na(log_income))

saveRDS(cleaned_data, "data/cleaned_data.rds")