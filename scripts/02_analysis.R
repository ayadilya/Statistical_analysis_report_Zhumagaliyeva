# ==============================================================================
# Script: 02-analysis.R
# Purpose: Estimate OLS and Interaction models, check VIF diagnostics
# Author: Dilnaz Zhumagaliyeva
# ==============================================================================

library(tidyverse)
library(car) 

analysis_data <- readRDS("data/cleaned_data.rds")

model_base <- lm(
  log_income ~ Education_Level + Potential_Experience + Potential_Experience_Sq + 
    Gender + Location + Employment_Status + Marital_Status + Household_Size, 
  data = analysis_data
)

# Testing whether the returns to education vary by gender
model_interaction <- lm(
  log_income ~ Education_Level * Gender + Potential_Experience + Potential_Experience_Sq + 
    Location + Employment_Status + Marital_Status + Household_Size, 
  data = analysis_data
)

# Checking if using Potential_Experience resolved the high multicollinearity issue
print("--- VIF Diagnostics for Base Model ---")
vif_results <- vif(model_base)
print(vif_results)

save(model_base, model_interaction, file = "data/regression_models.RData")