## ================================================================
## ESG and Market Capitalization Forecasting (1990–2022)
## ================================================================

## ========================================
## STEP 0: LOAD REQUIRED LIBRARIES
## ========================================

# Core Data Manipulation
library(tidyverse)       # For data wrangling and visualization
library(readxl)          # To read Excel files
library(zoo)             # For time-series interpolation
library(fastDummies)     # To generate dummy variables
library(skimr)           # For quick data overview

# Econometric and Modelling Libraries
library(plm)             # Panel data econometrics
library(lmtest)          # For Breusch-Pagan test
library(car)             # VIF for multicollinearity check
library(Metrics)         # MAE, RMSE evaluation
library(caret)           # For train/test splits and ML support
library(rpart)           # Decision Tree
library(rpart.plot)      # Visualizing Decision Tree
library(randomForest)    # Random Forest modelling

# Visualization Tools
library(ggplot2)         # Plotting system
library(corrplot)        # Correlation matrix plots
library(RColorBrewer)    # Color palettes
library(knitr)           # Tables and reporting

# App Deployment
library(shiny)           # For interactive model deployment
library(bslib)    # theming
library(scales)   # number() for clean labels

## ========================================
## STEP 1: LOAD AND INSPECT DATA
## ========================================

# Set working directory to where the Excel dataset is stored
setwd("C:/Users/HP/Desktop/Dessertation/Dataset")

# Load ESG-related panel data from Excel
# This dataset includes economic, social, and environmental indicators per country-year
data <- read_excel("WDI_Clean_Panel_Format.xlsx")

# Preview dataset structure: variable names, types, sample entries
glimpse(data)

# View summary statistics for numeric and character columns
summary(data)

## ========================================
## STEP 2: DATA CLEANING & TRANSFORMATION
## ========================================
# Rename variables for clarity and interpretability
# Example renaming – adjust according to your actual variable names
data <- data %>%
  rename(
    country = `Country Name`,
    country_code =`Country Code`,
    year = `Year`,
    domestic_credit_to_private_sector = `Domestic credit to private sector (% of GDP)`,
    market_cap = `Market capitalization of listed domestic companies (% of GDP)`,
    co2 = `Carbon dioxide (CO2) emissions excluding LULUCF per capita (t CO2e/capita)`,
    renewables = `Renewable energy consumption (% of total final energy consumption)`,
    fossil_fuel = `Fossil fuel energy consumption (% of total)`,
    gdp_per_capita = `GDP per capita (constant 2015 US$)`,
    labour_participation = `Labor force participation rate, total (% of total population ages 15-64) (modeled ILO estimate)`,
    unemployment = `Unemployment, total (% of total labor force) (modeled ILO estimate)`,
    school_enrollment = `School enrollment, secondary (% gross)`,
    corruption = `Control of Corruption: Estimate`,
    government_effectiveness = `Government Effectiveness: Estimate`,
    regulatory_quality = `Regulatory Quality: Estimate`
  )

###Check for placeholder values (like .. or blanks)
# Convert ".." or other non-numeric placeholders to NA
data[data == ".."] <- NA

## ========================================
## STEP 2.1: CONVERT COLUMN TYPES
## ========================================

# Ensure the year column is numeric (integer) and country is a factor.
# This helps maintain consistent behavior during grouping and plotting.

data <- data %>%
  mutate(
    year = as.integer(year),          # Convert year to integer for time-based operations
    country = as.factor(country)     # Convert country to factor for grouping and panel analysis
  )

# Quick check to confirm successful conversion
str(data[c("country", "year")])

## ========================================
## STEP 2.2: CONVERT INDICATOR COLUMNS TO NUMERIC
## ========================================

# Convert all columns except identifiers (country, country_code, year) to numeric
# This assumes all the rest are ESG, social, and economic indicators

data <- data %>%
  mutate(across(
    .cols = -c(country, country_code, year),  # Exclude non-numeric columns
    .fns = ~ as.numeric(.),                   # Convert character to numeric
    .names = "{.col}"                         # Retain original column names
  ))

# Check structure to confirm numeric types were applied correctly
str(data)

## ========================================
## STEP 2.3: MISSING DATA DIAGNOSTICS
## ========================================

# Calculate percentage of missing values for each column
missing_summary <- colSums(is.na(data)) / nrow(data) * 100

# Display missing percentage, rounded to 2 decimals
print(round(missing_summary, 2))

## ========================================
## STEP 2.4: DROP VARIABLES WITH >30% MISSINGNESS
## ========================================

# Drop the variables that exceed 30% missing values
data <- data %>%
  select(-c(
    corruption,
    government_effectiveness,
    regulatory_quality,
    domestic_credit_to_private_sector
  ))

# Double-check updated structure
glimpse(data)


## ========================================
## STEP 2.5: INTERPOLATE REMAINING MISSING VALUES (Panel-aware)
## ========================================

# Apply interpolation for each country across years
data_interp <- data %>%
  arrange(country, year) %>%   # Ensure data is sorted
  group_by(country) %>%        # Interpolate within each country
  mutate(across(
    .cols = where(is.numeric) & !c(year),    # Interpolate only numeric columns excluding year
    .fns = ~ na.approx(., year, na.rm = FALSE),
    .names = "{.col}"
  )) %>%
  ungroup()

# Check final structure and any remaining missing values
missing_final <- colSums(is.na(data_interp))
print(missing_final)

## ========================================
## STEP 2.6: FILL REMAINING GAPS (LOCF + BOCF)
## ========================================

# At this point, some missing values might remain even after interpolation.
# This step uses two common imputation techniques:
# - LOCF (Last Observation Carried Forward): fills forward from the most recent known value.
# - BOCF (Backward Observation Carried Forward): fills backward from the next known value.
# 
# Combined, they ensure all numeric gaps are filled sensibly within each country's time series.

data_filled <- data_interp %>%
  group_by(country) %>%  # Apply imputation separately within each country group
  mutate(across(
    .cols = where(is.numeric) & !c(year),  # Apply only to numeric indicator columns, excluding year
    .fns = ~ na.locf(na.locf(., na.rm = FALSE), fromLast = TRUE),  # First fill forward, then backward
    .names = "{.col}"
  )) %>%
  ungroup()

# Final check to make sure no missing values remain
final_missing_check <- colSums(is.na(data_filled))
print(final_missing_check)

# ========================================
# STEP 2.7: SUBSET DATA TO 1990–2022 PERIOD
# ========================================

# Filter the already filled dataset to only include years from 1990 to 2022
data_esg_1990_2022 <- data_filled %>%
  filter(year >= 1990 & year <= 2022) %>%
  arrange(country, year)

# Double-check the range
range(data_esg_1990_2022$year)

# Preview the structure
glimpse(data_esg_1990_2022)

## ========================================
## STEP 3: EXPLORATORY DATA ANALYSIS (EDA)
## ========================================

# 3.1 - Descriptive statistics summary 
# Gives quick numeric insights: min, max, mean, median, etc.
summary(select(data_esg_1990_2022, where(is.numeric), -year))

# 3.2 - Correlation Matrix of Numeric Variables
# Helps detect multicollinearity and explore linear relationships
numeric_vars <- data_esg_1990_2022 %>%
  select(where(is.numeric)) %>%
  select(-year)  # Avoid year which naturally correlates with time trends

cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)  # View raw matrix if needed

# Visualize correlation matrix using readable layout and gradient color scale
corrplot(
  cor_matrix,
  method = "color",
  col = colorRampPalette(c("red", "white", "blue"))(200),
  title = "Correlation Matrix of ESG and Economic Indicators (1990–2022)",
  mar = c(0, 0, 3, 0),  # top margin increased for title
  tl.cex = 0.9,         # text label size
  tl.col = "black",     # text color
  tl.srt = 45,          # x-axis label slant angle
  addCoef.col = "black",
  number.cex = 0.7,
  cl.cex = 0.8,         # color legend size
  type = "full"
)
###Note: In RStudio, use Plots > Export to save your ggplot. Choose PNG or JPEG, set size and DPI, then save.

# ========================================
# STEP 3.3: Time Series Trends by Country
# ========================================

# Time Trend Plots for Key Indicators Across Countries
# Line plots show how each variable changes over time per country

# Define consistent color palette for all country lines
country_colors <- scales::hue_pal()(length(unique(data_esg_1990_2022$country)))

# Set common X-axis formatting for all plots
year_axis <- scale_x_continuous(breaks = seq(1990, 2022, 4), limits = c(1990, 2022), expand = c(0, 0))

# Set common theme for professional plot appearance
plot_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_blank()
  )

# (a) Market Capitalization Trend
ggplot(data_esg_1990_2022, aes(x = year, y = market_cap, color = country)) +
  geom_line(size = 0.7) +
  year_axis +
  labs(
    title = "Market Capitalization as Percentage of GDP (1990–2022)",
    y = "Market Capitalization (% of GDP)", x = "Year"
  ) +
  plot_theme +
  scale_color_manual(values = country_colors)

###Note: In RStudio, use Plots > Export to save your ggplot. Choose PNG or JPEG, set size and DPI, then save.

# (b) CO₂ Emissions Trend
ggplot(data_esg_1990_2022, aes(x = year, y = co2, color = country)) +
  geom_line(size = 0.7) +
  year_axis +
  labs(
    title = "CO2 Emissions per Capita (Metric Tons, 1990–2022)",
    y = "CO2 Emissions (tons per capita)", x = "Year"
  ) +
  plot_theme +
  scale_color_manual(values = country_colors)

# (c) Renewable Energy Consumption Trend
ggplot(data_esg_1990_2022, aes(x = year, y = renewables, color = country)) +
  geom_line(size = 0.9) +
  year_axis +
  labs(
    title = "Renewable Energy Consumption (% of Final Energy, 1990–2022)",
    y = "Renewable Energy (%)", x = "Year"
  ) +
  plot_theme +
  scale_color_manual(values = country_colors)

###Note: In RStudio, use Plots > Export to save your ggplot. Choose PNG or JPEG, set size and DPI, then save.

# (d) Unemployment Rate Trend
ggplot(data_esg_1990_2022, aes(x = year, y = unemployment, color = country)) +
  geom_line(size = 0.9) +
  year_axis +
  labs(
    title = "Unemployment Rate (% of Labor Force, 1990–2022)",
    y = "Unemployment Rate (%)", x = "Year"
  ) +
  plot_theme +
  scale_color_manual(values = country_colors)

# (e) GDP per Capita Trend
ggplot(data_esg_1990_2022, aes(x = year, y = gdp_per_capita, color = country)) +
  geom_line(size = 0.9) +
  year_axis +
  labs(
    title = "GDP per Capita (Constant 2015 USD, 1990–2022)",
    y = "GDP per Capita (USD)", x = "Year"
  ) +
  plot_theme +
  scale_color_manual(values = country_colors)


## ========================================
## STEP 4: DATA PREPARATION FOR MODELLING
## ========================================

# Set a seed to ensure reproducibility of train-test split.
# This guarantees that every time the code runs, it produces the same split.
set.seed(123)

# -----------------------------------------
# 4.1 — Create Dummy Variables for Countries
# -----------------------------------------
# This step transforms the categorical 'country' column into binary dummy variables 
# so that machine learning and regression models can interpret them numerically.
# 
# - 'remove_selected_columns = TRUE' drops the original 'country' column
# - 'remove_first_dummy = TRUE' prevents multicollinearity (dummy variable trap)
#   by using the first country alphabetically (e.g., Austria) as the reference category.


data_model <- data_esg_1990_2022 %>%
  mutate(country = as.factor(country)) %>%  # Ensure 'country' is a factor
  fastDummies::dummy_cols(
    select_columns = "country",
    remove_selected_columns = TRUE,
    remove_first_dummy = TRUE
  )

# -----------------------------------------
# 4.2 — Create an 80/20 Train-Test Split
# -----------------------------------------
# The data is randomly split into training (80%) and testing (20%) subsets.
# - Training data is used to fit the models.
# - Testing data is used to evaluate their forecasting accuracy.

total_rows <- nrow(data_model)  # Total number of records in the dataset

# Randomly sample 80% of row indices for training
train_indices <- sample(seq_len(total_rows), size = 0.8 * total_rows)

# Subset data into train and test sets
train_data <- data_model[train_indices, ]
test_data  <- data_model[-train_indices, ]

# -----------------------------------------
# 4.3 — Print Row Counts for Verification
# -----------------------------------------
cat("Train rows:", nrow(train_data), "\n")
cat("Test rows:", nrow(test_data), "\n")

# -----------------------------------------
# 4.4 — Structure Check of Training Data
# -----------------------------------------
# Use glimpse to inspect variable types and confirm dummy variables were added correctly.
glimpse(train_data)

## ========================================
## STEP 5: FIXED EFFECTS PANEL REGRESSION
## ========================================

# -----------------------------------------
# 5.0 — Prepare Panel Data Structure
# -----------------------------------------
# Convert the ESG dataset to a panel-aware pdata.frame using country and year as indexes.
panel_data <- pdata.frame(data_esg_1990_2022, index = c("country", "year"))

# -----------------------------------------
# 5.1 — Fit Fixed Effects (Within) Model
# -----------------------------------------
# This model estimates the effect of ESG indicators on stock market capitalization
# while controlling for unobserved, time-invariant country-specific effects.

fixed_effects_model <- plm(
  formula = market_cap ~ co2 + renewables + fossil_fuel + gdp_per_capita +
    labour_participation + school_enrollment + unemployment,
  data    = panel_data,
  model   = "within",         # Within estimator for fixed effects
  effect  = "individual"      # Country-level fixed effects
)

# View model summary with coefficients and statistical significance
summary(fixed_effects_model)

## ========================================
## STEP 5.2 — DIAGNOSTIC: Actual vs Predicted Plot
## ========================================

# Extract actual, predicted, and residual values
actual_values    <- as.numeric(panel_data$market_cap)
predicted_values <- as.numeric(predict(fixed_effects_model))
residual_values  <- as.numeric(residuals(fixed_effects_model))

# Combine into a dataframe for diagnostics
diagnostic_data <- data.frame(
  actual    = actual_values,
  predicted = predicted_values,
  residuals = residual_values
)

# Residual Scatter Plot: Actual vs Predicted
ggplot(diagnostic_data, aes(x = predicted, y = actual)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(
    title = "Actual vs Predicted Market Capitalization",
    x = "Predicted Market Cap (% of GDP)",
    y = "Actual Market Cap (% of GDP)"
  ) +
  theme_minimal()


## ========================================
## STEP 5.3 — DIAGNOSTIC: Model Assumption Checks
## ========================================

# -----------------------------------------
# (a) Normality of Residuals
# -----------------------------------------
# Q-Q Plot
qqnorm(diagnostic_data$residuals, main = "Q-Q Plot of Residuals")
qqline(diagnostic_data$residuals, col = "red", lwd = 2)

# Histogram
hist(diagnostic_data$residuals,
     breaks = 25,
     col = "lightblue",
     main = "Histogram of Residuals",
     xlab = "Residuals")

# -----------------------------------------
# (b) Homoskedasticity Test (Breusch-Pagan)
# -----------------------------------------
# Tests whether residual variance is constant (homoskedastic)
bptest(fixed_effects_model)

# -----------------------------------------
# (c) Multicollinearity Check via VIF
# -----------------------------------------
# Refit a pooled OLS model to enable VIF testing (plm models don’t support it)
ols_model <- lm(
  market_cap ~ co2 + renewables + fossil_fuel + gdp_per_capita +
    labour_participation + school_enrollment + unemployment,
  data = data_esg_1990_2022
)

# Check Variance Inflation Factors (VIF)
vif(ols_model)

# -----------------------------------------
# 5.4 — Fixed Effects Model Performance Metrics
# -----------------------------------------
# These metrics allow comparison with Decision Tree and Random Forest later

# Compute RMSE (Root Mean Square Error)
rmse_ols <- sqrt(mean((actual_values - predicted_values)^2))

# Compute MAE (Mean Absolute Error)
mae_ols <- mean(abs(actual_values - predicted_values))

# Compute R² (Within, FE) — single R² to report 
##Goodness of Fit
fe_summary     <- summary(fixed_effects_model)
r2_fe_within   <- as.numeric(fe_summary$r.squared["rsq"])

# Print for verification
cat(" OLS RMSE:", round(rmse_ols, 2), "\n")
cat(" OLS MAE:", round(mae_ols, 2), "\n")
cat(" OLS R² (within):", round(r2_fe_within, 3), "\n")


## ========================================
## STEP 6: DECISION TREE REGRESSION (rpart)
## ========================================

# -----------------------------------------
# 6.1 — Define Predictors and Response
# -----------------------------------------
# We will use key ESG and macro variables only, no country dummies
# Response: market_cap (% of GDP)
# Predictors: ESG and economic indicators

formula_tree <- market_cap ~ co2 + renewables + fossil_fuel +
  gdp_per_capita + labour_participation + school_enrollment + unemployment

# -----------------------------------------
# 6.2 — Train Decision Tree on Training Data
# -----------------------------------------
# This fits a regression tree to model non-linear relationships between predictors and market_cap
tree_model <- rpart(
  formula = formula_tree,
  data    = train_data,
  method  = "anova",  # For continuous output
  control = rpart.control(cp = 0.01)  # Pruning to reduce overfitting
)

# -----------------------------------------
# 6.3 — Visualize the Decision Tree
# -----------------------------------------
# Node labels include predicted values and % of samples
rpart.plot(
  tree_model,
  type = 2,
  extra = 101,
  fallen.leaves = TRUE,
  cex = 0.5,  # Increase font size
  main = "Decision Tree: ESG Influence on Market Capitalization"
)

# -----------------------------------------
# 6.4 — Predict on Test Data
# -----------------------------------------
# Generate predictions on unseen test set
tree_predictions <- predict(tree_model, newdata = test_data)

# -----------------------------------------
# 6.5 — Evaluate Performance
# -----------------------------------------
# Root Mean Square Error (lower = better)
rmse_tree <- rmse(test_data$market_cap, tree_predictions)

# Mean Absolute Error
mae_tree <- mae(test_data$market_cap, tree_predictions)

# R² (goodness of fit)
sse <- sum((test_data$market_cap - tree_predictions)^2)
sst <- sum((test_data$market_cap - mean(test_data$market_cap))^2)
sse <- sum((test_data$market_cap - tree_predictions)^2)
sst <- sum((test_data$market_cap - mean(test_data$market_cap))^2)
r2_tree <- 1 - sse/sst

# Print Metrics
cat(" Decision Tree RMSE:", round(rmse_tree, 2), "\n")
cat(" Decision Tree MAE:", round(mae_tree, 2), "\n")
cat(" Decision Tree R²:", round(r2_tree, 3), "\n")

# -----------------------------------------
# 6.5.1 — Actual vs Predicted Plot
# -----------------------------------------
dtree_plot_data <- data.frame(
  actual    = test_data$market_cap,
  predicted = tree_predictions
)

ggplot(dtree_plot_data, aes(x = predicted, y = actual)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(
    title = "Decision Tree: Actual vs Predicted Market Cap",
    x = "Predicted Market Cap (% of GDP)",
    y = "Actual Market Cap (% of GDP)"
  ) +
  theme_minimal()

# -----------------------------------------
# 6.6 — Variable Importance
# -----------------------------------------
# Shows which ESG indicators the tree used most
cat("\n Variable Importance:\n")
print(tree_model$variable.importance)

#Variable Importance bar plot
var_imp <- tree_model$variable.importance

ggplot(data.frame(Variable = names(var_imp), Importance = var_imp),
       aes(x = reorder(Variable, -Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Variable Importance (Decision Tree)", x = "", y = "") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))

## ========================================
## STEP 7: RANDOM FOREST REGRESSION
## ========================================

# -----------------------------------------
# 7.1 — Define Formula (same as before)
# -----------------------------------------
formula_rf <- market_cap ~ co2 + renewables + fossil_fuel +
  gdp_per_capita + labour_participation + school_enrollment + unemployment

# -----------------------------------------
# 7.2 — Train Random Forest on Training Data
# -----------------------------------------
# ntree = number of trees; mtry = variables tried at each split (optional tuning)
rf_model <- randomForest(
  formula = formula_rf,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

# -----------------------------------------
# 7.3 — Predict on Test Data
# -----------------------------------------
rf_predictions <- predict(rf_model, newdata = test_data)

# -----------------------------------------
# 7.4 — Evaluate of Performance
# -----------------------------------------
rmse_rf <- rmse(test_data$market_cap, rf_predictions)
mae_rf  <- mae(test_data$market_cap, rf_predictions)
sse_rf  <- sum((test_data$market_cap - rf_predictions)^2)
sst_rf  <- sum((test_data$market_cap - mean(test_data$market_cap))^2)
r2_rf   <- 1 - sse_rf/sst_rf

# Print metrics
cat("Random Forest RMSE:", round(rmse_rf, 2), "\n")
cat("Random Forest MAE:", round(mae_rf, 2), "\n")
cat("Random Forest R²:", round(r2_rf, 3), "\n")

# -----------------------------------------
# 7.4.1 — Actual vs Predicted Plot
# -----------------------------------------
rf_plot_data <- data.frame(
  actual = test_data$market_cap,
  predicted = rf_predictions
)

ggplot(rf_plot_data, aes(x = predicted, y = actual)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Random Forest: Actual vs Predicted Market Cap",
    x = "Predicted Market Cap (% of GDP)",
    y = "Actual Market Cap (% of GDP)"
  ) +
  theme_minimal()

# -----------------------------------------
# 7.5 — Variable Importance
# -----------------------------------------
cat("\n Random Forest Variable Importance:\n")
print(importance(rf_model))

# Variable importance plot
varImpPlot(rf_model,
           main = "Random Forest Variable Importance",
           col = "steelblue",
           pch = 19,
           cex.main = 1.2)


## ========================================
## STEP 8: COMPARE MODEL PERFORMANCE
## ========================================

# Create a comparison data frame
model_comparison <- data.frame(
  Model = c("Fixed Effects (OLS)", "Decision Tree", "Random Forest"),
  RMSE  = c(round(rmse_ols, 2), round(rmse_tree, 2), round(rmse_rf, 2)),
  MAE   = c(round(mae_ols, 2), round(mae_tree, 2), round(mae_rf, 2)),
  R2    = c(round(r2_fe_within , 3), round(r2_tree, 3), round(r2_rf, 3))
)

# Print the table
kable(model_comparison, caption = "Comparison of Model Performance Metrics")


# ============================================================
# STEP 10: SHINY APP DEPLOYMENT
# Shiny App — Forecast Stock Market Capitalization (% of GDP)
# Model: Random Forest (uses the variables and data pipeline)
# ============================================================

# NOTE:
# - The app expects a trained Random Forest model object `rf_model` or will fit one below.
# - The cleaned panel data `data_esg_1990_2022` must already exist in the workspace.

##### From the above Comparison Random Forest gives the best result
#### Hence RF is used for the app development environment
# Fit final model on entire dataset (not just train_data)

# ---- Plot styling ----
# Global ggplot theme for consistent, readable charts.
theme_set(theme_minimal(base_size = 13))

# ------------------------------------------------------------
# Data and Model hooks
# ------------------------------------------------------------
# Precondition: cleaned data is present.
# If `rf_model` is missing, fit it once using the full dataset and the chosen formula.
# Using a fixed seed ensures reproducible trees and predictions.
stopifnot(exists("data_esg_1990_2022"))

# All predictors used by the model (match your earlier modeling work).
formula_rf <- market_cap ~ co2 + renewables + fossil_fuel +
  gdp_per_capita + labour_participation + school_enrollment + unemployment

if (!exists("rf_model")) {
  set.seed(123) # reproducibility for RF fitting
  rf_model <- randomForest(
    formula = formula_rf,
    data = data_esg_1990_2022,
    ntree = 500,
    importance = TRUE
  )
}

# Build sensible UI bounds (min/max) from the data so inputs stay realistic.
rng <- within(list(), {
  co2                  <- range(data_esg_1990_2022$co2, na.rm = TRUE)
  renewables           <- range(data_esg_1990_2022$renewables, na.rm = TRUE)
  fossil_fuel          <- range(data_esg_1990_2022$fossil_fuel, na.rm = TRUE)
  gdp_per_capita       <- range(data_esg_1990_2022$gdp_per_capita, na.rm = TRUE)
  labour_participation <- range(data_esg_1990_2022$labour_participation, na.rm = TRUE)
  school_enrollment    <- range(data_esg_1990_2022$school_enrollment, na.rm = TRUE)
  unemployment         <- range(data_esg_1990_2022$unemployment, na.rm = TRUE)
})

# Default input values set to the median of each field (good neutral starting point).
med <- function(v) median(v, na.rm = TRUE)
defs <- list(
  co2                  = round(med(data_esg_1990_2022$co2), 2),
  renewables           = round(med(data_esg_1990_2022$renewables), 1),
  fossil_fuel          = round(med(data_esg_1990_2022$fossil_fuel), 1),
  gdp_per_capita       = round(med(data_esg_1990_2022$gdp_per_capita), 0),
  labour_participation = round(med(data_esg_1990_2022$labour_participation), 1),
  school_enrollment    = round(med(data_esg_1990_2022$school_enrollment), 1),
  unemployment         = round(med(data_esg_1990_2022$unemployment), 1)
)

# **NEW**: country choices vector
# Used to let a user anchor inputs to a particular country's typical (median) values.
countries <- sort(unique(as.character(data_esg_1990_2022$country)))

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------
ui <- fluidPage(
  # Visual theme (requires {bslib} loaded). Only affects widgets/layout, not ggplot colors.
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#1E88E5"),
  
  # Small custom CSS for card-like panels and KPI styling.
  tags$head(
    tags$style(HTML("
      body { background: #f5f7fb; }
      .app-card {
        background:#fff; border-radius:14px; padding:16px 18px;
        box-shadow: 0 6px 18px rgba(16,24,40,0.08);
      }
      .kpi-box {
        background:#eef5ff; border:1px solid #d6e4ff; color:#0f3e8a;
        border-radius:12px; padding:14px 16px; margin-bottom:12px;
        font-weight:600; display:flex; align-items:center; gap:8px;
      }
      .kpi-value { font-size:20px; font-weight:700; }
      .section-title { font-size:18px; font-weight:700; margin:8px 0 12px; }
      .btn-row { display:flex; gap:10px; align-items:center; }
    "))
  ),
  
  titlePanel("Forecast Stock Market Capitalization (% of GDP)"),
  
  sidebarLayout(
    # ---------------------- Left: Inputs ----------------------
    sidebarPanel(
      class = "app-card",
      div(class = "section-title", "Inputs"),
      
      # Country selector drives auto-fill of inputs with that country's medians.
      selectInput("country", "Country", choices = countries, selected = countries[1]),
      
      # **NEW (READ-ONLY CONTEXT): Forecast year selector shown for labeling only.**
      # This does not feed the model; it's purely to label results.
      selectInput("forecast_year", "Forecast Year (display only)", choices = 2026:2035, selected = 2026),
      
      # Numeric inputs: initialized with dataset medians and bounded by observed ranges.
      numericInput("co2", "CO₂ Emissions (metric tons per capita)",
                   value = defs$co2, min = floor(rng$co2[1]), max = ceiling(rng$co2[2]), step = 0.1),
      
      numericInput("renewables", "Renewable Energy Consumption (% of total energy)",
                   value = defs$renewables, min = max(0, floor(rng$renewables[1])),
                   max = min(100, ceiling(rng$renewables[2])), step = 0.5),
      
      numericInput("fossil_fuel", "Fossil Fuel Use (% of total energy)",
                   value = defs$fossil_fuel, min = max(0, floor(rng$fossil_fuel[1])),
                   max = min(100, ceiling(rng$fossil_fuel[2])), step = 0.5),
      
      numericInput("gdp_per_capita", "GDP per Capita (constant 2015 US$)",
                   value = defs$gdp_per_capita, min = floor(rng$gdp_per_capita[1]),
                   max = ceiling(rng$gdp_per_capita[2]), step = 100),
      
      numericInput("labour_participation", "Labour Force Participation Rate (%)",
                   value = defs$labour_participation, min = 0, max = 100, step = 0.1),
      
      numericInput("school_enrollment", "Secondary School Enrollment (% gross)",
                   value = defs$school_enrollment, min = 0, max = 200, step = 0.1),
      
      numericInput("unemployment", "Unemployment Rate (% of labour force)",
                   value = defs$unemployment, min = 0, max = 100, step = 0.1),
      
      tags$hr(),
      
      # Scenario control: adjusts renewables by ±% to see impact on forecast.
      sliderInput("delta_renewables", "Scenario: change renewables (%)",
                  min = -20, max = 20, value = 10, step = 1),
      
      # Actions: run forecast + download chart image.
      div(class = "btn-row",
          actionButton("simulate", "Run Forecast", class = "btn-primary"),
          downloadButton("download_plot", "Download PNG")
      ),
      
      # Simple how-to note for saving plots from the Viewer.
      helpText("Tip: Plots › Export can also save the chart (PNG/JPEG, size, DPI).")
    ),
    
    # ---------------------- Right: Outputs ----------------------
    mainPanel(
      class = "app-card",
      
      # KPI-style banner showing the current forecast value prominently.
      div(class = "kpi-box",
          span("Predicted Market Capitalization (% of GDP):"),
          span(class = "kpi-value", textOutput("kpi_value", inline = TRUE)),
          span(" | Year:"),
          span(class = "kpi-value", textOutput("kpi_year", inline = TRUE))   # **NEW (display only)**
      ),
      
      # Scenario comparison bar chart with labels on bars.
      plotOutput("scenario_plot", height = "420px")
    )
  )
)

# ------------------------------------------------------------
# Server
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  # Helper to keep percentage-like inputs within valid bounds.
  clamp <- function(x, lo, hi) pmin(hi, pmax(lo, x))
  
  # When the user changes country, auto-fill inputs with that country's medians.
  observeEvent(input$country, {
    country_df <- data_esg_1990_2022[data_esg_1990_2022$country == input$country, , drop = FALSE]
    upd_med <- function(x) median(x, na.rm = TRUE)
    
    updateNumericInput(session, "co2",                  value = round(upd_med(country_df$co2), 2))
    updateNumericInput(session, "renewables",           value = round(upd_med(country_df$renewables), 1))
    updateNumericInput(session, "fossil_fuel",          value = round(upd_med(country_df$fossil_fuel), 1))
    updateNumericInput(session, "gdp_per_capita",       value = round(upd_med(country_df$gdp_per_capita), 0))
    updateNumericInput(session, "labour_participation", value = round(upd_med(country_df$labour_participation), 1))
    updateNumericInput(session, "school_enrollment",    value = round(upd_med(country_df$school_enrollment), 1))
    updateNumericInput(session, "unemployment",         value = round(upd_med(country_df$unemployment), 1))
  }, ignoreInit = FALSE)
  
  # Build the one-row data.frame used for prediction from current inputs.
  # Includes minimal validation and clamping to prevent impossible values.
  make_input_row <- reactive({
    df <- data.frame(
      co2                  = input$co2,
      renewables           = input$renewables,
      fossil_fuel          = input$fossil_fuel,
      gdp_per_capita       = input$gdp_per_capita,
      labour_participation = input$labour_participation,
      school_enrollment    = input$school_enrollment,
      unemployment         = input$unemployment
    )
    
    # Guard against missing/invalid entries before prediction.
    if (any(is.na(df))) {
      showNotification("Please fill all inputs with valid numbers.", type = "error")
      return(NULL)
    }
    
    # Keep rate-based fields within sensible bounds.
    df$renewables           <- clamp(df$renewables, 0, 100)
    df$fossil_fuel          <- clamp(df$fossil_fuel, 0, 100)
    df$labour_participation <- clamp(df$labour_participation, 0, 100)
    df$unemployment         <- clamp(df$unemployment, 0, 100)
    df
  })
  
  # Compute predictions only when the user clicks "Run Forecast".
  # Also runs once on load (ignoreInit = FALSE).
  preds <- eventReactive(input$simulate, {
    base_df <- make_input_row()
    if (is.null(base_df)) return(NULL)
    
    # Current prediction using the Random Forest model.
    p_current <- as.numeric(predict(rf_model, newdata = base_df))
    
    # Scenario: increase/decrease renewables, clamp to [0, 100], and re-predict.
    scen_df <- base_df
    scen_df$renewables <- clamp(scen_df$renewables + input$delta_renewables, 0, 100)
    p_scenario <- as.numeric(predict(rf_model, newdata = scen_df))
    
    # Human-friendly label for the scenario bar.
    delta_lab <- if (input$delta_renewables >= 0)
      paste0("Renewables +", input$delta_renewables, "%") else
        paste0("Renewables ", input$delta_renewables, "%")
    
    list(current = p_current, scenario = p_scenario, delta_lab = delta_lab)
  }, ignoreInit = FALSE)
  
  # KPI text: show the current prediction with two decimal places.
  output$kpi_value <- renderText({
    req(preds())
    scales::number(preds()$current, accuracy = 0.01)
  })
  
  # **NEW (display only): Echo the selected forecast year in the KPI strip.**
  output$kpi_year <- renderText({
    req(input$forecast_year)
    input$forecast_year
  })
  
  # Labeled bar chart comparing current vs. scenario forecast.
  scenario_plot_obj <- reactive({
    req(preds())
    
    # Two-row frame the chart will plot.
    df <- data.frame(
      Scenario = factor(c("Current Inputs", preds()$delta_lab),
                        levels = c("Current Inputs", preds()$delta_lab)),
      value    = c(preds()$current, preds()$scenario)
    )
    
    # Add headroom so the value labels sit above bars.
    ymax <- max(df$value) * 1.12
    lab  <- preds()$delta_lab
    
    # Fixed palette: steelblue for current, green for scenario.
    cols <- setNames(c("#2C7FB8", "#2E7D32"), c("Current Inputs", lab))
    
    ggplot(df, aes(Scenario, value, fill = Scenario)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = scales::number(value, accuracy = 0.01)),
                vjust = -0.35, fontface = "bold", size = 5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.08)),
                         limits = c(0, ymax)) +
      scale_fill_manual(values = cols) +
      labs(
        # **NEW**: include forecast year in the title for clarity. Read-only label.
        title = paste("Scenario Simulation: Market Cap Forecast —",
                      input$country, "| Year:", input$forecast_year),
        x = NULL,
        y = "Predicted Market Cap (% of GDP)"
      ) +
      theme(
        plot.title   = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none",
        axis.text.x  = element_text(face = "bold")
      )
  })
  
  # Render chart to the UI.
  output$scenario_plot <- renderPlot(scenario_plot_obj(), res = 110)
  
  # Export the current chart as a high-resolution PNG 
  output$download_plot <- downloadHandler(
    filename = function() paste0("market_cap_forecast_", Sys.Date(), ".png"),
    content = function(file) {
      p <- scenario_plot_obj()  # build plot once
      ggsave(
        filename = file,
        plot     = p,
        device   = "png",   # explicit PNG device
        width    = 9,
        height   = 5.2,
        dpi      = 300,
        bg       = "white"  # <-- key: ensures non-transparent white background
      )
    }
  )
}

# ------------------------------------------------------------
# Launch the app
# ------------------------------------------------------------
shinyApp(ui = ui, server = server)








