# ======================================================================
# CORRELATION ANALYSIS BETWEEN ENVIRONMENTAL VARIABLES AND DIVERSITY INDICES
# ======================================================================

# --------------------------------------------
# 1. LOAD REQUIRED LIBRARIES
# --------------------------------------------
# tidyverse: collection of packages (dplyr, readr, tidyr, etc.) for data manipulation.
# Hmisc: provides rcorr() which computes correlation matrices AND p-values.
library(tidyverse)
library(Hmisc)

# --------------------------------------------
# 2. READ THE DATA
# --------------------------------------------
# Assumes the CSV file is in your working directory.
# Replace path if needed (e.g., "data/diversity_ind_with_months.csv").
data <- read_csv("diversity_ind_with_months.csv")

# Optional: print column names to verify they match the variable lists below.
names(data)

# --------------------------------------------
# 3. DEFINE VARIABLE GROUPS
# --------------------------------------------
# Based on your CSV, these are the environmental variables (predictors).
# AT = Ambient Temperature, WT = Water Temperature, OS = Oxygen Saturation,
# DO = Dissolved Oxygen, pH = acidity, TP = Transparency, D = Depth,
# TDS = Total Dissolved Solids, C = Conductivity, Equitability = Pielou's evenness.
env_vars <- c("AT", "WT", "OS", "DO", "pH", "TP", "D", "TDS", "C", "Equitability")

# Diversity indices (response variables).
div_vars <- c("Simpson", "Shannon", "Evenness", "Margalef", "Fisher")

# --------------------------------------------
# 4. PREPARE COMPLETE DATA (NO MISSING VALUES)
# --------------------------------------------
# Select only the columns we need (env + diversity).
# Then remove any row that has at least one NA (listwise deletion).
# This ensures that all correlations use the same set of observations.
data_complete <- data %>%
  select(all_of(c(env_vars, div_vars))) %>%
  na.omit()

# --------------------------------------------
# 5. COMPUTE CORRELATION MATRIX AND P‑VALUES
# --------------------------------------------
# rcorr() returns a list with:
#   - r : matrix of Pearson correlation coefficients
#   - n : number of observations per pair
#   - P : matrix of p‑values (for testing H0: correlation = 0)
# 'type = "pearson"' gives linear correlation; also "spearman" available.
cor_result <- rcorr(as.matrix(data_complete), type = "pearson")

# Extract the correlation coefficients (r) and p‑values (P) for the subset
# env_vars (rows) vs div_vars (columns).
cor_values <- cor_result$r[env_vars, div_vars]   # matrix: 10 x 5
p_values   <- cor_result$P[env_vars, div_vars]   # same dimensions

# --------------------------------------------
# 6. OUTPUT OPTION 1: LONG FORMAT TABLE
# --------------------------------------------
# Long format is easy to filter, plot, or export to databases.
# expand.grid() creates all combinations of env and diversity indices.
long_table <- expand.grid(Parameter = env_vars, Diversity = div_vars) %>%
  mutate(
    Correlation = as.vector(cor_values),   # flatten matrix column‑wise
    p_value     = as.vector(p_values)
  ) %>%
  # Round to a reasonable number of decimals for readability.
  mutate(
    Correlation = round(Correlation, 3),
    p_value     = round(p_value, 4)
  )

# Print the long table to the console.
print(long_table)

# --------------------------------------------
# 7. OUTPUT OPTION 2: WIDE FORMAT (SEPARATE TABLES)
# --------------------------------------------
# Wide format is often preferred for quick visual scanning.
# Convert the correlation matrix to a data frame, then add row names as a column.
cor_wide <- as.data.frame(cor_values) %>%
  rownames_to_column("Parameter") %>%
  # Rename columns to match the actual diversity index names.
  rename_with(~ c("Parameter", "Simpson_1-D", "Shannon_H", 
                  "Evenness_e^H/S", "Margalef", "Fisher_alpha"))

# Same for p‑values.
p_wide <- as.data.frame(p_values) %>%
  rownames_to_column("Parameter") %>%
  rename_with(~ c("Parameter", "Simpson_1-D", "Shannon_H", 
                  "Evenness_e^H/S", "Margalef", "Fisher_alpha"))

# Print wide tables.
print("Correlation coefficients (r):")
print(cor_wide)

print("P-values (significance):")
print(p_wide)

# --------------------------------------------
# 8. OUTPUT OPTION 3: COMBINED WIDE TABLE (r + p in one cell)
# --------------------------------------------
# This format is compact for publication: "0.45 (0.023)".
# Start from the correlation table and replace each numeric column.
combined_wide <- cor_wide
for (i in 2:ncol(cor_wide)) {
  combined_wide[[i]] <- paste0(
    round(cor_wide[[i]], 2),        # correlation rounded to 2 decimals
    " (", 
    round(p_wide[[i]], 3),          # p‑value rounded to 3 decimals
    ")"
  )
}
print("Combined (correlation and p-value in parentheses):")
print(combined_wide)

# --------------------------------------------
# 9. EXPORT TABLES TO CSV FILES
# --------------------------------------------
# Save all three formats for later use or reporting.
write_csv(long_table, "correlation_pvalues_long.csv")
write_csv(cor_wide,    "correlations_wide.csv")
write_csv(p_wide,      "pvalues_wide.csv")
write_csv(combined_wide, "correlation_with_pvalues.csv")

# --------------------------------------------
# 10. FULL CORRELATION MATRIX (ALL VARIABLES)
# --------------------------------------------
# If you also need correlations among all variables (including env‑env and div‑div).
all_vars <- c(env_vars, div_vars)
cor_all <- cor(data_complete[, all_vars], use = "pairwise.complete.obs")

# Print the full matrix (optional). It is not saved by default – uncomment if needed.
# print(cor_all)

# You could also save it:
# write_csv(as.data.frame(cor_all) %>% rownames_to_column("Variable"), "correlation_all_vars.csv")

# --------------------------------------------
# END OF SCRIPT
# --------------------------------------------
