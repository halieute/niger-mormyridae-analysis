# ======================================================
# FULL COMMENTED CODE: Correlation Matrix & Corrplot
# ======================================================

# Load the corrplot package for visualizing correlation matrices
library(corrplot)

# ------------------------------------------------------
# 1. READ THE DATA
# ------------------------------------------------------
# Read a CSV file that contains diversity indices and month information.
# The file is assumed to have a header row with column names.
data <- read.csv("diversity_ind_with_months.csv", header = TRUE)

# ------------------------------------------------------
# 2. SELECT ONLY NUMERIC COLUMNS
# ------------------------------------------------------
# 'sapply(data, is.numeric)' checks each column; returns TRUE if numeric.
# We keep only those columns to compute correlations (ignoring factors, dates, etc.)
data_num <- data[, sapply(data, is.numeric)]

# ------------------------------------------------------
# 3. COMPUTE CORRELATION MATRIX
# ------------------------------------------------------
# cor() calculates pairwise correlations.
# - use = "pairwise.complete.obs": uses all available pairs, omitting missing values per pair.
# - method = "pearson": standard linear correlation (can also be "spearman" or "kendall").
cor_mat <- cor(data_num, use = "pairwise.complete.obs", method = "pearson")

# ------------------------------------------------------
# 4. FIRST CORRPLOT (DISPLAY ONLY – not saved)
# ------------------------------------------------------
# This corrplot is shown on the screen (e.g., in RStudio Plots pane).
# Parameters explained:
#   method = "circle"    : shape of the correlation markers (also "square", "ellipse", "number", etc.)
#   type = "lower"       : show only the lower triangular part (upper is redundant)
#   tl.col = "red"       : color of the variable labels (row/col names)
#   tl.srt = 45          : rotate variable labels by 45 degrees to avoid overlap
#   addCoef.col = "black": add correlation coefficients inside the plot in black
#   number.cex = 0.9     : size of the coefficient numbers (relative)
#   col = colorRampPalette(...)(200): custom color palette from darkred (strong negative)
#                                     through white (zero) to darkblue (strong positive)
corrplot(
  cor_mat,
  method = "circle",
  type = "lower",
  tl.col = "red",
  tl.srt = 45,
  addCoef.col = "black",
  number.cex = 0.9,
  col = colorRampPalette(c("darkred", "white", "darkblue"))(200)
)

# ------------------------------------------------------
# 5. SAVE A HIGH-RESOLUTION CORRPLOT TO A PNG FILE
# ------------------------------------------------------
# Open a PNG graphics device with custom dimensions and resolution.
#   width = 2000 pixels, height = 2000 pixels, resolution = 300 DPI
# The resulting file will be 2000/300 ≈ 6.67 inches wide/high, suitable for publications.
png("correlation_plot.png", width = 2000, height = 2000, res = 300)

# Generate the same plot again, but this time it will be written to the PNG file.
# Additional/different parameters:
#   diag = TRUE          : show the diagonal (correlation of a variable with itself = 1)
#   tl.cex = 0.9         : size of variable labels (same as number.cex for consistency)
#   cl.lim = c(-1, 1)    : set the color legend limits to full correlation range (-1 to 1)
#   cl.cex = 0.9         : size of the color legend text
#   col = colorRampPalette(c("#7F0000", "#FFFFFF", "#08306B"))(200)
#                        : alternative palette (dark red → white → dark blue) using hex codes
corrplot(
  cor_mat,
  method = "circle",
  type = "lower",
  diag = TRUE,
  addCoef.col = "black",
  number.cex = 0.9,
  tl.col = "red",
  tl.cex = 0.9,
  tl.srt = 45,
  col = colorRampPalette(c("#7F0000", "#FFFFFF", "#08306B"))(200),
  cl.lim = c(-1, 1),
  cl.cex = 0.9
)

# Close the PNG device – finalises and saves the file.
# Without this, the plot may not be written properly.
dev.off()

# Optional message to confirm saving
cat("Correlation plot saved as 'correlation_plot.png'\n")
