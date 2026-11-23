# ======================================================================
# HIERARCHICAL CLUSTERING, HEATMAPS, AND VALIDATION FOR ECOLOGICAL DATA
# ======================================================================

# --------------------------------------------
# 1. LOAD REQUIRED LIBRARIES
# --------------------------------------------
library(vegan)      # For distance matrices (vegdist) and PERMANOVA (adonis2)
library(cluster)    # For silhouette coefficients (silhouette)
library(pheatmap)   # For annotated heatmaps
library(factoextra) # For visualization of silhouette plots (fviz_silhouette)

# --------------------------------------------
# 2. READ THE DATA
# --------------------------------------------
# Assumes a CSV file with rows as samples/sites and columns as variables.
# The first column may contain sample IDs, but we use all columns for analysis.
your_data <- read.csv("Hierar_clust_data.csv")

# --------------------------------------------
# 3. DEFINE COLUMN GROUPS
# --------------------------------------------
# List all species abundance columns (adjust names to match your CSV)
species_columns <- c(
  "Brienomyrus_niger", "Campylomormyrus_tamandua", "Hippopotamyrus_pictus",
  "Hyperopisus_bebe", "Cyphomyrus_pssittacus", "Marcusenius_cyprinoides",
  "Marcusenius_senegalensis", "Mormyrops_anguiloides", "Mormyrops_oudoti",
  "Mormyrus_macrophthalmus", "Mormyrus_rume", "Petrocephalus_bovei",
  "Brevimyrus_niger", "Pollimyrus_isidori"
)

# List all environmental variable columns
env_columns <- c(
  "Ambient_Temp", "Water_Temp", "Oxygen_Saturation_Rate", "Dissolved_Oxygen_mgL",
  "pH", "Transparency_cm", "Depth_m", "TDS_ppm", "Conductivity_mS"
)

# --------------------------------------------
# 4. STANDARDIZE THE DATA (Z-SCORES)
# --------------------------------------------
# decostand() with method "standardize" centers and scales each column to mean=0, sd=1.
# This prevents variables with large units from dominating distance calculations.
data_std <- decostand(your_data[, c(species_columns, env_columns)], method = "standardize")

# --------------------------------------------
# 5. CALCULATE DISTANCE MATRICES
# --------------------------------------------
# Bray-Curtis dissimilarity is recommended for species abundance data (sensitive to composition).
species_dist <- vegdist(data_std[, species_columns], method = "bray")

# Euclidean distance is common for environmental variables after standardization.
env_dist <- vegdist(data_std[, env_columns], method = "euclidean")

# --------------------------------------------
# 6. HIERARCHICAL CLUSTERING (WARD'S METHOD)
# --------------------------------------------
# Ward.D2 minimizes within-cluster variance – often produces compact clusters.
species_clust <- hclust(species_dist, method = "ward.D2")
env_clust <- hclust(env_dist, method = "ward.D2")

# --------------------------------------------
# 7. CUT DENDROGRAMS INTO CLUSTERS
# --------------------------------------------
# k = 3 means we ask for 3 clusters. Choose k based on dendrogram inspection or silhouette.
species_groups <- cutree(species_clust, k = 3)
env_groups <- cutree(env_clust, k = 3)

# --------------------------------------------
# 8. VALIDATE CLUSTERS USING SILHOUETTE
# --------------------------------------------
# Silhouette width: >0.5 = good cluster, <0.2 = weak/overlapping.
sil_species <- silhouette(species_groups, species_dist)
fviz_silhouette(sil_species)  # Visualizes each site's silhouette score

# --------------------------------------------
# 9. COMPARE SPECIES AND ENVIRONMENTAL CLUSTERS
# --------------------------------------------
# Cross-tabulation: how many sites belong to each combination of clusters?
table(species_groups, env_groups)

# --------------------------------------------
# 10. INITIAL HEATMAP WITH ANNOTATIONS (FULL DATA)
# --------------------------------------------
# This first heatmap attempts to use all variables – may fail if dimensions mismatch.
pheatmap(data_std,
         cluster_rows = species_clust,
         cluster_cols = hclust(dist(t(data_std))),
         annotation_row = data.frame(Cluster = factor(species_groups)),
         annotation_col = data.frame(Env_Cluster = factor(env_groups)))

# --------------------------------------------
# 11. DEBUGGING DIMENSION MISMATCH (COMMON ISSUE)
# --------------------------------------------
# The code below shows typical debugging steps.
# The problem: env_groups length = number of samples (rows), but annotation_col expects one value per column (variable).
# Correct approach: annotation_col should refer to environmental VARIABLES, not samples.

# Print length of env_groups – should equal number of rows in data_std (samples)
print(length(env_groups))

# This line incorrectly tries to reuse env_groups for columns – will produce error if dimensions differ.
# (It's kept here to illustrate the mistake.)
# env_dist <- vegdist(data_std[, env_columns], method = "euclidean")
# env_clust <- hclust(env_dist, method = "ward.D2")
# env_groups <- cutree(env_clust, k = 3)
# print(length(env_groups))  # Should be number of SAMPLES, not number of variables

# --------------------------------------------
# 12. FIXED ANNOTATION FOR COLUMNS (ENVIRONMENTAL VARIABLES)
# --------------------------------------------
# Now we create a cluster assignment for the ENVIRONMENTAL VARIABLES themselves.
# This is different: we transpose the environmental data so that variables become rows.
env_dist_vars <- vegdist(t(data_std[, env_columns]), method = "euclidean")
env_clust_vars <- hclust(env_dist_vars, method = "ward.D2")
env_groups_vars <- cutree(env_clust_vars, k = 3)  # Now length = number of env variables (9)

# Build annotation_col with one row per environmental variable.
annotation_col <- data.frame(Env_Cluster = factor(env_groups_vars))
rownames(annotation_col) <- colnames(data_std[, env_columns])

# Check dimensions: should be 9 x 1
print(dim(annotation_col))
print(rownames(annotation_col))

# Check full data dimensions: rows = samples, columns = species + env
print(dim(data_std))

# Now the heatmap should work.
pheatmap(data_std,
         cluster_rows = species_clust,
         cluster_cols = env_clust_vars,   # clustering of variables
         annotation_row = data.frame(Cluster = factor(species_groups)),
         annotation_col = annotation_col)

# --------------------------------------------
# 13. TEST WITH A SMALLER SUBSET (IF THE FULL HEATMAP FAILS)
# --------------------------------------------
# Use only the first 5 rows (samples) and first 5 columns (variables) for testing.
data_test <- data_std[1:5, 1:5]
species_test <- species_groups[1:5]
env_test <- env_groups[1:5]   # Note: still using sample groups, not variable groups

# This test will also have dimension issues because annotation_col expects variable clusters.
pheatmap(data_test,
         cluster_rows = hclust(dist(data_test)),
         cluster_cols = hclust(dist(t(data_test))),
         annotation_row = data.frame(Cluster = factor(species_test)),
         annotation_col = data.frame(Env_Cluster = factor(env_test)))  # ERROR: env_test length 5, but columns = 5? Actually matches if data_test has 5 columns.

# The above works only if the number of columns equals number of samples? Not conceptually correct.
# The right approach: always assign column annotations to VARIABLES, not samples.

# --------------------------------------------
# 14. PROPER TEST WITH CORRECT ANNOTATIONS
# --------------------------------------------
# Re‑cluster the 5 columns of data_test as variables.
env_dist_test <- vegdist(t(data_test), method = "euclidean")
env_clust_test <- hclust(env_dist_test, method = "ward.D2")
env_groups_test <- cutree(env_clust_test, k = 2)  # k=2 because only 5 columns

# Build annotation_row (for rows = samples)
annotation_row_test <- data.frame(Cluster = factor(species_test))
rownames(annotation_row_test) <- rownames(data_test)

# Build annotation_col (for columns = variables)
annotation_col_test <- data.frame(Env_Cluster = factor(env_groups_test))
rownames(annotation_col_test) <- colnames(data_test)

# Check dimensions
print(dim(annotation_row_test))  # Should be 5 x 1
print(dim(annotation_col_test))  # Should be 5 x 1

# Plot heatmap with correct annotations
pheatmap(data_test,
         cluster_rows = hclust(dist(data_test)),
         cluster_cols = hclust(dist(t(data_test))),
         annotation_row = annotation_row_test,
         annotation_col = annotation_col_test)

# --------------------------------------------
# 15. SILHOUETTE PLOTS FOR THE TEST DATA
# --------------------------------------------
# For species clusters (based on samples)
sil_species <- silhouette(species_test, dist(data_test))
fviz_silhouette(sil_species) +
  theme_minimal() +
  labs(title = "Silhouette Plot for Species Clusters (Test Data)")

# For environmental clusters (based on variables – transposed)
sil_env <- silhouette(env_groups_test, dist(t(data_test)))
fviz_silhouette(sil_env) +
  theme_minimal() +
  labs(title = "Silhouette Plot for Environmental Clusters (Test Data)")

# --------------------------------------------
# 16. PERMANOVA: DO ENVIRONMENTAL CLUSTERS EXPLAIN SPECIES COMPOSITION?
# --------------------------------------------
# adonis2 performs a permutational multivariate ANOVA.
# Here we test if the environmental cluster assignment (for samples) explains the species distance matrix.
# Important: Env_Cluster must be a factor with the same length as the number of rows in species_dist.
permanova_result <- adonis2(species_dist ~ Env_Cluster,
                            data = data.frame(Env_Cluster = factor(env_groups)),  # env_groups is per sample
                            permutations = 999)
print(permanova_result)
# If p < 0.05, environmental clusters significantly correlate with species community structure.

# --------------------------------------------
# 17. ENHANCED HEATMAP WITH CONTINUOUS ENVIRONMENTAL VARIABLES
# --------------------------------------------
# This shows how to add extra annotation rows (for samples) with continuous variables like conductivity.
# Assume your_data contains columns "Conductivity_mS", "Water_Temp_°C", etc.

# Build annotation_row for all 12 samples (replace with actual row count)
# Note: In the original code, species_test had only 5 rows. For full data, use the original data_std.
annotation_row_full <- data.frame(
  Cluster = factor(species_groups),                     # from earlier (12 samples)
  Conductivity = your_data$Conductivity_mS,             # replace with exact column name
  Water_Temp = your_data$Water_Temp_°C,                 # adjust column name
  Oxygen_Saturation = your_data$Oxygen_Saturation_Rate_% # adjust column name
)
rownames(annotation_row_full) <- rownames(data_std)     # ensure row names match

# Build annotation_col for the environmental VARIABLES (9 variables)
# env_groups_vars was created earlier for the full set of env variables.
annotation_col_full <- data.frame(Env_Cluster = factor(env_groups_vars))
rownames(annotation_col_full) <- colnames(data_std[, env_columns])

# Optional: Add a continuous gradient for a specific variable in the column annotation.
# For example, if you want to colour the conductivity values of each environmental variable (not typical).
# Here we add Oxygen_Saturation_Rate as a gradient to the column annotation (just as demonstration).
annotation_col_full$Oxygen_Sat <- your_data$Oxygen_Saturation_Rate[1:ncol(annotation_col_full)]  # careful: this is contrived

# Define colour palettes
gradient_colors <- colorRampPalette(c("blue", "red"))(100)

# Plot final enhanced heatmap
pheatmap(data_std,
         cluster_rows = hclust(species_dist),          # using original distance
         cluster_cols = env_clust_vars,                # cluster environmental variables
         annotation_row = annotation_row_full,
         annotation_col = annotation_col_full,
         annotation_colors = list(
           Cluster = c("1" = "#1F77B4", "2" = "#FF7F0E", "3" = "#2CA02C"),  # fixed colours
           Env_Cluster = c("1" = "#E41A1C", "2" = "#377EB8", "3" = "#4DAF4A"),
           Conductivity = gradient_colors,
           Oxygen_Sat = gradient_colors
         ),
         main = "Heatmap of Standardized Variables with Annotations"
)

# End of script
