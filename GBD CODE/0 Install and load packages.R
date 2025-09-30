############################################################
# Step 1: Install and load required R packages
# This script automatically installs missing CRAN and Bioconductor packages.
############################################################

# ---- CRAN packages ----
cran_packages <- c(
  # General data manipulation and plotting
  "dplyr", "ggplot2", "readr", "here", "patchwork", 
  "tidyverse", "scales", "gridExtra", "grid","archive","gtable",
  
  # Mapping and spatial analysis
  "rnaturalearth", "sf", "tmaptools","rnaturalearthdata"
)

# ---- Bioconductor packages ----
# (Add any Bioconductor-specific packages your analysis uses here)
bioc_packages <- c(
  #
)

# ---- Install missing CRAN packages ----
installed_packages <- rownames(installed.packages())

for (pkg in cran_packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# ---- Install missing Bioconductor packages ----
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}
for (pkg in bioc_packages) {
  if (!pkg %in% installed_packages) {
    BiocManager::install(pkg, ask = FALSE, update = TRUE)
  }
}

# ---- Load all packages ----
all_packages <- c(cran_packages, bioc_packages)
lapply(all_packages, library, character.only = TRUE)

############################################################
# Packages loaded:
# ---- CRAN ----
# dplyr         - Data manipulation
# ggplot2       - Visualization
# readr         - Read CSV and other delimited files
# here          - Relative file paths
# patchwork     - Combine multiple ggplot figures
# tidyverse     - Collection of core data science packages
# scales        - Axis and scale formatting
# gridExtra     - Additional grid-based layout functions
# grid          - Grid graphics system
# rnaturalearth - Download and use Natural Earth map data
# sf            - Simple Features for spatial data
# tmaptools     - Map-related helper functions
#
# ---- Bioconductor ----
# (Add Bioconductor package descriptions here if applicable)
############################################################

