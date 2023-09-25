library(dplyr)
library(visdat)
library(tidyverse)
library(factoextra)

# Step 1: Import
med_data <- read.csv("performance review/clean_data.csv")

# Step 2: Define features for PCA
med_nums <- med_data %>%
  select(c('Income', 'VitD_levels', 'VitD_supp', 'TotalCharge', 'Additional_charges'))

# Step 3. Normalize Data (to prevent one variable being overly influential) and Apply PCA
med_pca <- prcomp(med_nums, center = TRUE, scale. = TRUE)

# Step 4. PCA Loading
med_pca$rotation

# Step 5. Selecting PCs
fviz_eig(med_pca, choice = "eigenvalue", addlabels = TRUE)


# Visualize
fviz_pca_var(med_pca, col.var = "purple")
fviz_cos2(med_pca, choice = "var", axes = 1:2)
