
# Replication Script for the Paper:
# "Voting for Deforestation in the Brazilian Cerrado"
# Authors: Gabriel Granjo and Maria Luiza Sousa

# This script replicates the analysis and figures presented in the paper.

# Clear the environment
rm(list = ls())

# Load necessary libraries
library(fixest)
library(sensemakr)
library(tidyverse)
library(readr)

# Load data
data <- read.csv("final_dataset.csv")

# Model estimation
model <- feols(increment ~ hhi_pref + hhi_gov + hhi_de + hhi_df + hhi_ver + log(total_pasture_agr) + log(Forest.Formation) + log(Soy.Beans + 1) +
                 Urban.Infrastructure_l + log(Total_GDP) + log(total_exp + 1) + log(total_imp + 1) | id_mun + micro_region + year, 
                 vcov = "cluster", 
                 ssc = ssc(adj = TRUE, fixef.K = "nested", cluster.adj = TRUE, fixef.force_exact = TRUE),
                 data = data)
summary(model)

# Sensitivity analysis for 'hhi_de'
sens_hhi_de <- sensemakr(
  model,
  treatment = "hhi_de",
  benchmark_covariates = "log(Forest.Formation)",
  kd = 2
)

jpeg("hhi_de_sens.jpeg", units = "in", width = 4, height = 4, res = 1000)
ovb_contour_plot(sens_hhi_de, label.bump.x = 0.08, label.bump.y = 0.04)
dev.off()

# Sensitivity analysis for 'hhi_df'
sens_hhi_df <- sensemakr(
  model,
  treatment = "hhi_df",
  benchmark_covariates = "log(Forest.Formation)",
  kd = 2
)

jpeg("hhi_df_sens.jpeg", units = "in", width = 4, height = 4, res = 1000)
ovb_contour_plot(sens_hhi_df, label.bump.x = 0.05, label.bump.y = 0.04)
dev.off()
