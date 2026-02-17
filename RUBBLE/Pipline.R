# ==========================================================
# RUBBLE Project – Master Pipeline for Multiple Stations
# ==========================================================

library(fs)
library(dplyr)
library(readxl)
library(writexl)
library(vegan)
library(ggplot2)
library(ggrepel)
library(compositions)
library(tidyr)
library(factoextra)
library(pheatmap)
library(purrr)
library(RColorBrewer)
library(vcd)

# ---------------------- USER SETTINGS ---------------------
stations <- c("SA3-1", "SA4_3", "SA5_3", "SAX_1")  # List of stations
seed_value <- 123
set.seed(seed_value)

base_data_folder <- "data"
base_output_folder <- "outputs"
n_images <- 100       # number of images per habitat
min_interval <- 8     # for image selection

# ---------------------- MASTER LOOP -----------------------
for(station in stations){
  
  cat("\n====================================================\n")
  cat("Processing station:", station, "\n")
  cat("====================================================\n")
  
  station_output <- file.path(base_output_folder, station)
  if(!dir.exists(station_output)) dir.create(station_output, recursive = TRUE)
  
  # ---------------------- 1️⃣ IMAGE SELECTION ----------------------
  source("scripts/01_image_selection.R")   # selects images per habitat
  
  # ---------------------- 2️⃣ BIODIVERSITY COVERAGE -----------------
  source("scripts/02_biodiversity_coverage.R")  # SAC & iNEXT per habitat
  
  # ---------------------- 3️⃣ BIOLOGICAL ANALYSIS -----------------
  source("scripts/03_biological_analysis.R")   # NMDS + pairwise PERMANOVA/PERMDISP
  
  # ---------------------- 4️⃣ GEOLOGICAL ANALYSIS -----------------
  source("scripts/04_geological_analysis.R")  # CLR + pairwise PERMANOVA/PERMDISP + PCA
  
  # ---------------------- 5️⃣ FUNCTIONAL ANALYSIS -----------------
  source("scripts/05_functional_analysis.R")  # NMDS + functional habitat + affinity
  
  cat("\nFinished station:", station, "\n\n")
}

cat("====================================================\n")
cat("Master pipeline completed for all stations!\n")
cat("Outputs saved under:", base_output_folder, "\n")
cat("====================================================\n")
