# ==========================================================
# RUBBLE Project – Biodiversity Coverage Script
# Module: 02_biodiversity_coverage.R
# ==========================================================

library(readxl)
library(dplyr)
library(writexl)
library(vegan)
library(iNEXT)

# ---------------------- USER SETTINGS ---------------------
# این متغیرها توسط master loop پر می‌شوند:
# station_name
# base_data_folder
# base_output_folder
# seed_value

set.seed(seed_value)
nrep <- 200  # برای species accumulation

# ---------------------- FUNCTION --------------------------
accum_by_area <- function(mat, areas, nrep = 100){
  n <- nrow(mat)
  res <- list()
  for(r in 1:nrep){
    ord <- sample(1:n)
    seen <- rep(0, ncol(mat))
    S <- numeric(n)
    cum_area <- cumsum(areas[ord])
    for(i in seq_len(n)){
      seen <- seen + (mat[ord[i], ] > 0)
      S[i] <- sum(seen > 0)
    }
    res[[r]] <- data.frame(rep = r, cum_area = cum_area, cum_S = S)
  }
  bind_rows(res)
}

# ---------------------- READ DATA -------------------------
input_file <- file.path(base_data_folder, paste0(station_name, "_biological.xlsx"))
if(!file.exists(input_file)){
  warning("Biological file not found for station: ", station_name)
} else {
  
  df <- read_excel(input_file)
  
  species_cols <- setdiff(names(df), c("image_filename", "annotation_area_sqm", "Habitat"))
  df[species_cols] <- lapply(df[species_cols], function(x) as.numeric(as.character(x)))
  
  mat <- as.matrix(df[, species_cols])
  areas <- df$annotation_area_sqm
  
  # ---------------------- SPECIES ACCUMULATION -----------------
  accum_results <- accum_by_area(mat, areas, nrep = nrep)
  summary_curve <- accum_results %>%
    group_by(cum_area) %>%
    summarise(mean_S = mean(cum_S),
              lower = quantile(cum_S, 0.025),
              upper = quantile(cum_S, 0.975))
  
  # iNEXT coverage
  abundance <- colSums(mat)
  if(sum(abundance) > 0){
    data_info <- iNEXT::DataInfo(abundance, datatype = "abundance")
  } else {
    data_info <- tibble(Species = species_cols, Abundance = 0)
  }
  
  # ---------------------- OUTPUT ---------------------------
  output_folder <- file.path(base_output_folder, station_name, "coverage_results")
  if(!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
  
  write_xlsx(data_info, file.path(output_folder, "iNEXT_data_info.xlsx"))
  write_xlsx(summary_curve, file.path(output_folder, "species_accum_summary.xlsx"))
  
  # Species accumulation curve
  community_data <- df[, species_cols]
  community_data[is.na(community_data)] <- 0
  sac <- specaccum(community_data, method = "random")
  
  png(file.path(output_folder, "species_accum_curve.png"), width = 800, height = 600)
  plot(sac,
       xlab = "Number of Samples",
       ylab = "Accumulated Species",
       main = paste("Species Accumulation Curve:", station_name),
       col = "darkgreen",
       lwd = 2,
       ci.type = "poly",
       ci.col = "lightgreen",
       ci.lty = 0)
  dev.off()
  
  # Try Lomolino fit (skip if fails)
  try({
    sac_fit <- fitspecaccum(sac, model = "lomolino")
    png(file.path(output_folder, "species_accum_lomolino.png"), width = 800, height = 600)
    plot(sac_fit)
    dev.off()
  }, silent = TRUE)
  
  cat("Biodiversity coverage completed for station:", station_name, "\n")
}
