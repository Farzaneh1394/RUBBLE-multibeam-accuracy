# ==========================================================
# RUBBLE Project – Biological Analysis per Station
# Module: 03_biological_analysis.R
# ==========================================================

library(readxl)
library(dplyr)
library(vegan)
library(ggplot2)
library(writexl)

# ---------------------- USER SETTINGS ---------------------
station_name <- "SA3_1"                 # Example station
input_file   <- file.path("data", paste0(station_name, "_biological.xlsx"))
output_folder <- file.path("outputs", station_name, "Biological_results")
seed_value    <- 123

if(!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
set.seed(seed_value)

# ---------------------- READ DATA -------------------------
df <- read_excel(input_file)

# ---------------------- ENSURE HABITAT COLUMN ----------------
if(!"Habitat" %in% colnames(df)){
  df$Habitat <- "Unknown"
} else {
  # NAها را به "Unknown" تبدیل کن
  df$Habitat[is.na(df$Habitat)] <- "Unknown"
}

# ستون Habitat را factor کن تا ggplot رنگ‌بندی کند
df$Habitat <- factor(df$Habitat)

# ---------------------- SPECIES COLUMNS -------------------
species_cols <- setdiff(names(df), c("image_filename", "annotation_area_sqm", "Habitat"))

# ستون‌های گونه را numeric کن و NAها را صفر کن
df[ , species_cols] <- lapply(df[ , species_cols], function(x) as.numeric(as.character(x)))
df[ , species_cols][is.na(df[ , species_cols])] <- 0

# ---------------------- COMMUNITY MATRIX ------------------
comm <- as.data.frame(df[ , species_cols])
row.names(comm) <- df$image_filename

# ---------------------- NMDS -----------------------------
if(nrow(comm) < 2 | ncol(comm) < 2){
  warning("Not enough data for NMDS. Skipping NMDS for this station.")
} else {
  nmds <- metaMDS(comm, distance = "bray", k = 2, trymax = 100, autotransform = FALSE)
  
  site_scores <- as.data.frame(scores(nmds, display = "sites"))
  site_scores$image_filename <- df$image_filename
  site_scores$Habitat <- df$Habitat
  
  # ---------------------- NMDS PLOT -----------------------
  p <- ggplot(site_scores, aes(x = NMDS1, y = NMDS2, color = Habitat)) +
    geom_point(size = 3, alpha = 0.8) +
    theme_minimal(base_size = 14) +
    labs(title = paste("NMDS of Biological Data –", station_name),
         color = "Habitat") +
    scale_color_brewer(palette = "Set2") # رنگ‌بندی استاندارد
  
  ggsave(file.path(output_folder, "NMDS_plot_Biological.png"), plot = p, width = 8, height = 6)
  write_xlsx(site_scores, file.path(output_folder, "NMDS_scores_Biological.xlsx"))
}

# ---------------------- PAIRWISE PERMANOVA / PERMDISP ----------------
habitats <- levels(df$Habitat)

if(length(habitats) > 1){
  perm_results <- list()
  disp_results <- list()
  
  for(i in 1:(length(habitats)-1)){
    for(j in (i+1):length(habitats)){
      h1 <- habitats[i]
      h2 <- habitats[j]
      sub_comm <- comm[df$Habitat %in% c(h1, h2), ]
      sub_group <- df$Habitat[df$Habitat %in% c(h1, h2)]
      
      # PERMANOVA
      adonis_res <- adonis2(sub_comm ~ sub_group, method = "bray")
      perm_results[[paste(h1, h2, sep="_vs_")]] <- adonis_res
      
      # PERMDISP
      dist_mat <- vegdist(sub_comm, method = "bray")
      disp <- betadisper(dist_mat, sub_group)
      disp_test <- permutest(disp)
      disp_results[[paste(h1, h2, sep="_vs_")]] <- disp_test
    }
  }
  
  saveRDS(perm_results, file.path(output_folder, "pairwise_PERMANOVA.rds"))
  saveRDS(disp_results, file.path(output_folder, "pairwise_PERMDISP.rds"))
}

cat("Biological analysis completed for station:", station_name, "\n")
