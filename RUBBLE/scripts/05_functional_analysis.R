# ==========================================================
# RUBBLE Project – Functional Group Analysis per Station
# Module: 05_functional_analysis.R
# ==========================================================

library(readxl)
library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)
library(ggrepel)
library(factoextra)
library(pheatmap)
library(writexl)
library(vcd)

# ---------------------- USER SETTINGS ---------------------
station_name <- "SA3_1"   # Example station
input_file_bio <- file.path("data", paste0(station_name, "_geobio_dataset.xlsx"))
input_file_fg  <- file.path("data", paste0(station_name, "_functional_groups.xlsx"))
output_folder  <- file.path("outputs", station_name, "Functional_results")
seed_value     <- 123

if(!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
set.seed(seed_value)

# ---------------------- READ DATA -------------------------
df <- read_excel(input_file_bio)
fg <- read_excel(input_file_fg)

# ---------------------- PREPARE COMMUNITY MATRIX ----------
species_cols <- names(df)[4:ncol(df)]
long_df <- df %>%
  pivot_longer(cols = all_of(species_cols),
               names_to = "Species_code",
               values_to = "abundance")

functional_matrix <- long_df %>%
  left_join(fg, by = "Species_code") %>%
  group_by(image_filename, group) %>%
  summarise(total_abundance = sum(abundance, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = group, values_from = total_abundance, values_fill = 0)

final_df <- df %>%
  select(image_filename, annotation_area_sqm, PredictedHabitat) %>%
  left_join(functional_matrix, by = "image_filename")

func_cols <- final_df %>%
  select(-image_filename, -annotation_area_sqm, -PredictedHabitat) %>%
  mutate(across(everything(), as.numeric))

comm <- as.data.frame(func_cols)

# ---------------------- ADD DUMMY SPECIES -----------------
comm_dummy <- comm
comm_dummy$dummy <- 1   # Clarke et al. 2006

# ---------------------- HELLINGER + NMDS -----------------
comm_hell <- comm_dummy

nmds <- metaMDS(comm_hell, distance = "bray", k = 2, trymax = 100, autotransform = FALSE)
site_scores <- as.data.frame(scores(nmds, display = "sites"))

nmds_df <- site_scores %>%
  mutate(image_filename = final_df$image_filename,
         PredictedHabitat = final_df$PredictedHabitat)

# ---------------------- K-MEANS CLUSTERING ---------------
# Decide number of functional habitats (adjustable per station)
fviz_nbclust(nmds_df[, c("NMDS1", "NMDS2")], kmeans, method = "silhouette")

set.seed(seed_value)
km <- kmeans(nmds_df[, c("NMDS1", "NMDS2")], centers = 4, nstart = 50)
nmds_df$RealHabitat <- factor(km$cluster, labels = paste0("Functional_Hab", 1:4))

# ---------------------- NMDS PLOT -------------------------
p <- ggplot(nmds_df, aes(NMDS1, NMDS2, color = RealHabitat)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal(base_size = 14) +
  labs(title = paste("NMDS of Functional Groups –", station_name),
       color = "Functional Habitat")

ggsave(file.path(output_folder, "NMDS_plot_Functional.png"), plot = p, width = 8, height = 6)
write_xlsx(nmds_df, file.path(output_folder, "NMDS_scores_Functional.xlsx"))

# ---------------------- FUNCTIONAL HABITAT VS MULTIBEAM ----
tab <- table(Real = nmds_df$RealHabitat, Predicted = nmds_df$PredictedHabitat)
write_xlsx(as.data.frame(tab), file.path(output_folder, "Functional_vs_Multibeam.xlsx"))

# Association statistics
assoc_stats <- vcd::assocstats(tab)
write_xlsx(as.data.frame(assoc_stats$chisq_tests), file.path(output_folder, "Functional_vs_Multibeam_chisq.xlsx"))

# ---------------------- REMOVE DUMMY -----------------------
comm_nodummy <- comm

# ---------------------- FUNCTIONAL COMPOSITION -----------
comm_func <- cbind(comm_nodummy, RealHabitat = nmds_df$RealHabitat)
cluster_totals <- comm_func %>%
  group_by(RealHabitat) %>%
  summarise(across(where(is.numeric), sum), .groups = "drop")

cluster_percent <- cluster_totals %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~ .x / sum(c_across(where(is.numeric))) * 100)) %>%
  ungroup()

# ---------------------- HEATMAP ---------------------------
mat <- as.matrix(cluster_percent[, -1])
rownames(mat) <- cluster_percent$RealHabitat

pheatmap(mat,
         cluster_rows = FALSE,
         cluster_cols = TRUE,
         fontsize = 8,
         main = paste("Functional Group Share (%) per Functional Habitat –", station_name))

# ---------------------- DOMINANT FUNCTIONAL GROUPS ---------
cluster_percent_long <- cluster_percent %>%
  pivot_longer(-RealHabitat, names_to = "FunctionalGroup", values_to = "Percent") %>%
  filter(Percent >= 5)

p2 <- ggplot(cluster_percent_long, aes(x = FunctionalGroup, y = Percent, fill = RealHabitat)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#D55E00","#0072B2","#009E73","#CC79A7","#F0E442")) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  labs(y = "Contribution (%)", x = "Functional group",
       title = paste("Functional Composition of Functional Habitats –", station_name))

ggsave(file.path(output_folder, "Functional_Composition.png"), plot = p2, width = 8, height = 6)

cat("Functional analysis completed for station:", station_name, "\n")
