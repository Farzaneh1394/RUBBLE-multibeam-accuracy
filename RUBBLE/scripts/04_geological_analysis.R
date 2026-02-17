# ==========================================================
# RUBBLE Project – Geological Analysis (CLR + PCA + PERMANOVA + PERMDISP)
# Module: 04_geology_multivariate.R
# ==========================================================

library(readxl)
library(dplyr)
library(compositions)    # CLR transformation
library(vegan)
library(writexl)
library(ggplot2)
library(ggrepel)
library(readr)
library(fs)

# ---------------------- USER SETTINGS --------------------
station_name  <- "SA3_1"
input_file    <- file.path("data", paste0(station_name, "_geological.xlsx"))
output_folder <- file.path("outputs", station_name, "Geology_results")
seed_value    <- 123
pseudo        <- 0.01   # pseudo-count for zeros in CLR

if(!dir_exists(output_folder)) dir_create(output_folder, recursive = TRUE)
set.seed(seed_value)

# ---------------------- READ DATA ------------------------
data <- read_excel(input_file)

geo_vars <- grep("%$", names(data), value = TRUE)  # columns ending with %
data[geo_vars] <- lapply(data[geo_vars], as.numeric)

# ---------------------- CLR TRANSFORMATION ----------------
geo_pc <- data[geo_vars] + pseudo
geo_clr <- clr(geo_pc)
geo_clr_df <- as.data.frame(geo_clr)
colnames(geo_clr_df) <- paste0(geo_vars, "_clr")
data_clr <- bind_cols(data, geo_clr_df)

write_xlsx(data_clr, file.path(output_folder, "data_geo_clr.xlsx"))

clr_cols <- grep("_clr$", names(data_clr), value = TRUE)

# ---------------------- PERMANOVA ------------------------
habitats <- unique(data_clr$Habitat)

# Overall PERMANOVA
dist_clr <- dist(data_clr[, clr_cols], method = "euclidean")
adon_res <- adonis2(dist_clr ~ Habitat, data = data_clr, permutations = 999)
overall_permanova <- tibble(
  Term = rownames(adon_res),
  Df = adon_res$Df,
  SumOfSqs = adon_res$SumOfSqs,
  R2 = adon_res$R2,
  F = adon_res$F,
  p = adon_res$`Pr(>F)`
)

# Pairwise PERMANOVA - manual loop
pairwise_permanova <- data.frame()
for(i in 1:(length(habitats)-1)){
  for(j in (i+1):length(habitats)){
    sub_data <- data_clr %>% filter(Habitat %in% c(habitats[i], habitats[j]))
    sub_dist <- dist(sub_data[, clr_cols], method = "euclidean")
    ad <- adonis2(sub_dist ~ Habitat, data = sub_data, permutations = 999)
    pairwise_permanova <- rbind(pairwise_permanova, data.frame(
      Group1 = habitats[i],
      Group2 = habitats[j],
      R2 = ad$R2[1],
      F = ad$F[1],
      p_value = ad$`Pr(>F)`[1]
    ))
  }
}
pairwise_permanova$p_adj <- p.adjust(pairwise_permanova$p_value, method = "bonferroni")

# ---------------------- PERMDISP ------------------------
disp_clr <- betadisper(dist_clr, data_clr$Habitat)
permdisp_res <- permutest(disp_clr, permutations = 999)

# Pairwise PERMDISP - manual loop
pairwise_disp <- data.frame()
for(i in 1:(length(habitats)-1)){
  for(j in (i+1):length(habitats)){
    sub_data <- data_clr %>% filter(Habitat %in% c(habitats[i], habitats[j]))
    sub_dist <- dist(sub_data[, clr_cols], method = "euclidean")
    disp_sub <- betadisper(sub_dist, sub_data$Habitat)
    disp_test <- anova(disp_sub)
    pairwise_disp <- rbind(pairwise_disp, data.frame(
      Group1 = habitats[i],
      Group2 = habitats[j],
      F_value = disp_test$`F value`[1],
      p_value = disp_test$`Pr(>F)`[1]
    ))
  }
}
pairwise_disp$p_adj <- p.adjust(pairwise_disp$p_value, method = "bonferroni")
pairwise_disp$Significance <- cut(pairwise_disp$p_adj,
                                  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                                  labels = c("***","**","*","ns"))

# ---------------------- PCA -----------------------------
pca_clr <- prcomp(data_clr[, clr_cols], center = TRUE, scale. = FALSE)

scores <- as.data.frame(pca_clr$x[,1:2])
scores$Habitat <- factor(data_clr$Habitat)
loadings <- as.data.frame(pca_clr$rotation[,1:2])
loadings$varname <- rownames(loadings)

arrow_scale <- 5
loadings[,1:2] <- loadings[,1:2] * arrow_scale
centroids <- aggregate(scores[,1:2], by=list(Habitat=scores$Habitat), FUN=mean)
var_explained <- summary(pca_clr)$importance[2,1:2]*100
x_lab <- paste0("PC1 (", round(var_explained[1],1), "%)")
y_lab <- paste0("PC2 (", round(var_explained[2],1), "%)")

p <- ggplot(scores, aes(PC1, PC2, colour=Habitat)) +
  geom_point(size=3, alpha=0.8) +
  geom_point(data=centroids, aes(PC1, PC2), inherit.aes=FALSE, size=6, shape=18, colour="darkgreen") +
  geom_text_repel(data=centroids, aes(PC1, PC2, label=Habitat), inherit.aes=FALSE,
                  fontface="bold", size=4, box.padding=0.2, point.padding=0.8,
                  min.segment.length=0, segment.color="grey40") +
  geom_segment(data=loadings, aes(x=0, y=0, xend=PC1, yend=PC2),
               arrow=arrow(length=unit(0.25,"cm")), linewidth=1, colour="purple") +
  geom_text_repel(data=loadings, aes(x=PC1, y=PC2, label=varname),
                  inherit.aes=FALSE, color="purple", size=3.5, box.padding=0.6,
                  point.padding=0.4, max.overlaps=Inf, force=3) +
  theme_classic() +
  labs(title=paste0("PCA of geological variables: ", station_name),
       x=x_lab, y=y_lab)

ggsave(file.path(output_folder,"PCA_plot.png"), plot=p, width=8, height=6)

# ---------------------- SAVE RESULTS ---------------------
write_csv(overall_permanova, file.path(output_folder,"PERMANOVA_overall.csv"))
write_csv(pairwise_permanova, file.path(output_folder,"PERMANOVA_pairwise.csv"))
write_csv(data.frame(permdisp_res$tab), file.path(output_folder,"PERMDISP_overall.csv"))
write_csv(pairwise_disp, file.path(output_folder,"PERMDISP_pairwise.csv"))

cat("--------------------------------------------------\n")
cat("Station:", station_name, "\n")
cat("Geological CLR + PCA + PERMANOVA + PERMDISP (pairwise loop) completed.\n")
cat("Results saved in:", output_folder, "\n")
cat("--------------------------------------------------\n")
