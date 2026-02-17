– R-Based Pipeline for Deep-Sea Benthic Assemblages

Author: Farzaneh Momtazi
Project: RUBBLE (Remote Underwater Benthic Biodiversity & Ecology)
Description: This repository contains a modular R-based pipeline for analyzing deep-sea benthic communities from image-based annotations, geological variables, and functional group data. It supports multiple stations (currently: SA3-1, SA4_3, SA5_3, SAX_1) and provides reproducible outputs for biodiversity, geological, and functional analyses.

Table of Contents

Repository Structure

Dependencies

Data Requirements

Workflow

Running the Pipeline

Outputs

Citation & License

Repository Structure
RUBBLE/
├── README.md
├── LICENSE
├── RUBBLE_pipeline.R        # Master pipeline script
├── scripts/                # Modular scripts
│   ├── 01_image_selection.R
│   ├── 02_biodiversity_coverage.R
│   ├── 03_biological_NMDS.R
│   ├── 04_geology_CLR_preparation.R
│   ├── 05_geology_multivariate.R
│   └── 06_functional_NMDS.R
├── data/                   # Example input files
│   ├── SA3-1_biological.xlsx
│   ├── SA3-1_geological.xlsx
│   └── functional_groups.xlsx
└── outputs/                # Output folders (not tracked in Git)
    ├── SA3-1/
    ├── SA4_3/
    ├── SA5_3/
    └── SAX_1/


Notes:

outputs/ should be listed in .gitignore because results can be reproduced from the input files.

All scripts are modular and rely on station_name and data paths defined in RUBBLE_pipeline.R.

Dependencies

The pipeline uses the following R packages:

readxl, openxlsx, writexl – for reading/writing Excel files

dplyr, tidyr, tibble – for data manipulation

vegan – for NMDS, species accumulation, PERMANOVA

ggplot2, ggrepel, RColorBrewer – for visualization

factoextra – PCA and clustering visualization

vcd – contingency table statistics

pheatmap – heatmaps for functional groups

fs – file handling

compositions – CLR transformations for geological variables

Install missing packages with:

install.packages(c("readxl","openxlsx","writexl","dplyr","tidyr","tibble",
                   "vegan","ggplot2","ggrepel","RColorBrewer",
                   "factoextra","vcd","pheatmap","fs","compositions"))

Data Requirements

Biological data: Excel file per station (station_name_biological.xlsx)

Must include image filenames, annotation area, habitat, and species abundances.

Geological data: Excel file per station (station_name_geological.xlsx)

Must include percentage coverage of geological features per image.

Functional groups: Single Excel file (functional_groups.xlsx)

Maps species codes to functional groups.

Example folder: data/ contains template files to illustrate formatting.

Workflow

Image selection: select representative images while considering minimum overlap.

Biodiversity coverage: check sufficiency of images via species accumulation curves.

Taxonomic NMDS: NMDS ordination of species densities.

Geological CLR preparation: Centered log-ratio transformation of geological variables.

Geological multivariate analyses: PCA, PERMANOVA, PERMDISP for habitat differentiation.

Functional group analyses: NMDS, clustering, functional habitat definition, and affinity to predicted habitats.

All analyses are modular and can be applied to each station independently.

Running the Pipeline

Clone the repository:

git clone https://github.com/<username>/RUBBLE.git


Open RUBBLE_pipeline.R in RStudio.

Define the stations you want to analyze:

stations <- c("SA3-1", "SA4_3", "SA5_3", "SAX_1")


Run the script. Each station will produce outputs in outputs/<station_name>/.

Outputs

For each station, the pipeline produces:

Selected images (from image selection module)

Species accumulation and coverage results

NMDS ordination plots (taxonomic and functional)

PCA, PERMANOVA, PERMDISP results for geological variables

Functional group composition tables and heatmaps

Affinity plots comparing functional habitats vs predicted multibeam habitats

All figures are saved in outputs/<station_name>/figures/.
All numeric tables are saved in outputs/<station_name>/results/.

Citation & License

Please cite the repository if used in publications.

License: MIT (or your preferred open-source license).