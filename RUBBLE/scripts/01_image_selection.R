# ---------------------- USER SETTINGS ---------------------
station_name <- "SA3_1"                # Name of the station
habitats     <- c("Sediment", "Transitional", "Hard_rock") # List of habitats in this station
input_root   <- "data"                 # Root folder for all stations
output_root  <- "outputs"              # Root folder for outputs

n_images     <- 100                     # Number of images per habitat
min_interval <- 8                       # Minimum interval
seed_value   <- 123                      # Reproducibility

# -----------------------------------------------------------

library(fs)
library(dplyr)
library(openxlsx)

set.seed(seed_value)

for (habitat_name in habitats) {
  
  input_folder  <- file.path(input_root, "image", station_name, habitat_name)
  output_folder <- file.path(output_root, station_name, habitat_name, "selected_images")
  
  # List JPG files
  image_files <- dir_ls(input_folder, glob = "*.JPG")
  if (length(image_files) == 0) {
    warning("No JPG files found in: ", input_folder)
    next
  }
  
  image_data <- data.frame(
    filename = basename(image_files),
    number   = seq_along(image_files)
  )
  
  shuffled_data <- image_data[sample(nrow(image_data)), ]
  selected_rows <- data.frame()
  
  for (i in 1:nrow(shuffled_data)) {
    current_row <- shuffled_data[i, ]
    current_number <- current_row$number
    
    if (nrow(selected_rows) == 0 ||
        all(abs(selected_rows$number - current_number) >= min_interval)) {
      selected_rows <- bind_rows(selected_rows, current_row)
    }
    
    if (nrow(selected_rows) >= n_images) break
  }
  
  # Create output folder if needed
  if (!dir_exists(output_folder)) dir_create(output_folder, recursive = TRUE)
  
  # Copy images
  matching_files <- image_files[
    basename(image_files) %in% selected_rows$filename
  ]
  file_copy(matching_files, output_folder, overwrite = TRUE)
  
  # Save table
  write.xlsx(selected_rows,
             file.path(output_folder, "selected_images.xlsx"))
  
  cat("--------------------------------------------------\n")
  cat("Station:", station_name, "- Habitat:", habitat_name, "\n")
  cat("Input folder:", input_folder, "\n")
  cat("Output folder:", output_folder, "\n")
  cat("Selected images:", nrow(selected_rows), "\n")
  cat("--------------------------------------------------\n\n")
}
