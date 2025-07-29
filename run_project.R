# run_project.R
# This script automate the report production

# Check the data files
check_data <- function(file_name, file_dir = "01_rawdata"){
  # Check the existence of data files before running scripts
  file_path = paste0(file_dir, file_name)
  if (file.exists(file_path)) {
  } else {
    message(paste("The data file ", file_name, "does not exist."))
  }
  }

# Prepare the data 
source("scripts/load.R")