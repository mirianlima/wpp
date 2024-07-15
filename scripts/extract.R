# Load required libraries
pacman::p_load(
  tidyverse,
  here,
  janitor,
  data.table
)

######################################
##### 1. Define Helper Functions #####
######################################

read_zip_csv_from_url <- function(url) {
  temp_zip <- tempfile(fileext = ".zip")
  temp_dir <- tempfile()
  
  download.file(url, temp_zip) # Download
  unzip(temp_zip, exdir = temp_dir) # Unzip
  
  # Find the CSV file in the temp directory - assumes only 1 csv file
  csv_file <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE)
  
  # Ensure that there is exactly one CSV file
  if (length(csv_file) != 1) {
    stop("Expected exactly one CSV file in the ZIP archive, but found: ", length(csv_files))
  }
  
  # Read the CSV file with fread
  data <- fread(csv_file[1]) |> clean_names()
  
  # Clean up temp files
  unlink(temp_zip)
  unlink(temp_dir, recursive = TRUE)
  
  return(data)
}

# Function to save data to CSV file in the "raw" folder
save_raw_data <- function(df, file_name) {
  file_path <- here("data", "raw", file_name)
  fwrite(df, file_path)
}

#############################################
##### 2. Get URLs and Meta Data #############
#############################################

# WPP Data URLs (All Data is MEDIUM Variant) - source: https://population.un.org/wpp/Download/Standard/MostUsed/
wpp_urls <- list(
  ref = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Demographic_Indicators_notes.csv",
  dem = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Demographic_Indicators_Medium.zip",
  fert = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Fertility_by_Age1.zip",
  pop1950_2021 = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.zip",
  pop2022_2100 = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Population1JanuaryBySingleAgeSex_Medium_2022-2100.zip"
)

##############################################
##### 3. Load WPP Data from URLs #############
##############################################

# Load all WPP raw data into list
wpp_raw_data <- list(
  dem = read_zip_csv_from_url(wpp_urls$dem),
  fert = read_zip_csv_from_url(wpp_urls$fert),
  # Load and bind both periods for population data
  pop = rbind(
    pop1950_202 = read_zip_csv_from_url(wpp_urls$pop1950_202),
    pop2022_2100 = read_zip_csv_from_url(wpp_urls$pop2022_2100)
  )
)

# Load and save WPP reference csv file (Codebook for demographic indicators)
codebook <- fread(wpp_urls$ref) 
fwrite(codebook, here("data", "codebook.csv"))

##############################################
##### 4. Save Raw Data to CSV Files ##########
##############################################

# Save all WPP raw data to data/raw folder
walk(names(wpp_raw_data), ~save_raw_data(wpp_raw_data[[.x]], paste0(.x, ".csv")))
