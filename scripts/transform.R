# This script performs ETL operations for demographic data, reducing, integrating, and exporting the datasets.
pacman::p_load(
  tidyverse,
  here,
  data.table,
  owidR
)

# Load required libraries for data manipulation, visualization, and analysis.

######################################
##### 1. Define Helper Functions #####
######################################

# 1. DATA LOADING AND SAVING

# Function to load CSV files
load_csv <- function(dir, file_path) {
  read_csv(here("data", dir, file_path))
}

# Function to save processed data
save_processed_data <- function(df, file_name) {
  file_path <- here("data", "processed", file_name)
  fwrite(df, file_path)
}

# 2. REDUCE DATA

# Function to reduce dataframe size
df_reduce <- function(df) {
  
  # "VarID", "Variant" are meaningful for fert df - may be preserved for different analysis
  cols_to_exclude <- c("sort_order", "notes", "sdmx_code", "mid_period", 
                       "age_grp_span", "var_id", "variant", "t_population1jan")
  
  existing_cols <- cols_to_exclude[cols_to_exclude %in% names(df)]
  
  df |> 
    filter(time > 1999, variant == "Medium") |> 
    select(-all_of(existing_cols))
}

# 3. COMPUTE NEW COLUMNS

# Function to add continent names for SDG regions
add_continent_names <- function(df){
  df |> 
    mutate(parent_name = case_when(
      parent_id == 903 ~ "Africa",
      parent_id == 935 ~ "Asia",
      parent_id == 908 ~ "Europe",
      parent_id == 904 ~ "Latin America and the Caribbean",
      parent_id == 909 ~ "Oceania",
      .default = ""
    )
    )
}

# Function to compute 5-year age bands
compute_5yr_age_bands <- function(df) {
  df |> 
    mutate(
      age_grp_5yr = cut(
        age_grp,
        breaks = seq(0, 100, 5),
        right = FALSE,
        labels = paste(seq(0, 95, 5), seq(4, 99, 5), sep="-")
      ),
      age_grp_5yr = ifelse(age_grp == 100, "100+", as.character(age_grp_5yr)),
      age_grp_start = parse_number(age_grp_5yr)
    )
}

# Function to add Labor-relevant age categories to single-age-level data
compute_age_cols <- function(df) {
  df |> 
    mutate(
      # Working Age Population (15-64) - (in COUNTRY level dataset)
      age_wa = case_when( 
        age_grp_start %in% 0:14 ~ "Children (0-14)",
        age_grp_start %in% 15:64 ~ "Working-Age (15-64)",
        age_grp_start >= 65 ~ "Old-Age (65+)",
        TRUE ~ "Other"  # Catch-all for any data that doesn't fit the specified groups
      ),
      # ILO working-Age classifications
      age_labor = case_when(
        age_grp_start %in% 0:14 ~ "Dependents (0-14)",
        age_grp_start %in% 15:24 ~ "Youth (15-24)",
        age_grp_start %in% 25:34 ~ "Younger Prime-age Workers (25-34)",
        age_grp_start %in% 35:54 ~ "Older Prime-age Workers (35-54)",
        age_grp_start >= 55 ~ "Older Workers (55+)"
      ),
      # Education level age classification
      age_ed = case_when(
        age_grp_start %in% 6:11 ~ "Primary (6-11)",
        age_grp_start %in% 12:17 ~ "Secondary (12-17)",
        age_grp_start %in% 18:23 ~ "Tertiary (18-23)",
        TRUE ~ "Other"  # For ages outside the specified ranges
      )
    )
}

# Function to compute in country-level data: 
  # 1) compute working age category, 
  # 2) compute working age category totals (per country & year)
  # 3) calculate dependency ratios (per country & year)
calculate_dependency_ratio <- function(df) {
  df |> 
    # 1) compute working age category
    mutate(
      age_wa = case_when(
        age_grp %in% 0:14 ~ "Children (0-14)",
        age_grp %in% 15:64 ~ "Working-Age (15-64)",
        age_grp >= 65 ~ "Old-Age (65+)",
        TRUE ~ "Other"  # Catch-all for any data that doesn't fit the specified groups
      )
    ) |>
    group_by(loc_type_id, loc_id, time) |> 
    # 2) compute working age category totals
    summarise(
      total_children = sum(pop_total[age_wa == "Children (0-14)"]),
      total_working_age = sum(pop_total[age_wa == "Working-Age (15-64)"]),
      total_old_age = sum(pop_total[age_wa == "Old-Age (65+)"]),
      .groups = "drop"
    )  |>
    # 3) calculate dependency ratios
    mutate(
      dep_ratio_total = 100 * (total_children + total_old_age) / total_working_age,
      dep_ratio_youth_1014 = 100 * total_children / total_working_age,
      dep_ratio_old_age_65 = 100 * total_old_age / total_working_age
    )
}

###########################
##### 2. Data Loading #####
###########################

# Load WPP raw data
raw_data <- list(
  dem = load_csv("raw","dem.csv"), # Country-level demographic data
  fert = load_csv("raw","fert.csv"), # Single-age fertility data
  pop = load_csv("raw","pop.csv") # Single-age demographic data
)

# Load country reference data from osaa repo
reference <- read_csv("https://raw.githubusercontent.com/UN-OSAA/cluster6.data/main/data/reference.csv") |> 
  select(iso3c, region, region_int, income, ldc, lldc, sids, lending) |> 
  rename(iso3_code = iso3c)

# OWID Long run population growth data
pop_long_run <- owid("population-long-run-with-projections") |> 
  pivot_longer(cols = 4:5, names_to = "source", values_to = "pop") |> 
  na.omit() |> 
  mutate(
    source = case_when(
      source=="Population (future projections) (future projections)" ~ "Projection",
      source=="Population (historical estimates)" ~ "Historical"
    )
)

#############################
##### 3. Data Reduction #####
#############################

# Reduce loaded data (from 2GB to 417MB)
reduced_data <- list(
  dem = df_reduce(raw_data$dem) |> select(-c(deaths:le80female), -c(q5:q1560female)),
  pop = df_reduce(raw_data$pop) |> mutate(age_grp = parse_number(age_grp)),
  fert = df_reduce(raw_data$fert)
)

#############################################################
##### 4. Generate aggregate and country level dataframes ####
#############################################################

# Join fertility and population single age data
joined_data <- list(
  age1 = reduced_data$pop |> 
    left_join(reduced_data$fert, 
              by = join_by(loc_id, iso3_code, iso2_code, loc_type_id, loc_type_name, parent_id, location, time, age_grp, age_grp_start))
)

# Process regional (aggregate) and country data separately
clean_agg <- function(df){
  df |> 
    filter(is.na(iso3_code)) |> 
    select(-c(iso3_code, iso2_code)) |> 
    add_continent_names()
}

clean_country <- function(df){
  df |> 
    filter(!is.na(iso3_code)) |> 
    select(-c(loc_type_name, parent_id))
}

# Generate processed dataframes
wpp <- list(
  # Regional (aggregate) data
  a_dem = reduced_data$dem |> clean_agg(),
  a_age1 = joined_data$age1 |> clean_agg(),
  
  # Country data
  c_dem = reduced_data$dem |> clean_country(),
  c_age1 = joined_data$age1 |> clean_country()
)

###########################################
##### 5. Dependency Ratios Calculation ####
###########################################

# Calculate dependency ratios
dep_ratios <- list(
  agg = calculate_dependency_ratio(wpp$a_age1),
  country = calculate_dependency_ratio(wpp$c_age1)
)

# Merge dependency ratios into regional and country data
wpp$a_dem <- wpp$a_dem |> left_join(calculate_dependency_ratio(wpp$a_age1))
wpp$c_dem <- wpp$c_dem |> left_join(calculate_dependency_ratio(wpp$c_age1))

#############################################################
##### 6. Compute all age categories' for single age DF ######
#############################################################

# Compute for single-age dataframe:
  # Working Age Population (15-64) - (in COUNTRY level dataset)
  # ILO working-Age classifications
  # Education level age classification

wpp$a_age1 <- wpp$a_age1 |> compute_age_cols()
wpp$c_age1 <- wpp$c_age1 |> compute_age_cols()

#####################################
##### 7. Age Bands Calculation ######
#####################################

# Compute 5-year age bands
wpp$a_age1 <- wpp$a_age1 |> compute_5yr_age_bands()
wpp$c_age1 <- wpp$c_age1 |> compute_5yr_age_bands()

###########################################################
##### 8. Add reference data to country-level dataframe ####
###########################################################

# Merge OSAA country level reference data
merge_ref <- function(df){
  df |> 
    left_join(reference, by = join_by(iso3_code)) |> 
    filter(!income %in% c("", "Not classified"), !is.na(region))
}

wpp$c_dem <- wpp$c_dem |> merge_ref()
wpp$c_age1 <- wpp$c_age1 |> merge_ref()
  
########################
#### 9. Data Export ####
########################

# Save processed data to CSV files
walk(names(wpp), ~save_processed_data(wpp[[.x]], paste0(.x, ".csv")))
save_processed_data(pop_long_run, "owid.csv")

