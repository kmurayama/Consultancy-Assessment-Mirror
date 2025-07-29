# load.R
# This script load the data files, clean them up, and merge them into one file.

# Initial set up ---------------------------------------------------------------
# Load up libraries and set parameters
library(tidyverse)
library(readxl)
new_data_path <- "data"
plot_path <- "plots"
# Check the output storage folders
if (!dir.exists(new_data_path)) {
  dir.create(new_data_path)
  message(paste(new_data_path, "directory created successfully."))
} else {
  message("Directory already exists.")
}
if (!dir.exists(plot_path)) {
  dir.create(plot_path)
  message(paste(plot_path, "directory created successfully."))
} else {
  message("Directory already exists.")
}

# Load datasets
cares_raw <- read_csv('01_rawdata/unicef-data-20250727.csv')
pop_raw <- read_xlsx('01_rawdata/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx',
                     sheet = "Projections", skip = 16,
                     col_types = 'text') # Avoid wrong types
mortality <- read_xlsx('01_rawdata/On-track and off-track countries.xlsx')


# Clean up data 1 Care-related data -------------------------------------------
# (The data comes with definitions. See RSDMX for direct download and handling.
# Here, it's handled manually.)
# First, split the columns into value and definition columns.
cares <- cares_raw %>%
  rename_with(~ str_remove(.x, ":.*"), .cols = contains(":")) %>% 
  separate_wider_delim(cols = everything(),
                       delim = ":", names_sep = "_", too_few = "align_start") %>% 
  mutate_all(str_trim)
# Then, select the relevant columns for this project and fix the types.
cares <- cares %>%
  select(ISO3Code = REF_AREA_1, 
         OfficialName = REF_AREA_2, 
         Indicator = INDICATOR_1,
         Year = TIME_PERIOD_1, 
         Value = OBS_VALUE_1) %>% 
  mutate(across(c(Year, Value), as.numeric))
# Finally, filter observations to keep the country observations only (no region)
# and the latest data within the range given.
cares <- cares %>% 
  filter(Year > 2017, Year < 2023) %>% 
  filter(str_length(ISO3Code) == 3) %>% 
  group_by(ISO3Code) %>% slice_max(Year) %>% ungroup()
# In addition, catalog available countries for checks
cares_area <- levels(as_factor(cares$ISO3Code))

# Clean up data 2 Population data ---------------------------------------------
# Select the relevant columns for this project.
pop <- pop_raw %>% select(OfficialName = `Region, subregion, country or area *`,
                          ISO3Code = `ISO3 Alpha-code`, Year,
                          Births = `Births (thousands)`) %>% 
  mutate(across(c(Year, Births), as.numeric))
# Then, filter observations for the country and the year 2022.
pop <- pop %>% 
  filter(Year == 2022) %>% 
  filter(str_length(ISO3Code) == 3)
# Check the country availability in the data
pop_area <- levels(as_factor(pop$ISO3Code))
if (sum(!cares_area %in% pop_area) == 0){
  message("All the countries in the base data are covered by population data")
} else {
  message("Missing the following countries in the population data:")
  message(paste(cares_area[!cares_area %in% pop_area], collapse = " "))
}

# Clean up data 3 Mortality data ----------------------------------------------
# Adjust the status per the requirement 
mortality <- mortality %>% mutate(Pace = case_when(
  Status.U5MR == "Acceleration Needed" ~ "Off-track",
  Status.U5MR == "On Track" ~ "On-track",
  Status.U5MR == "Achieved" ~ "On-track",
  TRUE ~ "Other"
))
# Check the country availability in the data
mort_area <- levels(as_factor(mortality$ISO3Code))
if (sum(!cares_area %in% mort_area) == 0){
  message("All the countries in the base data are covered by mortality data")
} else {
  message("Missing the following countries in the mortality data:")
  message(paste(cares_area[!cares_area %in% mort_area], collapse = " "))
} # 2 countries missing

################################################################################
# Join the datasets 
# Use the care index data (UNICEF) as the basis. Drop missing cases.
merged <- cares %>%
  left_join(pop, by = join_by(ISO3Code), suffix = c("_index", "_birth")) %>%
  left_join(mortality, by = join_by(ISO3Code))
# Take care of the observations with missing values 
merged <- drop_na(merged)

# Save the merged data file 
saveRDS(merged, "data/merged.Rds")

