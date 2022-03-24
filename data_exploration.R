# LIBRARIES

library(dplyr)
library(tidyverse)
library(purrr)
library(janitor)
library(plotly)
library(lubridate)

# LOAD DATASET

data_path = "data/Patient Data/"

datafiles <- paste0(data_path, list.files('data/Patient Data', pattern="*.csv"))
list2env(
  lapply(setNames(datafiles, make.names(gsub("data/Patient Data/", "", gsub("*.csv$", "", datafiles)))), 
         read.csv), envir = .GlobalEnv)

# patients_id <- patients$patient

# CREATE PATIENT WISE DATASETS

data_list <- list(allergies = allergies, careplans = careplans, 
                  claims = claims, conditions = conditions, encounters = encounters, immunizations = immunizations,
                  medications = medications, observations = observations, procedures = procedures)

saveRDS(data_list, paste0(data_path, "raw_data_list.RDS"))

selected_patient_dataset <- function(selected_patient_id, dataset) {
  
  data_list <- map(data_list, ~clean_names(.x))
  sample_list <- data_list
  final_list <- map(sample_list, ~filter(.x, patient %in% selected_patient_id))
  final_list
}
