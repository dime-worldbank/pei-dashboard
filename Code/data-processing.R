# Load packages

packages <-
  c(
   "here",
   "tidyverse"
  )

pacman::p_load(
  packages,
  character.only = TRUE
)

# Load data --------------------------------------------------------------------

# Set file paths in your computer
if (Sys.getenv("USERNAME") %>% tolower == "wb501238") {
  onedrive <- "C:/Users/wb501238/Dropbox/WB/pei-dashboard"
}

# Load the raw survey data
raw <- 
  read.csv(
    here(
      onedrive,
      "Data",
      "raw",
      "Landscape Survey of Ongoing Economic Inclusion Impact Evaluations_WIDE.csv"
    )
  ) %>%
  select(
    -starts_with("country_"),
    -c(SubmissionDate:duration),
    -c(instanceID:formdef_version)
  )

# Survey options ---------------------------------------------------------------

source(
  here(
    "Dashboard",
    "auxiliary",
    "define_labels.R"
  )
)

# Process data -----------------------------------------------------------------

## Process PI information ------------------------------------------------------

pi <-
  raw %>%
  select(
    KEY,
    starts_with("pi_")
  ) %>%
  pivot_longer(
    cols = starts_with("pi_"),
    names_pattern = "pi_(.*)_(.)",
    names_to = c("variable", "rep")
  ) %>%
  filter(value != "") %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  select(-rep)

pi %>%
  select(
    KEY,
    affiliation # Removing names because that's sensitive information
  ) %>%
  write_rds(
    here(
      "Dashboard",
      "data",
      "pis.rds"
    )
  )

countries <-
  raw %>%
  select(
    KEY,
    country
  ) %>%
  separate(
    country,
    into = paste0("country_", 1:100)
  ) %>%
  pivot_longer(
    cols = starts_with("country"),
    names_prefix = "country",
    names_to = "count",
    values_to = "country_code"
  ) %>%
  select(-count) %>%
  filter(!is.na(country_code))
  

numeric <-
  raw %>%
  select(
    KEY,
    total_beneficiary,
    learning_priority
  )

labeled <-
  raw %>%
  transmute(
    KEY = KEY,
    geo_cov = geo_cov %>% str_replace_all(" ", "<br>") %>% str_replace_all(geo_cov_lab),
    lead = lead %>% str_replace_all(" ", "<br>") %>% str_replace_all(lead_lab),
    area = area %>% str_replace_all(" ", "<br>") %>% str_replace_all(area_lab),
    pov_seg = pov_seg %>% str_replace_all(" ", "<br>") %>% str_replace_all(pov_seg_lab),
    priority_group = priority_group %>% str_replace_all(" ", "<br>") %>% str_replace_all(priority_group_lab),
    target_method = target_method %>% str_replace_all(" ", "<br>") %>% str_replace_all(target_method_lab),
    target_level = target_level %>% str_replace_all(" ", "<br>") %>% str_replace_all(target_level_lab),
    ie_method = ie_method %>% str_replace_all(" ", "<br>") %>% str_replace_all(ie_method_lab),
    cluster = cluster %>% str_replace_all(" ", "<br>") %>% str_replace_all(cluster_lab)
  )

