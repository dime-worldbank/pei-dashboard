# Load packages ----------------------------------------------------------------

packages <-
  c(
   "here",
   "tidyverse",
   "rnaturalearth",
   "sf"
  )

pacman::p_load(
  packages,
  character.only = TRUE
)

# Country codes
countries <-
  read_csv(
    here(
      "Documentation",
      "country-codes.csv"
    )
  ) %>%
  mutate(country_code = as.character(country_code))

# Process survey data ---------------------------------------------------------

## Load data -------------------------------------------------------------------

country_lab <-
  countries$country

names(country_lab) <-
  countries$country_code

# Load the raw survey data
raw <- 
  read.csv(
    here(
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

## Survey options --------------------------------------------------------------

source(
  here(
    "Dashboard",
    "auxiliary",
    "define_labels.R"
  )
)

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

pi_affiliation <-
  pi %>%
  select(
    KEY,
    affiliation # Removing names because that's sensitive information
  ) 

pi_affiliation %>%
  write_rds(
    here(
      "Dashboard",
      "data",
      "pis.rds"
    )
  )

countries <-
pi_affiliation_project <-
  pi_affiliation %>%
  group_by(KEY) %>%
  summarize(
    pi_affiliation = paste(affiliation %>% unique, collapse = "<br>")
  )

## Process project information -------------------------------------------------

### Label variables ------------------------------------------------------------

label_select_one <-
  function(x, label, other = NULL) {
    var <- 
      str_replace_all(
        string = x,
        pattern = label
      )
    
    if (!is.null(other)) {
      var <-
        str_replace_all(
          string = var,
          pattern = "Other",
          replace = paste("Other:", other)
        )
    }

    return(var)
  }

label_select_multiple <-
  function(x, label, other = NULL) {
    var <-
      x %>%
      str_replace_all(" ", "<br>") %>%
      str_replace_all(label)
    
    if (!is.null(other)) {
      var <-
        str_replace_all(
          string = var,
          pattern = "Other",
          replace = paste("Other:", other)
        )
    }
    
    return(var)
  }

labeled <-
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
  
  transmute(
    KEY = KEY,
    lead = label_select_one(lead, lead_lab, lead_s),
    target_level = label_select_one(target_level, target_level_lab),
    geo_cov = label_select_one(geo_cov, geo_cov_lab, geo_cov_s),
    area = label_select_multiple(area, area_lab),
    pov_seg = label_select_multiple(pov_seg, pov_seg_lab, pov_seg_s),
    priority_group = label_select_multiple(priority_group, priority_group_lab, priority_group_s),
    target_method = label_select_multiple(target_method, target_method_lab, target_method_s),
    ie_method = label_select_multiple(ie_method, ie_method_lab, ie_method_s),
    cluster = label_select_one(cluster, cluster_lab),
    country = label_select_multiple(country, country_lab)
  )

### Combine all data -----------------------------------------------------------

data <-
  raw %>%
  select(
    KEY,
    total_beneficiary,
    learning_priority
  ) %>%
  left_join(labeled) %>%
  left_join(pi_affiliation_project)

data %>%
  write_rds(
    here(
      "Dashboard",
      "data",
      "projects.rds"
    )
  )

# Process geospatial data ------------------------------------------------------

map <- 
  read_rds(
    here(
      "Data",
      "raw",
      "wb_country_geom.rds"
    )
  ) %>%
  left_join(countries)


