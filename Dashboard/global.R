# Load packages ----------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(shinyhelper)
library(shinycssloaders)


library(tidyverse)
library(ggplot2)
library(sf)

library(DT)
library(plotly)

# Options ----------------------------------------------------------------------

categorical_colors <-
  c(
    "#18BC9C",
    "#2C3E50",
    "#F39C12",
    "#E74C3C",
    "#3498DB",
    "#18BC9C",
    "#2C3E50",
    "#F39C12"
  )

# Survey options ---------------------------------------------------------------

source(
  file.path(
    "auxiliary",
    "define_labels.R"
  )
)

for (object in ls()) {
  if (object != "column_list") {
    assign(
      object,
      object %>% get %>% unname
    )
  }
}

# Load data --------------------------------------------------------------------

pi_data <-
  read_rds(
    file.path(
      "data",
      "pis.rds" 
    )
  )

pi_affiliation_lab <-
  pi_data %>%
  pull(affiliation) %>%
  unique

project_data <-
  read_rds(
    file.path(
      "data",
      "projects.rds"
    )
  )

map <-
  read_rds(
    file.path(
      "data",
      "map.rds"
    )
  )

centroids <-
  read_rds(
    file.path(
      "data",
      "centroids.rds"
    )
  )

projects_country <-
  read_rds(
    file.path(
      "data",
      "projects-country.rds"
    )
  )

priorities <-
  read_rds(
    file.path(
      "data",
      "priorities.rds"
    )
  )

targeting <-
  read_rds(
    file.path(
      "data",
      "targeting.rds"
    )
  )

# List of learning priorities --------------------------------------------------

learning_priority <-
  list(
    "Effectiveness at scale" = 
      c(
        "Cost-effectiveness of large-scale government-led programs" = "1",
        "Spillovers and general equilibrium effects" = "2"
      ),
    "Scalable delivery modalities" =  c("Effects on impact and cost-effectiveness" = "3"), 
    "Dynamics over time" = c("How impacts vary over time" = "4"),
    "Bundling of Interventions" =
      c(
        "Optimal bundle/marginal contribution of constituent interventions" = "5",
        "Timing, sequencing, and intensity" = "6"
      ),
    "Targeting/heterogeneity" =
      c(
        "Cost-effectiveness across population groups" = "7",
        "Increasing cost-effectiveness for sub-groups" = "8"
      ), 
    "External validity" =
      c(
        "Adapting to urban contexts" = "9",
        "Adapting to fragile contexts"= "10"
      ),
    "Resilience and shock-responsiveness" = c("Effects on resilience and mechanisms" = "11"),
    "Other" = c("Other" = "12")
  )

# Map colors -------------------------------------------------------------------

region_colors <-
  c(
    "#688ade",
    "#d1a04b",
    "#b86867",
    "#76abad",
    "#936d9e",
    "#6d9e7d",
    "#dbcd60"
  )

# Functions --------------------------------------------------------------------

strs_detect_any <- 
  function(x, y) {
    sapply(y, 
           str_detect, 
           string = x) %>%
      rowSums() > 0
  }

shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(id[i], ...))
  }
  inputs
}


st_crs(map) <- "+proj=robin"
st_crs(centroids) <- "+proj=robin"