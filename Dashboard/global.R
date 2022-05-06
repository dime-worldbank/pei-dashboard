# Load packages ----------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(DT)
library(here)
library(shinyWidgets)
library(dplyr)
library(shinyjs)
library(sf)
library(ggplot2)
library(plotly)
library(leaflet.extras)
library(leaflet)

# Options ----------------------------------------------------------------------

# Survey options ---------------------------------------------------------------

source(
  here(
    "Dashboard",
    "auxiliary",
    "define_labels.R"
  )
)

for (object in ls()) {
  assign(
    object,
    object %>% get %>% unname
  )
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

icons <- awesomeIcons(
  icon = 'circle-dot',
  iconColor = 'black',
  library = 'ion',
  markerColor = "#808080"
)

pal <- colorFactor(
  palette = region_colors,
  domain = map$region)

# Functions --------------------------------------------------------------------

strs_detect_any <- 
  function(x, y) {
    sapply(y, 
           str_detect, 
           string = x) %>%
      any
  }
