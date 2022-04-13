# Load packages ----------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(dplyr)
library(shinyjs)
library(sf)
library(ggplot2)
library(plotly)

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
    "data",
    "pis.rds"
  )

pi_affiliation_lab <-
  pi_data %>%
  select(affiliation) %>%
  unique

# List of learning priorities --------------------------------------------------

learning_priority <-
  list(
    "Effectiveness at scale" = 
      c(
        "1" = "Cost-effectiveness of large-scale government-led programs",
        "2" = "Nature and extent of spillovers and general equilibrium effects"
      ),
    "Scalable delivery modalities" =  c("3" = "Effects on impact and cost-effectiveness"), 
    "Dynamics over time" = c("4" = "How impacts vary over time"),
    "Bundling of Interventions" =
      c(
        "5" = "Optimal bundle/marginal contribution of constituent interventions",
        "6" = "Timing, sequencing, and intensity"
      ),
    "Targeting/heterogeneity" =
      c(
        "7" = "Cost-effectiveness across population groups",
        "8" = "Increasing cost-effectiveness for sub-groups"
      ), 
    "External validity" =
      c(
        "9" = "Adapting to urban contexts",
        "10" = "Adapting to fragile contexts"
      ),
    "Resilience and shock-responsiveness" = c("11" = "Effects on resilience and mechanisms"),
    "Other" = c("12" = "Other")
  )

# Functions --------------------------------------------------------------------

strs_detect_any <- 
  function(x, y) {
    sapply(y, 
           str_detect, 
           string = x) %>%
      any
  }
