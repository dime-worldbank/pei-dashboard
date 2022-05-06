area_lab <- c(
  "1" = "Rural",
  "2" = "Urban",
  "3" = "Peri-urban"
)

yesno_lab <-
  c(
    "1" = "Yes",
    "2" = "No"
  )

geo_cov_lab <- c(
  "1" = "National",
  "2" = "Several states/regions within the country",
  "3" = "Single state/region within the country"
)

pov_seg_lab <- c(
  "1" = "Poor",
  "2" = "Extreme poor",
  "3" = "Ultra-poor",
  "4" = "Other vulnerable"
)

priority_group_lab <- c(
  "1" = "Women",
  "2" = "Children",
  "3" = "Youth",
  "4" = "Elderly",
  "5" = "People with disabilities",
  "6" = "Refugees",
  "7" = "Internally displaced",
  "8" = "Ethnic minorities",
  "9" = "Other"
)

target_method_lab <- c(
  "1" = "Geographical",
  "2" = "Categorical",
  "3" = "Community-based",
  "4" = "Vulnerability scoring or index",
  "5" = "Other",
  "6" = "Not applicable"
)

target_level_lab <-
  c(
    "1" = "Individual",
    "2" = "Household",
    "3" = "Group"
  )

ie_method_lab <-
  c(
    "1" = "Randomized Controlled Trial",
    "2" = "Regression Discontinutiy Design",
    "3" = "Matching",
    "4" = "Difference-in-Difference",
    "5" = "Instrumental Variable",
    "6" = "Other"
  )

learning_priority_lab <-
  c(
    "10" = "Adapting programs to fragile contexts",
    "11" = "Effects on resilience and mechanisms",
    "12" = "Other",
    "1"  = "Cost-effectiveness of large-scale government-led programs",
    "2"  = "Spillovers and general equilibrium effects",
    "3"  = "Scalability, impact and cost-effectiveness",
    "4"  = "How impacts vary over time",
    "5"  = "Optimal bundle/marginal contribution of bundled interventions",
    "6"  = "Timing, sequencing, and intensity of bundled interventions",
    "7"  = "Cost-effectiveness across population groups",
    "8"  = "Increasing cost-effectiveness for sub-groups",
    "9"  = "Adapting programs to urban contexts"
  )

lead_lab <-
  c(
    "1" = "National/Central government",
    "2" = "Regional/District government",
    "3" = "Local (municipal) government",
    "4" = "Non-governmental organization (not microfinance)",
    "5" = "Financial service provider (private)",
    "6" = "World Bank",
    "7" = "Multilateral organization (not World Bank)",
    "8" = "Bilateral organization",
    "9" = "Other"
  )

cluster_lab <-
  c(
    "1" = "Clustered",
    "2" = "Not clustered"
  )

column_list <- 
  c(
    "Title" = "ie_name",
    "Country" = "country",
#    "Research Team" = "research_team",
    "Summary" = "ie_summary", 
#    "Research questions" = "research_questions",
    "Geographic coverage" = "geo_cov", 
    "Area" = "area", 
    "Target method" = "target_method", 
    "Poverty segment" = "pov_seg", 
    "Priority population" = "priority_group",
    "Identification strategy" = "ie_method",
    "PEI learning priority" = "learning_priority",
    "PI affiliation" = "pi_affiliation"
)
