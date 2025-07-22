# app.R
# -----------------------------------------------------------------------------
# Title:   TCWP Community Wellbeing Dashboard
# Project: UCLA–AUA Poppy Project on Forcibly Displaced Populations
# Author:  Maxwell Chien 
# Date:    2025-07-22
#
# Description:
#   This Shiny dashboard provides interactive visualizations of the
#   Transcaucasian Community Wellbeing Project (TCWP) survey data. 
#   Modules (“Panels”) cover:
#     • Pre- and post-displacement employment
#     • Health behaviors & quality of life
#     • Life events & trauma scales
#     • Population movement patterns
#     • And more…
#
#   The app reads a locally stored RDS (lightweight) version of the SPSS 
#   survey data, performs dynamic filtering, and renders value boxes, tables, 
#   plots, and maps for exploratory analysis and reporting.
#
# Prerequisites:
#   - TCWP data (translated) in rds file (not included here)
#   - All libraries below installed
#
# Usage:
#   1. Place the RDS file in the app directory
#   2. Run `shiny::runApp()` from your R console
# -----------------------------------------------------------------------------

# 0. Libraries ----------------------------------------------------------------
library(dplyr)           # for data manipulation (filter, mutate, summarize, etc.)
library(DT)              # for rendering interactive tables in Shiny
library(forcats)         # for factor (categorical variable) manipulation and reordering
library(geodata)         # for downloading and processing global administrative boundary data (GADM)
library(ggplot2)         # for creating static graphics via the Grammar of Graphics
library(leaflet)         # for building interactive maps with markers, polygons, etc.
library(plotly)          # for converting ggplot2 plots into interactive visualizations
library(purrr)           # for functional programming helpers (map, walk, reduce, etc.)
library(readr)           # for fast and friendly reading of flat files (CSV, TSV)
library(sf)              # for reading, writing, and manipulating spatial (simple feature) data
library(shiny)           # for building interactive web applications
library(shinydashboard)  # for creating dashboard layouts with Shiny
library(tidyr)           # for data tidying (pivoting, unnesting, handling missing values)

## 1. Load & Preprocess -------------------------------------------------------
df_tcwp <- readRDS("need the actual data here.rds") %>%
  
  # Rename columns to standardize nested naming format for Q23 responses
  # Original format: q23_<subq>_<member>_<field>
  # Desired format:  q23_<subq>_<field>_<member> (for easier pivoting/analysis)
  rename_with(
    ~ sub("^q23_(\\d+)_(\\d+)_(.+)$", "q23_\\1_\\3_\\2", .x),
    matches("^q23_\\d+_\\d+_.+")
  ) %>%
  
  # Add cleaner and more consistent variable names for core demographic fields
  mutate(
    household_id     = q3_household_ID,         # Unique household ID
    participant_id   = q4_participant_ID,       # Unique participant ID
    main_respondent  = MAIN_RESPONDENT,         # Flag for main household respondent
    region_name      = as_factor(q7_marz),      # Region (province) of interview
    age              = q9_b_age,                # Respondent’s age
    gender           = as_factor(q10_gender),   # Gender of respondent
    nationality      = as_factor(q11_nationality), # Nationality of respondent
    
    # Recode displacement region before Sept. 2023 into labeled factors
    region_before    = factor(
      q27_region_Artsakh_44_day,
      levels = 1:5,
      labels = c("Artsakh", "Martuni", "Askeran", "Hadrut", "Shushi")
    ),
    
    # Recode displacement region after Sept. 2023 into labeled factors
    region_after     = factor(
      q28_region_Artsakh_September,
      levels = 1:5,
      labels = c("Artsakh", "Martuni", "Askeran", "Hadrut", "Shushi")
    )
  ) %>%
  
  # ───────────────────────────────────────────────────────────
  # FIX Panel 7 raw → friendly column names
  # Rename difficult-to-read variable names to more readable labels
  # Used in housing and standard of living module (Panel 7)
  # ───────────────────────────────────────────────────────────
  rename(
    q54_rating           = q54_standart_of_living,    # Perceived standard of living
    q55_housing_type     = q55_housing_situation,     # Type of housing
    q56_rooms            = q56_rooms_in_house,        # Number of rooms
    q57_satisfaction     = q57_saisfied_house,        # Housing satisfaction
    q58_pay_flag         = q58_pay_rent_,             # Do they pay rent?
    q58_amount           = q58_yes_specify,           # Rent amount if yes
    q60_water            = q60_source_water,          # Main water source
    q62_needs_met        = q62_enough_basic_needs,    # Are basic needs met?
    q63_food_worry       = q63_worried_enough_food,   # Worried about food insecurity
    q64_future_hardship  = q64_hardship               # Anticipated hardship
  ) %>%
  
  # ───────────────────────────────────────────────────────────
  # Ensure proper numeric format for plotting histograms
  # ───────────────────────────────────────────────────────────
  mutate(
    q56_rooms  = as.numeric(q56_rooms),               # Convert room count to numeric
    q58_amount = readr::parse_number(q58_amount)      # Extract numeric value from rent (text input)
  )

# 1a. Friendly labels for Panel 2 ----------------------------------------------
mod2_friendly <- c(
  # q13_: reasons for mistrust (check-all)
  q13_reasons_mistrust            = "Mistrust Authorities",
  q13_reasons_interference        = "Fear of Interference",
  q13_reasons_eligible            = "Ineligible for Services",
  q13_reasons_loosing_opportunity = "Loss of Opportunities",
  q13_reasons_familiar            = "Prefer Familiar Areas",
  q13_reasons_time                = "Lack of Time",
  q13_reasons_having_plans        = "Already Have Plans",
  q13_reasons_other               = "Other Reason",
  # employment status
  q16_employed_before_displacement = "Employed Before Displacement",
  q17_employed_after_displacement  = "Employed After Displacement",
  q20_currently_employed           = "Currently Employed"
)

# swap names/values so the dropdown shows nice labels but returns raw names
mod2_choices <- setNames(names(mod2_friendly), mod2_friendly)


# 2. Armenia Boundaries ------------------------------------------------------
gadm_sp    <- geodata::gadm(country = "ARM", level = 1, path = ".")
regions_sf <- st_as_sf(gadm_sp)

# 3. Helper to pick module variables -----------------------------------------
pick_vars <- function(prefixes) {
  pattern <- paste0("^(", paste(prefixes, collapse = "|"), ")")
  grep(pattern, names(df_tcwp), value = TRUE)
}

module_vars <- list(
  `2` = pick_vars(c("q13_", "q16_", "q17_", "q18_", "q19_", "q20_", "q21_")),
  `3` = pick_vars(c("q22_", "q23_")),
  `4` = pick_vars(c("q27_", "q28_", "q29_")),
  `5` = pick_vars(c(
    paste0("q", 30:44, "_")
  )),
  `6` = pick_vars(c(
    "q45_",  # Why choose this community (check‐all)
    "q46_",  # Planning to leave current accommodation?
    "q47_",  # Reasons for leaving current accommodation (check‐all)
    "q48_",  # Stay in Armenia vs. move?
    "q49_",  # Reasons for planning to move (check‐all)
    "q50_",  # Destination country
    "q51_",  # Why that country (check‐all)
    "q52_",  # Missing documents?
    "q53_"   # Types of documents missing (check‐all)
  )),
  `7` = pick_vars(c(
    "q54_", "q55_", "q56_", "q57_",
    "q58_", "q59_", "q60_", "q61_",
    "q62_", "q63_", "q64_"
  )),
  `8` = pick_vars(c("q65_", "q66_", "q68_", "q69_")),
  `9` = pick_vars(c("q81_", "q82_", "q83_", "q84_", "q85_", "q86_", "q87_", "q88_", "q89_", "q90_", "q91_", "q92_", "q93_", "q94_"))
)

# ────────────────────────────────────────────────────────────────────────────
# Friendly labels for Panel 5 (single-choice metrics)
# ────────────────────────────────────────────────────────────────────────────
mod5_friendly <- c(
  # Q30: Which services did you ever need? (Yes/No)
  q30_healthcare_service        = "Needed healthcare services?",
  q30_food_assistance           = "Needed food assistance?",
  q30_childcare                 = "Needed childcare?",
  q30_education                 = "Needed education support?",
  q30_legal_counseling          = "Needed legal counseling?",
  q30_psycological_counseling   = "Needed psychological counseling?",
  q30_shelter_opportunity       = "Needed shelter opportunities?",
  q30_household_goods           = "Needed household goods?",
  q30_clothing                  = "Needed clothing?",
  q30_financial_assistance      = "Needed financial assistance?",
  # Q30 follow-ups on coverage/offering
  q30_1_services_not_covered          = "Services needed but not covered?",
  q30_1_services_not_covered_other    = "Other services not covered (specify)",
  q30_2_services_not_offered          = "Services not offered at all?",
  q30_2_services_not_offered_other    = "Other services not offered (specify)",
  
  # Q31–Q32: Social services
  q31_need_social_services      = "Needed social services?",
  q32_get_social_services       = "Received social services?",
  
  # Q34–Q35: Mental health support
  q34_need_mental_health        = "Needed mental health support?",
  q35_get_mental_health         = "Received mental health support?",
  
  # Q37–Q38: Legal support
  q37_need_legal_support        = "Needed legal support?",
  q38_get_legal                 = "Received legal support?",
  
  # Q40–Q43: Doctor / pharmacy / meds
  q40_need_doctor               = "Needed to see a doctor?",
  q41_go_doctor                 = "Went to a doctor?",
  q42_go_pharmacy               = "Went to a pharmacy instead?",
  q43_recommend_medic           = "Recommended to buy medicine?",
  q43_other_specify             = "Other medicine recommendation (specify)"
)


