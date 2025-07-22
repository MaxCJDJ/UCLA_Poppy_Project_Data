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

## ────────────────────────────────────────────────────────────────────────────
## Panel 6: Intentions & Perspectives — label mappings
## ────────────────────────────────────────────────────────────────────────────

mod6_choose_labels <- c(
  q45_family_friends   = "Have family or friends here",
  q45_accomodation     = "Accommodation opportunities",  #miss spelling. 
  q45_work             = "Work opportunities",
  q45_edu              = "Education opportunities",
  q45_support          = "Benefits & support available",
  q45_proximity        = "Proximity to capital city",
  q45_no_reason        = "Not planned / no reason",
  q45_no_alternative   = "No other alternative",
  q45_cost_house       = "Cost of housing",
  q45_familiar_city    = "Familiar city / been here before",
  q45_advised          = "Was advised",
  q45_other            = "Other",
  q45_prefer_no_answer = "Prefer no answer",
  q45_other_specify    = "Other (specify)"
)

mod6_reasons_leave_labels <- c(
  q47_asked_leave_accom  = "Have been asked to leave",
  q47_rental_run_out     = "Rental/grace period runs out",
  q47_free_accom_ends    = "Free accommodation ends",
  q47_cannot_pay         = "Cannot afford rent",
  q47_not_enough_spae    = "Not enough space",
  q47_poor_conditions    = "Poor conditions",
  q47_look_for_accom     = "Looking for long‐term accommodation",
  q47_plan_to_move       = "Plan to move to another city",
  q47_found_other_accom  = "Found better accommodation",
  q47_no_feel_safe       = "Don’t feel safe",
  q47_other              = "Other",
  q47_prefer_no_answer   = "Prefer no answer",
  q47_other_specify      = "Other (specify)"
)

mod6_reasons_move_labels <- c(
  q49_lack_house        = "Lack of adequate housing",
  q49_lack_support      = "Lack of support programming",
  q49_lack_edu          = "Lack of education options",
  q49_lack_access       = "Lack of basic utilities",
  q49_no_work           = "Cannot find work",
  q49_no_afford         = "Cannot afford to live here",
  q49_movein_relative   = "Moving in with relatives",
  q49_security          = "Security reasons",
  q49_discrimination    = "Discrimination / harassment",
  q49_other             = "Other",
  q49_prefer_no_asnwer  = "Prefer no answer",      
  q49_other_specify     = "Other (specify)"
)

mod6_reasons_country_labels <- c(
  q51_family_friends   = "Have family or friends there",
  q51_language         = "Language spoken",
  q51_work             = "Work opportunities",
  q51_edu              = "Education opportunities",
  q51_health           = "Specific health needs",
  q51_support          = "Benefits & support available",
  q51_accomodation     = "Accommodation opportunities",
  q51_asylum           = "Temporary protection / asylum",
  q51_cost             = "Lower cost of living",
  q51_other            = "Other",
  q51_prefer_no_answer = "Prefer no answer",
  q51_other_specify    = "Other (specify)"
)

mod6_types_docs_labels <- c(
  q53_passport          = "Passport",
  q53_military          = "Military documents",
  q53_driving           = "Driving license",
  q53_birth_certificate = "Birth certificate",
  q53_marriage_cert     = "Marriage certificate",
  q53_divorce_cert      = "Divorce certificate",
  q53_employment_proof  = "Employment proofs",
  q53_ownership_cert    = "Ownership certificates",
  q53_diplomas          = "Educational certificates",
  q53_disability_proof  = "Disability proofs",
  q53_medical_doc       = "Medical documents",
  q53_other             = "Other",
  q53_prefer_no_answer  = "Prefer no answer",
  q53_other_specify     = "Other (specify)"
)

## Panel 6 — friendly labels for each check‐all question
mod6_choose_labels <- c(
  q45_friends            = "Have family or friends here",
  q45_accommodation      = "Accommodation opportunities",
  q45_work               = "Work opportunities",
  q45_education          = "Education opportunities",
  q45_support            = "Benefits & support available",
  q45_proximity          = "Proximity to capital city",
  q45_not_planned        = "Not planned / no reason",
  q45_no_alternative     = "No other alternative",
  q45_cost               = "Cost of housing",
  q45_familiar           = "Familiar city / been here before",
  q45_advised            = "Was advised",
  q45_other              = "Other"
)

mod6_reasons_leave_labels <- c(
  q47_asked              = "Have been asked to leave",
  q47_grace              = "Rental/grace period runs out",
  q47_free_program       = "Free accommodation ends",
  q47_cannot_afford      = "Cannot afford rent",
  q47_not_enough_space   = "Not enough space",
  q47_poor_conditions    = "Poor conditions",
  q47_longterm_search    = "Looking for long-term",
  q47_move_city          = "Moving to another city",
  q47_found_better       = "Found better accommodation",
  q47_not_safe           = "Don't feel safe",
  q47_other              = "Other"
)

mod6_reasons_move_labels <- c(
  q49_housing           = "Lack of adequate housing",
  q49_support_programs  = "Lack of support programming",
  q49_education         = "Lack of education options",
  q49_utilities         = "Lack of basic utilities",
  q49_employment        = "Cannot find work",
  q49_unaffordable      = "Cannot afford to live",
  q49_with_relatives    = "Moving in with relatives",
  q49_security          = "Security reasons",
  q49_discrimination    = "Discrimination/harassment",
  q49_other             = "Other"
)

mod6_reasons_country_labels <- c(
  q51_friends       = "Have family or friends there",
  q51_language      = "Language spoken",
  q51_work          = "Work opportunities",
  q51_education     = "Education opportunities",
  q51_health        = "Specific health needs",
  q51_benefits      = "Benefits & support",
  q51_accommodation = "Accommodation opportunities",
  q51_asylum        = "Temporary protection / asylum",
  q51_cost_living   = "Lower cost of living",
  q51_other         = "Other"
)

mod6_types_docs_labels <- c(
  q53_passport        = "Passport",
  q53_military        = "Military documents",
  q53_driving_license = "Driving license",
  q53_birth_cert      = "Birth certificate",
  q53_marriage_cert   = "Marriage certificate",
  q53_divorce_cert    = "Divorce certificate",
  q53_employment      = "Employment proofs",
  q53_ownership       = "Ownership certificates",
  q53_education_cert  = "Educational certificates",
  q53_disability      = "Disability proofs",
  q53_medical         = "Medical documents",
  q53_other           = "Other"
)

mod7_friendly <- c(
  q54_rating           = "Standard of Living",
  q55_housing_type     = "Housing Situation",
  q56_rooms            = "Number of Rooms",
  q57_satisfaction     = "Housing Satisfaction",
  q58_pay_flag         = "Pay Rent/Mortgage (Yes/No)",
  q58_amount           = "Rent/Mortgage Amount (AMD)",
  q59_heating          = "Primary Heating Source",
  q60_water            = "Drinking Water Source",
  q61_expenditure      = "Monthly Expenditure Range",
  q62_needs_met        = "Income Meets Basic Needs",
  q63_food_worry       = "Worry About Food Insecurity",
  q64_future_hardship  = "Perceived Future Hardships"
)

## ────────────────────────────────────────────────────────────────────────────
## Panel 7: Socio‐Economic Status — raw→friendly mapping
## ────────────────────────────────────────────────────────────────────────────
mod7_friendly <- c(
  q54_rating           = "Standard of Living",
  q55_housing_type     = "Housing Situation",
  q56_rooms            = "Number of Rooms",
  q57_satisfaction     = "Housing Satisfaction",
  q58_pay_flag         = "Pay Rent/Mortgage (Yes/No)",
  q58_amount           = "Rent/Mortgage Amount (AMD)",
  q59_heating          = "Primary Heating Source",
  q60_water            = "Drinking Water Source",
  q61_expenditure      = "Monthly Expenditure Range",
  q62_needs_met        = "Income Meets Basic Needs",
  q63_food_worry       = "Worry About Food Insecurity",
  q64_future_hardship  = "Perceived Future Hardships"
)

## swap names/values so the dropdown shows the friendly labels
mod7_choices <- setNames(names(mod7_friendly), mod7_friendly)

## ────────────────────────────────────────────────────────────────────────────
## Panel 8: Life Events & Trauma — raw→friendly mapping
## ────────────────────────────────────────────────────────────────────────────
mod8_friendly <- c(
  # serious life events (Q68)
  q68_serious_loss                 = "Serious Loss (e.g. loved one)",
  q68_serous_injury_to_you         = "Serious Injury to You",
  q68_serous_injury_to_family      = "Serious Injury to Family",
  q68_separation                   = "Separation from Family/Community",
  q68_jail                         = "Incarceration (Self or Household)",
  q68_other                        = "Other Serious Event",
  # most frequent traumatic exposures (Q69)
  q69_natural_disaster_happened_to_me = "Natural Disaster – Experienced",
  q69_fire_happened_to_me             = "Fire – Experienced",
  q69_displacement_happened_to_me     = "Forced Displacement – Experienced",
  q69_assault_happen_to_me            = "Assault – Experienced",
  q69_violent_death_happened_to_me    = "Violent Death of Loved One",
  # overall worst trauma (Q70)
  q70_most_traumatic_event            = "Most Traumatic Event (self-reported)",
  # PTSD symptoms (Q71)
  q71_1_memories                      = "Intrusive Memories",
  q71_2_disturbing_dreams             = "Disturbing Dreams",
  q71_3_stressful_experience          = "Flashbacks / Reliving",
  q71_20_troube_falling_asleep        = "Trouble Falling Asleep",
  # PTSD thresholds
  at_least_20                         = "PTSD Symptoms ≥ 20",
  at_least_19                         = "PTSD Symptoms ≥ 19",
  at_least_18                         = "PTSD Symptoms ≥ 18",
  # functional impairment (Q72)
  q72_functioning_suffer              = "Impaired Functioning Due to Trauma"
)

## swap names/values so the dropdown shows the friendly labels
mod8_choices <- setNames(names(mod8_friendly), mod8_friendly)

## ────────────────────────────────────────────────────────────────────────────
## Panel 9: Health Behaviors, QoL & Info Sources — raw→friendly mapping
## ────────────────────────────────────────────────────────────────────────────

# Friendly labels for Panel 9 --------------------------------------------
mod9_friendly <- c(
  # Health behaviors
  q84              = "Smoking status",
  q85              = "Alcohol frequency",
  q86              = "Days drinking per week",
  q87              = "Drinks per occasion",
  q88              = "Ever binge-drank",
  q89              = "Ever used narcotic drugs",
  q90              = "Types of drugs used",
  q91              = "Household drug use",
  q92              = "Self-rated health (last 30d)",
  # QoL sub-questions (check-all matrix)
  q93              = "Problems with daily activities",
  q94              = "Pain or anxious/depressed",
  # Health information sources (frequency matrix)
  q82              = "Info sources frequency",
  q83              = "Preferred info methods"
)

mod9_choices <- setNames(names(mod9_friendly), mod9_friendly)


mod9_matrix_labels <- list(
  q82 = c(
    q82_TV                   = "TV",
    q82_friends              = "Friends/Family",
    q82_governmental         = "Governmental Sources",
    q82_health_organizations = "Health Orgs",
    q82_internet             = "Internet",
    q82_other_provider       = "Other Providers",
    q82_pharmacist           = "Pharmacist",
    q82_physician            = "Physician"
  ),
  q83 = c(
    q83_health_information   = "Health Information Sites",
    q83_Fb                   = "Facebook",
    q83_instagram            = "Instagram",
    q83_whatsup              = "WhatsApp",
    q83_viber                = "Viber",
    q83_telegram             = "Telegram",
    q83_mobile_text          = "SMS",
    q83_other                = "Other",
    q83_prefer_not_answer    = "Prefer not to answer",
    q83_other_specify        = "Other (specify)"
  ),
  q90 = c(
    q90_drugs_used           = "Types of drugs used"
  ),
  q93 = c(
    q93_1_walking            = "Walking",
    q93_2_washing            = "Washing / Dressing",
    q93_3_usual_activities   = "Usual activities",
    q93_4_pain               = "Pain interfering with activities"
  ),
  q94 = c(
    q94_5_depressed          = "Anxious / Depressed"
  )
)



