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


# 4. UI ----------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = tagList(icon("chart-bar"), "TCWP Survey Explorer")),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("1. Admin & Demographic",       tabName = "admin", 
               icon = icon("users")),
      menuItem("2. Pre-Displacement & Emp.",    tabName = "mod2",
               icon = icon("truck")),
      menuItem("3. Household Composition",      tabName = "mod3",
               icon = icon("home")),
      menuItem("4. Population Movement",        tabName = "mod4",
               icon = icon("map-marked-alt")),
      menuItem("5. Access & Utilization of Svcs", tabName = "mod5",
               icon = icon("hands-helping")),
      menuItem("6. Intentions & Perspectives",  tabName = "mod6",
               icon = icon("lightbulb")),
      menuItem("7. Socio-Economic Status",      tabName = "mod7",
               icon = icon("chart-line")),
      menuItem("8. Life Events & Trauma",       tabName = "mod8",
               icon = icon("heartbeat")),
      menuItem("9. Health Behaviors & QoL",     tabName = "mod9",
               icon = icon("stethoscope"))
    )
  ),
  
  dashboardBody(
    # need to ensure consistent valueBox height
    tags$head(tags$style(HTML(".small-box {height: 120px !important;}"))),
    
    tabItems(
      
      # Panel 1: Administrative & Demographic --------------------------------
      tabItem(tabName = "admin",
              
              ## Q3, Q4, Q6 valueBoxes only
              fluidRow(
                valueBoxOutput("vb_households",   width = 4),  # Q3
                valueBoxOutput("vb_participants", width = 4),  # Q4
                valueBoxOutput("vb_main_resp",    width = 4)   # Q6
              ),
              
              ## Q9.b / Q10 histograms
              fluidRow(
                box(
                  title = "Q9.b Age Distribution",
                  status = "primary", solidHeader = TRUE,
                  width = 6, plotlyOutput("age_dist",    height = "250px")
                ),
                box(
                  title = "Q10 Gender Breakdown",
                  status = "primary", solidHeader = TRUE,
                  width = 6, plotlyOutput("gender_bar",  height = "250px")
                )
              ),
              
              ## Q11 / Q7 map
              fluidRow(
                box(
                  title = "Q11 Nationality Breakdown",
                  status = "primary", solidHeader = TRUE,
                  width = 6, plotlyOutput("nat_bar",    height = "250px")
                ),
                box(
                  title = "Q7 Current Marz (map)",
                  status = "primary", solidHeader = TRUE,
                  width = 6, leafletOutput("map1",      height = "250px")
                )
              )
              
      ),
      
      # Panel 2: Pre-Displacement & Employment --------------------------------
      tabItem(
        tabName = "mod2",
        
        # 2a) snapshot valueBoxes
        fluidRow(
          valueBoxOutput("vb_emp_before", width=4),
          valueBoxOutput("vb_emp_after",  width=4),
          valueBoxOutput("vb_emp_current",width=4)
        ),
        
        # 2b) employment status over time + occupation details
        fluidRow(
          box(
            title = "Employment % by Stage",
            status = "primary", solidHeader = TRUE, width = 6,
            plotlyOutput("emp_status_plot", height = "300px")
          ),
          box(
            title = "Occupations (Pre-Displacement)",
            status = "primary", solidHeader = TRUE, width = 6,
            DTOutput("emp_occ_tbl")
          )
        ),
        
        # 2c) aggregated reasons for mistrust
        fluidRow(
          box(
            title = "Reasons for Distrust (all check-all)",
            status = "warning", solidHeader = TRUE, width = 12,
            plotlyOutput("mistrust_reasons", height = "350px")
          )
        ),
        
        fluidRow(
          box(
            title = "Barriers to Finding Work (Q21)",
            status = "danger", solidHeader = TRUE, width = 12,
            plotlyOutput("barriers_work", height = "300px")
          )
        ),
        
        # 2d) generic selector → table + plot
        fluidRow(
          box(
            title = "Explore any metric",
            status = "success", solidHeader = TRUE, width = 4,
            selectInput(
              "var2", "Metric:", 
              choices  = mod2_choices,    
              selected = mod2_choices[[1]]    
            )
          ),
          box(
            title = "Count Table",
            status = "info", solidHeader = TRUE, width = 8,
            DTOutput("tbl2")
          )
        ),
        fluidRow(
          box(
            title = "Distribution Plot",
            status = "info", solidHeader = TRUE, width = 12,
            plotOutput("plt2", height = "350px")
          )
        )
      ),
      
      ## Panel 3: Household Composition --------------------------------
      
      tabItem(
        tabName = "mod3",
        
        ## A) Household size
        fluidRow(
          box(
            title = "Household Size", status = "warning", solidHeader = TRUE,
            width = 4,
            selectInput(
              "hh_size_var", "Which size?",
              # take the first two module_vars[["3"]] entries (Q22 & Q23 totals)
              choices = setNames(
                module_vars[["3"]][1:2],
                c("Current household size (Q22)", "Before displacement (Q23)")
              ),
              selected = module_vars[["3"]][1]
            )
          ),
          box(
            title = "Size distribution", status = "info", solidHeader = TRUE,
            width = 8,
            plotlyOutput("hh_size_plot", height = "300px")
          )
        ),
        
        ## B) Member‐level characteristics
        fluidRow(
          box(
            title = "Member characteristic", status = "warning",
            solidHeader = TRUE, width = 4,
            selectInput(
              "member_var", "Pick a question:",
              choices = c(
                "Year of birth (Q23.2)"             = "year_birth",
                "Gender (Q23.3)"                    = "gender",
                "Relation to respondent (Q23.4)"    = "relation_respondent",
                "Current status (alive/dead) (Q23.5)" = "current_health_status",
                "Lives with respondent? (Q23.6)"    = "living_with_respond",
                "Employment status (Q23.7)"         = "employment"
              ),
              selected = "gender"
            )
          ),
          box(
            title = textOutput("member_title"), status = "info",
            solidHeader = TRUE, width = 8,
            plotlyOutput("member_plot", height = "300px")
          )
        )
      ),
      
      ## Panel 4: Population Movement — UI --------------------------------

      tabItem(
        tabName = "mod4",
        
        ## A) Region before / after
        fluidRow(
          box(
            title = "Region before the 44-day war", status = "primary",
            solidHeader = TRUE,
            width = 6, plotlyOutput("mod4_before_plot", height = "300px")
          ),
          box(
            title = "Region moved from (Sep 2023)", status = "primary",
            solidHeader = TRUE,
            width = 6, plotlyOutput("mod4_after_plot",  height = "300px")
          )
        ),
        
        ## B) # moves + stats
        fluidRow(
          box(
            title = "# of moves since Sep 19, 2023", status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("mod4_nmoves_hist",   height = "300px"),
            verbatimTextOutput("mod4_nmoves_stats")
          ),
        )
      ),
      
      # Panel 5: Access & Utilization of Services ----------------------------
      tabItem(
        tabName = "mod5",
        fluidRow(
          box(
            title = "Single‐Choice Metrics",
            status = "warning", solidHeader = TRUE, width = 4,
            selectInput(
              "var5", "Variable:", 
              choices  = setNames(names(mod5_friendly), mod5_friendly),
              selected = names(mod5_friendly)[1]
            )
          ),
          box(
            title = "Count Table",
            status = "info", solidHeader = TRUE, width = 8,
            DTOutput("tbl5")
          )
        ),
        fluidRow(
          box(
            title = "Distribution Plot",
            status = "info", solidHeader = TRUE, width = 12,
            plotlyOutput("plt5", height = "350px")
          )
        ),
        # now I want to dedicate one row for each reasons‐type question
        fluidRow(
          box(
            title = "Reasons for Not Getting Social Services (Q33)",
            status = "danger", solidHeader = TRUE, width = 6,
            plotlyOutput("reasons_social")
          ),
          box(
            title = "Reasons for Not Getting Mental Health Support (Q36)",
            status = "danger", solidHeader = TRUE, width = 6,
            plotlyOutput("reasons_mental")
          )
        ),
        fluidRow(
          box(
            title = "Reasons for Not Getting Legal Support (Q39)",
            status = "danger", solidHeader = TRUE, width = 6,
            plotlyOutput("reasons_legal")
          ),
          box(
            title = "Reasons for Not Going to a Doctor (Q44)",
            status = "danger", solidHeader = TRUE, width = 6,
            plotlyOutput("reasons_doctor")
          )
        )
      ),
      
      # Panel 6: Intentions & Perspectives -----------------------------------
      tabItem(tabName = "mod6",
              
              ## Q45: Why choose this community?
              fluidRow(
                box(
                  title = "Why did you/your family choose this community?",
                  status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("mod6_choose_comm", height = "300px")
                ),
                
                ## Q46: Planning to leave current accommodation?
                box(
                  title = "Are you planning to leave your current accommodation?",
                  status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("mod6_plan_leave", height = "300px")
                )
              ),
              
              ## Q47: Reasons for planning to leave
              fluidRow(
                box(
                  title = "If so, why are you planning to leave?",
                  status = "warning", solidHeader = TRUE, width = 12,
                  plotlyOutput("mod6_reasons_leave", height = "300px")
                )
              ),
              
              ## Q48 + follow-up Q48.1
              fluidRow(
                box(
                  title = "Within the next 6 months, stay in Armenia or move?",
                  status = "info", solidHeader = TRUE, width = 6,
                  plotlyOutput("mod6_stay_vs_move", height = "300px")
                ),
                box(
                  title = "If staying, which Marz/City?",
                  status = "info", solidHeader = TRUE, width = 6,
                  plotlyOutput("mod6_stay_location", height = "300px")
                )
              ),
              
              ## Q49: Reasons for moving
              fluidRow(
                box(
                  title = "Why are you planning to move somewhere else?",
                  status = "danger", solidHeader = TRUE, width = 12,
                  plotlyOutput("mod6_reasons_move", height = "300px")
                )
              ),
              
              ## Q50 + Q51
              fluidRow(
                box(
                  title = "Which country are you planning to go to?",
                  status = "success", solidHeader = TRUE, width = 6,
                  plotlyOutput("mod6_dest_country", height = "300px")
                ),
                box(
                  title = "Why that country?",
                  status = "success", solidHeader = TRUE, width = 6,
                  plotlyOutput("mod6_reasons_country", height = "300px")
                )
              ),
              
              ## Q52 + Q53 (fixed statuses)
              fluidRow(
                box(
                  title = "Are you missing official documentation?",
                  status = "info",         
                  solidHeader = TRUE, width = 6,
                  plotlyOutput("mod6_missing_docs", height = "300px")
                ),
                box(
                  title = "What type of documents are you missing?",
                  status = "info",        
                  solidHeader = TRUE, width = 6,
                  plotlyOutput("mod6_types_docs", height = "300px")
                )
              )
              
      ),
      
      # Panel 7: Socio-Economic Status ---------------------------------------
      tabItem(tabName = "mod7",
              fluidRow(
                box(
                  title = "Controls", status = "warning", solidHeader = TRUE,
                  width = 3,
                  selectInput(
                    "var7", "Metric:",
                    choices  = mod7_choices,
                    selected = names(mod7_choices)[1]
                  )
                ),
                box(
                  title = "Table", status = "info", solidHeader = TRUE,
                  width = 9,
                  DTOutput("tbl7")
                )
              ),
              fluidRow(
                box(
                  title = "Distribution Plot", status = "info",
                  solidHeader = TRUE, width = 12,
                  plotOutput("plt7", height = "350px")
                )
              )
      ),
      
      # Panel 8: Life Events & Trauma Scales ---------------------------------
      tabItem(tabName = "mod8",
              fluidRow(
                box(title="Controls", status="warning", solidHeader=TRUE,
                    width=3,
                    selectInput("var8", "Variable:", 
                                choices = mod8_choices, 
                                selected = names(mod8_choices)[1])
                    
                ),
                box(title="Table", status="info", solidHeader=TRUE, width=9,
                    DTOutput("tbl8")
                )
              ),
              fluidRow(
                box(title="Distribution Plot", status="info", solidHeader=TRUE,
                    width=12,
                    plotOutput("plt8", height="350px")
                )
              )
      ),
      
      # Panel 9: Health Behaviors & Quality of Life --------------------------
      tabItem(
        tabName = "mod9",
        fluidRow(
          box(
            title = "Select question", status = "warning", solidHeader = TRUE,
            width = 3,
            selectInput(
              "var9", "Question:",
              choices  = mod9_choices,
              selected = names(mod9_choices)[1]
            )
          ),
          box(
            title = "Count table", status = "info", solidHeader = TRUE,
            width = 9,
            DTOutput("tbl9")
          )
        ),
        fluidRow(
          box(
            title = "Distribution / Pivot plot", status = "info",
            solidHeader = TRUE, width = 12,
            plotOutput("plt9", height = "350px")
          )
        )
      )
    )
  )
)


# 5. Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  ## Panel 1: Administrative & Demographic -----------------------------------
  
  # Q3: # of households
  output$vb_households <- renderValueBox({
    valueBox(
      scales::comma(n_distinct(df_tcwp$q3_household_ID)),
      "Households (Q3)", icon = icon("home"), color = "teal"
    )
  })
  
  # Q4: # of participants
  output$vb_participants <- renderValueBox({
    valueBox(
      scales::comma(n_distinct(df_tcwp$q4_participant_ID)),
      "Participants (Q4)", icon = icon("users"), color = "green"
    )
  })
  
  # Q6: % main respondent = Yes
  output$vb_main_resp <- renderValueBox({
    pct <- mean(df_tcwp$MAIN_RESPONDENT == "Yes", na.rm = TRUE)
    valueBox(
      scales::percent(pct, accuracy = 0.1),
      "Main Respondent (Q6 = Yes)", icon = icon("check-circle"),
      color = "purple"
    )
  })
  
  # Q9.b: Age distribution
  output$age_dist <- renderPlotly({
    plot_ly(df_tcwp, x = ~q9_b_age, type = "histogram", nbinsx = 15) %>%
      layout(
        title = "Q9.b Age (years)",
        xaxis = list(title = "Age"),
        yaxis = list(title = "Count")
      )
  })
  
  # Q10: Gender breakdown
  output$gender_bar <- renderPlotly({
    df_tcwp %>%
      count(q10_gender) %>%
      filter(!is.na(q10_gender)) %>%
      plot_ly(x = ~q10_gender, y = ~n, type = "bar") %>%
      layout(
        title = "Q10 Gender",
        xaxis = list(title = ""),
        yaxis = list(title = "Count")
      )
  })
  
  # Q11: Nationality breakdown
  output$nat_bar <- renderPlotly({
    df_tcwp %>%
      count(q11_nationality) %>%
      filter(!is.na(q11_nationality)) %>%
      arrange(n) %>%
      mutate(nationality = factor(q11_nationality, levels = q11_nationality)) %>%
      plot_ly(x = ~n, y = ~nationality, type = "bar", orientation = "h") %>%
      layout(
        title = "Q11 Nationality",
        xaxis = list(title = "Count"),
        yaxis = list(title = "")
      )
  })
  
  # Q7: Marz map
  output$map1 <- renderLeaflet({
    rc <- df_tcwp %>%
      filter(!is.na(q7_marz)) %>%
      count(q7_marz, name = "n")
    mdf <- regions_sf %>%
      left_join(rc, by = c("NAME_1" = "q7_marz")) %>%
      mutate(n = replace_na(n, 0))
    pal <- colorBin("YlOrRd", domain = mdf$n, bins = 5)
    leaflet(mdf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(n), fillOpacity = 0.7, color = "#444", weight = 1,
        label = ~paste0(NAME_1, ": ", n),
        highlightOptions = highlightOptions(weight = 2, color = "#000")
      ) %>%
      addLegend(pal = pal, values = ~n, title = "Respondents")
  })

  ## Panel 2: Pre‐Displacement & Employment ----------------------------------
  
  # grab all occupation columns (everything q16_ *except* the before‐flag)
  occ_cols <- grep("^q16_", names(df_tcwp), value = TRUE)
  occ_cols <- setdiff(occ_cols, "q16_employed_before_displacement")
  
  # 2a) snapshot valueBoxes (2 decimals)
  output$vb_emp_before <- renderValueBox({
    pct <- mean(df_tcwp$q16_employed_before_displacement == "Yes", na.rm = TRUE)
    valueBox(
      value    = sprintf("%.2f%%", pct * 100),
      subtitle = "Employed Before Displacement",
      icon     = icon("briefcase"),
      color    = "blue"
    )
  })
  output$vb_emp_after <- renderValueBox({
    pct <- mean(df_tcwp$q17_employed_after_displacement == "Yes", na.rm = TRUE)
    valueBox(
      value    = sprintf("%.2f%%", pct * 100),
      subtitle = "Employed After Displacement",
      icon     = icon("globe"),
      color    = "green"
    )
  })
  output$vb_emp_current <- renderValueBox({
    pct <- mean(df_tcwp$q20_currently_employed == "Yes", na.rm = TRUE)
    valueBox(
      value    = sprintf("%.2f%%", pct * 100),
      subtitle = "Currently Employed",
      icon     = icon("user-tie"),
      color    = "purple"
    )
  })
  
  # 2b) Employment % by Stage (two‐decimal % on the y‐axis)
  output$emp_status_plot <- renderPlotly({
    df_pct <- tibble(
      Stage   = c("Before", "After", "Current"),
      Percent = c(
        mean(df_tcwp$q16_employed_before_displacement == "Yes", na.rm = TRUE),
        mean(df_tcwp$q17_employed_after_displacement  == "Yes", na.rm = TRUE),
        mean(df_tcwp$q20_currently_employed          == "Yes", na.rm = TRUE)
      )
    )
    plot_ly(df_pct, x = ~Stage, y = ~Percent, type = "bar") %>%
      layout(
        yaxis = list(title = "Proportion", tickformat = ".2%"),
        xaxis = list(title = "")
      )
  })
  
  # 2b) Occupations (Pre‐Displacement) — pivots **any** q16_ field
  output$emp_occ_tbl <- renderDT({
    df_tcwp %>%
      filter(q16_employed_before_displacement == "Yes") %>%
      select(all_of(occ_cols)) %>%
      pivot_longer(everything(), names_to = "code", values_to = "occupation") %>%
      filter(!is.na(occupation)) %>%
      count(occupation, name = "Count") %>%
      arrange(desc(Count)) %>%
      datatable(options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  # 2c) Reasons for Distrust (check‐all → stacked counts)
  output$mistrust_reasons <- renderPlotly({
    df_tcwp %>%
      select(starts_with("q13_reasons_")) %>%
      pivot_longer(everything(), names_to = "reason", values_to = "sel") %>%
      filter(sel == "Yes") %>%
      count(reason) %>%
      mutate(label = mod2_friendly[reason]) %>%
      plot_ly(x = ~label, y = ~n, type = "bar") %>%
      layout(
        xaxis = list(title = "", tickangle = -45),
        yaxis = list(title = "Count")
      )
  })
  
  # 2d) “Explore any metric”
  output$tbl2 <- renderDT({
    req(input$var2)
    var <- input$var2
    cnt <- df_tcwp %>%
      filter(!is.na(.data[[var]])) %>%
      count(Value = .data[[var]], name = "Count") %>%
      arrange(desc(Count))
    
    datatable(cnt, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  output$plt2 <- renderPlot({
    req(input$var2)
    var   <- input$var2
    label <- mod2_friendly[[var]]
    
    cnt <- df_tcwp %>%
      filter(!is.na(.data[[var]])) %>%
      count(Value = .data[[var]], name = "n")
    
    ggplot(cnt, aes(x = Value, y = n)) +
      geom_col(fill = "#2C3E50") +
      labs(
        x     = label,
        y     = "Count",
        title = if (startsWith(var, "q13_"))
          paste0(label, " (check‐all that apply)")
        else
          label
      ) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$barriers_work <- renderPlotly({
    df_tcwp %>%
      select(starts_with("q21_")) %>%
      pivot_longer(everything(), names_to="barrier", values_to="sel") %>%
      filter(sel == "Yes") %>%
      count(barrier) %>%
      mutate(label = recode(barrier,
                            q21_documentation             = "Lack of Documentation",
                            q21_employemnt_opportunities  = "No Decent Jobs",
                            q21_opportunities_suited_to_me= "Not Suited To My Skills",
                            q21_opportunities_someone     = "Jobs for My Age",
                            q21_access                    = "No Childcare Access",
                            q21_education                 = "Education Not Recognized",
                            q21_information               = "Lack of Info, How to Apply",
                            q21_not_planning_stay         = "Not Planning To Stay",
                            q21_discrimination            = "Discrimination",
                            q21_other                     = "Other"
      )) %>%
      arrange(desc(n)) %>%
      plot_ly(x=~n, y=~reorder(label, n), type="bar", orientation="h") %>%
      layout(xaxis=list(title="Count"), yaxis=list(title="Barrier"))
  })
  
  ## Panel 3: Household Composition ------------------------------------------
  
  # A) Size distribution
  output$hh_size_plot <- renderPlotly({
    req(input$hh_size_var)
    var <- input$hh_size_var
    df_tcwp %>%
      filter(!is.na(!!sym(var))) %>%
      count(size = !!sym(var)) %>%
      plot_ly(x = ~size, y = ~n, type = "bar") %>%
      layout(
        xaxis = list(title = "Household size"),
        yaxis = list(title = "Count")
      )
  })
  
  # B) Member‐level pivot + plot
  members_long <- reactive({
    df_tcwp %>%
      # grab _all_ q23_*_*_* columns
      select(matches("^q23_")) %>%
      mutate(across(everything(), as.character)) %>%
      # pivot into long, capturing:
      #   * question (e.g. "living_with_respond")
      #   * member_index (the final “_1", "_2", …)
      pivot_longer(
        cols        = matches("^q23_\\d+_.+_\\d+$"),
        names_to    = c("q_num", "question", "member_index"),
        names_pattern = "q23_(\\d+)_(.+)_(\\d+)$",
        values_to   = "value"
      ) %>%
      filter(!is.na(value) & value != "")
  })
  
  # friendly labels for the sub‐questions
  member_labels <- c(
    year_birth              = "Year of birth",
    gender                  = "Gender",
    relation_respondent     = "Relation to respondent",
    current_health_status   = "Current status (alive/dead)",
    living_with_respond     = "Lives with respondent?",
    employment              = "Employment status"
  )
  
  output$member_title <- renderText({
    member_labels[[ input$member_var ]]
  })
  
  output$member_plot <- renderPlotly({
    req(input$member_var)
    members_long() %>%
      filter(question == input$member_var) %>%
      count(answer = value) %>%
      plot_ly(x = ~answer, y = ~n, type = "bar") %>%
      layout(
        xaxis = list(title = member_labels[[input$member_var]]),
        yaxis = list(title = "Count")
      )
  })

  
  ## Panel 4: Population Movement — SERVER ------------------------------------
  
  ## A) Region before the war
  output$mod4_before_plot <- renderPlotly({
    df_tcwp %>%
      filter(!is.na(q27_region_Artsakh_44_day)) %>%
      mutate(region_before = as_factor(q27_region_Artsakh_44_day)) %>%
      count(region_before) %>%
      plot_ly(x = ~region_before, y = ~n, type = "bar") %>%
      layout(
        xaxis = list(title = "Region before war", tickangle = -45),
        yaxis = list(title = "# households"),
        margin = list(b = 100)
      )
  })
  
  ## B) Region moved from (Sep 2023)
  output$mod4_after_plot <- renderPlotly({
    df_tcwp %>%
      filter(!is.na(q28_region_Artsakh_September)) %>%
      mutate(region_after = as_factor(q28_region_Artsakh_September)) %>%
      count(region_after) %>%
      plot_ly(x = ~region_after, y = ~n, type = "bar") %>%
      layout(
        xaxis = list(title = "Region moved from (Sep 2023)", tickangle = -45),
        yaxis = list(title = "# households"),
        margin = list(b = 100)
      )
  })
  
  
  # C) # moves histogram + stats
  output$mod4_nmoves_hist <- renderPlotly({
    df_tcwp %>%
      filter(!is.na(q29_change_living_frquesncy)) %>%
      plot_ly(x = ~q29_change_living_frquesncy, type = "histogram") %>%
      layout(xaxis = list(title = "# moves"), yaxis = list(title = "Count"))
  })
  output$mod4_nmoves_stats <- renderText({
    stats <- df_tcwp %>%
      summarise(
        mean   = mean(q29_change_living_frquesncy, na.rm = TRUE),
        median = median(q29_change_living_frquesncy, na.rm = TRUE)
      )
    sprintf("Mean = %.2f, median = %d", stats$mean, stats$median)
  })
  

  ## Panel 5: Access & Utilization of Services — SERVER -----------------------
  
  ## Q33: Reasons for not getting social services
  output$reasons_social <- renderPlotly({
    df_tcwp %>%
      select(starts_with("q33_")) %>%
      pivot_longer(everything(), names_to = "code", values_to = "sel") %>%
      filter(sel == "Yes") %>%
      count(code) %>%
      mutate(label = recode(code,
                            q33_lack_money              = "Lack of money/too expensive",
                            q33_lack_transportation     = "Lack of transportation",
                            q33_lack_time               = "Lack of time",
                            q33_lack_documentation      = "Lack of documentation",
                            q33_lack_services           = "No available services",
                            q33_donot_trust_services    = "Didn’t trust services",
                            q33_prefer_not_to_answer    = "Prefer not to answer",
                            q33_other                   = "Other"
      )) %>%
      plot_ly(x = ~n, y = ~reorder(label, n),
              type = "bar", orientation = "h") %>%
      layout(xaxis = list(title = "Count"),
             yaxis = list(title = "Reason"))
  })
  
  ## Q36: Reasons for not getting mental health support
  output$reasons_mental <- renderPlotly({
    df_tcwp %>%
      select(starts_with("q36_")) %>%
      pivot_longer(everything(), names_to = "code", values_to = "sel") %>%
      filter(sel == "Yes") %>%
      count(code) %>%
      mutate(label = recode(code,
                            q36_lack_money            = "Lack of money/too expensive",
                            q36_lack_transport        = "Lack of transportation",
                            q36_lack_time             = "Lack of time",
                            q36_lack_specialist       = "Lack of specialists",
                            q36_no_believe_effect     = "Didn’t believe in effectiveness",
                            q36_fear_diagnosis        = "Fear of diagnosis/treatment",
                            q36_no_trust              = "Didn’t trust specialists",
                            q36_prefer_selcare        = "Preferred self‐care",
                            q36_prefer_not_answer     = "Prefer not to answer",
                            q36_other                 = "Other"
      )) %>%
      plot_ly(x = ~n, y = ~reorder(label, n),
              type = "bar", orientation = "h") %>%
      layout(xaxis = list(title = "Count"),
             yaxis = list(title = "Reason"))
  })
  
  ## Q39: Reasons for not getting legal support
  output$reasons_legal <- renderPlotly({
    df_tcwp %>%
      select(starts_with("q39_")) %>%
      pivot_longer(everything(), names_to = "code", values_to = "sel") %>%
      filter(sel == "Yes") %>%
      count(code) %>%
      mutate(label = recode(code,
                            q39_lack_money            = "Lack of money/too expensive",
                            q39_lack_transport        = "Lack of transportation",
                            q39_lack_time             = "Lack of time",
                            q39_lack_specialist       = "Lack of specialists",
                            q39_no_trust_specilaist   = "Didn’t trust specialists",
                            q39_prefer_not_answer     = "Prefer not to answer",
                            q39_other                 = "Other"
      )) %>%
      plot_ly(x = ~n, y = ~reorder(label, n),
              type = "bar", orientation = "h") %>%
      layout(xaxis = list(title = "Count"),
             yaxis = list(title = "Reason"))
  })
  
  ## Q44: Reasons for not going to a doctor
  output$reasons_doctor <- renderPlotly({
    df_tcwp %>%
      select(starts_with("q44_")) %>%
      pivot_longer(everything(), names_to = "code", values_to = "sel") %>%
      filter(sel == "Yes") %>%
      count(code) %>%
      mutate(label = recode(code,
                            q44_lack_money          = "Lack of money/too expensive",
                            q44_lack_transport       = "Lack of transportation",
                            q44_lack_time            = "Lack of time",
                            q44_fear_diagnosis       = "Fear of diagnosis/treatment",
                            q44_no_trust             = "Didn’t trust providers",
                            q44_self_treatment       = "Preferred self‐treatment",
                            q44_prefer_not_answer    = "Prefer not to answer",
                            q44_other                = "Other"
      )) %>%
      plot_ly(x = ~n, y = ~reorder(label, n),
              type = "bar", orientation = "h") %>%
      layout(xaxis = list(title = "Count"),
             yaxis = list(title = "Reason"))
  })
  
  # Single‐choice count table
  output$tbl5 <- renderDT({
    req(input$var5)
    df_tcwp %>%
      filter(!is.na(.data[[input$var5]])) %>%
      count(answer = .data[[input$var5]], name = "Count") %>%
      arrange(desc(Count)) %>%
      datatable(options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # Single‐choice bar chart
  output$plt5 <- renderPlotly({
    req(input$var5)
    var   <- input$var5
    label <- mod5_friendly[[var]]
    
    cnt <- df_tcwp %>%
      filter(!is.na(.data[[var]])) %>%
      count(answer = .data[[var]], name = "n")
    
    plot_ly(cnt, x = ~answer, y = ~n, type = "bar") %>%
      layout(
        title  = label,
        xaxis  = list(title = label, tickangle = -45),
        yaxis  = list(title = "Count"),
        margin = list(b = 100)
      )
  })

  
  ## Panel 6: Intentions & Perspectives — server adjustments ------------------
  
  # Q45: Why did you choose this community?
  output$mod6_choose_comm <- renderPlotly({
    df_tcwp %>%
      select(any_of(names(mod6_choose_labels))) %>%
      pivot_longer(everything(), names_to="code", values_to="sel") %>%
      filter(sel == "Yes") %>%
      count(code) %>%
      mutate(label = mod6_choose_labels[code]) %>%
      plot_ly(x=~label, y=~n, type="bar") %>%
      layout(
        xaxis = list(title="", tickangle = -45),
        yaxis = list(title="Count")
      )
  })
  
  # Q46: are you planning to leave?
  output$mod6_plan_leave <- renderPlotly({
    df_tcwp %>%
      filter(!is.na(q46_plan_leave_accom)) %>%
      count(q46_plan_leave_accom, name = "n") %>%
      plot_ly(x = ~q46_plan_leave_accom, y = ~n, type = "bar") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Count"))
  })
  
  # Q47: Reasons for planning to leave
  output$mod6_reasons_leave <- renderPlotly({
    df_tcwp %>%
      select(any_of(names(mod6_reasons_leave_labels))) %>%
      pivot_longer(everything(), names_to="code", values_to="sel") %>%
      filter(sel == "Yes") %>%
      count(code) %>%
      mutate(label = mod6_reasons_leave_labels[code]) %>%
      plot_ly(x=~label, y=~n, type="bar") %>%
      layout(xaxis = list(title="", tickangle = -45),
             yaxis = list(title="Count"))
  })
  
  # Q48: stay vs. move
  output$mod6_stay_vs_move <- renderPlotly({
    df_tcwp %>%
      filter(!is.na(q48_plan_to_stay)) %>%
      count(q48_plan_to_stay, name = "n") %>%
      plot_ly(x = ~q48_plan_to_stay, y = ~n, type = "bar") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Count"))
  })
  
  # Q48.1: if staying, which Marz/City?
  output$mod6_stay_location <- renderPlotly({
    df_tcwp %>%
      filter(q48_plan_to_stay == "Stay in Armenia") %>%
      count(q48_1_which_marz_city, name = "n") %>%
      plot_ly(x = ~q48_1_which_marz_city, y = ~n, type = "bar") %>%
      layout(xaxis = list(title = "Marz/City"), yaxis = list(title = "Count"))
  })
  
  # Q49: Reasons for moving elsewhere
  output$mod6_reasons_move <- renderPlotly({
    df_tcwp %>%
      select(any_of(names(mod6_reasons_move_labels))) %>%
      pivot_longer(everything(), names_to="code", values_to="sel") %>%
      filter(sel == "Yes") %>%
      count(code) %>%
      mutate(label = mod6_reasons_move_labels[code]) %>%
      plot_ly(x=~label, y=~n, type="bar") %>%
      layout(xaxis = list(title="", tickangle = -45),
             yaxis = list(title="Count"))
  })
  
  # Q50: which country?
  output$mod6_dest_country <- renderPlotly({
    df_tcwp %>%
      filter(!is.na(q50_country_go)) %>%
      count(q50_country_go, name = "n") %>%
      plot_ly(x = ~q50_country_go, y = ~n, type = "bar") %>%
      layout(xaxis = list(title = "Country"), yaxis = list(title = "Count"))
  })
  
  # Q51: Why that country?
  output$mod6_reasons_country <- renderPlotly({
    df_tcwp %>%
      select(any_of(names(mod6_reasons_country_labels))) %>%
      pivot_longer(everything(), names_to="code", values_to="sel") %>%
      filter(sel == "Yes") %>%
      count(code) %>%
      mutate(label = mod6_reasons_country_labels[code]) %>%
      plot_ly(x=~label, y=~n, type="bar") %>%
      layout(xaxis = list(title="", tickangle = -45),
             yaxis = list(title="Count"))
  })
  
  # Q52: missing docs?
  output$mod6_missing_docs <- renderPlotly({
    df_tcwp %>%
      filter(!is.na(q52_missing_doc)) %>%
      count(q52_missing_doc, name = "n") %>%
      plot_ly(x = ~q52_missing_doc, y = ~n, type = "bar") %>%
      layout(xaxis = list(title = "Missing documentation?"),
             yaxis = list(title = "Count"))
  })
  
  # Q53: What type of documents are you missing?
  output$mod6_types_docs <- renderPlotly({
    df_tcwp %>%
      select(any_of(names(mod6_types_docs_labels))) %>%
      pivot_longer(everything(), names_to="code", values_to="sel") %>%
      filter(sel == "Yes") %>%
      count(code) %>%
      mutate(label = mod6_types_docs_labels[code]) %>%
      plot_ly(x=~label, y=~n, type="bar") %>%
      layout(xaxis = list(title="", tickangle = -45),
             yaxis = list(title="Count"))
  })
  
  