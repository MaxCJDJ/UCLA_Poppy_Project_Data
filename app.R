# app.R
# -----------------------------------------------------------------------------
# Title:   TCWP Community Wellbeing Dashboard
# Project: UCLA–AUA Poppy Project on Forcibly Displaced Populations
# Author:  Maxwell Chien 
# Date:    2025-07-22
# Date: Updates on 2025-09-01
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

# Make sure everything we serialize to the browser is UTF-8
options(encoding = "UTF-8")
try(stringi::stri_enc_set("UTF-8"), silent = TRUE)
if (.Platform$OS.type == "windows") {
  suppressWarnings(try(Sys.setlocale("LC_CTYPE", "English_United States.utf8"),
                       silent = TRUE))
  suppressWarnings(try(Sys.setlocale("LC_ALL",   "English_United States.utf8"), 
                       silent = TRUE))
}

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
library(stringr)         # strings
library(stringi)         # encoding
library(tidyr)           # for data tidying (pivoting, unnesting, handling missing values)
library(tibble)          # tibble()

arm_font <- "Noto Sans Armenian"  # used where Armenian text may appear

# 0a. Tiny utility ------------------------------------------------------------
`%||%` <- function(x, y) if (!is.null(x)) x else y

# Try to fix UTF-8 text that was mis-read as latin1/Windows-1252
fix_arm_encoding <- function(x) {
  x <- as.character(x)
  has_arm <- function(v) {
    v <- as.character(v); v[is.na(v)] <- ""
    nzchar(v) & stringr::str_detect(v, "[\\u0530-\\u058F]")
  }
  ok <- has_arm(x)
  try1252  <- suppressWarnings(iconv(x, from = "windows-1252", to = "UTF-8"))
  use1252  <- (!ok) & has_arm(try1252)
  tryl1    <- suppressWarnings(iconv(x, from = "latin1", to = "UTF-8"))
  usel1    <- (!ok) & (!use1252) & has_arm(tryl1)
  trydouble <- suppressWarnings(iconv(
    iconv(x, from = "UTF-8", to = "windows-1252", sub = "byte"),
    from = "windows-1252", to = "UTF-8"
  ))
  usedouble <- (!ok) & (!use1252) & (!usel1) & has_arm(trydouble)
  tryarm   <- suppressWarnings(iconv(x, from = "ARMSCII-8", to = "UTF-8"))
  usearm   <- (!ok) & (!use1252) & (!usel1) & (!usedouble) & has_arm(tryarm)
  out <- x
  i <- which(use1252);   if (length(i)) out[i] <- try1252[i]
  i <- which(usel1);     if (length(i)) out[i] <- tryl1[i]
  i <- which(usedouble); if (length(i)) out[i] <- trydouble[i]
  i <- which(usearm);    if (length(i)) out[i] <- tryarm[i]
  leftover <- !(ok | use1252 | usel1 | usedouble | usearm)
  i <- which(leftover)
  if (length(i)) out[i] <- stringi::stri_enc_toutf8(out[i], 
                                                    is_unknown_8bit = TRUE)
  Encoding(out) <- "UTF-8"
  out
}

# Reinterpret Latin-1/CP1252 mojibake as UTF-8 (e.g., "Ô±Ö€..." → Armenian)
fix_utf8_mojibake <- function(x) {
  x <- as.character(x)
  is_arm <- stringr::str_detect(x, "[\\u0530-\\u058F]")
  looks_8bit <- stringr::str_detect(x, "[\\u00A0-\\u00FF]")
  y <- x
  bad <- !is_arm & looks_8bit & !is.na(x)
  if (any(bad)) {
    Encoding(y[bad]) <- "latin1"
    y[bad] <- enc2utf8(y[bad])
  }
  stringi::stri_enc_toutf8(y, is_unknown_8bit = TRUE)
}

# One robust Armenian text normalizer
repair_arm <- function(x) {
  x <- as.character(x)
  a1 <- fix_utf8_mojibake(x); a2 <- fix_arm_encoding(a1)
  a2 <- stringi::stri_enc_toutf8(a2, is_unknown_8bit = TRUE)
  b1 <- fix_arm_encoding(x);   b2 <- fix_utf8_mojibake(b1)
  b2 <- stringi::stri_enc_toutf8(b2, is_unknown_8bit = TRUE)
  has_arm <- function(v) stringr::str_detect(v %||% "", "[\\u0530-\\u058F]")
  out <- ifelse(has_arm(a2) | is.na(a2) | a2 == "", a2, b2)
  out <- stringr::str_squish(out); Encoding(out) <- "UTF-8"; out
}

# Armenian admin words (marz/city/village) + common case endings
.rm_admin_words <- function(x) {
  x <- stringr::str_replace(x, "\\s*\\(.*?specify.*\\)$", "")
  x <- stringr::str_replace_all(x, "[\\r\\n]+", " ")
  x <- stringr::str_replace_all(x, "[[:punct:]]+", " ")
  admin <- "(?iu)\\b(?:մարզ(?:ում|ից|ի)?|քաղաք(?:ում|ից|ի)?|գյուղ(?:ում|ից|ի)?|ք\\.|գ\\.|marz|province|region|city|village)\\b"
  x <- stringr::str_replace_all(x, admin, " ")
  stringr::str_squish(x)
}

# Regex recognizers for the 11 marzes (+ Yerevan)
.marz_rx <- list(
  "Yerevan"      = "(?i)\\b(?:Երևան|Երեւան|Yerevan|Erevan)(?:ում|ից|ի)?\\b",
  "Ararat"       = "(?i)\\b(?:Արարատ|Ararat)(?:ում|ից|ի)?\\b",
  "Armavir"      = "(?i)\\b(?:Արմավիր|Armavir)(?:ում|ից|ի)?\\b",
  "Aragatsotn"   = "(?i)\\b(?:Արագածոտն|Aragatsotn)(?:ում|ից|ի)?\\b",
  "Kotayk"       = "(?i)\\b(?:Կոտայք|Kotayk)(?:ում|ից|ի)?\\b",
  "Gegharkunik"  = "(?i)\\b(?:Գեղարքունիք|Gegharkunik|Gegharqunik)(?:ում|ից|ի)?\\b",
  "Lori"         = "(?i)\\b(?:Լոռի|Lori)(?:ում|ից|ի)?\\b",
  "Shirak"       = "(?i)\\b(?:Շիրակ|Shirak)(?:ում|ից|ի)?\\b",
  "Tavush"       = "(?i)\\b(?:Տավուշ|Tavush)(?:ում|ից|ի)?\\b",
  "Vayots Dzor"  = "(?i)\\b(?:Վայոց\\s*ձոր|Վայոց\\s*Ձոր|Vayots\\s*Dzor|Vayotz\\s*Dzor)(?:ում|ից|ի)?\\b",
  "Syunik"       = "(?i)\\b(?:Սյունիք|Syunik)(?:ում|ից|ի)?\\b"
)

# City → marz roll-up (Arm + Eng variants; extend as needed)
.city_to_marz <- c(
  # Ararat
  "Մասիս"="Ararat","Masis"="Ararat","Արտաշատ"="Ararat","Artashat"="Ararat",
  "Վեդի"="Ararat","Vedi"="Ararat","Արարատ"="Ararat","Ararat"="Ararat",
  # Armavir
  "Վաղարշապատ"="Armavir","Vagharshapat"="Armavir","Էջմիածին"="Armavir","Etchmiadzin"="Armavir",
  "Մեծամոր"="Armavir","Metsamor"="Armavir","Արմավիր"="Armavir","Armavir"="Armavir",
  # Kotayk
  "Աբովյան"="Kotayk","Abovyan"="Kotayk","Հրազդան"="Kotayk","Hrazdan"="Kotayk",
  "Չարենցավան"="Kotayk","Charentsavan"="Kotayk","Ծաղկաձոր"="Kotayk","Tsaghkadzor"="Kotayk",
  # Aragatsotn
  "Աշտարակ"="Aragatsotn","Ashtarak"="Aragatsotn","Ապարան"="Aragatsotn","Aparan"="Aragatsotn",
  "Թալին"="Aragatsotn","Talin"="Aragatsotn",
  # Gegharkunik
  "Գավառ"="Gegharkunik","Gavar"="Gegharkunik","Սևան"="Gegharkunik","Սեւան"="Gegharkunik","Sevan"="Gegharkunik",
  "Մարտունի"="Gegharkunik","Martuni"="Gegharkunik","Վարդենիս"="Gegharkunik","Vardenis"="Gegharkunik",
  # Lori
  "Վանաձոր"="Lori","Vanadzor"="Lori","Ալավերդի"="Lori","Alaverdi"="Lori","Սպիտակ"="Lori","Spitak"="Lori",
  # Shirak
  "Գյումրի"="Shirak","Gyumri"="Shirak","Արթիկ"="Shirak","Artik"="Shirak",
  # Tavush
  "Իջևան"="Tavush","Իջեւան"="Tavush","Ijevan"="Tavush","Դիլիջան"="Tavush","Dilijan"="Tavush",
  "Բերդ"="Tavush","Berd"="Tavush","Նոյեմբերյան"="Tavush","Noyemberyan"="Tavush",
  # Vayots Dzor
  "Եղեգնաձոր"="Vayots Dzor","Yeghegnadzor"="Vayots Dzor","Վայք"="Vayots Dzor","Vayk"="Vayots Dzor",
  "Ջերմուկ"="Vayots Dzor","Jermuk"="Vayots Dzor",
  # Syunik
  "Կապան"="Syunik","Kapan"="Syunik","Գորիս"="Syunik","Goris"="Syunik",
  "Սիսիան"="Syunik","Sisian"="Syunik","Մեղրի"="Syunik","Meghri"="Syunik",
  # Yerevan
  "Երևան"="Yerevan","Երեւան"="Yerevan","Yerevan"="Yerevan","Erevan"="Yerevan",
  # New villages & spellings seen in the codebook
  "Zorak"="Ararat","Sis"="Ararat","Sayat-Nova"="Ararat","Ranchpar"="Ararat","Noramarg"="Ararat",
  "Nor Kharberd"="Ararat","Masis"="Ararat","Marmarashen"="Ararat","Kharberd"="Ararat","Khachpar"="Ararat",
  "Hovtashat"="Ararat","Hayanist"="Ararat","Getapnya"="Ararat","Ejmiadzin"="Armavir","Azatashen"="Ararat",
  "Ayntap"="Ararat","Ashtarak"="Aragatsotn","Artashat"="Ararat",
  "Nor Kyuri"="Shirak","Nor Kyurik"="Shirak"
)

# Vectorized: return the canonical marz name or NA
to_marz <- function(place) {
  p <- .rm_admin_words(as.character(place))
  hit_mat <- vapply(.marz_rx, function(rx) stringr::str_detect(p, rx),
                    FUN.VALUE = rep(FALSE, length(p)))
  if (is.null(dim(hit_mat))) {
    hit_mat <- matrix(hit_mat, nrow = length(p), ncol = length(.marz_rx))
    colnames(hit_mat) <- names(.marz_rx)
  }
  any_hit <- rowSums(hit_mat) > 0L
  first_idx <- apply(hit_mat, 1, function(row) { w <- which(row);
  if (length(w)) w[1] else NA_integer_ })
  marz_from_rx <- rep(NA_character_, length(p))
  marz_from_rx[any_hit] <- names(.marz_rx)[first_idx[any_hit]]
  base <- stringr::str_replace(p, "(?i)(ում|ից|ի)$", "")
  city_rollup <- dplyr::recode(base, !!!.city_to_marz, .default = NA_character_)
  dplyr::coalesce(marz_from_rx, city_rollup)
}

# Helper to normalize Artsakh regions from free-text labels
normalize_region_artsakh <- function(x) {
  x  <- as.character(x) |> stringr::str_trim()
  x  <- stringr::str_replace(x, "\\s*\\(specify.*$", "")
  x  <- ifelse(stringr::str_detect(x, "\\[.*prefer.*answer.*\\]"),
               NA_character_, x)
  xl <- stringr::str_to_lower(x)
  out <- dplyr::case_when(
    stringr::str_detect(xl, "^stepanakert") ~ "Stepanakert",
    stringr::str_detect(xl, "^martuni")     ~ "Martuni",
    stringr::str_detect(xl, "^martakert")   ~ "Martakert",
    stringr::str_detect(xl, "^askeran")     ~ "Askeran",
    stringr::str_detect(xl, "^hadrut")      ~ "Hadrut",
    stringr::str_detect(xl, "^shushi")      ~ "Shushi",
    TRUE ~ NA_character_
  )
  factor(out, levels = c("Stepanakert","Martuni","Martakert","Askeran","
                         Hadrut","Shushi"))
}

# Rebuild original UTF-8 from text like "Ã‰Ã¼..."
fix_double_utf8_mojibake <- function(v) {
  v <- as.character(v)
  bad <- !stringr::str_detect(v, "[\\u0530-\\u058F]") & stringr::str_detect(v, "[\\u00A0-\\u00FF]")
  if (!any(bad, na.rm = TRUE)) return(v)
  raw_list <- iconv(v[bad], from = "UTF-8", to = "latin1", toRaw = TRUE)
  v[bad] <- vapply(raw_list, function(r) {
    if (!length(r)) return(NA_character_)
    s <- rawToChar(r); Encoding(s) <- "UTF-8"; s
  }, "", USE.NAMES = FALSE)
  v
}

# ---- Panel 5 aliases ------------------------------------------------------
alias_map_mod5 <- list(
  q42_go_pharmacy     = c("q42_go_pharmacy", "INSTEAD_GO_PHARMACY"),
  q43_recommend_medic = c("q43_recommend_medic", "RECOMMENDED_BUY_MEDICINE"),
  q43_other_specify   = c("q43_other_specify", 
                          "RECOMMENDED_BUY_MEDICINE_OTHER")
)

# This helps me resolve canonical var to the first matching existing column in df_tcwp
resolve_mod5_col <- function(var) {
  cand <- alias_map_mod5[[var]] %||% var
  cand <- cand[cand %in% names(df_tcwp)]
  if (length(cand)) cand[[1]] else NA_character_
}

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
    household_id     = q3_household_ID,
    participant_id   = q4_participant_ID,
    main_respondent  = MAIN_RESPONDENT,
    region_name      = forcats::as_factor(q7_marz),
    age              = q9_b_age,
    gender           = forcats::as_factor(q10_gender),
    nationality      = forcats::as_factor(q11_nationality),
    region_before    = normalize_region_artsakh(q27_region_Artsakh_44_day),
    region_after     = normalize_region_artsakh(q28_region_Artsakh_September)
  ) %>%
  # Panel 7 name update
  rename(
    q54_rating           = q54_standart_of_living,
    q55_housing_type     = q55_housing_situation,
    q56_rooms            = q56_rooms_in_house,
    q57_satisfaction     = q57_saisfied_house,
    q58_pay_flag         = q58_pay_rent_,
    q58_amount           = q58_yes_specify,
    q60_water            = q60_source_water,
    q62_needs_met        = q62_enough_basic_needs,
    q63_food_worry       = q63_worried_enough_food,
    q64_future_hardship  = q64_hardship
  ) %>%
  # Numeric coercions used in plotting
  mutate(
    q56_rooms  = as.numeric(q56_rooms),
    q58_amount = readr::parse_number(q58_amount)
  ) %>%
  # ---- Encoding normalization (key update here) ----
mutate(
  across(any_of("q48_1_which_marz_city"), fix_double_utf8_mojibake),
  across(where(is.character), repair_arm),
  across(where(is.factor), ~ forcats::fct_relabel(.x, repair_arm))
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
  `9` = pick_vars(c("q81_", "q82_", "q83_", "q84_", "q85_", "q86_", "q87_", 
                    "q88_", "q89_", "q90_", "q91_", "q92_", "q93_", "q94_"))
)

# ────────────────────────────────────────────────────────────────────────────
# Panel 5: Friendly labels (single-choice metrics)
# ────────────────────────────────────────────────────────────────────────────
mod5_friendly <- c(
  # Q30: Service needs
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
  # Q30 follow-ups
  q30_1_services_not_covered       = "Services needed but not covered?",
  q30_1_services_not_covered_other = "Other services not covered (specify)",
  q30_2_services_not_offered       = "Services not offered at all?",
  q30_2_services_not_offered_other = "Other services not offered (specify)",
  # Q31–Q32
  q31_need_social_services      = "Needed social services?",
  q32_get_social_services       = "Received social services?",
  # Q34–Q35
  q34_need_mental_health        = "Needed mental health support?",
  q35_get_mental_health         = "Received mental health support?",
  # Q37–Q38
  q37_need_legal_support        = "Needed legal support?",
  q38_get_legal                 = "Received legal support?",
  # Q40–Q43
  q40_need_doctor               = "Needed to see a doctor?",
  q41_go_doctor                 = "Went to a doctor?",
  q42_go_pharmacy               = "Went to a pharmacy instead?",
  q43_recommend_medic           = "Recommended to buy medicine?",
  q43_other_specify             = "Other medicine recommendation (specify)"
)
has_data_mod5 <- function(var) {
  cands <- alias_map_mod5[[var]] %||% var
  cands <- cands[cands %in% names(df_tcwp)]
  if (!length(cands)) return(FALSE)
  any(vapply(cands, function(nm) {
    v <- df_tcwp[[nm]]
    any(!(is.na(v) | as.character(v) == ""), na.rm = TRUE)
  }, logical(1)))
}
mod5_vars_with_data <- names(mod5_friendly)[vapply(names(mod5_friendly), 
                                                   has_data_mod5, logical(1))]
mod5_choices <- setNames(mod5_vars_with_data, 
                         unname(mod5_friendly[mod5_vars_with_data]))

## ────────────────────────────────────────────────────────────────────────────
## Panel 6: Intentions & Perspectives — label mappings
## ────────────────────────────────────────────────────────────────────────────

mod6_choose_labels <- c(
  q45_family_friends   = "Have family or friends here",
  q45_accomodation     = "Accommodation opportunities",
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

# Alias the known typo so selection works
alias_map_mod8 <- list(
  q71_20_troube_falling_asleep = c("q71_20_troube_falling_asleep",
                                   "q71_20_trouble_falling_asleep")
)
resolve_mod8_col <- function(var, df = df_tcwp) {
  if (grepl("^at_least_\\d+$", var)) return(var)
  cand <- (alias_map_mod8[[var]] %||% var)
  cand <- cand[cand %in% names(df)]
  if (length(cand)) cand[[1]] else var
}
mod8_enriched <- shiny::reactive({
  df <- df_tcwp
  total_col <- grep("^q71_.*(total|sum|score)$", names(df), value = TRUE, 
                    ignore.case = TRUE)
  if (length(total_col)) {
    total <- suppressWarnings(as.numeric(df[[ total_col[[1]] ]]))
  } else {
    qcols <- grep("^q71_\\d+", names(df), value = TRUE)
    if (length(qcols)) {
      mat <- df |>
        dplyr::select(dplyr::all_of(qcols)) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ suppressWarnings(as.numeric(.)))) |>
        as.matrix()
      total  <- rowSums(mat, na.rm = TRUE)
      all_na <- apply(mat, 1, function(r) all(is.na(r)))
      total[all_na] <- NA_real_
    } else {
      total <- rep(NA_real_, nrow(df))
    }
  }
  if (!"q71_total" %in% names(df)) df$q71_total <- total
  mk <- function(cut) ifelse(is.na(total), NA_character_, 
                             ifelse(total >= cut, "Yes", "No"))
  df$at_least_20 <- mk(20)
  df$at_least_19 <- mk(19)
  df$at_least_18 <- mk(18)
  df
})
mod8_choices <- setNames(names(mod8_friendly), mod8_friendly)

## ────────────────────────────────────────────────────────────────────────────
## Panel 9: Health Behaviors, QoL & Info Sources — raw→friendly mapping
## ────────────────────────────────────────────────────────────────────────────

# Friendly labels for Panel 9 --------------------------------------------
mod9_friendly <- c(
  # Health behaviors
  q84 = "Smoking status",
  q85 = "Alcohol frequency",
  q86 = "Days drinking per week",
  q87 = "Drinks per occasion",
  q88 = "Ever binge-drank",
  q89 = "Ever used narcotic drugs",
  q90 = "Types of drugs used",
  q91 = "Household drug use",
  q92 = "Self-rated health (last 30d)",
  # QoL sub-questions (check-all matrix)
  q93 = "Problems with daily activities",
  q94 = "Pain or anxious/depressed",
  # Health information sources (frequency matrix)
  q82 = "Info sources frequency",
  q83 = "Preferred info methods"
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
    q93_4_pain               = "Pain interfering with activities",
    q93_5_depressed          = "Anxious / Depressed"
  ),
  q94 = c( # map q94 to two q93 items (no native q94_* columns)
    q93_4_pain               = "Pain interfering with activities",
    q93_5_depressed          = "Anxious / Depressed"
  )
)

# --- Q48.1: decode numeric codes to text and coalesce with the original free-text ---
if ("q48_1_which_marz_city_new" %in% names(df_tcwp)) {
  q48_code_to_label <- c(
    "0"="Zorak","1"="Yerevan","2"=NA_character_,"3"=NA_character_,
    "4"="Vayots Dzor","5"="Syunik","6"="Sis","7"="Sayat-Nova","8"="Ranchpar",
    "9"="Noramarg","10"="Nor Kyuri","11"="Nor Kharberd","12"="Masis","13"="Marmarashen",
    "14"="Kharberd","15"="Khachpar","16"="Hovtashat","17"="Hayanist","18"="Getapnya",
    "19"="Gegharkunik","20"="Ejmiadzin","21"=NA_character_,"22"="Azatashen",
    "23"="Ayntap","24"="Ashtarak","25"="Artashat","26"="Armavir","27"="Ararat"
  )
  df_tcwp <- df_tcwp %>%
    mutate(
      q48_1_from_code = dplyr::recode(as.character(q48_1_which_marz_city_new), !!!q48_code_to_label),
      q48_1_which_marz_city = dplyr::coalesce(as.character(q48_1_which_marz_city), q48_1_from_code),
      q48_1_which_marz_city = repair_arm(q48_1_which_marz_city)
    )
}


# 4. UI ----------------------------------------------------------------------
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = tagList(icon("bar-chart"), "TCWP Survey Explorer")),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("1. Admin & Demographic",        tabName = "admin", 
               icon = icon("users")),
      menuItem("2. Pre-Displacement & Emp.",    tabName = "mod2",  
               icon = icon("truck")),
      menuItem("3. Household Composition",      tabName = "mod3",  
               icon = icon("home")),
      menuItem("4. Population Movement",        tabName = "mod4",  
               icon = icon("map")),
      menuItem("5. Access & Utilization of Svcs", tabName = "mod5", 
               icon = icon("handshake-o")),
      menuItem("6. Intentions & Perspectives",  tabName = "mod6", 
               icon = icon("lightbulb-o")),
      menuItem("7. Socio-Economic Status",      tabName = "mod7", 
               icon = icon("line-chart")),
      menuItem("8. Life Events & Trauma",       tabName = "mod8",  
               icon = icon("heartbeat")),
      menuItem("9. Health Behaviors & QoL",     tabName = "mod9", 
               icon = icon("stethoscope"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Noto+Sans+Armenian:wght@400;600&display=swap",
        rel  = "stylesheet"
      ),
      tags$style(HTML("
    .small-box { height: 120px !important; }
    .content-wrapper, .plotly, .js-plotly-plot * {
      font-family: 'Noto Sans Armenian','Noto Sans','DejaVu Sans',sans-serif !important;
    }
    .box { border-radius: 12px; }
    .box .box-title { font-weight: 600; }
    .sidebar-menu .treeview-menu>li>a { white-space: normal; }
  "))
    ),
    tabItems(
      # Panel 1
      tabItem(tabName = "admin",
              fluidRow(
                valueBoxOutput("vb_households",   width = 4),
                valueBoxOutput("vb_participants", width = 4),
                valueBoxOutput("vb_main_resp",    width = 4)
              ),
              fluidRow(
                valueBoxOutput("vb_age_median",    width = 6),
                valueBoxOutput("vb_hhsize_median", width = 6)
              ),
              fluidRow(
                box(title = "Q22 Household Size (Current)", status = "primary",
                    solidHeader = TRUE, width = 6, 
                    plotlyOutput("hh_size_overview", height = "250px")),
                box(title = "Q10 Gender Breakdown", status = "primary",
                    solidHeader = TRUE, width = 6, 
                    plotlyOutput("gender_bar",  height = "250px")),
                box(title = "Q9.b Age Distribution (Median & IQR)", status = "primary",
                    solidHeader = TRUE, width = 6, 
                    plotlyOutput("age_dist_overview", height = "250px")),
                box(title = "Age Pyramid by Gender (5-year bands)", status = "primary",
                    solidHeader = TRUE, width = 6, 
                    plotlyOutput("age_pyramid", height = "250px"))
              ),
              fluidRow(
                box(title = "Q11 Nationality Breakdown", status = "primary",
                    solidHeader = TRUE, width = 6, 
                    plotlyOutput("nat_bar", height = "250px")),
                box(title = "Q7 Current Marz (map)", status = "primary",
                    solidHeader = TRUE, width = 6, 
                    leafletOutput("map1", height = "250px"))
              )
      ),
      # Panel 2
      tabItem(tabName = "mod2",
              fluidRow(
                valueBoxOutput("vb_emp_before",  width = 4),
                valueBoxOutput("vb_emp_after",   width = 4),
                valueBoxOutput("vb_emp_current", width = 4)
              ),
              fluidRow(
                box(title = "Employment % by Stage", 
                    status = "primary", solidHeader = TRUE, width = 6,
                    plotlyOutput("emp_status_plot", height = "300px")),
                box(title = "Occupations (Pre-Displacement)", 
                    status = "primary", solidHeader = TRUE, width = 6,
                    DTOutput("emp_occ_tbl"))
              ),
              fluidRow(
                box(title = "Reasons for Distrust (all check-all)", 
                    status = "warning",
                    solidHeader = TRUE, width = 12, 
                    plotlyOutput("mistrust_reasons", height = "350px"))
              ),
              fluidRow(
                box(title = "Barriers to Finding Work (Q21)", status = "danger",
                    solidHeader = TRUE, width = 12, 
                    plotlyOutput("barriers_work", height = "300px"))
              ),
              fluidRow(
                box(title = "Explore any metric", status = "success", 
                    solidHeader = TRUE, width = 4,
                    selectInput("var2", "Metric:", choices = mod2_choices, 
                                selected = mod2_choices[[1]])),
                box(title = "Count Table", status = "info", solidHeader = TRUE,
                    width = 8, DTOutput("tbl2"))
              ),
              fluidRow(
                box(title = "Distribution Plot", status = "info",
                    solidHeader = TRUE, width = 12,
                    plotOutput("plt2", height = "350px"))
              )
      ),
      # Panel 3
      tabItem(tabName = "mod3",
              fluidRow(
                box(title = "Household Size", status = "warning",
                    solidHeader = TRUE, width = 4,
                    selectInput("hh_size_var", "Which size?",
                                choices = setNames(module_vars[["3"]][1:2],
                                                   c("Current household size (Q22)", "Before displacement (Q23)")),
                                selected = module_vars[["3"]][1])),
                box(title = "Size distribution", status = "info", 
                    solidHeader = TRUE, width = 8,
                    plotlyOutput("hh_size_plot", height = "300px"))
              ),
              fluidRow(
                box(title = "Member characteristic", status = "warning",
                    solidHeader = TRUE, width = 4,
                    selectInput("member_var", "Pick a question:", choices = c(
                      "Year of birth (Q23.2)"="year_birth",
                      "Gender (Q23.3)"="gender",
                      "Relation to respondent (Q23.4)"="relation_respondent",
                      "Current status (alive/dead) (Q23.5)"="current_health_status",
                      "Lives with respondent? (Q23.6)"="living_with_respond",
                      "Employment status (Q23.7)"="employment"
                    ), selected = "gender")),
                box(title = textOutput("member_title"), status = "info",
                    solidHeader = TRUE, width = 8, 
                    plotlyOutput("member_plot", height = "300px"))
              )
      ),
      # Panel 4
      tabItem(tabName = "mod4",
              fluidRow(
                box(title = "Display options", status = "warning",
                    solidHeader = TRUE, width = 12,
                    checkboxInput("mod4_keep_na",
                                  "Include '(Missing/NA)' & '[prefer not to answer]' in charts",
                                  value = FALSE),
                    selectInput("mod4_sort", "Sort bars by",
                                choices = c("Count (descending)"="count_desc",
                                            "Alphabetical (A→Z)"="alpha"),
                                selected = "count_desc"),
                    numericInput("mod4_min_flow",
                                 "Minimum flow size for Sankey (count)", 
                                 value = 2, min = 1, step = 1))
              ),
              fluidRow(
                box(title = "Region before the 44-day war", status = "primary",
                    solidHeader = TRUE, width = 6, 
                    plotlyOutput("mod4_before_plot", height = "300px")),
                box(title = "Region moved from (Sep 2023)", status = "primary",
                    solidHeader = TRUE, width = 6, 
                    plotlyOutput("mod4_after_plot",  height = "300px"))
              ),
              fluidRow(
                box(title = "# of moves since Sep 19, 2023", status = "info",
                    solidHeader = TRUE, width = 8, 
                    plotlyOutput("mod4_nmoves_hist", height = "300px"),
                    verbatimTextOutput("mod4_nmoves_stats")),
                box(title = "Notes", status = "info", solidHeader = TRUE, width = 4,
                    helpText("• Region names are normalized (e.g., remove '(specify...)')."),
                    helpText("• Toggle the checkbox above to include missing/'prefer not to answer'."),
                    helpText("• Use 'Minimum flow size' to filter tiny flows in the Sankey."))
              ),
              fluidRow(
                box(title = "Flows: Region before → Region moved from (Sep 2023)",
                    status = "info",
                    solidHeader = TRUE, width = 8,
                    plotlyOutput("mod4_flow", height = "380px")),
                box(title = "Top flows (pairs)", status = "info", 
                    solidHeader = TRUE, width = 4, DTOutput("mod4_flow_tbl"))
              )
      ),
      # Panel 5
      tabItem(tabName = "mod5",
              fluidRow(
                box(title = "Single‐Choice Metrics", status = "warning", 
                    solidHeader = TRUE, width = 4,
                    selectInput("var5", "Variable:", choices = mod5_choices, 
                                selected = names(mod5_choices)[1])),
                box(title = "Count Table", status = "info", solidHeader = TRUE,
                    width = 8, DTOutput("tbl5"))
              ),
              fluidRow(
                box(title = "Distribution Plot", status = "info", 
                    solidHeader = TRUE, width = 12,
                    plotlyOutput("plt5", height = "350px"))
              ),
              fluidRow(
                box(title = "Reasons for Not Getting Social Services (Q33)",
                    status = "danger",
                    solidHeader = TRUE, width = 6, plotlyOutput("reasons_social")),
                box(title = "Reasons for Not Getting Mental Health Support (Q36)", 
                    status = "danger",
                    solidHeader = TRUE, width = 6, plotlyOutput("reasons_mental"))
              ),
              fluidRow(
                box(title = "Reasons for Not Getting Legal Support (Q39)", 
                    status = "danger",
                    solidHeader = TRUE, width = 6, plotlyOutput("reasons_legal")),
                box(title = "Reasons for Not Going to a Doctor (Q44)",
                    status = "danger",
                    solidHeader = TRUE, width = 6, plotlyOutput("reasons_doctor"))
              )
      ),
      # Panel 6
      tabItem(tabName = "mod6",
              fluidRow(
                box(title = "Why did you/your family choose this community?",
                    status = "primary",
                    solidHeader = TRUE, width = 6, plotlyOutput("mod6_choose_comm",
                                                                height = "300px")),
                box(title = "Are you planning to leave your current accommodation?",
                    status = "primary",
                    solidHeader = TRUE, width = 6, plotlyOutput("mod6_plan_leave", 
                                                                height = "300px"))
              ),
              fluidRow(
                box(title = "If so, why are you planning to leave?", 
                    status = "warning",
                    solidHeader = TRUE, width = 12, 
                    plotlyOutput("mod6_reasons_leave", height = "300px"))
              ),
              fluidRow(
                box(title = "Within the next 6 months, stay in Armenia or move?",
                    status = "info",
                    solidHeader = TRUE, width = 6, 
                    plotlyOutput("mod6_stay_vs_move", height = "300px")),
                box(title = "If staying, which Marz/City?", status = "info",
                    solidHeader = TRUE, width = 6, 
                    plotlyOutput("mod6_stay_location", height = "300px"))
              ),
              fluidRow(
                box(title = "Why are you planning to move somewhere else?", 
                    status = "danger",
                    solidHeader = TRUE, width = 12,
                    plotlyOutput("mod6_reasons_move", height = "300px"))
              ),
              fluidRow(
                box(title = "Which country are you planning to go to?", 
                    status = "success",
                    solidHeader = TRUE, width = 6, 
                    plotlyOutput("mod6_dest_country", height = "300px")),
                box(title = "Why that country?", status = "success",
                    solidHeader = TRUE, width = 6, 
                    plotlyOutput("mod6_reasons_country", height = "300px"))
              ),
              fluidRow(
                box(title = "Are you missing official documentation?",
                    status = "info",
                    solidHeader = TRUE, width = 6, 
                    plotlyOutput("mod6_missing_docs", height = "300px")),
                box(title = "What type of documents are you missing?", 
                    status = "info",
                    solidHeader = TRUE, width = 6,
                    plotlyOutput("mod6_types_docs", height = "300px"))
              )
      ),
      # Panel 7
      tabItem(tabName = "mod7",
              fluidRow(
                box(title = "Controls", status = "warning", solidHeader = TRUE,
                    width = 3,
                    selectInput("var7", "Metric:", choices = mod7_choices, 
                                selected = names(mod7_choices)[1])),
                box(title = "Table", status = "info", solidHeader = TRUE, 
                    width = 9, DTOutput("tbl7"))
              ),
              fluidRow(
                box(title = "Distribution Plot", status = "info", 
                    solidHeader = TRUE, width = 12,
                    plotOutput("plt7", height = "350px"))
              )
      ),
      # Panel 8
      tabItem(tabName = "mod8",
              fluidRow(
                box(title = "Controls", status = "warning", 
                    solidHeader = TRUE, width = 3,
                    selectInput("var8", "Variable:", choices = mod8_choices,
                                selected = names(mod8_choices)[1])),
                box(title = "Table", status = "info", solidHeader = TRUE,
                    width = 9, DTOutput("tbl8"))
              ),
              fluidRow(
                box(title = "Distribution Plot", status = "info", 
                    solidHeader = TRUE, width = 12,
                    plotOutput("plt8", height = "350px"))
              )
      ),
      # Panel 9
      tabItem(tabName = "mod9",
              fluidRow(
                box(title = "Select question", status = "warning", 
                    solidHeader = TRUE, width = 3,
                    selectInput("var9", "Question:", choices = mod9_choices, 
                                selected = names(mod9_choices)[1])),
                box(title = "Count table", status = "info", solidHeader = TRUE,
                    width = 9, DTOutput("tbl9"))
              ),
              fluidRow(
                box(title = "Distribution / Pivot plot", status = "info",
                    solidHeader = TRUE, width = 12, 
                    plotOutput("plt9", height = "350px"))
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
  
  # Gender breakdown
  output$gender_bar <- renderPlotly({
    df_tcwp %>%
      count(q10_gender) %>%
      filter(!is.na(q10_gender)) %>%
      plot_ly(x = ~q10_gender, y = ~n, type = "bar") %>%
      layout(title = "Q10 Gender", xaxis = list(title = ""), 
             yaxis = list(title = "Count"))
  })
  
  # Nationality breakdown
  output$nat_bar <- renderPlotly({
    df_tcwp %>%
      count(q11_nationality) %>%
      filter(!is.na(q11_nationality)) %>%
      arrange(n) %>%
      mutate(nationality = factor(q11_nationality, levels = q11_nationality)) %>%
      plot_ly(x = ~n, y = ~nationality, type = "bar", orientation = "h") %>%
      layout(title = "Q11 Nationality", xaxis = list(title = "Count"),
             yaxis = list(title = ""))
  })
  
  # Marz map
  output$map1 <- renderLeaflet({
    rc <- df_tcwp %>% filter(!is.na(q7_marz)) %>% count(q7_marz, name = "n")
    mdf <- regions_sf %>% left_join(rc, by = c("NAME_1" = "q7_marz")) %>% mutate(n = replace_na(n, 0))
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
  
  # I will have a single clear Q22 size column if present
  get_hh_size_col <- reactive({
    preferred <- c("q22_household_size", "q22_total_members", "q22_size")
    cand_pref <- preferred[preferred %in% names(df_tcwp)]
    if (length(cand_pref)) return(cand_pref[[1]])
    cand_any <- grep("^q22_", names(df_tcwp), value = TRUE)
    if (length(cand_any)) return(cand_any[[1]])
    NA_character_
  })
  
  # Median age (with IQR)
  output$vb_age_median <- renderValueBox({
    a <- suppressWarnings(as.numeric(df_tcwp$q9_b_age))
    a <- a[is.finite(a)]
    if (!length(a)) {
      valueBox("—", "Median Age (Q9.b)", icon = icon("user"), 
               color = "light-blue")
    } else {
      med <- stats::median(a, na.rm = TRUE)
      q25 <- stats::quantile(a, 0.25, na.rm = TRUE, names = FALSE)
      q75 <- stats::quantile(a, 0.75, na.rm = TRUE, names = FALSE)
      valueBox(sprintf("%.0f (IQR %.0f–%.0f)", med, q25, q75),
               "Median Age (Q9.b)", icon = icon("user"), color = "light-blue")
    }
  })
  
  # Median household size (with IQR)
  output$vb_hhsize_median <- renderValueBox({
    col <- get_hh_size_col()
    if (is.na(col)) {
      valueBox("—", "Median Household Size (Q22)", icon = icon("house"), 
               color = "olive")
    } else {
      v <- suppressWarnings(as.numeric(df_tcwp[[col]]))
      v <- v[is.finite(v)]
      if (!length(v)) {
        valueBox("—", "Median Household Size (Q22)", icon = icon("house"),
                 color = "olive")
      } else {
        med <- stats::median(v, na.rm = TRUE)
        q25 <- stats::quantile(v, 0.25, na.rm = TRUE, names = FALSE)
        q75 <- stats::quantile(v, 0.75, na.rm = TRUE, names = FALSE)
        valueBox(sprintf("%.0f (IQR %.0f–%.0f)", med, q25, q75),
                 "Median Household Size (Q22)", icon = icon("house"), 
                 color = "olive")
      }
    }
  })
  
  # Household size distribution (Median & IQR overlays)
  output$hh_size_overview <- renderPlotly({
    col <- get_hh_size_col()
    if (is.na(col)) {
      return(plot_ly() %>% layout(
        title = "Q22 household size not found",
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
    }
    df <- df_tcwp %>%
      filter(!is.na(.data[[col]]), .data[[col]] != "") %>%
      mutate(size = suppressWarnings(as.numeric(.data[[col]]))) %>%
      filter(is.finite(size))
    if (!nrow(df)) return(plot_ly())
    med <- stats::median(df$size); q25 <- stats::quantile(df$size, 0.25); q75 <- stats::quantile(df$size, 0.75)
    df %>% count(size) %>%
      plot_ly(x = ~size, y = ~n, type = "bar") %>%
      layout(
        xaxis = list(title = "Household size"),
        yaxis = list(title = "Count"),
        shapes = list(
          list(type="line", x0=med, x1=med, y0=0, y1=1, xref="x", yref="paper",
               line=list(color="black", width=2)),
          list(type="line", x0=q25, x1=q25, y0=0, y1=1, xref="x", yref="paper",
               line=list(color="gray", dash="dash")),
          list(type="line", x0=q75, x1=q75, y0=0, y1=1, xref="x", yref="paper",
               line=list(color="gray", dash="dash"))
        ),
        annotations = list(
          list(x = med, y = 1.02, xref="x", yref="paper",
               text = paste0("Median ", round(med,1)),
               showarrow = FALSE, yanchor="bottom"),
          list(x = q25, y = 1.02, xref="x", yref="paper", 
               text = paste0("Q1 ", round(q25,1)),
               showarrow = FALSE, yanchor="bottom"),
          list(x = q75, y = 1.02, xref="x", yref="paper", 
               text = paste0("Q3 ", round(q75,1)),
               showarrow = FALSE, yanchor="bottom")
        )
      )
  })
  
  # Age histogram with overlays
  output$age_dist_overview <- renderPlotly({
    a <- suppressWarnings(as.numeric(df_tcwp$q9_b_age))
    a <- a[is.finite(a) & a >= 0 & a <= 100]
    if (!length(a)) {
      return(plot_ly() %>% layout(
        title = "Q9.b age not available",
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
    }
    med <- stats::median(a); q25 <- stats::quantile(a, 0.25); q75 <- stats::quantile(a, 0.75)
    plot_ly(x = ~a, type = "histogram", nbinsx = 20) %>%
      layout(
        xaxis = list(title = "Age (years)"),
        yaxis = list(title = "Count"),
        shapes = list(
          list(type="line", x0=med, x1=med, y0=0, y1=1, xref="x", yref="paper",
               line=list(color="black", width=2)),
          list(type="line", x0=q25, x1=q25, y0=0, y1=1, xref="x", yref="paper", 
               line=list(color="gray", dash="dash")),
          list(type="line", x0=q75, x1=q75, y0=0, y1=1, xref="x", yref="paper",
               line=list(color="gray", dash="dash"))
        ),
        annotations = list(
          list(x = med,  y = 1.02, xref="x", yref="paper",
               text = paste0("Median ", round(med,0)),
               showarrow = FALSE, yanchor="bottom"),
          list(x = q25,  y = 1.02, xref="x", yref="paper", 
               text = paste0("Q1 ", round(q25,0)),
               showarrow = FALSE, yanchor="bottom"),
          list(x = q75,  y = 1.02, xref="x", yref="paper", 
               text = paste0("Q3 ", round(q75,0)),
               showarrow = FALSE, yanchor="bottom")
        )
      )
  })
  
  # Age pyramid by gender (5-year bands)
  output$age_pyramid <- renderPlotly({
    gcol <- if ("gender" %in% names(df_tcwp)) "gender"
    else if ("q10_gender" %in% names(df_tcwp)) "q10_gender" else NA_character_
    a <- suppressWarnings(as.numeric(df_tcwp$q9_b_age))
    if (is.na(gcol) || !length(a)) {
      return(plot_ly() %>% layout(
        title = "Age/Gender not available",
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
    }
    a <- a[is.finite(a) & a >= 0 & a <= 100]; if (!length(a)) return(plot_ly())
    brks <- seq(floor(min(a)/5)*5, ceiling(max(a)/5)*5 + 5, by = 5)
    labs <- paste(brks[-length(brks)], brks[-1]-1, sep = "–")
    d <- df_tcwp %>%
      transmute(age  = suppressWarnings(as.numeric(q9_b_age)), 
                sex0 = as.character(.data[[gcol]])) %>%
      filter(is.finite(age)) %>%
      mutate(
        band = cut(age, breaks = brks, labels = labs, 
                   right = TRUE, include.lowest = TRUE),
        sex  = dplyr::case_when(
          grepl("female|^f$", tolower(sex0)) ~ "Female",
          grepl("male|^m$",   tolower(sex0)) ~ "Male",
          TRUE ~ "Other/NA"
        )
      )
    df_m <- d %>% filter(sex == "Male")   %>% count(band, name = "n")
    df_f <- d %>% filter(sex == "Female") %>% count(band, name = "n")
    all_bands <- data.frame(band = factor(labs, levels = labs))
    df_m <- all_bands %>% left_join(df_m, by = "band") %>% mutate(n = tidyr::replace_na(n, 0))
    df_f <- all_bands %>% left_join(df_f, by = "band") %>% mutate(n = tidyr::replace_na(n, 0))
    plot_ly() %>%
      add_bars(y = df_m$band, x = -df_m$n, name = "Male",   orientation = "h") %>%
      add_bars(y = df_f$band, x =  df_f$n, name = "Female", orientation = "h") %>%
      layout(
        barmode = "overlay",
        xaxis = list(title = "Count (← Male | Female →)",
                     tickmode = "array",
                     tickvals = c(-max(df_m$n), 0, max(df_f$n)),
                     ticktext = c(max(df_m$n), 0, max(df_f$n))),
        yaxis = list(title = "Age band"),
        legend = list(x = 0.02, y = 0.98)
      )
  })

  ## Panel 2: Pre‐Displacement & Employment ----------------------------------
  
  occ_cols_all <- grep("^q16_", names(df_tcwp), value = TRUE)
  occ_cols <- setdiff(occ_cols_all, c("q16_employed_before_displacement"))
  occ_cols_spec <- grep("(specify|occupation|job|work)", occ_cols, 
                        value = TRUE, ignore.case = TRUE)
  if (length(occ_cols_spec) > 0) occ_cols <- occ_cols_spec
  
  # Value boxes
  output$vb_emp_before <- renderValueBox({
    pct <- mean(df_tcwp$q16_employed_before_displacement == "Yes", na.rm = TRUE)
    valueBox(sprintf("%.2f%%", pct * 100), "Employed Before Displacement", 
             icon = icon("briefcase"), color = "blue")
  })
  output$vb_emp_after <- renderValueBox({
    pct <- mean(df_tcwp$q17_employed_after_displacement == "Yes", na.rm = TRUE)
    valueBox(sprintf("%.2f%%", pct * 100), "Employed After Displacement",
             icon = icon("globe"), color = "green")
  })
  output$vb_emp_current <- renderValueBox({
    pct <- mean(df_tcwp$q20_currently_employed == "Yes", na.rm = TRUE)
    valueBox(sprintf("%.2f%%", pct * 100), "Currently Employed",
             icon = icon("user-tie"), color = "purple")
  })
  
  # Employment % by Stage
  output$emp_status_plot <- renderPlotly({
    df_pct <- tibble(
      Stage   = c("Before", "After", "Current"),
      Percent = c(
        mean(df_tcwp$q16_employed_before_displacement == "Yes", na.rm = TRUE),
        mean(df_tcwp$q17_employed_after_displacement  == "Yes", na.rm = TRUE),
        mean(df_tcwp$q20_currently_employed           == "Yes", na.rm = TRUE)
      )
    )
    plot_ly(df_pct, x = ~Stage, y = ~Percent, type = "bar") %>%
      layout(yaxis = list(title = "Proportion", tickformat = ".2%"), 
             xaxis = list(title = ""))
  })
  
  # Occupations (Pre-Displacement)
  output$emp_occ_tbl <- renderDT({
    employed_yes <- c("Yes","YES","yes","Y","1",1,TRUE)
    dat <- df_tcwp %>%
      filter(q16_employed_before_displacement %in% employed_yes) %>%
      select(any_of(occ_cols)) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(
        everything(), names_to = "code", values_to = "occupation",
        values_drop_na = TRUE,
        values_transform = list(occupation = as.character)
      ) %>%
      mutate(occupation = trimws(occupation)) %>%
      filter(occupation != "", 
             !occupation %in% c("NA","N/A","na","n/a","-","--")) %>%
      count(occupation, name = "Count", sort = TRUE)
    datatable(dat, options = list(pageLength = 10, autoWidth = TRUE), 
              rownames = FALSE)
  })
  
  # Reasons for Distrust
  output$mistrust_reasons <- renderPlotly({
    df_tcwp %>%
      select(starts_with("q13_reasons_")) %>%
      pivot_longer(everything(), names_to = "reason", values_to = "sel") %>%
      filter(sel == "Yes") %>%
      count(reason) %>%
      mutate(label = ifelse(is.na(mod2_friendly[reason]), 
                            reason, unname(mod2_friendly[reason]))) %>%
      plot_ly(x = ~label, y = ~n, type = "bar") %>%
      layout(xaxis = list(title = "", tickangle = -45), 
             yaxis = list(title = "Count"))
  })
  
  # Explore any metric
  output$tbl2 <- renderDT({
    req(input$var2); var <- input$var2
    cnt <- df_tcwp %>%
      filter(!is.na(.data[[var]])) %>%
      count(Value = .data[[var]], name = "Count") %>%
      arrange(desc(Count))
    datatable(cnt, options = list(pageLength = 10, autoWidth = TRUE))
  })
  output$plt2 <- renderPlot({
    req(input$var2); var <- input$var2; label <- mod2_friendly[[var]]
    cnt <- df_tcwp %>%
      filter(!is.na(.data[[var]])) %>%
      count(Value = .data[[var]], name = "n")
    ggplot(cnt, aes(x = Value, y = n)) +
      geom_col(fill = "#2C3E50") +
      labs(x = label, y = "Count",
           title = if (startsWith(var, "q13_")) paste0(label, " (check‐all that apply)") else label) +
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
                            q21_documentation="Lack of Documentation",
                            q21_employemnt_opportunities="No Decent Jobs",
                            q21_opportunities_suited_to_me="Not Suited To My Skills",
                            q21_opportunities_someone="Jobs for My Age",
                            q21_access="No Childcare Access",
                            q21_education="Education Not Recognized",
                            q21_information="Lack of Info, How to Apply",
                            q21_not_planning_stay="Not Planning To Stay",
                            q21_discrimination="Discrimination",
                            q21_other="Other"
      )) %>%
      arrange(desc(n)) %>%
      plot_ly(x=~n, y=~reorder(label, n), type="bar", orientation="h") %>%
      layout(xaxis=list(title="Count"), yaxis=list(title="Barrier"))
  })
  
  ## Panel 3: Household Composition ------------------------------------------
  
  output$hh_size_plot <- renderPlotly({
    req(input$hh_size_var); var <- input$hh_size_var
    df_tcwp %>%
      filter(!is.na(!!sym(var))) %>%
      count(size = !!sym(var)) %>%
      plot_ly(x = ~size, y = ~n, type = "bar") %>%
      layout(xaxis = list(title = "Household size"), 
             yaxis = list(title = "Count"))
  })
  
  members_long <- reactive({
    df_tcwp %>%
      select(matches("^q23_")) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(
        cols = matches("^q23_\\d+_.+_\\d+$"),
        names_to = c("q_num", "question", "member_index"),
        names_pattern = "q23_(\\d+)_(.+)_(\\d+)$",
        values_to = "value"
      ) %>%
      filter(!is.na(value) & value != "")
  })
  member_labels <- c(
    year_birth = "Year of birth",
    gender = "Gender",
    relation_respondent = "Relation to respondent",
    current_health_status = "Current status (alive/dead)",
    living_with_respond = "Lives with respondent?",
    employment = "Employment status"
  )
  output$member_title <- renderText({ member_labels[[ input$member_var ]] })
  output$member_plot <- renderPlotly({
    req(input$member_var)
    members_long() %>%
      filter(question == input$member_var) %>%
      count(answer = value) %>%
      plot_ly(x = ~answer, y = ~n, type = "bar") %>%
      layout(xaxis = list(title = member_labels[[input$member_var]]),
             yaxis = list(title = "Count"))
  })
  
  ## Panel 4: Population Movement --------------------------------------------
  
  resolve_nmoves_var <- function(df = df_tcwp) {
    priority <- c(
      "q29_change_living_frequency",
      "q29_change_living_frequesncy",
      "q29_change_living_frquesncy",
      "q29_nmoves","q29_number_of_moves","q29_moves"
    )
    hit <- priority[priority %in% names(df)]; if (length(hit)) return(hit[[1]])
    q29_cols <- grep("^q29_", names(df), value = TRUE)
    cand <- grep("(move|moves|chang|change|freq(u|e)?n?c?y?|freq$|number)",
                 q29_cols, value = TRUE, ignore.case = TRUE)
    if (!length(cand)) return(NA_character_)
    score <- sapply(cand, function(nm) {
      v <- df[[nm]]; num <- suppressWarnings(readr::parse_number(as.character(v)))
      mean(is.finite(num), na.rm = TRUE)
    })
    cand[which.max(score)]
  }
  coerce_nmoves <- function(v) {
    x <- as.character(v) |> trimws()
    x[x %in% c("None","No","No moves","None/Zero","0 moves")] <- "0"
    x[x %in% c("One","Once")]  <- "1"
    x[x %in% c("Two","Twice")] <- "2"
    num <- suppressWarnings(readr::parse_number(x))
    num[!is.finite(num)] <- NA_real_
    num
  }
  maybe_keep_missing <- function(before, after, keep_na = FALSE) {
    lab <- "(Missing/NA)"
    tibble::tibble(
      before = ifelse(is.na(before) | before == "", lab, before),
      after  = ifelse(is.na(after)  | after  == "", lab, after)
    ) %>% { if (!keep_na) dplyr::filter(., before != lab, after != lab) else . }
  }
  p_empty <- function(title_txt) {
    plotly::plotly_empty(type = "scatter", mode = "markers") %>%
      layout(title = list(text = title_txt),
             xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
             showlegend = FALSE)
  }
  
  output$mod4_before_plot <- renderPlotly({
    keep_na <- isTRUE(input$mod4_keep_na)
    sort_by <- input$mod4_sort %||% "count_desc"
    d <- df_tcwp %>%
      transmute(region = as.character(region_before)) %>%
      mutate(region = ifelse(is.na(region) | region == "",
                             "(Missing/NA)", region)) %>%
      { if (!keep_na) dplyr::filter(., region != "(Missing/NA)") else . } %>%
      count(region, name = "n")
    if (!nrow(d)) return(p_empty("No region-before data available"))
    xax <- if (sort_by == "alpha") {
      list(title = "Region before war", tickangle = -45, 
           categoryorder = "array", categoryarray = sort(d$region))
    } else {
      list(title = "Region before war", tickangle = -45, 
           categoryorder = "total descending")
    }
    plot_ly(d, x = ~region, y = ~n, type = "bar") %>%
      layout(xaxis = xax, yaxis = list(title = "# households"),
             margin = list(b = 100))
  })
  
  output$mod4_after_plot <- renderPlotly({
    keep_na <- isTRUE(input$mod4_keep_na)
    sort_by <- input$mod4_sort %||% "count_desc"
    d <- df_tcwp %>%
      transmute(region = as.character(region_after)) %>%
      mutate(region = ifelse(is.na(region) | region == "", "(Missing/NA)", region)) %>%
      { if (!keep_na) dplyr::filter(., region != "(Missing/NA)") else . } %>%
      count(region, name = "n")
    if (!nrow(d)) return(p_empty("No region-after data available"))
    xax <- if (sort_by == "alpha") {
      list(title = "Region moved from (Sep 2023)", tickangle = -45,
           categoryorder = "array", categoryarray = sort(d$region))
    } else {
      list(title = "Region moved from (Sep 2023)", tickangle = -45, 
           categoryorder = "total descending")
    }
    plot_ly(d, x = ~region, y = ~n, type = "bar") %>%
      layout(xaxis = xax, yaxis = list(title = "# households"),
             margin = list(b = 100))
  })
  
  output$mod4_nmoves_hist <- renderPlotly({
    col <- resolve_nmoves_var(); if (is.na(col)) return(p_empty("Q29 '# of moves' column not found"))
    df <- df_tcwp %>% mutate(nmoves = coerce_nmoves(.data[[col]])) %>% filter(is.finite(nmoves) & nmoves >= 0)
    if (!nrow(df)) return(p_empty("No numeric values for # of moves"))
    xmax <- max(df$nmoves, na.rm = TRUE)
    plot_ly(df, x = ~nmoves, type = "histogram", 
            xbins = list(start = -0.5, end = xmax + 0.5, size = 1)) %>%
      layout(xaxis = list(title = "# of moves since Sep 19, 2023"), 
             yaxis = list(title = "Count"))
  })
  
  output$mod4_nmoves_stats <- renderText({
    col <- resolve_nmoves_var(); if (is.na(col)) return("Q29 '# of moves' column not found.")
    df <- df_tcwp %>% mutate(nmoves = coerce_nmoves(.data[[col]])) %>% filter(is.finite(nmoves) & nmoves >= 0)
    if (!nrow(df)) return("No numeric values for # of moves.")
    s <- df %>% summarise(
      mean = mean(nmoves), median = median(nmoves),
      p75 = quantile(nmoves, 0.75), p90 = quantile(nmoves, 0.90),
      ge1 = mean(nmoves >= 1), ge2 = mean(nmoves >= 2), ge3 = mean(nmoves >= 3)
    )
    sprintf("Mean = %.2f, median = %.1f, P75 = %.1f, P90 = %.1f | ≥1: %.1f%%, ≥2: %.1f%%, ≥3: %.1f%%",
            s$mean, s$median, s$p75, s$p90, 100*s$ge1, 100*s$ge2, 100*s$ge3)
  })
  
  output$mod4_flow <- renderPlotly({
    keep_na <- isTRUE(input$mod4_keep_na)
    min_n   <- as.integer(input$mod4_min_flow %||% 2)
    d <- maybe_keep_missing(
      before = as.character(df_tcwp$region_before),
      after  = as.character(df_tcwp$region_after),
      keep_na = keep_na
    )
    flows <- d %>% count(before, after, name = "n") %>% filter(n >= min_n) %>% arrange(desc(n))
    if (!nrow(flows)) return(p_empty("No flow data available (check filters)"))
    nodes <- tibble::tibble(name = unique(c(flows$before, flows$after)))
    flows <- flows %>% mutate(
      source = match(before, nodes$name) - 1,
      target = match(after,  nodes$name) - 1
    )
    plot_ly(
      type = "sankey", arrangement = "snap",
      node = list(label = nodes$name, pad = 10, thickness = 14),
      link = list(source = flows$source, target = flows$target, value = flows$n)
    ) %>% layout(margin = list(l = 20, r = 20, t = 20, b = 20))
  })
  
  output$mod4_flow_tbl <- renderDT({
    keep_na <- isTRUE(input$mod4_keep_na)
    min_n   <- as.integer(input$mod4_min_flow %||% 2)
    lab     <- "(Missing/NA)"
    d <- df_tcwp %>%
      transmute(`Region before` = as.character(region_before),
                `Region after (Sep 2023)` = as.character(region_after)) %>%
      mutate(
        `Region before`           = ifelse(is.na(`Region before`) | `Region before` == "", lab, `Region before`),
        `Region after (Sep 2023)` = ifelse(is.na(`Region after (Sep 2023)`) | `Region after (Sep 2023)` == "", lab, `Region after (Sep 2023)`)
      ) %>%
      { if (!keep_na) dplyr::filter(., `Region before` != lab, `Region after (Sep 2023)` != lab) else . } %>%
      count(`Region before`, `Region after (Sep 2023)`, 
            name = "Count", sort = TRUE) %>%
      filter(Count >= min_n)
    if (!nrow(d)) {
      return(DT::datatable(tibble::tibble(Note = "No pairs at current filters"),
                           options = list(dom = 't'), rownames = FALSE))
    }
    datatable(d, options = list(pageLength = 8, autoWidth = TRUE), 
              rownames = FALSE)
  })

  ## Panel 5: Access & Utilization of Services --------------------------------
  
  output$reasons_social <- renderPlotly({
    df_tcwp %>%
      select(starts_with("q33_")) %>%
      pivot_longer(everything(), names_to = "code", values_to = "sel") %>%
      filter(sel == "Yes") %>%
      count(code) %>%
      mutate(label = recode(code,
                            q33_lack_money="Lack of money/too expensive",
                            q33_lack_transportation="Lack of transportation",
                            q33_lack_time="Lack of time",
                            q33_lack_documentation="Lack of documentation",
                            q33_lack_services="No available services",
                            q33_donot_trust_services="Didn’t trust services",
                            q33_prefer_not_to_answer="Prefer not to answer",
                            q33_other="Other"
      )) %>%
      plot_ly(x = ~n, y = ~reorder(label, n), type = "bar", orientation = "h") %>%
      layout(xaxis = list(title = "Count"), yaxis = list(title = "Reason"))
  })
  
  output$reasons_mental <- renderPlotly({
    df_tcwp %>%
      select(starts_with("q36_")) %>%
      pivot_longer(everything(), names_to = "code", values_to = "sel") %>%
      filter(sel == "Yes") %>%
      count(code) %>%
      mutate(label = recode(code,
                            q36_lack_money="Lack of money/too expensive",
                            q36_lack_transport="Lack of transportation",
                            q36_lack_time="Lack of time",
                            q36_lack_specialist="Lack of specialists",
                            q36_no_believe_effect="Didn’t believe in effectiveness",
                            q36_fear_diagnosis="Fear of diagnosis/treatment",
                            q36_no_trust="Didn’t trust specialists",
                            q36_prefer_selcare="Preferred self‐care",
                            q36_prefer_not_answer="Prefer not to answer",
                            q36_other="Other"
      )) %>%
      plot_ly(x = ~n, y = ~reorder(label, n), type = "bar", orientation = "h") %>%
      layout(xaxis = list(title = "Count"), yaxis = list(title = "Reason"))
  })
  
  output$reasons_legal <- renderPlotly({
    df_tcwp %>%
      select(starts_with("q39_")) %>%
      pivot_longer(everything(), names_to = "code", values_to = "sel") %>%
      filter(sel == "Yes") %>%
      count(code) %>%
      mutate(label = recode(code,
                            q39_lack_money="Lack of money/too expensive",
                            q39_lack_transport="Lack of transportation",
                            q39_lack_time="Lack of time",
                            q39_lack_specialist="Lack of specialists",
                            q39_no_trust_specilaist="Didn’t trust specialists",
                            q39_prefer_not_answer="Prefer not to answer",
                            q39_other="Other"
      )) %>%
      plot_ly(x = ~n, y = ~reorder(label, n), type = "bar", orientation = "h") %>%
      layout(xaxis = list(title = "Count"), yaxis = list(title = "Reason"))
  })
  
  output$reasons_doctor <- renderPlotly({
    df_tcwp %>%
      select(starts_with("q44_")) %>%
      pivot_longer(everything(), names_to = "code", values_to = "sel") %>%
      filter(sel == "Yes") %>%
      count(code) %>%
      mutate(label = recode(code,
                            q44_lack_money="Lack of money/too expensive",
                            q44_lack_transport="Lack of transportation",
                            q44_lack_time="Lack of time",
                            q44_fear_diagnosis="Fear of diagnosis/treatment",
                            q44_no_trust="Didn’t trust providers",
                            q44_self_treatment="Preferred self‐treatment",
                            q44_prefer_not_answer="Prefer not to answer",
                            q44_other="Other"
      )) %>%
      plot_ly(x = ~n, y = ~reorder(label, n), type = "bar", orientation = "h") %>%
      layout(xaxis = list(title = "Count"), yaxis = list(title = "Reason"))
  })
  
  output$tbl5 <- renderDT({
    req(input$var5); var <- input$var5; col <- resolve_mod5_col(var)
    if (is.na(col)) {
      return(datatable(
        data.frame(Note = sprintf("Column for '%s' not found (looked for aliases).",
                                  mod5_friendly[[var]] %||% var)),
        options = list(pageLength = 5, autoWidth = TRUE), rownames = FALSE
      ))
    }
    df_tcwp %>%
      filter(!is.na(.data[[col]])) %>%
      count(answer = .data[[col]], name = "Count") %>%
      arrange(desc(Count)) %>%
      datatable(options = list(pageLength = 10, autoWidth = TRUE),
                rownames = FALSE)
  })
  
  output$plt5 <- renderPlotly({
    req(input$var5)
    var <- input$var5; label <- mod5_friendly[[var]] %||% var; col <- resolve_mod5_col(var)
    if (is.na(col)) {
      return(plotly::plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = sprintf("Variable not found in this dataset: %s",
                                      label),
                      xaxis = list(visible = FALSE), 
                      yaxis = list(visible = FALSE), showlegend = FALSE))
    }
    cnt <- df_tcwp %>% filter(!is.na(.data[[col]])) %>% count(answer = .data[[col]], name = "n")
    plot_ly(cnt, x = ~answer, y = ~n, type = "bar") %>%
      layout(title = label, xaxis = list(title = label, tickangle = -45),
             yaxis = list(title = "Count"), margin = list(b = 100))
  })
  
  ## Panel 6: Intentions & Perspectives --------------------------------------
  
  output$mod6_choose_comm <- renderPlotly({
    df_tcwp %>%
      select(any_of(names(mod6_choose_labels))) %>%
      pivot_longer(everything(), names_to="code", values_to="sel") %>%
      filter(sel == "Yes") %>%
      count(code) %>%
      mutate(label = mod6_choose_labels[code]) %>%
      plot_ly(x=~label, y=~n, type="bar") %>%
      layout(xaxis = list(title="", tickangle = -45), 
             yaxis = list(title="Count"))
  })
  
  output$mod6_plan_leave <- renderPlotly({
    df_tcwp %>%
      filter(!is.na(q46_plan_leave_accom)) %>%
      count(q46_plan_leave_accom, name = "n") %>%
      plot_ly(x = ~q46_plan_leave_accom, y = ~n, type = "bar") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Count"))
  })
  
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
  
  output$mod6_stay_vs_move <- renderPlotly({
    df_tcwp %>%
      filter(!is.na(q48_plan_to_stay)) %>%
      count(q48_plan_to_stay, name = "n") %>%
      plot_ly(x = ~q48_plan_to_stay, y = ~n, type = "bar") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Count"))
  })
  
  output$mod6_stay_location <- renderPlotly({
    stay_pat <- "(?i)stay|մնալ|մնամ|կմնամ"
    raw <- df_tcwp %>%
      mutate(plan = repair_arm(as.character(q48_plan_to_stay))) %>%
      filter(stringr::str_detect(plan %||% "", stay_pat)) %>%
      transmute(place = as.character(q48_1_which_marz_city))
    if (!nrow(raw)) {
      return(plotly::plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "No answers for “If staying, which Marz/City?”"))
    }
    df <- raw %>%
      mutate(place = fix_double_utf8_mojibake(place)) %>%
      mutate(place = repair_arm(place)) %>%
      mutate(place = .rm_admin_words(place)) %>%
      tidyr::separate_rows(place,
                           sep = "\\s*[,;/\\|]+\\s*|\\s+և\\s+|\\s+եւ\\s+|\\s+and\\s+") %>%
      mutate(place = stringr::str_squish(place)) %>%
      filter(place != "") %>%
      mutate(marz = to_marz(place)) %>%
      mutate(marz = dplyr::coalesce(
        marz,
        dplyr::recode(place,
                      "Երևան"="Yerevan","Երեւան"="Yerevan","Erevan"="Yerevan","Yerevan"="Yerevan",
                      "Արարատ"="Ararat","Armavir"="Armavir","Արմավիր"="Armavir",
                      "Արագածոտն"="Aragatsotn","Կոտայք"="Kotayk","Գեղարքունիք"="Gegharkunik",
                      "Լոռի"="Lori","Շիրակ"="Shirak","Տավուշ"="Tavush",
                      "Վայոց ձոր"="Vayots Dzor","Վայոց Ձոր"="Vayots Dzor","Սյունիք"="Syunik",
                      .default = NA_character_)
      ))
    cnt <- df %>% mutate(marz = tidyr::replace_na(marz, "Other")) %>% count(marz, sort = TRUE)
    df_top <- cnt %>%
      mutate(label = ifelse(n <= 3 & marz != "Other", "Other (≤3)", marz)) %>%
      group_by(label) %>% summarise(n = sum(n), .groups = "drop") %>%
      arrange(n) %>% mutate(label = forcats::fct_reorder(label, n))
    plot_ly(df_top, x = ~n, y = ~label, type = "bar", orientation = "h",
            text = ~n, textposition = "outside",
            hovertemplate = "%{y}<br>Count: %{x}<extra></extra>") %>%
      layout(
        xaxis = list(title = "Count"),
        yaxis = list(title = "", tickfont = list(family = arm_font)),
        font  = list(family = arm_font),
        uniformtext = list(minsize = 10, mode = "hide"),
        margin = list(l = 220, r = 24, t = 10, b = 24)
      )
  })
  
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
  
  output$mod6_dest_country <- renderPlotly({
    df_tcwp %>%
      filter(!is.na(q50_country_go)) %>%
      count(q50_country_go, name = "n") %>%
      plot_ly(x = ~q50_country_go, y = ~n, type = "bar") %>%
      layout(xaxis = list(title = "Country"), yaxis = list(title = "Count"))
  })
  
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
  
  output$mod6_missing_docs <- renderPlotly({
    df_tcwp %>%
      filter(!is.na(q52_missing_doc)) %>%
      count(q52_missing_doc, name = "n") %>%
      plot_ly(x = ~q52_missing_doc, y = ~n, type = "bar") %>%
      layout(xaxis = list(title = "Missing documentation?"), 
             yaxis = list(title = "Count"))
  })
  
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
  
  ## Panel 7: Socio-Economic Status ------------------------------------------
  
  output$tbl7 <- renderDT({
    req(input$var7); var <- input$var7
    if (var %in% c("q56_rooms", "q58_amount")) {
      df_tcwp %>% select(Value = .data[[var]]) %>% filter(!is.na(Value)) %>%
        datatable(options = list(pageLength = 10, autoWidth = TRUE))
    } else {
      df_tcwp %>% filter(!is.na(.data[[var]])) %>%
        count(Value = .data[[var]], name = "Count") %>%
        arrange(desc(Count)) %>%
        datatable(options = list(pageLength = 10, autoWidth = TRUE))
    }
  })
  
  output$plt7 <- renderPlot({
    req(input$var7); var <- input$var7; label <- mod7_friendly[[var]]
    if (var %in% c("q56_rooms", "q58_amount")) {
      ggplot(df_tcwp, aes(x = .data[[var]])) +
        geom_histogram(bins = 10, na.rm = TRUE) +
        labs(x = label, y = "Count", title = paste("Distribution of", label)) +
        theme_minimal(base_size = 14)
    } else {
      df_tcwp %>%
        filter(!is.na(.data[[var]])) %>%
        count(Value = .data[[var]], name = "n") %>%
        ggplot(aes(x = Value, y = n)) +
        geom_col() +
        labs(x = label, y = "Count", title = label) +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  ## Panel 8: Life Events & Trauma -------------------------------------------
  
  output$tbl8 <- renderDT({
    req(input$var8)
    var_in  <- input$var8
    label   <- mod8_friendly[[var_in]] %||% var_in
    var_col <- resolve_mod8_col(var_in, df = mod8_enriched())
    mod8_enriched() |>
      dplyr::transmute(v = as.character(.data[[var_col]])) |>
      tidyr::replace_na(list(v = "Missing")) |>
      dplyr::count(v, name = "Count") |>
      dplyr::rename(!!label := v) |>
      datatable(options = list(pageLength = 10, autoWidth = TRUE), 
                rownames = FALSE)
  })
  
  output$plt8 <- renderPlot({
    req(input$var8)
    var_in  <- input$var8
    label   <- mod8_friendly[[var_in]] %||% var_in
    var_col <- resolve_mod8_col(var_in, df = mod8_enriched())
    mod8_enriched() |>
      dplyr::mutate(category = as.character(.data[[var_col]]),
                    category = tidyr::replace_na(category, "Missing")) |>
      dplyr::count(category, name = "n") |>
      ggplot(aes(x = category, y = n)) +
      geom_col() +
      labs(x = label, y = "Count", title = paste("Distribution of", label)) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  ## Panel 9: Health Behaviors & Quality of Life ------------------------------
  
  resolve_col <- function(var, df = df_tcwp) {
    if (var %in% names(df)) return(var)
    if (var == "q94") {
      cols <- c("q93_4_pain", "q93_5_depressed")
      return(intersect(cols, names(df)))
    }
    grep(paste0("^", var, "(_|$)"), names(df), value = TRUE, ignore.case = TRUE)
  }
  
  matrix_vars  <- c("q82","q83","q90","q93","q94")
  numeric_vars <- c("q86","q87")
  
  output$tbl9 <- renderDT({
    req(input$var9); var <- input$var9; cols <- resolve_col(var)
    if (var %in% matrix_vars) {
      df_counts <- df_tcwp %>%
        select(all_of(cols)) %>%
        pivot_longer(everything(), names_to = "subq", values_to = "resp") %>%
        filter(!is.na(resp) & resp != "") %>%
        mutate(Question = mod9_friendly[[var]],
               Source = mod9_matrix_labels[[var]][subq]) %>%
        count(Question, Source, resp, name = "n") %>%
        pivot_wider(names_from = resp, values_from = n, values_fill = 0) %>%
        select(Question, Source, everything())
    } else if (var %in% numeric_vars) {
      df_counts <- df_tcwp %>%
        mutate(raw = as.character(.data[[cols]])) %>%
        count(!!mod9_friendly[[var]] := raw, name = "Count")
    } else {
      df_counts <- df_tcwp %>%
        filter(!is.na(.data[[cols]])) %>%
        count(!!mod9_friendly[[var]] := .data[[cols]], name = "Count")
    }
    datatable(df_counts, colnames = unname(make.names(names(df_counts),
                                                      unique = TRUE)),
              options  = list(pageLength = 10, autoWidth = TRUE))
  })
  
  output$plt9 <- renderPlot({
    req(input$var9); var <- input$var9; lab <- mod9_friendly[[var]]; cols <- resolve_col(var)
    if (var %in% matrix_vars) {
      df_tcwp %>%
        select(all_of(cols)) %>%
        pivot_longer(everything(), names_to = "subq", values_to = "resp") %>%
        filter(!is.na(resp) & resp != "") %>%
        mutate(Source = mod9_matrix_labels[[var]][subq]) %>%
        ggplot(aes(x = resp, fill = Source)) +
        geom_bar(position = "dodge") +
        labs(x = "Response", y = "Count", title = lab) +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (var %in% numeric_vars) {
      df_plot <- df_tcwp %>%
        mutate(raw = as.character(.data[[cols]]),
               num = suppressWarnings(as.numeric(raw)))
      non_num <- sum(is.na(df_plot$num) & df_plot$raw != "")
      ggplot(df_plot %>% filter(!is.na(num)), aes(x = num)) +
        geom_histogram(bins = 15) +
        labs(x = lab, y = "Count", title = lab,
             subtitle = paste0(non_num, " non-numeric responses")) +
        theme_minimal(base_size = 14)
    } else {
      df_tcwp %>%
        filter(!is.na(.data[[cols]])) %>%
        count(answer = .data[[cols]], name = "n") %>%
        ggplot(aes(x = answer, y = n)) +
        geom_col() +
        labs(x = lab, y = "Count", title = lab) +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
}

# 6. Launch ------------------------------------------------------------------
shinyApp(ui, server)
