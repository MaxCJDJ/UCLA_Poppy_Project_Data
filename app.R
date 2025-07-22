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

