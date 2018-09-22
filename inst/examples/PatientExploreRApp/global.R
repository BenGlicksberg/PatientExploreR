library(PatientExploreR)
library(DBI)
library(odbc)
library(RMySQL)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyalert)
library(shinythemes)
library(plotly)
library(timevis)
library(stringr)
library(dplyr)


options("currentPath" = paste0(getwd(),'/'))

### this will not be in the main package as wd will be set by button
#options("currentPath" = "/srv/shiny-server/patientexplorer/")
#options("currentPath" = "/Users/bglicksberg/Desktop/Ben Glicksberg/Butte Lab/Projects/PatientExploreR/PatientExploreR/OMOP/PatientExploreR_Sandbox/")

