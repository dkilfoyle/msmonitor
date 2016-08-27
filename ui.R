library(shiny)
library(rhandsontable)
library(dplyr)
library(lubridate)
library(stringr)
library(shinyBS)
library(timelinevis)
library(listviewer)
library(shinyjs)

source("utils.R")
source("events.R")
source("patients.R")
source("setup.R")

ui <- shinyUI(
  navbarPage("MS Monitoring Program",
    
    tabPanel("Info",
      useShinyjs(),
      wellPanel(includeMarkdown("info.md"))),
    
    tabPanel("Events",
      insertEventsUI()),
      
    tabPanel("Patients",
      insertPatientsUI()),
  
    tabPanel("Setup",
      insertSetupUI()),
  
    id="mainTabPanel",
    selected="Events"
))