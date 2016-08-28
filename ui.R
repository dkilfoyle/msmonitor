library(shiny)
library(shinyBS)
library(shinyjs)

source("utils.R")
source("events.R")
source("patients.R")
source("drugs.R")

ui <- shinyUI(
  navbarPage("MS Monitoring Program",
    
    tabPanel("Info",
      useShinyjs(),
      wellPanel(includeMarkdown("info.md"))),
    
    tabPanel("Events",
      insertEventsUI()),
      
    tabPanel("Patients",
      insertPatientsUI()),
  
    tabPanel("Drugs",
      insertDrugsUI()),
  
    id="mainTabPanel",
    selected="Events"
))
