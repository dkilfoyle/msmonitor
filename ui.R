library(shiny)
library(rhandsontable)
library(dplyr)
library(lubridate)
library(stringr)
library(shinyBS)
library(timelinevis)

source("utils.R")

ui <- shinyUI(navbarPage(
  "MS Monitoring Program",
  
  tabPanel("Events",
    sidebarLayout(
      sidebarPanel(
        includeCSS("www/msmonitor.css"),
        titledPanel("Filter Events",
          textButtonInput("evtsSearchNHI", "NHI", "evtsClearSearchButton", "Clear", placeholder = "Leave blank to search all"),
          radioButtons(
            "evtsTimeframe",
            "Timeframe",
            choices = c(
              "All Pending",
              "This week",
              "Next 6 weeks",
              "Next 3 months",
              "Overdue",
              "Completed",
              "All"
            )
          )
        )
      ), # end sidebarPanel
      mainPanel(
        bsCollapse(
          bsCollapsePanel(
            "Selected Event",
            fluidRow(
              column(width=4,
                hiddenTextInput("evtsId", "Id"),
                selectInput("evtsType", "Type", choices=c("LFT","MRI","JCV","FBC"), selected=NULL),
                textInput("evtsNumber", "Number", ""),
                textareaInput("evtsComment", "Comment")
              ),
              column(width=4,
                dateInput("evtsDueDate", "Due Date", ""),
                dateInput("evtsCompleted", "Date Completed", ""),
                textareaInput("evtsResult", "Result")
              ),
              column(width=4,
                textInput("evtsNHI", "NHI", ""),
                # titledPanel("Patient Info",
                #   uiOutput("evtsInfo")
                # ),
                div(
                  class = "btn-group-vertical",
                  role = "group",
                  style = "width:100%",
                  actionButton("evtsCompleteButton", "Mark as Completed"),
                  actionButton("evtsRepeatButton", "Repeat Event"),
                  actionButton("evtsNewButton", "New Blank Event"),
                  actionButton("evtsDeleteButton", "Delete Event"),
                  actionButton("evtsSaveButton", "Save Changes")
                )
              ))
          ),
          id = "evtsCollapse",
          multiple = T
        ),
        tabsetPanel(
          tabPanel("Timeline",
            uiOutput("evtsFilterMessage"),
            timelinevisOutput("evtsTimeline")
          ), # end timeline tabPanel
          tabPanel("Table", 
            # rHandsontableOutput("evtsTable"))
            div(class="top-gap"),
            DT::dataTableOutput("evtsTable")),
          id="evtsViewerTabset")) # end mainpanel
    )), # end tabpanel Events
  tabPanel(
    "Patients",
    sidebarLayout(
      sidebarPanel(
        titledPanel("Filter Patients",
          textInput("ptsSearchNHI", "NHI", "", placeholder = "Leave blank to search all"))
      ), # end sidebarPanel
      mainPanel(
        bsCollapse(
          bsCollapsePanel(
            "Selected Patient",
            fluidRow(
              column(
                width = 6,
                textInput("ptsNHI", "NHI", ""),
                textInput("ptsSurname", "Surname", ""),
                textInput("ptsFirstName", "First Name", "")
              ),
              column(
                width = 6,
                selectInput(
                  "ptsDrug",
                  "Drug",
                  choices = c("Tecfidera", "Natalizumab", "Fingolimod", "Interferon"),
                  selected = ""
                ),
                dateInput("ptsDateStarted", "Date Started", "")
              )
            ),
            #action buttons
            actionButton("ptsSave", "Save"),
            actionButton("ptsNew", "New"),
            actionButton("ptsDelete", "Delete")
          ),
          id = "ptsCollapse",
          multiple = T
        ),
        DT::dataTableOutput("ptsTable")
      ) # end main panel
    ) # end sidebarlayout
  ), # end tabpanel patients
  tabPanel("Setup",
    sidebarLayout(
      sidebarPanel(
        #action buttons
        actionButton("drugsSave", "Save"),
        actionButton("drugsNew", "New"),
        actionButton("drugsDelete", "Delete")
      ),
      mainPanel(
        helpText("Recommended interval in months for each test"),
        rHandsontableOutput("drugsTable")
      )
    ))
))