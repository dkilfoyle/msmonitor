library(shiny)
library(rhandsontable)
library(dplyr)
library(lubridate)
library(stringr)
library(shinyBS)
library(timelinevis)

textareaInput <-
  function(id,
    label,
    value = "",
    rows = 5,
    cols = 20,
    class = "form-control") {
    tags$div(
      class = "form-group shiny-input-container",
      tags$label('for' = id, label),
      tags$textarea(
        id = id,
        class = class,
        rows = rows,
        cols = cols,
        value
      )
    )
  }

hiddenTextInput = function (inputId,
  label,
  value = "",
  width = NULL,
  placeholder = NULL)
{
  tags$input(
    id = inputId,
    type = "text",
    class = "form-control",
    value = value,
    placeholder = placeholder,
    style = "display:none;"
  )
}

headerPanel = function (title, ...) {
  div(class="header-panel", div(class="panel panel-default",
    div(class="panel-heading",
      h3(title, class="panel-title")
    ),
    div(class="panel-body", ...)
  ))
}

ui <- shinyUI(navbarPage(
  "MS Monitoring Program",
  
  tabPanel("Events",
    sidebarLayout(
      sidebarPanel(
        includeCSS("www/msmonitor.css"),
        headerPanel("Filter Events",
          textInput("evtsSearchNHI", "NHI", placeholder = "Leave blank to search all"),
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
      mainPanel(tabsetPanel(
        tabPanel(
          "Timeline",

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
                  dateInput("evtsStartDate", "Due Date", ""),
                  dateInput("evtsCompleted", "Date Completed", ""),
                  textareaInput("evtsResult", "Result")
                ),
                column(width=4,
                  headerPanel("Patient Info",
                    uiOutput("evtsInfo")
                  ),
                  div(
                    class = "btn-group-vertical",
                    role = "group",
                    style = "width:100%",
                    actionButton("evtsCompletedButton", "Mark as Completed"),
                    actionButton("evtsGenerateNew", "Generate new event"),
                    actionButton("evtsSaveChanges", "Save Changes")
                  )
                ))
            ),
            id = "evtsCollapse",
            multiple = T
          ),
          uiOutput("evtsFilterMessage"),
          timelinevisOutput("evtsTimeline")
        ), # end timeline tabPanel
        tabPanel("Table", rHandsontableOutput("evtsTable"))
      )) # end mainpanel
    )), # end tabpanel Events
  tabPanel(
    "Patients",
    DT::dataTableOutput("ptsTable"),
    tags$hr(),
    
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
  ), # end tabpanel patients
  tabPanel("Setup")
))