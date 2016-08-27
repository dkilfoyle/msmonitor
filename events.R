insertEventsUI = function() {
  sidebarLayout(
    sidebarPanel(
      includeCSS("www/msmonitor.css"),
      tags$script(
        HTML(
          '
          Shiny.addCustomMessageHandler("jsCode",
          function(message) {
          eval(message.value);
          }
          );'
        )
        ),
      titledPanel(
        "Filter Events",
        textButtonInput(
          "evtsSearchNHI",
          "NHI",
          "evtsClearSearchButton",
          "Clear",
          placeholder = "Leave blank to search all"
        ),
        radioButtons(
          "evtsFilterTimeframe",
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
        ),
        radioButtons(
          "evtsFilterType",
          "Event Type",
          choices = c("All",
            "MRI",
            "JCV",
            "MSTAC",
            "Followup")
        )
      )
      ),
    # end sidebarPanel
    mainPanel(
      bsCollapse(
        bsCollapsePanel("Selected Event",
          conditionalPanel(condition="input.evtsNHI ==''",
            wellPanel(
              p("Find events using the search bar to the left. Then select an event from the timeline."),
              p("OR: Select a patient from the Patients tab and generate a new event from for the patient."))),
          conditionalPanel(condition="input.evtsNHI != ''", 
          fluidRow(
            column(
              width = 4,
              hiddenTextInput("evtsId", "Id", value = -1),
              selectInput(
                "evtsType",
                "Type",
                choices = c("LFT", "MRI", "JCV", "FBC", "MSTAC"),
                selected = NULL
              ),
              textButtonInput("evtsNumber", "Number", "evtsNumberCalc", "Calc", value =
                  ""),
              textAreaInput("evtsComment", "Comment", value = "")
            ),
            column(
              width = 4,
              dkdateInput("evtsDueDate", "Due Date", NA),
              dkdateInput("evtsCompleted", "Date Completed", NA),
              textAreaInput("evtsResult", "Result", value = "")
            ),
            column(
              width = 4,
              hiddenTextInput("evtsNHI", "NHI", ""),
              titledPanel("Patient Info",
                uiOutput("evtsInfo")),
              div(
                class = "btn-group-vertical",
                role = "group",
                style = "width:100%",
                actionButton("evtsCompleteButton", "Mark as Completed"),
                actionButton("evtsRepeatButton", "Repeat Event"),
                actionButton("evtsNewButton", "New Blank Event"),
                actionButton("evtsDeleteButton", "Delete Event"),
                disabled(actionButton("evtsSaveButton", "Save Changes"))
              )
            )
          ))),
        id = "evtsCollapse",
        multiple = T
      ),
      tabsetPanel(
        tabPanel(
          "Timeline",
          uiOutput("evtsFilterMessage"),
          timelinevisOutput("evtsTimeline")
        ),
        # end timeline tabPanel
        tabPanel(
          "Table",
          # rHandsontableOutput("evtsTable"))
          div(class = "top-gap"),
          DT::dataTableOutput("evtsTable")
        ),
        id = "evtsViewerTabset"
      )
    ) # end mainpanel
  ) # end sidebarlayout Events
}

