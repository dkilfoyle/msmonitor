insertPatientsUI <- function() {
  sidebarLayout(
    sidebarPanel(
      titledPanel("Filter Patients",
        textInput("ptsSearchNHI", "NHI", "", placeholder = "Leave blank to search all"),
        radioButtons(
          "ptsSearchJCV",
          "JCV Status",
          choices = c(
            "All",
            "Pos",
            "Neg"
          )
        ),
        radioButtons(
          "ptsSearchDrug",
          "Drug",
          choices = c(
            "All",
            "Natalizumab"
          )
        ))
    ), # end sidebarPanel
    mainPanel(
      bsCollapse(
        bsCollapsePanel(
          "Selected Patient",
          fluidRow(
            column(
              width = 4,
              textInput("ptsNHI", "NHI", ""),
              textInput("ptsSurname", "Surname", ""),
              textInput("ptsFirstName", "First Name", "")
            ),
            column(
              width = 4,
              selectInput("ptsDrug","Drug",choices=""),
              dateInput("ptsDateStarted", "Date Started", ""),
              radioButtons("ptsJCV", "JCV Status", choices=c("Pos","Neg"), inline=T)
            ),
            column(
              width = 4,
              #action buttons
              div(
                class = "btn-group-vertical",
                role = "group",
                style = "width:100%; margin-top: 20px", #TODO proper vertical alignment
                actionButton("ptsSave", "Save"),
                actionButton("ptsNew", "New"),
                actionButton("ptsDelete", "Delete"),
                actionButton("ptsGenerateInitiationEvents","Generate Initiation Events"),
                actionButton("ptsNewEvent","Add New Event")
              )
            )
          )
        ),
        id = "ptsCollapse",
        multiple = T
      ),
      DT::dataTableOutput("ptsTable")
    ) # end main panel
  ) # end sidebarlayout
}