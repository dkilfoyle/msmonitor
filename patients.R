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
              width = 6,
              textInput("ptsNHI", "NHI", ""),
              textInput("ptsSurname", "Surname", ""),
              textInput("ptsFirstName", "First Name", "")
            ),
            column(
              width = 6,
              selectInput("ptsDrug","Drug",choices=""),
              dateInput("ptsDateStarted", "Date Started", ""),
              radioButtons("ptsJCV", "JCV Status", choices=c("Pos","Neg"), inline=T)
            )
          ),
          #action buttons
          actionButton("ptsSave", "Save"),
          actionButton("ptsNew", "New"),
          actionButton("ptsDelete", "Delete"),
          actionButton("ptsGenerateInitiationEvents","Generate Initiation Events"),
          actionButton("ptsNewEvent","Add New Event")
        ),
        id = "ptsCollapse",
        multiple = T
      ),
      DT::dataTableOutput("ptsTable")
    ) # end main panel
  ) # end sidebarlayout
}