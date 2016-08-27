insertSetupUI <- function() {
  sidebarLayout(
    sidebarPanel(
      #action buttons
      actionButton("drugsSave", "Save"),
      actionButton("drugsNew", "New"),
      actionButton("drugsDelete", "Delete")
    ),
    mainPanel(
      helpText("Recommended interval in months for each test"),
      # rHandsontableOutput("drugsTable")
      jsoneditOutput("drugsList")
    )
  )
}