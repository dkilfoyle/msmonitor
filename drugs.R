library(listviewer)

insertDrugsUI <- function() {
  sidebarLayout(
    sidebarPanel(
      actionButton("drugsSave", "Save"),
      actionButton("drugsNew", "New Drug"),
      actionButton("drugsDelete", "Delete")
    ),
    mainPanel(
      helpText("Recommended interval in months for each test"),
      jsoneditOutput("drugsList")
    )
  )
}