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

textButtonInput = function(inputid, inputlabel, buttonid, buttonlabel, value="", width=NULL, placeholder=NULL) {
  div(class="form-group shiny-input-container",
    tags$label(`for`=inputid, inputlabel),
    div(class="input-group", style="width: 18em;",
      tags$input(id=inputid, type="text", class="form-control", value=value, placeholder=placeholder),
      span(class="input-group-btn",
        actionButton(buttonid, buttonlabel))
    )
  )
}

titledPanel = function (title, ...) {
  div(class="header-panel", div(class="panel panel-default",
    div(class="panel-heading",
      h3(title, class="panel-title")
    ),
    div(class="panel-body", ...)
  ))
}

library(htmltools)

dkdateInput <- function(inputId, label, value = NULL, min = NULL, max = NULL,
  format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en",
  width = NULL) {
  
  # If value is a date object, convert it to a string with yyyy-mm-dd format
  # Same for min and max
  if (inherits(value, "Date"))  value <- format(value, "%Y-%m-%d")
  if (inherits(min,   "Date"))  min   <- format(min,   "%Y-%m-%d")
  if (inherits(max,   "Date"))  max   <- format(max,   "%Y-%m-%d")
  
  value <- restoreInput(id = inputId, default = value)
  
  attachDependencies(
    tags$div(id = inputId,
      class = "shiny-date-input form-group shiny-input-container",
      style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
      
      controlLabel(inputId, label),
      tags$input(type = "text",
        # datepicker class necessary for dropdown to display correctly
        class = "form-control datepicker",
        `data-date-language` = language,
        `data-date-weekstart` = weekstart,
        `data-date-format` = format,
        `data-date-start-view` = startview,
        `data-min-date` = min,
        `data-max-date` = max,
        `data-date-autoclose` = T,
        `data-date-clear-btn` = T,
        `data-initial-date` = value
      )
    ),
    dkdatePickerDependency
  )
}
controlLabel <- function(controlName, label) {
  label %AND% tags$label(class = "control-label", `for` = controlName, label)
}

`%AND%` <- function(x, y) {
  if (!is.null(x) && !is.na(x))
    if (!is.null(y) && !is.na(y))
      return(y)
  return(NULL)
}

dkdatePickerDependency <- htmlDependency(
  "bootstrap-datepicker", "1.6.4", c(href = "datepicker"),
  script = "js/bootstrap-datepicker.js",
  stylesheet = "css/bootstrap-datepicker.css")


# ?? not needed now - cant remember why this needed for blank dates
# js_string = '$("#evtsDueDate input").eq(0).val("").datepicker("update"); $("#evtsCompleted input").eq(0).val("").datepicker("update");'
# session$sendCustomMessage(type='jsCode', list(value = js_string))