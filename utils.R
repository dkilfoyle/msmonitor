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


# ?? not needed now - cant remember why this needed for blank dates
# js_string = '$("#evtsDueDate input").eq(0).val("").datepicker("update"); $("#evtsCompleted input").eq(0).val("").datepicker("update");'
# session$sendCustomMessage(type='jsCode', list(value = js_string))