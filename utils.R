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

titledPanel = function (title, ...) {
  div(class="header-panel", div(class="panel panel-default",
    div(class="panel-heading",
      h3(title, class="panel-title")
    ),
    div(class="panel-body", ...)
  ))
}