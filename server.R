library(shiny)

msevents = read.csv("events.csv", stringsAsFactors = F)
msevents$DueDate = ymd(msevents$DueDate)
msevents$Completed = ymd(msevents$Completed)

mspts = read.csv("patients.csv", stringsAsFactors = F)
mspts$DateStarted = ymd(mspts$DateStarted)

server <- shinyServer(function(input, output, session) {
  values = reactiveValues(msevents = msevents, mspts = mspts)
  
  # detect event selection
  observe({
    x = input$tlSelectEvent
    if (is.null(x))
      return()
    cat(str(x))
    if (x$id == "evtsTimeline") {
      updateCollapse(session,
        "evtsCollapse",
        open = "Selected Event",
        close = "Filter Events")
      se = values$msevents[values$msevents$EventId == x$items$id,]
      updateTextInput(session, "evtsId", value = x$items$id)
      updateTextInput(session, "evtsStartDate", value = se$DueDate)
      updateTextInput(session, "evtsType", value = se$Type)
      updateTextInput(session, "evtsNumber", value = se$Number)
      updateTextInput(session, "evtsResult", value = se$Result)
      updateTextInput(session, "evtsComment", value = se$Comment)
      updateTextInput(session, "evtsCompleted", value = se$Completed)
      output$evtsInfo = renderUI(tagList(h3(x$items$NHI),p(values$mspts$Surname[values$mspts$NHI==x$items$NHI])))
    }
  })
  
  observe({
    x = input$tlMoveEvent
    updateDateInput(session, "evtsStartDate", value = x$item$start)
  })
  
  observeEvent(input$evtsCompletedButton, {
    updateDateInput(session, "evtsCompleted", value = today())
  })
  
  observeEvent(input$evtsSaveChanges, {
    saveRow = which(values$msevents$EventId == input$evtsId)
    values$msevents[saveRow, "DueDate"] = input$evtsStartDate
    values$msevents[saveRow, "Type"] = input$evtsType
    values$msevents[saveRow, "Number"] = input$evtsNumber
    values$msevents[saveRow, "Result"] = input$evtsResult
    values$msevents[saveRow, "Completed"] = input$evtsCompleted
    values$msevents[saveRow, "Comment"] = input$evtsComment
  })
  
  getFilteredEvents = reactive({
    items = values$msevents
    
    nhi = toupper(input$evtsSearchNHI)
    if (str_length(nhi) == 7) {
      items = items %>%
        filter(NHI == nhi)
    }
    
    endDate = switch(
      input$evtsTimeframe,
      "All Pending" = ymd("2100/01/01"),
      "This week" = today() + weeks(1),
      "Next 6 weeks" = today() + weeks(6),
      "Next 3 months" = today() + months(3),
      ymd("2100/01/01")
    )
    endDate = as_date(endDate)
    
    if (input$evtsTimeframe == "Overdue") {
      items = items %>%
        filter(DueDate < today(), is.na(Completed))
    } else if (input$evtsTimeframe == "Completed") {
      items = items %>%
        filter(!is.na(Completed))
    } else
    {
      items = items %>%
        filter(DueDate > today(), DueDate < endDate, is.na(Completed))
    }
    
    items = items %>%
      mutate(
        content = Type,
        start = DueDate,
        group = NHI,
        id = EventId
      )
    
    items
  })
  
  
  output$evtsTimeline <- renderTimelinevis({
    items = getFilteredEvents()
    
    if (nrow(items)==0) {
      output$evtsFilterMessage = renderUI(h3("No Events Found"))
      return(NULL)
    }
    else
      output$evtsFilterMessage = renderUI("")
    
    groups = data.frame(
      id = unique(items$NHI),
      content = unique(items$NHI),
      title = values$mspts$Surname[values$mspts$NHI == unique(items$NHI)]
    )
    timelinevis(
      items,
      groups,
      id = "evtsTimeline",
      editable = list(
        add = F,
        updateGroup = F,
        updateTime = T,
        remove = F
      ),
      snap = NULL
    )
  })
  
  output$evtsTable <- renderRHandsontable({
    items = getFilteredEvents()
    rhandsontable(values$msevents[values$msevents$EventId == items$id,])
  })
  
  observeEvent(input$addpt, {
    newrow = data.frame(
      EventId = max(values$msevents$EventId) + 1,
      NHI = "ACJ2321",
      Type = "MRI",
      Number = 1,
      DueDate = dmy("1/23/2016"),
      Completed = F
    )
    values$msevents = rbind(values$msevents, newrow)
  })
  
  output$ptsTable = DT::renderDataTable(
    values$mspts,
    options = list(
      lengthChange = F,
      order = list(list(1, "asc")),
      paging = F,
      info = F
    ),
    selection = "single",
    class = 'cell-border stripe'
  )
  
  observe({
    if (length(input$ptsTable_rows_selected) > 0) {
      selrow = values$mspts[input$ptsTable_rows_selected,]
      updateTextInput(session, "ptsNHI", value = selrow$NHI)
      updateTextInput(session, "ptsFirstName", value = selrow$FirstName)
      updateTextInput(session, "ptsSurname", value = selrow$Surname)
      updateSelectInput(session, "ptsDrug", selected = selrow$Drug)
      updateDateInput(session, "ptsDateStarted", value = selrow$DateStarted)
    }
  })
  
  observeEvent(input$ptsNew, {
    updateTextInput(session, "ptsNHI", value = "Enter details")
    updateTextInput(session, "ptsFirstName", value = "then")
    updateTextInput(session, "ptsSurname", value = "click save")
    updateSelectInput(session, "ptsDrug", selected = "")
    updateDateInput(session, "ptsDateStarted", value = "")
  })
  
  observeEvent(input$ptsSave, {
    saveRow = which(values$mspts$NHI == input$ptsNHI)
    newRow = list(
      NHI = input$ptsNHI,
      Surname = input$ptsSurname,
      FirstName = input$ptsFirstName,
      Drug = input$ptsDrug,
      DateStarted = input$ptsDateStarted
    )
    
    if (length(saveRow) == 0)
      # new entry
      values$mspts = rbind(values$mspts, newRow)
    else
      values$mspts[saveRow,] = newRow
  })
  
})