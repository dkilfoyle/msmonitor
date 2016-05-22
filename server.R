library(shiny)

server <- shinyServer(function(input, output, session) {
  
  values = reactiveValues()
  
  getEvents = reactive({
    if (is.null(values[["msevents"]])) {
      DF = read.csv("events.csv", stringsAsFactors = F)
      DF$DueDate = ymd(DF$DueDate)
      DF$Completed = ymd(DF$Completed)
      DF$Type = as.factor(DF$Type)
    }
    else
      DF = values[["msevents"]]
    
    values[["msevents"]] = DF
    DF
  })
  
  getFilteredEvents = reactive({
    items = getEvents()
    
    nhi = toupper(input$evtsSearchNHI)
    if (str_length(nhi) == 7) {
      items = items %>%
        filter(NHI == nhi)
    }
    
    if (input$evtsTimeframe == "All") {
      return(items)
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
    
    items
  })
  
  editEvent = function(id) {
    cat("editing ", id, "\n")
    updateCollapse(session, "evtsCollapse", open = "Selected Event")
    se = getEvents()[getEvents()$EventId == id,]
    updateTextInput(session, "evtsId", value = id)
    updateTextInput(session, "evtsDueDate", value = se$DueDate)
    updateTextInput(session, "evtsType", value = se$Type)
    updateTextInput(session, "evtsNumber", value = se$Number)
    updateTextInput(session, "evtsResult", value = se$Result)
    updateTextInput(session, "evtsComment", value = se$Comment)
    updateTextInput(session, "evtsCompleted", value = se$Completed)
    updateTextInput(session, "evtsNHI", value=se$NHI)
    # output$evtsInfo = renderUI(tagList(h3(se$NHI),p(values$mspts$Surname[values$mspts$NHI==se$NHI])))
  }
  
  # detect event selection from timeline
  observe({
    x = input$tlSelectEvent
    if (is.null(x))
      return()
    if (x$id == "evtsTimeline") {
      editEvent(x$items$id)
    }
  })
  
  # detect event selection from DT table
  observe({
    if (length(input$evtsTable_rows_selected) > 0) {
      id = getFilteredEvents()[input$evtsTable_rows_selected, "EventId"]
      editEvent(id)
    }
  })
  
  observeEvent(input$evtsCompleteButton, {
    updateDateInput(session, "evtsCompleted", value = today())
  })
  
  observeEvent(input$evtsNewButton, {
    # TODO Check unsaved
    newid = max(values$msevents$EventId)+1
    values$msevents = rbind(values$msevents, data.frame(EventId = newid, NHI="", Type="", Number=1, DueDate=ymd(""), Completed=ymd(""), Result="", Comment=""))
    updateRadioButtons(session, "evtsTimeframe", selected="All")
    editEvent(newid)
  })
  
  observeEvent(input$evtsRepeatButton, {
    # TODO Check unsaved
    newid = max(values$msevents$EventId)+1
    newduedate = input$evtsDueDate + months(3)
    xx = data.frame(EventId = newid, NHI=input$evtsNHI, Type=input$evtsType, Number=as.numeric(input$evtsNumber)+1, DueDate=newduedate, Completed=ymd(""), Result="", Comment="")
    values$msevents = rbind(values$msevents, xx) 
    updateRadioButtons(session, "evtsTimeframe", selected="All")
    editEvent(newid)
  })

  observeEvent(input$evtsSaveButton, {
    saveRow = which(values$msevents$EventId == input$evtsId)
    values$msevents[saveRow, "DueDate"] = input$evtsDueDate
    values$msevents[saveRow, "Type"] = input$evtsType
    values$msevents[saveRow, "Number"] = input$evtsNumber
    values$msevents[saveRow, "Result"] = input$evtsResult
    values$msevents[saveRow, "Completed"] = input$evtsCompleted
    values$msevents[saveRow, "Comment"] = input$evtsComment
    values$msevents[saveRow, "NHI"] = input$evtsNHI
  })
  
  observe({
    x = input$tlMoveEvent
    updateDateInput(session, "evtsDueDate", value = x$item$start)
  })
  
  output$evtsTimeline <- renderTimelinevis({
    
    items = getFilteredEvents() %>% 
      mutate(
        content = Type,
        start = DueDate,
        group = NHI,
        id = EventId
      )
    
    if (nrow(items)==0) {
      output$evtsFilterMessage = renderUI(div(class="alert alert-warning top-gap", h4("No Events Found")))
      return(NULL)
    }
    else
      output$evtsFilterMessage = renderUI("")
    
    groups = data.frame(
      id = unique(items$NHI),
      content = unique(items$NHI),
      title = getPatients()$Surname[getPatients()$NHI == unique(items$NHI)]
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
  
  # output$evtsTable <- renderRHandsontable({
  #   rhandsontable(getFilteredEvents(), rowHeaders=NULL)
  # })
  
  output$evtsTable = DT::renderDataTable(
    getFilteredEvents(),
    options = list(
      lengthChange = F,
      order = list(list(1, "asc")),
      paging = F,
      info = F
    ),
    selection = "single",
    class = 'cell-border stripe'
  )
  
  
  #################################################
  ## Patients
  #################################################
  
  getPatients = reactive({
    if (is.null(values[["mspts"]])) {
        DF = read.csv("patients.csv", stringsAsFactors = F)
        DF$DateStarted = ymd(DF$DateStarted)
      }
      else
        DF = values[["mspts"]]
    
    values[["mspts"]] = DF
    DF
  })
  
  getFilteredPatients = reactive({
    pts = getPatients()
    nhi = toupper(input$ptsSearchNHI)
    if (str_length(nhi) == 7) {
      pts = pts %>%
        filter(NHI == nhi)
    }
    pts
  })
  
  observe({
    if (length(input$ptsTable_rows_selected) > 0) {
      updateCollapse(session, "ptsCollapse", open = "Selected Patient")
      selrow = getFilteredPatients()[input$ptsTable_rows_selected,]
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
    saveRow = which(getPatients()$NHI == input$ptsNHI)
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
  
  output$ptsTable = DT::renderDataTable(
    getFilteredPatients(),
    options = list(
      lengthChange = F,
      order = list(list(1, "asc")),
      paging = F,
      info = F
    ),
    selection = "single",
    class = 'cell-border stripe'
  )
  
})

# observeEvent(input$evtsTableSaveButton,{
#   cat(str(input$evtsTable))
#   if (!is.null(input$evtsTable)) {
#     DF = values[["msevents"]]
#     DF2 = hot_to_r(input$evtsTable)
#     for (i in 1:nrow(DF2)) {
#       DF[DF$EventId == DF2$EventId[i],] = DF2[i, ]
#     }
#     values$msevents=DF
#   }
# })