library(shiny)
library(DT)
library(yaml)
library(shinyjs)

server <- shinyServer(function(input, output, session) {
  
  values = reactiveValues()
  
  getEvents = reactive({
    if (is.null(values[["msevents"]])) {
      DF = read.csv("data/events.csv", stringsAsFactors = F)
      DF$DueDate = dmy(DF$DueDate)
      DF$Completed = dmy(DF$Completed)
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
    
    if (input$evtsFilterTimeframe != "All") {
      endDate = switch(
        input$evtsFilterTimeframe,
        "All Pending" = dmy("01/01/2100"),
        "This week" = today() + weeks(1),
        "Next 6 weeks" = today() + weeks(6),
        "Next 3 months" = today() + months(3),
        dmy("01/01/2100")
      )
      endDate = as_date(endDate)
      
      if (input$evtsFilterTimeframe == "Overdue") {
        items = items %>%
          filter(DueDate < today(), is.na(Completed))
      } else if (input$evtsFilterTimeframe == "Completed") {
        items = items %>%
          filter(!is.na(Completed))
      } else
      {
        items = items %>%
          filter(DueDate > today(), DueDate < endDate, is.na(Completed))
      }
    }
    
    if (input$evtsFilterType != "All") {
      items = items %>% 
        filter(Type == input$evtsFilterType)
    }
    
    items
  })
  
  editEvent = function(id) {
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
  
  observe({
    x = input$tlMoveEvent
    updateDateInput(session, "evtsDueDate", value = x$item$start)
  })
  
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
  
  observeEvent(input$evtsClearSearchButton, {
    updateTextInput(session, "evtsSearchNHI", value="")
  })
  
  observeEvent(input$evtsCompleteButton, {
    updateDateInput(session, "evtsCompleted", value = today())
  })
  
  newEvent = function(NHI="", Type="", Number=1, DueDate=dmy(""), Completed=dmy(""), Result="", Comment="") {
    newid = max(values$msevents$EventId)+1
    values$msevents = rbind(values$msevents, data.frame(EventId = newid, NHI=NHI, Type=Type, Number=Number, DueDate=DueDate, Completed=Completed, Result=Result, Comment=Comment))
    return(newid)
  }
  
  blankEvent = function(NHI="", Type="", Number=1, DueDate=dmy(""), Completed=dmy(""), Result="", Comment="") {
    updateTextInput(session, "evtsId", value = -1)
    updateTextInput(session, "evtsType", value = Type)
    updateTextInput(session, "evtsNumber", value = Number)
    updateTextInput(session, "evtsResult", value = Result)
    updateTextInput(session, "evtsComment", value = Comment)
    updateTextInput(session, "evtsNHI", value=NHI)
    updateTextInput(session, "evtsDueDate", value="")
    js_string = '$("#evtsDueDate input").eq(0).val("").datepicker("update"); $("#evtsCompleted input").eq(0).val("").datepicker("update");'
    session$sendCustomMessage(type='jsCode', list(value = js_string))
  }
  
  observeEvent(input$evtsNewButton, {
    # updateRadioButtons(session, "evtsFilterTimeframe", selected="All")
    blankEvent(NHI=input$evtsSearchNHI)
  })
  
  observeEvent(input$evtsRepeatButton, {
    # TODO Check unsaved
    
    pt = getPatients() %>% 
      filter(NHI == input$evtsNHI) # get the patient associated with this event
    drug = getDrugs() %>%  
      filter(Name == pt$Drug) # get the drug associated with this patient
    
    stop() # need to implement getDrugEventTime
    newduedate = input$evtsDueDate + months(drug[1, input$evtsType])
    
    newid = newEvent(
      NHI=input$evtsNHI,
      Type=input$evtsType,
      Number=as.numeric(input$evtsNumber)+1,
      DueDate=newduedate,
      Completed=dmy(""),
      Result="", 
      Comment=""
    )
    
    updateTextInput(session, "evtsSearchNHI", value=input$evtsNHI)
    updateRadioButtons(session, "evtsFilterTimeframe", selected="All")
    updateTabsetPanel(session, "evtsViewerTabset", selected="Table")
    
    x=getEvents() %>% 
      filter(NHI==input$evtsNHI)
    selectRows(dataTableProxy("evtsTable"), selected=which(x$EventId == newid))
  })

  observeEvent(input$evtsSaveButton, {
    
    if (input$evtsId == -1) {
      newid = newEvent()
      saveRow = which(values$msevents$EventId == newid) # generate a new event id
    }
    else
      saveRow = which(values$msevents$EventId == input$evtsId)

    values$msevents[saveRow, "DueDate"] = input$evtsDueDate
    values$msevents[saveRow, "Type"] = input$evtsType
    values$msevents[saveRow, "Number"] = input$evtsNumber
    values$msevents[saveRow, "Result"] = input$evtsResult
    values$msevents[saveRow, "Completed"] = input$evtsCompleted
    values$msevents[saveRow, "Comment"] = input$evtsComment
    values$msevents[saveRow, "NHI"] = input$evtsNHI
    write.csv(getEvents(), file="data/events.csv", row.names=F)
  })
  
  observeEvent(input$evtsDeleteButton, {
    evts = getEvents()
    evts = evts[-which(evts$EventId == input$evtsId), ]
    values[["msevents"]] = evts
    write.csv(getEvents(), file="data/events.csv", row.names=F)
    blankEvent()
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
      title = getPatients()$Surname[getPatients()$NHI %in% unique(items$NHI)]
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
        DF = read.csv("data/patients.csv", stringsAsFactors = F)
        DF$DateStarted = dmy(DF$DateStarted)
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
      updateRadioButtons(session, "ptsJCV", selected=selrow$JCVStatus)
    }
  })
  
  observeEvent(input$ptsNew, {
    updateTextInput(session, "ptsNHI", value = "Enter details")
    updateTextInput(session, "ptsFirstName", value = "then")
    updateTextInput(session, "ptsSurname", value = "click save")
    updateSelectInput(session, "ptsDrug", selected = "")
    updateDateInput(session, "ptsDateStarted", value = "")
    updateRadioButtons(session, "ptsJCV", selected="Neg")
  })
  
  observeEvent(input$ptsDelete, {
    js_string = 'Shiny.onInputChange("ptsDeleteConfirm",confirm("This will also delete all events associated with this NHI! Are you sure?"));'
    session$sendCustomMessage(type='jsCode', list(value = js_string))
  })
  
  observeEvent(input$ptsDeleteConfirm, {
    if(input$ptsDeleteConfirm) {
      evts = getEvents()
      evts = evts[-which(evts$NHI == input$ptsNHI), ]
      values[["msevents"]] = evts
      pts = getPatients()
      pts = pts[-which(pts$NHI==input$ptsNHI), ]
      values[["mspts"]] = pts
    }
  })
  
  observeEvent(input$ptsSave, {
    saveRow = which(getPatients()$NHI == input$ptsNHI)
    newRow = list(
      NHI = input$ptsNHI,
      Surname = input$ptsSurname,
      FirstName = input$ptsFirstName,
      Drug = input$ptsDrug,
      DateStarted = input$ptsDateStarted,
      JCVStatus = input$ptsJCV
    )
    
    if (length(saveRow) == 0)
      # new entry
      values$mspts = rbind(values$mspts, newRow)
    else
      values$mspts[saveRow,] = newRow
    
    write.csv(getPatients(), file="data/patients.csv", row.names=F)
  })
  
  initiationEvent = function(type, emonths) {
    newEvent(
      NHI=input$ptsNHI,
      Type=type,
      Number=1,
      DueDate = today() %m+% months(emonths)
    )
  }
  
  observeEvent(input$ptsGenerateInitiationEvents, {
    # TODO: check is this pt saveed yet
    drug = getDrugs()[[input$ptsDrug]]
    cat(input$ptsJCV)
    if (!is.null(drug)) {
      sapply(names(drug$Initial), function (event) {
        if (event == "JCPos")
          if (input$ptsJCV == "Pos") {
              sapply(names(drug$Initial$JCPos), function(event2) { initiationEvent(event2, drug$Initial$JCPos[[event2]])})
          }
        else if (event == "JCNeg")
          if (input$ptsJCV == "Neg") {
            sapply(names(drug$Initial$JCNeg), function(event2) { initiationEvent(event2, drug$Initial$JCNeg[[event2]])})
          }
        else {
          initiationEvent(event, drug$Initial[[event]])
        }
      })
    }
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
  
  ##########################################################################
  ## Setup
  ##########################################################################
  
  
  getDrugs = reactive({
    # if (!is.null(input$drugsTable)) {
    #   DF = hot_to_r(input$drugsTable)
    #   values$msdrugs=DF
    # }
    # else if (is.null(values[["msdrugs"]])) {
    #   DF = read.csv("drugs.csv", stringsAsFactors = F)
    # }
    if (is.null(values[["msdrugs"]])) {
      DF = yaml.load_file("data/drugs.yml")
    }
    else
      DF = values[["msdrugs"]]
    
    values[["msdrugs"]] = DF
    DF
  })
  
  output$ptsDrugsUI = renderUI({
    selectInput(
      "ptsDrug",
      "Drug",
      choices = names(getDrugs()), #$Name, #c("Tecfidera", "Natalizumab", "Fingolimod", "Interferon"),
      selected = ""
    )
  })
  
  observeEvent(input$drugsSave, {
    
   stop("implement")
    
    # make jsonEditorChangeEvent to intput$drugsTable - see handonstbale for how
    # below will only work if there is a change
    
    save(as.yaml(input$jsonEditorChangeEvent), file="drugs2.yml")
    
    
    # write.csv(getDrugs(), file="drugs.csv", row.names=F)
  })
  
  # output$drugsTable <- renderRHandsontable({
  #   rhandsontable(getDrugs(), rowHeaders=NULL)
  # })
  
  output$drugsList = renderJsonedit({
    jsonedit(getDrugs(), "change" = htmlwidgets::JS('function(){
        Shiny.onInputChange("jsonEditorChangeEvent", 
          {id: event.currentTarget.parentNode.id, data:event.currentTarget.parentNode.editor.get()});}'))
  })
  
})