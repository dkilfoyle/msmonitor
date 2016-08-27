library(shiny)
library(DT)
library(yaml)
library(shinyjs)
library(sweetalertR)

server <- shinyServer(function(input, output, session) {
  
  values = reactiveValues()
  
  getEvents = reactive({
    if (is.null(values[["msevents"]])) {
      DF = read.csv("data/events.csv", stringsAsFactors = F)
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
    
    if (input$evtsFilterTimeframe != "All") {
      endDate = switch(
        input$evtsFilterTimeframe,
        "All Pending" = ymd("2100/01/01"),
        "This week" = today() + weeks(1),
        "Next 6 weeks" = today() + weeks(6),
        "Next 3 months" = today() + months(3),
        ymd("2100/01/01")
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
  
  observeEvent(input$evtsNumberCalc, {
    x = getEvents() %>% filter(NHI == input$evtsNHI, Type == input$evtsType)
    req(x$Number)
    updateTextInput(session, "evtsNumber", value=(max(x$Number,na.rm=T) + 1))
  })
  
  editEvent = function(id) {
    updateCollapse(session, "evtsCollapse", open = "Selected Event")
    se = getEvents() %>% filter(EventId==id)
    updateTextInput(session, "evtsId", value = id)
    updateTextInput(session, "evtsDueDate", value = se$DueDate)
    updateTextInput(session, "evtsType", value = se$Type)
    updateTextInput(session, "evtsNumber", value = se$Number)
    updateTextInput(session, "evtsResult", value = se$Result)
    updateTextInput(session, "evtsComment", value = se$Comment)
    updateTextInput(session, "evtsCompleted", value = se$Completed)
    updateTextInput(session, "evtsNHI", value=se$NHI)
    updateTextInput(session, "evtsSaveEnabled", value="NoStart") # prevent the updates above triggering a save enable
  }
  
  # update the NHI sticker
  observe({
    output$evtsInfo = renderUI(tagList(
      h3(input$evtsNHI, style="margin-top:0px"),
      p(values$mspts$Surname[values$mspts$NHI==input$evtsNHI],style="margin-bottom:0px")
    ))
  })
  
  observe({
    x = input$tlMoveEvent
    req(x$item$start)
    # min and max hack for bug introduced in shiny
    updateDateInput(session, "evtsDueDate", value = x$item$start, min="2000-01-01", max="2100-01-01")
  })
  
  # detect event selection from timeline
  observe({
    # TODO Check Unsaved - save changes first?
    isolate({
      if (input$evtsSaveEnabled == "Yes")
        sweetalert()
    })
    
    x = input$tlSelectEvent
    req(x$items$id)
    if (x$id == "evtsTimeline") {
      editEvent(x$items$id)
    }
  })
  
  # detect event selection from DT table
  observe({
    # TODO Check Unsaved
    
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
  
  newEvent = function(NHI="", Type="", Number=1, DueDate=ymd(""), Completed=ymd(""), Result="", Comment="") {
    newid = max(values$msevents$EventId)+1
    values$msevents = rbind(values$msevents, data.frame(EventId = newid, NHI=NHI, Type=Type, Number=Number, DueDate=DueDate, Completed=Completed, Result=Result, Comment=Comment))
    return(newid)
  }
  
  blankEvent = function(NHI="", Type="", Number=1, DueDate=ymd(""), Completed=ymd(""), Result="", Comment="") {
    # cat("blank event\n")
    updateTextInput(session, "evtsId", value = -1)
    updateTextInput(session, "evtsType", value = Type)
    updateTextInput(session, "evtsNumber", value = Number)
    updateTextInput(session, "evtsResult", value = Result)
    updateTextInput(session, "evtsComment", value = Comment)
    updateTextInput(session, "evtsNHI", value=NHI)
    updateTextInput(session, "evtsDueDate", value="")
    updateTextInput(session, "evtsSaveEnabled", value="NoStart")
    js_string = '$("#evtsDueDate input").eq(0).val("").datepicker("update"); $("#evtsCompleted input").eq(0).val("").datepicker("update");'
    session$sendCustomMessage(type='jsCode', list(value = js_string))
  }
  
  observeEvent(input$evtsNewButton, {
    # TODO Checked unsaved
    
    isolate({
      if (input$evtsSaveEnabled == "Yes")
        sweetalert()
    })
    
    # updateRadioButtons(session, "evtsFilterTimeframe", selected="All")
    req(input$evtsNHI)
    updateTextInput(session, "evtsSearchNHI", input$evtsNHI)
    blankEvent(NHI=input$evtsNHI)
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
      Completed=ymd(""),
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
  
  # observe unsaved changes
  observe({
    input$evtsComment
    input$evtsCompleted
    input$evtsDueDate
    input$evtsNumber
    input$evtsResult
    input$evtsType
    
    isolate({
      if (input$evtsSaveEnabled=="NoStart")
        updateTextInput(session, "evtsSaveEnabled", value="No")
      else
        updateTextInput(session, "evtsSaveEnabled", value="Yes")
    })
  })
  
  # observe change in unsaved change status
  observe({
    # cat(input$evtsSaveEnabled,"\n")
    if (input$evtsSaveEnabled == "Yes")
      enable("evtsSaveButton")
    else
      disable("evtsSaveButton")
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
    
    updateTextInput(session, "evtsSaveEnabled", "No")
  })
  
  observeEvent(input$evtsDeleteButton, {
    showModal(modalDialog(
      title = "Delete Event",
      "Are you sure?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("evtsDeleteOK", "OK")
      )
    ))
  })
  
  observeEvent(input$evtsDeleteOK, {
    removeModal()
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
    
    if (input$ptsSearchJCV != "All") {
      pts = pts %>% 
        filter(JCVStatus == input$ptsSearchJCV)
    }
    
    if (input$ptsSearchDrug == "Natalizumab") {
      pts = pts %>% 
        filter(Drug == input$ptsSearchDrug)
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
      updateSelectInput(session, "ptsDrug", selected = selrow$Drug, choices = names(getDrugs()))
      updateDateInput(session, "ptsDateStarted", value = selrow$DateStarted)
      updateRadioButtons(session, "ptsJCV", selected=selrow$JCVStatus)
    }
  })
  
  observeEvent(input$ptsNew, {
    updateTextInput(session, "ptsNHI", value = "Enter details")
    updateTextInput(session, "ptsFirstName", value = "then")
    updateTextInput(session, "ptsSurname", value = "click save")
    updateSelectInput(session, "ptsDrug", selected = "", choices = names(getDrugs()))
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
  
  observeEvent(input$ptsNewEvent, {
    updateTabsetPanel(session, "mainTabPanel","Events")
    updateCollapse(session, "evtsCollapse", open = "Selected Event")
    blankEvent(NHI=input$ptsNHI)
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
  
  # output$ptsDrugsUI = renderUI({
  #   selectInput(
  #     "ptsDrug",
  #     "Drug",
  #     choices = names(getDrugs()), #$Name, #c("Tecfidera", "Natalizumab", "Fingolimod", "Interferon"),
  #     selected = ""
  #   )
  # })
  
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