library(shiny)
library(DT)
library(yaml)
library(shinyjs)
library(dplyr)
library(lubridate)
library(stringr)

server <- shinyServer(function(input, output, session) {
  
  cat("Shiny server starting...\n")
  
  values = reactiveValues()
  values$unsavedEventStatus = "No"
  demoVersion = T

  showAllEventsForPatient = function(NHI) {
    updateTabsetPanel(session, "mainTabPanel","Events")
    updateCollapse(session, "evtsCollapse", open = "Selected Event")
    updateTabsetPanel(session, "evtsViewerTabset", "Table")
    updateTextInput(session, "evtsSearchNHI", value=NHI)
    updateRadioButtons(session, "evtsFilterTimeframe", selected="All")
    updateRadioButtons(session, "evtsFilterType", selected="All")
  }
  
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
  
  # create a new event record in values, usually activated by save button
  createNewEvent = function(NHI="", Type="", Number=1, DueDate=ymd(""), Completed=ymd(""), Result="", Comment="") {
    newid = max(values$msevents$EventId)+1
    cat("Create new event: ",newid,"\n")
    values$msevents = rbind(values$msevents, data.frame(
      EventId = newid,
      NHI=NHI,
      Type=Type,
      Number=Number,
      DueDate=DueDate,
      Completed=Completed,
      Result=Result,
      Comment=Comment))
    return(newid)
  }
  
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
          filter(DueDate >= today(), DueDate < endDate, is.na(Completed))
      }
    }
    
    if (input$evtsFilterType != "All") {
      items = items %>% 
        filter(Type == input$evtsFilterType)
    }
    
    items
  })
  
  updateEventInputs = function(id, NHI, Type, Number, DueDate, Completed, Result, Comment) {
    # hidden inputs
    updateTextInput(session, "evtsId", value = id)
    updateTextInput(session, "evtsNHI", value=NHI)

    # visible inputs
    if (is.null(DueDate)) DueDate=NA
    if (is.null(Completed)) Completed=NA
    
    updateTextInput(session, "evtsDueDate", value = DueDate)
    updateTextInput(session, "evtsCompleted", value = Completed)
    
    updateTextInput(session, "evtsType", value = Type)
    updateTextInput(session, "evtsNumber", value = Number)
    updateTextInput(session, "evtsResult", value = Result)
    updateTextInput(session, "evtsComment", value = Comment)
    
    values$unsavedEventStatus = "Update"
  }
  
  # edit an existing event
  editEvent = function(id) {
    cat("Edit event: ",id, "\n")
    updateCollapse(session, "evtsCollapse", open = "Selected Event")
    se = getEvents() %>% filter(EventId==id)
    
    # ensure EventId is unique
    req(nrow(se)==1)
    
    updateEventInputs(id=id, 
      NHI=se$NHI, Type=se$Type, Number=se$Number, 
      DueDate=se$DueDate, Completed=se$Completed,
      Result=se$Result, Comment=se$Comment)
  }
  
  # edit an unsaved blank event
  editNewEvent = function(NHI="", Type="", Number=1, DueDate=ymd(""), Completed=ymd(""), Result="", Comment="") {
    cat("editNewEvent", NHI, "\n")
    updateEventInputs(id=-1, 
      NHI=NHI, Type=Type, Number=Number, 
      DueDate=DueDate, Completed=Completed,
      Result=Result, Comment=Comment)
  }
  
  # update the NHI sticker
  # observes input$evtsNHI and generates a sticker html in output$evtsInfo
  observe({
    output$evtsInfo = renderUI(tagList(
      h3(input$evtsNHI, style="margin-top:0px; margin-bottom:0px;"),
      p(paste0(values$mspts$Surname[values$mspts$NHI==input$evtsNHI],", ",
        values$mspts$FirstName[values$mspts$NHI==input$evtsNHI]),style="margin-bottom:0px"),
      p(values$mspts$Drug[values$mspts$NHI==input$evtsNHI],style="margin-bottom:0px")
    ))
  })
  
  # observe unsaved changes
  observe({
    input$evtsComment
    input$evtsCompleted
    input$evtsDueDate
    input$evtsNumber
    input$evtsResult
    input$evtsType
    
    # ignore non-user changes due to update function
    isolate({
      values$unsavedEventStatus = ifelse(values$unsavedEventStatus=="Update", "No", "Yes")
    })
  })
  
  # observe change in unsaved change status
  # observe values$unsavedEventStatus
  observe({
    if (values$unsavedEventStatus=="Yes"){
      enable("evtsSaveButton")
      updateActionButton(session, "evtsSaveButton", label="<b>Save Changes</b>")
    }
    else
    {
      disable("evtsSaveButton")
      updateActionButton(session, "evtsSaveButton", label="Save Changes")
    }
  })
  
  #############################################
  # Event UI Buttons
  #############################################
  
  observeEvent(input$evtsClearSearchButton, {
    updateTextInput(session, "evtsSearchNHI", value="")
  })
  
  observeEvent(input$evtsNumberCalc, {
    x = getEvents() %>% filter(NHI == input$evtsNHI, Type == input$evtsType)
    req(x$Number)
    updateTextInput(session, "evtsNumber", value=(max(x$Number,na.rm=T) + 1))
  })
  
  observeEvent(input$evtsCompleteButton, {
    updateDateInput(session, "evtsCompleted", value = today())
  })
  
  observeEvent(input$evtsNewButton, {
    if (values$unsavedEventStatus == "No")
      evtsNewButtonConfirmed()
    else
      showModal(modalDialog(
        title = "New Event",
        "You have unsaved changes to the current event. Are you sure?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("evtsNewOK", "OK")
        )
      ))
  })
  
  observeEvent(input$evtsNewOK, {
    removeModal()
    evtsNewButtonConfirmed()
  })
    
  evtsNewButtonConfirmed = function() {
    req(input$evtsNHI)
    updateTextInput(session, "evtsSearchNHI", input$evtsNHI)
    updateRadioButtons(session, "evtsFilterTimeframe", selected="All")
    updateRadioButtons(session, "evtsFilterType", selected="All")
    editNewEvent(NHI=input$evtsNHI)
  }
  
  observeEvent(input$evtsRepeatButton, {
    if (values$unsavedEventStatus == "No")
      evtsRepeatButtonConfirmed()
    else
      showModal(modalDialog(
        title = "Repeat Event",
        "You have unsaved changes to the current event. Are you sure?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("evtsRepeatOK", "OK")
        )
      ))
  })
  
  observeEvent(input$evtsRepeatOK, {
    removeModal()
    evtsRepeatButtonConfirmed()
  })
  
  evtsRepeatButtonConfirmed = function() {
    cat("Repeat event: ")
  
    pt = getPatients() %>%
      filter(NHI == input$evtsNHI) # get the patient associated with this event
    
    drug = getDrugs()[[pt$Drug]]$Maintenance
    req(drug)
    
    if (input$evtsType %in% names(drug))
    {
      newduedate = input$evtsDueDate + months(drug[[input$evtsType]])
      showNotification(paste0("Setting due date for ", drug[[input$evtsType]], " months"))
    }
    else
    {
      newduedate = NA
      showNotification("No maintenance schedule defined. Specify Due Date.")
    }
    
    editNewEvent(NHI=input$evtsNHI,
      Type=input$evtsType,
      Number=as.numeric(input$evtsNumber)+1,
      DueDate=newduedate,
      Completed=ymd(""),
      Result="",
      Comment="")
    
    showAllEventsForPatient(input$evtsNHI)
  }
  
  observeEvent(input$evtsSaveButton, {
    
    req(input$evtsNHI)
    cat("Save Event\n")
    
    if (is.null(input$evtsDueDate))
      showNotification("Warning: no Due Date set for this event")
    
    if (input$evtsId == -1) {
      newid = createNewEvent()
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
    saveEventsFile()
    
    values$unsavedEventStatus="No"
  })
  
  saveEventsFile = function() {
    if (!demoVersion)
      write.csv(getEvents(), file="data/events.csv", row.names=F)
    else
      showNotification("Save changes inactivated in demo version")
  }
  
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
    editNewEvent()
  })

    
  # =================================
  # Timeline and DT
  # =================================
  
  # detect timeline event drag
  # observes input$tlMoveEvent and updates evtsDueDate
  observe({
    x = input$tlMoveEvent
    req(x$item$start)
    # min and max hack for bug introduced in shiny
    updateDateInput(session, "evtsDueDate", value = x$item$start, min="2000-01-01", max="2100-01-01")
  })
  
  # detect timeline event selection
  # observes input$tlSelectEvents, checks for unsaved changes, calls editEvent with the selected event id
  observe({
    x = input$tlSelectEvent
    
    req(x$items$id)
    if (x$id == "evtsTimeline") {
      editEvent(x$items$id)
      cat("timeline selection event: ", x$items$id,"\n")
    }

  })
  
  # detect datatable event row selection
  # observe input$evtsTable_rows_selected
  observe({
    # TODO Check Unsaved
    
    if (length(input$evtsTable_rows_selected) > 0) {
      id = getFilteredEvents()[input$evtsTable_rows_selected, "EventId"]
      editEvent(id)
      cat("Data table row selection event: ",id,"\n")
    }
  })
  
  output$evtsTimeline <- renderTimelinevis({
    cat("Rendering timeline\n")
    
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
  
  # patient table row selection event
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
  
  # new patient button
  observeEvent(input$ptsNew, {
    updateTextInput(session, "ptsNHI", value = "Enter details")
    updateTextInput(session, "ptsFirstName", value = "then")
    updateTextInput(session, "ptsSurname", value = "click save")
    updateSelectInput(session, "ptsDrug", selected = "", choices = names(getDrugs()))
    updateDateInput(session, "ptsDateStarted", value = "")
    updateRadioButtons(session, "ptsJCV", selected="Neg")
  })
  
  # delete patient button
  observeEvent(input$ptsDelete, {
    showModal(modalDialog(
      title = "Delete Patient",
      "This will also delete all events associated with this NHI! Are you sure?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ptsDeleteConfirm", "OK")
      )
    ))   
  })
  
  observeEvent(input$ptsDeleteConfirm, {
    cat("Delete patient: ", pts$NHI, "\n")
    evts = getEvents()
    evts = evts[-which(evts$NHI == input$ptsNHI), ]
    values[["msevents"]] = evts
    pts = getPatients()
    pts = pts[-which(pts$NHI==input$ptsNHI), ]
    values[["mspts"]] = pts
  })
  
  # save patient button
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
    
    savePatientsFile()
  })
  
  savePatientsFile = function() {
    if (!demoVersion)
      write.csv(getPatients(), file="data/patients.csv", row.names=F)
    else
      showNotification("Save changes disabled in demo version!")
  }
  
  initiationEvent = function(type, emonths) {
    cat("Create Initiation Event: ", type, "\n")
    id=createNewEvent(
      NHI=input$ptsNHI,
      Type=type,
      Number=1,
      DueDate = today() %m+% weeks(emonths*4)
    )
    return(id)
  }
  
  observeEvent(input$ptsGenerateInitiationEvents, {
    # TODO: check is this pt saveed yet
    drug = getDrugs()[[input$ptsDrug]]

    req(drug)
    req(input$ptsJCV)
    ids = c()
    
    for (event in names(drug$Initial)) {
      if (event == "JCPos" & input$ptsJCV=="Pos")
        for (event2 in names(drug$Initial$JCPos))
          ids=c(ids, initiationEvent(event2, drug$Initial$JCPos[[event2]]))
      else if (event == "JCNeg" & input$ptsJCV=="Neg")
        for (event2 in names(drug$Initial$JCNeg))
          ids=c(ids, initiationEvent(event2, drug$Initial$JCNeg[[event2]]))
      else if ((event %in% c("JCNeg","JCPos"))==F)
        ids=c(ids, initiationEvent(event, drug$Initial[[event]]))
    }
    
    saveEventsFile()
    showAllEventsForPatient(input$ptsNHI)
    req(ids[1])
    editEvent(ids[1])
  })
  
  observeEvent(input$ptsNewEvent, {
    showAllEventsForPatient(pts$NHI)
    editNewEvent(NHI=input$ptsNHI)
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
    jsonedit(getDrugs(), name="Drugs", "change" = htmlwidgets::JS('function(){
        Shiny.onInputChange("jsonEditorChangeEvent", 
          {id: event.currentTarget.parentNode.id, data:event.currentTarget.parentNode.editor.get()});}'))
  })
  
})