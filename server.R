library(shiny)
library(DT)
library(yaml)
library(shinyjs)
library(sweetalertR)

server <- shinyServer(function(input, output, session) {
  
  values = reactiveValues()
  values$unsavedEventStatus = "No"

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
  
  createNewEvent = function(NHI="", Type="", Number=1, DueDate=ymd(""), Completed=ymd(""), Result="", Comment="") {
    newid = max(values$msevents$EventId)+1
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
          filter(DueDate > today(), DueDate < endDate, is.na(Completed))
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
      h3(input$evtsNHI, style="margin-top:0px"),
      p(values$mspts$Surname[values$mspts$NHI==input$evtsNHI],style="margin-bottom:0px")
    ))
  })
  
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
    cat("timeline event selection\n")
    
    req(x$items$id)
    if (x$id == "evtsTimeline")
      editEvent(x$items$id)
  })
  
  # detect datatable event row selection
  # observe input$evtsTable_rows_selected
  observe({
    cat("Data table row selection event\n")
    # TODO Check Unsaved
    
    if (length(input$evtsTable_rows_selected) > 0) {
      id = getFilteredEvents()[input$evtsTable_rows_selected, "EventId"]
      editEvent(id)
    }
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
  
  observeEvent(input$evtsNumberCalc, {
    x = getEvents() %>% filter(NHI == input$evtsNHI, Type == input$evtsType)
    req(x$Number)
    updateTextInput(session, "evtsNumber", value=(max(x$Number,na.rm=T) + 1))
  })
  
  observeEvent(input$evtsClearSearchButton, {
    updateTextInput(session, "evtsSearchNHI", value="")
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
    # updateRadioButtons(session, "evtsFilterTimeframe", selected="All")
    req(input$evtsNHI)
    updateTextInput(session, "evtsSearchNHI", input$evtsNHI)
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
  
    pt = getPatients() %>%
      filter(NHI == input$evtsNHI) # get the patient associated with this event
    drug = getDrugs() %>%
      filter(Name == pt$Drug) # get the drug associated with this patient

    stop() # need to implement getDrugEventTime
    newduedate = input$evtsDueDate + months(drug[1, input$evtsType])

    newid = createNewEvent(
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
  }
  
  observeEvent(input$evtsSaveButton, {
    cat("Save Event\n")
    
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
    write.csv(getEvents(), file="data/events.csv", row.names=F)
    
    values$unsavedEventStatus="No"
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
    editNewEvent()
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
    createNewEvent(
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
    jsonedit(getDrugs(), "change" = htmlwidgets::JS('function(){
        Shiny.onInputChange("jsonEditorChangeEvent", 
          {id: event.currentTarget.parentNode.id, data:event.currentTarget.parentNode.editor.get()});}'))
  })
  
})