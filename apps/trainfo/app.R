suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(rjson))
suppressPackageStartupMessages(library(htmltools))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(shinythemes))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(DT))

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #3F3F3F;
      }
      .import_shift {
        margin-top: 25px;
      }
      .red_text {
        color: #BB0000;
      }
      .green_text {
        color: #00DD00;
      }
      .blue_text {
        color: #2050C0;
      }
      .shiny-split-layout > div {
        overflow: visible;
      }
      input[type='checkbox'],
      input[type='radio'] {
        accent-color: #007BFF;
      }
      table.dataTable thead th {
        background-color: #2c3e50 !important; /* dark blue */
        color: white !important;              /* white text */
        text-align: center;
      }
      div.dataTables_length label {
        color: white;
        font-weight: bold;
      }
      div.dataTables_filter label {
        color: white; 
        font-style: bold;
      }
      .dataTables_info {
        color: white !important;
        font-weight: bold;
      }
      .dataTables_length select {
        background-color: #f0f8ff !important; /* White background */
        color: black !important;              /* Text color */
      }
      .dataTables_empty {
        color: white !important;
      }
      /* Normal buttons */
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        background-color: #007bff !important; /* Button background */
        color: white !important;              /* Button text color */
        border-radius: 4px;                    /* Rounded corners */
        padding: 5px 10px;
        margin: 2px;
        cursor: pointer;
      }
      /* Active page button */
      .dataTables_wrapper .dataTables_paginate .paginate_button.current {
          background-color: #28a745 !important; /* Green for active page */
          color: white !important;
          border: none !important;
      }
      /* Disabled buttons */
      .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
          background-color: #AAAAAA !important;
          color: #666 !important;
          cursor: not-allowed;
      }
      .tight-wrap {
        display: flex;
        flex-wrap: wrap;       /* Allow wrapping to next line */
        gap: 10px;              /* Space between items */
        align-items: flex-start; /* Align items to top */
      }
      .tight-wrap > div {
        flex: 0 0 auto;         /* Prevent stretching */
      }
      .modal-dialog {
        width: 80%
      }
      ")),
    tags$script(src = "../www/data_importing.js"),
    tags$script(src = "../www/discord_data_transfer.js"),
    tags$a(href = "../www/station_names.json", id = "stations")
  ),
  
  titlePanel(tags$p(style = "color: white; text-align: center", "Traⓘnfo Beta 1.3.0")),
  sidebarLayout(
    sidebarPanel(
      width = 6,
      style = "background-color: #7F7F7F;",
      tags$div(tags$h4(strong(tags$i(icon("circle-info")), "The current MRT/LRT network status information:"))),
      htmlOutput("train_info_out"),
      tags$div(tags$h4(strong(tags$i(icon("triangle-exclamation")), "Manage Reports"))),
      conditionalPanel(
        condition = "input.report_select == 'search'",
        tags$div(tags$h5(strong(class = "blue_text" , tags$i(icon("circle-info")), "Please enure that you select the lines and directions. Leaving them blank will NOT give any result."))),
        tags$div(tags$h5(strong(class = "blue_text" , tags$i(icon("circle-info")), "Leaving lines and directions as N/A gives results without a line or direction. That could be the case sometimes.")))
      ),
      conditionalPanel(
        condition = "input.report_select == 'submit'",
        tags$div(tags$h5(strong(class = "red_text" , tags$i(icon("triangle-exclamation")), "Each sector is only supposed to have one line!"))),
        tags$div(tags$h5(strong(class = "blue_text" , tags$i(icon("circle-info")), "You need to select a sector that is higher than your current max sector to create a new sector. The sector number auto jumps after sector creation."))),
        tags$div(tags$h5(strong(class = "blue_text" , tags$i(icon("circle-info")), "The current date is highlighted in blue, the current time can be quickly accessed by clicking the clock."))),
        tags$div(tags$h5(strong(class = "blue_text" , tags$i(icon("circle-info")), "When on the main sheet view and you would want to return to sector sheet view, click on \"Reload Edits\".")))
      ),
      tags$div(tags$h5(strong(class = "blue_text" , tags$i(icon("circle-info")), "The direction and stations input changes depending on what line(s) you selected. You can also report a whole line by selecting the line in the station from box."))),
      radioButtons("report_select","I would like to", choices = c("Search Reports" = "search", "Submit Report" = "submit", "Amend Report" = "amend"), inline = T),
      conditionalPanel(
        condition = "input.report_select == 'search' || input.report_select == 'submit'",
        selectizeInput("train_line","Which lines?", choices = c("ALL", "N/A" = "#N/A", "NSL", "EWL", "NEL", "CCL", "DTL", "TEL", "BPLRT", "SKLRT", "PGLRT"), multiple = TRUE, selected = "ALL"),
        selectizeInput("direction","Which directions?", choices = c("N/A" = "#N/A"), multiple = TRUE),
        div(class = "tight-wrap",
          div(airDatepickerInput("date",HTML(paste(icon("calendar"), "What Date?")), dateFormat = "yyyy-MM-dd", clearButton = TRUE,
                             view = "months", minView = "days", width = "140px", addon = "none", readonly = TRUE, autoClose = TRUE)),
          div(timeInput("time",HTML(paste(icon("clock"), "What Time?")), width = "100px")),
          div(textInput("submitted_user","User?"))
        ),
      ),
      conditionalPanel(
        condition = "input.report_select == 'search'",
        actionButton("search", "Search Reports", width = "140px", icon = icon("magnifying-glass"))
      ),
      conditionalPanel(
        condition = "input.report_select == 'submit'",
        selectizeInput("stations_from", "The start station. If the whole line is selected, there is no need to put an end station.", NA),
        selectizeInput("stations_till", "The end station.", NA),
        textAreaInput("remarks_input", "Any remarks? Describe how the breakdown/accident happened and perpetrators if any. This is optional."),
        selectizeInput("sector", "Which sector to store or modify? Each sector consists of one line. All affected sectors reported are combined into a single report.", choices = 1),
        div(class ="tight-wrap",
          div(actionButton("add_sector", "Add Sector", width = "110px", icon = icon("plus"))),
          div(actionButton("modify_sector", "Modify Sector", width = "130px", icon= icon("pencil"))),
          div(actionButton("delete_sector", "Delete Sector", width = "130px", icon= icon("trash"))),
          div(actionButton("reload_edits", "Reload Edits", width = "125px", icon= icon("arrow-rotate-right"))),
          div(actionButton("submit_report", "Submit Report", width = "130px", icon= icon("upload"))),
        )
      )
    ),
    mainPanel(
      div(
        style = "width:150%;",
        DTOutput("report_sheet")
      ),
      textOutput("clicked")
    )
  )
)

server <- function (input, output, session) {
  
  session$sendCustomMessage("fetch_datamall", '')
  session$sendCustomMessage("fetch_sheet", '')
  train_info <- reactiveVal('')
  names_json <- reactiveVal('')
  sheet_out <- reactiveVal(NULL)
  sheet_out2 <- reactiveVal(data.frame(NULL))
  pre_sheet <- reactiveVal(NULL)
  prev_sector <- reactiveVal(NULL)
  submitted_user <- reactive({input$submitted_user})
  filter_date <- reactive({input$date})
  filter_time <- reactive({input$time})
  train_line <- reactive({input$train_line})
  filter_direction <- reactive({input$direction})
  stations_from <- reactive({input$stations_from})
  stations_till <- reactive({input$stations_till})
  remarks_input <- reactive({input$remarks_input})
  input_sector <- reactive({input$sector})
  
  init_data <- function() {
    user <- submitted_user()
    date <- filter_date()
    time <- filter_time()
    line <- train_line()
    direction <- filter_direction()
    return(list(user = user, date = date, time = time, line = line, direction = direction))
  }
  init_names <- function(line) {
    station_names <- names_json()
    station_names <- paste(names(station_names), unlist(station_names))
    dl <- c(
      "NSL - Jurong East" = "NSL-JRB",
      "NSL - Marina South Pier" = "NSL-MRB",
      "EWL - Tuas Link" = "EWL-WB",
      "EWL - Pasir Ris" = "EWL-EB",
      "CGL - Tanah Merah" = "CGL-TMB",
      "CGL - Changi Airport" = "CGL-CGB",
      "NEL - Punggol Coast" = "NEL-NB",
      "NEL - HarbourFront" = "NEL-SB",
      "CCL - Clockwise" = "CCL-CW",
      "CCL - Anti-Clockwise" = "CCL-ACW",
      "CCL - Clockwise to Dhoby Ghaut" = "CCL-CW-DBG",
      "CCL - Anti-Clockwise from Dhoby Ghaut" = "CCL-ACW-DBG",
      "DTL - Bukit Panjang" = "DTL-BPB",
      "DTL - Expo" = "DTL-XPB",
      "TEL - Woodlands North" = "TEL-WLB",
      "TEL - Sungei Bedok" = "TEL-CGB",
      "BPLRT - Service A via Senja" = "BPLRT-SVA",
      "BPLRT - Service B via Petir" = "BPLRT-SVB",
      "SKLRT - West Loop Inner" = "SKLRT-WLI",
      "SKLRT - West Loop Outer" = "SKLRT-WLO",
      "SKLRT - East Loop Inner" = "SKLRT-ELI",
      "SKLRT - East Loop Outer" = "SKLRT-ELO",
      "PGLRT - West Loop Inner" = "PGLRT-WLI",
      "PGLRT - West Loop Outer" = "PGLRT-WLO",
      "PGLRT - East Loop Inner" = "PGLRT-ELI",
      "PGLRT - East Loop Outer" = "PGLRT-ELO"
    )
    dir_choices <- c("N/A" = "#N/A")
    valid_names <- NULL
    if ("ALL" %in% line) {dir_choices <- c(
      dir_choices, "ALL", dl
    ); valid_names <- c("NSL", "EWL", "NEL", "CCL", "DTL", "TEL", "BPLRT", "SKLRT", "PGLRT", station_names)}
    if ("#N/A" %in% line) valid_names <- "N/A"
    if ("NSL" %in% line) {dir_choices <- c(
      dir_choices, dl["NSL - Jurong East"], dl["NSL - Marina South Pier"]
    ); valid_names <- c("NSL",valid_names, grep("NS", station_names, value = TRUE))}
    if ("EWL" %in% line) {dir_choices <- c(
      dir_choices, dl["EWL - Tuas Link"], dl["EWL - Pasir Ris"], dl["CGL - Tanah Merah"], dl["CGL - Changi Airport"]
    ); valid_names <- c("EWL",valid_names, grep("EW", station_names, value = TRUE), grep("CG", station_names, value = TRUE))}
    if ("CCL" %in% line) {dir_choices <- c(
      dir_choices, dl["CCL - Clockwise"], dl["CCL - Anti-Clockwise"], dl["CCL - Clockwise to Dhoby Ghaut"], dl["CCL - Anti-Clockwise from Dhoby Ghaut"]
    ); valid_names <- c("CCL",valid_names, grep("CC", station_names, value = TRUE), grep("CE", station_names, value = TRUE))}
    if ("NEL" %in% line) {dir_choices <- c(
      dir_choices, dl["NEL - Punggol Coast"], dl["NEL - HarbourFront"]
    ); valid_names <- c("NEL",valid_names, grep("NE", station_names, value = TRUE))}
    if ("DTL" %in% line) {dir_choices <- c(
      dir_choices, dl["DTL - Bukit Panjang"], dl["DTL - Expo"]
    ); valid_names <- c("DTL", valid_names, grep("DT", station_names, value = TRUE))}
    if ("TEL" %in% line) {dir_choices <- c(
      dir_choices, dl["TEL - Woodlands North"], dl["TEL - Sungei Bedok"]
    ); valid_names <- c("TEL",valid_names, grep("TE", station_names, value = TRUE))}
    if ("BPLRT" %in% line) {dir_choices <- c(
      dir_choices, dl["BPLRT - Service A via Senja"], dl["BPLRT - Service B via Petir"]
    ); valid_names <- c("BPLRT", valid_names, grep("BP", station_names, value = TRUE))}
    if ("SKLRT" %in% line) {dir_choices <- c(
      dir_choices, dl["SKLRT - West Loop Inner"], dl["SKLRT - West Loop Outer"], dl["SKLRT - East Loop Inner"], dl["SKLRT - East Loop Outer"]
    ); valid_names <- c("SKLRT",valid_names, grep("SW", station_names, value = TRUE), grep("SE", station_names, value = TRUE), station_names["STC Sengkang"])}
    if ("PGLRT" %in% line) {dir_choices <- c(
      dir_choices, dl["PGLRT - West Loop Inner"], dl["PGLRT - West Loop Outer"], dl["PGLRT - East Loop Inner"], dl["PGLRT - East Loop Outer"]
    ); valid_names <- c("PGLRT",valid_names, grep("PW", station_names, value = TRUE), grep("PE", station_names, value = TRUE), station_names["PTC Punggol"])}
    return(list(dir_choices = dir_choices, valid_names = valid_names))
  }
  format_data <- function(df) {
    df$REMARKS <- "View"
    df$STATIONS <- "View"
    df$REMARKS <- sprintf(
      '<a href="javascript:void(0)" onclick="Shiny.setInputValue(\'remark_view\', %d, {priority: \'event\'});">%s</a>',
      seq_len(nrow(df)), df$REMARKS
    )
    df$STATIONS <- sprintf(
      '<a href="javascript:void(0)" onclick="Shiny.setInputValue(\'aff_stns_view\', %d, {priority: \'event\'});">%s</a>',
      seq_len(nrow(df)), df$STATIONS
    )
    df$TIME <- sapply(str_split(df$TIME, ":"), function(x) paste(x[1], x[2], sep=":"))
    df$LINE <- str_split(df$LINE, "\n")
    df$DIRECTION <- sapply(str_split(df$DIRECTION, "\n"), function(x) list(x))
    return(df)
  }
  create_dt <- function(df) {
    dt <- datatable(df, options = list(pagingType = "full_numbers"), escape = FALSE, selection = "none", rownames = FALSE) %>%
    formatStyle(
      colnames(df),
      color = "white",
      fontWeight = "bold"
    )
    return(dt)
  }
  prep_df <- function() {
    existing_df <- sheet_out2()
    d <- init_data()
    station_from <- stations_from()
    station_till <- stations_till()
    remarks <- remarks_input()
    sector <- input_sector()
    return(c(d, list(existing_df = existing_df, station_from = station_from, station_till = station_till, remarks = remarks, sector = sector)))
  }
  observeEvent(input$train_info_in, {
    train_info(input$train_info_in)
  })
  observeEvent(input$station_names, {
    names_json(fromJSON(input$station_names, simplify = FALSE))
  })
  observeEvent(input$train_line, {
    line <- train_line()
    direction <- filter_direction()
    station_from <- stations_from()
    n <- init_names(line)
    dir_choice <- if (all(direction %in% n$dir_choices)) direction else NULL
    stn_choice <- if (all(station_from %in% n$valid_names)) station_from else NULL
    updateSelectizeInput(session, "direction", choices = n$dir_choices, selected = dir_choice)
    updateSelectInput(session, "stations_from", choices = n$valid_names, selected = stn_choice)
  })
  observeEvent(input$stations_from, {
    line <- train_line()
    n <- init_names(line)
    station_from <- stations_from()
    station_till <- stations_till()
    if (all(station_till %in% n$valid_names)) station_till else NULL
    if (station_from %in% c("NSL","EWL","NEL","CCL","DTL","TEL","BPLRT","SKLRT","PGLRT")) {
      updateSelectInput(session, "stations_till", choices = NA)
    } else {
      updateSelectInput(session, "stations_till", choices = n$valid_names[!n$valid_names %in% c("NSL", "EWL", "NEL", "CCL", "DTL", "TEL", "BPLRT", "SKLRT", "PGLRT")], selected = station_till)
    }
  })
  observeEvent(input$sheets_in, {
    raw_sheet <- fromJSON(input$sheets_in, simplify = FALSE)
    headers <- raw_sheet[[1]]
    rows <- do.call(rbind, raw_sheet[-1])
    df <- data.frame(rows, stringsAsFactors = FALSE)
    colnames(df) <- headers
    pre_sheet(df)
  })
  
  observeEvent(input$add_sector, {
    d <- prep_df()
    last_sector <- prev_sector()
    sector <- as.numeric(input_sector())
    if (sector <= max(length(d$existing_df$SECTOR))) stop("You need to select the sector higher than the current maximum!")
    df <- rbind(d$existing_df, data.frame(
      SECTOR = sector, DATE = d$date, TIME = d$time, LINE = d$line, DIRECTION = d$direction, STATION_FROM = d$station_from, STATION_TILL = d$station_till, USER = d$user, REMARKS = d$remarks
    ))
    sheet_out2(df)
    sheet_out(create_dt(df))
    prev_sector(sector)
    updateSelectizeInput(session, "sector", selected = (sector + 1), choices = c(1:(sector+1)))
  })
  observeEvent(input$modify_sector, {
    d <- prep_df()
    if(!d$sector %in% d$existing_df$SECTOR) stop(paste("There is no sector", d$sector, "to modify!"))
    d$existing_df[d$existing_df$SECTOR == d$sector, ] <- data.frame(
      SECTOR = d$sector, DATE = d$date, TIME = d$time, LINE = d$line, DIRECTION = d$direction, STATION_FROM = d$station_from, STATION_TILL = d$station_till, USER = d$user, REMARKS = d$remarks
    )
    sheet_out2(d$existing_df)
    sheet_out(create_dt(d$existing_df))
    updateSelectizeInput(session, "sector", choices = c(1:(length(d$existing_df$SECTOR)+1)))
  })
  observeEvent(input$reload_edits, {
    existing_df <- sheet_out2()
    sheet_out(create_dt(existing_df))
  })
  observeEvent(input$delete_sector, {
    existing_df <- sheet_out2()
    sector <- input_sector()
    if(!sector %in% existing_df$SECTOR) stop(paste("There is no sector", sector, "to delete!"))
    new_df <- existing_df[existing_df$SECTOR != sector, ]
    new_df$SECTOR <- sapply(new_df$SECTOR, function(x) if(x > sector) x-1 else x)
    sheet_out2(new_df)
    sheet_out(create_dt(new_df))
    if (length(new_df$SECTOR) != 0) updateSelectizeInput(session, "sector", choices = c(1:(length(new_df$SECTOR)+1)))
    else updateSelectizeInput(session, "sector", selected = 1, choices = 1)
  })
  
  # Render table with clickable HTML in Action column
  report_sheet <- eventReactive(input$search, {
    if (is.null(pre_sheet())) return(NULL)
    df <- pre_sheet(); d <- init_data(); df <- format_data(df);
    user_filter <- if (d$user == '' || is.null(d$user) || is.na(d$user)) quo(TRUE) else quo(USER %in% d$user)
    date_filter <- if (is.null(d$date)) quo(TRUE) else quo(DATE %in% d$date) 
    time_filter <- if (d$time == '') quo(TRUE) else quo(TIME %in% d$time)
    filtered_df <- df %>%
      filter(!!user_filter, !!date_filter, !!time_filter,
        sapply(LINE, function(x) if ("ALL" %in% d$line) {TRUE} else {any(sapply(d$line, function(y) any(y %in% x)))}),
        sapply(DIRECTION, function(x) if ("ALL" %in% d$direction) {TRUE} else {any(sapply(d$direction, function(y) any(y %in% x)))})
      )
    sheet_out(create_dt(filtered_df))
  })
  observe(report_sheet())
  output$report_sheet <- renderDT({sheet_out()})
  
  cfm_submission <- eventReactive(input$submit_report, {
    sector_df <- sheet_out2()
    idx <- sector_df$STATION_TILL != "NA" & !is.na(sector_df$STATION_TILL)
    sector_df$STATION_FROM[idx] <- paste(
      sector_df$STATION_FROM[idx], "to", sector_df$STATION_TILL[idx]
    )
    colnames(sector_df)[colnames(sector_df) == 'STATION_FROM'] <- 'STATIONS'
    sector_df <- sector_df %>% select(-STATION_TILL) %>% select(-SECTOR) %>% summarise(
      DATE = DATE[1], TIME = TIME[1],
      LINE = paste(LINE, collapse = "\n"), 
      DIRECTION = paste(DIRECTION, collapse = "\n"),
      STATIONS = paste(STATIONS, collapse = "\n"),
      USER = USER[1],
      REMARKS = paste(REMARKS, collapse = "\n\n"))
    sector_df2 <- sector_df %>%
      mutate(across(everything(), ~ str_replace_all(., "\n", "<br>")))
    showModal(
      modalDialog(
        title = paste("Submit Report?"),
        renderDT(sector_df2, rownames = FALSE, escape = FALSE),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("submit_confirm","Confirm")
        )
      )
    )
    return(sector_df)
  })
  observe(cfm_submission())
  observeEvent(input$submit_confirm, {
    sector_df <- cfm_submission()
    main_df <- pre_sheet()
    sector_df$DATE <- as.character(sector_df$DATE)
    main_df <- rbind(main_df, sector_df)
    pre_sheet(main_df)
    sheet_out(create_dt(format_data(main_df)))
    json_sheet <- c(
      list(colnames(main_df)),
      lapply(seq_len(nrow(main_df)), function(i) as.character(unlist(main_df[i, ])))
    )
    session$sendCustomMessage("publish_sheet", toJSON(json_sheet))
    removeModal()
  })
  
  observeEvent(input$remark_view, {
    df <- pre_sheet()
    remarks_content <- df$REMARKS
    clicked_row <- input$remark_view
    showModal(
      modalDialog(
        title = paste("Report by", df$USER[clicked_row]),
        HTML(str_replace_all(remarks_content[clicked_row],"\n","<br>")),
        easyClose = TRUE,  # Allow closing by clicking outside
        footer = tagList(
          modalButton("Close")
        )
      )
    )
  })
  
  observeEvent(input$aff_stns_view, {
    df <- pre_sheet()
    stations_content <- df$STATIONS
    clicked_row <- input$aff_stns_view
    showModal(
      modalDialog(
        title = paste("Affected Stations:"),
        HTML(str_replace_all(stations_content[clicked_row],"\n","<br>")),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Close")
        )
      )
    )
  })
  
  output$train_info_out <- renderText({HTML(train_info())})
}

shinyApp(ui, server)
shinyApp(ui = ui, server = server)
