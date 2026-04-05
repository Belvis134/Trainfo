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
      ")),
    tags$script(src = "../www/data_importing.js"),
    tags$script(src = "../www/discord_data_transfer.js")
  ),
  
  titlePanel(tags$p(style = "color: white; text-align: center", "Traⓘnfo Alpha 1.0")),
  sidebarLayout(
    sidebarPanel(
      width = 6,
      style = "background-color: #7F7F7F;",
      tags$div(tags$h4(strong(tags$i(icon("circle-info")), "The current MRT/LRT network status information:"))),
      htmlOutput("train_info_out"),
      tags$div(tags$h4(strong(tags$i(icon("triangle-exclamation")), "Manage Reports"))),
      radioButtons("report_select","I would like to", choices = c("Search Reports" = "search", "Submit Report" = "submit", "Amend Report" = "amend"), inline = T),
      conditionalPanel(
        condition = "input.report_select == 'search'",
        fluidRow(splitLayout(
          paste(""),
          selectInput("train_line","Which line?", choices = c("NSL" = "nsl"), width = "80px"),
          airDatepickerInput("date",HTML(paste(icon("calendar"), "What Date?")), dateFormat = "yyyy-MM-dd", view = "months", minView = "days", width = "100px", addon = "none", readonly = TRUE, autoClose = TRUE),
          textInput("submitted_user","User?"),
          cellWidths = c("10px", "80px", "100px","200px")
        )),
        actionButton("search", "Search Reports", width = "140px", icon = icon("magnifying-glass"))
      ),
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
  
  print("Wait")
  session$sendCustomMessage("fetch_datamall", '')
  session$sendCustomMessage("fetch_sheet", '')
  train_info <- reactiveVal('')
  pre_sheet <- reactiveVal(NULL)
  
  observeEvent(input$train_info_in, {
    train_info(input$train_info_in)
  })
  observeEvent(input$sheets_in, {
    print("Data Loading")
    raw_sheet <- fromJSON(input$sheets_in, simplify = FALSE)
    headers <- raw_sheet[[1]]
    rows <- do.call(rbind, raw_sheet[-1])
    df <- data.frame(rows, stringsAsFactors = FALSE)
    colnames(df) <- headers
    pre_sheet(df)
  })
  
  # Render table with clickable HTML in Action column
  output$report_sheet <- renderDT({
    print("Table Loading")
    if (is.null(pre_sheet())) {return(NULL)}
    else {
      df <- pre_sheet()
      df$REMARKS <- "View"
      df$STATIONS <- "View"
      df$REMARKS <- sprintf(
        '<a href="#" onclick="Shiny.setInputValue(\'remark_view\', %d, {priority: \'event\'});">%s</a>',
        seq_len(nrow(df)), df$REMARKS
      )
      df$STATIONS <- sprintf(
        '<a href="#" onclick="Shiny.setInputValue(\'aff_stns_view\', %d, {priority: \'event\'});">%s</a>',
        seq_len(nrow(df)), df$STATIONS
      )
      datatable(df, options = list(pagingType = "full_numbers"), escape = FALSE, selection = "none", rownames = FALSE) %>%
        formatStyle(
          colnames(df),
          color = "white",
          fontWeight = "bold"
        )
    }
  })
  
  observeEvent(input$remark_view, {
    df <- pre_sheet()
    remarks_content <- df$REMARKS
    clicked_row <- input$remark_view
    showModal(
      modalDialog(
        title = paste("Report by", df$USER[clicked_row]),
        remarks_content[clicked_row],
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
        stations_content[clicked_row],
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
