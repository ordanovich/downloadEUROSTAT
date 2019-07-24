library(dplyr)
library(magrittr)
library(eurostat)
library(tidyr)
library(DT)
library(sparkline)
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(htmlwidgets)
library(openxlsx)
library(rmarkdown)

load("nuts.RData")

myModal <- function() {
  div(id = "test",
      modalDialog(downloadButton("download1","Download table as csv"),
                  br(),
                  br(),
                  downloadButton("download2","Download table as xlsx"),
                  easyClose = TRUE, title = "Download Table")
  )
}

##--------------------------------------

header <- dashboardHeader(disable=T)

sidebar <- dashboardSidebar(disable=T)

body <- dashboardBody(
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  
  tags$head(
    tags$style(HTML(
      ".tabbable ul li:nth-child(1) { float: left; }
      .tabbable ul li:nth-child(2) { float: left; }
      .tabbable > .nav > li > a  {background-color: white;  color:black}"
    ))
  ),
  
  
  fluidRow(
    
    column(width = 12,
           
           boxPlus(
             title = "Set criteria for data retrieval", 
             closable = FALSE,
             width = 12,
             
             fluidRow(
               
               column(width = 6,
                      htmlOutput("table")),
               column(width = 6,
                      htmlOutput("period"))
             )
             
           )
           
           
    )
    
  ),
  
  fluidRow(
    
    column(width = 6,
           
           gradientBox(
             title = "Full table",
             width = 12,
             icon = "fa fa-table",
             gradientColor = "purple",
             boxToolSize = "xs",
             closable = FALSE,
             collapsible = TRUE,
             
             
             fluidRow(
               
               column(width = 12,
                      
                      fluidRow(
                        
                        column(width = 6,
                               
                               radioButtons(inputId = "selectAllCountries",
                                            label = "",
                                            choices = c("Select all countries", "Choose specific country/-s"),
                                            selected = "Choose specific country/-s",
                                            inline = T),
                               
                               uiOutput("raw_countries")),
                        column(width = 6,
                               
                               radioButtons(inputId = "selectAllVariables",
                                            label = "",
                                            choices = c("Select all variables", "Choose specific variable/-s"),
                                            selected = "Choose specific variable/-s",
                                            inline = T),
                               
                               uiOutput("raw_variables"))
                        
                      )
                      
                      
               )
             ),
             
             footer = fluidRow(
               
               column(width = 12,
                      uiOutput("d0")
                      
               )
               
             )
             
           )
    ),
    
    
    
    column(width = 6,
           
           gradientBox(
             title = "Summary table",
             width = 12,
             icon = "fa fa-chart-bar",
             gradientColor = "maroon",
             boxToolSize = "xs",
             closable = FALSE,
             collapsible = TRUE,
             
             fluidRow(
               
               column(width = 12,
                      
                      fluidRow(
                        
                        column(width = 6,
                               htmlOutput("summary_countries")),
                        column(width = 6,
                               htmlOutput("summary_variable"))
                        
                      )
                      
                      
               )
             ),
             
             footer = fluidRow(
               
               column(width = 12,
                      
                      tabsetPanel(type = "pills",
                                  
                                  tabPanel("Whiskers (free)",
                                           DT::dataTableOutput("d3")
                                           
                                  ),
                                  
                                  tabPanel("Whiskers (fixed to a single range)",
                                           DT::dataTableOutput("d4")
                                           
                                  )
                      ),
                      
                      downloadButton("report", "Generate report")
                      
               )
               
               
             )
           )
    )
  )
)



shinyApp(
  
  ui = dashboardPage(header, sidebar, body),
  
  server = function(input, output, session) { 
    
    options(shiny.usecairo=T)
    
    
    
    
    
    
    
    toc_original <- reactive({
      
      get_eurostat_toc() %>% filter(type %in% c("dataset", "table")) %>% distinct() %>% 
        mutate(data_start_year = as.numeric(substr(`data start`, start = 1, stop = 4)),
               data_end_year = as.numeric(substr(`data end`, start = 1, stop = 4))) 
      
    })
    
    toc_filtered <- reactive({
      
      toc_original() %>% 
        filter(data_start_year <= input$period[1] &
                 data_end_year >= input$period[1] &
                 data_end_year <= input$period[2])
      
    })
    
    
    output$period <- renderUI({
      
      tt <- unique(append(toc_original()$data_start_year, toc_original()$data_end_year))
      tt[!is.na(tt)] -> tt
      
      
      sliderInput(inputId = "period",
                  label = "Set the time window",
                  min = min(tt),
                  max = max(tt),
                  value = c(quantile(tt)[2], quantile(tt)[4]),
                  sep = ""
      )
      
    })
    
    output$table <- renderUI({
      
      selectizeInput(inputId = "table",
                     label = "Select table",
                     choices = sort(toc_filtered()$title),
                     multiple = F)
      
      
    })
    
    data_raw <- reactive({
      
      ii <- toc_filtered()[toc_filtered()$title == input$table, "code"]
      
      eurostat.data <- as.data.frame(get_eurostat(id = ii)) %>%
        label_eurostat(fix_duplicated=T, code = "geo") %>%
        mutate_if(is.factor, as.character)
      
      lbl <- label_eurostat_vars(names(eurostat.data)) %>% na.omit()
      names(eurostat.data) <- append(append("NUTS code", lbl), "Values")
      
      eurostat.data %>% merge(nuts[,c("NUTS_ID", "CNTR_CODE", "NUTS_NAME", "LEVL_CODE")] %>%
                               set_colnames(c("NUTS_ID", "Country_code", "Country_name", "NUTS level")), by.x = "NUTS code", by.y = "NUTS_ID")
      
      
    })
    
    
    
    
    output$raw_countries_display <- renderUI({
      
      selectizeInput(inputId = "raw_countries_display",
                     label = "Select countries to filter the table by",
                     choices = sort(unique(data_raw()[data_raw()$`NUTS level` == 0, "Country_name"])),
                     selected = sample(unique(data_raw()[data_raw()$`NUTS level` == 0, "Country_name"]),6),
                     multiple = T) 
      
    })
    
    output$raw_countries <- renderUI({
      
      
      if(input$selectAllCountries == "Choose specific country/-s") {
        
        htmlOutput("raw_countries_display")
      
        }
   
      })
    
    
    output$raw_variables_display <- renderUI({
      
      selectizeInput(inputId = "raw_variables_display",
                     label = "Add variables to the table",
                     choices = sort(names(data_raw())[!(names(data_raw()) %in% c("Country_name",
                                                                                 "Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)",
                                                                                 "Values"))]),
                     selected = sample(sort(names(data_raw())[!(names(data_raw()) %in% c("Country_name",
                                                                                         "Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)",
                                                                                         "Values"))]),1),
                     multiple = T)
      
      
    })
    
    output$raw_variables <- renderUI({
      
      if(input$selectAllVariables == "Choose specific variable/-s") {
      
        htmlOutput("raw_variables_display")
        
      }
      
      
    })
    
    cc <- reactive({cc <- data_raw() %>% filter(Country_name %in% input$raw_countries_display) %>% select(Country_code) %>% distinct(); cc$Country_code})
    
    
    data0_allC_allV <- reactive({
      
      nn <- append("Period_of_time",
                   names(data_raw())[!(names(data_raw()) == "Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)")])
      
      
      data_raw() %>%
        filter(as.numeric(lubridate::year(`Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)`)) %in% seq(input$period[1], input$period[2], 1)) %>%
        mutate(Period_of_time = `Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)`) %>% 
        select(nn) %>%
        arrange(Period_of_time, Country_name)
      
      
    })
    
    
    data0_selC_allV <- reactive({
      
      nn <- append("Period_of_time",
                   names(data_raw())[!(names(data_raw()) == "Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)")])
      
      
      data_raw() %>%
        filter(Country_code %in% cc() & as.numeric(lubridate::year(`Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)`)) %in% seq(input$period[1], input$period[2], 1)) %>%
        mutate(Period_of_time = `Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)`) %>% 
        select(nn) %>%
        arrange(Period_of_time, Country_name)
      
      
    })
    
    
    data0_allC_selV <- reactive({
      
      data_raw() %>%
        filter(as.numeric(lubridate::year(`Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)`)) %in% seq(input$period[1], input$period[2], 1)) %>%
        mutate(Period_of_time = `Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)`) %>% 
        select("Period_of_time",
               "Country_name",
               input$raw_variables_display, 
               "Values") %>%
        arrange(Period_of_time, Country_name)
      
      
    })
    
    data0_selC_selV <- reactive({
      
      data_raw() %>%
        filter(Country_code %in% cc() & as.numeric(lubridate::year(`Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)`)) %in% seq(input$period[1], input$period[2], 1)) %>%
        mutate(Period_of_time = `Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)`) %>% 
        select("Period_of_time",
               "Country_name",
               input$raw_variables_display, 
               "Values") %>%
        arrange(Period_of_time, Country_name)
      
      
    })
    

    
    
    output$d0_allC_allV <- renderDataTable(
      
      datatable( data0_allC_allV(),
                 extensions = 'Buttons',
                 options = list(
                   scrollX = TRUE,
                   dom = 'Bfrtip',
                   buttons = list(
                     "copy",
                     list(
                       extend = "collection",
                       text = 'Download entire dataset',
                       action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
}")
                     )
                   )
                 )
    ))
    
    
    output$d0_allC_selV <- renderDataTable(
      
      datatable( data0_allC_selV(),
                 extensions = 'Buttons',
                 options = list(
                   scrollX = TRUE,
                   dom = 'Bfrtip',
                   buttons = list(
                     "copy",
                     list(
                       extend = "collection",
                       text = 'Download entire dataset',
                       action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
}")
                     )
                   )
                 )
      ))
    
    
    output$d0_selC_allV <- renderDataTable(
      
      datatable( data0_selC_allV(),
                 extensions = 'Buttons',
                 options = list(
                   scrollX = TRUE,
                   dom = 'Bfrtip',
                   buttons = list(
                     "copy",
                     list(
                       extend = "collection",
                       text = 'Download entire dataset',
                       action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
}")
                     )
                   )
                 )
      ))
    
    
    output$d0_selC_selV <- renderDataTable(
      
      datatable( data0_selC_selV(),
                 extensions = 'Buttons',
                 options = list(
                   scrollX = TRUE,
                   dom = 'Bfrtip',
                   buttons = list(
                     "copy",
                     list(
                       extend = "collection",
                       text = 'Download entire dataset',
                       action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
}")
                     )
                   )
                 )
      ))
    
    
    output$d0 <- renderUI({
      
      if(input$selectAllCountries == "Choose specific country/-s" & input$selectAllVariables == "Choose specific variable/-s") {
        
        dataTableOutput("d0_selC_selV")
      }
      
      else if(input$selectAllCountries == "Select all countries" & input$selectAllVariables == "Select all variables") {
        
        dataTableOutput("d0_allC_allV")
      }
      
      else if(input$selectAllCountries == "Choose specific country/-s" & input$selectAllVariables == "Select all variables") {
        
        dataTableOutput("d0_selC_allV")
      }
      
      else if(input$selectAllCountries == "Select all countries" & input$selectAllVariables == "Choose specific variable/-s") {
        
        dataTableOutput("d0_allC_selV")
      }
      
    })
    
    
    
    observeEvent(input$test, {
      print("hello")
      showModal(myModal())
    })
    
    
    output$download1 <- downloadHandler(
      filename = function() {
        paste(input$table, ".csv", sep="")
      },
      content = function(file) {
        
        if(input$selectAllCountries == "Choose specific country/-s" & input$selectAllVariables == "Choose specific variable/-s") { write.csv(data0_selC_selV(), file)}
        else if(input$selectAllCountries == "Select all countries" & input$selectAllVariables == "Select all variables") { write.csv(data0_allC_allV(), file)}
        else if(input$selectAllCountries == "Choose specific country/-s" & input$selectAllVariables == "Select all variables") { write.csv(data0_selC_allV(), file)}
        else if(input$selectAllCountries == "Select all countries" & input$selectAllVariables == "Choose specific variable/-s") { write.csv(data0_allC_selV(), file)}
    }
    )
    
    output$download2 <- downloadHandler(
      filename = function() {
        paste(input$table, ".xlsx", sep="")
      },
      content = function(file) {
        if(input$selectAllCountries == "Choose specific country/-s" & input$selectAllVariables == "Choose specific variable/-s") { write.xlsx(data0_selC_selV(), file)}
        else if(input$selectAllCountries == "Select all countries" & input$selectAllVariables == "Select all variables") { write.xlsx(data0_allC_allV(), file)}
        else if(input$selectAllCountries == "Choose specific country/-s" & input$selectAllVariables == "Select all variables") { write.xlsx(data0_selC_allV(), file)}
        else if(input$selectAllCountries == "Select all countries" & input$selectAllVariables == "Choose specific variable/-s") { write.xlsx(data0_allC_selV(), file)}
        
      }
    )
    
    
    output$summary_countries <- renderUI({
      
      selectizeInput(inputId = "summary_countries",
                     label = "Select up to 6 countries",
                     choices = sort(unique(data_raw()[data_raw()$`NUTS level` == 0, "Country_name"])),
                     selected = sample(unique(data_raw()[data_raw()$`NUTS level` == 0, "Country_name"]),6),
                     multiple = T)
      
      
      
    })
    
    output$summary_variable <- renderUI({
      
      selectizeInput(inputId = "summary_variable",
                     label = "Select one grouping variable",
                     choices = sort(names(data_raw())[!(names(data_raw()) %in% c("NUTS code","NUTS level","Country_code","Country_name",
                                                                                 "Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)",
                                                                                 "Values"))]),
                     selected = sample(sort(names(data_raw())[!(names(data_raw()) %in% c("NUTS code","NUTS level","Country_code","Country_name",
                                                                                         "Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)",
                                                                                         "Values"))]),1),
                     multiple = F)
      
      
    })
    
    
    
    
    data_summary_filtered <- reactive({
      
      cc <- data_raw() %>% filter(Country_name %in% input$summary_countries) %>% select(Country_code) %>% distinct(); cc <- cc$Country_code
      
      data_raw() %>%
        filter(Country_code %in% cc & as.numeric(lubridate::year(`Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)`)) %in% seq(input$period[1], input$period[2], 1)) %>%
        select(input$summary_variable, Country_code, `Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)`, Values) %>%
        arrange( `Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)`) %>%
        select(-c(`Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)`)) %>%
        set_colnames(c("Grouping Variable", "geoVar", "Value")) %>%
        na.omit()
      
      
      
      
    })
    
    
    data_summary_summarised <- reactive({
      
      data_summary_filtered() %>%
        group_by(geoVar,`Grouping Variable`) %>%
        summarize(Value = paste(Value, collapse = ",")) %>%
        spread(geoVar, Value)
      
      
    })
    
    
    
    
    output$d3 <- DT::renderDataTable({
      
      js <- "function(data, type, full){ return '<span class=spark>' + data + '</span>' }"
      colDefs2 <- list(list(targets = c(1:6), render = JS(js)))
      
      
      r <- range(data_summary_filtered()$Value)
      
      x <- "function (oSettings, json) { $('.spark:not(:has(canvas))').sparkline('html', { "
      
      box_string <- "type: 'box', lineColor: 'black', whiskerColor: 'black', outlierFillColor: 'black', outlierLineColor: 'black', medianColor: 'black', boxFillColor: 'orange', boxLineColor: 'black'"
      cb_box1 <- JS(paste0(x, box_string, " }); }"), collapse = "")
      
      d3 <- datatable(as.data.frame(data_summary_summarised()),
                      rownames = FALSE, 
                      options = list(columnDefs = colDefs2,
                                     fnDrawCallback = cb_box1))
      
      d3$dependencies <- append(d3$dependencies, htmlwidgets:::getDependency("sparkline"))
      
      d3
      
      
    })
    
    
    output$d4 <- DT::renderDataTable({
      
      
      js <- "function(data, type, full){ return '<span class=spark>' + data + '</span>' }"
      colDefs2 <- list(list(targets = c(1:6), render = JS(js)))
      r <- range(data_summary_filtered()$Value)
      box_string <- "type: 'box', lineColor: 'black', whiskerColor: 'black', outlierFillColor: 'black', outlierLineColor: 'black', medianColor: 'black', boxFillColor: 'orange', boxLineColor: 'black'"
      cb_box2 <- JS(paste0(x, box_string, ", chartRangeMin: ", r[1], ", chartRangeMax: ",
                           r[2], " }); }"), collapse = "")
      
      d4 <- DT::datatable(as.data.frame(data_summary_summarised()), 
                          rownames = FALSE, 
                          
                          options = list(columnDefs = colDefs2, 
                                         fnDrawCallback = cb_box2
                          ))
      
      d4$dependencies <- append(d4$dependencies, htmlwidgets:::getDependency("sparkline"))
      
      d4
      
    })
    
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(n = data_summary_filtered(),
                       m = data_summary_summarised(),
                       y1 = input$period[1],
                       y2 = input$period[2])
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    
    

    
  }
)

