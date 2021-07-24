# setwd("shinymde")
# source("ui.R")
# source("server.R")
# Use devtools::load_all 
# https://mastering-shiny.org/scaling-packaging.html


#' #' 
#' if (any(!c("shiny", "mde", "vroom", "dplyr", "ggplot2",
#'            "forcats") %in% installed.packages())){
#'   stop("Please install shiny package >=1.6.0, mde >= 0.3.1, 
#'        vroom >=1.5.3, dplyr >=1.0.7, ggplot2 >= 3.3.4, forcats>=0.5.1  
#'        and readxl >=1.3.1")
#' }
#' library(shiny)
options(shiny.autoload.r=FALSE)
ui <- fluidPage(
  # theme=bslib::bs_theme(bootswatch = "darkly"),
  shinyjs::useShinyjs(),
  tabsetPanel(
    tabPanel("Input Data",
             fluidRow(
               column(6, selectInput("data_source", "Data Source",
                                     choices = c("inbuilt",
                                                 "remote",
                                                 "user_data"),
                                     selected = "inbuilt")),
               column(6, conditionalPanel(condition =
                                            "input.data_source == 'user_data'",
                                          uiOutput("input_file"))),
               column(6,  conditionalPanel(condition=
                                             "input.data_source=='inbuilt'",
                                           uiOutput("dataset"))),
               column(3, conditionalPanel(condition = 
                                            "input.data_source == 'remote'",
                                          uiOutput("remote"))),
               column(3, conditionalPanel(condition = 
                                            "input.data_source == 'remote'",
                                          uiOutput("file_type"))))),
    
    tabPanel("Summarise Missingness",
             dataTableOutput("summary_na"),
             column(2, downloadButton("downloadfile", "Download")),
             fluidRow(
               column(2, uiOutput("sort_by")),
               column(2, selectInput("sort_order", "Sort Order",
                                     choices=c("ascending", "descending"))),
               column(2, numericInput("round_to", "Round to", 
                                      value = options("digits"))),
               column(2, uiOutput("group_by")),
               column(2, uiOutput("exclude_columns"))
               
             )),
    tabPanel("Recode Values",
             dataTableOutput("recode_values"),
             fluidRow(
               column(2, selectInput("recode_type", "Kind of recoding",
                                     choices = c("recode_as_na",
                                                 "recode_na_as",
                                                 "recode_as_na_for",
                                                 "recode_as_na_if"),
                                     selected="recode_as_na")),
               column(2, textInput("value_to_recode", "Value")),
               
               column(2, uiOutput("criteria")),
               column(2, uiOutput("subset_cols")),
               column(2,
                      # need pattern_type and subset_cols not both so need
                      # to set one to NULL
                      # This in shiny is done like so 
                      # see stackoverflow.com/a/53698788/10323798
                      selectInput("pattern_type", "Pattern type",
                                  choices = c("starts_with",
                                              "ends_with","contains",
                                              "regex"),
                                  selected = FALSE,
                                  selectize = FALSE,
                                  size = 4)), 
               column(2,textInput("pattern", "Pattern", value=NULL))
               # TODO: Automate ui creation. 
               
             )),
    tabPanel("Drop Values",
             dataTableOutput("drop_na"),
             fluidRow(
               column(2, selectInput("drop_type", "Kind of drop",
                                     choices = c("drop_all_na",
                                                 "drop_na_if",
                                                 "drop_na_at"),
                                     selected="drop_all_na")),
               
               column(2, numericInput("percent_na_drop",
                                      "Percent NA", value=20)),
               column(2, uiOutput("sign")),
               column(2, uiOutput("group_by_drop")),
               column(2, uiOutput("keep_columns_drop")),
               column(2, uiOutput("target_cols")),
               
               column(2,
                      selectInput("pattern_type_drop", "Pattern type",
                                  choices = c("starts_with",
                                              "ends_with","contains",
                                              "regex"),
                                  selected = FALSE,
                                  selectize = FALSE, 
                                  size = 4)),
               column(2,textInput("pattern_drop", "Pattern", value=NULL))
               
             )),
    tabPanel("Visualise Missingness",
             plotOutput("visual_summary"),
             fluidRow(
               column(4, uiOutput("y_variable")),
               column(4, uiOutput("x_variable")),
               column(2, uiOutput("fill_variable")),
               column(2, numericInput("round_to_visual", "Round to", 
                                      value = 2))
             ))
  ))





#' @import shiny 
#' @import ggplot2
#' @importFrom dplyr %>% 

server <- function(input, output, session){
  
  
  
  
  output$input_file <- renderUI({
    
    
    fileInput("input_file",
              label = "Please provide a file path")
  })
  
  output$dataset <- renderUI(
    {selectInput("dataset", "Dataset", 
                 choices = ls("package:datasets"),
                 selected = "airquality")
    }
  )
  
  output$remote <- renderUI({
    textInput("remote", "Remote Dataset Link", 
              value = "https://github.com/Nelson-Gon/shinymde/blob/c6cd1b8b3acc28225a907e00f80ac4031b755966/testdata/airquality.csv?raw=TRUE")
  })
  
  output$file_type <- renderUI({
    textInput("file_type", "File Extension", value = "csv")
  })
  
  
  guess_input <- reactive({
    
    if(input$data_source=="user_data"){
      return(gsub("(.*)(\\..*$)", "\\2",
                  input$input_file$datapath, perl=TRUE))
    }
    
    if(input$data_source=="remote"){
      return(input$file_type)
    }
    else{
      warning("Cannot guess input type, defaulting to csv")
      return(".csv")
    }
    
  })
  
  in_data <- reactive({
    
    
    
    if(input$data_source=="inbuilt"){
      return(get(input$dataset, "package:datasets"))
      
    }
    
    
    if(input$data_source=="remote"){
      stopifnot("Only csv, tsv, xlsx currently supported not" = 
                  input$file_type %in% c("csv", "xlsx", "tsv")
                
      )
      sep_switch = switch(input$file_type,
                          "csv" = read.table(url(input$remote), sep=",",
                                             header=TRUE),
                          "tsv" = read.table(url(input$remote), sep="\t",
                                             header=TRUE),
                          "xlsx" = readxl::read_xlsx(input$remote))
      return(sep_switch)
    }
    
    if(input$data_source=="user_data"){
      
      if(is.null(input$input_file$datapath)){
        stop("Please provide a valid dataset path")
      }
      
      
      
      
      if(!guess_input() %in% c(".csv", ".xlsx", ".tsv")){
        stop(paste0("Only .csv, .xlsx, and .tsv are currently supported, not ",
                    guess_input(),"."))
      }
      switch(guess_input(),
             ".csv"=vroom::vroom(input$input_file$datapath, 
                                 delim = ",",
                                 show_col_types = FALSE),
             ".xlsx" = readxl::read_xlsx(input$input_file$datapath),
             ".tsv" = vroom::vroom(input$input_file$datapath, 
                                   delim="\t",
                                   show_col_types = FALSE)
      )
      
      
      
    }
  })
  
  
  
  
  
  
  output$sort_by <- renderUI({
    selectInput("sort_by", 
                "Column to sort_by", 
                choices = names(na_summary(in_data())))
  })
  
  
  
  output$group_by <- renderUI({
    selectInput("group_by", "Grouping Columns", 
                choices = names(in_data()),
                multiple = TRUE)
  })
  
  output$exclude_columns <- renderUI({
    selectInput("exclude_columns", 
                "Columns to exclude", 
                choices = names(in_data()),
                multiple=TRUE)})
  
  
  
  
  sort_order <- reactive({ifelse(input$sort_order=="descending",
                                 TRUE, FALSE)})
  summary_na <- reactive(na_summary(in_data(),
                                    sort_by = input$sort_by,
                                    grouping_cols = input$group_by,
                                    exclude_cols = input$exclude_columns,
                                    descending = sort_order(),
                                    round_to=input$round_to))
  
  output$summary_na <- renderDataTable(summary_na(),
                                       options = list(pageLength=5))
  
  delimiters <- reactive({ 
    
    switch(guess_input(),
           ".csv" = ",",
           ".tsv" = "\t",
           ".xlsx" = ";")
    
    
    
  })
  
  
  
  output$downloadfile <- downloadHandler(
    filename = function() { paste0(substitute(in_data()),
                                   "_missingness_report_mde_", 
                                   format(Sys.time(), "%b-%d-%Y"),
                                   guess_input()) },
    content = function(x) {
      delim = switch(guess_input(),
                     ".csv" = ",",
                     ".xlsx" = ";",
                     ".tsv" = "\t")
      vroom::vroom_write(summary_na(), x,
                         delim = delim)
    }
  )
  
  values_to_recode <- reactive({
    # split and convert to numeric if applicable 
    values = unlist(strsplit(input$value_to_recode, ","))
    if(any(grepl("[0-9]", values))){
      # TODO Show user warnings  
      # shinyFeedback::showFeedback("value_to_recode",
      # text="Input values converted to numeric")
      values <- as.numeric(values)
    }
    values 
  })
  
  output$subset_cols <- renderUI(  
    selectInput("subset_cols",
                "A subset to recode",
                choices=names(in_data()),
                multiple=TRUE))
  
  output$criteria <- renderUI(selectInput("criteria", 
                                          "Criteria",
                                          choices=c("gt", "lt",
                                                    "lteq", "gteq", "eq"),
                                          selected = "gt"))
  
  output$keep_columns <- renderUI(
    selectInput("keep_columns", "Keep Columns", 
                choices = names(in_data()),
                multiple=TRUE))
  
  
  recode_switch <- reactive({
    # recode_as_na_for() --> df, criteria, value, subset_cols 
    # recode_as_na_if() --> df, sign , percent_na, keep_columns
    # recode_na_if() --> df, grouping_cols, target_groups, replacement 
    # recode_as_na() --> df, value, subset_cols, pattern_type, pattern 
    # recode_na_as() --> df, value, subset_cols, pattern_type, pattern 
    # dict_recode() --> df, use_func, pattern_type, patterns, values
    
    # NOTES
    # This could be done with do.call or some switch but for whatever reason
    # Such calls fail with bugs that I could not identify readily. 
    # See https://github.com/Nelson-Gon/shinymde/issues/1 and 
    # https://github.com/Nelson-Gon/shinymde/issues/2 
    
    shinyjs::hide("criteria")
    
    shinyjs::hide("pattern_type")
    shinyjs::hide("pattern")
    if(input$recode_type %in% c("recode_as_na", "recode_na_as")){
      shinyjs::toggle("pattern_type")
      shinyjs::toggle("pattern")
      
    }
    if(input$recode_type=="recode_as_na"){
      # NOTE This requires explicit returns unlike in "normal"
      # R programming mode. 
      
      return(mde::recode_as_na(df = in_data(), value = values_to_recode(),
                               subset_cols = input$subset_cols, 
                               pattern_type = input$pattern_type,
                               pattern = input$pattern))
    }
    
    if(input$recode_type=="recode_na_as"){
      return(mde::recode_na_as(df = in_data(), value = values_to_recode(),
                               subset_cols = input$subset_cols, 
                               pattern_type = input$pattern_type,
                               pattern = input$pattern)) 
    }
    
    if(input$recode_type == "recode_as_na_if"){
      shinyjs::hide("subset_cols")
      shinyjs::show("criteria")
      return(mde::recode_as_na_if(df = in_data(), 
                                  percent_na = values_to_recode(),
                                  sign=input$criteria,
                                  keep_columns=input$keep_columns))
    }
    if(input$recode_type == "recode_as_na_for"){
      shinyjs::show("criteria")
      shinyjs::show("subset_cols")
      return(mde::recode_as_na_for(df=in_data(), criteria=input$criteria,
                                   value = values_to_recode(),
                                   subset_cols = input$subset_cols))
    }
    
  })
  
  
  
  output$recode_values <- renderDataTable(
    recode_switch(),
    options = list(pageLength=5)
  )
  
  
  # Dropping NAs 
  output$group_by_drop <- renderUI({
    selectInput("group_by_drop", "Grouping Columns", 
                choices = names(in_data()),
                multiple = TRUE)
  })
  
  output$keep_columns_drop <- renderUI(
    selectInput("keep_columns_drop", "Keep Columns", 
                choices = names(in_data()),
                multiple=TRUE))
  output$target_cols <- renderUI(
    selectInput("target_cols", "Target Columns", 
                choices = names(in_data()),
                multiple=TRUE))
  output$sign <- renderUI(
    selectInput("sign", "Sign", 
                choices = c("gt", "gteq", "lt", "lteq", "eq"),
                selected = "gt",
                multiple=FALSE))
  
  drop_switch <- reactive({
    
    shinyjs::hide("pattern_type_drop")
    shinyjs::hide("pattern_drop")
    shinyjs::hide("keep_columns_drop")
    shinyjs::hide("target_cols")
    shinyjs::hide("percent_na_drop")
    shinyjs::hide("sign")
    shinyjs::show("group_by_drop")
    
    
    # mde::drop_all_na() --> df, grouping_cols 
    # mde::drop_na_at() --> df, pattern_type, pattern, case_sensitivity, ...
    # mde::drop_na_if() --> df, sign, percent_na, keep_columns, grouping_cols,
    # target_columns 
    if(input$drop_type=="drop_all_na"){
      return(mde::drop_all_na(df = in_data(), 
                              grouping_cols = input$group_by_drop))
    }
    
    if(input$drop_type == "drop_na_at"){
      shinyjs::toggle("pattern_type_drop")
      shinyjs::toggle("pattern_drop")
      shinyjs::toggle("group_by_drop")
      return(mde::drop_na_at(df=in_data(), 
                             pattern_type = input$pattern_type_drop,
                             pattern = input$pattern_drop))
    }
    
    if(input$drop_type=="drop_na_if"){
      shinyjs::toggle("percent_na_drop")
      shinyjs::toggle("sign")
      shinyjs::toggle("keep_columns_drop")
      shinyjs::toggle("target_cols")
      
      mde::drop_na_if(df = in_data(),
                      sign = input$sign,
                      percent_na = input$percent_na_drop,
                      keep_columns = input$keep_columns_drop,
                      grouping_cols = input$group_by_drop,
                      target_columns = input$target_cols)
    }
  }
  
  )
  
  output$drop_na <- renderDataTable(drop_switch(),
                                    
                                    options = list(pageLength=5))
  
  # Visual summaries 
  output$y_variable <- renderUI(
    {
      selectInput("y_variable",
                  "Y axis variable",
                  choices = names(summary_na()),
                  selected = "percent_missing")
    }
  )
  
  output$x_variable <- renderUI(
    {
      selectInput("x_variable",
                  "X axis variable",
                  choices = names(summary_na()),
                  selected = "variable")
    }
  )
  
  output$fill_variable <- renderUI(
    {
      {
        selectInput("fill_variable",
                    "Fill variable",
                    choices = names(summary_na()),
                    selected = "variable")
      }
    }
  )
  
  
  output$visual_summary <- renderPlot(
    {
      summary_na() %>% 
        ggplot(aes(forcats::fct_reorder(.data[[req(input$x_variable)]],
                                        .data[[req(input$y_variable)]]), 
                   .data[[req(input$y_variable)]], 
                   fill=.data[[req(input$fill_variable)]]))+
        geom_col()+
        coord_flip()+
        
        guides(fill="none")+
        geom_label(aes(label=round(.data[[input$y_variable]], 
                                   input$round_to_visual)))+
        theme_minimal() +
        labs(x=input$x_variable)
      #      y =substitute(input$y_variable) )
    }
  )
  
  
  
  
}

shinyApp(ui, server)