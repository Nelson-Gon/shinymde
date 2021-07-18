if (any(!c("shiny", "mde", "vroom", "dplyr", "ggplot2",
           "forcats", "readxl", "shinyjs") %in% installed.packages())){
  stop("Please install shiny package >=1.6.0, mde >= 0.3.1, 
       vroom >=1.5.3, dplyr >=1.0.7, ggplot2 >= 3.3.4, forcats>=0.5.1,
       shinyjs >= 2.0.0,
       and readxl >=1.3.1")
}
library(shiny)
library(mde)
library(ggplot2)
library(dplyr)
server <- function(input, output, session){
  
  
  guess_input <- reactive({
    return(gsub("(.*)(\\..*$)", "\\2", input$input_file$datapath, perl=TRUE)
    )})
  in_data <- reactive(
    if(is.null(input$input_file$datapath)){
      stop("Please provide a valid dataset path")
    }
    else{
      if(!guess_input() %in% c(".csv", ".xlsx", ".tsv")){
        stop(paste0("Only .csv, .xlsx, and .tsv are currently supported, not ",
                    guess_input(),"."))
      }
      switch(guess_input(),
             ".csv"=vroom::vroom(input$input_file$datapath, delim = ","),
             ".xlsx" = readxl::read_xlsx(input$input_file$datapath),
             ".tsv" = vroom::vroom(input$input_file$datapath, delim="\t")
      )
      
      
      
    }
    
  )
  
  
  
  
  output$sort_by <- renderUI({
    selectInput("sort_by", "Column to sort_by", 
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
  
  

  
  sort_order <- reactive({ifelse(input$sort_order=="descending", TRUE, FALSE)})
  summary_na <- reactive(na_summary(in_data(),
                                    sort_by = input$sort_by,
                                    grouping_cols = input$group_by,
                                    exclude_cols = input$exclude_columns,
                                    descending = sort_order()))
  
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
                                   "_missingness_report_mde", 
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
  
  

    # optional arguments list 
    # Filter only non NULL elements 
    # TODO: Figure out how to choose subset columns and pattern types. 
    # Currently NULL is not recognized as NULL for some reason. 
    # Allow multiple values in value --> convert to split
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
    # NOTE This requires explcit returns unlike in "normal"
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
        geom_label(aes(label=round(.data[[input$y_variable]], 2)))+
        theme_minimal() +
        labs(x=input$x_variable)
        #      y =substitute(input$y_variable) )
    }
  )
  
  
  
  
}
