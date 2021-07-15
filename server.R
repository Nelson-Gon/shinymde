if (any(!c("shiny", "mde", "vroom", "dplyr", "ggplot2",
           "forcats") %in% installed.packages())){
  stop("Please install shiny package >=1.6.0, mde >= 0.3.1, 
       vroom >=1.5.3, dplyr >=1.0.7, ggplot2 >= 3.3.4, forcats>=0.5.1  
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
  
  
  
  # sort_by 
  
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
  
  
  
  output$subset_cols <- renderUI({selectInput("subset_cols",
                                              "a subset to recode",
                                              choices=names(in_data()),
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
  
  
  
  recode_type <- reactive({
    # optional arguments list 
    # Filter only non NULL elements 
    # TODO: Figure out how to choose subset columns and pattern types. 
    # Currently NULL is not recognized as NULL for some reason. 
    arguments = 
      Filter(function(x) !is.null(x), list(df=in_data(),
                                           value=input$value_to_recode))
    
    do.call(input$recode_type, arguments)
  })
  
  output$recode_values <- renderDataTable({
    recode_type()
  },
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
