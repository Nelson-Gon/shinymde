if (any(!c("shiny", "mde", "vroom") %in% installed.packages())){
  stop("Please install shiny package >=1.6.0, mde >= 0.3.1, 
       vroom >=1.5.3, and readxl >=1.3.1")
}
library(shiny)
library(mde)
 ui <- fluidPage(
   tabsetPanel(
     tabPanel("Input Data",
              fluidRow(
                column(4, fileInput("input_file",
                                    label = "Please provide a file path"))
              )),
     tabPanel("Summarise Missingness",
              dataTableOutput("summary_na"),
              column(2, downloadButton("downloadfile", "Download")),
              fluidRow(
                column(2, uiOutput("sort_by")),
                column(2, selectInput("sort_order", "Sort Order",
                                      choices=c("ascending", "descending"))),
                column(2, uiOutput("group_by")),
                column(2, uiOutput("exclude_columns"))
                
              )),
     tabPanel("Recode Values",
              dataTableOutput("recode_values"),
              fluidRow(
              column(2, selectInput("recode_type", "Kind of recoding",
                                     choices = c("recode_as_na",
                                                 "recode_na_as"),
                                     selected="recode_na_as")),
              column(2, textInput("value_to_recode", "Value"))
              # column(2, uiOutput("subset_cols")),
              # column(2, textInput("pattern_type", "Pattern type",
              #                     value = NULL)),
              # column(2, textInput("pattern", "Pattern", value=NULL)),
              
     
   ))
))
 
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
        stop("Only csv, xlsx, and tsv are currently supported.")
      }
      switch(guess_input(),
             ".csv"=vroom::vroom(input$input_file$datapath),
             ".xlsx" = readxl::read_xlsx(input$input_file$datapath),
             ".tsv" = vroom::vroom(input$input_file$datapath)
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
  content = function(x) {vroom::vroom_write(summary_na(), x,
                                            delim = delimiters()) }
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
  
  
  
  
}

shinyApp(ui, server)


