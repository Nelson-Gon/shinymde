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
              # column(2, uiOutput("subset_cols")),
              # column(2, textInput("pattern_type", "Pattern type", 
              #                     value = NULL)),
              # column(2, textInput("pattern", "Pattern", value=NULL)),
              column(2, textInput("value_to_recode", "Value"))
     
   ))
))
 
server <- function(input, output, session){
  in_data <- reactive(
    if(is.null(input$input_file$datapath)){
      stop("Please provide a valid dataset path")
    }
    else{
      read.csv(input$input_file$datapath)
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
output$summary_na <- renderDataTable(na_summary(in_data(),
                                                sort_by = input$sort_by,
                                          grouping_cols = input$group_by,
                                   exclude_cols = input$exclude_columns,
                                   descending = sort_order()),
                              options = list(pageLength=5))
  
# Create a switch to select recode_type

recode_type <- reactive({
  # optional arguments list 
  # Filter only non NULL elements 
  arguments = 
    Filter(function(x) !is.null(x), list(df=in_data(),
                value=input$value_to_recode))
                #    pattern_type=input$pattern_type,
                # pattern = input$pattern,
                # subset_cols = input$subset_cols))
 
  do.call(input$recode_type, arguments)
})

output$recode_values <- renderDataTable({
recode_type()
},
options = list(pageLength=5)
)
  
  
  
  
}

shinyApp(ui, server)

