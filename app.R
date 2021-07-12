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
                column(4, uiOutput("sort_by")),
                column(4, selectInput("sort_order", "Sort Order",
                                      choices=c("ascending", "descending"))),
                column(4, uiOutput("group_by")),
                column(4, uiOutput("exclude_columns"))
                
              ))
     
   )
 )
 
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

sort_order <- reactive({ifelse(input$sort_order=="descending", TRUE, FALSE)})   
output$summary_na <- renderDataTable(na_summary(in_data(),
                                                sort_by = input$sort_by,
                                                grouping_cols = input$group_by,
                                   exclude_cols = input$exclude_columns,
                                   descending = sort_order()),
                              options = list(pageLength=5))
  
  
  
  
  
}

shinyApp(ui, server)

