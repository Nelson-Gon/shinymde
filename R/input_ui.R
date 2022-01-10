input_ui <- function() {
  tabItem(tabName = "input",
          tags$div(id = "input_controls",
            inputPanel(
              
            shinyWidgets::awesomeRadio(
              "data_source",
              "Data Source",
              choices = c("inbuilt",
                          "remote",
                          "user_data"),
              selected = "inbuilt"
            ),
            conditionalPanel(
              condition =
                "input.data_source == 'user_data'",
              fileInput("input_file",
                        label = "Input File",
                        placeholder =  "Please provide a file path")
            ),
            conditionalPanel(condition = "input.data_source == 'user_data'",
                             numericInput("sheet", "Sheet", value =
                                            1)),
            conditionalPanel(
              condition =
                "input.data_source=='inbuilt'",
              selectize_input(id="dataset", label="Dataset",
                              choices = c("mtcars", "airquality"),
                              selected = "airquality")
            ),
            conditionalPanel(condition =
                               "input.data_source == 'remote'",
                             textInput("remote",
                                       "Remote",
                                       value = "")),
            conditionalPanel(
              condition =
                "input.data_source == 'remote'",
              selectize_input(
                id="file_type",
                label="File Type",
                choices = c("csv", "tsv"),
                selected = "csv"
              )
            ),
            
       fluidRow(
         column(6, 
                shinyWidgets::actionBttn(inputId = "confirm_in",
                                         label = "Confirm",
                                         color = "default",
                                         icon = shiny::icon("check"),
                                         style = "fill")),
         column(6,shinyWidgets::actionBttn(inputId = "reset_input",
                                           label = "Reset",
                                           color = "default",
                                           icon = shiny::icon("undo"),
                                           style = "fill") )
       )     
        
        )),
           br(),
            div(
              id = "sys_details",sys_info_boxes()
            )
            ,
            div(id = "data_summarise",
            verbatimTextOutput("data_summary")
            )
            
          
        )
  
}