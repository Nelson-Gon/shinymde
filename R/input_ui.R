input_ui <- function() {
  tabItem(tabName = "input",
        sidebarLayout(
          sidebarPanel(
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
            
            
        input_action_buttons()
            
            
          ),
          mainPanel(
            div(
              id = "sys_details",sys_info_boxes()
            ),
            verbatimTextOutput("data_summary")
            
          )
        ))
  
}