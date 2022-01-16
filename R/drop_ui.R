drop_ui <- function() {
  tabItem(
  tabName = "drop_values",

    sidebarLayout(
      sidebarPanel(
        selectInput(
          "drop_type",
          "Kind of drop",
          width = "180px",
          choices = c("drop_all_na",
                      "drop_na_if",
                      "drop_na_at"),
          selected = "drop_na_if"
        ),
        fluidRow(
          two_columns(
            numericInput("percent_na_drop",
                         "Percent NA", value = 20),
            selectInput(
              "sign",
              "Sign",
              choices = c("gt", "gteq", "lt", "lteq", "eq"),
              selected = "gt",
              multiple = FALSE
            ))
          
          
        ),
        
        fluidRow(
          column(9,selectInput(
            "group_by_drop",
            "Group BY",
            choices = c("A", "B"),
            multiple = TRUE
          ))),
        fluidRow(
          
          two_columns(
            selectInput(
              "keep_columns_drop",
              "Keep Cols",
              choices = c("A", "B"),
              multiple = TRUE
            ),
            selectInput(
              "target_cols",
              "Target Cols",
              choices = c("A", "B"),
              multiple = TRUE
            )
          )
          
        ),
        fluidRow(
          column(8, 
            selectInput(
              "pattern_type_drop",
              "Pattern type",
              choices = c("starts_with",
                          "ends_with", "contains",
                          "regex"),
              selected = FALSE,
              selectize = FALSE,
              size = 4
            )) ,
          column(4, 
            textInput("pattern_drop", "Pattern", value = NULL)
          )
          ),
        
        shinyWidgets::downloadBttn(
          "downloadfile_drop",
          label = "",
          size = "sm",
          style = "bordered",
          color = "default"
        )
        
        
        
      ),
      mainPanel(
      
    shinycssloaders::withSpinner(dataTableOutput("drop_na"))
            
        
        
      )
    ))

    
    
    
}