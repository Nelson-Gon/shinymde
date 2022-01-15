recode_ui<- function() {
  tabItem(
  tabName = "recode_values",
  div(id= "recode_controls",
      wellPanel(
        
          
          selectizeInput(
            "recode_type",
            "Recode Kind",
            choices = c(
              "recode_as_na",
              "recode_na_as",
              "recode_as_na_for",
              "recode_as_na_if"
            ),
            selected = "recode_as_na"
          ), 
          fluidRow(
            column(6,
                   textInput("value_to_recode", "Value")
                   ),
            column(6,
                   selectInput(
                     "criteria",
                     "Criteria",
                     choices = c("gt", "lt",
                                 "lteq", "gteq", "eq"),
                     selected = "gt"
                     
                   )
                   )
          )
         , 
         fluidRow(column(
           7,
           selectInput(
             "pattern_type",
             "Pattern type",
             choices = c("starts_with",
                         "ends_with", 
                         "contains",
                         "regex"),
             selected = FALSE,
             selectize = FALSE,
             size = 4
           )
         ),
         column(
           5,
           textInput("pattern", "Pattern", value = NULL)
         ))
        
      )
    ,
      
      br(),
    wellPanel(
      
      fluidRow(
        column(6,
               shinyWidgets::dropdown(
                 style = "bordered",
                 size = "sm", 
                 width = "240px",
                 animate = shinyWidgets::animateOptions(
                   enter = "fadeInLeft", 
                   exit = "fadeOut"),
                 icon = icon("filter"),
                 label = "SUBSET",
                 Map(function(id, lab, choices)
                   selectInput(
                     inputId = id,
                     label = lab,
                     choices = choices,
                     multiple = TRUE
                   ), 
                   c("subset_cols",
                     "keep_columns"),
                   c("Subset", "Keep Cols"),
                   list(c("A", "B"), c("A", "B")))
                 
               )
               ),
        column(6, 
               shinyWidgets::downloadBttn(
                 "downloadfile_recode",
                 size = "sm",
                 label = "", 
                 style = "bordered",
                 color = "default"
               )
               )
      )
      
    )
             
              
        ) ,
    div( id = "recode_table",
      
      shinycssloaders::withSpinner(dataTableOutput("recode_values"))
    )
  ) 
}