recode_ui<- function() {
  tabItem(
  tabName = "recode_values",
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
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
      
      
      fluidRow(column(6,
                      textInput("value_to_recode", "Value")),
               column(
                 6,
                 selectInput(
                   "criteria",
                   "Criteria",
                   choices = c("gt", "lt",
                               "lteq", "gteq", "eq"),
                   selected = "gt"
                   
                 )
               )
      ),
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
      )),
      br(),
      
      fluidRow(
        column(5,
               shinyWidgets::dropdown(
                 style = "bordered",
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
        column(7,
               # need pattern_type and subset_cols not both so need
               # to set one to NULL
               # This in shiny is done like so
               # see stackoverflow.com/a/53698788/10323798
               shinyWidgets::downloadBttn(
                 "downloadfile_recode",
                 "Download this report",
                 style = "bordered",
                 color = "default"
               )
        )
      )
      
      
      
    ),
    mainPanel(
      
      shinycssloaders::withSpinner(dataTableOutput("recode_values"))
    )
  ) )
}