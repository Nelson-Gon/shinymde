missingness_ui <- function() {
  tabItem(
    tabName = "missingness_summary",
    
    tags$div(
      id = "summarise_controls",
      inputPanel(fluidRow(
        column(
          6,
          shinyWidgets::dropdown(
            animate = shinyWidgets::animateOptions(enter = "fadeInLeft",
                                                   exit = "fadeOut"),
            label = "FILTER",
            icon = icon("filter"),
            style = "bordered",
            size = "sm",
            width = "260px",
            
            multi_selectize(
              ids = c("select_kind",
                      "pattern_type_summary"),
              titles = c("Selection Kind",
                         "Pattern type"),
              choices = list(
                c("exclusion",
                  "inclusion"),
                c("contains",
                  "starts_with",
                  "ends_with",
                  "regex")
              ),
              size = c(2, 4),
              selected = FALSE,
              selectize = FALSE
            )
            
            
            ,
            
            textInput("pattern_summary", label = "Pattern",
                      value = NULL)
          )
          
        ),
        
        column(
          6,
          shinyWidgets::downloadBttn(
            outputId = "downloadfile",
            label = "", 
            size = "sm",
            style = "bordered",
            color = "default"
          )
          
        )
      ))
      ,
      br(), 
      h4(strong("Group"),
         br(),
         wellPanel(fluidRow(
           column(5,
                  numeric_input(
                    "round_to", "Round to",
                    value = options("digits")
                  )),
           column(
             7,
             selectize_input(
               id = "group_by",
               label = "Group BY",
               choices = c("A", "B"),
               multiple = TRUE,
               selected = FALSE
             )
           )
         )))  ,
      br(),
      
      h4(strong("Sort"),
         br(),
         wellPanel(fluidRow(
           lapply(list(
             selectizeInput(
               "sort_by",
               "By",
               choices = c("percent_missing", "variable"),
               selected = "percent_missing"
             ),
             selectize_input(
               id = "sort_order",
               label = "Order",
               choices = c("ascending", "descending"),
               selected = "descending"
             )
           ), function(x)
             column(6, x))
         )))
    ),
    tags$div(id = "summary_na_table",
             
  shinycssloaders::withSpinner(dataTableOutput("summary_na")))
    
    
  )
}