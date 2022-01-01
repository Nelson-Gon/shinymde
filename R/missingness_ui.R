missingness_ui <- function(){
  tabItem(tabName = "missingness_summary",
          
          sidebarLayout(
            sidebarPanel(
              width = 4, 
              
              fluidRow(
                
        lapply(list(uiOutput("sort_by"),
          selectize_input(id= "sort_order",label = "Sort Order",
                        choices = c("ascending","descending"),
                        selected = "descending"
                            )), function(x) column(6,x))
                    )
              ,
              
              fluidRow(column(
                5,
                numeric_input("round_to", "Round to",
                              value = options("digits"))
              ),
              column(
                7,
                selectize_input(id = "group_by",label = "Group BY",
                  choices = c("A", "B"),multiple = TRUE,selected = FALSE
                )
              )),
              br(),
              fluidRow(
                column(5,
                       shinyWidgets::dropdown(
                         animate = shinyWidgets::animateOptions(
                           enter = "fadeInLeft",
                           exit = "fadeOut"
                         ),
                         label = "FILTER",
                         icon = icon("filter"),
                         style = "bordered",
                         
                         width = "260px",
                         
                         multi_selectize(ids=c("select_kind", 
                                               "pattern_type_summary"),
                                         titles = c("Selection Kind",
                                                    "Pattern type"),
                                         choices = list(c("exclusion",
                                                          "inclusion"),
                                                        c("contains",
                                                          "starts_with",
                                                          "ends_with",
                                                          "regex")),
                                         size = c(2, 4),
                                         selected = FALSE,
                                         selectize = FALSE)
                  
                     
                         ,
                         
                         textInput("pattern_summary", label = "Pattern",
                                   value = NULL)
                       ) ),
                column(7,
                       shinyWidgets::downloadBttn(
                         "downloadfile",
                         "Download this report",
                         style = "bordered",
                         color = "default"
                       )
                )
              )
              
              
              
            ),
            mainPanel(
              shinycssloaders::withSpinner(dataTableOutput("summary_na"))
              
              
            )
            
            
          ))
}