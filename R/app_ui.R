#' The application User-Interface
#' @import shinydashboard
#' @importFrom utils packageVersion
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyWidgets animateOptions dropdown
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(# Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      shinydashboard::dashboardPage(
        skin = "red",
        header = shinydashboard::dashboardHeader(title =
                                  paste0( "shinymde ",
                                            "v",
                                       packageVersion("shinymde")
                                                   )),
        sidebar = shinydashboard::dashboardSidebar(
          sidebarMenu(
            id = "shiny_mde",
            
           menu_render()
           
          )
          
        ),
        body = dashboardBody(
          shinyjs::useShinyjs(),
          shinyFeedback::useShinyFeedback(),
          
          tags$head(tags$style(
            HTML(
              ".info-box:hover,
    .btn:hover, .info-box-icon, .radio:hover,
    .option:hover, .odd:hover, .even:hover{
    background-color: #d73925 !important;
    color: white;
    }


    .bttn-bordered:hover{
    background: #fff;
    color:#d73925;
    }
    .bttn-fill.bttn-default:after{
    color: #fff;
    }
    .bttn-bordered, .bttn-fill.bttn-default:before,
    .bttn-fill.bttn-danger:before {
    background: #d73925;
    color: #fff;
    }
    "
            )
          )),
    
    
    
    tabItems(
      
      home_ui()
     ,
     input_ui()
      ,
      tabItem(tabName = "missingness_summary",
              
              sidebarLayout(
                sidebarPanel(
               width = 4, 
                        
                fluidRow(column(6,uiOutput("sort_by")),
                         column(6, 
                           selectize_input(
                            id= "sort_order",
                             label = "Sort Order",
                             choices = c("ascending",
                                         "descending"),
                             selected = "descending"
                           )
                           
                         ))
                        ,
                    
                  fluidRow(column(
                    5,
                    numeric_input("round_to", "Round to",
                                 value = options("digits"))
                  ),
                  column(
                    7,
                    selectize_input(
                     id = "group_by",
                      label = "Group BY",
                      choices = c("A", "B"),
                      multiple = TRUE,
                     selected = FALSE
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
                          
                          selectInput(
                            "select_kind",
                            "Selection Kind",
                            choices = c("exclusion",
                                        "inclusion"),
                            selected = FALSE,
                            selectize = FALSE,
                            size = 2
                          )
                          ,
                          selectInput(
                            "pattern_type_summary",
                            label = "Pattern type",
                            choices = c("contains", "starts_with",
                                        "ends_with", "regex"),
                            selected = FALSE,
                            selectize = FALSE,
                            size = 4
                          )
                          ,
                          
                          textInput("pattern_summary",
                                    label = "Pattern",
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
                
                
              )),
      
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
               selectInput(
                 "subset_cols",
                 "Subset",
                 choices = c("A", "B"),
                 multiple = TRUE
               ),
               selectInput(
                 "keep_columns",
                 "Keep Cols",
                 choices = c("A", 'B'),
                 multiple = TRUE
               )
               
               
               
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
        ) ),
      
      tabItem(
        tabName = "drop_values",
        div(
          id = "drop_zone",
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
                column(6,
                       numericInput("percent_na_drop",
                                    "Percent NA", value = 20)
                       ),
                column(6,
                       selectInput(
                         "sign",
                         "Sign",
                         choices = c("gt", "gteq", "lt", "lteq", "eq"),
                         selected = "gt",
                         multiple = FALSE
                       )
                       )
              ),
              fluidRow(
                column(9,selectInput(
                "group_by_drop",
                "Group BY",
                choices = c("A", "B"),
                multiple = TRUE
              ))),
              fluidRow(

                column(6,
                       selectInput(
                         "keep_columns_drop",
                         "Keep Cols",
                         choices = c("A", "B"),
                         multiple = TRUE
                       )
                       ),
                column(
                  6,
                  selectInput(
                    "target_cols",
                    "Target Cols",
                    choices = c("A", "B"),
                    multiple = TRUE
                  )
                )
                
              ),
              fluidRow(column(
                6,
                selectInput(
                  "pattern_type_drop",
                  "Pattern type",
                  choices = c("starts_with",
                              "ends_with", "contains",
                              "regex"),
                  selected = FALSE,
                  selectize = FALSE,
                  size = 4
                )
              ),
              column(
                6,
                textInput("pattern_drop", "Pattern", value = NULL)
              )),
              br(),
              
              shinyWidgets::downloadBttn(
                "downloadfile_drop",
                "Download this report",
                style = "bordered",
                color = "default"
              )
              
            ),
            mainPanel(
              shinycssloaders::withSpinner(dataTableOutput("drop_na"))
              
            )
          )
          

          
        )
      ),
      
      tabItem(
        tabName = "visual_summary",
        sidebarLayout(
          sidebarPanel(
                     fluidRow(
                       column(
                         6,
                         selectInput(
                           "plot_type",
                           "Type of plot",
                           choices = c("bar",
                                       "lollipop"),
                           selected = "bar"
                         )
                       ),
                       column(
                         6,
                         conditionalPanel(
                           condition = "input.plot_type=='bar'",
                           shinyWidgets::materialSwitch(
                             "show_text",
                             "Show Text?",
                             value = FALSE,
                             status = "primary"
                           )
                         ),
                         conditionalPanel(
                           condition = "input.plot_type=='lollipop'",
                           sliderInput(
                             "size",
                             "Size",
                             min = 0,
                             max = 5,
                             step = 0.2,
                             value = 2
                           )
                         )
                       )
                     ),
                     
                     div(
                       id = "plot_inputs_panel",
                       fluidRow(column(
                         6,
                         selectInput(
                           "y_variable",
                           "Y",
                           choices = c("A", "B"),
                           selected = "A"
                         )
                       ),
                       column(
                         6,
                         selectInput(
                           "x_variable",
                           "X",
                           choices = c("A", "B"),
                           selected = "B"
                         )
                       )),
                       fluidRow(column(
                         6,
                         selectInput(
                           "fill_variable",
                           "Fill",
                           choices = c("A", "B"),
                           selected = "A"
                         )
                       ),
                       column(
                         6,
                         numericInput("round_to_visual", "Round to",
                                      value = 2)
                       )),
                       style = "width:300px;"
                     )
                   
                   ,
                   br(), 
                   br(),
                   shinyWidgets::dropdown(
                     label = "Theming",
                     style = "bordered",
                     width = "340px",
                     animate = shinyWidgets::animateOptions(enter = "fadeInleft",
                                                            exit = "fadeOut"),
                     icon = icon("cog"),
                     
                     fluidRow(
                       fluidRow(column(
                         6,
                         selectizeInput(
                           "theme",
                           "Plot theme",
                           selected = "theme_minimal",
                           choices = c("theme_minimal",
                                       "theme_classic")
                         )
                       ),
                       column(
                         6, textInput("pkg",
                                      "Source package",
                                      value = "ggplot2")
                       )),
                       shinyWidgets::actionBttn(
                         inputId = "confirm_pkg",
                         label = "Confirm",
                         style = "bordered",
                         color = "default",
                         icon = icon("check")
                       )
                     )
                   ),
          br(),
          br(),
      fluidRow(
        column(6,
               shinyWidgets::dropdown(
                 label = "save",
                 style = "bordered",
                 animate = shinyWidgets::animateOptions(enter = "fadeInleft",
                                                        exit = "fadeOut"),
                 icon = icon("save"),
                 fluidRow(column(
                   6,
                   textInput("extension", "Save Format",
                             value = "png")
                 ),
                 column(
                   6,
                   textInput("dims", "Dimensions",
                             value = "1137x720")
                 )),
                 shinyWidgets::downloadBttn(
                   "download_plot",
                   "Save Plot",
                   style = "bordered",
                   color = "default"
                 ))
               ),
        column(6,
               shinyWidgets::actionBttn(
                 inputId = "plot_reset_button",
                 label = "Reset",
                 style = "bordered",
                 color = "default"
               )
               )
      ) ),

          mainPanel(
          shinycssloaders::withSpinner(plotOutput("visual_summary"))
          )
        )
       

      )
    )
        )
      )
    ))
  
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path('www', app_sys('app/www'))
  
  tags$head(favicon(),
            bundle_resources(path = app_sys('app/www'),
                             app_title = 'shinymde'))
  # Add here other external resources
  # for example, you can add shinyalert::useShinyalert() )
}
