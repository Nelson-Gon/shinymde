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
                                                   paste0(
                                                     "shinymde ",
                                                     "v",
                                                     packageVersion("shinymde")
                                                   )),
        sidebar = shinydashboard::dashboardSidebar(
          sidebarMenu(
            # This id allows us to access the currently active tab as in tabsetpanel
            id = "shiny_mde",
            menuItem("Home", tabName = "home", icon = shiny::icon("home")),
            menuItem(
              "Input Data",
              tabName = "input",
              icon = shiny::icon("database")
            ),
            menuItem(
              "Summarise Missingness",
              tabName = "missingness_summary",
              icon = shiny::icon("table")
            ),
            menuItem(
              "Recode Values",
              tabName = "recode_values",
              icon = shiny::icon("exchange-alt")
            ),
            menuItem(
              "Drop Values",
              tabName = "drop_values",
              icon = shiny::icon("eraser")
            ),
            menuItem(
              "Visualise Missingness",
              tabName = "visual_summary",
              icon = shiny::icon("chart-bar")
            )
            
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
      tabItem(
        tabName = "home",
        div(id = "welcome",
            strong(
              tags$p("Welcome to shinymde!",
                     style = "font-size:20px;")
            )),
        tags$br(),
        fluidRow(
          infoBox(
            title = "Documentation",
            value = "Read Project Documentation",
            href = "https://nelson-gon.github.io/shinymde",
            icon = shiny::icon("book"),
            color = "blue",
            width = 6
          ),
          
          infoBox(
            title = "Contribute",
            value = "Nelson-Gon/shinymde",
            href = "https://github.com/Nelson-Gon/shinymde",
            icon = shiny::icon("laptop"),
            color = "blue",
            width = 5
          )
        )
        ,
        fluidRow(
          tags$br(),
          tags$br(),
          tags$br(),
          
          infoBox(
            title = "Author",
            value = "Nelson Gonzabato",
            icon = shiny::icon("robot"),
            href = "https://nelson-gon.github.io",
            color = "blue",
            width = 6
          ),
          
          infoBox(
            title = "Related projects",
            value = "View related projects",
            href = "https://nelson-gon.github.io/projects",
            icon = shiny::icon("tools"),
            color = "blue",
            width = 5
          )
          
        )
        
      ),
      tabItem(
        tabName = "input",
        fluidRow(
          column(
            6,
            shinyWidgets::dropdown(
              inputId = "indata_button",
              style = "bordered",
              label = "Input Data",
              icon = icon("cog"),
              width = "250px",
              animate = shinyWidgets::animateOptions(enter = "fadeInLeft",
                                                     exit = "fadeOut"),
              
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
                selectInput(
                  "dataset",
                  "Dataset",
                  choices = c("mtcars", "airquality"),
                  selected = "airquality"
                )
              ),
              conditionalPanel(condition =
                                 "input.data_source == 'remote'",
                               textInput("remote",
                                         "Remote",
                                         value = "")),
              conditionalPanel(
                condition =
                  "input.data_source == 'remote'",
                selectInput(
                  "file_type",
                  "File Type",
                  choices = c("csv", "tsv"),
                  selected = "csv"
                )
              ),
              shinyWidgets::actionBttn(
                "confirm_in",
                label = "confirm",
                color = "default",
                style = "bordered",
                icon = icon("check")
              )
            )
          ),
          column(
            6,
            shinyWidgets::actionBttn(
              "reset_input",
              label = "Reset",
              style = "bordered",
              icon = icon("undo"),
              color = "default"
            )
          )
        )
        
        
        ,
        br(),
        br(),
        
        div(
          id = "sys_details",
          
          infoBox(
            title = "Date of Analysis",
            icon = shiny::icon("calendar"),
            value = format(Sys.Date(), "%A %b %d %Y"),
            width = 12
          ),
          infoBox(
            title = "R version",
            value =
              R.version.string,
            icon = shiny::icon("cog"),
            width = 12
          ),
          infoBox(
            title = "mde version",
            value =
              as.character(packageVersion("mde")),
            icon = shiny::icon("tools"),
            width = 12
          )
        ),
        
        verbatimTextOutput("data_summary")
      ),
      tabItem(
        tabName = "missingness_summary",
        div(
          id = "sort_group_subset",
          fluidRow(
            column(
              4,
              shinyWidgets::dropdown(
                label = "Sort",
                style = "bordered",
                width = "250px",
                animate = shinyWidgets::animateOptions(enter = "fadeInLeft",
                                                       exit = "fadeOut"),
                icon = icon("cog"),
                
                uiOutput("sort_by"),
                selectInput(
                  "sort_order",
                  "Sort Order",
                  choices = c("ascending",
                              "descending"),
                  selected = "descending"
                ),
                numericInput("round_to", "Round to",
                             value = options("digits"))
              )
            ),
            
            column(
              4,
              shinyWidgets::dropdown(
                style = "bordered",
                label = "Group",
                width = "250px",
                icon = icon("cog"),
                
                selectInput(
                  "group_by",
                  "Group BY",
                  choices = c("A", "B"),
                  multiple = TRUE
                )
                
              )
            ),
            
            column(
              4,
              shinyWidgets::dropdown(
                label = "Subset",
                style = "bordered",
                width = "250px",
                icon = icon("cog"),
                animate = shinyWidgets::animateOptions(enter = "fadeInLeft",
                                                       exit = "fadeOut"),
                selectInput(
                  "select_kind",
                  "Selection Kind",
                  choices = c("exclusion",
                              "inclusion"),
                  selected = FALSE,
                  selectize = FALSE,
                  size = 2
                ),
                
                selectInput(
                  "pattern_type_summary",
                  label = "Pattern type",
                  choices = c("contains", "starts_with",
                              "ends_with", "regex"),
                  selected = FALSE,
                  selectize = FALSE,
                  size = 4
                ),
                
                
                textInput("pattern_summary",
                          label = "Pattern",
                          value = NULL)
              )
            )
          ),
          style = "margin-top:6px;margin-left:-12px;width:400px;"
        ),
        br(),
        br(),
        
        shinycssloaders::withSpinner(dataTableOutput("summary_na")),
        shinyWidgets::downloadBttn(
          "downloadfile",
          "Download this report",
          style = "bordered",
          color = "default"
        )
        
        
      ),
      
      tabItem(
        tabName = "recode_values",
        fluidRow(
          column(
            6,
            shinyWidgets::dropdown(
              style = "bordered",
              width = "380px",
              animate = shinyWidgets::animateOptions(enter = "fadeInLeft", exit = "fadeOut"),
              icon = icon("cog"),
              label = "Recode",
              fluidRow(
                column(
                  6,
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
                  )
                ),
                
                
                column(3, textInput("value_to_recode", "Value")),
                
                column(
                  3,
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
                6,
                selectInput(
                  "subset_cols",
                  "Subset",
                  choices = c("A", "B"),
                  multiple = TRUE
                )
              ),
              column(
                6,
                selectInput(
                  "keep_columns",
                  "Keep Cols",
                  choices = c("A", 'B'),
                  multiple = TRUE
                )
              )),
              # need pattern_type and subset_cols not both so need
              # to set one to NULL
              # This in shiny is done like so
              # see stackoverflow.com/a/53698788/10323798
              fluidRow(column(
                6,
                selectInput(
                  "pattern_type",
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
                textInput("pattern", "Pattern", value = NULL)
              ))
              
            )
          ),
          column(
            6,
            shinyWidgets::downloadBttn(
              "downloadfile_recode",
              "Download this report",
              style = "bordered",
              color = "default"
            )
          )
        ),
        
        br(),
        br(),
        
        
        shinycssloaders::withSpinner(dataTableOutput("recode_values"))
        
        
        
      ),
      
      tabItem(
        tabName = "drop_values",
        div(
          id = "drop_zone",
          
          fluidRow(
            column(
              6,
              shinyWidgets::dropdown(
                label = "Drop",
                style = "bordered",
                width = "400px",
                animate = shinyWidgets::animateOptions(enter = "fadeInLeft", exit = "fadeOut"),
                icon = icon("cog"),
                
                fluidRow(
                  column(
                    4,
                    selectInput(
                      "drop_type",
                      "Kind of drop",
                      width = "180px",
                      choices = c("drop_all_na",
                                  "drop_na_if",
                                  "drop_na_at"),
                      selected = "drop_na_if"
                    )
                  ),
                  column(4,
                         numericInput("percent_na_drop",
                                      "Percent NA", value = 20)),
                  column(
                    4,
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
                  column(
                    4,
                    selectInput(
                      "group_by_drop",
                      "Group BY",
                      choices = c("A", "B"),
                      multiple = TRUE
                    )
                  ),
                  column(
                    4,
                    selectInput(
                      "keep_columns_drop",
                      "Keep Cols",
                      choices = c("A", "B"),
                      multiple = TRUE
                    )
                  ),
                  column(
                    4,
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
                ))
              )
            ),
            column(
              6,
              
              shinyWidgets::downloadBttn(
                "downloadfile_drop",
                "Download this report",
                style = "bordered",
                color = "default"
              )
            )
          ),
          br(),
          br(),
          
          
          shinycssloaders::withSpinner(dataTableOutput("drop_na"))
          
          
        )
      ),
      
      tabItem(
        tabName = "visual_summary",
        fluidRow(
          column(
            6,
            shinyWidgets::dropdown(
              icon = icon("cog"),
              style = "bordered",
              animate = shinyWidgets::animateOptions(enter = "fadeInLeft",
                                                     exit = "fadeOut"),
              label = "Plot Settings",
              
              
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
                    selectInput(
                      "show_text",
                      "Show Text?",
                      choices = c("yes",
                                  "no"),
                      selected = "no"
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
                  3,
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
            )
          ),
          column(3,
                 shinyWidgets::dropdown(
                   label="Theming",
                   style = "bordered",
                   animate = shinyWidgets::animateOptions(
                     enter="fadeInleft",
                     exit = "fadeOut"
                   ),
                   icon = icon("cog"),
                   
                   fluidRow(
                     fluidRow(
                     column(6,
                          selectizeInput("theme", "Plot theme", 
                                         selected = "theme_minimal",
                                         choices = c("theme_minimal",
                                                     "theme_classic"))),
                     column(6, textInput("pkg",
                                         "Source package",
                                         value = "ggplot2"))
                   ),
                   shinyWidgets::actionBttn(
                     inputId = "confirm_pkg",
                     label = "Confirm",
                     style = "bordered",
                     color = "default",
                     icon = icon("check")
                   ))
                 )),
          column(
            3,
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
              )
              
            )
          ),
          column(
            3,
            shinyWidgets::actionBttn(
              inputId = "plot_reset_button",
              label = "Reset",
              style = "bordered",
              color = "default"
            )
          )
        ),
        
        br(),
        
        shinycssloaders::withSpinner(plotOutput("visual_summary")),
        
        
        
        
        
        
        
        
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
