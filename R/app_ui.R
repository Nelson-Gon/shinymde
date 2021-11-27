#' The application User-Interface
#' @import shinydashboard
#' @importFrom utils packageVersion
#' @importFrom shinyBS bsTooltip
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
          collapsed = TRUE,
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
          # TODO: Make page selection same color as app skin.
          # tags$head(tags$style(shiny::HTML(
          #   ".dataTables_paginate .paginate_button.active {background-color: #dd4b39;}"
          # ))),
          
          tags$head(tags$style(
            HTML(
              ".info-box:hover,
    .btn:hover, .info-box-icon, .radio:hover,
    .option:hover, .odd:hover, .even:hover{
    background-color: #0073b7 !important;
    color: white;
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
              
              shinyWidgets::dropdownButton(
                inputId = "indata_button",
                circle = "TRUE",
                icon = icon("cog"),
                width = "400px",
                
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
                
                
                actionButton("confirm", "Confirm"),
                actionButton("reset_input", "Reset")
                
              ),
              
              
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
              fluidRow(
                column(
                  4,
                  shinyWidgets::dropdownButton(
                    label = "Sort",
                    circle = TRUE, 
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
                  
                  shinyWidgets::dropdownButton(
                    circle = TRUE, 
                    label = "Group",
                    icon = icon("cog"),
    
                    selectInput(
                      "group_by",
                      "Grouping Columns",
                      choices = c("A", "B"),
                      multiple = TRUE
                    ),
                    conditionalPanel(
                      "input.regex_based=='no'",
                      selectInput(
                        "exclude_columns",
                        "Columns to exclude",
                        choices = c("A", "B"),
                        multiple = TRUE
                      )
                    )
                  )
                ),
                column(
                  4,
                  shinyWidgets::dropdownButton(
                    label = "Subset",
                    circle = TRUE, 
                    icon = icon("cog"),
                  
                    selectInput(
                      "regex_based",
                      "Select columns based on RegEx?",
                      choices = c("yes", "no"),
                      selected = "no"
                    ),
                    
                    selectInput(
                      "select_kind",
                      "Nature of selection",
                      choices = c("exclusion",
                                  "inclusion"),
                      selected = FALSE,
                      selectize = FALSE,
                      size = 2
                    ),
                    
                    
                    selectInput(
                      "pattern_type_summary",
                      label = "Pattern type for regex",
                      choices = c("contains", "starts_with",
                                  "ends_with", "regex"),
                      selected = FALSE,
                      selectize = FALSE,
                      size = 4
                    ),
                    
                    
                    textInput("pattern_summary",
                              label = "Pattern to use for regex",
                              value = NULL)
                  )
                )
              ),
              
              
              
              shinycssloaders::withSpinner(dataTableOutput("summary_na")),
              
              downloadButton("downloadfile", "Download this report")
              
            )
            ,
            
            tabItem(
              tabName = "recode_values",
              fluidRow(column(6,
              shinyWidgets::dropdownButton(
                circle = TRUE, 
                icon = icon("cog"),
                label = "Recode",
    
                selectInput(
                  "recode_type",
                  "Kind of recoding",
                  choices = c(
                    "recode_as_na",
                    "recode_na_as",
                    "recode_as_na_for",
                    "recode_as_na_if"
                  ),
                  selected = "recode_as_na"
                ),
                
                
                textInput("value_to_recode", "Value"),
                
                selectInput(
                  "criteria",
                  "Criteria",
                  choices = c("gt", "lt",
                              "lteq", "gteq", "eq"),
                  selected = "gt"
                  
                ),
                
                selectInput(
                  "subset_cols",
                  "A subset to recode",
                  choices = c("A", "B"),
                  multiple = TRUE
                ),
                
                selectInput(
                  "keep_columns",
                  "Keep Columns",
                  choices = c("A", 'B'),
                  multiple = TRUE
                ),
                # need pattern_type and subset_cols not both so need
                # to set one to NULL
                # This in shiny is done like so
                # see stackoverflow.com/a/53698788/10323798
                selectInput(
                  "pattern_type",
                  "Pattern type",
                  choices = c("starts_with",
                              "ends_with", "contains",
                              "regex"),
                  selected = FALSE,
                  selectize = FALSE,
                  size = 4
                ),
                
                textInput("pattern", "Pattern", value = NULL)
                
              )),
              column(6,  
                     downloadButton("downloadfile_recode",
                                    "Download this report"))), 
              
              
              
              
              shinycssloaders::withSpinner(dataTableOutput("recode_values"))
              
             
              
            ),
            
            tabItem(
              tabName = "drop_values",
              fluidRow(column(6,shinyWidgets::dropdownButton(
                label = "Drop",
                circle = TRUE, 
                icon = icon("cog"),
                
                selectInput(
                  "drop_type",
                  "Kind of drop",
                  choices = c("drop_all_na",
                              "drop_na_if",
                              "drop_na_at"),
                  selected = "drop_all_na"
                ),
                
                numericInput("percent_na_drop",
                             "Percent NA", value = 20),
                
                selectInput(
                  "sign",
                  "Sign",
                  choices = c("gt", "gteq", "lt", "lteq", "eq"),
                  selected = "gt",
                  multiple = FALSE
                ),
                
                selectInput(
                  "group_by_drop",
                  "Grouping Columns",
                  choices = c("A", "B"),
                  multiple = TRUE
                ),
                
                selectInput(
                  "keep_columns_drop",
                  "Keep Columns",
                  choices = c("A", "B"),
                  multiple = TRUE
                ),
                
                selectInput(
                  "target_cols",
                  "Target Columns",
                  choices = c("A", "B"),
                  multiple = TRUE
                ),
                
                selectInput(
                  "pattern_type_drop",
                  "Pattern type",
                  choices = c("starts_with",
                              "ends_with", "contains",
                              "regex"),
                  selected = FALSE,
                  selectize = FALSE,
                  size = 4
                ),
                
                textInput("pattern_drop", "Pattern", value = NULL)
              )),column(6,
                       
                       downloadButton("downloadfile_drop",
                                      "Download this report"))),
              
              
              
              shinycssloaders::withSpinner(dataTableOutput("drop_na"))
              
              
            ),
            
            tabItem(
              tabName = "visual_summary",
              fluidRow(column(6,shinyWidgets::dropdownButton(
                icon = icon("cog"),
                circle = TRUE, 
                label = "Plot Settings",
               
                div(
                  id = "plot_area",
                  selectInput(
                    "plot_type",
                    "Type of plot",
                    choices = c("bar",
                                "lollipop"),
                    selected = "bar"
                  ),
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
                  ),
                  selectInput(
                    "y_variable",
                    "Y axis variable",
                    choices = c("A", "B"),
                    selected = "A"
                  ),
                  
                  selectInput(
                    "x_variable",
                    "X axis variable",
                    choices = c("A", "B"),
                    selected = "B"
                  ),
                  selectInput(
                    "fill_variable",
                    "Fill variable",
                    choices = c("A", "B"),
                    selected = "A"
                  ),
                  numericInput("round_to_visual", "Round to",
                               value = 2),
                )
              )),
              
              column(3, 
                shinyWidgets::dropdownButton(
                  circle = TRUE,
                  icon = icon("save"),
                  textInput("extension", "Save Format",
                            value = "png"),
                  
                  textInput("dims", "Dimensions",
                            value = "1137x720"),
                  
                  downloadButton("download_plot",
                                 "Save Plot")
                  
                )),
              column(3,
                     shinyWidgets::actionBttn(
                       inputId = "plot_reset_button",
                       label = "Reset",
                       style = "unite", 
                       color = "danger"
                     ))), 
              
              
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
