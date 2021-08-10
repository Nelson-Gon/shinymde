#' Builds the user interface for shinymde. 
#' @import shinydashboard 
#' @importFrom utils packageVersion
#' @return shinymde's user interface. 
#' @export

shinymde_ui <- shinydashboard::dashboardPage(
  header = shinydashboard::dashboardHeader(title=
                                             paste0("shinymde ",
                                                    "v",
                                              packageVersion("shinymde"))),
  sidebar= shinydashboard::dashboardSidebar(
    
    sidebarMenu(
      menuItem("Home", tabName = "home", icon=shiny::icon("home")),
      menuItem("Input Data", tabName = "input", 
               icon = shiny::icon("database")),
      menuItem("Summarise Missingness", tabName = "missingness_summary",
               icon = shiny::icon("table")),
      menuItem("Recode Values", tabName = "recode_values",
               icon = shiny::icon("exchange")),
      menuItem("Drop Values", tabName = "drop_values",
               icon = shiny::icon("eraser")),
      menuItem("Visualise Missingness", tabName = "visual_summary",
               icon = shiny::icon("chart-bar") )
      
    )
  
  ),
  body=dashboardBody(
    shinyjs::useShinyjs(), 
    shinyFeedback::useShinyFeedback(), 
    # TODO: Make page selection same color as app skin. 
    # tags$head(tags$style(shiny::HTML(
    #   ".dataTables_paginate .paginate_button.active {background-color: #dd4b39;}"
    # ))), 
   
    tags$head(tags$style(HTML(".info-box:hover,
    .btn:hover, .info-box-icon, .radio:hover,
    .option:hover, .odd:hover, .even:hover{
    background-color: #0073b7 !important; 
    color: white;
    }
    "))), 

    tabItems(
      tabItem(tabName = "home",
              div(id="welcome",
                  strong(tags$p("Welcome to shinymde!",
                                style="font-size:20px;"))),
              
              fluidRow(
                infoBox(title="Documentation",
                        value = "Read Project Documentation",
                        href = "https://nelson-gon.github.io/shinymde",
                        icon = shiny::icon("book"),
                        color = "blue", width = 6), 
                
                infoBox(title="Contribute",
                        value = "Nelson-Gon/shinymde",
                        href = "https://github.com/Nelson-Gon/shinymde",
                        icon = shiny::icon("laptop"),
                        color = "blue", width = 5)
              )
              ,
              fluidRow(
                tags$br(), 
                tags$br(),
                tags$br(),
               
                infoBox(title = "Author", value="Nelson Gonzabato",
                        icon = shiny::icon("robot"),
                        href="https://nelson-gon.github.io", 
                        color = "blue", width = 6),
               
                infoBox(title="Related projects",
                        value = "View related projects",
                        href="https://nelson-gon.github.io/projects",
                        icon = shiny::icon("tools"),
                        color = "blue", width = 5),
               
              )
              
              ),
      tabItem(tabName = "input",
             
              sidebarLayout(
              
              sidebarPanel(
                radioButtons("data_source", "Data Source",
                             choices = c("inbuilt",
                                         "remote",
                                         "user_data"),
                             selected = "inbuilt"),
                conditionalPanel(condition =
                                   "input.data_source == 'user_data'",
                                 uiOutput("input_file")),
                conditionalPanel(condition=
                                   "input.data_source=='inbuilt'",
                                 uiOutput("dataset")),
                conditionalPanel(condition = 
                                   "input.data_source == 'remote'",
                                 uiOutput("remote")),
                conditionalPanel(condition = 
                                   "input.data_source == 'remote'",
                                 uiOutput("file_type")), 
                        
                actionButton("confirm","Confirm"),
                actionButton("reset_input", "Reset"),
                shinyBS::bsTooltip(id="data_source",
                                   title = "Choose a dataset source."), 
                shinyBS::bsTooltip(
                  id = "confirm",
                  title = "Click to confirm input.",
                  trigger = "hover",
                  placement = "bottom"
                ),
                shinyBS::bsTooltip(
                  id = "reset_input",
                  title = "Click to reset input to defaults.",
                  trigger = "hover",
                  placement = "bottom"
                ),
                
                shinyBS::bsTooltip(
                  id = "input_file",
                  title = "Click to select a csv, tsv, or xlsx file.",
                  trigger = "hover",
                  placement = "bottom"
                )
                
                ),
              mainPanel(
                div(id="sys_details",
                  
                   infoBox(title="Date of Analysis",
                            icon = shiny::icon("calendar"),
                            value = format(Sys.Date(), "%A %b %d %Y"),
                           width = 12),
                    infoBox(title="R version", value = 
                              R.version.string,
                            icon=shiny::icon("gear"),
                            width = 12),
                   infoBox(title="mde version", value = 
                             as.character(packageVersion("mde")),
                           icon=shiny::icon("tools"),
                           width = 12)
                    ), 
              
                verbatimTextOutput("data_summary")
               ))),
              tabItem(
              tabName = "missingness_summary",
              sidebarLayout(
                sidebarPanel( uiOutput("sort_by"),
                              selectInput("sort_order", "Sort Order",
                                          choices=c("ascending", 
                                                    "descending"),
                                          selected="descending"),
                              numericInput("round_to", "Round to", 
                                           value = options("digits")),
                              uiOutput("group_by"),
                              uiOutput("exclude_columns")),
                mainPanel( 
                  dataTableOutput("summary_na"),
                  
                  downloadButton("downloadfile", "Download this report"),
                  shinyBS::bsTooltip(id="downloadfile",
                                      title = "Click to save file in input format."))
              )),
              
              tabItem(
                tabName = "recode_values",
                sidebarLayout(
                  sidebarPanel(
                    selectInput("recode_type", "Kind of recoding",
                                choices = c("recode_as_na",
                                            "recode_na_as",
                                            "recode_as_na_for",
                                            "recode_as_na_if"),
                                selected="recode_as_na"),
                    textInput("value_to_recode", "Value"),
                    
                    uiOutput("criteria"),
                    uiOutput("subset_cols"),
                    # need pattern_type and subset_cols not both so need
                    # to set one to NULL
                    # This in shiny is done like so 
                    # see stackoverflow.com/a/53698788/10323798
                    selectInput("pattern_type", "Pattern type",
                                choices = c("starts_with",
                                            "ends_with","contains",
                                            "regex"),
                                selected = FALSE,
                                selectize = FALSE,
                                size = 4), 
                    textInput("pattern", "Pattern", value=NULL)
                  ),
                  mainPanel(
                    dataTableOutput("recode_values"),
                    
                    downloadButton("downloadfile_recode", "Download this report"),
                    shinyBS::bsTooltip(id="downloadfile_recode",
                                       title = "Click to save file in input format.")
                  )
                )),
              
              tabItem(tabName = "drop_values",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("drop_type", "Kind of drop",
                                      choices = c("drop_all_na",
                                                  "drop_na_if",
                                                  "drop_na_at"),
                                      selected="drop_all_na"),
                          numericInput("percent_na_drop",
                                       "Percent NA", value=20),
                          uiOutput("sign"),
                          uiOutput("group_by_drop"),
                          uiOutput("keep_columns_drop"),
                          uiOutput("target_cols"),
                          selectInput("pattern_type_drop", "Pattern type",
                                      choices = c("starts_with",
                                                  "ends_with","contains",
                                                  "regex"),
                                      selected = FALSE,
                                      selectize = FALSE, 
                                      size = 4),
                          textInput("pattern_drop", "Pattern", value=NULL)
                        ),
                        mainPanel(
                          dataTableOutput("drop_na"),
                          downloadButton("downloadfile_drop", 
                                         "Download this report"),
                          shinyBS::bsTooltip(id="downloadfile_drop",
                                             title = "Click to save file in input format.")
                        )
                      )), 
              
             tabItem(tabName = "visual_summary",
                     sidebarLayout(
                       sidebarPanel(uiOutput("y_variable"),
                                    uiOutput("x_variable"),
                                    uiOutput("fill_variable"),
                                    numericInput("round_to_visual", "Round to", 
                                                 value = 2))
                       ,
                       mainPanel(plotOutput("visual_summary"),
                                 
                                 fluidRow(
                                   
                                   column(4,textInput("extension", "Save Format", value="png")),
                                   
                                   column(4,textInput("dims", "Dimensions", 
                                                      value="1137x720")),
                                   column(4,downloadButton("download_plot", 
                                                           "Save Plot")),
                                   actionButton("reset_opts", "Restore Defaults")
                                   
                                 ),
                                 shinyBS::bsTooltip(id="reset_opts",
                                            title = "Click to restore defaults."),
                                 shinyBS::bsTooltip(id="download_plot",
                                                  title = "Click to save plot."),
                                 shinyBS::bsTooltip(id="dims",
                                 title = "Input save dimensions as WidthxHeight."),
                                 
                                 ))
                     ))))
                
             
 
              
   
  


