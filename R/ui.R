#' Builds the user interface for shinymde. 
#' @import shinydashboard 
#' @importFrom utils packageVersion
#' @return shinymde's user interface. 
#' @export

shinymde_ui <- shinydashboard::dashboardPage(
  skin = "blue", 
  header = shinydashboard::dashboardHeader(title=
                                             paste0("shinymde ",
                                                    "v",
                                              packageVersion("shinymde"))),
  sidebar= shinydashboard::dashboardSidebar(
    
    sidebarMenu(
      # This id allows us to access the currently active tab as in tabsetpanel
      id = "shiny_mde", 
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
              tags$br(), 
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
                        color = "blue", width = 5)
               
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
                conditionalPanel(condition="input.data_source == 'user_data'",
                                 numericInput("sheet", "Sheet", value=1)), 
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
                shinyBS::bsTooltip(id = "data_source",
                                   title = "Choose a dataset source."),
                shinyBS::bsTooltip(id="remote", 
                                    title="Link to a remote dataset."), 
                shinyBS::bsTooltip(id="confirm", 
                                   title="Click to confirm input."),
                shinyBS::bsTooltip(id="reset_input", 
                                   title = "Click to restore defaults."),
                shinyBS::bsTooltip(id="dataset", title="Choose a dataset.",
                                   placement = "top"),
                shinyBS::bsTooltip(id="input_file",
                                   title="Path to a csv, tsv, or xlsx file."
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
              
                verbatimTextOutput("data_summary"),
                shinyBS::bsTooltip(id="data_summary", 
                             title = "A statistical summary of input data.")
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
                              uiOutput("exclude_columns"),
                              shinyBS::bsTooltip("sort_by",
                                      title = "Select sort column.",
                                      placement = "top"),
                              shinyBS::bsTooltip(id="sort_order",
                                                 title="Choose sort order.",
                                                 placement = "top"),
                              shinyBS::bsTooltip(id="round_to",
                                        title = "Number of decimal places"),
                              shinyBS::bsTooltip(id="group_by",
                                          title = "Choose columns to group by.",
                                          placement = "top"),
                              shinyBS::bsTooltip(id="exclude_columns",
                                    title = "Columns to exclude from analysis.",
                                    placement = "top")
                              ),
                mainPanel( 
                  dataTableOutput("summary_na"),
                  shinyBS::bsTooltip(id="summary_na",
                                title = "A tabular summary of missingness."), 
                  
                  downloadButton("downloadfile", "Download this report"),
                  shinyBS::bsTooltip(id="downloadfile",
                                     title = "Click to save this report.")
                  
              ))),
              
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
                    shinyBS::bsTooltip(id="recode_type",
                                  title = "Select the kind of recoding.",
                                  placement = "top"),
                    
                    textInput("value_to_recode", "Value"),
                    shinyBS::bsTooltip(id="value_to_recode",
                          title = "Comma separated values to recode.",
                          placement = "top"),
                    uiOutput("criteria"),
                    shinyBS::bsTooltip(id="criteria",
                            title ="Criteria to use e.g. gt ~ greater than.",
                            placement = "top"),
                    uiOutput("subset_cols"),
                    shinyBS::bsTooltip(id="subset_cols",
                                title ="A subset of columns to recode.",
                                placement = "top"),
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
                    # This would only work if we set selectize to TRUE? 
                    # shinyBS::bsTooltip(id="pattern_type",
                    #                    title ="Pattern type to use for 
                    #                    RegEX subset.",
                    #                    placement = "top"), 
                    textInput("pattern", "Pattern", value=NULL),
                    shinyBS::bsTooltip(id="pattern",
                                    title ="Pattern to use for RegEx subsets.")
                  ),
                  mainPanel(
                    dataTableOutput("recode_values"),
                    shinyBS::bsTooltip(id="recode_values",
                                       title = "A table with recoded values."),
                    
                  downloadButton("downloadfile_recode", "Download this report"),
                  shinyBS::bsTooltip(id="downloadfile_recode",
                                title = "Click to save the recoded dataset.")
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
                          shinyBS::bsTooltip(id="drop_type",
                                          title = "Select the kind of drop.",
                                          placement = "top"),
                          numericInput("percent_na_drop",
                                       "Percent NA", value=20),
                          shinyBS::bsTooltip(id="percent_na_drop",
                                          title ="Input percent NA criteria."),
                          uiOutput("sign"),
                          shinyBS::bsTooltip("sign",
                              title = "Criteria e.g. gt ~ greater than.",
                              placement = "top"), 
                          uiOutput("group_by_drop"),
                          shinyBS::bsTooltip(id="group_by_drop",
                                             title ="Columns to group by.",
                                             placement = "top"), 
                          uiOutput("keep_columns_drop"),
                          shinyBS::bsTooltip(id="keep_columns_drop",
                          title = "Keep these columns regardless of criteria",
                          placement = "top"),
                          uiOutput("target_cols"),
                          shinyBS::bsTooltip(id="target_cols",
                                      title ="Target columns to drop at.",
                                      placement = "top"),
                          selectInput("pattern_type_drop", "Pattern type",
                                      choices = c("starts_with",
                                                  "ends_with","contains",
                                                  "regex"),
                                      selected = FALSE,
                                      selectize = FALSE, 
                                      size = 4),
                          shinyBS::bsTooltip(id = "pattern_type_drop",
                                       title = "Pattern type for RegEx drop.",
                                       placement = "top"), 
                          textInput("pattern_drop", "Pattern", value=NULL),
                          shinyBS::bsTooltip(id ="pattern_drop",
                                             title = "Pattern for RegEx drop.")
                        ),
                        mainPanel(
                          dataTableOutput("drop_na"),
                          shinyBS::bsTooltip(id="drop_na",
                                             title = "Table with NAs dropped."),
                          downloadButton("downloadfile_drop", 
                                         "Download this report"),
                          shinyBS::bsTooltip(id="downloadfile_drop",
                                             title ="Click to save this table.")
                         
                        )
                      )), 
              
             tabItem(tabName = "visual_summary",
                     sidebarLayout(
                       sidebarPanel(
                         selectInput("plot_type",
                                     "Type of plot",
                                     choices = c("bar",
                                                 "lollipop"),
                                     selected = "bar"),
                         conditionalPanel(condition="input.plot_type=='bar'",
                                          selectInput("show_text",
                                                      "Show Text?",
                                                      choices = c("yes",
                                                                  "no"),
                                                      selected = "no")), 
                         conditionalPanel(condition="input.plot_type=='lollipop'",
                                          sliderInput("size", "Size",min=0,
                                                      max = 5, step = 0.2,
                                                      value = 2)), 
                         uiOutput("y_variable"),
                                    shinyBS::bsTooltip(id="y_variable",
                                    title = "Variable to use on the Y axis"),
                                    uiOutput("x_variable"),
                                    uiOutput("fill_variable"),
                                    numericInput("round_to_visual", "Round to", 
                                                 value = 2))
                       ,
                       mainPanel(plotOutput("visual_summary"),
                                 shinyBS::bsTooltip(id="visual_summary",
                                  title = "A visual summary of missingness."),
                                 shinyBS::bsTooltip(id = "plot_type",
                                            title="Type of plot to render.",
                                            placement = "top"), 
                                 shinyBS::bsTooltip(id="show_text",
                                        title="Should bars have text labels?",
                                        placement = "top"),
                                 shinyBS::bsTooltip(id="size",
                                                    title="Lollipop size"), 
                                 
                                 shinyBS::bsTooltip(id="x_variable",
                                                    title="Value on  X axis",
                                                    placement = "top"),
                                 shinyBS::bsTooltip(id="y_variable",
                                                    title="Value on Y axis",
                                                    placement = "top"),
                                 shinyBS::bsTooltip(id = "fill_variable",
                                               title="Value to colour bars.",
                                               placement = "top"),
                                 shinyBS::bsTooltip(id="round_to_visual",
                                  title="For text, number of decimal places."), 
                                 
                                 
                                 fluidRow(
                                   
                                   column(4,textInput("extension", "Save Format", 
                                                      value="png")),
                                   
                                   shinyBS::bsTooltip(id="extension",
                                      title = "Save plot in this format"),
                                   
                                   column(4,textInput("dims", "Dimensions", 
                                                      value="1137x720")),
                                   shinyBS::bsTooltip(id="dims",
                                              title = "Plot save dimensions."),
                                   column(4,downloadButton("download_plot", 
                                                           "Save Plot")),
                                   shinyBS::bsTooltip(id = "download_plot",
                                                      
                                              title = "Click to save plot."),
                                   actionButton("reset_opts", 
                                                "Restore Defaults"),
                                   shinyBS::bsTooltip(id="reset_opts",
                                          title = "Click to restore defaults.")
                                   
                              
                                 )
                                 ))
                     ))))
                
             
 
              
   
  


