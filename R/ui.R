#' Builds the user interface for shinymde. 
#' @return shinymde's user interface. 
#' @export
shinymde_ui <- fluidPage(
  shinyjs::useShinyjs(), 
  shinyFeedback::useShinyFeedback(), 
  tabsetPanel(id="shinymde",
              selected = "Input Data",
    tabPanel("Input Data",
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
                                          uiOutput("file_type"))),
            mainPanel(
              verbatimTextOutput("data_summary")
            ))),
    
            
    tabPanel("Summarise Missingness",
             sidebarLayout(
               
     
             sidebarPanel(
               uiOutput("sort_by"),
               selectInput("sort_order", "Sort Order",
                                     choices=c("ascending", 
                                               "descending"),
                           selected="descending"),
               numericInput("round_to", "Round to", 
                                      value = options("digits")),
               uiOutput("group_by"),
               uiOutput("exclude_columns")
               
             ),
             mainPanel(dataTableOutput("summary_na"),
                       
                downloadButton("downloadfile", "Download this report")))),
 
      tabPanel("Recode Values",
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
                   # 
                   # actionButton("reset", "Reset Dataset")
                   
                 ),
                 mainPanel(
                   dataTableOutput("recode_values"),
                   
                   downloadButton("downloadfile_recode", "Download this report"))
                 
               )), 
   
    tabPanel("Drop Values",
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
                                "Download this report")
                 
               )
             )),
    tabPanel("Visualise Missingness",
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
    
  )))
)))


