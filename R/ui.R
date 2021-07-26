#' Builds the user interface for shinymde. 
#' @return shinymde's user interface. 
#' @export
shinymde_ui <- fluidPage(
  shinyjs::useShinyjs(),
  tabsetPanel(id="shinymde",
              selected = "Input Data",
    tabPanel("Input Data",
             fluidRow(
               column(6, selectInput("data_source", "Data Source",
                                     choices = c("inbuilt",
                                                 "remote",
                                                 "user_data"),
                                     selected = "inbuilt")),
               column(6, conditionalPanel(condition =
                                    "input.data_source == 'user_data'",
                                          uiOutput("input_file"))),
               column(6,  conditionalPanel(condition=
                                  "input.data_source=='inbuilt'",
                      uiOutput("dataset"))),
               column(3, conditionalPanel(condition = 
                                  "input.data_source == 'remote'",
                                  uiOutput("remote"))),
               column(3, conditionalPanel(condition = 
                                            "input.data_source == 'remote'",
                                          uiOutput("file_type"))))),
             # tags$h5("shinymde, freely brought to you by"),
             # div(
             #   strong(tags$p("Nelson Gonzabato")), 
             #   style="text-align: left;"),
             # div(tags$a("https://nelson-gon.github.io"),
             #     style="text-align: left")),
            
    tabPanel("Summarise Missingness",
             dataTableOutput("summary_na"),
             fluidRow(
               column(2, uiOutput("sort_by")),
               column(2, selectInput("sort_order", "Sort Order",
                                     choices=c("ascending", "descending"))),
               column(2, numericInput("round_to", "Round to", 
                                      value = options("digits"))),
               column(2, uiOutput("group_by")),
               column(2, uiOutput("exclude_columns"))
               
             ), 
             downloadButton("downloadfile", "Download this report")),
    tabPanel("Recode Values",
             dataTableOutput("recode_values"),
             fluidRow(
               column(2, selectInput("recode_type", "Kind of recoding",
                                     choices = c("recode_as_na",
                                                 "recode_na_as",
                                                 "recode_as_na_for",
                                                 "recode_as_na_if"),
                                     selected="recode_as_na")),
               column(2, textInput("value_to_recode", "Value")),
              
               column(2, uiOutput("criteria")),
               column(2, uiOutput("subset_cols")),
               column(2,
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
                             size = 4)), 
               column(2,textInput("pattern", "Pattern", value=NULL))
               ),
             downloadButton("downloadfile_recode", "Download this report")),
    tabPanel("Drop Values",
             dataTableOutput("drop_na"),
             fluidRow(
               column(2, selectInput("drop_type", "Kind of drop",
                                     choices = c("drop_all_na",
                                                 "drop_na_if",
                                                 "drop_na_at"),
                                     selected="drop_all_na")),
               
               column(2, numericInput("percent_na_drop",
                                      "Percent NA", value=20)),
               column(2, uiOutput("sign")),
               column(2, uiOutput("group_by_drop")),
               column(2, uiOutput("keep_columns_drop")),
               column(2, uiOutput("target_cols")),
              
               column(2,
                      selectInput("pattern_type_drop", "Pattern type",
                                  choices = c("starts_with",
                                              "ends_with","contains",
                                              "regex"),
                                  selected = FALSE,
                                  selectize = FALSE, 
                                  size = 4)),
               column(2,textInput("pattern_drop", "Pattern", value=NULL))

             ),
             downloadButton("downloadfile_drop", "Download this report")),
    tabPanel("Visualise Missingness",
                plotOutput("visual_summary"),
                fluidRow(
                  column(4, uiOutput("y_variable")),
                  column(4, uiOutput("x_variable")),
                  column(2, uiOutput("fill_variable")),
                  column(2, numericInput("round_to_visual", "Round to", 
                                         value = 2))
                ))
  ))


