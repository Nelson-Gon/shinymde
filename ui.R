if (any(!c("shiny", "mde", "vroom", "dplyr", "ggplot2",
           "forcats") %in% installed.packages())){
  stop("Please install shiny package >=1.6.0, mde >= 0.3.1, 
       vroom >=1.5.3, dplyr >=1.0.7, ggplot2 >= 3.3.4, forcats>=0.5.1  
       and readxl >=1.3.1")
}
library(shiny)
ui <- fluidPage(
  shinyjs::useShinyjs(),
  tabsetPanel(
    tabPanel("Input Data",
             fluidRow(
               column(4, fileInput("input_file",
                                   label = "Please provide a file path"))
             )),
    tabPanel("Summarise Missingness",
             dataTableOutput("summary_na"),
             column(2, downloadButton("downloadfile", "Download")),
             fluidRow(
               column(2, uiOutput("sort_by")),
               column(2, selectInput("sort_order", "Sort Order",
                                     choices=c("ascending", "descending"))),
               column(2, uiOutput("group_by")),
               column(2, uiOutput("exclude_columns"))
               
             )),
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
                 selectInput("pattern_type", "Pattern type",
                                    choices = c("starts_with",
                                    "ends_with","contains",
                                    "regex"),
                                    selected = "regex")), 
               column(2,textInput("pattern", "Pattern", value=".*"))
               # TODO: Automate ui creation. 
               
             )),
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
                                  selected = "regex")),
               column(2,textInput("pattern_drop", "Pattern", value="^O"))

             )),
    tabPanel("Visualise Missingness",
                plotOutput("visual_summary"),
                fluidRow(
                  column(4, uiOutput("y_variable")),
                  column(4, uiOutput("x_variable")),
                  column(4, uiOutput("fill_variable"))
                ))
  ))


