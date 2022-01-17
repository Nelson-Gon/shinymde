visual_ui <- function() {
  
  tabItem(
  tabName = "visual_summary",
  
  
  sidebarLayout(
    sidebarPanel(
      

  fluidRow(
      column(6, 
        shinyWidgets::dropdown(
          label = "",
          size = "sm", 
          style = "bordered",
          animate = shinyWidgets::animateOptions(enter = "fadeInleft",
                                                 exit = "fadeOut"),
          icon = icon("save"),
          fluidRow(
            two_columns(
              textInput("extension", "Save Format",
                        value = "png"),
              textInput("dims", "Dimensions",
                        value = "1137x720")
            )))),
      column(6, 
          shinyWidgets::downloadBttn(
            "download_plot",
            label = "",
            size = "sm", 
            style = "bordered",
            color = "default"
          ))

      
),

br(),
fluidRow(column(6, selectInput(
  "plot_type",
  "Type of plot",
  choices = c("bar",
              "lollipop"),
  selected = "bar")),
  column(6,
conditionalPanel(
  condition = "input.plot_type=='bar'",
  shiny::checkboxInput(
    "show_text",
    "Text labels",
    value = FALSE
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
))),

fluidRow(
  column(6, 
         selectizeInput(
           "theme",
           "Plot theme",
           selected = "theme_minimal",
           choices = c("theme_minimal",
                       "theme_classic")
         )),
  column(6, 
         textInput("pkg",
                   "Source package",
                   value = "ggplot2")
  )
  
  
),

fluidRow(
column(6,selectInput(
           "y_variable",
           "Y",
           choices = c("A", "B"),
           selected = "A"
         )),
         column(6,
                selectInput(
                  "x_variable",
                  "X",
                  choices = c("A", "B"),
                  selected = "B"
                )
         )),

fluidRow( two_columns(
           selectInput(
             "fill_variable",
             "Fill",
             choices = c("A", "B"),
             selected = "A"
           ),
           
           numericInput("round_to_visual", "Round to",
                        value = 2)
         ))
         
  ),
    mainPanel(
      fillPage(
        tags$style(type = "text/css", "#visual_summary
                   {height: calc(100vh - 80px) !important;}"),
        shinycssloaders::withSpinner(plotOutput("visual_summary"))   
      )
       
      
    )
  )
  
 
   
  )
  
  

  
}