visual_ui <- function() {
  
  tabItem(
  tabName = "visual_summary",
   div(id="visual_controls",
      fluidRow(
        
        two_columns( selectInput(
          "plot_type",
          "Type of plot",
          choices = c("bar",
                      "lollipop"),
          selected = "bar"), 
        
        
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
      
        fluidRow(
          
          two_columns(
            selectInput(
              "y_variable",
              "Y",
              choices = c("A", "B"),
              selected = "A"
            ),
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
          fluidRow(
            two_columns(
              selectizeInput(
                "theme",
                "Plot theme",
                selected = "theme_minimal",
                choices = c("theme_minimal",
                            "theme_classic")
              ),
              textInput("pkg",
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
        two_columns(
          shinyWidgets::dropdown(
            label = "save",
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
            )),
            shinyWidgets::downloadBttn(
              "download_plot",
              "Save Plot",
              style = "bordered",
              color = "default"
            )),
          
          shinyWidgets::actionBttn(
            inputId = "plot_reset_button",
            label = "Reset",
            style = "bordered",
            color = "default"
          )
          
          
        )
      ) ),
    
    div( id = "summary_plot", 
      shinycssloaders::withSpinner(plotOutput("visual_summary"))
    )
  )
  
  

  
}