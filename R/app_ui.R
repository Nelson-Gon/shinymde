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
    #input_controls, #summarise_controls,
    #recode_controls, #drop_zone, #visual_controls {float: left; width:30%}
    #data_summarise, #sys_details, #summary_na_table,
    #recode_table, #drop_table, #summary_plot {float:right; width:70%;}
    #summary_plot.img {width:500px; height:600px;}

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
     missingness_ui()
      ,
      recode_ui()
      ,
      drop_ui()
      ,
      
     visual_ui()
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
