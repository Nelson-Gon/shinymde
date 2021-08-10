#' shinymde App Launcher 
#' @return A runnable shiny application 
#' @import shiny 
#' @export 
#' 
#' @examples 
#' if(interactive()) launch_app()

launch_app <- function(){
  shinyApp(ui=shinymde_ui, server=shinymde_server)
}

