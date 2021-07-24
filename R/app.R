#' shinymde App Launcher 
#' @return A runnable shiny application 
#' @import shiny 
#' @export 

run_app <- function(){
  shinyApp(ui=shinymde_ui, server=shinymde_server)
}
