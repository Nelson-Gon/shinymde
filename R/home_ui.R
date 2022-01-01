home_ui <- function(){
  tabItem(
  tabName = "home",
  div(id = "welcome",
      strong(
        tags$p("Welcome to shinymde!",
               style = "font-size:20px;")
      )),
  tags$br(),
  fluidRow(
    
    Map(function(title, val, ico) info_box(title = title,
                                           value = val,
                                           use_icon = ico),
        c("Documentation", "Contribute"),
        c("Read Project Documentation", "Nelson-Gon/shinymde"),
        c("book", "laptop"))
  )
  ,
  fluidRow(
    tags$br(),
    tags$br(),
    tags$br(),
    
    Map(function(title, val, ico, url) info_box(title = title,
                                                value = val,
                                                use_icon = ico,
                                                href = url),
        c("Author", "Related Projects"),
        c("Nelson Gonzabato", "View Related Projects"),
        c("robot", "tools"),
        c("https://nelson-gon.github.io/about",
          "https://nelson-gon.github.io/projects"))
    
    
    
  )
  
)
}

