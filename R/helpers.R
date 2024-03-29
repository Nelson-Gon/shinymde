menu_render <- function(text, tab_name, use_icon,...){
  titles = c("Home", "Input Data", "Summarise Missingness",
             "Recode Values", "Drop Values", "Visualize Missingness")
  tab_names <- c("home", "input", "missingness_summary",
                 "recode_values", "drop_values", "visual_summary")
  icons = c("home", "database", "table", "exchange-alt", "eraser",
            "chart-bar")
  Map(function(title, use_lab, use_ico) 
    shinydashboard::menuItem(text=title,tabName=use_lab,
                             icon=shiny::icon(use_ico)),
    titles, tab_names, icons)
}


info_box <- function(title, value, use_icon, 
                     href = "https://nelson-gon.github.io/shinymde",
                     width = 6,
                     color = "blue",
                     ...){
  infoBox(
    title = title,
    value = value,
    href = href,
    icon = shiny::icon(use_icon),
    color = color,
    width = width,
    ...
  )  
}

sys_info_boxes <- function(width = 12, ...){
  
  titles = c("Date of analysis", "R Version", "mde Version")
  icons = c("calendar", "cog", "tools")
  values = c(format(Sys.Date(), "%A %b %d %Y"),
             R.version.string, as.character(packageVersion("mde")))
  
  Map(function(use_title, use_icon, use_value)
    shinydashboard::infoBox(
      title = use_title,
      icon = shiny::icon(use_icon),
      value = use_value,
      width = width
    ), titles, icons, values)
}



selectize_input <- function(id, label, choices, selected,...){
  shiny::selectizeInput(inputId = id,
                        label = label,
                        choices = choices, selected = selected,
                        ...)
}

text_input <- function(id, label, value, ...){
  shiny::textInput(inputId = id, label = label, value = value,...)
}

numeric_input <- function(id, label, value, ...){
  shiny::numericInput(inputId = id, label = label, value = value,...)
}

on_off_toggle <- function(elements, kind = "hide") {
  switch(
    kind,
    hide = lapply(elements, shinyjs::hide),
    toggle = lapply(elements, shinyjs::toggle),
    show = lapply(elements, shinyjs::show)
  )
}

multi_selectize <- function(ids, titles, choices,size,
                            selected=NULL,
                            selectize = TRUE,
                            multiple = FALSE){
  
  # Using selectInput instead of selectize
  # See why at https://stackoverflow.com/q/70551373/10323798
  Map(
    function(id, lab, choices,
             size) 
      selectInput(inputId = id,
        label = lab,
        choices = choices,
        selected = selected,
        selectize = selectize,
        multiple = multiple, 
        size = size
      )
    ,
    ids,
    titles,
    choices,
    size
  )

}

two_columns <- function(...){
  lapply(list(...), function(x) column(6, x))
}
