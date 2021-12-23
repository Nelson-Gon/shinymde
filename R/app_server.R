#' The application server-side
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import ggplot2
#' @importFrom dplyr %>%
#' @importFrom mde na_summary
#' @importFrom grDevices dev.off
#' @importFrom grDevices png
#' @importFrom utils read.table
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  on_off_toggle <- function(elements, kind = "hide") {
    switch(
      kind,
      hide = lapply(elements, shinyjs::hide),
      toggle = lapply(elements, shinyjs::toggle),
      show = lapply(elements, shinyjs::show)
    )
  }
  
  
 
  # Get only data.frame objects since that's all mde supports.
  observe({
    updateSelectInput(session,
                      "dataset",
                      "Dataset",
                      choices = Filter(function(x)
                        is.data.frame(get(x)), ls("package:datasets")),
                      selected = "airquality")
  })
  
  observe({
    updateTextInput(session,
      "remote",
      "Remote Source",
      value = "https://github.com/Nelson-Gon/shinymde/blob/c6cd1b8b3acc28225a907e00f80ac4031b755966/testdata/airquality.csv?raw=TRUE"
    )
  })
  

  observe(
    {
      updateSelectInput(session,
                        "file_type",
                        "File Extension",
                        choices = c("csv", "tsv", "xlsx"),
                        selected = "csv")
    }
  )
  


  
  
  
  on_off_toggle("sheet", kind = "hide")
  guess_input <- reactive({
    if (req(input$data_source) == "user_data") {
      file_extension <- gsub("(.*)(\\..*$)(.*)",
                             "\\2",
                             req(input$input_file$datapath),
                             perl = TRUE)
      
      return(file_extension)
    }
    
    if (input$data_source == "remote") {
      return(input$file_type)
    }
    else{
      warning("Cannot guess input type, defaulting to csv")
      return(".csv")
    }
    
  })
  
  
  
  
  
  
  
  
  in_data <- reactive({
    if (input$data_source == "inbuilt") {
      return(get(req(input$dataset), "package:datasets"))
      
    }
    
    
    if (input$data_source == "remote") {
      stopifnot(
        "Only csv, tsv, xlsx currently supported not" =
          input$file_type %in% c("csv", "xlsx", "tsv")
        
      )
      sep_switch = switch(
        req(input$file_type),
        "csv" = read.table(url(input$remote), sep = ",",
                           header = TRUE),
        "tsv" = read.table(url(input$remote), sep = "\t",
                           header = TRUE),
        "xlsx" = readxl::read_xlsx(input$remote)
      )
      return(sep_switch)
    }
    
    if (input$data_source == "user_data") {
   
      if (!guess_input() %in% c(".csv", ".xlsx", ".tsv")) {
        stop(
          paste0(
            "Only .csv, .xlsx, and .tsv are currently supported, not ",
            guess_input(),
            "."
          )
        )
      }
      switch(
        guess_input(),
        ".csv" = vroom::vroom(
          req(input$input_file$datapath),
          delim = ",",
          show_col_types = FALSE
        ),
        ".xlsx" = {
          on_off_toggle("sheet", kind = "show")
          readxl::read_xlsx(req(input$input_file$datapath),
                            sheet = req(input$sheet))
          
        },
        ".tsv" = vroom::vroom(
          req(input$input_file$datapath),
          delim = "\t",
          show_col_types = FALSE
        )
      )
      
      
      
    }
  })
  
  
  
  on_off_toggle("data_summary", kind = "hide")
  
  output$data_summary <- renderPrint({
    summary(in_data())
  })
  
  # Hide sys_details on click of button
  observeEvent(input$confirm_in,
               {
                 on_off_toggle("sys_details", kind = "hide")
                 on_off_toggle("data_summary", kind = "show")
               })

  observeEvent(input$reset_input, {
    # TODO: Only reset data at current location not the entire UI
    # Why not the entire UI? Seems like a waste of resources.
    lapply(
      c(
        "data_source",
        "input_file",
        "file_type",
        "remote",
        "dataset"
      ),
      shinyjs::reset
    )
    
    on_off_toggle("sys_details", kind = "show")
    on_off_toggle("data_summary", kind = "hide")
   
    
  })
  
  output$sort_by <- renderUI({
    selectInput(
      "sort_by",
      "Sort by",
      choices = names(na_summary(in_data())),
      selected = "percent_missing"
    )
  })
  
  
  
  observe({
    updateSelectInput(session, 
      "group_by",
      "Group BY",
      choices = names(in_data())
    )
  })
  # Hide on app start
  # on_off_toggle(c("pattern_type_summary", "pattern_summary",
  #                 "select_kind"),
  #               kind = "hide")
  # observeEvent(input$regex_based,
  #              {
  #                if (input$regex_based == "yes") {
  #                  on_off_toggle("select_kind", kind = "show")
  #                  on_off_toggle("pattern_type_summary", kind = "show")
  #                  on_off_toggle("pattern_summary", kind = "show")
  #                  
  #                }
  #              })
  
  
  observe({
    updateSelectInput(session, 
      "exclude_columns",
      "Exclude Cols",
      choices = names(in_data())
    )
  
  })
  
  
  
  
 
  summary_na <- reactive(
    if(is.null(input$select_kind)){
      return(   na_summary(
        in_data(),
        sort_by = input$sort_by,
        grouping_cols = input$group_by,
        exclude_cols = input$exclude_columns,
        descending = req(input$sort_order)=="descending",
        round_to = NULL,
        regex_kind = NULL,
        pattern_type = NULL,
        pattern = NULL
        
      ))
    }
    else{
      return(
        na_summary(
          in_data(),
          sort_by = input$sort_by,
          grouping_cols = input$group_by,
          exclude_cols = input$exclude_columns,
          descending = req(input$sort_order)=="descending",
          round_to = input$round_to,
          regex_kind = req(input$select_kind),
          pattern_type = req(input$pattern_type_summary),
          pattern = req(input$pattern_summary)
          
        )
        
      )
    }
 
  )
  
  
  
  
  output$summary_na <- renderDataTable(summary_na(),
                                       options = list(pageLength = 25))
  
  delimiters <- reactive({
    switch(
      guess_input(),
      ".csv" = ",",
      ".tsv" = "\t",
      ".xlsx" = ";"
    )
    
    
    
  })
  
  values_to_recode <- reactive({
    # split and convert to numeric if applicable
    values = unlist(strsplit(input$value_to_recode, ","))
    if (any(grepl("[0-9]", values))) {
      # TODO Show user warnings
      # shinyFeedback::showFeedback("value_to_recode",
      # text="Input values converted to numeric")
      values <- as.numeric(values)
    }
    values
  })
  
  observe({
    updateSelectInput(
      session,
      "subset_cols",
      "A subset to recode",
      choices = names(in_data())
      
    )
  })
  observe({
    updateSelectInput(
      session,
      "keep_columns",
      "Keep Cols", 
      choices = names(in_data())
    )
  })
  

  
 
  
  
  recode_switch <- reactive({
    # recode_as_na_for() --> df, criteria, value, subset_cols
    # recode_as_na_if() --> df, sign , percent_na, keep_columns
    # recode_na_if() --> df, grouping_cols, target_groups, replacement
    # recode_as_na() --> df, value, subset_cols, pattern_type, pattern
    # recode_na_as() --> df, value, subset_cols, pattern_type, pattern
    # dict_recode() --> df, use_func, pattern_type, patterns, values
    
    # NOTES
    # This could be done with do.call or some switch but for whatever reason
    # Such calls fail with bugs that I could not identify readily.
    # See https://github.com/Nelson-Gon/shinymde/issues/1 and
    # https://github.com/Nelson-Gon/shinymde/issues/2
    
    on_off_toggle(elements = c("criteria", "pattern_type", "pattern"),
                  kind = "hide")
    
    
    if (input$recode_type %in% c("recode_as_na", "recode_na_as")) {
      on_off_toggle(elements = c("pattern_type", "pattern"),
                    kind = "toggle")
      
      
    }
    if (input$recode_type == "recode_as_na") {
      # NOTE This requires explicit returns unlike in "normal"
      # R programming mode.
      
      return(
        mde::recode_as_na(
          df = in_data(),
          value = values_to_recode(),
          subset_cols = input$subset_cols,
          pattern_type = input$pattern_type,
          pattern = input$pattern
        )
      )
    }
    
    if (input$recode_type == "recode_na_as") {
      return(
        mde::recode_na_as(
          df = in_data(),
          value = values_to_recode(),
          subset_cols = input$subset_cols,
          pattern_type = input$pattern_type,
          pattern = input$pattern
        )
      )
    }
    
    if (input$recode_type == "recode_as_na_if") {
      on_off_toggle(elements = "subset_cols", kind = "hide")
      on_off_toggle(elements = "criteria", kind = "show")
      return(
        mde::recode_as_na_if(
          df = in_data(),
          percent_na = values_to_recode(),
          sign = input$criteria,
          keep_columns = input$keep_columns
        )
      )
    }
    if (input$recode_type == "recode_as_na_for") {
      on_off_toggle(elements = c("criteria", "subset_cols"),
                    kind = "show")
      return(
        mde::recode_as_na_for(
          df = in_data(),
          criteria = input$criteria,
          value = values_to_recode(),
          subset_cols = input$subset_cols
        )
      )
    }
    
  })
  
  
  output$recode_values <- renderDataTable(recode_switch()
                                          
                                          
                                          ,
                                          options = list(pageLength = 25))
  
  
  
  
  downloader <- reactive(switch(
    input$shiny_mde,
    "missingness_summary" = summary_na(),
    
    "recode_values" = recode_switch(),
    "drop_values" = drop_switch()
  ))
  output$downloadfile <- downloadHandler(
    filename = function() {
      paste0(
        substitute(in_data()),
        "_missingness_report_mde_",
        format(Sys.time(), "%b-%d-%Y"),
        guess_input()
      )
    },
    content = function(x) {
      delim = switch(
        guess_input(),
        ".csv" = ",",
        ".xlsx" = ";",
        ".tsv" = "\t"
      )
      vroom::vroom_write(downloader(),
                         x, delim = delim)
    }
  )
  output$downloadfile_drop <- downloadHandler(
    filename = function() {
      paste0(
        substitute(in_data()),
        "_missingness_report_mde_",
        format(Sys.time(), "%b-%d-%Y"),
        guess_input()
      )
    },
    content = function(x) {
      delim = switch(
        guess_input(),
        ".csv" = ",",
        ".xlsx" = ";",
        ".tsv" = "\t"
      )
      vroom::vroom_write(downloader(),
                         x, delim = delim)
    }
  )
  output$downloadfile_recode <- downloadHandler(
    filename = function() {
      paste0(
        substitute(in_data()),
        "_missingness_report_mde_",
        format(Sys.time(), "%b-%d-%Y"),
        guess_input()
      )
    },
    content = function(x) {
      delim = switch(
        guess_input(),
        ".csv" = ",",
        ".xlsx" = ";",
        ".tsv" = "\t"
      )
      vroom::vroom_write(downloader(),
                         x, delim = delim)
    }
  )
  
  observe({
    updateSelectInput(session,
      "group_by_drop",
      "Group BY",
      choices = names(in_data())
    )
  })
  observe({
    updateSelectInput(session,
        "keep_columns_drop",
        "Keep Cols",
        choices = names(in_data())
      )
  }
    )
  

  observe({
    updateSelectInput(session, 
      "target_cols",
      "Target Cols",
      choices = names(in_data())
    )
  })
 
  
  drop_switch <- reactive({
    on_off_toggle(
      elements = c(
        "pattern_type_drop",
        "pattern_drop",
        "keep_columns_drop",
        "target_cols",
        "percent_na_drop",
        "sign"
      ),
      kind = "hide"
    )
    on_off_toggle(elements = c("group_by_drop"), kind = "show")
    # mde::drop_all_na() --> df, grouping_cols
    # mde::drop_na_at() --> df, pattern_type, pattern, case_sensitivity, ...
    # mde::drop_na_if() --> df, sign, percent_na, keep_columns, grouping_cols,
    # target_columns
    if (input$drop_type == "drop_all_na") {
      return(mde::drop_all_na(
        df = in_data(),
        grouping_cols = input$group_by_drop
      ))
    }
    
    if (input$drop_type == "drop_na_at") {
      on_off_toggle(c("pattern_type_drop", "pattern_drop",
                      "group_by_drop"),
                    kind = "toggle")
      return(
        mde::drop_na_at(
          df = in_data(),
          pattern_type = req(input$pattern_type_drop),
          pattern = req(input$pattern_drop)
        )
      )
    }
    
    if (input$drop_type == "drop_na_if") {
      on_off_toggle(
        elements = c(
          "percent_na_drop",
          "sign",
          "keep_columns_drop",
          "target_cols"
        ),
        kind = "toggle"
      )
      
      mde::drop_na_if(
        df = in_data(),
        sign = input$sign,
        percent_na = input$percent_na_drop,
        keep_columns = input$keep_columns_drop,
        grouping_cols = input$group_by_drop,
        target_columns = input$target_cols
      )
    }
  })
  
  output$drop_na <- renderDataTable(drop_switch(),
                                    
                                    options = list(pageLength = 25))
  
  # Visual summaries
  observe({
    updateSelectInput(session,
      "y_variable",
      "Y",
      choices = names(summary_na()),
      selected = "percent_missing"
    )
  })
  
  observe({
    updateSelectInput(session,
                        "x_variable",
                        "X",
                        choices = names(summary_na()),
                        selected = "variable"
                      )
                      
  })
  
observe({
  updateSelectInput(session,
                      "fill_variable",
                      "Fill",
                      choices = names(summary_na()),
                      selected = "variable"
                    )
                    
})

get_all_themes <- eventReactive(input$confirm_pkg,
                                {
  if(!req(input$pkg) %in% loadedNamespaces()){
    
    suppressPackageStartupMessages(
      library(req(input$pkg), character.only = TRUE)
    )
  }
  all_pkg_funs <- getNamespaceExports(req(input$pkg))
  all_themes<-all_pkg_funs[grep("^theme_",all_pkg_funs)]
  return(all_themes)

})
get_theme <- reactive(
  # akrun credit:https://stackoverflow.com/q/70414757/10323798
  getFunction(get_all_themes()[grep(req(input$theme), get_all_themes())])()
)
# Update available themes based on the above
observe(
updateSelectizeInput(session=session,
                     "theme",
                     "Plot theme",
                     choices = get_all_themes(),
                     selected = "theme_grey")
)
  
  base_plot <- reactive(
    summary_na() %>%
      ggplot(aes(
        forcats::fct_reorder(.data[[req(input$x_variable)]],
                             .data[[req(input$y_variable)]]),
        .data[[req(input$y_variable)]],
        fill = .data[[req(input$fill_variable)]]
      ))  + 
      guides(fill = "none") +
      labs(x = req(input$x_variable)) +
      theme_minimal()
      # get_theme()
      
  )

  
  
  visual_plot <- reactive({
    base_plot() +
      geom_col() -> res
    if (input$plot_type == "bar" & input$show_text) {
      res <- res + geom_label(aes(
                      label = round(.data[[input$y_variable]],
                                    input$round_to_visual)
                    ))
    }
    
    return(res)
    
    
    
  })
  
  
  
  visual_plot_lollipop <- reactive({
    base_plot() +
      geom_point(aes(col = .data[[req(input$fill_variable)]]),
                 size = input$size) +
      geom_segment(aes(
        x = .data[[req(input$x_variable)]],
        
        
        xend = .data[[req(input$x_variable)]],
        y = 0,
        yend = .data[[req(input$y_variable)]],
        
        col = .data[[req(input$fill_variable)]]
      ),
      
      size = input$size)
  })
  output$visual_summary <- renderPlot(switch(
    input$plot_type,
    "bar" = visual_plot(),
    "lollipop" = visual_plot_lollipop()
  ))
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("shinymde_plot_",
             ".", input$extension)
      
    },
    content = function(file) {
      dims = as.numeric(strsplit(input$dims, "x")[[1]])
      png(file,
          width = dims[1], height = dims[2])
      
      switch(
        input$plot_type,
        "bar" = print(visual_plot()),
        "lollipop" = print(visual_plot_lollipop())
      )
      dev.off()
    }
    
  )
  
  
  observeEvent(input$plot_reset_button,
               {
                 shinyjs::reset("plot_area")
               })
  
  # Hide text labels if plot_type is set to lollipop
  observeEvent(input$plot_type, {
    if (input$plot_type == "lollipop") {
      on_off_toggle("round_to_visual", kind = "hide")
      # TODO: Dynamic updates to ensure reset buttons reset these too. 
      updateSelectInput(inputId = "fill_variable", label = "Colour Variable")
    }
  })
  
  observeEvent(input$extension, {
    if (input$extension != "png") {
      shinyFeedback::showFeedbackDanger("extension",
                                    text = "Only PNG is currently supported.")
    }
  })
  
  
}
