#' Server ("backend") for shinymde
#' @import shiny
#' @import ggplot2
#' @importFrom dplyr %>%
#' @importFrom mde na_summary
#' @importFrom grDevices dev.off
#' @importFrom grDevices png
#' @importFrom utils read.table
#' @param input input as defined in the ui
#' @param output output of a defined ui tag.
#' @param session Session under which the app is running.
#' @return Server for the user interface.
#' @export

shinymde_server <- function(input, output, session) {
  
  on_off_toggle <- function(elements, kind = "hide") {
    switch(
      kind,
      hide = lapply(elements, shinyjs::hide),
      toggle = lapply(elements, shinyjs::toggle),
      show = lapply(elements, shinyjs::show)
    )
  }
 

output$input_file <- renderUI({
    fileInput("input_file",
              label = "Input File",
              placeholder =  "Please provide a file path")
  })
  # Get only data.frame objects since that's all mde supports. 
  output$dataset <- renderUI({
    selectInput(
      "dataset",
      "Dataset",
      choices = Filter(function(x) is.data.frame(get(x)),ls("package:datasets")),
      selected = "airquality"
    )
  })
  
  output$remote <- renderUI({
    textInput("remote", "Remote Dataset Link",
              value = "https://github.com/Nelson-Gon/shinymde/blob/c6cd1b8b3acc28225a907e00f80ac4031b755966/testdata/airquality.csv?raw=TRUE")
  })
  
  output$file_type <- renderUI({
    selectInput("file_type", "File Extension", 
                choices = c("csv", "tsv", "xlsx"),
                selected = "csv", multiple = FALSE)
  })
  shinyBS::addTooltip(session=session,
                      id="file_type", 
                     title="Select the remote dataset's file type.")
 
  

  on_off_toggle("sheet", kind="hide")
  guess_input <- reactive({
    if (req(input$data_source == "user_data")){
      file_extension <-gsub("(.*)(\\..*$)(.*)", "\\2",
                  input$input_file$datapath, perl = TRUE)
      
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
      
     
      # It is unlikely that this would happen since a user has to choose a 
      # file that exists anyway. 
      # observeEvent(input$input_file$datapath, {
      #   if (any(is.null(input$input_file$data_path),
      #           !file.exists(req(input$input_file$datapath)))) {
      #     
      # shinyFeedback::showFeedbackDanger("input_file",
      #                     text = "Please provide a valid data path.")
      #     
      #   }
      # 
      #   })
      # 
      
      
      
      
      
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
          
          on_off_toggle("sheet", kind="show")
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
  
  # Hide Tabs on start 
  # hidden_on_start <- c("Summarise Missingness", "Drop Values",
  #                      "Recode Values", "Visualise Missingness")
  
  show_hide_tab <- function(id, tabs, kind="hide"){
    switch(kind,
           hide =  lapply(tabs, function(tab)
             shiny::hideTab(inputId=id,
                            target=tab)),
           show = lapply(tabs, function(tab)
             shiny::showTab(inputId=id,
                            target=tab)))
    
  }
  
  # show_hide_tab(tabs=hidden_on_start, id="shinymde", kind="hide")
  # if user confirms input, show the above tabs 
  # observeEvent(input$confirm,
  #              {
  #                
  #                show_hide_tab(tabs=hidden_on_start, 
  #                              id="shinymde", kind="show")
  #              })
 
  

  on_off_toggle("data_summary", kind = "hide")
  
  output$data_summary <- renderPrint({
    summary(in_data())
  })
  observeEvent(input$confirm,
               {
                 on_off_toggle("data_summary", kind="show")
                 on_off_toggle("sys_details", kind="hide")
               })
  observeEvent(input$reset_input,{
    # TODO: Only reset data at current location not the entire UI 
    # Why not the entire UI? Seems like a waste of resources. 
    lapply(c("data_source", "input_file",
             "file_type", "remote", "dataset"), shinyjs::reset)
    
    on_off_toggle("sys_details", kind="show")
    on_off_toggle("data_summary", kind="hide")
    
  }
  )

  output$sort_by <- renderUI({
    selectInput(
      "sort_by",
      "Column to sort_by",
      choices = names(na_summary(in_data())),
      selected = "percent_missing"
    )
  })
  
  
  
  output$group_by <- renderUI({
    selectInput(
      "group_by",
      "Grouping Columns",
      choices = names(in_data()),
      multiple = TRUE
    )
  })
  
  output$exclude_columns <- renderUI({
    selectInput(
      "exclude_columns",
      "Columns to exclude",
      choices = names(in_data()),
      multiple = TRUE
    )
  })
  
  
  
  
  sort_order <- reactive({
    ifelse(input$sort_order == "descending",
           TRUE, FALSE)
  })
  summary_na <- reactive(
    na_summary(
      in_data(),
      sort_by = input$sort_by,
      grouping_cols = input$group_by,
      exclude_cols = input$exclude_columns,
      descending = sort_order(),
      round_to = input$round_to
    )
  )
  
  output$summary_na <- renderDataTable(summary_na(),
                                       options = list(pageLength = 10))
  
  delimiters <- reactive({
    switch(
      guess_input(),
      ".csv" = ",",
      ".tsv" = "\t",
      ".xlsx" = ";"
    )
    
    
    
  })
  
  tabsetPanel()
  
  
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
  
  output$subset_cols <- renderUI(selectInput(
    "subset_cols",
    "A subset to recode",
    choices = names(in_data()),
    multiple = TRUE
  ))
  
  output$criteria <- renderUI(selectInput(
    "criteria",
    "Criteria",
    choices = c("gt", "lt",
                "lteq", "gteq", "eq"),
    selected = "gt"
  ))
  
  output$keep_columns <- renderUI(selectInput(
    "keep_columns",
    "Keep Columns",
    choices = names(in_data()),
    multiple = TRUE
  ))
  
  
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
                             options = list(pageLength = 5))
 
  
  
  
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
  
  # Dropping NAs
  output$group_by_drop <- renderUI({
    selectInput(
      "group_by_drop",
      "Grouping Columns",
      choices = names(in_data()),
      multiple = TRUE
    )
  })
  
  output$keep_columns_drop <- renderUI(selectInput(
    "keep_columns_drop",
    "Keep Columns",
    choices = names(in_data()),
    multiple = TRUE
  ))
  output$target_cols <- renderUI(selectInput(
    "target_cols",
    "Target Columns",
    choices = names(in_data()),
    multiple = TRUE
  ))
  output$sign <- renderUI(selectInput(
    "sign",
    "Sign",
    choices = c("gt", "gteq", "lt", "lteq", "eq"),
    selected = "gt",
    multiple = FALSE
  ))
  
  drop_switch <- reactive({
    on_off_toggle(
      elements = c(
        "pattern_type_drop",
        "pattern_drop",
        "keep_columns_drop",
        "target_cols",
        "parcent_na_drop",
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
          pattern_type = input$pattern_type_drop,
          pattern = input$pattern_drop
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
                                    
                                    options = list(pageLength = 5))
  
  # Visual summaries
  output$y_variable <- renderUI({
    selectInput(
      "y_variable",
      "Y axis variable",
      choices = names(summary_na()),
      selected = "percent_missing"
    )
  })
  
  output$x_variable <- renderUI({
    selectInput(
      "x_variable",
      "X axis variable",
      choices = names(summary_na()),
      selected = "variable"
    )
  })
  
  output$fill_variable <- renderUI({
    {
      selectInput(
        "fill_variable",
        "Fill variable",
        choices = names(summary_na()),
        selected = "variable"
      )
    }
  })
  
  base_plot <- reactive(summary_na() %>%
                          ggplot(aes(
                            forcats::fct_reorder(.data[[req(input$x_variable)]],
                                                 .data[[req(input$y_variable)]]),
                            .data[[req(input$y_variable)]],
                            fill = .data[[req(input$fill_variable)]]
                          )) +
                          theme_minimal() +
                          guides(fill = "none") +
                          labs(x = input$x_variable)
                        )
  
  visual_plot <- reactive({
     base_plot() + 
      geom_col() +
      geom_label(aes(label = round(.data[[input$y_variable]],
                                   input$round_to_visual))) 
    
  })
  
  
  
  visual_plot_lollipop <- reactive({
   base_plot() + 
      geom_point(aes( 
                 col = .data[[req(input$fill_variable)]]),
                 size = input$size) +
      geom_segment(aes(x=.data[[req(input$x_variable)]],
                       
                       
                       xend=.data[[req(input$x_variable)]], y=0, 
                       yend=.data[[req(input$y_variable)]],
                       
                       col = .data[[req(input$fill_variable)]]),
                   
                   size = input$size) 
  })
  output$visual_summary <- renderPlot(
    switch(input$plot_type,
           "bar" = visual_plot(),
           "lollipop" = visual_plot_lollipop()))
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("shinymde_plot_",
             ".", input$extension)
      
    },
    content = function(file) {
      dims = as.numeric(strsplit(input$dims, "x")[[1]])
      png(file,
          width = dims[1], height = dims[2])
      
      switch(input$plot_type,
             "bar" = print(visual_plot()),
             "lollipop" = print(visual_plot_lollipop())) 
      dev.off()
    }
    
    
    
    
  )
  # This resets plot save preferences to the default.
  observeEvent(input$reset_opts, {
    shinyjs::reset("dims")
    shinyjs::reset("extension")
  })
  
  # Hide text labels if plot_type is set to lollipop 
  observeEvent(input$plot_type,{
    if(input$plot_type=="lollipop"){
      on_off_toggle("round_to_visual", kind="hide")
      # This does not work as expected. IT should vary whenever one 
      # changes plot type.
      # updateTextInput(inputId = "fill_variable", label="Colour Variable")
    }
  })
  
  observeEvent(input$extension, {
    if (input$extension != "png") {
      shinyFeedback::showFeedbackDanger("extension",
                                        text = "Only PNG is currently supported.")
    }
  })
  
  
}
