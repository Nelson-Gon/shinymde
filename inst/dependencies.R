# Remotes ----
install.packages("remotes")
remotes::install_github('Nelson-Gon/mde')
# Attachments ----
to_install <- c("config", "dplyr", "forcats", "ggplot2", "golem", "magrittr", "readxl", "shiny", "shinycssloaders", "shinydashboard", "shinyFeedback", "shinyjs", "shinyWidgets", "vroom")
  for (i in to_install) {
    message(paste("looking for ", i))
    if (!requireNamespace(i, quietly = TRUE)) {
      message(paste("     installing", i))
      install.packages(i)
    }
  }

