.onAttach <- .onAttach <- function(lib, pkg, ...) {
  startup_msg <-
    paste0(
      "Welcome to shinymde. This is shinymde version ",
      packageVersion("shinymde"),
      " using mde version ",
      packageVersion("mde"),
      ".\n",
"Please report issues at https://github.com/Nelson-Gon/shinymde/issues",
"\nTurn this message off with suppressPackageStartupMessages(library(shinymde))",
"\nHappy Exploration :)"
)
packageStartupMessage(startup_msg)
}
