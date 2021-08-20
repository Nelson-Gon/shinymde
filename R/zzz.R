.onAttach <- .onAttach <- function(lib, pkg, ...){
  
  startup_msg <- paste0("Welcome to shinymde. This is shinymde version ",
                        packageVersion("shinymde"),
                        " using mde version ", packageVersion("mde"),
                        ".\n",
            "Please file issues and feedback at https://www.github.com/Nelson-Gon/shinymde/issues\n",
            "Turn this message off using 'suppressPackageStartupMessages(library(mde))'\n",
            "Happy Exploration :)")
  packageStartupMessage(startup_msg)
}
