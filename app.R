# setwd("shinymde")
source("ui.R")
source("server.R")
shinyApp(ui, server)


head(recode_as_na_if(airquality, sign="gt", percent_na=20))
