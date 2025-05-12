library(shiny)

# Load global/static data (replace with your own)
# Example:
# f241021adG <- readRDS("data/f241021ad.rds")

# Source UI
source("ui/ui_main.R")
source("ui/ui_listing.R")
source("ui/ui_constituents.R")
source("ui/ui_newpage.R")

# Source server
source("R/server_common.R")
source("R/server_listing.R")
source("R/server_constituents.R")
source("R/server_newpage.R")

ui <- ui_main()

server <- function(input, output, session) {
  common <- server_common(input, output, session)
  server_listing(input, output, session, common)
  server_constituents(input, output, session, common)
  server_newpage(input, output, session, common)
}

shinyApp(ui, server)