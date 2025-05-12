library(shiny)

# Global data + shared infrastructure
source("R/global/global.R", local = TRUE)
source("R/shared/server_common.R", local = TRUE)
source("R/shared/ui_header.R", local = TRUE)
source("R/shared/ui_sidebar.R", local = TRUE)
source("R/shared/ui_action.R", local = TRUE)

# UI for each page
source("R/page_timeseries/ui_timeseries.R", local = TRUE)
source("R/page_listing/ui_listing.R", local = TRUE)
source("R/page_constituents/ui_constituents.R", local = TRUE)
source("R/page_accuracy/ui_accuracy.R", local = TRUE)
source("R/page_notes/ui_notes.R", local = TRUE)

# Area2 layout for navbar pages
source("R/shared/ui_area2.R", local = TRUE)

# Server logic for each page
source("R/page_timeseries/server_timeseries.R", local = TRUE)
source("R/page_listing/server_listing.R", local = TRUE)
source("R/page_constituents/server_constituents.R", local = TRUE)
source("R/page_accuracy/server_accuracy.R", local = TRUE)
source("R/page_notes/server_notes.R", local = TRUE)

# Top-level UI layout
source("ui_main.R", local = TRUE)

ui <- ui_main()

server <- function(input, output, session) {
  common <- server_common(input, output, session)
  server_timeseries(input, output, session, common)
  server_listing(input, output, session, common)
  server_constituents(input, output, session, common)
  server_accuracy(input, output, session, common)
  server_notes(input, output, session, common)
}

shinyApp(ui, server)