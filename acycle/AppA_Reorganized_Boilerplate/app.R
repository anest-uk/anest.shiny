library(shiny)
#-------------------------------packages
library(broom)
library(bslib)
library(car) # linear hypothesis test
library(colorspace)
library(data.table)
library(devtools)
library(DT)
library(ggplot2)
library(ggrepel)
library(grid)
library(gt)
library(gtExtras)
library(leaflet)
library(lubridate)
library(magrittr)
library(PerformanceAnalytics)
library(scales)
library(shinyvalidate)
library(shinyWidgets)
library(sp)
library(zoo)
library(plotly)
library(gridlayout)
#------------------------------source
stepripG <<- "data/smallrip/"
source("R/c-cleanlib.R")
source("R/rctree.R")
#load("acycle/A - copy/data/.RData")
#--------------------------------ui
gridheight <<- "630px"
gridheight2 <<- "830px"
gridheight3 <<- "1020px"
colx <<- cobalt()[c(4, 2, 1)]
sf <<- 3
pgmc <<- "grey50"
#---------------------------------end GH 


library(shiny)
source("R/global/load_data.R")
source("R/shared/ui_header.R")
source("R/shared/ui_sidebar.R")
source("R/shared/ui_action.R")
source("R/shared/server_common.R")

# Page UI
source("R/page_timeseries/ui_timeseries.R")
source("R/page_listing/ui_listing.R")
source("R/page_constituents/ui_constituents.R")
source("R/page_accuracy/ui_accuracy.R")
source("R/page_notes/ui_notes.R")
source("R/shared/ui_area2.R")

# Page Servers
source("R/page_timeseries/server_timeseries.R")
source("R/page_listing/server_listing.R")
source("R/page_constituents/server_constituents.R")
source("R/page_accuracy/server_accuracy.R")
source("R/page_notes/server_notes.R")

source("ui_main.R")

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