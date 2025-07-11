library(shiny)

#-----------------------------packages--1----
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
library(htmltools)
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
#-------------------------------source--2----
stepripG <<- "data/smallrip/"
source("R/c-cleanlib.R")
source("R/rctree.R")
# source("R/CDFlib.R")
source("R/d-lib.R") #
source("R/dx-lib.R") # gen2
source("R/c-lib.R")
source("R/v-lib.R") # vgeo(); vkss(); vestdt(); vpva()
#---------------------app code
source("R/ui4000.R") # 0
#gen1
source("R/ui_accuracy.R") # 4-acc
source("R/ui_action.R") # 0-act
source("R/ui_constituents.R") # 3-con
source("R/ui_header.R") # 0-hea
source("R/ui_listing.R") # 2-lis
source("R/ui_notes.R") # 0-not
source("R/ui_timeseries.R") # 1-tim
#gen2
source("R/ui3000.R") # grid 3 : sidebar
source("R/ui4100.R") # grid 4 : tabset
source("R/ui4200.R") # 6-lis
source("R/ui4300.R") # 7-con

if (T) { # revisit this
  #--frig static data until moved upstream----
  resS$geo <- resS$geo[, .(nx, rc6)] %>% sco(., F)
  resS$pva <- resS$pva[, .(m2, nid, pv, rc6)] %>% sco(., F)
  resS$lab <- resS$lab %>% sco(., F)
  resS$rsi <- resS$rsi %>% sco(., F)
  resS$kss <- resS$kss %>% sco(., F)
  resS$f250618b <- resS$f250618b %>% sco(., F)
  resS$f250618c <- resS$f250618c %>% sco(., F)
  #---------valid global data--2.1----
  vres(resS)

  #--------------------------=parameters--3----
  gridheight <<- "630px"
  gridheight2 <<- "830px"
  gridheight3 <<- "1020px"
  coltab <<- # uses data.table so cannot go in global.R
    rbind(
      data.table(light = cobalt(light = T)[c(4, 3, 2, 2, 1, 5)], dark = cobalt(light = F)[c(4, 3, 2, 2, 1, 5)], code = c("1.3", "1.2", "1.1", "2.3", "2.2", "3.3")),
      data.table(light = "grey50", dark = "grey10", code = "0.0")
    )
  colx <<- cobalt()[c(4, 2, 1)]
  sf <<- 3
  pgmc <<- "grey50"
  #-------------------Pseudo=Control for dev purposes
  hoflC <<- c("house", "flat", "all")[3] # ,
  itriC <<- c(".0" = 1, ".1" = 2, ".5" = 3)[2] # , #Trim ---
  neusC <<- c("new", "used", "all")[3] # ,
  rc3coC <<- c("B--", "E--", "AL-") # ,  #comp
  rc6cuC <<- c("W--8--") # , #custom
  tbinC <<- c(lo = 1, hi = 2, an = 3)[2] # ,  #lo hi an ---
  typeC <<- c("A", "L", "N", "C")[2] # , #All Local National ---
  typerC <<- typeC
  zerorefC <- F # , #set reference asset NULL
  #---------------#dubious globals needing tidyup
  nfig2 <<- -1 # for ppm2
  nfig3 <<- 4 # for frac
  verbose <<- T
  showtradetriangle <- F
}

#---ui---------------------------------------5----

ui <- grid_page(
  layout = c(
    "action  header",
    "sidebar  area2 "
  ),
  row_sizes = c(
    "200px",
    "1fr"
  ),
  col_sizes = c(
    "250px",
    "1fr"
  ),
  gap_size = ".1rem",
  ui_card_action(),
  ui3000(),
  ui_card_header(),
  ui_4000()
)

#---server-----------------------------------6----
server <- function(
    input,
    output,
    session) {
  #--------------------------------server
  source("R/server_common.R") # leave with full name
  source("R/s3000.R") # 0-sidebar
  
  #gen1 pages
  source("R/server_listing.R") # 2-lis
  source("R/server_timeseries.R") # 1-tim
  source("R/server_constituents.R") # 3-con
  source("R/server_accuracy.R") # 4-acc
  #gen2 pages
  source("R/s4100.R") # 5-tim1
  source("R/s4200.R") # 6-lis1
  source("R/s4300.R") # 7-con1

  common <- server_common(input, output, session)


  server_timeseries(input, output, session, common)
  server_listing(input, output, session, common)
  server_constituents(input, output, session, common)
  server_accuracy(input, output, session, common)
  
  #gen2 servers named s[gpij] g=grid p=page i=row j=col
  s3000(input, output, session, common)
  s4100(input, output, session, common) #timeseries
  s4200(input, output, session, common) #listing
  s4300(input, output, session, common) #constituent

  # ===-output: controls select/compute/suggest----
  selectedrc6R <- reactive({ # --rc6 selected----
    x0 <- sort(unique(input$rctreeC))
    x1 <- # exclude non-rc6 higher tree nodes
      x0[
        which(
          (nchar(x0) == 6) &
            (substr(x0, 3, 3) == "-")
        )
      ]
    x <- paste0(paste0(x1, collapse = ","))
    print(paste0("selectedrc6R: ", x))
    selectedrc6G <<- copy(x)
    x
  })
  computedrc6R <- reactive({ # ---rc6 computed----
    rsicX <- common$rescR()
    x1 <- rsicX$kfoldsse[, rc6] %>% .[nchar(.) == 6]
    x <- paste0(paste0(sort(unique(x1)), collapse = ","))
    print(paste0("computedrc6R: ", x))
    computedrc6G <<- copy(x)
    x
  })
  output$selrc6 <- renderText({ #------selected----
    paste0("selected:   ", selectedrc6R())
  })
  output$comrc6 <- renderText({ #-----computed----
    paste0("computed: ", computedrc6R())
  })
  output$defrc6 <- renderText({ #----suggested----
    paste0(c("suggested: ", rc6deR()), collapse = "")
  })
  output$selrc6forjstest <- renderText({ #--js----
    selectedrc6R()
  })
  output$comrc6forjstest <- renderText({ #--js----
    computedrc6R()
  })
  output$cuseqcom <- renderText({ #------match----
    "Custom index matches selection"
  }) # span(, style="size:8")
  output$cusnecom <- renderText({ #------recalc----
    "Recalc for selected districts"
  }) # span(, style="size:8")


  observe( #-----observe target, update custom----
    x = {
      if (verbose) {
        print("enter updateTreeInput")
      }
      if ( # guard against invalid selection
        (!is.null(input$rc6tC)) & # empty tree
          (all(nchar(input$rc6tC) == 6)) # non-leaf selection
      ) {
        rc6c <-
          rc6deR()
        if (verbose) {
          print("treeinput update with custom peers:")
          print(rc6c)
        }
        updateTreeInput(
          inputId = "rctreeC",
          label = NULL,
          selected = rc6c,
          session = shiny::getDefaultReactiveDomain()
        )
        if (verbose) {
          print("updateTreeInput complete")
        }
      } else {
        print("no updateTreeInput")
      }
    }
  )

  rc6deR <- eventReactive( # default custom rc6 ----
    list(
      input$rc6tC, # target
      common$rc6tR()
    ),
    {
      if (verbose) {
        print("enter updateTreeInput")
      }
      if ( # guard against invalid selection
        (!is.null(input$rc6tC)) & # empty tree
          (all(nchar(input$rc6tC) == 6)) # non-leaf selection
      ) {
        x <-
          f241229bd[common$rc6tR() == target, f240920b(id)]
        if (verbose) {
          print("custom peers:")
          print(x)
        }
        rc6deG <<- copy(x)
        x
      }
    }
  )

} # end server

shinyApp(ui, server)
