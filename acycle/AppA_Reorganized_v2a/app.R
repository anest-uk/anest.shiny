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
source("R/shared/c-cleanlib.R")
source("R/shared/rctree.R")
#load("acycle/A - copy/data/.RData")
#------------------------------data
#-------------------------------load data, dataprep is in v164/250503 rdata for app.R (probably...)
load("data/.RData") # contains "f241021ad"  "f241208fd"  "pxosrdo2dd" "x101"       "z110"

f241021adG <- f241021ad
#f241208fdG <- f241208fd not used in app A so remove 250506
pxosrdo2ddG <- pxosrdo2dd
x101G <- x101
z110G <- z110




#--------------------------------ui
gridheight <<- "630px"
gridheight2 <<- "830px"
gridheight3 <<- "1020px"
colx <<- cobalt()[c(4, 2, 1)]
sf <<- 3
pgmc <<- "grey50"
source("ui_main.R")
source("R/page_timeseries/ui_timeseries.R")
source("R/page_listing/ui_listing.R")
source("R/page_constituents/ui_constituents.R")
source("R/page_accuracy/ui_accuracy.R")
source("R/shared/ui_action.R")
source("R/shared/ui_header.R")
source("R/page_notes/ui_notes.R")
source("R/shared/ui_sidebar.R")

#--------------------------------Pseudo=Control----2----
hoflC <<- c("house", "flat", "all")[3] # ,
itriC <<- c(".0" = 1, ".1" = 2, ".5" = 3)[2] # , #Trim ---
neusC <<- c("new", "used", "all")[3] # ,
rc3coC <<- c("B--", "E--", "AL-") # ,  #comp
rc6cuC <<- c("W--8--") # , #custom
tbinC <<- c(lo = 1, hi = 2, an = 3)[2] # ,  #lo hi an ---
typeC <<- c("A", "L", "N", "C")[2] # , #All Local National ---
typerC <<- typeC
nfig2 <<- -1 # for ppm2
nfig3 <<- 4 # for frac
verbose <<- T

zerorefC <- F # , #set reference asset NULL
showtradetriangle <- F

#---ui
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
  ui_card_sidebar(),
  ui_card_header(),
  ui_area2()
)

#---server

server <-  function(
    input, 
    output,
    session
) {
  
  #--------------------------------server
  source("R/shared/server_common.R")
  source("R/page_listing/server_listing.R")
  
  common <- server_common(input, output, session)
  
  server_listing(input, output, session, common)
  
  #----------------------------------------------------1ij-Time-series summary
  ###loads transferred to other pages....
  #----------------------------------------------------Time-series summary end
  selectedrc6R <- reactive({
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
  computedrc6R <- reactive({
    rsicuX <- common$rsicuR()
    x1 <- rsicuX$kfoldsse[, rc6] %>% .[nchar(.) == 6]
    x <- paste0(paste0(sort(unique(x1)), collapse = ","))
    print(paste0("computedrc6R: ", x))
    computedrc6G <<- copy(x)
    x
  })
  output$selrc6 <- renderText({
    paste0("selected:   ", selectedrc6R())
  })
  output$comrc6 <- renderText({
    paste0("computed: ", computedrc6R())
  })
  output$defrc6 <- renderText({
    paste0(c("suggested: ", rc6deR()), collapse = "")
  })
  output$selrc6forjstest <- renderText({
    selectedrc6R()
  })
  output$comrc6forjstest <- renderText({
    computedrc6R()
  })
  
  output$cuseqcom <- renderText({
    "Custom index matches selection"
  }) # span(, style="size:8")
  output$cusnecom <- renderText({
    "Recalc for selected districts"
  }) # span(, style="size:8")
  rssR <- reactive({
    #x <- copy(common$x00R()$rss)
    x <- copy(f241021ad$rss)
    rssG <<- copy(x)
    x
  })
  pxosrdo2ddR <- reactive({
    x <- copy(pxosrdo2dd)
    pxosrdo2ddG <<- copy(x)
    x
  })
  x101R <- reactive({
    x <- copy(x101) # initial dates
    x101G <<- copy(x)
    x
  })
  dfnR <- reactive({ # not used?
    x <- data.table(date = c(as.Date("1994-12-31"), common$estdtR()[, sort(unique(date))]))[, let(i, (0:(.N - 1)))]
    dfnG <<- copy(x)
    x
  })

    tslideR <- reactive({
    x <- input$tslider
    tslideG <<- copy(x)
    x
  })
  
  #---custom   section----
  observe(
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
      } # length(rc6c)>0 end
    }
  )
  
  rc6deR <- #---default custom rc6 ----
  eventReactive(
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
  
  nxcuR <- #---custom nx compute    ----
  eventReactive(
    common$geocuR(),
    {
      if (verbose) print("enter nxcuR")
      x <-
        common$geocuR()[, .(nx, rc3, qtile, lab)] %>%
        unique(.)
      nxcuG <<- copy(x)
      x
    }
  )

  rsscuR <- #---custom rss select    ----
  eventReactive(
    list(
      common$rsicuR()
    ),
    {
      if (verbose) print("enter rsscuR")
      x <- cbind(common$rsicuR()$kfoldsse, common$rsicuR()$all)
      rsscuG <<- copy(x)
      x
    }
  )
 
  nxaR <- #---area nx select       ----
  eventReactive(
    common$geoaR(),
    {
      if (verbose) print("enter nxaR")
      x <-
        common$geoaR()[, .(nx, rc3, qtile, lab)] %>%
        unique(.)
      nxaG <<- copy(x)
      if (verbose) print("exit nxaR")
      x
    }
  )
  estdtaR <- #---area estdt compute   ----
  eventReactive(
    list(
      nxaR(),
      common$estdtR()
    ),
    {
      if (verbose) print("enter estdtaR")
      x <-
        common$estdtR()[nxaR(), on = c(nx = "nx")] %>%
        .[, .(nx, date, ii, lab, rc3, qtile, xdotd, days, xdot, x)]
      estdtaG <<- copy(x)
      x
    }
  )
  rssaR <- #---area rss compute     ----
  eventReactive(
    nxaR(),
    {
      if (verbose) print("enter rssaR")
      x <-
        rssR()[nxaR(), on = c(nx = "nx")]
      rssaG <<- copy(x)
      x
    }
  )
  #---combo    section                  ----
  festdtxX <- #----112 x(t)              ----
  function(
    estdtcuX = estdtcuG, estdtaX = estdtaG, geocuX = geocuG) {
    x <-
      rbind(
        # estdtcuX[,.(nx,date,xdotd,days,xdot,x,lab,ii,qtile=0,rc3=geocuX[,substr(rc9,1,3)])],
        estdtcuX[, .(nx, date, xdotd, days, xdot, x, lab, ii, qtile = 0, rc3 = lab)],
        estdtaX[, .(nx, date, xdotd, days, xdot, x, lab, ii, qtile, rc3)]
      )[, qq := as.factor(qtile)]
    x
  }
  
  estdtxR <- #----112 x(t)              ----
  eventReactive(
    list(common$estdtcuR(), estdtaR(), geocuX = common$geocuR()),
    {
      print('enter estdtxR')
      x <-
        festdtxX(estdtcuX = common$estdtcuR(), estdtaX = estdtaR(), geocuX = common$geocuR())
      estdtxG <<- copy(x)
      print('exit estdtxR')
      x
    }
  )
  
  #---utility  section----
  ylimR <- # ylim                    ----
  eventReactive(
    estdtxR(),
    {
      x <-
        estdtxR()[, range(x)] * 1.1
      ylimG <<- copy(x)
      x
    }
  )


  
  #---render section------------
  output$x111 <- renderLeaflet(x111D())
  output$x112 <- renderPlot(x112D())
  output$x121a <- gt::render_gt(x121D()[[1]])
  output$x121b <- gt::render_gt(x121D()[[2]])
  output$x122 <- gt::render_gt(x122D())
  output$x131 <- gt::render_gt(x131D())
  output$x132a <- gt::render_gt(x132D()[["local"]][[1]])
  output$x132b <- gt::render_gt(x132D()[["local"]][[2]])
  output$x132c <- gt::render_gt(x132D()[["custom"]][[1]])
  output$x132d <- gt::render_gt(x132D()[["custom"]][[2]])
  output$x411 <- gt::render_gt(x411D())
  output$x421 <- gt::render_gt(x421D())
  output$x431 <- gt::render_gt(x431D())
  output$x412 <- gt::render_gt(x412D())
  output$x422 <- gt::render_gt(x422D())
  output$x432 <- gt::render_gt(x432D())
  output$x311 <- DT::renderDT(x311D())
}

shinyApp(ui, server)
