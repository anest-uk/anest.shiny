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
  stepripG <<- "data/smallrip/"
  gridheight <<- "630px"
  gridheight2 <<- "830px"
  gridheight3 <<- "1020px"
  gridheight4 <<- "1200px"
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

#---ui-grid of 4-----------------------------5----
ui <- tagList(
  # Theme and CSS
  bslib::bs_theme_dependencies(bs_theme()),
  tags$style(HTML("
    /* Target vertical pills globally */
    .nav-pills.flex-column .nav-link {
      background-color: red !important;  /* sanity check */
      flex: 0 0 auto !important;
      width: auto !important;
      padding-left: 0.5rem !important;
      padding-right: 0.5rem !important;
    }
  ")),

  # Actual page
  grid_page(
    layout = c(
      "A1000  A2000", # actionbutton text-heading
      "A3000  A4000" # sidebar-control nav_panel
    ),
    row_sizes = c("200px", "1fr"),
    col_sizes = c("250px", "1fr"),
    gap_size = ".1rem",
    ui1000(),
    ui3000(),
    ui2000(),
    ui4000()
  )
)
#---server-----------------------------------6----
server <- function(
    input,
    output,
    session) {
  #--------------------------------server
  source("R/server_common.R") # leave with full name
  source("R/s3000.R") # 0-sidebar

  # gen1 pages
  source("R/server_listing.R") # 2-lis
  source("R/server_timeseries.R") # 1-tim
  source("R/server_constituents.R") # 3-con
  source("R/server_accuracy.R") # 4-acc
  # gen2 pages
  source("R/s4100.R") # 5-tim1
  source("R/s4200.R") # 6-lis1
  source("R/s4300.R") # 7-con1

  # make_price_colormap <- function(light = FALSE) {
  #   base_palette <- c( #from cobalt() which is sampled from the Rstudio theme
  #     "#FF628C", # punk
  #     "#ED9304", # onch
  #     "#B5C800", # best-shot yellow-green (new)
  #     "#35CA05", # green
  #     "#0082F4", # blue
  #     "#7B2FF7", # midnight
  #     "#9434F8"  # 250715 add
  #   )
  #   light_palette <- c(
  #     "#FF9FB5", # punk
  #     "#F7B84F", # onch
  #     "#84E26A", # green
  #     "#66B2F7", # blue
  #     "#BBA3F9"  # midnight
  #   )
  #
  #   colors <- if (light) light_palette else base_palette
  #   grDevices::colorRampPalette(colors, space = "Lab")
  # }
  #

  common <- server_common(input, output, session)


  server_timeseries(input, output, session, common)
  server_listing(input, output, session, common)
  server_constituents(input, output, session, common)
  server_accuracy(input, output, session, common)

  # gen2 servers named s[gpij] g=grid p=page i=row j=col
  s3000(input, output, session, common) # sidepanel
  s4100(input, output, session, common) # timeseries
  s4200(input, output, session, common) # listing
  s4300(input, output, session, common) # constituent

  # ===-output: controls select/compute/suggest----
  # selectedrc6R <- reactive({ # --rc6 selected----
  #   x0 <- sort(unique(input$rc6cC))
  #   x1 <- # exclude non-rc6 higher tree nodes
  #     x0 %>%
  #     .[(nchar(.) == 6) &
  #       (substr(., 3, 3) == "-") &
  #       (substr(., 6, 6) == "-")]
  #   x <- paste0(paste0(x1, collapse = ","))
  #   print(paste0("selectedrc6R: ", x))
  #   selectedrc6G <<- copy(x)
  #   x
  # })
  selectedrc6R <- reactive({ # --rc6 selected----
    x1 <- 
      common$rc6cR()%>% #rc6c: includes rc6t
      sort(.)%>%
      paste0(., collapse = ",") #1 comma-collapse
    print(paste0("selectedrc6R: ", x1))#2 add description
    selectedrc6G <<- copy(x1)
    x1
  })

  computedrc6R <- reactive({ # ---rc6 computed----
    rsicX <- common$rescR() #rescR: includes rc6t
    x1 <- rsicX$kfoldsse[, rc6] %>% .[(nchar(.) == 6) & (substr(., 3, 3) == "-") & (substr(., 6, 6) == "-")]
    x <- paste0(sort(unique(x1)), collapse = ",") #1 comma-collapse
    print(paste0("computedrc6R: ", x)) #2 add description
    computedrc6G <<- copy(x)
    x
  })
  output$selrc6 <- renderText({ #------selected----
    paste0("selected:   ", selectedrc6R()) #includes rc6t
  })
  output$comrc6 <- renderText({ #-----computed----
    paste0("computed: ", computedrc6R())#includes rc6t
  })
  output$defrc6 <- renderText({ #----suggested----
    paste0(c("suggested: ", paste0(sort(rc6deR()),collapse=',')), collapse = "")#includes rc6t
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
      if (any(nchar(input$rc6tC) != 6)) {
      }

      if (verbose) {
        print("enter updateTreeInput")
      }
      if ( # guard against invalid selection
        (!is.null(input$rc6tC)) # & # empty tree
      ) {
        rc6c <-
          rc6deR()
        if (verbose) {
          print("treeinput update with custom peers:")
          print(rc6c)
        }
        updateTreeInput(
          inputId = "rc6cC",
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
      if (!is.null(input$rc6tC)) # guard against empty tree
        {
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
