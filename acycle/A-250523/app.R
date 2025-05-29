library(shiny)


{ #----
  { #----
    { #----
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
      source("R/function.R")
      #---------------------app code
      source("ui_main.R")
      source("R/ui_accuracy.R")
      source("R/ui_action.R")
      source("R/ui_constituents.R")
      source("R/ui_header.R")
      source("R/ui_listing.R")
      source("R/ui_notes.R")
      source("R/ui_sidebar.R")
      source("R/ui_timeseries.R")
      source("R/ui_tim1.R")
      #--------------------------=parameters--3----
      gridheight <<- "630px"
      gridheight2 <<- "830px"
      gridheight3 <<- "1020px"
      colx <<- cobalt()[c(4, 2, 1)]
      sf <<- 3
      pgmc <<- "grey50"
      #-------------------Pseudo=Control
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

      #       print('######################################################################## app.R global assign')
      #this is done for debug purposes only: the LHS G globals are not used in the app; the app uses RHS as static, types
      f241021adG <<- f241021ad
      f250519adG <<- f250519ad
      pxosrdo2ddG <<- pxosrdo2dd
      f241229bdG <<- f241229bd
      z110G <<- z110
      x101G <<- x101

      # # f250509ed #no
      # f241021ad # yes
      # f250519ad # yes
      # pxosrdo2dd # yes
      # f241229bd # yes
      # z110 # yes
      # x101 # yes
      # rm(f250509ed)

    }
  }
}
#---ui---------------------------------------5----
{ #---
  { #---
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
  }
}
#---server-----------------------------------6----
server <- function(
    input,
    output,
    session) {
  #--------------------------------server
  source("R/server_common.R")
  source("R/server_listing.R")
  source("R/server_timeseries.R")
  source("R/server_constituents.R")
  source("R/server_accuracy.R")
  source("R/server_tim1.R")

  common <- server_common(input, output, session)

  server_timeseries(input, output, session, common)
  server_listing(input, output, session, common)
  server_constituents(input, output, session, common)
  server_accuracy(input, output, session, common)
  server_tim1(input, output, session, common)

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
    rsiccX <- common$rsiccR()
    x1 <- rsiccX$kfoldsse[, rc6] %>% .[nchar(.) == 6]
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

  nxccR <- eventReactive( #--custom nx compute----
    common$geoccR(),
    {
      if (verbose) print("enter nxccR")
      x <-
        common$geoccR()[, .(nx, rc3, qtile, lab)] %>%
        unique(.)
      nxccG <<- copy(x)
      x
    }
  )
}

shinyApp(ui, server)
