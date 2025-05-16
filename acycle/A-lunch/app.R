library(shiny)
{ #----
  { #----
    { #----
      #-------packages-----------------------1----
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
      #------------------------source--------2----
      stepripG <<- "data/smallrip/"
      source("R/c-cleanlib.R")
      source("R/rctree.R")
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
      #--------------------------parameters--3----
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

      # Load static data (global) ------------4----
      load("data/.RData") # creates: f241021ad, pxosrdo2dd, x101, z110
      f241021adG <- f241021ad
      pxosrdo2ddG <- pxosrdo2dd
      x101G <- x101
      z110G <- z110
    }
  }
}
#---ui---------------------------------------5----
{ #----
  { #----
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

  common <- server_common(input, output, session)

  server_listing(input, output, session, common)
  print("Calling server_timeseries()...")
  server_timeseries(input, output, session, common)

  #--------------------Time-series summary end----
  selectedrc6R <- reactive({ # string: rc6 selected----
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
  computedrc6R <- reactive({ # string: rc6 computed----
    rsicuX <- common$rsicuR()
    x1 <- rsicuX$kfoldsse[, rc6] %>% .[nchar(.) == 6]
    x <- paste0(paste0(sort(unique(x1)), collapse = ","))
    print(paste0("computedrc6R: ", x))
    computedrc6G <<- copy(x)
    x
  })
  #-output----------------------------------------
  #-messages about selected/computed/suggested----
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

  #----------------------------a few reactives----

  x101R <- reactive({ #---------=initial dates----
    x <- copy(x101)
    x101G <<- copy(x)
    x
  })
  dfnR <- reactive({ # not used?
    x <- data.table(date = c(as.Date("1994-12-31"), common$estdtR()[, sort(unique(date))]))[, let(i, (0:(.N - 1)))]
    dfnG <<- copy(x)
    x
  })

  tslideR <- reactive({ #---------------slider----
    x <- input$tslider
    tslideG <<- copy(x)
    x
  })

  #-----------------------------custom section----
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
      } # length(rc6c)>0 end
    }
  )

  rc6deR <- eventReactive( #--ault custom rc6 ----
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

  nxcuR <- eventReactive( #--custom nx compute----
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






  #---utility----


  f311D <- function( #--------311 constituents----
                    geo0X = geo0G,
                    z110X = z110G,
                    rc6tX = rc6tG) {
    if (verbose) print("enter 311")
    x1 <-
      geo0X[, .(rc3, rc6, qtile)] %>%
      z110X[., on = c(rcx = "rc6")] %>%
      .[, .(rc3, rc6 = rcx, nid, ppm2 = round(ppm2), quantile = paste0("local-", qtile))]
    x <-
      DT::datatable(
        x1,
        options = list(
          search = list(search = rc6tX),
          columnDefs = list(list(className = "dt-center", targets = 1:4, searchable = F, targets = 3:5)),
          paging = T,
          pageLength = 100,
          initComplete = JS(
            "function(settings, json) {",
            "$('body').css({'font-family': 'Calibri'});",
            "}"
          )
        ),
        rownames = F
      ) %>%
      DT::formatStyle(0, target = "row", lineHeight = "70%")
    x311G <<- copy(x)
    x
  }

  #---render section----
  x311D <- eventReactive( #-----311 constituents----
    list(
      common$geo0R(),
      common$z110R(),
      rc6tX = common$rc6tR()
    ), # -311 custom constituents output table----
    {
      if (verbose) print("enter 311")
      x <- f311D(
        geo0X = common$geo0R(),
        z110X = common$z110R(),
        rc6tX = substr(common$rc6tR(), 1, 3) # arbitrarily initialise it to the area
      )
      x311G <<- copy(x)
      x
    }
  )

  #------------------ accuracy
  f411D <- function( # ---   accuracy------tbin----
                    geoqX = geoqG,
                    rc6tX = rc6tG,
                    rssX = rssG) {
    if (verbose) print("enter f411G")
    x1 <-
      data.table(tbin = 1:3, freq = c("lo", "hi", "an"))
    x2 <-
      rssX %>% # use global no filters
      .[geoqX, on = c(rc6 = "rc6")] %>%
      .[type == "L"] %>%
      .[itrim == itriC] %>%
      .[, .(n = sum(n), ssek = sum(ssek)), .(tbin, rc6)]
    x3 <-
      rbind(
        x2[, .(span = "index.average", mse = round(sqrt(sum(ssek) / sum(n)), 4)), tbin],
        x2[rc6 == rc6tX, .(span = rc6tX, mse = round(sqrt(sum(ssek) / sum(n)), 4)), tbin]
      ) %>%
      dcast(., tbin ~ span, value.var = "mse") %>%
      x1[., on = c(tbin = "tbin")] %>%
      .[, -"tbin"]
    x <-
      gt::gt(x3) %>%
      gt::tab_footnote(
        footnote = f241108a(typeC, tbinC)[[1]]
      )
    x411G <<- copy(x)
    x
  }


  f412D <- function( # - accuracy--custom--tbin----
                    geocuX = geocuG,
                    rc6tX = rc6tG,
                    rsscuX = rsscuG) {
    x1 <-
      data.table(tbin = 1:3, freq = c("lo", "hi", "an"))
    x2 <-
      rsscuX %>% # use global no filters
      .[geocuX, on = c(rc6 = "rc9")] %>%
      .[, .(n, ssek, tbin = tbinC, rc6)]
    x3 <-
      rbind(
        x2[, .(span = "index.average", mse = round(sqrt(sum(ssek) / sum(n)), 4)), tbin],
        x2[rc6 == rc6tX, .(span = rc6tX, mse = round(sqrt(sum(ssek) / sum(n)), 4)), tbin]
      ) %>%
      dcast(., tbin ~ span, value.var = "mse") %>%
      x1[., on = c(tbin = "tbin")] %>%
      .[, -"tbin"]
    x <-
      gt::gt(x3) %>%
      gt::tab_footnote(footnote = f241108a(tc = "C", tbinC)[[1]]) %>%
      gt::tab_footnote(footnote = paste0("only freq=hi is computed for custom"))
    x412G <<- copy(x)
    x
  }


  f421D <- function( # --------accuracy----trim----
                    geoqX = geoqG,
                    rc6tX = rc6tG,
                    rssX = rssG) {
    x1 <-
      data.table(itrim = 1:3, threshold = c("0.0", "0.1", "0.5"))
    x2 <-
      rssX %>%
      .[geoqX, on = c(rc6 = "rc6")] %>%
      .[type == "L"] %>%
      .[tbin == tbinC] %>%
      .[, .(n = sum(n), ssek = sum(ssek)), .(itrim, rc6)]
    x3 <- rbind(
      x2[, .(span = "index.average", mse = round(sqrt(sum(ssek) / sum(n)), 4)), itrim],
      x2[rc6 == rc6tX, .(span = rc6tX, mse = round(sqrt(sum(ssek) / sum(n)), 4)), itrim]
    ) %>%
      dcast(., itrim ~ span, value.var = "mse") %>%
      x1[., on = c(itrim = "itrim")] %>%
      .[, -"itrim"]
    x <-
      gt::gt(x3) %>%
      gt::tab_footnote(
        footnote = f241108a(typeC, tbinC)[[1]]
      ) %>%
      gt::tab_footnote(
        footnote = f241108a(typeC, tbinC)[[2]]
      )
    x421G <<- copy(x)
    x
  }


  f422D <- function( # ------- accuracy----trim----
                    geocuX = geocuG,
                    rc6tX = rc6tG,
                    rsscuX = rsscuG) {
    x1 <-
      data.table(itrim = 1:3, threshold = c("0.0", "0.1", "0.5"))
    x2 <-
      rsscuX %>%
      .[geocuX, on = c(rc6 = "rc9")] %>%
      .[, .(n, ssek, itrim = itriC, rc6)]
    x3 <- rbind(
      x2[, .(span = "index.average", mse = round(sqrt(sum(ssek) / sum(n)), 4)), itrim],
      x2[rc6 == rc6tX, .(span = rc6tX, mse = round(sqrt(sum(ssek) / sum(n)), 4)), itrim]
    ) %>%
      dcast(., itrim ~ span, value.var = "mse") %>%
      x1[., on = c(itrim = "itrim")] %>%
      .[, -"itrim"]
    x <-
      gt::gt(x3) %>%
      gt::tab_footnote(footnote = f241108a(tc = "C", tbinC)[[1]]) %>%
      gt::tab_footnote(footnote = paste0("only threshold=0.1 is computed for custom"))
    x432G <<- copy(x)
    x
  }


  f431D <- function( # ------accuracy----in/out----
                    geoqX = geoqG,
                    rc6tX = rc6tG,
                    rssX = rssG) {
    x1 <-
      rssX %>%
      .[geoqX, on = c(rc6 = "rc6")] %>%
      .[type == "L"] %>%
      .[tbin == tbinC] %>%
      .[itrim == itriC] %>%
      .[, .(n = sum(n), ssek = sum(ssek), ssei = sum(ssei)), .(itrim, rc6)]
    x2 <-
      rbind(
        x1[, .(outsamp = round(sqrt(sum(ssek) / sum(n)), 4), insamp = round(sqrt(sum(ssei) / sum(n)), 4))],
        x1[rc6 == rc6tX, .(outsamp = round(sqrt(sum(ssek) / sum(n)), 4), insamp = round(sqrt(sum(ssei) / sum(n)), 4))]
      ) %>%
      as.matrix(.) %>%
      t(.) %>%
      as.data.table(., keep.rownames = T)
    setnames(x2, c("domain", "index.average", rc6tX)[1:ncol(x2)])
    if (ncol(x2) == 3) x2 <- x2[, c(1, 3, 2)]
    x <-
      gt::gt(x2) %>%
      gt::tab_footnote(
        footnote = f241108a(typeC, tbinC)[[1]]
      ) %>%
      gt::tab_footnote(
        footnote = f241108a(typeC, tbinC)[[2]]
      )
    x431G <<- copy(x)
    x
  }


  f432D <- function( # ------accuracy----in/out----
                    geocuX = geocuG,
                    rc6tX = rc6tG,
                    rsscuX =
                      rsscuG) {
    x1 <-
      rsscuX %>%
      .[geocuX, on = c(rc6 = "rc9")] %>%
      .[, .(n, ssek, ssei, itrim = itriC, rc6)]
    x2 <-
      rbind(
        x1[, .(outsamp = round(sqrt(sum(ssek) / sum(n)), 4), insamp = round(sqrt(sum(ssei) / sum(n)), 4))],
        x1[rc6 == rc6tX, .(outsamp = round(sqrt(sum(ssek) / sum(n)), 4), insamp = round(sqrt(sum(ssei) / sum(n)), 4))]
      ) %>%
      as.matrix(.) %>%
      t(.) %>%
      as.data.table(., keep.rownames = T)
    setnames(x2, c("domain", "index.average", rc6tX)[1:ncol(x2)])
    if (ncol(x2) == 3) x2 <- x2[, c(1, 3, 2)]
    x <-
      gt::gt(x2) %>%
      gt::tab_footnote(footnote = f241108a(tc = "C", tbinC)[[1]])
    x432G <<- copy(x)
    x
  }


  x411D <- eventReactive( # --accuracy----tbin----
    list(common$geoqR(), common$rc6tR(), rssR()),
    {
      if (verbose) print("enter x411G")
      x <- f411D(geoqX = common$geoqR(), rc6tX = common$rc6tR(), rssX = rssR())
      x411G <<- copy(x)
      x
    }
  )

  x412D <- eventReactive( # accuracy--custom--tbin----
    list(common$geocuR(), common$rc6tR(), common$rsscuR()),
    {
      if (verbose) print("enter x411Gcu")
      x <- f412D(geocuX = common$geocuR(), rc6tX = common$rc6tR(), rsscuX = common$rsscuR())
      x412G <<- copy(x)
      x
    }
  )

  x421D <- eventReactive( #-- accuracy----trim----
    list(common$geoqR(), common$rc6tR(), rssR()),
    {
      if (verbose) print("enter x421D")
      x <- f421D(geoqX = common$geoqR(), rc6tX = common$rc6tR(), rssX = rssR())
      x421G <<- copy(x)
      x
    }
  )

  x422D <- eventReactive( #-- accuracy----trim----
    list(common$geocuR(), common$rc6tR(), common$rsscuR()),
    {
      if (verbose) print("enter x422D")
      x <- f422D(geocuX = common$geocuR(), rc6tX = common$rc6tR(), rsscuX = common$rsscuR())
      x422G <<- copy(x)
      x
    }
  )

  x431D <- eventReactive( #-accuracy----in/out----
    list(common$geoqR(), common$rc6tR(), rssR()),
    {
      if (verbose) print("enter x431D")
      x <- f431D(geoqX = common$geoqR(), rc6tX = common$rc6tR(), rssX = rssR())
      x431G <<- copy(x)
      x
    }
  )

  x432D <- eventReactive( #-accuracy----in/out----
    list(common$geocuR(), common$rc6tR(), common$rsscuR()),
    {
      if (verbose) print("enter x432D")
      x <- f432D(geocuX = common$geocuR(), rc6tX = common$rc6tR(), rsscuX = common$rsscuR())
      x432G <<- copy(x)
      x
    }
  )

  output$x411 <- gt::render_gt(x411D())
  output$x421 <- gt::render_gt(x421D())
  output$x431 <- gt::render_gt(x431D())
  output$x412 <- gt::render_gt(x412D())
  output$x422 <- gt::render_gt(x422D())
  output$x432 <- gt::render_gt(x432D())
  output$x311 <- DT::renderDT(x311D())
}

shinyApp(ui, server)
