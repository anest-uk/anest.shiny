library(shiny)
#-------------------------------packages-----------------------1
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
#------------------------------source---------------------------2
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
#--------------------------------parameters---------------------3
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

#-----------------Load static data (global) ------------------4
load("data/.RData")  # creates: f241021ad, pxosrdo2dd, x101, z110
f241021adG <- f241021ad
pxosrdo2ddG <- pxosrdo2dd
x101G <- x101
z110G <- z110


#---ui--------------------------------------------------------5
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

#---server----------------------------------------------------6

server <-  function(
    input, 
    output,
    session
) {
  
  #--------------------------------server
  source("R/server_common.R")
  source("R/server_listing.R")
  
  common <- server_common(input, output, session)
  
  server_listing(input, output, session, common)
  
  #----------------------------------------------------1ij-Time-series summary
  #------------------------------------------111 map
  
  x111D <- eventReactive(
    list(common$rc6tR(), common$rc6cuR(), common$geoaR(), pxosrdo2ddR(), common$z110R()),
    {
      if (verbose) {
        print("enter x111D")
      }
      #browser()
      
      x <- f111D(
        rc6tX = common$rc6tR(),
        rc6cuX = common$rc6cuR(),
        geoaX = common$geoaR(),
        pxosrdo2ddX = pxosrdo2ddR(),
        z110X = common$z110R(),
        colX = colx # punk green blue
      )
      x111G <<- copy(x)
      x
    }
  )
  
  f111D <- function(
    rc6tX = rc6tG,
    rc6cuX = rc6cuG,
    geoaX = geoaG,
    pxosrdo2ddX = pxosrdo2ddG,
    z110X = z110G,
    colX = colx, # punk green blue
    minzoom = 9, # 7 for national
    lightx = .7 # higher is lighter
  ) {
    x <-
      geoaX %>%
      .[, .(
        rc6,
        col = lighten(colX, lightx)[.BY[[1]]], ### capital in colX <<<<
        qtile, # shade tiles light
        lab
      ), by = .(qtile)] %>%
      .[
        rc6 == rc6tX, # with target district darker
        col := colX[.BY[[1]]],
        by = .(qtile)
      ] %>%
      f240810b( #->leaflet, colours for areas-to-shade in column 'col'
        .,
        x2 = pxosrdo2ddX,
        pva = z110X,
        minzoom = minzoom,
        maxzoom = 12
      ) %>%
      addPolygons( # outline custom districts
        data = pxosrdo2ddX[which(pxosrdo2ddX@data$name %in% irregpcode(rc6cuX)), ],
        fill = F,
        color = "orange",
        weight = 1,
        opacity = 1
      )
    x
  }
  #------------------------------------------112 timeseries
  f112D <-   function(
    tslideX = tslideG,
    estdtxX = estdtxG,
    ylimX = ylimG,
    geocuX = geocuG
  ) {
    x2c <- estdtxX %>%
      .[, .SD[, .(ii, date, lab, x = x - ifelse(tslideX == 0, 0, x[tslideX]))], .(qtile)] %>%
      .[, .SD[, .(ii, date, lab, x)], .(qtile)] %>%
      .[, qq := as.factor(qtile)] %>%
      .[, labx := ifelse(date == max(date), lab, NA)]
    x0 <- setNames(cobalt()[c("punk", "green", "blue")], as.character(1:3))
    x3 <- estdtxX[, .SD[, .(ifelse(tslideX == 0, 0, x[tslideX]))], .(qtile)][, mean(V1)] # base value for rebase level
    x2 <-
      estdtxX[, .SD[, .(ii, date, lab, x = x - ifelse(tslideX == 0, 0, x[tslideX]))], .(qtile)] %>%
      .[, qq := as.factor(qtile)] %>%
      .[, labx := ifelse(date == max(date), lab, NA)]
    x <- x2 %>%
      ggplot(., aes(date, x, color = qq, label = labx)) +
      geom_hline(yintercept = 0, linewidth = .4, linetype = "dotted", color = "grey40") +
      geom_line() +
      geom_point(size = .3) +
      geom_text_repel() +
      ylim(ylimX - x3) +
      labs(caption = geocuX[, paste0("Custom districts: ", paste0(sort(irregpcode(rc9)), collapse = ", "))]) +
      xlab("") +
      ylab(bquote(Delta ~ P ~ log ~ price ~ change)) +
      theme_bw() +
      theme(
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(size = .2, linetype = "dotted", color = pgmc),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 16, face = "plain"),
        axis.line.y.left = element_line(size = .1),
        axis.line.x.bottom = element_line(size = .1),
        legend.position = "none"
      ) +
      scale_color_manual(values = x0) +
      scale_x_date(
        breaks = as.Date(c("1995-01-01", "2000-01-01", "2010-01-01", "2020-01-01", "2024-01-01")),
        date_labels = "%Y",
        limits = c(as.Date(c("1994-12-31", "2027-12-31")))
      )
    x
  }
  
  x112D <-  eventReactive(
    list(input$tslider, estdtxR(), ylimR()), # 112 x(t)----
    {
      if (verbose) print("enter x112D")
      x <-
        f112D(
          tslideX = tslideR(),
          estdtxX = estdtxR(),
          ylimX = ylimR(),
          geocuX = common$geocuR()
        )
      x112G <<- copy(x)
      x
    }
  )
  
  #-------------------------------------------121 winding
  
  f121D <-function(
    estdtX = estdtlG, dfnxX = dfnxG, #----
    drangeX = range(dfnxxX),
    typeX = typeC, # L
    tbinX = tbinC,
    dfnxxX = dfnxX[-1, tbinC + 1, with = F] %>%
      setnames(., "x") %>%
      .[, sort(unique(x))], # current tbin
    d2X = dfnxX[-1, tbinC + 2, with = F] %>%
      setnames(., "x") %>%
      .[, sort(unique(x))] # annual dates t>0
  ) {
    d1 <- # daily
      seq.Date(from = drangeX[1], to = drangeX[2], by = "d")
    x1 <-
      estdtX %>% # local
      .[.(date = d1), on = c(date = "date"), roll = -Inf, j = .(date, xdotd)] %>%
      .[, .(ii = 1:.N, date, x = cumsum(xdotd))] %>%
      .[.(date2 = d2X), on = c(date = "date2")] %>%
      .[, .(date, x, xdot = c(x[1], diff(x)), ii = 1:.N)] %>%
      .[, .(ii, date, xdot, x)] %>%
      .[, .(date, xdot)] %>%
      .[date == as.Date("2009-02-28"), let(date, as.Date("2008-12-31"))] %>%
      .[, .(decade = substr(date, 1, 3), yr = substr(date, 4, 4), xdot = round(xdot, 3))] %>%
      dcast(., decade ~ yr, value.var = "xdot") %>%
      .[, decade := c(1990, 2000, 2010, 2020)]
    for (i in 2:length(x1)) x1[[i]] <- ifelse(is.na(x1[[i]]), "", as.character(round(x1[[i]], 3)))
    x2 <- gt::gt(x1) %>%
      gt::tab_footnote(
        footnote = f241108a(typeX, tbinX)[[1]]
      ) %>%
      gt::tab_footnote(
        footnote = f241108a(typeX, tbinX)[[2]]
      )
    x2
  }
  
  x121D <-
    eventReactive(
      list(common$estdtlR(), common$estdtcuR(), common$dfnxxR()), # 121 winding----
      {
        if (verbose) print("enter x121D")
        x2 <- f121D(
          estdt = common$estdtlR(),
          dfnxX = common$dfnxR()
        )
        x4 <- f121D(
          estdt = common$estdtcuR(),
          dfnxX = common$dfnxR(),
          typeX = "C"
        )
        x <- list(x2, x4)
        x121G <<- copy(x)
        x
      }
    )
  
  #------------------------------------------122 characteristics
  f122D <- function( # 122 characteristics----
                     rc6tX = rc6tG,
                     rssaX = rssaG,
                     rsscuX = rsscuG,
                     z110X = z110G
  ) {
    rsscux <- copy(rsscuX)[, lab := "CU000"] # R()
    f122 <- # combine rss and P characteristics
      function(rssx, z110X) {
        x0 <-
          z110X[rssx, on = c(rcx = "rc6")] %>%
          .[
            , .(
              frac = round(sum(nid) / z110X[nchar(rcx) == 6, sum(nid)], nfig3),
              nid = sum(nid),
              ppm2max = round(max(ppm2), nfig2),
              ppm2min = round(min(ppm2), nfig2),
              p = round(sum(pv) / sum(m2), nfig2)
            ),
            lab
          ] %>%
          .[rssx[, .(R2rsi = 1 - sum(ssek) / sum(sstr)), lab], on = c(lab = "lab")] %>%
          .[, .(
            lab = substr(lab, 1, 4),
            frac,
            R2rsi = round(R2rsi, 3),
            pnum = p,
            p = prettyNum(round(p, nfig3), big.mark = ","),
            p.cus = paste0(prettyNum(round(ppm2min, nfig2), big.mark = ","), "-", prettyNum(round(ppm2max, nfig2), big.mark = ","))
          )]
      }
    x0 <- f122(rssx = rsscux, z110X = z110X)
    x1 <- f122(rssx = rssaX, z110X = z110X)
    x2 <-
      rbind(x1, x0)[order(-pnum)][, -"pnum"]
    print(x2)
    x <-
      x2 %>%
      gt::gt(.) %>%
      cols_label(
        lab = gt::html("Area-band"),
        frac = gt::html("Fraction<br>properties"),
        R2rsi = gt::html("RSI R<sup>2</sup>"),
        p = gt::html("Aggregate"),
        p.cus = gt::html("Range")
      ) %>%
      tab_spanner(
        label = gt::html("£/m<sup>2</sup>"),
        columns = c(p.cus, p)
      ) %>%
      gt::tab_footnote(
        footnote = f241108a(typeC, tbinC)[[1]]
      ) %>%
      gt::tab_footnote(
        footnote = f241108a(typeC, tbinC)[[2]]
      )
    x122G <<- copy(x)
    x
  }
  
  x122D <- eventReactive(
    list(common$rc6tR(), rssaR(), rsscuR(), common$z110R()), # 122 characteristics----
    {
      if (verbose) print("enter x122D")
      x <- f122D(
        rc6tX = common$rc6tR(),
        rssaX = rssaR(),
        rsscuX = rsscuR(), 
        z110X = common$z110R()
      )
      x122G <<- copy(x)
      if (verbose) print("exit x122D")
      x
    }
  )
  
  #------------------------------------------131 summary
  f131D <- function( # 131 summary----
                     estdtxX = estdtxG,
                     tslideX = tslideG
  ) {
    x <-
      estdtxX %>%
      .[ii >= tslideX] %>%
      dcast(., ii ~ lab, value.var = "xdot") %>%
      .[, -"ii"] %>%
      as.matrix(.) %>%
      zoo(., estdtxX[, sort(unique(date))]) %>%
      table.Stats(., digits = 3) %>%
      data.table(., keep.rownames = T) %>%
      `[`(., i = -c(1, 2, 7, 11, 12, 13)) %>%
      gt::gt(.) %>%
      cols_label(
        rn = gt::html("Log return<br>summary")
      ) %>%
      gt::tab_footnote(
        footnote = f241108a(typeC, tbinC)[[1]]
      ) %>%
      gt::tab_footnote(
        footnote = f241108a(typeC, tbinC)[[2]]
      )
    x
  }
  
  x131D <- eventReactive(
    list(tslideR(), estdtxR()), # 131 summary----
    {
      if (verbose) print("enter x131D")
      x <- f131D(
        estdtxX = estdtxR(),
        tslideX = tslideR()
      )
      x131G <<- copy(x)
      x
    }
  )
  
  f132 <-      function( #
    geox = geoqG,
    steprip = stepripG,
    estdtlx = estdtlG, # only used for its date(ii) relation
    tmin = 20) { # tmin=input$tslider
    x0 <-
      geox[, grepstring(rc6)] %>%
      coread2(., steprip) %>% # or rc6tC
      .[, .(N = .N, mean = round(mean(as.numeric(retsa)), 4)), .(buy = substr(as.Date(buydate), 1, 4), sell = substr(as.Date(selldate), 1, 4))] %>%
      .[(buy >= estdtlx[ii >= tmin, substr(min(as.character(date)), 1, 4)])]
    x1 <-
      x0 %>%
      dcast(.,
            buy ~ sell,
            value.var = "mean" # the value is unique so any aggregator function is ok
      )
    for (i in 2:length(x1)) x1[[i]] <- ifelse(is.na(x1[[i]]), "", as.character(round(x1[[i]], 3)))
    x2 <-
      x0 %>%
      dcast(.,
            buy ~ sell,
            value.var = "N"
      )
    for (i in 2:length(x2)) x2[[i]] <- ifelse(is.na(x2[[i]]), "", x2[[i]])
    x3 <- list(x1, x2)
    x3
  }
  
  #------------------------------------------132 trade summary
  f132D <-      function( # 132 trade summary(2)----
                          tslideX = tslideG,
                          geoqX = geoqG,
                          geocuX = geocuG,
                          estdtlX = estdtlG) {
    steprip <- stepripG
    tminx <- tslideX
    x1 <- f132(
      geox = geoqX, # geoqR()
      steprip = steprip,
      estdtlx = estdtlX, # estdtlR()
      tmin = tminx # tmin=input$tslider
    )
    x2 <- f132(
      geox = geocuX[, .(rc6 = rc9)], # geoqR()
      steprip = steprip,
      estdtlx = estdtlX, # estdtlR()
      tmin = tminx # tmin=input$tslider
    )
    x <- list(
      local = x1,
      custom = x2
    )
    x[["local"]][[1]] <-
      x[["local"]][[1]] %>%
      gt::gt(.) %>%
      tab_header(., title = "Local - Return") %>%
      opt_align_table_header(., align = "left") %>%
      tab_options(heading.title.font.size = 14) %>%
      tab_spanner(
        label = gt::html("sell"),
        columns = 2:ncol(x[["local"]][[1]])
      )
    x[["local"]][[2]] <-
      x[["local"]][[2]] %>%
      gt::gt(.) %>%
      tab_header(., title = "Local - Number") %>%
      opt_align_table_header(., align = "left") %>%
      tab_options(heading.title.font.size = 14) %>%
      tab_spanner(
        label = gt::html("sell"),
        columns = 2:ncol(x[["local"]][[2]])
      )
    x[["custom"]][[1]] <-
      x[["custom"]][[1]] %>%
      gt::gt(.) %>%
      tab_header(., title = "Custom - Return") %>%
      opt_align_table_header(., align = "left") %>%
      tab_options(heading.title.font.size = 14) %>%
      tab_spanner(
        label = gt::html("sell"),
        columns = 2:ncol(x[["custom"]][[1]])
      )
    x[["custom"]][[2]] <-
      x[["custom"]][[2]] %>%
      gt::gt(.) %>%
      tab_header(., title = "Custom - Number") %>%
      opt_align_table_header(., align = "left") %>%
      tab_options(heading.title.font.size = 14) %>%
      tab_spanner(
        label = gt::html("sell"),
        columns = 2:ncol(x[["custom"]][[2]])
      )
    x132G <<- copy(x)
    if (verbose) print("exit x132D")
    x
  }
  
  x132D <-      eventReactive(
    list(tslideR(), common$geoqR(), common$estdtlR()), # 132 trade summary(2)----
    {
      if (verbose) print("enter x132D")
      x <- f132D(
        tslideX = tslideR(),
        geoqX = common$geoqR(),
        geocuX = common$geocuR(),
        estdtlX = common$estdtlR()
      )
      x132G <<- copy(x)
      if (verbose) print("exit x132D")
      x
    }
  )
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
  
  
  f311D <- function(geo0X = geo0G, z110X = z110G, rc6tX = rc6tG) # 311 constituents----
  {
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
  
  x311D <- eventReactive(
    list(
      common$geo0R(), 
      common$z110R(), 
      rc6tX = common$rc6tR()), # 311 custom constituents output table----
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
  
  f411D <- function(geoqX = geoqG, rc6tX = rc6tG, rssX = rssG) { # 2x11 accuracy----tbin----
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
  
  
  f412D <- function(geocuX = geocuG, rc6tX = rc6tG, rsscuX = rsscuG) { # 2x11cu accuracy--custom--tbin----
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
  
  
  f421D <- function(geoqX = geoqG, rc6tX = rc6tG, rssX = rssG) { # 221 accuracy----trim----
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
  
  
  f422D <- function(geocuX = geocuG, rc6tX = rc6tG, rsscuX = rsscuG) { # 221cu accuracy----trim----
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
  
  
  f431D <- function(geoqX = geoqG, rc6tX = rc6tG, rssX = rssG) { # 231 accuracy----in/out----
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
  
  
  f432D <- function(geocuX = geocuG, rc6tX = rc6tG, rsscuX = rsscuG) { # 231cu accuracy----in/out----
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
  
  
  
  
  
  #-----------------------------reactive
  x411D <- eventReactive(
    list(common$geoqR(), common$rc6tR(), rssR()), # 2x11 accuracy----tbin----
    {
      if (verbose) print("enter x411G")
      x <- f411D(geoqX = common$geoqR(), rc6tX = common$rc6tR(), rssX = rssR())
      x411G <<- copy(x)
      x
    }
  )
  
  x412D <- eventReactive(
    list(common$geocuR(), common$rc6tR(), rsscuR()), # 2x11cu accuracy--custom--tbin----
    {
      if (verbose) print("enter x411Gcu")
      x <- f412D(geocuX = common$geocuR(), rc6tX = common$rc6tR(), rsscuX = rsscuR())
      x412G <<- copy(x)
      x
    }
  )
  
  x421D <- eventReactive(
    list(common$geoqR(), common$rc6tR(), rssR()), # 221 accuracy----trim----
    {
      if (verbose) print("enter x421D")
      x <- f421D(geoqX = common$geoqR(), rc6tX = common$rc6tR(), rssX = rssR())
      x421G <<- copy(x)
      x
    }
  )
  
  x422D <- eventReactive(
    list(common$geocuR(), common$rc6tR(), rsscuR()), # 221cu accuracy----trim----
    {
      if (verbose) print("enter x422D")
      x <- f422D(geocuX = common$geocuR(), rc6tX = common$rc6tR(), rsscuX = rsscuR())
      x422G <<- copy(x)
      x
    }
  )
  
  x431D <- eventReactive(
    list(common$geoqR(), common$rc6tR(), rssR()), # 231 accuracy----in/out----
    {
      if (verbose) print("enter x431D")
      x <- f431D(geoqX = common$geoqR(), rc6tX = common$rc6tR(), rssX = rssR())
      x431G <<- copy(x)
      x
    }
  )
  
  x432D <- eventReactive(
    list(common$geocuR(), common$rc6tR(), rsscuR()), # 231cu accuracy----in/out----
    {
      if (verbose) print("enter x432D")
      x <- f432D(geocuX = common$geocuR(), rc6tX = common$rc6tR(), rsscuX = rsscuR())
      x432G <<- copy(x)
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
