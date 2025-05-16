server_timeseries <- function(input, output, session, common) {
  #---combo section----

  x111D <- eventReactive( # 111 leaflet----
    list(
      common$rc6tR(),
      common$rc6cuR(),
      common$geoaR(),
      common$pxosrdo2ddR(),
      common$z110R()
    ),
    {
      print("=== enter server_timeseries ===")



      if (verbose) {
        print("enter x111D xxx")
      }
      x <- f111D(
        rc6tX = common$rc6tR(),
        rc6cuX = common$rc6cuR(),
        geoaX = common$geoaR(),
        pxosrdo2ddX = common$pxosrdo2ddR(),
        z110X = common$z110R(),
        colX = colx # punk green blue
      )
      x111G <<- copy(x)
      if (verbose) {
        print("exit x111D")
      }

      x
    }
  )

  f111D <- function( #----
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
  x112D <- eventReactive( # 112 x(t)----
    list(input$tslider, common$estdtxR(), common$ylimR()),
    {
      if (verbose) print("enter x112D<<<<<<<<<<<<<<<<<<<<<<<<")
      x <-
        f112D(
          tslideX = common$tslideR(),
          estdtxX = common$estdtxR(),
          ylimX = common$ylimR(),
          geocuX = common$geocuR()
        )
      x112G <<- copy(x)
      x
    }
  )
  # print("Creating x112D...")
  # x112D <- eventReactive( # 112 x(t)----
  #                         list(input$tslider),#input$tslider,
  #                         #list(common$tslideR),
  #                         {
  #                           req(common$estdtxR())
  #                           x <- ggplot(mtcars,aes(wt,drat))+geom_point()
  #                           x
  #                         }
  # )

  f112D <- function( #----
                    tslideX = tslideG,
                    estdtxX = estdtxG,
                    ylimX = ylimG,
                    geocuX = geocuG) {
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
  #-------------------------------------------121 winding

  x121D <- eventReactive( # 121 winding----
    list(common$estdtlR(), common$estdtcuR(), common$dfnxxR()),
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
  f121D <- function( #----
                    estdtX = estdtlG, dfnxX = dfnxG,
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

  x122D <- eventReactive( #----
    list(common$rc6tR(), common$rssaR(), common$rsscuR(), common$z110R()),
    {
      if (verbose) print("enter x122D")
      x <- f122D(
        rc6tX = common$rc6tR(),
        rssaX = common$rssaR(),
        rsscuX = common$rsscuR(),
        z110X = common$z110R()
      )
      x122G <<- copy(x)
      if (verbose) print("exit x122D")
      x
    }
  )

  #------------------------------------------131 summary
  f122D <- function( # 122 characteristics----
                    rc6tX = rc6tG,
                    rssaX = rssaG,
                    rsscuX = rsscuG,
                    z110X = z110G) {
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
    # print(x2)
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

  x131D <- eventReactive( #----
    list(common$tslideR(), common$estdtxR()),
    {
      if (verbose) print("enter x131D")
      x <- f131D(
        estdtxX = common$estdtxR(),
        tslideX = common$tslideR()
      )
      x131G <<- copy(x)
      x
    }
  )
  f132 <- function(
      #
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
  f131D <- function( # 131 summary----
                    estdtxX = estdtxG,
                    tslideX = tslideG) {
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

  x132D <- eventReactive( # 132 trade summary(2)----
    list(common$tslideR(), common$geoqR(), common$estdtlR()),
    {
      if (verbose) print("enter x132D")
      x <- f132D(
        tslideX = common$tslideR(),
        geoqX = common$geoqR(),
        geocuX = common$geocuR(),
        estdtlX = common$estdtlR()
      )
      x132G <<- copy(x)
      if (verbose) print("exit x132D")
      x
    }
  )
  f132D <- function( # 132 trade summary(2)----
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



  f132 <- function( #-----132 trade summary(2)----
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


  #---render section----
  output$x111 <- renderLeaflet(x111D())


  # stoutput$x112 <- renderPlot(x112D())
  output$x112 <- renderPlot({
    print("=== in renderPlot for x112 ===")
    print("execute: req(common$estdtxR())")
    req(common$estdtxR())
    tryCatch(
      x112D(),
      error = function(e) {
        print("!!! Error inside renderPlot(x112D())")
        print(e)
        ggplot() +
          ggtitle("ERROR")
      }
    )
    x112D()
  })

  output$x121a <- gt::render_gt(x121D()[[1]])
  output$x121b <- gt::render_gt(x121D()[[2]])
  output$x122 <- gt::render_gt(x122D())
  output$x131 <- gt::render_gt(x131D())
  output$x132a <- gt::render_gt(x132D()[["local"]][[1]])
  output$x132b <- gt::render_gt(x132D()[["local"]][[2]])
  output$x132c <- gt::render_gt(x132D()[["custom"]][[1]])
  output$x132d <- gt::render_gt(x132D()[["custom"]][[2]])
  print("Leaving server_timeseries()...")
}
