server_listing <- function(input, output, session, common) {
  f211D <- #---summary called in both listings ----
    function(estdtlX = common$estdtlR(), # single
             geoqX = common$geoqR(), # footnote only 'this qtile'
             dfnxxX = common$dfnxxR(), # single
             typeX = typeC) {
      if (verbose) print("enter x211D")
        #fread("data/f241122ad.csv") %>%
        fpx <- file.path(data_dirG, "f241122ad.csv")
        print(paste0('filepath : ',fpx))
        ddd <- fread(fpx)[,sort(unique(date))]
        x1 <-
        fread(fpx) %>%
        .[geoqX[, .(rc6, lab)], on = c(rc6 = "rc6")] %>%
        .[, .(cum = sum(cum)), .(lab, nh, date)] %>%
        .[data.table(date = ddd[-1], i = 1:(length(ddd) - 1)), on = c(date = "date")] %>% # dfnG is all dates, all frequencies
        dcast(., date + i + lab ~ nh, value.var = "cum") %>% #
        .[order(date), .(date, t = i, lab, NF, NH, UF, UH)]
         #
        #browser()
        # 
        # 
        # browser()
        # fread(fpx) %>% #250520 remove dependency on dfnxxX
        # .[geoqX[, .(rc6, lab)], on = c(rc6 = "rc6")] %>%
        # .[, .(cum = sum(cum)), .(lab, nh, date)] %>%
        # .[data.table(date = dfnxxX[-1], i = 1:(length(dfnxxX) - 1)), on = c(date = "date")] %>% # dfnG is all dates, all frequencies
        # dcast(., date + i + lab ~ nh, value.var = "cum") %>% #
        # .[order(date), .(date, t = i, lab, NF, NH, UF, UH)]
        
      x2 <-
        estdtlX %>%
        .[, .(t = c(0, ii), days = c(NA, days), date = c(date[1] - days[1], date), xdot = c(NA, xdot), x = c(0, x))] %>%
        x1[., on = c(t = "t")] %>%
        .[1, let(NF, 0)] %>%
        .[1, let(NH, 0)] %>%
        .[1, let(UF, 0)] %>%
        .[1, let(UH, 0)] %>%
        .[, .(t,
          date = i.date, days, xdot, x,
          NF = c(0, diff(NF)),
          NH = c(0, diff(NH)),
          UF = c(0, diff(UF)),
          UH = c(0, diff(UH)),
          tot = c(0, diff(NF + NH + UF + UH))
        )] %>%
        .[-1, .(
          t,
          date,
          days,
          return = round(xdot, sf),
          cumreturn = round(x, sf),
          newhouse = round(NH / tot, sf),
          usedhouse = round(UH / tot, sf),
          newflat = round(NF / tot, sf),
          usedflat = round(UF / tot, sf),
          total = round(tot),
          perday = round(tot / days, 1)
        )]
      x3 <- # districts footnote
        geoqX[
          , paste0("Districts: ", paste0(sort(irregpcode(rc6)), collapse = ", "))
        ]
      x <-
        gt::gt(x2) %>%
        gt::tab_footnote(
          footnote = f241108a(typeX, tbinC)[[1]]
        ) %>%
        gt::tab_footnote(
          footnote = f241108a(typeX, tbinC)[[2]],
          locations = NULL,
          placement = c("auto", "right", "left")
        ) %>%
        gt::tab_header(
          title = x3
        ) %>%
        cols_label(
          date = gt::html("end date"),
          cumreturn = gt::html("cumulative"),
          newhouse = gt::html("new house"),
          usedhouse = gt::html("used house"),
          newflat = gt::html("new flat"),
          usedflat = gt::html("used flat"),
          perday = gt::html("per day"),
          total = gt::html("total")
        ) %>%
        tab_spanner(
          label = gt::html("Period"),
          columns = c(date, days)
        ) %>%
        tab_spanner(
          label = gt::html("Log price"),
          columns = c(return, cumreturn)
        ) %>%
        tab_spanner(
          label = gt::html("Fraction"),
          columns = c(newhouse, usedhouse, newflat, usedflat)
        ) %>%
        tab_spanner(
          label = gt::html("Count"),
          columns = c(total, perday)
        ) %>%
        tab_spanner(
          label = gt::html("Sales Breakdown"),
          columns = c(newhouse, usedhouse, newflat, usedflat, total, perday)
        ) %>%
        tab_options(
          heading.align = "left",
          heading.title.font.size = 12
        )
      x211G <<- copy(x)
      x
    }

  x211D <- # ---listing-1-local-estdtl,qeoq,dfn----
    eventReactive(
      list(
        common$estdtlR(),
        common$geoqR(),
        common$dfnxxR()
      ),
      {
        if (verbose) print("enter x211D")
        x <- f211D(
          estdtlX = common$estdtlR(),
          geoqX = common$geoqR(),
          dfnxxX = common$dfnxxR()
        )
        x211G <<- copy(x)
        x
      }
    )
  x211cuD <- # listing-2-custom-estdtcu,qeocu,dfn----
    eventReactive(
      list(
        common$estdtccR(),
        common$geoccR(),
        common$dfnxxR()
      ),
      {
        if (verbose) print("enter x211D")
        geox <- copy(common$geoccR())[, let(rc6, rc9)] # used for aggregation and label
        x <- f211D(
          estdtlX = common$estdtccR(),
          geoqX = common$geoccR()[, .(nx, lab, rc6 = rc9)], # non-standard geo
          dfnxxX = common$dfnxxR(),
          typeX = "C"
        )
        x211ccG <<- copy(x)
        x
      }
    )
  output$x211 <- gt::render_gt(x211D()) #-----.#----
  output$x211cu <- gt::render_gt(x211cuD()) #--.#----
}
