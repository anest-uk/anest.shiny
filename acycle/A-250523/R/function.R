#--------------functions

# utility----
sco <- function(x,namesonly=T){
  x1 <- setcolorder(x, order(names(x)))
  if(namesonly==T){
    x1 <- x1%>%names(.)%>%c('{',.,'}')%>%paste0(.,collapse=' ')
    
  }
  x1
}

# common----
festdtxX <- #------------rbind estdtcc,estdta----
  function(estdtccX = estdtccG, estdtaX = estdtaG, geoccX = geoccG) {
  x <-
    rbind(
      estdtccX[, .(nx, date, xdotd, days, xdot, x, lab, ii, qtile = 0, rc3 = lab)],
      estdtaX[, .(nx, date, xdotd, days, xdot, x, lab, ii, qtile, rc3)]
    )#[, qq := as.factor(qtile)]
  x
}

fgeoccX <- #------------custom geo compute----
  function(rc6ccX = rc6ccG) {
    x <-
      data.table(rc9 = rc6ccX, nx = 0, lab = "CU00")
    x
  }


# timeseries

f111D <- function( #-----------------------.#----
                  rc6tX = rc6tG,                  #scalar: target rc6 that implicitly defines rc3
                  rc6ccX = rc6ccG,                #vector: to outline
                  geoaX = geoaG,                  #geo {nx    gx    lab    rc3    rc6 qtile} : shade by qtile this-rc3-geo
                  pxosrdo2ddX = pxosrdo2ddG,      #global
                  z110X = z110G,                  #global {rcx ppm2} : pva
                  colX = colx,                    #global named vector : punk green blue 
                  minzoom = 9,                    #7 for national
                  lightx = .7                     #higher is lighter
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
      data = pxosrdo2ddX[which(pxosrdo2ddX@data$name %in% irregpcode(rc6ccX)), ],
      fill = F,
      color = "orange",
      weight = 1,
      opacity = 1
    )
  x
} #"leaflet"  "htmlwidget"

f112D <- function( #-----------------------.#----
                  tslideX = tslideG,              #integer: zero index
                  estdtxX = estdtxG,              #estdt: nx date xdotd days xdot x lab  ii qtile rc3 qq
                  ylimX = ylimG,                  #vector: ylim
                  geoccX = geoccG)                #geo {rc9 nx lab}: custom 
  {
  x2c <- estdtxX %>%
    copy(.)%>%
    .[, .SD[, .(ii, date, lab, x = x - ifelse(tslideX == 0, 0, x[tslideX]))], .(qtile)] %>%
    .[, .SD[, .(ii, date, lab, x)], .(qtile)] %>%
    .[, qq := as.factor(qtile)] %>%
    .[, labx := ifelse(date == max(date), lab, NA)]
  x0 <- setNames(cobalt()[c("punk", "green", "blue")], as.character(1:3))
  x3 <- estdtxX[, .SD[, .(ifelse(tslideX == 0, 0, x[tslideX]))], .(qtile)][, mean(V1)] # base value for rebase level
  x2 <-
    estdtxX%>%
    copy(.)%>%
    .[, .SD[, .(ii, date, lab, x = x - ifelse(tslideX == 0, 0, x[tslideX]))], .(qtile)] %>%
    .[, qq := as.factor(qtile)] %>%
    .[, labx := ifelse(date == max(date), lab, "")]
  x <- x2 %>%
    ggplot(., aes(date, x, color = qq, label = labx)) +
    geom_hline(yintercept = 0, linewidth = .4, linetype = "dotted", color = "grey40") +
    geom_line() +
    geom_point(size = .3) +
    geom_text_repel() +
    # ylim(ylimX - x3) +
    labs(caption = geoccX[, paste0("Custom districts: ", paste0(sort(irregpcode(rc9)), collapse = ", "))]) +
    xlab("") +
    ylab(bquote(Delta ~ P ~ log ~ price ~ change)) +
    theme_bw() +
    theme(
      axis.line = element_line(colour = "black"),
      panel.grid.major = element_line(linewidth = .2, linetype = "dotted", color = pgmc),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      text = element_text(size = 16, face = "plain"),
      axis.line.y.left = element_line(linewidth = .1),
      axis.line.x.bottom = element_line(linewidth = .1),
      legend.position = "none"
    ) +
    scale_color_manual(values = x0) +
    scale_x_date(
      breaks = as.Date(c("1995-01-01", "2000-01-01", "2010-01-01", "2020-01-01", "2024-01-01")),
      date_labels = "%Y",
      limits = c(as.Date(c("1994-12-31", "2027-12-31")))
    )
  x
} #"gg" "ggplot" 

f121D <- function( #-------------121 winding----
                  estdtX = estdtlG,               #estdt {date days ii lab nx qtile rc3 x xdot xdotd}
                  dfnxX = dfnxG,                  #date {tbin1 tbin2 tbin3}
                  drangeX = range(dfnxxX),
                  typeX = typeC,                  #'L' always
                  tbinX = tbinC,                  #'hi' always, tbinC=2 always
                  dfnxxX =                        #drc dates excluding date0
                    dfnxX[-1, tbinC + 1, with = F] %>% 
                    setnames(., "x") %>%
                    .[, sort(unique(x))], 
                  d2X =                           #annual dates excluding date0
                    dfnxX[-1, tbinC + 2, with = F] %>%
                    setnames(., "x") %>%
                    .[, sort(unique(x))]          
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

f122D <- function( # ----122 characteristics----
                  rc6tX = rc6tG,                  #scalar: target
                  rssaX = rssaG,                  #rss { itrim lab n nx qtile rc3 rc6 ssei ssek sser sstr tbin type } : for area
                  rssccX = rssccG,                #rss { itrim lab n nx qtile rc3 rc6 ssei ssek sser sstr tbin type } : for custom geo
                  z110X = z110G) {
  rsscux <- copy(rssccX)[, lab := "CU000"] # R()
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
}            #global {rcx ppm2} : pva

f131D <- function( #-------------131 summary----
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

f132 <- function( #-------------------------.#----
                 geox = geoqG,                    #estdt { gx lab nx qtile rc3 rc6 }
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

f132D <- function( # ---132 trade summary(2)----
                  tslideX = tslideG,
                  geoqX = geoqG,
                  geoccX = geoccG,
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
    geox = geoccX[, .(rc6 = rc9)], # geoqR()
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

# listing----
f211D <- #---summary called in both listings ----
  function(estdtlX = common$estdtlR(), # single
           geoqX = common$geoqR(), # footnote only 'this qtile'
           dfnxxX = common$dfnxxR(), # single
           typeX = typeC) {
    if (verbose) print("enter x211D")
    # fread("data/f241122ad.csv") %>%
    fpx <- file.path(data_dirG, "f241122ad.csv")
    print(paste0("filepath : ", fpx))
    ddd <- fread(fpx)[, sort(unique(date))]
    x1 <-
      fread(fpx) %>%
      .[geoqX[, .(rc6, lab)], on = c(rc6 = "rc6")] %>%
      .[, .(cum = sum(cum)), .(lab, nh, date)] %>%
      .[data.table(date = ddd[-1], i = 1:(length(ddd) - 1)), on = c(date = "date")] %>% # dfnG is all dates, all frequencies
      dcast(., date + i + lab ~ nh, value.var = "cum") %>% #
      .[order(date), .(date, t = i, lab, NF, NH, UF, UH)]

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

# constituent----
f311D <- function( #--'----311 constituents----
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

# accuracy----

f411D <- function( #-------411 accuracy tbin----
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


f412D <- function( #--------412 accuracy tbin----
                  geoccX = geoccG,
                  rc6tX = rc6tG,
                  rssccX = rssccG) {
  x1 <-
    data.table(tbin = 1:3, freq = c("lo", "hi", "an"))
  x2 <-
    rssccX %>% # use global no filters
    .[geoccX, on = c(rc6 = "rc9")] %>%
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


f421D <- function( #--------421 accuracy trim----
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

f422D <- function( # #--------422 accuracy trim----
                  geoccX = geoccG,
                  rc6tX = rc6tG,
                  rssccX = rssccG) {
  x1 <-
    data.table(itrim = 1:3, threshold = c("0.0", "0.1", "0.5"))
  x2 <-
    rssccX %>%
    .[geoccX, on = c(rc6 = "rc9")] %>%
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


f431D <- function( #------431 accuracy in/out----
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


f432D <- function( #------432 accuracy in/out----
                  geoccX = geoccG,
                  rc6tX = rc6tG,
                  rssccX = rssccG) {
  x1 <-
    rssccX %>%
    .[geoccX, on = c(rc6 = "rc9")] %>%
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
