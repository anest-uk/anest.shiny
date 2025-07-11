D111x <- #  leaflet ----
  function(
      statics = c("resS", "datS"), #              S static
      rc6tx = rc6tG, # target                     C control 
      rc6cx = rc6cG, # custom                     C
      rc6xx = c(rc6tx, rc6cx), # custom+target    E exposed
      rc3tx = substr(rc6tx, 1, 3), # target rc3   E
      minzoom = 9, #                              P parameter
      maxzoom = 12 #                              P
      ) {
    x1 <- apol(datS) # polygon
    x2 <- apva(resS) # pva
    x4 <-
      f240810b( #->leaflet, colours for areas-to-shade in column 'col'
        resS$f250618c[grep(rc3tx, rc6), .(col, rc6)],
        x2 = x1, # map polygons
        pva = resS$pva[, .(rcx = rc6, ppm2 = pv / m2)], # for tooltip
        minzoom = minzoom,
        maxzoom = maxzoom
      ) %>%
      addPolygons( # outline custom districts
        data = x1[which(x1@data$name %in% irregpcode(rc6xx)), ],
        fill = F,
        color = "black",
        weight = 1,
        opacity = 1
      ) %>%
      addPolygons( # outline target district
        data = x1[which(x1@data$name %in% irregpcode(rc6tx)), ],
        fill = F,
        color = "black",
        weight = 1,
        opacity = 1
      )
    x4
  }
# D111x()

D112x <- # timeseries
  function(
      statics = c("resS"), # statics              S static
      rc6tx = rc6tG, #   target                   C control 
      rescxx = rescxG, #   custom                 R reactive gen2
      tslidex = tslideG #  basedate               C 
      ) {
    x1 <- # extremal indices  -> {nx,col,lab}(rc3)
      resS$f250618c %>%
      .[grep(substr(rc6tx, 1, 3), rc6)] %>%
      .[order(Pnx)] %>%
      .[c(1, .N), ] %>%
      .[resS$f250618b, on = c(rc6 = "rc6"), nomatch = NULL] %>%
      .[resS$lab, on = c(nx = "nx"), nomatch = NULL] %>%
      .[, .(Pnx, col, nx, lab = as.factor(lab))]
    x2 <- # static + custom
      rbind(
        aestdt1(rescxx)[, lab := "custom"][],
        aestdt1(list(rsi = resS$rsi[x1[, .(nx)], on = c(nx = "nx")]))[resS$lab, on = c(nx = "nx"), nomatch = NULL]
      ) %>%
      .[, col := as.factor(lab)] %>%
      .[, .SD[, .(
        ii,
        date,
        lab = ifelse(.I == which.max(x), ifelse(.BY[[1]] == "custom", "custom", ifelse(as.numeric(substr(lab, 5, 5)) == as.numeric(substr(lab, 7, 7)), "high-P", "low-P")), ""),
        x = x - ifelse(tslidex == 0, 0, x[ii == tslidex]),
        xset
      )],
      by = col
      ] %>%
      .[,
        .SD[, .(
          ii,
          date,
          lab,
          x,
          xpoint = ifelse(.I == which.max(x), x, NA),
          xset,
          xmin = ifelse(ii == max(ii), x - xset[tslidex + 2], NA),
          xmax = ifelse(ii == max(ii), x + xset[tslidex + 2], NA)
        )],
        by = col
      ]
    ggplot(x2, aes(date, x, color = col, label = lab, ymin = xmin, ymax = xmax)) +
      geom_line() +
      geom_point(data = x2[!is.na(xpoint)], aes(date, xpoint, color = col)) +
      geom_text_repel(size = 5, max.overlaps = Inf) +
      geom_errorbar() +
      geom_vline(xintercept = aestdt2(resS)$BA[tslidex + 1], color = "lightsteelblue") +
      geom_hline(yintercept = 0, color = "lightsteelblue") +
      xlab("") +
      ylab("index") +
      scale_color_manual(values = setNames(c(x1[, col], "black"), c(x1[, as.character(lab)], "custom"))) +
      theme_minimal() +
      theme(legend.position = "none")
  }
# D112x()

D121x <- # winding
  function(
      statics = c("resS"), # statics              S static
      rc6tx = rc6tG, #                            C control r
      rescx = rescxG # rescx                      R reactive gen2
      ) {
    typex <- c("Custom", "Local")
    x1 <-
      list(
        rescx %>% # cus
          aestdt1(.) %>%
          C121c( # wind table
            rcx = rc6tx,
            x4 = .
          ),
        areso(rc6tx, resS) %>% # loc opt
          aestdt1(.) %>%
          C121c( # wind table
            rcx = rc6tx,
            x4 = .
          )
      )
    x3 <- as.list(1:2)
    for (j in 1:2) { # custom, local
      x2 <- x1[[j]]
      for (i in 2:length(x2)) x2[[i]] <- ifelse(is.na(x2[[i]]), "", as.character(round(x2[[i]], 3)))
      x3[[j]] <- gt::gt(x2) %>% # gt constructor
        gt::tab_footnote(
          footnote = typex[j]
        )
    }
    x3
  }
# D121x()

D122x <- # char
  function(
      pvax = apva(resS), #                        S static
      rc6tx = rc6tG, #                            C control 
      rescxx = rescxG, #                          R reactive
      geocx = ageo(rescxG), # i                   E exposed
      geox = ageo(resS)[grep("^L", lab)] #        E georc6
      ) {
    geo1 <- rbind( #
      geox,
      geocx[, .(rc6, nx, lab = paste0("C", substr(rc6tx, 1, 3), "0.0CU"))]
    ) %>% # 1
      .[grep(substr(rc6tx, 1, 3), lab)]

    x6a <- #
      C122a(
        rc6tx = rc6tx,
        geocx = geocx, # is a georc6
        pvax  = pvax,
        geox  = geox # is a georc6
      )
    x6 <-
      x6a %>%
      .[, .(i.n, col, nrc6.est, nrc6.fit)]
    x6 <- x6a
    x7 <- # 5,6,7
      pvax %>%
      .[geo1, on = c(rc6 = "rc6")] %>%
      .[, .(agg = sum(pv) / sum(m2), max = max(pv / m2), min = min(pv / m2), nid = sum(nid)), .(nx, i.n = substr(lab, 5, 7))] %>%
      .[order(agg)]
    x8 <-
      x6[x7, on = c(i.n = "i.n")] # [, .(i.n, col, nrc6.est, nrc6.fit, nid, min, max, agg)]
    x9 <-
      x8[, .(
        i = substr(i.n, 1, 1),
        n = substr(i.n, 3, 3),
        col,
        nrc6.est,
        nrc6.fit,
        nid = prettyNum(nid, big.mark = ","),
        pmin = prettyNum(round(min), big.mark = ","),
        pmax = prettyNum(round(max), big.mark = ","),
        agg = prettyNum(round(agg), big.mark = ","),
        q2,
        key = "\u2589" # square symbol for color key
      )]
    x10 <-
      x9 %>%
      .[, .(q1 = paste0(i, " of ", n), q2, col, nrc6.est, nrc6.fit, nid, pmin, pmax, agg, key)] %>%
      .[order(-as.numeric(gsub(",", "", agg)))] %>%
      .[q2 %in% c("custom", "all"), q1 := ""]

    x10 %>%
      gt::gt(.) %>%
      cols_label(
        q1 = gt::html("rank"),
        q2 = gt::html("segment"),
        col = gt::html(""),
        nrc6.est = gt::html("train"),
        nrc6.fit = gt::html("fit"),
        nid = gt::html("properties"),
        pmin = gt::html("min"),
        pmax = gt::html("max"),
        agg = gt::html("aggregate"),
        key = gt::html(""),
      ) %>%
      tab_spanner(
        label = gt::html("£/m<sup>2</sup>"),
        columns = c(pmin, pmax, agg)
      ) %>%
      tab_spanner(
        label = gt::html("quantiles"),
        columns = c(q1, q2)
      ) %>%
      tab_spanner(
        label = gt::html("districts"),
        columns = c(nrc6.est, nrc6.fit)
      ) %>%
      text_transform(
        locations = cells_body(columns = key),
        fn = function(codes) {
          purrr::imap_chr(codes, function(code, i) {
            colval <- x10[i, col]
            is_target <- (x10[i, q2 == "custom"]) # <- change this condition as needed
            outline <- if (is_target) "border:4px solid black;" else ""
            paste0(
              "<div style='display:inline-block; width:1em; height:1em; background-color:", colval, "; ",
              outline, "'></div>"
            )
          }) %>%
            purrr::map(htmltools::HTML)
        }
      ) %>%
      cols_hide(columns = col) %>%
      cols_label(key = "") %>%
      cols_move_to_start(
        columns = c("key")
      )
  }
# D122x()

D131x <- # summ
  function(
      static = "resS", # statics                  S static
      rescxx = rescxG, #                          R reactive gen2
      rc6tx = rc6tG, #                            C control
      tslidex = tslideG #                         C
      ) {
    x1 <-
      C131x(
        static = "resS",
        tslidex = tslidex,
        rc6tx = rc6tx,
        rescxx = rescxx
      ) %>%
      .[order(-aggppm2), .(q1 = gsub("\\.", " of ", i.n), q2, col, agg = round(aggppm2, -2), min, mean, tot, pa, max, stdev, skew, kurtosis, key = "\u2589")] %>%
      .[q2 == "custom", q1 := ""]
    x1 %>%
      gt::gt(.) %>%
      cols_label(
        q1 = gt::html("rank"),
        q2 = gt::html("segment"),
        col = gt::html(""),
        min = gt::html("min"),
        mean = gt::html("mean"),
        tot = gt::html("total"),
        pa = gt::html("p.a."),
        max = gt::html("max"),
        stdev = gt::html("volatility"),
        skew = gt::html("skew"),
        kurtosis = gt::html("kurtosis"),
        agg = gt::html("£/m<sup>2</sup>"),
        key = gt::html(""),
      ) %>%
      fmt_number(
        columns = agg,
        decimals = 0,
        sep_mark = "," # Thousands separator
      ) %>%
      fmt_number(
        columns = c("skew", "kurtosis"),
        decimals = 2,
        sep_mark = "," # Thousands separator
      ) %>%
      tab_spanner(
        label = gt::html(aestdt2(resS)$BA %>% .[c(tslidex + 1, length(.))] %>% paste0(., collapse = " - ") %>% paste0("log returns : ", .)),
        columns = c(min, mean, tot, pa, max, stdev, skew, kurtosis)
      ) %>%
      tab_spanner(
        label = gt::html("quantiles"),
        columns = c(q1, q2)
      ) %>%
      text_transform(
        locations = cells_body(columns = key),
        fn = function(codes) {
          purrr::imap_chr(codes, function(code, i) {
            colval <- x1[i, col]
            is_target <- (x1[i, q2 == "custom"]) # <- change this condition as needed
            outline <- if (is_target) "border:4px solid black;" else ""
            paste0(
              "<div style='display:inline-block; width:1em; height:1em; background-color:", colval, "; ",
              outline, "'></div>"
            )
          }) %>%
            purrr::map(htmltools::HTML)
        }
      ) %>%
      cols_hide(columns = col) %>%
      cols_label(key = "") %>%
      cols_move_to_start(
        columns = c("key")
      )
  }
# D131x()

D132x <- # trade
  function(
      static = "resS", # statics                  S static
      rc6tx = rc6tG, #                            C control rc6tG rc6cG tslideG
      tslidex = tslideG, #                        C control rc6tG rc6cG tslideG
      rescxx = rescxG #                           R reactive gen2
      ) {
    x1 <- setNames(as.list(1:2), c("local", "custom"))
    x1[[1]] <-
      areso(rc6tx) %>% # opt loc
      ageo(.) %>% # geo
      C132a(
        geo = .,
        tmin = tslidex
      ) %>%
      setNames(., c("return", "count"))
    x1[[2]] <-
      ageo(rescxx) %>%
      C132a(
        geo = .,
        tmin = tslidex
      ) %>%
      setNames(., c("return", "count"))
    x2 <- copy(x1)
    x2[["local"]][[1]] <-
      x1[["local"]][[1]] %>%
      gt::gt(.) %>%
      tab_header(., title = "Local - Return") %>%
      opt_align_table_header(., align = "left") %>%
      tab_options(heading.title.font.size = 14) %>%
      tab_spanner(
        label = gt::html("sell"),
        columns = 2:ncol(x1[["local"]][[1]])
      )
    x2[["local"]][[2]] <-
      x1[["local"]][[2]] %>%
      gt::gt(.) %>%
      tab_header(., title = "Local - Number") %>%
      opt_align_table_header(., align = "left") %>%
      tab_options(heading.title.font.size = 14) %>%
      tab_spanner(
        label = gt::html("sell"),
        columns = 2:ncol(x1[["local"]][[2]])
      )
    x2[["custom"]][[1]] <-
      x1[["custom"]][[1]] %>%
      gt::gt(.) %>%
      tab_header(., title = "Custom - Return") %>%
      opt_align_table_header(., align = "left") %>%
      tab_options(heading.title.font.size = 14) %>%
      tab_spanner(
        label = gt::html("sell"),
        columns = 2:ncol(x1[["custom"]][[1]])
      )
    x2[["custom"]][[2]] <-
      x1[["custom"]][[2]] %>%
      gt::gt(.) %>%
      tab_header(., title = "Custom - Number") %>%
      opt_align_table_header(., align = "left") %>%
      tab_options(heading.title.font.size = 14) %>%
      tab_spanner(
        label = gt::html("sell"),
        columns = 2:ncol(x1[["custom"]][[2]])
      )
    x2
  }
# D132x()



D211x <- #---summary called in both listings ----
   function(
    statics=c('resS','salS'),
    estdtlx = estdtlG, #l=aestdt1(areso(rc6tx)) c=aestdt1(rescxG)
    geoqx = geoqG, #l=ageo(areso(rc6tx)) c=ageo(rescxG)
    dfnyx = dfnyG, #l=aestdt2(resS)%>%Reduce(c,.)%>%sort(.)%>%unique(.) c= same = aestdt2(resS)%>%Reduce(c,.)%>%sort(.)%>%unique(.)
    typex = typeC, #l='L' c='C' for footnote
    rc6tx = rc6tG,
    salx = salS
   ) {
    if (verbose) print("enter R211")
    x1 <- 
      salx %>%
      .[geoqx[, .(rc6, lab, nx)], on = c(rc6 = "rc6")] %>%
      .[,rbind(data.table(date=resS$da0,cum=0),.SD),.(nh,rc6,lab,nx)]%>%
      .[, .(cum = sum(cum)), .(nx, lab, nh, date)] %>%
      dcast(., date + nx + lab ~ nh, value.var = "cum") %>% #
      .[order(lab,date), .(date, NF, NH, UF, UH),.(lab,nx)]
    if(F) {x1[,.(tot=sum(NF+UF+UH+NH)),date][,.(days=as.integer(diff(date)),N=diff(tot),rate=diff(tot)/as.integer(diff(date)))]%>%.[,barplot(rate)]}
    x2 <-
      estdtlx %>%
      x1[., on = c(date = "date",nx='nx')] %>%
      .[, .(ii,
        date, days, xdot, x,
        NF = c(0, diff(NF)),
        NH = c(0, diff(NH)),
        UF = c(0, diff(UF)),
        UH = c(0, diff(UH)),
        tot = c(0, diff(NF + NH + UF + UH))
      ),nx] %>%
      .[-1, .(
        nx,
        ii,
        date,
        days,
        yrs=round(days/365.25,1),
        return = round(xdot, sf),
        cumreturn = round(x, sf),
        newhouse = round(NH / tot, sf),
        usedhouse = round(UH / tot, sf),
        newflat = round(NF / tot, sf),
        usedflat = round(UF / tot, sf),
        total = round(tot),
        perday = round(tot / days, 1)
      )
      ]
    x3 <- # districts footnote
      geoqx[
        , paste0("Districts: ", paste0(sort(irregpcode(rc6)), collapse = ", "))
      ]
    x <-
      gt::gt(x2) %>%
      gt::tab_footnote(
        footnote = f241108a(typex, tbinC)[[1]]
      ) %>%
      # gt::tab_footnote(
      #   footnote = f241108a(typex, tbinC)[[2]],
      #   locations = NULL,
      #   placement = c("auto", "right", "left")
      # ) %>%
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
    x
   }




D211a <- #---summary called in both listings ----
function(
    statics=c('resS','salS'),
    resx=aresn(resS,nx=resS$lab[grep(rc6tx,lab),nx]),
    estdtlx = aestdt1(resx), #l=aestdt1(areso(rc6tx)) c=aestdt1(rescxG)
    geoqx = ageo(resx),#geoqG, #l=ageo(areso(rc6tx)) c=ageo(rescxG)
    dfnyx = aestdt2(resx)$BA,#dfnyG, #l=aestdt2(resS)%>%Reduce(c,.)%>%sort(.)%>%unique(.) c= same = aestdt2(resS)%>%Reduce(c,.)%>%sort(.)%>%unique(.)
    typex = typeC, #l='L' c='C' for footnote
    rc6tx = rc6tG,
    salx = salS,
    jlist=list(
      date = "end date",
      return = "return",
      cumreturn = "cumulative",
      newhouse = "new house",
      usedhouse = "used house",
      newflat = "new flat",
      usedflat = "used flat",
      perday = "per day",
      total = "total"),
    c211x=C211a(
      statics=statics,
      estdtlx = estdtlx, #l=aestdt1(areso(rc6tx)) c=aestdt1(rescxG)
      geoqx = ageo(resx),#geoqx, #l=ageo(areso(rc6tx)) c=ageo(rescxG)
      salx = salx
    ),
    titlex=geoqx[, paste0("Districts: ", paste0(sort(irregpcode(rc6)), collapse = ", "))], #title
    footadd=T,
    headadd=T,
    addt0=F,
    tslidex=0
) {
  x2 <- c211x[date>aestdt2(resS)[[2]][tslidex+1]]
  x3 <- x2[,names(jlist),with=F]
  if(addt0) {
    #x2 <- rbind(x2[1,.(date=resS$da0,return=NA,cumreturn=NA,newhouse=NA,usedhouse=NA,newflat=NA,usedflat=NA,perday=NA,total=NA)],x2)
    x3 <- rbind(x3[1,.(ii=0,date=resS$da0)],x3,fill=T)
  }
  jlist2 <- jlist[which(names(jlist)%in%names(x3))]
  x <-
    gt::gt(x3) %>%
    cols_label(
      .list=jlist2,
      .fn=gt::html
    )
  if(all(c('return','cumreturn')%in%names(jlist))){
    x <- x%>%tab_spanner(
      label = gt::html("Log price"),
      columns = c(return, cumreturn)
    )
  }
  if(all(c('newhouse', 'usedhouse', 'newflat', 'usedflat')%in%names(jlist))){
    x <- x%>%
      tab_spanner(
        label = gt::html("Fraction"),
        columns = c(newhouse, usedhouse, newflat, usedflat)
      )
  }
  if(all(c('total', 'perday')%in%names(jlist))){
    x <- x%>%tab_spanner(
      label = gt::html("Count"),
      columns = c(total, perday)
    )
  }
  if(all(c('newhouse', 'usedhouse', 'newflat', 'usedflat', 'total', 'perday')%in%names(jlist))){
    x <- x%>%tab_spanner(
      label = gt::html("Sales Breakdown"),
      columns = c(newhouse, usedhouse, newflat, usedflat, total, perday)
    )
  }
  #print(jlist)
  if(all(c('date','days','yrs','ii')%in%names(jlist))) {
    print('end date, days spanner')
    x <-
      x%>%
      tab_spanner(
        label = gt::html("Period"),
        columns = c(ii, date, days, yrs)
      )%>%
      text_transform(
        locations = cells_body(columns = c(days, yrs), rows = (ii == 0)),
        fn = function(x) {
          rep("", length(x))
        }
      )

  }
  #   if(all(c('ii', 'date', 'days', 'yrs')%in%names(jlist))){
  #   x <- x%>%tab_spanner(
  #     label = gt::html("."),
  #     columns = c(ii, date, days, yrs)
  #   )
  # }

  if(footadd){
    x <- x%>%
      gt::tab_footnote(
      footnote = f241108a(typex, tbinC)[[1]]
    )
  }
  if(headadd){
    x <- x%>%
    gt::tab_header(
      title = titlex
    )
  }
  x
}


D211b <- function(
    statics='resS',
    rescx=rescxG,
    rc6tx=rc6tG,
    tslidex=0
    ) {
  list(
    date=
      D211a(
        res = areso(rc6tx),
        jlist = list(
          date = "end date",
          ii = "t",
          days = "days",
          yrs = "years"
        ),
        footadd=F,
        headadd=T,
        titlex=gt::html("&nbsp;"),
        addt0=T,
        tslidex=tslidex
      ),
    local=D211a(res=areso(rc6tx),
        tslidex=tslidex),
    custom=D211a(res=rescx,typex='C',
        tslidex=tslidex)
  )
}
