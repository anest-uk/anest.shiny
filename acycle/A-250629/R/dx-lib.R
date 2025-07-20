#-------------------------------------------------dx-lib disp gen2

#-------------------------------------------------41xx page=1 timeseries----
DD4212 <- #  leaflet : R4111x ----
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
      f240810b( #->leaflet, colours for areas-to-shade in column 'col'; depends global pxosrdo2dd
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

DD4222 <- #-------------- timeseries : R4112x ----
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

DD4221 <- #----------------- winding : R4121x ----
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
          CC4221( # wind table
            rcx = rc6tx,
            x4 = .
          ),
        areso(rc6tx, resS) %>% # loc opt
          aestdt1(.) %>%
          CC4221( # wind table
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

DD4222x <- #-------------------- char : R4122x ----
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
      CC4222a(
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
      .[, .(q1 = paste0("(",i, " of ", n,")"), q2, col, nrc6.est, nrc6.fit, nid, pmin, pmax, agg, key)] %>%
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


DD4211 <- #-------------------- summ : R4131x ----
  function(
      static = "resS", # statics                  S static
      rescxx = rescxG, #                          R reactive gen2
      rc6tx = rc6tG, #                            C control
      tslidex = tslideG #                         C
      ) {
    roundx=-2
    x1 <-
      CC4211(
        static = "resS",
        tslidex = tslidex,
        rc6tx = rc6tx,
        rescxx = rescxx
      ) %>%
      .[order(-aggppm2), .(q2, q1 = paste0('(',gsub("\\.", " of ", i.n),')'), col, minppm2=round(minppm2,roundx), maxppm2=round(maxppm2,roundx), aggppm2=round(aggppm2,roundx), meanan,minan,maxan, key = "\u2589")] %>%
      .[q2 == "custom", q1 := ""]
    x2 <- paste0('annual log returns : ',as.integer(substr(aestdt2(resS)$BA[c(tslidex + 1)],1,4))+1,' - ',as.integer(substr(max(aestdt2(resS)$BA),1,4))-1)
    x1 %>%
      gt::gt(.) %>%
      cols_label(
        q2 = gt::html(""),
        q1 = gt::html(""),
        col = gt::html(""),
        minppm2 = gt::html("min"),
        maxppm2 = gt::html("max"),
        aggppm2 = gt::html("aggregate"),
        meanan = gt::html("mean"),
        minan = gt::html("min"),
        maxan = gt::html("max"),
        key = gt::html(""),
      ) %>%
      fmt_number(
        columns = c(aggppm2,minppm2,maxppm2),
        decimals = 0,
        sep_mark = "," # Thousands separator
      )%>%
      fmt_number(
        columns = c(minan,meanan,maxan),
        decimals = 3
      ) %>%
      tab_spanner(
        label = gt::html(x2),
        columns = c(minan, meanan, maxan, )
      ) %>%
      tab_spanner(
        label = gt::html("quantiles"),
        columns = c(q1, q2)
      ) %>%
      tab_spanner(
        label = gt::html("district £/m<sup>2</sup>"),
        columns = c(minppm2,aggppm2,maxppm2)
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
      cols_move_to_start( columns = c("q2"))%>%
      cols_move_to_start( columns = c("key"))
  }

DD4231 <- #------------------- trade : R4132x ----
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
      CC4212(
        geo = .,
        tmin = tslidex
      ) %>%
      setNames(., c("return", "count"))
    x1[[2]] <-
      ageo(rescxx) %>%
      CC4212(
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

#-------------------------------------------------42xx page=2 listing----
D4211a <- #summary utility called in all 3 listings ----
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
    c211x=C4211a(
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


D4211b <- #----------------3 listings: R4211a ----
  function(
    statics='resS',
    rescx=rescxG,
    rc6tx=rc6tG,
    tslidex=0
    ) {
  list(
    date= #shared data
      D4211a(
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
    local= #local index
      D4211a(res=areso(rc6tx),
        tslidex=tslidex),
    custom= #custom index
      D4211a(res=rescx,typex='C',
        tslidex=tslidex)
  )
}

DD4111 <- #--------blobs for rc3/rc6 : R4311x ----
  function(
      rc6tx = rc6tG,
      rc6cx = rc6cG,
      x1 = CC4111(rc6 = rc6tx), # it returns entire rc3  <<<<replace this with a call to generalised (C4311b) which exposes geo,and adapt the idiom:
      cols_to_paint = names(x1)[5:8],
      shadecol1 = "#D3D3D3", # matches grid
      shadecolblock = "#D3D3D3", ## EEEEFF nice pale blue
      symbolsize = ".8em") {
    x1[, blobsuppress := (substr(rc6, 1, 3) != substr(rc6tx, 1, 3))] # only show for this rc3
    x1 %>%
      gt::gt() %>%
      gt::text_transform(
        locations = gt::cells_body(columns = "q0"),
        fn = function(hexvec) {
          vapply(seq_along(hexvec), function(i) {
            val <- hexvec[i]
            row_rc6 <- x1$rc6[i]
            outline <- if (row_rc6 %in% rc6cx) "border:4px solid black;" else ""
            paste0(
              "<div style='display:inline-block; width:", symbolsize,
              "; height:", symbolsize,
              "; background-color:", val,
              "; border-radius:50%; ", outline, "'></div>"
            )
          }, character(1))
        }
      ) %>%
      cols_label(
        rc6 = gt::html("postcode"),
        locality = gt::html("name"),
        ppm2 = gt::html("£/m<sup>2</sup>"),
        nid = gt::html("properties"),
        q0 = gt::html(""),
        q3 = gt::html("3"),
        q2 = gt::html("2"),
        q1 = gt::html("1")
      ) %>%
      tab_spanner(
        label = gt::html("district"),
        columns = c(rc6, locality, ppm2, nid)
      ) %>%
      tab_spanner(
        label = gt::html("number of £/m<sup>2</sup> bins"),
        columns = c(q3, q2, q1)
      ) %>%
      cols_move_to_start(
        columns = c("q0")
      ) %>%
      cols_move_to_start(
        columns = c("locality")
      ) %>%
      cols_move_to_start(
        columns = c("rc6")
      ) %>%
      cols_move_to_start(
        columns = c("nid")
      ) %>%
      cols_move_to_start(
        columns = c("ppm2")
      ) %>%
      fmt_number(
        columns = c(ppm2, nid),
        decimals = 0,
        sep_mark = "," # Thousands separator
      ) %>%
      gt::text_transform(
        locations = gt::cells_body(columns = setdiff(cols_to_paint, "q0")),
        fn = function(hexvec) {
          vapply(seq_along(hexvec), function(i) {
            if (x1$blobsuppress[i] == T) {
              "" # suppress blob
            } else {
              val <- hexvec[i]
              paste0(
                "<div style='display:inline-block; width:", symbolsize,
                "; height:", symbolsize,
                "; background-color:", val,
                "; border-radius:50%;'></div>"
              )
            }
          }, character(1))
        }
      ) %>%
      cols_hide(columns = blobsuppress) %>%
      #-------------------------------------------
      tab_style(
        style = cell_text(align = "center"),
        locations = cells_body(columns = c(q1, q2, q3))
      ) %>%
      tab_style(
        style = cell_text(align = "center"),
        locations = cells_column_labels(columns = c(q1, q2, q3))
      ) %>%
      # ---- Optional row highlighting (unchanged)
      gt::tab_style(
        style = gt::cell_fill(color = shadecol1),
        locations = gt::cells_body(
          rows = x1[, rc6 %in% ageo(areso(rc6tx))[, rc6]],
          columns = paste0("q", areso(rc6tx)$lab[, substr(lab, 7, 7)])
        )
      ) %>%
      gt::tab_style(
        style = gt::cell_fill(color = shadecol1),
        locations = gt::cells_body(rows = rc6 == rc6tx)
      )
  }


DD4131 <- #------------------blobs for custom ----
  function(
      rc6tx = rc6tG,
      rc6cx = rc6cG,
      x1 = CC4131(rc6 = rc6tx,rc6cx=rc6cx), # it returns entire rc3  <<<<replace this with a call to generalised (C4311b) which exposes geo,and adapt the idiom:
      cols_to_paint = 'q0',
       shadecol1 = "#D3D3D3", # matches grid
      symbolsize = ".8em") {
    x1[, blobsuppress := F] # only show for this rc3
    x1 %>%
      .[order(-ppm2)]%>%
      gt::gt() %>%
      # ---- q0 with conditional outline ----
      gt::text_transform(
        locations = gt::cells_body(columns = "q0"),
        fn = function(hexvec) {
          vapply(seq_along(hexvec), function(i) {
            val <- hexvec[i]
            row_rc6 <- x1$rc6[i]
            outline <- "" #if (row_rc6 %in% rc6cx) "border:4px solid black;" else ""
            paste0(
              "<div style='display:inline-block; width:", symbolsize,
              "; height:", symbolsize,
              "; background-color:", val,
              "; border-radius:50%; ", outline, "'></div>"
            )
          }, character(1))
        }
      ) %>%
      cols_label(
        rc6 = gt::html("postcode"),
        locality = gt::html("name"),
        ppm2 = gt::html("£/m<sup>2</sup>"),
        nid = gt::html("properties"),
        q0 = gt::html("")
      ) %>%
      tab_spanner(
        label = gt::html("custom"),
        columns = c(rc6, locality, ppm2, nid)
      ) %>%
      cols_move_to_start(
        columns = c("q0")
      ) %>%
      cols_move_to_start(
        columns = c("locality")
      ) %>%
      cols_move_to_start(
        columns = c("rc6")
      ) %>%
      cols_move_to_start(
        columns = c("nid")
      ) %>%
      cols_move_to_start(
        columns = c("ppm2")
      ) %>%
      fmt_number(
        columns = c(ppm2, nid),
        decimals = 0,
        sep_mark = "," # Thousands separator
      ) %>%
      cols_hide(columns = blobsuppress) %>%
      gt::tab_style(
        style = gt::cell_fill(color = shadecol1),
        locations = gt::cells_body(rows = rc6 == rc6tx)
      )
  }

DD4112 <-  #--------leaflet for rc3  : R4311x ----
  function(
    rc6tx=rc6tG,
    rc6cx=rc6cG,
    x1=apol(datS), # rc6 polygon
    x2=apva(resS), # pva
    x3=CC4112(rc6tx=rc6tx,rc6cx=rc6cx), #rc6 for this rc3, augmented with out of area peers for this rc6t
    x4=pxosrdo1dd, # rc3 polygon
    minzoom=9,
    maxzoom=13
  ){
    x5 <- 
      x4[which(x4@data$name == irregpcode(substr(rc6tx,1,3))),]
    x6 <- 
      x1[which(x1@data$name == irregpcode(rc6tx)),]
    x7 <- 
      f240810b( #->leaflet, colours for areas-to-shade in column 'col'
        x3[,.(col=q0,rc6)],
        x2 = x1, # map polygons
        pva = resS$pva[, .(rcx = rc6, ppm2 = pv / m2)], # for tooltip
        minzoom = minzoom,
        maxzoom = maxzoom
      )%>%
      addPolygons( # outline custom districts
        data = x5,
        fill = F,
        color='black',
        dashArray = "5,5",
        weight = 1,
        opacity = 1
      )%>%
      addPolygons( # outline custom districts
        data = x6,
        fill = F,
        color='#888888',
        weight = 1,
        opacity = 1
      )
    x7
  }

DD4121 <-  #-'identifies as' message : R4321x ----
  function(
    statics = "resS",
    rc6tx = rc6tG) {
  x1 <-
    data.table(
      i.n = c("1.1", "2.2", "1.2", "3.3", "2.3", "1.3"), 
      meaning = c("all", "upper half", "lower half", "top tertile", "middle tertile", "bottom tertile"))
  x2 <- areso(rcxtx = rc6tx)$f250618b[1, .(i.n)]
  paste0(
    "target district ", 
    rc6tx, 
    " identifies as **", 
    x1[x2, on = c(i.n = "i.n")][, meaning], 
    " price bin** (", 
    substr(x2[, i.n], 1, 1), 
    " out of ", 
    substr(x2[, i.n], 3, 3), 
    ") based on minimisation of out of sample error"
    )
}
