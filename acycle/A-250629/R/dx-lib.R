D111x <- #OK leaflet ---- 
  function(
      statics = c("res", "dat"),# declare static dependencies
      rc6tx = rc6tG,   # target           C control
      rc6cx = rc6cG,   # custom           C
      x1 = apol(datS), # polygon          S static
      x2 = apva(resS), # pva              S
      rc6xx = c(rc6tx, rc6cx),# custom+target
      rc3tx = substr(rc6tx, 1, 3), # target rc3
      minzoom = 9,
      maxzoom = 12) {
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
#D111x()

D112x <-
  function(
      statics = c("res"),# declare static dependencies
      resx = resS, #                      S static
      rescx = rescG,   # custom           R reactive
      rc6tx = rc6tG,   # target           C control
      tslidex=tslideG,  # basedate        C control
      x0 = Ccus(rescx=rescx) # resC exposed calc
      ) {
    x1 <- # extremal indices  -> {nx,col,lab}(rc3)
      resx$f250618c %>%
      .[grep(substr(rc6tG, 1, 3), rc6)] %>%
      .[order(Pnx)] %>%
      .[c(1, .N), ] %>%
      .[resx$f250618b, on = c(rc6 = "rc6"), nomatch = NULL] %>%
      .[resx$lab, on = c(nx = "nx"), nomatch = NULL] %>%
      .[, .(Pnx, col, nx, lab = as.factor(lab))]
    x2 <- # static + custom
      rbind(
        aestdt1(x0)[, lab := "custom"][],
        aestdt1(list(rsi=resx$rsi[x1[, .(nx)], on = c(nx = "nx")]))[resx$lab, on = c(nx = "nx"), nomatch = NULL]
      ) %>%
      .[, col := as.factor(lab)] %>%
      .[, .SD[, .(
        ii, 
        date, 
        lab=ifelse(.I == which.max(x), lab, ""), 
        x = x - ifelse(tslidex == 0, 0, x[ii==tslidex]),
        xset
        )], 
        by=col]%>%
        .[,
          .SD[,.(
            ii,
            date,
            lab,
            x,
            xpoint=ifelse(.I == which.max(x), x, NA), 
            xset,
            xmin=ifelse(ii==max(ii),x-xset[tslidex+2],NA),
            xmax=ifelse(ii==max(ii),x+xset[tslidex+2],NA)
            )],
          by=col]
    ggplot(x2, aes(date, x, color = col, label = lab, ymin=xmin, ymax=xmax)) +
      geom_line() +
      geom_point(data=x2[!is.na(xpoint)],aes(date, xpoint, color = col))+
      geom_text_repel(size=5) +
      geom_errorbar()+
      xlab("") +
      ylab("index") +
      scale_color_manual(values = setNames(c(x1[, col], "black"), c(x1[, as.character(lab)], "custom"))) +
      theme_minimal() +
      theme(legend.position = "none")
  }
#D112x()

D121x <- # winding ----
  function(
    statics = c("res"),# declare static dependencies
    rc6tx = rc6tG,   #                C control
    res2x = res2G    # res2           R reactive
           ){
    typex = c('Custom','Local')
    tbinx = rep("High Frequency",2)
    x1 <- list(
    res2x=res2x %>%
      aestdt1(.) %>%
      C121c(x4 = .) #table constructor type=1
    ,
    ress=resS$rsi %>%
      .[resS$f250618b[rc6==rc6tx, .(nx)], on = c(nx = "nx")] %>%
      list(rsi=.)%>%
      aestdt1(.) %>%
      C121c(x4 = .) #table constructor type=2
    )
    x3 <- as.list(1:2)
    for(j in 1:2) { #custom, local
      x2 <- x1[[j]]
      for (i in 2:length(x2)) x2[[i]] <- ifelse(is.na(x2[[i]]), "", as.character(round(x2[[i]], 3)))
    x3[[j]] <- gt::gt(x2) %>% #gt constructor
      gt::tab_footnote(
        footnote = typex[j]
      ) #%>%
      # gt::tab_footnote(
      #   footnote = tbinx[j]
      # )
    }
   x3
  }
#D121x()


D122x <- function(
    rc6tx = rc6tG,
    geocx = geocG, # is a georc9
    pvax  = apva(resS),
    geox  = ageo(resS)[grep("^L", lab)] # is a georc6
    ) {
  x1 <- resS$f250618b # not a geo
  geo <- rbind( # is georc6
    geox,
    geocx[, .(rc6 = rc9, nx, lab = paste0("C", substr(rc6tG, 1, 3), "0.0CU"))]
  )
  geo1 <- # is georc6 : this rc3 geo
    geo %>% # 1
    .[grep(substr(rc6tx, 1, 3), lab)]
  x2 <- #count per i.n : train
    geo1 %>%
    .[, .(nrc6.est = .N), .(i.n = substr(lab, 5, 7))]
  x3 <- # {rc6 i.n nx} 
    x1[grep(substr(rc6tx, 1, 3), rc6)] %>%
    rbind(., geocx[, .(rc6 = rc6tx, i.n = "0.0", nx = 0)])
  x4 <- # count per i.n : fit
    x3 %>%
    .[, .(nrc6.fit = .N), i.n] # %>%
  x5 <- # 3,4,5,6,7
    geo1 %>%
    pvax[., on = c(rc6 = "rc6")] %>%
    .[, .(nid.est = sum(nid), maxppm2 = max(ppm2), minppm2 = min(ppm2), aggppm2 = sum(pv) / sum(m2)), .(nx, lab)] %>%
    .[order(aggppm2), .(lab, nid.est, minppm2, maxppm2, aggppm2, col = color_price(log(aggppm2), log(min(.[, minppm2])), log(max(.[, maxppm2]))), i.n = substr(lab, 5, 7))]
  x6 <-
    x2[x4, on = c(i.n = "i.n")][x5, on = c(i.n = "i.n")] %>%
    .[, .(i.n, col, nrc6.est, nrc6.fit)]
  x7 <- # 5,6,7
    pvax %>%
    .[geo1, on = c(rc6 = "rc6")] %>%
    .[, .(agg = sum(pv) / sum(m2), max = max(pv / m2), min = min(pv / m2), nid = sum(nid)), .(nx, i.n = substr(lab, 5, 7))] %>%
    .[order(agg)]
  x8 <-
    x6[x7, on = c(i.n = "i.n")][, .(i.n, col, nrc6.est, nrc6.fit, nid, min, max, agg)]
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
      key = "\u2589" # square symbol for color key
    )]

  x10 <-
    x9[, .(i=as.character(c(1,1,1,2,2,3,0)), n=as.character(c(3,2,1,3,2,3,0)), q2 = c("bottom tertile", "lower half", "all", "middle tertile", "upper half", "top tertile", "custom"))][x9, on = c(i = "i", n = "n")] %>%
    .[, .(q1 = paste0(i, " of ", n), q2, col, nrc6.est, nrc6.fit, nid, pmin, pmax, agg, key)] %>%
    .[order(-as.numeric(gsub(',','',agg)))]%>%
    .[q2%in%c('custom','all'), q1 := ""]

   x10%>%
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
      is_target <- (x10[i,q2=='custom'])  # <- change this condition as needed
      outline <- if (is_target) "border:4px solid black;" else ""
      paste0(
        "<div style='display:inline-block; width:1em; height:1em; background-color:", colval, "; ",
        outline, "'></div>"
      )
    }) %>%
      purrr::map(htmltools::HTML)
  }
)%>%
# 
#      
#      text_transform(
#   locations = cells_body(columns = key),
#   fn = function(codes) {
#     purrr::imap_chr(codes, function(code, i) {
#       colval <- x10[i, col]
#       extra_style <- if (i == 3) "border:1px solid black; padding:1px; display:inline-block;" else ""
#       paste0(
#         "<span style='color:", colval, "; font-weight:bold;", extra_style, "'>", code, "</span>"
#       )
#     }) %>%
#       purrr::map(htmltools::HTML)
#   }
#)%>%
    # text_transform(
    #   locations = cells_body(columns = key),
    #   fn = function(codes) {
    #     purrr::map2_chr(codes, x10[, col], ~ paste0(
    #       "<span style='color:", .y, "; font-weight:bold;'>", .x, "</span>"
    #     )) %>%
    #       purrr::map(htmltools::HTML)
    #   }
    # ) %>%
    cols_hide(columns = col) %>%
    cols_label(key = "") %>%
    cols_move_to_start(
      columns = c('key')
    )
}#D122x()
D122x()
