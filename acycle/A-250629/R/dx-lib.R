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
      ) %>%
      gt::tab_footnote(
        footnote = tbinx[j]
      )
    }
   x3
  }
#D121x()


D122x <- #----122 characteristics----
  function(rc6tx = rc6tG, # scalar: target
           rssax = rssaG, # rss { itrim lab n nx qtile rc3 rc6 ssei ssek sser sstr tbin type } : for area
           rsscx = rsscG, # rss { itrim lab n nx qtile rc3 rc6 ssei ssek sser sstr tbin type } : for custom geo, rc6 and total ss
           pvax = apva(resS)) {
    rsscux <- copy(rsscx)[, lab := "CU000"] # R()
    x0 <- C122(rssx = rsscux, pvax = pvax)
    x1 <- C122(rssx = rssax, pvax = pvax)
    x <-
      data.table(coltab[, -"dark"])[, row := 1:.N][]
    x2 <-
      cbind(x1, x[c(1, 3, 6), ]) %>%
      .[, uu := "\u2589"]
    x3 <-
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
    x4 <-
      x3 %>%
      text_transform(
        locations = cells_body(columns = uu),
        fn = function(codes) {
          purrr::map2_chr(codes, x2$light, ~ paste0(
            "<span style='color:", .y, "; font-weight:bold;'>", .x, "</span>"
          )) %>%
            purrr::map(htmltools::HTML)
        }
      ) %>%
      cols_hide(columns = light) %>%
      cols_label(uu = "")
     x4
  } 
#D122x()
