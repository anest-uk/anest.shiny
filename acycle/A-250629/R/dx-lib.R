
D111x <- # leaflet ----
  function(nn = "res",
           rc6x = rc6tG, # target               C
           rc6ccx = rc6ccG, # custom            C
           x1 = resS$pol, # dataG$resS$pol  G
           x2 = z110, # dataG$z110              G
           rc6all = c(rc6x, rc6ccx),
           rc3x = substr(rc6x, 1, 3), # target area
           minzoom = 9,
           maxzoom = 12) {
    x4 <-
      f240810b( #->leaflet, colours for areas-to-shade in column 'col'
        resS$f250618c[grep(rc3x, rc6), .(col, rc6)],
        x2 = x1, # map polygons
        pva = resS$pva[, .(rcx = rc6, ppm2 = pv / m2)], # for tooltip
        minzoom = minzoom,
        maxzoom = maxzoom
      ) %>%
      addPolygons( # outline custom districts
        data = x1[which(x1@data$name %in% irregpcode(rc6all)), ],
        fill = F,
        color = "black",
        weight = 1,
        opacity = 1
      ) %>%
      addPolygons( # outline custom districts
        data = x1[which(x1@data$name %in% irregpcode(rc6x)), ],
        fill = F,
        color = "black",
        weight = 1,
        opacity = 1
      )
    x4
  }
D112x <-
  function(
      rc6tx = rc6tG,
      x0 = C111d(cus=rsiccG),
      tslidex=tslideG,
      r = resS
      ) {
    x1 <- # extremal indices  -> {nx,col,lab}(rc3)
      r$f250618c %>%
      .[grep(substr(rc6tG, 1, 3), rc6)] %>%
      .[order(Pnx)] %>%
      .[c(1, .N), ] %>%
      .[r$f250618b, on = c(rc6 = "rc6"), nomatch = NULL] %>%
      .[r$lab, on = c(nx = "nx"), nomatch = NULL] %>%
      .[, .(Pnx, col, nx, lab = as.factor(lab))]
    x2 <- # static + custom
      rbind(
        aestdt1(x0)[, lab := "custom"][],
        aestdt1(list(rsi=r$rsi[x1[, .(nx)], on = c(nx = "nx")]))[r$lab, on = c(nx = "nx"), nomatch = NULL]
      ) %>%
      .[, col := as.factor(lab)] %>%
      #.[, lab := ifelse(ii == max(ii)-10, lab, "")]%>%.[]%>%
      #.[, lab := ifelse(ii == which.max(x), lab, ""),col]%>%.[]%>%
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

# D121x <- # winding ----
#   function(rc6t = rc6tG,
#            x1 = C111d()$rsi %>%
#              aestdt1(.) %>%
#              C121c(x4 = .),
#            typex = c(A = "All", L = "Local", N = "National", C = "Custom")["C"],
#            tbinx = c(L = "Low Frequency", H = "High Frequency", A = "Annual")["H"]) {
#     for (i in 2:length(x1)) x1[[i]] <- ifelse(is.na(x1[[i]]), "", as.character(round(x1[[i]], 3)))
#     x2 <- gt::gt(x1) %>%
#       gt::tab_footnote(
#         footnote = typex
#       ) %>%
#       gt::tab_footnote(
#         footnote = tbinx
#       )
#     x2
#   }
D121x <- # winding ----
  function(
    rc6t = rc6tG, #fed reactive
    cus = G111dx #fed reactive
           ){
    typex = c('Custom','Local')
    tbinx = rep("High Frequency",2)
    x1 <- list(
    CUS=cus %>%
      aestdt1(.) %>%
      C121c(x4 = .)
    ,
    LOC=resS$rsi %>%
      .[resS$f250618b[rc6t == rc6, .(nx)], on = c(nx = "nx")] %>%
      list(rsi=.)%>%
      aestdt1(.) %>%
      C121c(x4 = .)
    )
    j <- 1
    x3 <- as.list(1:2)
    for(j in 1:2) {
      x2 <- x1[[j]]
      for (i in 2:length(x2)) x2[[i]] <- ifelse(is.na(x2[[i]]), "", as.character(round(x2[[i]], 3)))
    x3[[j]] <- gt::gt(x2) %>%
      gt::tab_footnote(
        footnote = typex[j]
      ) %>%
      gt::tab_footnote(
        footnote = tbinx[j]
      )
    }
   x3
  }

if (F) { #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<working here
  # custom computed using standard
  # use accessor akss for
  akss(f250509ed$kfoldsse) # this is fine for result, want the same for custom
  rssccG # oldstyle
  rssccxG # newstyle

  # common::rssccR is just the kss part
  # it consumes common::rsiccR or rsiccG which is raw output from f241119a
  # gen2 reformatter of f241119a output is akss(x = f250509ed$kfoldsse)
  # this needs
  # a) nx selected for the three local indices
  # b) columns rearranged/renamed/combined
  # the local indices
  x1 <- C112a()[rc3 == substr(rc6tG, 1, 3), .(lab, nx)] %>%
    unique(.) %>%
    .[order(nx), .(lab, nx)] # a)
  x2 <- akss(x = f250509ed$kfoldsse)
  x3 <- # this is good.  n looks odd - not balanced?  but right idea
    x2[[1]][x1, on = c(nx = "nx")][, .(n = sum(n), ssei = sum(ssei), ssek = sum(ssek), sser = sum(sser), sstr = sum(sstr), toti = sum(toti)), .(lab, nx)]
  # remember: this is for lab i.e. local tiles - do we report anything pan-rc3
  x2[[2]][nx %in% x3[, nx]] # this is the same info but does not have lab - maybe we just add lab to this step calc?

  # D122x consumes kss objects: custom, area
  akss() # keyed on rc6
  akss
}

D122x <- #----122 characteristics----
  function(rc6tx = rc6tG, # scalar: target
           rssax = rssaG, # rss { itrim lab n nx qtile rc3 rc6 ssei ssek sser sstr tbin type } : for area
           rssccx = rssccG, # rss { itrim lab n nx qtile rc3 rc6 ssei ssek sser sstr tbin type } : for custom geo
           z110x = z110) {
    rsscux <- copy(rssccx)[, lab := "CU000"] # R()
    x0 <- C122(rssx = rsscux, z110x = z110x)
    x1 <- C122(rssx = rssax, z110x = z110x)
    # browser()

    x <-
      data.table(coltab[, -"dark"])[, row := 1:.N][]
    x2 <-
      cbind(x1, x[c(1, 3, 6), ]) %>%
      .[, uu := "\u2589"]


    # x2 <-
    #   rbind(x1, x0)[order(-pnum)][, -"pnum"]
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
    # G122 <<- copy(x)
    x4
  } # global {rcx ppm2} : pva
