
D111x <- # leaflet ----
  function(rc6x = rc6tG, # target
           rc6ccx = rc6ccG, # custom
           x1 = pxosrdo2dd, # dataG$pxosrdo2dd
           x2 = z110, # dataG$z110
           #x3 = f250519ad,
           x3=C111c(),
           rc6all = c(rc6x, rc6ccx),
           rc3x = substr(rc6x, 1, 3), # target area
           minzoom = 9,
           maxzoom = 12) {
    x4 <-
      f240810b( #->leaflet, colours for areas-to-shade in column 'col'
        x3[grep(rc3x, rc6), .(col, rc6)],
        x2 = x1, # map polygons
        pva = pva, # for tooltip
        minzoom = minzoom,
        maxzoom = maxzoom
      ) %>%
      addPolygons( # outline custom districts
        data = x1[which(x1@data$name %in% irregpcode(rc6all)), ],
        fill = F,
        color = "orange",
        weight = 1,
        opacity = 1
      ) %>%
      addPolygons( # outline custom districts
        data = x1[which(x1@data$name %in% irregpcode(rc6x)), ],
        fill = F,
        color = "brown",
        weight = 1,
        opacity = 1
      )
    x4
  }


D112x <- # x(t) ----
  function(
      rc6tx = rc6tG,
      tslidex = tslideG,
      x0 = f250509ed,
      x1 = estdtccG # cus
      ) {
    x2 <- C112d(
      x1 = x1,
      rc6tx = rc6tx, # rc6t
      #x0 = x0, # kfx
      x2 = C112c(rc6tx = rc6tx)
    ) %>%
    .[, .SD[, .(ii, date, lab, x = x - ifelse(tslidex == 0, 0, x[tslidex]))], .(legendlab, dark)] # rebase
    last_points <- x2[, .SD[.N], by = legendlab]
    ggplot(x2, aes(ii, x, color = dark)) +
      geom_line() +
      scale_color_identity() +
      geom_text_repel(
        data = last_points,
        aes(label = legendlab),
        nudge_x = 0.1,
        segment.color = "grey50"
      ) +
      geom_point(size = .3) +
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
      )
  }

D121x <- # winding ----
  function(rc6t = rc6tG,
           x1 = C121c(
             rc6tx = rc6tG,
             x0 = f250509ed,
             x1 = C121a(x0 = f250509ed$estdt)
           ),
           typex = c(A = "All", L = "Local", N = "National", C = "Custom")["C"],
           tbinx = c(L = "Low Frequency", H = "High Frequency", A = "Annual")["H"]) {
    for (i in 2:length(x1)) x1[[i]] <- ifelse(is.na(x1[[i]]), "", as.character(round(x1[[i]], 3)))
    x2 <- gt::gt(x1) %>%
      gt::tab_footnote(
        footnote = typex
      ) %>%
      gt::tab_footnote(
        footnote = tbinx
      )
    x2
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
