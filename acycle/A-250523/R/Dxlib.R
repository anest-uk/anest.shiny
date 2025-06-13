D111x <- #leaflet ----
  function(
      rc6x = rc6tG, # target
      rc6ccx = rc6ccG, # custom
      x1 = pxosrdo2dd, # dataG$pxosrdo2dd
      x2 = z110, # dataG$z110
      x3 = f250519ad,
      rc6all = c(rc6x, rc6ccx),
      rc3x = substr(rc6x, 1, 3), # target area
      minzoom = 9,
      maxzoom = 12) {
    x4 <-
      f240810b( #->leaflet, colours for areas-to-shade in column 'col'
        x3[grep(rc3x, rc6), .(col, rc6)],
        x2 = x1, # map polygons
        pva = x2, # for tooltip
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

D112x <- #x(t) ----
  function(
    rc6tx = rc6tG,
    tslidex = tslideG,
    x1 = estdtccG # cus
    ) {
  x2 <- C112d(
    rc6t = rc6tx,
    x1 = estdtccG
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

D121x <- #winding ----
  function(
    x1 = C121c(),
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

