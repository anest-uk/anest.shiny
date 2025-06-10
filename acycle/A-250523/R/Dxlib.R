
D111x <-
function(
    #rc6x='SW-8--', #target
    rc6x=rc6tG, #target
    rc6ccx=rc6ccG, #custom
    x1=pxosrdo2dd, #dataG$pxosrdo2dd
    x2=z110, #dataG$z110
    x3=f250519ad,
    #rc6all=x2[nchar(rcx)==6,][grep(rc3x,rcx)][rcx!=rc6x][1,c(rcx,rc6x)], #peers to highlight
    rc6all=c(rc6x,rc6ccx),
    rc3x=substr(rc6x,1,3), #target area
    minzoom = 9,
    maxzoom=12
  ){
    x4 <- 
      f240810b( #->leaflet, colours for areas-to-shade in column 'col'
      x3[grep(rc3x,rc6),.(col,rc6)],       
      x2 = x1, #map polygons
      pva = x2, #for tooltip
      minzoom = minzoom,
      maxzoom = maxzoom
    ) %>%
      addPolygons(      #outline custom districts
        data = x1[which(x1@data$name %in% irregpcode(rc6all)), ],
        fill = F,
        color = "orange", 
        weight = 1,
        opacity = 1
      )%>%
      addPolygons(      #outline custom districts
        data = x1[which(x1@data$name %in% irregpcode(rc6x)), ],
        fill = F,
        color = "brown", 
        weight = 1,
        opacity = 1
      )
    x4
}

D112x <- function(
    rc6tx=rc6tG,
    tslidex=tslideG,
    x1=estdtccG #cus
) {
  x2 <- C112d(
    rc6t=rc6tx,
    x1=estdtccG
  )%>%
  .[, .SD[, .(ii, date, lab, x = x - ifelse(tslidex == 0, 0, x[tslidex]))], .(legendlab,dark)] #rebase
  last_points <- x2[, .SD[.N], by = legendlab]
  ggplot(x2,aes(ii,x,color=dark))+
    geom_line()+
    scale_color_identity()+
    geom_text_repel(
    data = last_points,
    aes(label = legendlab),
    nudge_x = 0.1,
    segment.color = "grey50"
  ) +
 geom_point(size = .3) +
    #geom_text_repel() +
    # ylim(ylimx - x3) +
    #labs(caption = geoccx[, paste0("Custom districts: ", paste0(sort(irregpcode(rc9)), collapse = ", "))]) +
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


 D121x <- #was f121g2
  function( #gen2
    x1 = f250509ed$estdt,
    x2 = labxG[1, lab], #replace this
    dfny = dfnxG,
    typex = typeC,
    tbinx = tbinC
) {
  x3 <- 
    D121( #-------------121 winding--------------wind
    estdtx = x1[lab == x2, .(ii, date, lab, qtile = substr(lab, 5, 7), x, xdotd)],
    dfny = dfny, # date {tbin1 tbin2 tbin3}
    typex = typex, #' L' always
    tbinx = tbinx, #' hi' always, tbinC=2 always
    dfnz = # vector: drc dates excluding date0
      dfny[, tbinx + 1, with = F] %>%
        setnames(., "x") %>%
        .[, sort(unique(x))],
    d2x = # vector: annual dates excluding date0
      dfny[, tbinx + 2, with = F] %>%
        setnames(., "x") %>%
        .[, sort(unique(x))]
  )
  x3
}

