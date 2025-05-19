f250517a <- #leaflet 1/2/3 colour + 'specials'=peers+
  function(
    rc6x='SW-8--', #target
    x0=dataG$f250509ed,  #SOT
    x1=dataG$pxosrdo2dd,
    x2=dataG$z110,
    rc6all=x2[nchar(rcx)==6,][grep(rc3x,rcx)][rcx!=rc6x][1,c(rcx,rc6x)], #peers to highlight
    rc3x=substr(rc6x,1,3), #target area
    lightx=.7,
    minzoom = 9
  ){
    x6 <-  #nbin(rc6x)
        x0$geo[rc9%in%rc6x]%>%
      .[grep('^L',lab)]%>%
      .[x0$kfoldsse,on=c(nx='nx'),nomatch=NULL]%>% #local only
      .[rc6==rc6x]%>% #target rc6
      .[order(ssek),.(rc6,ssek,n,nx,lab)]%>% #first row, lowest ssek
      .[1,as.numeric(substr(lab,7,7))]
    x7 <- #geo for nbin=x6, rc3=rc3x
      x0$geo%>%
      .[grep(paste0('L.....',x6,'BA'),lab)]%>%
      .[grep(rc3x,rc9)]%>% #all for this rc3
      .[,.(
        ibin=as.numeric(substr(lab,5,5)), #rank
        nbin=as.numeric(substr(lab,7,7)), #out of
        rc6=rc9,
        lab,
        nx)]
    x8 <- #palette
      data.table(
        col=lighten(cobalt()[c(4, 2, 1)],lightx),
        icol=1:3)
    x9 <- #palette(nbin=x6)
      rbind(
        x8[icol==2,.(col,ibin=1,nbin=1,cc='green')], #one -> green
        x8[icol%in%c(1,3),.(col,ibin=1:2,nbin=2,cc=c('red','blue'))], #two -> red,blue
        x8[,.(col,ibin=1:3,nbin=3,cc=c('red','green','blue'))] #three -> red,green,blue
      )
    x10 <- 
      x9%>% #palette
      .[nbin==x6]%>%
      .[x7,on=c(ibin='ibin',nbin='nbin')] #rc3x
    x11 <- 
      f240810b( #->leaflet, colours for areas-to-shade in column 'col'
      x10,       
      x2 = x1,
      pva = x2, #tooltip
      minzoom = minzoom,
      maxzoom = 12
    ) %>%
      addPolygons(      #outline custom districts
        data = x1[which(x1@data$name %in% irregpcode(rc6all)), ],
        fill = F,
        color = "orange",
        weight = 1,
        opacity = 1
      )
    x11
  }
