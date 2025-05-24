getgd('f250519ad')
pxosrdo2dd <- getlast('pxosrdo2dd')

f250517a <- #leaflet 1/2/3 colour + 'specials'=peers+
  function(
    rc6x='SW-8--', #target
    #nn='f250519ad',
    # x0=dataG$f250509ed,  #SOT
    x1=dataG$pxosrdo2dd,
    x2=dataG$z110,
    x3=f250519ad,
    rc6all=x2[nchar(rcx)==6,][grep(rc3x,rcx)][rcx!=rc6x][1,c(rcx,rc6x)], #peers to highlight
    rc3x=substr(rc6x,1,3), #target area
    #lightx=.7,
    minzoom = 9
  ){
    # x6 <-  #nbin(rc6x)
    #     x0$geo[rc9%in%rc6x]%>%
    #   .[grep('^L',lab)]%>%
    #   .[x0$kfoldsse,on=c(nx='nx'),nomatch=NULL]%>% #local only
    #   .[rc6==rc6x]%>% #target rc6
    #   .[order(ssek),.(rc6,ssek,n,nx,lab)]%>% #first row, lowest ssek
    #   .[1,as.numeric(substr(lab,7,7))]
    # x7 <- #geo for nbin=x6, rc3=rc3x
    #   x0$geo%>%
    #   .[grep(paste0('L.....',x6,'BA'),lab)]%>%
    #   .[grep(rc3x,rc9)]%>% #all for this rc3
    #   .[,.(
    #     ibin=as.numeric(substr(lab,5,5)), #rank
    #     nbin=as.numeric(substr(lab,7,7)), #out of
    #     rc6=rc9,
    #     lab,
    #     nx)]
    # x8 <- #palette
    #   data.table(
    #     col=lighten(cobalt()[c(4, 2, 1)],lightx),
    #     icol=1:3)
    # x8a <- data.table(code=c('1.1','1.2','2.2','1.3','2.3','3.3'),frac=c(.5,.25,.75,1/6,.5,5/6),col=cobalt(light=T)[c('green','onch','blue','punk','green','midnight')])[order(frac)]
    # x9 <- #palette(nbin=x6)
    #   rbind(
    #     x8[icol==2,.(col,ibin=1,nbin=1,cc='green')], #one -> green
    #     x8[icol%in%c(1,3),.(col,ibin=1:2,nbin=2,cc=c('red','blue'))], #two -> red,blue
    #     x8[,.(col,ibin=1:3,nbin=3,cc=c('red','green','blue'))] #three -> red,green,blue
    #   )
    # x10 <-
    #   x9%>% #palette
    #   .[nbin==x6]%>%
    #   .[x7,on=c(ibin='ibin',nbin='nbin')] #rc3x
    # x10a <- x8a[x10[,code:=substr(lab,5,7)],on=c(code='code')][,.(rc6,col)]
    
    x10b <- x3[grep(substr(rc6x,1,3),rc6),.(col,rc6)]
    #browser()
    x11 <- 
      f240810b( #->leaflet, colours for areas-to-shade in column 'col'
      x10b[,.(col,rc6)],       
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
f250517a(rc6x='SW-16-',x1=pxosrdo2dd,x2=z110)
f250517a(rc6x='W--1U-')
f250517a(rc6x='SW-7--')
#for one rc3
#rc6,bincode,binperc,colour,prc6,pbinlo,pbinhi

    kfx=f250509ed  #index
    spd=pxosrdo2dd #polygons
    pva=z110       #pv
    

    x1 <-
      kfx$geo%>%
      .[grep('L......BA',lab)]%>%
      .[kfx$kfoldsse,on=c(nx='nx',rc9='rc6'),nomatch=NULL]%>% #local only
      #.[,rc3:=substr(rc9,1,3)]
      #.[rc6==rc6x]%>% #target rc6
      .[order(rc9,ssek),.(rc9,ssek,n,nx,lab)]%>% #first row, lowest ssek
      .[,.SD[1,.(i=as.numeric(substr(lab,5,5)),n=as.numeric(substr(lab,7,7)),lab,nx)],rc9]
    x1[]
    
    x2 <- 
      kfx$kfoldsse%>% #
      .[kfx$geo,on=c(nx='nx',rc6='rc9')]%>%
      .[grep('L......BA',lab)]%>%
      .[,.(rc6,rc3=substr(rc6,1,3),ssek,lab)]%>%
      .[,.SD[order(ssek)][1,],rc6]%>%
      #.[rc3==rc3[1]]%>%
      .[,.(rc3,rc6,ssek,lab,i=substr(lab,5,5),n=substr(lab,7,7))]
    x1[x2,on=c(rc9='rc6')][(i!=i.i)|(n!=i.n)]

# f250519aFun <- # best local tile code
#   function(
#       nn = "f250509ed",
#       kfx = f250509ed, # index
#       pva = z110) {
#     getgd(nn)
#     x1 <-
#       kfx$kfoldsse %>% #
#       .[kfx$geo, on = c(nx = "nx", rc6 = "rc9")] %>%
#       .[grep("L......BA", lab)] %>%
#       .[, .(rc6, rmsek = sqrt(ssek / n), lab)] %>%
#       .[, .SD[order(rmsek)][1, ], rc6] %>%
#       .[, .(rc6, rmsek, lab, i = substr(lab, 5, 5), n = substr(lab, 7, 7))]
#     x2 <-
#       kfx$geo %>%
#       .[grep("L......BA", lab)] %>%
#       pva[., on = c(rcx = "rc9")] %>%
#       .[, .(plo = min(ppm2), pag = sum(pv) / sum(m2), phi = max(ppm2)), .(lab)] %>%
#       .[order(pag), .(lab, plo, pag, phi, code = substr(lab, 5, 7))]
#     x3 <-
#       data.table(
#         code = c("1.1", "1.2", "2.2", "1.3", "2.3", "3.3"),
#         frac = c(.5, .25, .75, 1 / 6, .5, 5 / 6),
#         col = cobalt(light = T)[c("green", "onch", "blue", "punk", "green", "midnight")]
#       )[order(frac)]
#     x4 <-
#       x1[x2, on = c(lab = "lab"), nomatch = NULL] %>%
#       x3[., on = c(code = "code")] %>%
#       .[pva[, .(rcx, ppm2)], on = c(rc6 = "rcx"), nomatch = NULL]
#     f250519ad <<- x4
#     putt(f250519ad)
#   }
f250519aFun()    
f250519ad[,plot(rmsek,ppm2)]
f250519ad[rc6=='NG-7--']
f250519ad[,qrmse:=rank(rmsek)/.N][order(qrmse)]
tail(f250519ad[order(rmsek)],20)
f250519ad[grep('M--',rc6)]#[order(frac),mean(ppm2),frac][,plot(frac,V1)]
    

    x4 <- 
      x3[x2,on=c(code='code')]
    x4[rc3=='SW-']
    
x1 <- kfx$geo[grep('L......BA',lab)]
x2 <

,.(rc6=rc9,lab,nx,bincode=substr(lab,))]
pva[,.(plo,phi,pag)]
