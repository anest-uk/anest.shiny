
  rsi=list(z221,z321,z421)[[ipanel]] #---global 
  prj=list(z223,z323,z423)[[ipanel]] #---global
  bwe=list(z224,z324,z424)[[ipanel]] #---global
  pva=z110 #-----------------------------global

nn<-c('z221','z321','z421','z223','z323','z423','z224','z324','z424','z110')
save(list=nn,file='t4dump.Rdata')

# 
# f230703c<-
# function(  #NUTS2 names
#   ) {
#     nname <-
#       structure(list(
#         X1 = c('L','K','J','I','H','G','F','E','D','C'), 
#         X2 = 
#           c('Wales','South West','South East','London','East of England',
#             'West Midlands','East Midlands','Yorkshire and Humber', 'North West',
#             'North East'),
#         X3 = c('Wales','Southwest','Southeast','London', 
#                'East','W Midlands','E Midlands','Yorkshire', 
#                'Northwest','Northeast'),
#         nx=c(4,7,9,10,8,5,6,3,2,1)
#       ), class = 'data.frame', 
#       row.names = c(NA, 
#                     -10L))%>%
#       data.table(.)%>%setnames(.,c('code','name','abbrev','nx'))%>%
#       .[order(-nx)]
#     nname
# }
# 
# f231204a <- 
#   function(#generate table 4, 'geo comparison' combining P, RSI, LFM, CIRC
#   ipanel=3,  #note this function *breaks* the universal rule: it has global references xnnn which are not passed
#   cardinal=c('TS-','L--','S--','M--','LS-','B--','BS-','AL-','N--','WC-') 
#     ) {
#   rsi=list(z221,z321,z421)[[ipanel]] #---global 
#   prj=list(z223,z323,z423)[[ipanel]] #---global
#   bwe=list(z224,z324,z424)[[ipanel]] #---global
#   pva=z110 #-----------------------------global
#   stat=rsi$ses$stat
#   geo=rsi$geo
#   x0 <- #expand rc3 parts of geo into rc6
#     pva[nchar(rcx)==6]%>%
#     .[,rc3:=substr(rcx,1,3)]%>%
#     .[rc3%in%geo[,rc9],.(rc6=rcx,rc3)]%>%
#     geo[.,on=c(rc9='rc3')]%>%
#     .[,.(rc9=rc6,nx,lab)]%>%
#     rbind(.,geo[nchar(rc9)==6])
#   cname=rbind(
#     data.table(panel=1,nx=1:10,lab=gsub('-','',cardinal)),
#     data.table(panel=2,nx=1:10,lab=paste0('np',1:10)),
#     data.table(panel=3,nx=1:10,lab=f230703c()[order(nx),abbrev])
#   )%>%
#     .[panel==ipanel,.(ipanel,nx,lab)]
#   x1 <- 
#     stat[type=='all',.(nx,rsq)]%>%
#     .[prj[,.(nx,rbarsqprj,aprj,ase=aprj/atprj)],on=c(nx='nx')]%>%
#     .[bwe[,.(nx,b1,tbw=atan2(b3w,b2w),dtbw=c(NA,diff(atan2(b3w,b2w))))],on=c(nx='nx')]%>%
#     .[pva[x0,on=c(rcx='rc9')][,.(nid=sum(nid),ppm2min=min(ppm2),ppm2max=max(ppm2),ppm2=sum(pv)/sum(m2)),nx],on=c(nx='nx')]%>%
#     .[,.(nx,ppm2min,ppm2max,ppm2,fid=nid/sum(nid),r2rsi=rsq,rbar2prj=rbarsqprj,b1,tbw,dtbw,aprj,ase)]%>%
#     cname[.,on=c(nx='nx')]%>%
#     .[order(-nx)]
#   x1
# }
# 
# 
library(scales) #hue_pal

load('acycle/z321.Rdata')
load('acycle/pxosrdo2dd.Rdata')


bins=seq(from=-8,to=8,length.out=11)/100
binlabel <- copy(bins)
bins[1] <- -.2
bins[length(bins)] <- .2
pal <- leaflet::colorBin(scales::hue_pal(direction=1)(length(bins)),bins = rev(bins),reverse=T)

pal <- leaflet::colorNumeric(palette=cobalt()[2:3],domain=0:1)
# 
# f240810a <- 
#   function(
#     zoomx=6.5,
#     x3a=pxosrdo2dd,#rc6 polygons 
#     rcx=regpcode(pxosrdo2dd$name[grep('^NG',pxosrdo2dd$name)]),#c('NG-7--','NG-2--')#,
#     targetrcx=rcx[1],
#     minzoom=6,
#     maxzoom=11
#   ){
#     gsx <- grepstring(rcx,caret=T,dollar=T)
#     width=1000*.7
#     height=1000*.7
#     w1=1
#     l1=.08
#     addlegend=F
#     uproj3 = #desired longlat
#       "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"%>%
#       CRS(.)
#     x3 <- #data slot is pc
#       x3a[grep(gsx,regpcode(x3a@data$name)),] #selected polygons named irreg
#     x4 <- #add rc6 and vv
#       data.table(x3@data)%>%
#       .[,rc6:=regpcode(name)]%>%
#       .[,.(name,rc6,col=as.numeric(rc6==targetrcx))]
#     x3@data <- data.frame(x4)
#     x5 <- z110[grep(gsx,rcx),.(rcx=sort(rcx))]
#     labels <- sprintf(
#       paste0("<strong>%s</strong><br/>%g"),
#       x5[,irregpcode(rcx)],z110[x5,round(ppm2,-1),on=c(rcx='rcx')]
#     ) %>%
#       lapply(htmltools::HTML)
#     x7 <- leaflet(
#       x3,
#       width=width,
#       height=height,
#       options=leafletOptions(
#         zoomControl=F,
#         minZoom=minzoom,
#         maxZoom=maxzoom,
#         zoomDelta=2
#       )
#     )
#     x8 <- x7 %>% 
#       addPolygons(
#         stroke=T,
#         fillColor = ~pal(col),
#         smoothFactor = 0, #remove white slivers
#         weight = 1,
#         opacity = 1,
#         color = "steelblue",
#         dashArray = "2",
#         fillOpacity = .5,
#         label = labels,
#         labelOptions = labelOptions(
#           style = list("font-weight" = "normal", padding = "3px 8px"),
#           textsize = "12px",
#           direction = "auto")
#       )%>%
#       addProviderTiles(providers$CartoDB.Positron)
#     x8
#   }
# f240810a()
# 
# 
# 
# 
# #   renderLeaflet(
# #   z321$geo[nx==z321$geo[rc9==input$tgtrc6,nx],rc9]%>%
# #     f240810a(rcx=.,x3a=pxosrdo2dd)
#   # )
#     z321$geo[nx==4,rc9]%>%
#       f240810a(rcx=.,x3a=pxosrdo2dd)