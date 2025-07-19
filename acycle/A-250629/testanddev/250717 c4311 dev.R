#working here but confused - do I have the augmented table with all rc6 or not?  where is it?  
C4311c() #<<<<<<<<<<<<<<<this is the info needed
D4311b #<<<<this is leaflet (wrongly numbered, should be D4312)
D4311a() #is a table but not augmented

#did I just not do a blobs table with the augmentation shown?  why not?  I seem to have moved on to subsetting it, below
#final thought : including custom 'exotics' in the table will be odd, they don't belong to quantiles
#leave as-is
#1 add the outline for custom, just like 4311
#2 on the index page 4311 show a row,col excerpt from this table : rows for optimum quantile, cols excluding 'number of bins, keep row-shade only
#3 in addition the same cols for the custom geo
#that's it, then move stuff around, then renumber, then add accuracy, then cycle

D4311b()#custom geo
x1 <- ageo()%>%.[grep(paste0('^C',rc6tx,'BA$'),lab),.(nx,tgt=substr(lab,2,7),rc6)]
x1
#local optimum  geo
x2 <- ageo(areso(rcxtx=rc6tx))
x2
ageo()[nx==9663]

#custom for rc6t circled

D4311a(
      rc6tx = rc6tx
)



ageo()%>%.[grep(paste0('^C',substr(rc6tx,1,3),'.+BA$'),lab)]%>%.[,.(unique(rc6))]

{
rc6tx='W--5--'
x1 <- ageo()%>%.[grep(paste0('^C',rc6tx,'BA$'),lab),.(nx,substr(lab,2,7),rc6)]
x2 <- C4311c(rc6 = rc6tx)
x3 <- x2[x1[,.(rc6)],on=c(rc6='rc6')][order(ppm2)]
D4311a(
      rc6tx = rc6tx,
      x = x3
#      x1 = C4311a(rc6 = rc6tx)
)
}


rc6tx <- copy(rc6tG)
ageo()[grep(paste0('^C',substr(rc6tx,1,3)),lab)][,.(rc3tpeer=sort(unique(rc6)))]
D4311a()
C4311b()

D4311b()

# C4311b <- #C4311b: a second function that just returns rc6,locality,ppm2,nid for the exotic peers
#   function(
#     rc6tx=rc6tG,
#     statics='resS'
#   ) {
#     x0 <- ageo()[grep(paste0('^C',substr(rc6tx,1,3)),lab)][,.(rc6=sort(unique(rc6)))]
#     resS$f250713a%>%
#       .[x0,on=c(rc6='rc6')]%>%
#       .[resS$pva,on=c(rc6='rc6'),nomatch=NULL]%>%
#       .[,.(rc6,locality,ppm2=pv/m2,nid),nomatch=NULL]
#   }
# C4311b()
# 
# 
# #then C4311c combines this with C4311a, adding the colors in q0 using the range in C4311a, setting q1,2,3 blank and a logical column 'blobsuppress
# C4311c <- 
#   function(
#     rc6tx=rc6tG,
#     x1=C4311a(rc6tx=rc6tx),
#     x2=C4311b(rc6tx=rc6tx)[!(rc6%in%x1[,rc6])] #out of area peers
#   ) {
#     x3 <- x1[,log(range(ppm2))]%>%setNames(.,c('minP','maxP'))
#     x2[,.(
#       rc6,
#       locality,
#       ppm2,
#       nid,
#       q0=color_price(log(ppm2),x3['minP'],x3['maxP']),
#       q3='',
#       q2='',
#       q1=''
#     )]%>%
#       rbind(x1,.)
#   }
# C4311c()

# D4311b <- 
#   function(
#     rc6tx=rc6tG,
#     x1=apol(datS), # rc6 polygon
#     x2=apva(resS), # pva
#     x3=C4311c(rc6tx=rc6tx), #rc6 for this rc3, augmented with out of area peers
#     x4=pxosrdo1dd, # rc3 polygon
#     minzoom=6,
#     maxzoom=11
#   ){
#     x5 <- 
#       x4[which(x4@data$name == irregpcode(substr(rc6tx,1,3))),]
#     x6 <- 
#       f240810b( #->leaflet, colours for areas-to-shade in column 'col'
#         x3[,.(col=q0,rc6)],
#         x2 = x1, # map polygons
#         pva = resS$pva[, .(rcx = rc6, ppm2 = pv / m2)], # for tooltip
#         minzoom = minzoom,
#         maxzoom = maxzoom
#       )%>%
#       addPolygons( # outline custom districts
#         data = x5,
#         fill = F,
#         color = "black",
#         weight = 1,
#         opacity = 1
#       )
#     x6
#   }
D4311b(maxz=13,minz=8,rc6='SW-7--')
#this is arg for leaflet display
#here from D4111x
      statics = c("resS", "datS")#, #              S static
      rc6tx = rc6tG#, # target                     C control 
      rc6cx = rc6cG#, # custom                     C
      rc6xx = c(rc6tx, rc6cx)#, # custom+target    E exposed
      rc3tx = substr(rc6tx, 1, 3)#, # target rc3   E
      minzoom = 9#, #                              P parameter
      maxzoom = 12 #                              P
    #  ) {
    x1 <- apol(datS) # polygon
    x2 <- apva(resS) # pva
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



#body of C4311a
    x1 <-
      resS$geo %>%
      .[resS$lab[grep("^L", lab)][grep(substr(rc6tx, 1, 3), lab)], on = c(nx = "nx")] %>%
      .[apva(resS)[, .(rc6, rc6P = log(pv / m2), pv, m2, rc6ppm2=round(pv/m2),rc6nid=nid)], on = c(rc6 = "rc6"), nomatch = NULL]%>%
      .[,rc6col:=color_price(rc6P,min(rc6P),max(rc6P))]
    x1[]
    x2 <- 
      x1%>%
      .[,.(grpppm2=sum(pv)/sum(m2)),.(lab,nx)]%>%
      .[,.(grpppm2,lab,nx,grpP=log(grpppm2))]%>%
      .[,.(grpppm2,lab,nx,grpP,grpcol=color_price(grpP,x1[,min(rc6P)],x1[,max(rc6P)]))]%>%
      .[,.(grpppm2,lab=paste0(substr(lab,1,4),'xx',substr(lab,7,9)),nx,grpP,grpcol)]
    x2[,]
    
    x3 <- 
      x2[,.(nx,lab,grpcol,grpppm2)]%>%
      .[x1[,.(nx,rc6,rc6nid,rc6ppm2,rc6col)],on=c(nx='nx'),mult='first']%>%
      .[order(nx,rc6ppm2)]%>%
      resS$f250713a[.,on=c(rc6='rc6')]
    x3[]
    x4 <- 
      x3 %>%
      dcast(., rc6+rc6ppm2+rc6nid+rc6col+locality ~ lab, value.var = "grpcol")%>%
      .[order(-rc6ppm2)]
    x4[]
    setnames(x4,c('rc6','ppm2','nid','q0','locality','q1','q2','q3'))
    x4[,.(rc6,locality,ppm2,nid,q0,q3,q2,q1),with=T]