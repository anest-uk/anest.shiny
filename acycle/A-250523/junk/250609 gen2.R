x1=f250509ed$estdt
x2=estdtccG

x3 <- 
  intersect(colnames(x1),colnames(x2))%>%
  sort(.)


#local indices for plot
rc6tx=rc6tG
rc6tx='NG-7--'
x0=f250509ed

# C112c <-
#   function(
#     rc6tx=rc6tG,
#     coltabx=coltab
#   ) {
#     x1 <- 
#       rbind(
#         C112b()[rc3==substr(rc6tx,1,3)][order(qq)][c(1,.N)], #top and bottom
#         C112b()[lab==C112a()[rc6==rc6tx,lab]] #target
#       )%>%
#       unique(.)%>%
#       coltab[.,on=c(code='i.n')]%>%
#       .[,legendlab:=ifelse(lab==C112a()[rc6==rc6tx,lab],'target',lab)]%>%
#       .[order(-qq)]
#     x1
#   }
C112c('SY-1--')


#x1[] #key for plot


#local + custom index for plot
# C112d <- 
#   function(
#     rc6tx=rc6tG, #rc6t
#     x0=f250509ed,#kfx
#     x1=estdtccG, #cus
#     x2=C112c(rc6tx=rc6tx)#local for plot
#   ) {
#     x3 <- 
#       intersect(names(x0$estdt),names(x1))%>%
#       sort(.)
#     x4 <- 
#       rbind(
#         x0$estdt[x2,on=c(nx='nx')][,x3,with=F],
#         estdtccG[,x3,with=F]
#       )
#     x5 <- 
#       rbind(
#       x2[,.(dark,lab,legendlab)],
#       data.table(dark='brown',lab='CU00',legendlab='custom'))
#     x6 <-
#       x5[x4,on=c(lab='lab')]%>%
#       .[,.(date,ii,x,col)]
#     x6
#   }

D112x <- function(
    rc6tx=rx6tG,
    x1=estdtccG #cus
) {
  x2 <- C112d(rc6t=rc6tx)
  ggplot(x2,aes(ii,x,color=col))+geom_line()
}

D112('SE-1--')
D112('SW-3--')



x2
x2[,.N,.(legendlab,dark)]


x0$estdt[]
coltab[x0$estdt,on=c(col=)]
#color for n.i
coltab
#legend label for [this]


C112a()
C112b()


x1 <- 
  union(
C112b()[rc3==substr(rc6tG,1,3)][c(1,.N),nx],
C112a()[rc6==rc6tG,nx]
)%>%
  


D111x()

D112(
    tslidex = tslideG,
    estdty = estdtxG[, .(ii, date, lab, x, qtile)],
    ylimx = ylimG,
    geoccx = geoccG) 

getgd("f250509ed")
f250509ed

rc6tx=copy(rc6tG)

rc6tx='HX-1--'
rc6tx='HG-1--'
rc6tx='M--1--'

x1 <- 
  estdtx2%>%
  .[grep(labx,lab),.(ii, date, lab, x, qtile=as.numeric(substr(lab,7,7)))]


#recall we want the 'extreme tiles for this rc3', plus the best for this rc6, plus custom
#where is the geo, the thing called lab?
#labxG - table with optimum first
#then take from 'opt' the extrema 

#exteme tiles for each rc3 in turn, defined by i.n, qq, lab, nx

# C112a <- #local optimum kss for all rc6    TRAMSFERRED tot clib
#   function(
#     x0=f250509ed,
#     nn='f250509ed'#static
#     ) {
#     x1 <-  #local solution for all rc6
#       x0$geo %>%
#       .[grep("^L", lab)] %>%
#       x0$kfoldsse[., on = c(nx = "nx",rc6='rc9'), nomatch = NULL] %>% # local only
#       .[,rc3:=substr(rc6,1,3)]%>%
#       .[order(rc6,ssek)]%>%
#       .[,.SD[1],rc6]
#     x1
#   }
C112a()

# C112b <- #local lab for all rc6 in rc3tx [join on col here?]    TRAMSFERRED tot clib
#   function(
#     nn='f250509ed', #static
#     x0=C112a() #over
#   ){
#     x1 <- #local solution set for rc3t
#       x0%>%
#       #.[substr(rc6,1,3) == rc3tx,.(rc3tx,nx,lab)] %>%
#       .[,.(rc3,nx,lab,i.n=substr(lab,5,7))]%>%
#       unique(.)%>%
#       data.table(i.n=c('1.3','1.2','1.1','2.3','2.2','3.3'),qq=c(1/6,1/4,1/2,1/2,3/4,5/6))[.,on=c(i.n='i.n'),mult='all']%>%
#       .[order(i.n)]
#     x1
#   }
C112bd <- C112b()
C112b()[rc3=='HG-']

#D112x(
        rc6tx = rc6tG#,
        rc6ccx=rc6ccG#,
 #       )
#a D function combines loc(tgt)={top,this,bot} with custom
rc6tx=rc6tG
x0=f250509ed #ssp
x1=estdtccG[]

x2 <- #loc 3row
  C112bd[rc3==substr(rc6tx,1,3)]%>% 
  .[order(qq,i.n)]%>%
  .[c(1,which(qq==.5)[1],.N)]%>% #all 3 exist; guaranteed 3 row; prefer 1.1 for qq=.5
  sco(.,F)
x2


x3 <-  #loc
  x0$estdt%>%
  .[grep("^L", lab)] %>%
  .[x2,on=c(lab='lab')]%>%
  .[,.(date,ii,lab,nx,x)]
x4 <-  #cus
  x1%>%
  .[,.(date,ii,lab,nx,x)]
x5 <-  #loc+cus
  rbind(x3,x4)
#add color
x5[,range(x),col]
x5[,col:=as.factor(lab)]#temp solution

ggplot(x5,aes(ii,x,col=col))+geom_line()
#issues:
# actually do not want qq=.5, instead want 'local optimum for rc6t'
# colors

x1[,.(n)]
x3
x1
x2
estdtccG[]
x1
x0$estdt

x2 <- x1[]


estdtx=estdtxG


loc <- C112b()
loc



f250509ed$geo %>%
  .[grep("^L", lab)] %>%
  .[substr(rc9,1,3) == substr(rc6tx,1,3),.(nx,lab)] %>%
  unique(.)%>%
  .[f250509ed$kfoldsse, on = c(nx = "nx"), nomatch = NULL] %>% # local only
  .[order(rc6,ssek)]%>%
  .[,.SD[1],rc6]%>%
  .[order(lab),.(nx,lab,i.n=substr(lab,5,7))]%>%
  unique(.)%>%
  data.table(i.n=c('1.3','1.2','1.1','2.3','2.2','3.3'),qq=c(1/6,1/4,1/2,1/2,3/4,5/6))[.,on=c(i.n='i.n'),mult='all']%>%
  .[order(qq)]


# .[rc6 == rc6tR()] %>% # target rc6
#   .[order(ssek), .(rc6, ssek, n, nx, lab)]


#need a plan for how/where stuff is computed
#is this a common reactive, or a static global (it is static...)?
#if a static global is it cached as global data, in which case where calculated - in global maybe?
#what type of a function calculates it, i.e. in which lib does it sit?
#are there other static globals like this?
#in leaflet, do I always show all five, or just the ones populated here?
#are the colours handlee right?

#doint this correctly does matter
            
            

D112x <- 
  function(
    rc6tx=rc6tG,
    tslidex=tslideG,
    estdtccx=estdtccG,
    ylimx = ylimG,
    geoccx = geoccG,
    estdtx2=f250509ed$estdt
    #f250509ed$estdt[grep('^L',lab)]
  )  {
    labx <- paste0('L',substr(rc6tx,1,3),'...BA')
    x1 <- 
      estdtx2%>%
      .[grep(labx,lab),.(ii, date, lab, x, qtile=as.numeric(substr(lab,7,7)))]%>%
      rbind(
        .,
        estdtccx[, .(ii, date, lab, x, qtile=0)]
      )
    D112(
      tslidex = tslidex,
      estdt = x1,
      ylimx = ylimx,
      geoccx = geoccx) 
    
  }

D112x()






