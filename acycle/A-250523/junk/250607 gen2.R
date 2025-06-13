D112(
    tslidex = tslideG,
    estdty = estdtxG[, .(ii, date, lab, x, qtile)],
    ylimx = ylimG,
    geoccx = geoccG) 

getgd("f250509ed")

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






