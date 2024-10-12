z222
pcaz(z222)
# 7 periods
# 0 1994-12-31 
# 1 1996-12-05 3 -+
# 2 2000-02-06 2 -+
# 3 2003-03-21 3 +-
# 4 2016-05-28 2 +-
# 5 2012-07-11 3 -+
# 6 2016-05-28 2 -+
# 7 2024-07-31 3 ??

#estimate rep areas
#function is f240710a/b

dfnx <- c(
'1994-12-31',
'1996-12-05',
'2000-02-06',
'2003-03-21',
'2016-05-28',
'2012-07-11',
'2016-05-28',
'2024-07-31'
)%>%
  as.Date(.)

#this is low freq drc used for testing, length 25 so 24 periods vs 46
#46 is number of dates in z222, which excludes t0=1994, so 46 periods
#ex gfc it is 23 periods vs 45
#  [1] "1994-12-31" "1996-12-05" "1997-10-14" "1999-04-05" "1999-12-13"
#  [6] "2001-08-09" "2002-08-23" "2003-03-21" "2003-08-10" "2004-01-17"
# [11] "2004-06-07" "2005-05-17" "2007-01-28" "2007-12-31" "2009-02-28"
# [16] "2009-06-13" "2011-03-18" "2012-07-11" "2014-05-12" "2015-04-01"
# [21] "2016-01-15" "2016-09-26" "2018-03-25" "2021-06-13" "2024-07-31"
dfnx <- c(x101[1],z222$date)[-seq(from=2,to=46,by=2)[-14]]
cocomkd("now\\ver005\\07pra") #ver
x900 <- #---here replicating drc
    sfLapply(
      geo0[,unique(nx)], #for this cowrited geo does not matter
      f230309a,
      geo=geo0,  #partition task on rc3
      dfn=dfnx, #tpx
      steprip=steprip, #in
      steppra="now\\ver005\\07pra"
    )


xx <- 
  f231113b( #             RSI (REP,DRC) *
    geo=geo, #           GEO (REP)  this is geoa which is rep. areas as desired
    steppra="now\\ver005\\07pra", #   DRC
    onepass=F,
    q1=.1,
    parx=T
  )

xx1 <- 
  pcaest( #               LFM (REP,DRC)
    x=xx$pan  #         RSI (REP,DRC)
  )
#xx1$tantheta<- z222$tantheta

plot(zoo(coredata(cumsum(pcaz(xx1)[,1:3]))),scr=1,col=1:3)

ggplot(melt(xx$pan,id.vars='date')%>%.[,.(value=cumsum(value),date),variable],aes(date,value,color=variable))+geom_line()

#ok so try and redo local tertiles on this pra/dfn
#f240719aFun <- ############################replaces 0719a
f240916aFun <- 
  function( #best 3-partition using kfold
    rc6min=4,#13, #minimum districts in rc3
    nn=c('z110','z222'), #dependencies
    nmin=3, #min rc6 in any of the 3-partition
    steppra="now\\ver005\\07pra",
    x0=z110[nchar(rcx)==3,rcx]
  ) {
    f1 <- #split string into rc6
      function(x,l=6) {
        x3 <- ''
        for(i in 1:(nchar(x)/l)) {
          x3[i] <- 
            substr(x,(i-1)*l+1,i*l)
        }
        x3
      }
    x11 <- x10 <- list(NULL)
    krange <- seq_along(x0)
    
    for(k in krange) {
     print(x0[k])
     print(paste0('start solution set/rc3 ',k-krange[1]+1,'/',length(krange),' ',x0[k]))
      x1 <- #tertile permutations, list(iat) of list(3), each a pasteup of rc6    ibar
        f240716a(
          rc3x=x0[k],
          maxp=0, #don't do it
          nmin=nmin #gets frigged to 2 for smaller rc3
        )
      x2 <- #unique sequences of rc6                                              nbar
        unique(unlist(x1)) #x1  (x2b
      x3 <- #split pasteup string into vector of rc6
        sapply(x2,f1)
      x3a <- list(NULL)
      for(nn in seq_along(x3)) {
        x3a[[nn]] <- #geo for this permutation
          data.table(rc9=x3[[nn]],nx=nn,lab=labxnnn(nn,len=4))
      }
      x4 <- #geo for all tertile permutations                                         348 nbar
        rbindlist(x3a)
      x5 <-  #rsi: //wrapper to 10a... slow, kfold solver--------------slow
        f240710b(
          geo=x4,
          dfn=dfnx,
          usepra=T,
          steppra=steppra
          )
      x6 <- list(NULL)
      x1range <- seq_along(x1) #x1
      for(i in x1range) {
        x6[[i]] <- #match tertile permutations into nx, each in range  {nx1,nx2,nx3}(iat)
          as.data.table(as.list(c(match(unlist(x1[[i]]),x2),i)))%>% #x1
          setnames(.,c('nx1','nx2','nx3','ix'))%>%.[]
      }
      x7 <- #sse(n,i)
        rbindlist(x6)%>%
        melt(.,id.vars='ix')%>%
        .[x5[grep(x0[k],rc6),.(sse=sum(ssek)),nx],on=c(value='nx'),nomatch=NULL] #changed to ssek 240914 and made metric only on target x0[k]
      x8 <- #aggregate by iat sse(i) and find min(sse) solution
        x7[,.(sse=sum(sse),n=.N),ix]%>%
        .[n==3]%>%
        .[which.min(sse),ix]
      x9 <- #parse out the rc6
        x6[x8]%>%
        unlist(.)%>%
        `[`(.,i=c('nx1','nx2','nx3'))%>%
        x2[.]%>%
        lapply(.,f1)
      x10[[k]] <- rbind(
        data.table(rc6=x9[[1]],i=1),
        data.table(rc6=x9[[2]],i=2),
        data.table(rc6=x9[[3]],i=3)
      )%>%
        .[,.(
          nx=3*(k-1)+i,
          i,
          rc3=x0[k],
          rc9=rc6, #convention for geo
          lab=paste0(x0[k],i)
        )]
      print(x10[[k]])
    }
    x11 <-  #geo
      rbindlist(x10)%>%
      .[,.(rc9,nx,lab=paste0(rc3,i))]
    f240916ad <<- x11
    putt(f240916ad)
  }
f240916aFun()


#--------------junk


plot(zoo(coredata(cumsum(pcaz(xx1)[,1:3]))),scr=1,col=1:3)
eigen(cov(xx$pan))
eigen(cov.wt(xx$pan[,-'date'],meth='ML')$cov)
pcaest(xx$pan,dopl=T)



dfnx

geo <- geoa



f231113b(    
  stepripx=steprip, 
    dfn=dfnx,    
    geo=geo,
    outthresh=.1,
    kfold=5,
    usepra=F
  )



x0 <- f240710a(
  nx=1,
    stepripx=steprip, 
    dfn=dfnx,    
    geo=geo,
    outthresh=.1,
    kfold=5,
    usepra=F
  )


x1 <- f240710b(
    #nx=c(1:3),
    stepripx=steprip, 
    dfn=dfnx,    
    geo=geo,
    outthresh=.1,
    kfold=5,
    usepra=F
  )
x1
dir(steprip)
coread('AL-1--1--',steprip)

