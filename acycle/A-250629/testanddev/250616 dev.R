#aggregates

#default args for testing
pvax=copy(z110)
x1=copy(f250509ed)
kssx=x1$kfoldss
geox=x1$geo[grep('BA$',lab)]

#aggregate kss using geo
kssx[,.(ssei=sum(ssei),toti=sum(toti),ssek=sum(ssek),sser=sum(sser),sstr=sum(sstr),n=sum(n)),.(nx)]%>%
  .[order(nx)]

#aggregate pva using ge
pvax[geox,on=c(rcx='rc9')]%>%
  .[,.(nid=sum(nid),m2=sum(m2),pv=sum(pv),pt=sum(pt),ppm2=sum(pv)/sum(m2)),.(nx)]%>%
  .[order(nx),]%>%
  sco(.,F)
geox[nx<4]


#we use one geo in multiple indices 'nx'
#an index nx is determined by calendar, geo
#this leads to a 'trap' - supposing that nx is dermined by 
#where is this done, the re-use of geo to define 'trials'?


#need 'pure geo' and 'pure calendar'?
C122a <- #kss(rc6x) 'nx referencing rc6x, covering rc6x' 
  function(rc6x = rc6tG) {
    rc3x <- substr(rc6x, 1, 3)
    x1 <-
      rbind(
        geox[grep(paste0("^C", rc3x), lab), .(nx, lab)] %>% unique(.) %>% .[order(nx)], # one custom per rc6
        geox[grep(paste0("^L", rc3x), lab), .(nx, lab)] %>%
          unique(.) %>%
          .[order(nx)], # 6 local 6
        geox[grep(paste0("^", rc3x), rc9)][grep("^R", lab), .(nx, lab)] %>%
          unique(.) %>%
          .[order(nx)], # 1 region
        geox[grep(paste0("^", rc3x), rc9)][grep("^N", lab), .(nx, lab)] %>%
          unique(.) %>%
          .[order(nx)], # 3 national
        geox[grep(paste0("^", rc3x), rc9)][grep("^M", lab), .(nx, lab)] %>%
          unique(.) %>%
          .[order(nx)] # 1 market
      )%>%
      sco(.,F)
    x2 <-
      rbind(
        geox[grep(paste0("^C", rc6x), lab), .(nx, lab)] %>% unique(.) %>% .[order(nx)], # one custom per rc6
        geox[rc9 == rc6x] %>%
          .[grep(paste0("^L", rc3x), lab), .(nx, lab)] %>%
          unique(.) %>%
          .[order(nx)], # 6 local 3
        geox[grep(paste0("^", rc6x), rc9)][grep("^R", lab), .(nx, lab)] %>%
          unique(.) %>%
          .[order(nx)], # 1 region
        geox[grep(paste0("^", rc6x), rc9)][grep("^N", lab), .(nx, lab)] %>%
          unique(.) %>%
          .[order(nx)], # 3 national
        geox[grep(paste0("^", rc6x), rc9)][grep("^M", lab), .(nx, lab)] %>%
          unique(.) %>%
          .[order(nx)] # 1 market
      )%>%
      sco(.,F)
    x3 <- list(
      rc3 = x1,
      rc6 = x2
    )
    x3[c(
      'rc3', #all {nx, lab} referencing rc6x
      'rc6'  #7 {nx, lab} covering rc6x
      )]
  }
C122a()
C122b <-
  function( #kss(rc6x) '7 covering' 
    rc6tx=rc6tG,
      res = f250509ed, #result
      x1 = C122a(rc6x=rc6tx)$rc6, #{lab nx} default is 7 geo covering
      pva=z110
      ) {
    x2 <- # geo 'all rc9'
      res$geo[x1[, .(nx)], on = c(nx = "nx")]
    #browser()
    x3 <- # kxx '7'
      res$kfoldss[x1, on = c(nx = "nx")] %>%
      #.[grep(paste0('^',rc6tx),rc6)] %>%
      .[, n := as.numeric(n)] %>%.[]%>% # avoid warnings in melt
      melt(., id.vars = c("nx", "lab", "rc6")) %>%
      .[, .(sumx = sum(value)), .(nx, lab, variable)] %>%
      dcast(., nx + lab ~ variable, value.var = "sumx") %>%
      .[, .(
        msei = sqrt(ssei / n),  #inlier/insample error
        mtoti = sqrt(toti / n), #inlier/insample total
        msek = sqrt(ssek / n),  #kfold error
        mser = sqrt(sser / n),  #all error
        mstr = sqrt(sstr / n)   #all total
        ), 
        .(nx, lab)] %>%
      .[order(-msek)]%>%
      sco(.,F)
    #browser()
    x4 <- 
      pva[x2,on=c(rcx='rc9')]%>%
      .[,.(nid=sum(nid),m2=sum(m2),pv=sum(pv),ppm2=sum(pv)/sum(m2),minppm2=round(min(pv/m2)),maxppm2=round(max(pv/m2))),.(lab,nx)]%>%
      sco(.,F)
    #x4[x3,on=c(lab='lab')] 
    list(
      geo=res$geo[x1[, .(nx)],on=c(nx='nx')],
      estdt=res$estdt[x1, on = c(nx = "nx")],
      kfoldsse=x3,
      pva=x4
      )
  }
C122b(rc6tG) #all 7 cover(rc6t) 
#for 7 rows in rc6 
#   aggregated pv 
#   aggregated kss
#   estdt
#   geo
pva=z110


pva[rssx, on = c(rcx = "rc6")] %>%
      .[
        , .(
          frac = round(sum(nid) / z110x[nchar(rcx) == 6, sum(nid)], nfig3),
          nid = sum(nid),
          ppm2max = round(max(ppm2), nfig2),
          ppm2min = round(min(ppm2), nfig2),
          p = round(sum(pv) / sum(m2), nfig2)
        ),
        lab
      ] 


#these two blocks of 6,7 have partial overlap 

#this is the one for tabulation in 122 - it is all local
#to this add custom
x1 <- C122a(rc6x=rc6tG)$rc3[grep('^L',lab)] # 6 local {nx lab} partitioning rc3x 6 6 6
x2 <- C122b(x1=x1) #
x2
#
x3 <- C122a(rc6x=rc6tG)$rc3[grep(paste0('^C',rc6tG),lab)] # 1 custom 1 1 1 
x4 <- C122b(x1=x3) #
x4

x5 <- C122b(x1=rbind(x3,x1)) #<<<<<this is how it should be done
x5$pva[x5$kfoldsse,on=c(nx='nx')]%>%
  .[,.(
    lab,
    frac = nid / pva[nchar(rcx)==6,sum(nid)],
    R2=1-(msek/mtoti)^2,
    minppm2,
    maxppm2,
    rangeppm2=round(log(maxppm2/minppm2),3),
    aggppm2=round(ppm2)
  )
  ]
#<<<<better to do this as a % ranking 0-100 min-max
#what sort of tertiles are these??? am I using the old or gen2 data?


x5 <- #c
rbind(
x2$pva[x2$kfoldsse,on=c(nx='nx')]%>%
  sco(.,F)%>%
  .[,.(lab,m2,msei,msek,mser,mstr,mtoti,nid,nx,ppm2,pv)]%>%
  .[order(-msek)],
x4$pva[x4$kfoldsse,on=c(nx='nx')]%>%
  sco(.,F)%>%
  .[,.(lab,m2,msei,msek,mser,mstr,mtoti,nid,nx,ppm2,pv)]%>%
  .[order(-msek)]




x7 <- C122a(rc6x=rc6tG)$rc6 # 7 various covering rc6x 7 7 7
x8 <- C122b(x1=x7)
x8$kfoldsse[x8$pva,on=c(nx='nx')]%>%
  sco(.,F)%>%
  .[,.(lab,m2,msei,msek,mser,mstr,mtoti,nid,nx,ppm2,pv)]%>%
  .[order(-msek)]





x5[
  grep("^L|^C", lab),
  .(lab,
    frac = nid / pva[
      nchar(rcx) == 6,
      sum(nid)
    R2=1-msek/mtoti,
    maxppm2=
    
    ]
  )
]




C122b(rc6tx=rc6tG,x1 = C122a(rc6x=rc6tx)$rc3[grep('^L',lab)]) #all 6 local(rc6t)


x1 <- C122b(rc6tx=rc6tG,x1 = C122a(rc6x=rc6tx)$rc3[grep('^L',lab)]) #all 6 local(rc6t)
x1$kfoldsse[x1$pva,on=c(nx=)]    

    x3 <- # kxx '7'
      res$estdt[x1, on = c(nx = "nx")]
    x3[,.N,.(nx,lab)]
























x3 <- #kxx '7'
  res$estdt[x1[,.(nx)],on=c(nx='nx')]
  res$kfoldss[x1,on=c(nx='nx')]%>%
  .[rc6==rc6x]%>%
  .[,n:=as.numeric(n)]%>% #avoid warnings in melt
  melt(.,id.vars=c('nx','lab','rc6'))%>%
  .[,.(sumx=sum(value)),.(nx,lab,variable)]%>%
  dcast(.,nx+lab~variable,value.var='sumx')%>%
  .[,.(msei=sqrt(ssei/n),mtoti=sqrt(toti/n),msek=sqrt(ssek/n),mser=sqrt(sser/n),mstr=sqrt(sstr/n)),.(nx,lab)]%>%
  .[order(-msek)]

  
  
  
  
    #this seems wrong....
  
  C112a()[rc6 == "W--8--", .(rc6, lab, sqrt(ssek / n))]
  C112b()
  C112a()
  C121a()
  C121b()
  C121c()
  
  x3 <- #pva '7'
  res$estdt[x1[,.(nx)],on=c(nx='nx')]


#geo defines aggreg
#estdt 7, one per geo-row (n.b. in graphic want instead the 'extremal local-selected' - where do I determine local-selected?)
#kss one per geo-row
#pva one per geo-row