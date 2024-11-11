
#---------------------------------Header-----------------------------------1----
  library(gt)
  library(leaflet)
load('app8 working script/.Rdata')
if(T) {
  #dependencies
  if(F) {
    f241020aFun()
    f241020bFun()
    #f241020dFun() #2 hrs
    f241021aFun()
  } else 
  {
    #getgd
    nn <- c(
      'x101',
      'z110',
      'z222',
      'f241020ad',# lab    gx    des    rc9    nx hashlab    
      'f241020bd',# gx itrim  tbin    nx    lab    rc9    des  
      'f241020dd',# rss, estdt
      'f241021ad' # pcalist, geoplus, estdt, rss, beta
    )
    getgd(nn)
    #}
    #getlast
    pxosrdo2dd <- getlast('pxosrdo2dd')
    #par
    steprip <- 'now\\ver001\\03rip'
    lightenx=.4
    sf=4
    #function
    palna <- #national ---
      c(lightenx,0)%>% #for non-target, lighten
      lighten('green',.)%>%
      leaflet::colorNumeric(palette=.,domain=0:1)
    
    
    print(getwd())
    pgmt='dotted'
    pgmc='grey50'
    pgms=.2
    lightenx <- .7
    gridheight="630px"
    colx <- cobalt()[c(4,2,1)]
    
    dfnx <- seq.Date( #same as t4dump but define in code
      from=as.Date('1994-12-31'),
      to=as.Date('2024-12-31'),
      by='y')
    dfnx[length(dfnx)] <- 
      as.Date('2024-07-31')
    steprip <- '03rip/'
    nfig1 <- 3 #for T2
    nfig2 <- -1 #for ppm2
    nfig3 <- 4 #for frac
    options(scipen=999) #no scientific format
    
    f241108a <- 
      function(tc=typeC,tb=tbinC) {
        x1 <- 
          list(
            paste0('Geographic bins: ',c(A='All',L='Local',N='National',C='Custom')[tc]),
            paste0('Time sampling: ',c('Low Frequency','High Frequency','Annual')[tb])
          )
        x1    
      }
    
    
    #-----------------------end header part
  }
}
#from app4              ---------------------------------------------------------------------

#--------------------------------Control-----------------------------------2----
f241105a <- #assign 'Controls' ----
function(
    hoflC   = c('house','flat','all')[3],
    itriC   = c('.0'=1,'.1'=2,'.5'=3)[2], #Trim ---
    neusC   = c('new','used','all')[3],
    #rc3coC  =  c('SW-','SE-','TN-','PE-','CT-','S--','LU-','KT-','L--','CA-'),  #comp
    rc3coC  =  c('NG-','MK-','AL-'),  #comp
    rc6cuC  = c('NG-7--'), #custom
    rc6rC   = 'NG-1--', #reference
    rc6tC   = 'NG-7--', #target
    rcumC   = c('ret','cum')[1],
    tbinC   = c(lo=1,hi=2,an=3)[2],  #lo hi an ---
    tstartC = 22, #for tables
    typeC   = c('A','L','N','C')[2], #All Local National ---
    typerC  = typeC,
    zerorefC =F, #set reference asset NULL
    showtradetriangle=F
) {
  hoflC  <<- hoflC
  itriC <<- itriC
  neusC <<- neusC
  rc3coC <<- rc3coC
  rc6cuC <<- rc6cuC
  rc6rC <<- rc6rC
  rc6tC <<- rc6tC
  rcumC <<- rcumC
  tbinC <<- tbinC
  tstartC <<- tstartC
  typeC <<- typeC
  typerC <<- typerC
  zerorefC <<- zerorefC
  x00 <<-  copy(f241021ad)
  geoplus <<- copy(x00$geoplus)
  estdt <<- copy(x00$estdt)
  rss <<- copy(x00$rss)
  showtradetriangle <<- showtradetriangle
}
f241105a()

#------------------------------------Reactive

geo0R <- #full geo (itriC tbinC rc6tC)
  geoplus[type=='L'][itrim==itriC][tbin==tbinC][,.(nx,gx,lab=des,rc6=rc9,rc3=substr(rc9,1,3),ltile=as.numeric(substr(des,4,4)))]%>%
  z110[.,on=c(rcx='rc6')]%>%.[,.(nx,gx,lab,rc3,rc6=rcx,ltile)]
geoaR <- #area geo  (itriC tbinC rc6tC)
  geo0R%>%
  .[rc3==substr(rc6tC,1,3)]
geodR <- #district geo  (itriC tbinC rc6tC)
  geoaR%>%
  .[rc6==rc6tC]
geotR <- 
  geoaR[geodR[,.(ltile)],on=c(ltile='ltile')]
nxaR <- #area peers (itriC tbinC rc6tC)
  geoaR[,.(nx,rc3,ltile,lab)]%>%
  unique(.)
nxtR <- #target tertile(itriC tbinC rc6tC)
  geotR[,.(nx,rc3,ltile,lab)]%>%
  unique(.)
estdtaR <- #area peers (itriC tbinC rc6tC)
  estdt[nxaR,on=c(nx='nx')]%>%
  .[,.(nx,date,ii,lab=i.lab,rc3,ltile,xdotd,days,xdot,x)] #estdtR[,.N,lab][,diff(range(N))]%>%`==`(x=.,y=0)%>%stopifnot(.)
estdttR <- #target tertile (itriC tbinC rc6tC)
  estdt[nxtR,on=c(nx='nx')]%>%
  .[,.(nx,date,ii,lab=i.lab,rc3,ltile,xdotd,days,xdot,x)]

rssaR <- #area peers (itriC tbinC rc6tC)
  rss[nxaR,on=c(nx='nx')]#%>%
#.[,.(nx,date,ii,lab=i.lab,rc3,ltile,xdotd,days,xdot,x)] #estdtR[,.N,lab][,diff(range(N))]%>%`==`(x=.,y=0)%>%stopifnot(.)
rsstR <- #target (itriC tbinC rc6tC)
  rss[nxtR,on=c(nx='nx')]#%>%
#.[,.(nx,date,ii,lab=i.lab,rc3,ltile,xdotd,days,xdot,x)]

ylimR <- 
  estdtaR[,range(x)]




#---1.1 map
geoaR%>% #lab=rc3-q and both parts are used here
  #  .[rc3==rc3xR]%>% #rc3 match
  .[,.(
    rc6,
    col=lighten(colx,lightenx)[.BY[[1]]],
    ltile,
    lab #include label 
  ),by=.(ltile)]%>% 
  .[
    rc6==rc6tC, #target district
    col:=colx[.BY[[1]]],
    by=.(ltile)
  ]%>%#[as.numeric(substr(lab,4,4))]  #overwrite target: colour without lightening
  .[]%>%
  f240810b(.) #fields rc6,col

#----1.2 x(t)


x1 <- 
  estdtaR[,.SD[,.(ii,x=x-ifelse(tstartC==0,0,x[tstartC]))],.(col=factor(ltile))]%>%
  ggplot(.,aes(ii,x,color=col))+
  geom_line()+
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    text=element_text(size=10,face='plain'),
    axis.line.y.left=element_line(size=.1),
    axis.line.x.bottom=element_line(size=.1),
    axis.text=element_text(size=10,face = "plain"),
    legend.title=element_blank()
    #legend.position='none')
  )+
  ylab('Cumulative log return')+
  xlab('Time period')+
  ggtitle(paste0('type:',typeC,' trim:',itriC,' tbin:',tbinC))

x0 <- setNames(cobalt()[c('punk','green','blue')],as.character(1:3))
x3 <- estdtaR[,.SD[,.(ifelse(tstartC==0,0,x[tstartC]))],.(ltile)][,mean(V1)]
x2 <- 
  #estdtaR%>%
  estdtaR[,.SD[,.(ii,date,lab,x=x-ifelse(tstartC==0,0,x[tstartC]))],.(ltile)]%>%
  .[,qq:=as.factor(ltile)]%>%
  .[,labx:=ifelse(date==max(date),lab,NA)]
x <- x2%>%
  ggplot(.,aes(date,x,color=qq,label=labx))+
  geom_hline(yintercept=0,linewidth=.4,linetype = "dotted",color='grey40')+
  geom_line()+
  geom_point(size=.3)+
  geom_text_repel()+
  ylim(ylimR-x3)+
  xlab('')+
  ylab(bquote(Delta~P~log~price~change))+
  theme_bw() +
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_line(size=pgms,linetype = pgmt,color=pgmc),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    text=element_text(size=16,face='plain'),
    axis.line.y.left=element_line(size=.1),
    axis.line.x.bottom=element_line(size=.1),
    legend.position='none')+
  scale_color_manual(values=x0)+#cobalt()[2:4]  as.character(cobalt())
  scale_x_date(
    breaks = as.Date(c('1995-01-01','2000-01-01','2010-01-01','2020-01-01','2024-01-01')),
    date_labels = "%Y",
    limits=c(as.Date(c('1994-12-31','2027-12-31')))
  )
x

#----1.3 winding
d1 <- #daily
  seq.Date(from=min(x101)+1,to=max(x101),by='d')
d2 <- #yearend + final date
  x101%>%
  ifelse(.==as.Date('2009-02-28'),as.Date('2008-12-31'),.)%>%
  as.Date(.)%>%
  .[-1]
x1 <- 
  estdttR%>%
  #.[nxR,on=c(nx='nx')]%>%
  .[.(date=d1),on=c(date='date'),roll=-Inf,j=.(date,xdotd)]%>%
  .[,.(ii=1:.N,date,x=cumsum(xdotd))]%>%
  .[.(date2=d2),on=c(date='date2')]%>%
  .[,.(date,x,xdot=c(x[1],diff(x)),ii=1:.N)]%>%
  .[,.(ii,date,xdot,x)]%>%
  .[,.(date,xdot)]%>%
  .[date==as.Date('2009-02-28'),let(date,as.Date('2008-12-31'))]%>%
  .[,.(decade=substr(date,1,3),yr=substr(date,4,4),xdot=round(xdot,3))]%>%
  dcast(.,decade~yr,value.var='xdot')%>%
  .[,decade:=c(1990,2000,2010,2020)]
for(i in 2:length(x1)) x1[[i]] <- ifelse(is.na(x1[[i]]),'',as.character(round(x1[[i]],3)))
x2 <- gt::gt(x1)%>%gt::tab_footnote(
  footnote=f241108a(typeC,tbinC)[[1]]
)%>%gt::tab_footnote(
  footnote=f241108a(typeC,tbinC)[[2]]
)
x2

#---- 1.4characteristics table
nfig1 <- 3 #for T2
nfig2 <- -1 #for ppm2
nfig3 <- 4 #for frac
z110[rssaR,on=c(rcx='rc6')]%>%
  .[,.(
    frac=round(sum(nid)/z110[nchar(rcx)==6,sum(nid)],nfig3),
    nid=sum(nid),
    ppm2max=round(max(ppm2),nfig2),
    ppm2min=round(min(ppm2),nfig2),
    p=round(sum(pv)/sum(m2),nfig2)
  ),
  lab
  ]%>% 
  .[rssaR[,.(R2rsi=1-sum(ssek)/sum(sstr)),lab],on=c(lab='lab')]%>%
  .[order(-p)]%>%
  .[,.(
    lab,
    frac,
    R2rsi=round(R2rsi,3),
    p=prettyNum(round(p,nfig3), big.mark=","),
    p.cus=paste0(prettyNum(round(ppm2min,nfig2), big.mark=","),'-',prettyNum(round(ppm2max,nfig2), big.mark=","))
  )]%>%
  gt::gt(.)%>%
  cols_label(
    lab = gt::html('Area-band'),
    frac = gt::html('Fraction<br>properties'),
    R2rsi = gt::html("RSI R<sup>2</sup>"),
    p = gt::html("Aggregate"),
    p.cus=gt::html("Range")
  )%>%
  tab_spanner(
    label = gt::html("£/m<sup>2</sup>"),
    columns = c(p.cus, p)
  )%>%gt::tab_footnote(
    footnote=f241108a(typeC,tbinC)[[1]]
  )%>%gt::tab_footnote(
    footnote=f241108a(typeC,tbinC)[[2]]
  )

#----1.5 summary table
estdtaR%>%
  .[ii>=tstartC]%>%
  dcast(.,ii~lab,value.var='xdot')%>%
  .[,-'ii']%>%
  as.matrix(.)%>%
  zoo(.,estdtaR[,sort(unique(date))])%>%
  table.Stats(.,digits=3)%>%
  data.table(.,keep.rownames = T)%>%
  `[`(.,i=-c(1,2,7,11,12,13))%>%
  gt::gt(.)%>%
  cols_label(
    rn = gt::html('Log return<br>summary')
  )%>%gt::tab_footnote(
    footnote=f241108a(typeC,tbinC)[[1]]
  )%>%gt::tab_footnote(
    footnote=f241108a(typeC,tbinC)[[2]]
  )

#----1.6 buy/sell/trade table
steprip <- 'now/ver001/03rip/'
x1 <- 
  geotR[,rc6]%>%
  coread(.,steprip)%>% #or rc6tC
  .[,.(N=.N,mean=round(mean(as.numeric(retsa)),4)),.(buy=substr(buydate,1,4),sell=substr(selldate,1,4))]%>%
  .[(buy>=estdttR[ii>=tstartC,substr(min(as.character(date)),1,4)])]%>%
  dcast(.,
        buy~sell,
        value.var='mean'
  )
for(i in 2:length(x1)) x1[[i]] <- ifelse(is.na(x1[[i]]),'',as.character(round(x1[[i]],3)))
x2 <- 
  geotR[,rc6]%>%
  coread(.,steprip)%>% #or rc6tC
  .[,.(N=.N),.(buy=substr(buydate,1,4),sell=substr(selldate,1,4))]%>%
  .[(buy>=estdttR[ii>=tstartC,substr(min(as.character(date)),1,4)])]%>%
  dcast(.,
        buy~sell,
        value.var='N'
  )
for(i in 2:length(x2)) x2[[i]] <- ifelse(is.na(x2[[i]]),'',x2[[i]])
x3 <- 
  gt::gt(x1)%>%gt::tab_footnote(
    footnote=f241108a(typeC,tbinC)[[1]]
  )%>%gt::tab_footnote(
    footnote=f241108a(typeC,tbinC)[[2]]
  )
x4 <- 
  gt::gt(x2)%>%gt::tab_footnote(
    footnote=f241108a(typeC,tbinC)[[1]]
  )%>%gt::tab_footnote(
    footnote=f241108a(typeC,tbinC)[[2]]
  )
x3
x4


#---accuracy 4
#1 tbin
x1 <- 
  data.table(tbin=1:3,freq=c('lo','hi','an'))
x2 <- rss%>%
  .[geoaR,on=c(rc6='rc6')]%>%
  .[type=='L']%>%
  .[itrim==itriC]%>%
  .[,.(n=sum(n),ssek=sum(ssek)),.(tbin,rc6)]
x3 <- 
  rbind(
    x2[,.(span='index.average',mse=round(sqrt(sum(ssek)/sum(n)),4)),tbin],
    x2[rc6==rc6tC,.(span=rc6tC,mse=round(sqrt(sum(ssek)/sum(n)),4)),tbin]
  )%>%
  dcast(.,tbin~span,value.var='mse')%>%
  x1[.,on=c(tbin='tbin')]%>%
  .[,-'tbin']
gt::gt(x3)%>%gt::tab_footnote(
  footnote=f241108a(typeC,tbinC)[[1]]
)%>%gt::tab_footnote(
  footnote=f241108a(typeC,tbinC)[[2]]
)

#2 trim
x1 <- 
  data.table(itrim=1:3,threshold=c('0.0','0.1','0.5'))
x2 <- 
  rss%>%
  .[geoaR,on=c(rc6='rc6')]%>%
  .[type=='L']%>%
  .[tbin==tbinC]%>%
  .[,.(n=sum(n),ssek=sum(ssek)),.(itrim,rc6)]
x3 <- rbind(
  x2[,.(span='index.average',mse=round(sqrt(sum(ssek)/sum(n)),4)),itrim],
  x2[rc6==rc6tC,.(span=rc6tC,mse=round(sqrt(sum(ssek)/sum(n)),4)),itrim]
)%>%
  dcast(.,itrim~span,value.var='mse')%>%
  x1[.,on=c(itrim='itrim')]%>%
  .[,-'itrim']
gt::gt(x3)%>%gt::tab_footnote(
  footnote=f241108a(typeC,tbinC)[[1]]
)%>%gt::tab_footnote(
  footnote=f241108a(typeC,tbinC)[[2]]
)

#3 in/out  
x1 <- 
  rss%>%
  .[geoaR,on=c(rc6='rc6')]%>%
  .[type=='L']%>%
  .[tbin==tbinC]%>%
  .[itrim==itriC]%>%
  .[,.(n=sum(n),ssek=sum(ssek),ssei=sum(ssei)),.(itrim,rc6)]
x2 <- 
  rbind(
    x1[,.(outsamp=round(sqrt(sum(ssek)/sum(n)),4),insamp=round(sqrt(sum(ssei)/sum(n)),4))],
    x1[rc6==rc6tC,.(outsamp=round(sqrt(sum(ssek)/sum(n)),4),insamp=round(sqrt(sum(ssei)/sum(n)),4))]
  )%>%
  as.matrix(.)%>%t(.)%>%as.data.table(.,keep.rownames=T)
setnames(x2,c('domain','index.average',rc6tC)[1:ncol(x2)])
if(ncol(x2)==3) x2 <- x2[,c(1,3,2)]
gt::gt(x2)%>%gt::tab_footnote(
  footnote=f241108a(typeC,tbinC)[[1]]
)%>%gt::tab_footnote(
  footnote=f241108a(typeC,tbinC)[[2]]
)


#---constituents 1
x0 <- 
  #  geodR
  geo0R[z110,on=c(rc6='rcx'),nomatch=NULL]%>%
  .[,.(ppm2=sum(pv)/sum(m2)),.(gx,nx)]%>%
  #unique(.)%>%
  #.[geotR,on=c(nx='nx')]%>%
  .[1,-c('ppm2')]

x1 <- 
  fread('./now/ver001/f241024ad.csv')%>%
  .[order(tbin,gx,date)]%>%.[,let(t,1:.N),.(tbin,gx)]%>%
  .[tbin==tbinC]%>%
  #.[gx==x0[,gx]]
  #.[,let(t,1:.N),.(tbin,gx,date)]%>%
  .[geodR,on=c(gx='gx')]%>%
  .[order(date),.(date,t,lab,NF,NH,UF,UH)]#%>%.[date==min(date)]
#.[,let(t,1:.N),.(tbin,gx,date)]%>%.[]
x1#[rc6==min(rc6)]
x2 <- 
  estdttR%>%
  .[,.(t=c(0,ii),days=c(NA,days),date=c(date[1]-days[1],date),xdot=c(NA,xdot),x=c(0,x))]%>%
  x1[.,on=c(t='t')]%>%
  .[,.(t,date=i.date,days,xdot,x,NF,NH,UF,UH,tot=NF+NH+UF+UH)]%>%
  .[-1,.(
    t,
    date,
    days,
    return=round(xdot,sf),
    cumreturn=round(x,sf),
    newhouse=round(NH/tot,sf),
    usedhouse=round(UH/tot,sf),
    newflat=round(NF/tot,sf),
    usedflat=round(UF/tot,sf),
    total=round(tot/1000,1),
    perday=round(tot/days)
  )]
x3 <- #districts footnote
  geotR[
    ,paste0('Districts: ',paste0(sort(irregpcode(rc6)),collapse=', '))]
gt::gt(x2)%>%gt::tab_footnote(
  footnote=f241108a(typeC,tbinC)[[1]]
)%>%gt::tab_footnote(
  footnote=f241108a(typeC,tbinC)[[2]]
)%>%gt::tab_footnote(
  footnote=x3,
  locations = NULL,
  placement = c("auto", "right", "left")
)


#---all-district table 1
x3 <- 
  geo0R[,.(rc3,rc6,ltile)]%>%
  z110[.,on=c(rcx='rc6')]%>%
  .[,.(rc3,rc6=rcx,nid,ppm2=round(ppm2),quantile=paste0('local-',ltile))]
DT::datatable(
  x3,
  options = list(
    columnDefs = list(list(className = 'dt-center', targets = 1:4)),
    initComplete = JS(
      "function(settings, json) {",
      "$('body').css({'font-family': 'Calibri'});",
      "}"
    )
  ),
  rownames=F
)%>%
  formatStyle( 0, target= 'row', lineHeight='70%')

