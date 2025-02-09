                               #accrue delta annual
                               #bmn    index drc
stepnac='now\\ver001\\01nac'   #nac
stepsip ='now\\ver001\\02sip'  #sip
steprip ='now\\ver001\\03rip'  #rip

stepprav2='now\\ver002\\07pra' #accrue delta drc
stepprav3='now\\ver003\\07pra' #bmn    delta drc
stepprav4='now\\ver004\\07pra' #bmn    index annual
#stepnac ='now\\ver001\\01nac'  #annual bmn
coread('AL-1--1--',stepprav2) #accrue delta drc
coread('AL-1--1--',stepprav3) #bmn    delta annual
coread('AL-1--1--',stepprav4) #bmn    index annual - not correct

x1 <- 
  f240817b(  #read pra pseudo-BMN, write true-BMN
    nxx=1,
    geo=geo0[rc9=='AL-'],
    dfn=x101,
    steppra0='now\\ver003\\07pra', #bmn    delta drc
    steppra1='now\\ver004\\07pra'  #bmn    index annual
  )
x1


#coread('AL-1--',stepnac)[,.(idhash,deed_date,)]
#check pra is correct
rcx='AL-1--1--'
x1 <- 
  rcx%>% #check each row has buy and sell
  coread(.,stepprav4)%>%
  .[,which(nchar(names(.))==10),with=F]%>% 
  as.matrix(.)%>%
  `mode<-`(.,value='numeric')
x1%>% #check buy and sell balance
  apply(.,1,sum)%>%
  `==`(.,value=0)%>%
  all(.)%>%
  stopifnot(.)
x1%>% #check sell exists
  apply(.,1,max)%>%
  `==`(.,value=1)%>%
  all(.)%>%
  stopifnot(.)

  coread(rcx,stepnac)[idhash==x3[1]]
  coread(rcx,stepprav2)[substr(idhash.selldate,1,16)==x3[1]]
  coread(rcx,stepprav3)[substr(idhash.selldate,1,16)==x3[1]]
  coread(rcx,stepprav4)[substr(idhash.selldate,1,16)==x3[1]]


# x1 <- 
#   coread(rcx,stepprav2)%>%
#   .[,which(nchar(names(.))==10),with=F]
# x2 <- 
#   x1%>%
#   as.matrix(.)%>%
#   `mode<-`(.,value='numeric')%>%
#   apply(.,1,sum)%>%
#   `<`(.,value=0)%>%
#   which(.)
# x1[x2]
  geo0 <- f230703d()
rcx='NG-2--'
rcx='S--11-'
sfn=#seas 'from nowt'
  getlast('f220822ad')
x0 <- 
  sfn[,.(rc3=rc,buy,sell,seas)]%>%
  .[(rc3==substr(rcx,1,3))&(buy=='01')]%>%
  .[,.(
    rc3,
    mon=sell,
    seas=seas-mean(seas)
    )
    ]


x1b <-  #integer dummies BMN
  coread(rcx,stepprav4)%>% #has selldate but no buydate
  .[,id:=substr(idhash.selldate,1,16)]%>%
  .[,selldate:=substr(idhash.selldate,18,28)]%>%
  .[,-c('rc9')]
x1a <- #for buydate, rip
  coread(rcx,steprip)[,.(idhash.selldate,buydate)]
x1 <- x1b[x1a,on=c(idhash.selldate='idhash.selldate')]
x2 <- #for all prices,nac, adj seas
  coread(rcx,stepnac)[,.(idx,idhash,deed_date,price_paid,rc3=substr(rc9,1,3),mon=substr(deed_date,6,7))]%>%
  x0[.,on=c(rc3='rc3',mon='mon')]%>%
  .[idx=='0ok']%>%
  .[,.(idhash,deed_date,price_paid,pxsa=as.numeric(price_paid)*exp(-seas))]
x3 <- #buy, sell, with pra
  x2[x1,on=c(idhash='id',deed_date='selldate'),nomatch=NULL]%>%
  setnames(.,old=c('deed_date','pxsa','price_paid'),new=c('selldate','sellpxsa','sellpx'))%>%
  x2[.,on=c(idhash='idhash',deed_date='buydate'),nomatch=NULL]%>%
  setnames(.,old=c('deed_date','pxsa','price_paid'),new=c('buydate','buypxsa','buypx'))
x4b <- #zero rows
  melt(x3[,which(grepl('[0-9]-',names(x3))|(names(x3)=='idhash.selldate')),with=F],id.vars='idhash.selldate')%>%
  .[x3[,.(idhash.selldate,buypxsa,sellpxsa)],on=c(idhash.selldate='idhash.selldate')]%>%
  .[value==0,.(idhash.selldate,variable,value)]
x4a <- #buy, sell rows (non-zero rows)
  melt(x3[,which(grepl('[0-9]-',names(x3))|(names(x3)=='idhash.selldate')),with=F],id.vars='idhash.selldate')%>%
  .[x3[,.(idhash.selldate,buypxsa,sellpxsa)],on=c(idhash.selldate='idhash.selldate')]%>%
  .[value!=0]%>%
  .[value==-1,value:=-buypxsa]%>%
  .[value==1,value:=sellpxsa]
x4 <- 
  rbind(x4a[,.(idhash.selldate,variable,value=round(as.numeric(value)))],x4b)%>%
  dcast(.,idhash.selldate~variable,value.var='value')
yy <- x4[,2,with=F]%>%setnames(.,'p0')%>%.[,.(p0=abs(as.numeric(p0)))]
xx <- x4[,-(1:2),with=F]%>%setnames(.,paste0('p',gsub('-','',substr(names(.),1,7))))%>%
  lapply(.,`class<-`,value='numeric')%>%
  data.frame(.)
x5 <- 
  cbind(yy,xx)
zmat <- as.matrix(xx/abs(xx))
zmat[is.nan(zmat)] <- 0
xmat <- as.matrix(xx)
ymat <- as.matrix(yy)

beta_hat <- lm.fit(t(zmat) %*% xmat, t(zmat) %*% ymat)$coefficients
index <- 1/beta_hat
plot(log(index))

x6 <- 
  lm(p0~.-1,x5)%>%
  coefficients(.)
x6
plot(1/x6)

stopifnot(nrow(x4)==nrow(x1))
stopifnot(all.equal(x4[,sort(idhash.selldate)],x1[,sort(idhash.selldate)]))

x5 <- 
  x4[,-1]%>%
  as.matrix(.)
mode(x5) <- 'numeric'  



sales_matrix <- x5
ncol(sales_matrix)

colnames(sales_matrix) <- paste0("Period_", (1:ncol(x5))-1)
rownames(sales_matrix) <- paste0("Property_", 1:nrow(x5))

Y <- sales_matrix[,1,drop=F]
X <- sales_matrix[,-1,drop=F]
sales_matrix[which(apply(sales_matrix,1,function(x){sum(x>0)})<2)[1],]

