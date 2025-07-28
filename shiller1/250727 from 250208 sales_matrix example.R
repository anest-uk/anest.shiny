#f250727a() #prep the full 'sal' file


base='NOW'
salpath=file.path('.',base,'ver001','10sal',fsep='\\')
rippath=steprip
#fread(colClasses=(pxsa='real'))
?fread

sal <- coread('AL-1--1--',salpath,colClasses=c(pxsa='numeric',selldate='Date'))
rip <- coread('AL-1--1--',rippath,colClasses=c(buydate='Date',selldate='Date'))
# warnings()
# sal
# rip
# 

x1 <- 
  rip[,.(idhash=substr(idhash.selldate,1,16),buydate,selldate,retsa)]
x2 <- 
  sal[,.(idhash,buydate=selldate,pxsabuy=pxsa)][x1,on=c(idhash='idhash',buydate='buydate')]%>%
  sal[.,on=c(selldate='selldate',idhash='idhash')]%>%
  .[,.(idhash,buydate,selldate,pxsabuy,pxsasell=pxsa)]
stopifnot(!anyNA(x2))
stopifnot(x2[,.N]==x1[,.N])
x2[,.(t0=buydate,t1=selldate,p0=pxsabuy,p1=pxsasell)]
x2


f250727b(x2[313:313,.(t0=buydate,t1=selldate,p0=pxsabuy,p1=pxsasell)],aestdt2(resS)[[2]])

x2[1:10,.(t0=buydate,t1=selldate,p0=pxsabuy,p1=pxsasell)]




x1[x2]
x1[idhash=='b4826ec2462efcc7']
sal[idhash=='b4826ec2462efcc7']

x2
#read rip, cowrite full strict BMN array 'NOW\\ver004\\08BMN'
#f250727a <- 
#  function(  #read rip, cowrite BMN
    nxx=1#,
    geo=geo0#,  #104 areas
    dfn=x101#, #annual
    steprip=c(steprip='NOW\\ver001\\03rip')#,
    #steppra=c(steppra='ver004\\08bmn')#, output
    #bmn=T #BMN in levels means difference in reverse time and substitute diffdays with sign(diffdays), 
#  ) {
    x0 <-  #rip
      coread(
        rcx=geo[nx==nxx][,rc9],
        step=steprip,
        colClasses=list(numeric=c('retsa'),Date=c('buydate','selldate'))
      )
    pra <-
      f230508a(
        dfn=dfn, 
        x0=x0
      )
    #if(bmn) {
      x1 <- #3 non-date columns
        pra[,!grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}",names(pra)),with=F]
      x2 <- #date columns
        pra[,grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}",names(pra)),with=F]
      x3 <- as.matrix(x2)
      x3[] <- as.integer(x3>0)
      x4 <- cbind(x3[,1]*0,x3,x3[,1]*0)
      x5 <- -t(apply(x4,1,diff)) #flip sign
      
      range(apply(x5,1,sum))
      dim(x5)


 #repeat stepprav3 as scripted in pva, but without deleting first column
  #prepend a zero column and diff row-wise, flip sign
  #this is the bmn -1/0/1/ dummy matrix including the first column
  
  #use code below using melt to assign prices in place of dummies
  #test it
  #cowrite it
  #regress it IV
  #test the IV power
  






# f250727a <- #like sip but without m2, all sales inc new build
#   function( 
#     nn='f220822ad',
#     base='NOW',
#     rc6exc=f221007a(),
#     rc3inc=f230306a(),
#     nac=fread(file.path('.',base,'ver001','01nac','all.csv',fsep='\\')),
#     stepout=file.path('.',base,'ver001','10sal',fsep='\\'),
#     hashlen=16,
#     f220822ad=getlast('f220822ad'),
#     sfn=f220822ad 
#   ){
#     print('SAL(NAC) : all price seas adj')
#     x8 <- nac%>% 
#       .[idx=='0ok',.(
#         idhash,
#         pxraw=as.numeric(price_paid),
#         selldate=deed_date,
#         mo=substr(deed_date,6,7),
#         rc3=substr(rc9,1,3),
#         rc9
#       )]%>%
#       .[rc3%in%f230306a()]
#     x9 <- 
#       sfn%>% #sfn: seasonal
#       .[,.(rc3=rc,sell,sellseas)]%>%
#       unique(.)%>%
#       .[x8, #nac: sale
#         on=c(rc3='rc3',sell='mo') 
#       ]%>%
#       .[rc3%in%rc3inc]%>%
#       .[,.(
#         idhash,
#         rc9,
#         selldate,
#         pxraw,
#         pxsa=round(exp(log(pxraw)-sellseas)) #seas adj
#       )]
#     x9 #ready to cowrite
#     cowrite(x9,stepout,key='rc9',format='csv') #45s
#   }
f250727a(nac=fread(file.path('.',base,'ver001','01nac','AL-1--1--.csv',fsep='\\')))

  stepout

  #the above with rip gives idhash.selldate, pxsabuy, pxsasell  
  

#-----------------------junk  
#1 binary +/-1 'levels' bmn pra with all columns -> coco keyed on idhash.selldate
#2 rip has buydate and selldate for idhash.selldate
#3 sip has pxsa for idhash.selldate
#4 hence idhash.selldate, pxsabuy, pxsasell 
#5 then use logic below based on melt of pra to assign full buy, sell price matrix 1995->2024 ready for CS
#6 regress final column on the rest using IV
#7 

#sales are special they are 'those with m2'
#need another one which is all sales, no m2

coread(rcx,stepsip)[coread(rcx,steprip)[,.(idhash=substr(idhash.selldate,1,16),selldate)],on=c(idhash='idhash',selldate='selldate')]

stepprav3
rcx <- 'AL-1--1--'
coread(rcx,stepprav3)
x0 <- coread(rcx,stepnac)
x1 <- coread(rcx,stepsip)[,idhash.selldate:=paste0(idhash,'.',selldate)]
x2 <- coread(rcx,steprip)[,.(buydate,selldate,idhash.selldate,idhash=substr(idhash.selldate,1,16))]
x3 <- x1[x2,on=c(idhash='idhash',selldate='selldate')][,.(idhash.selldate,idhash,rc9,buydate,selldate,sellpxsa=pxsa)][is.na(sellpxsa)]
x3[,.(idhash)]%>%
  x0[.,on=c(idhash='idhash')]
  #x1[.,on=c(idhash.selldate='idhash.selldate')]
#  x2[.,on=c(idhash.selldate='idhash.selldate')]

#f250725a <- 
#  function(  #read rip, cowrite pra
    nxx=1#,
    geo=geoa#,
    dfn=x101#,
    steprip=c(steprip='NOW\\ver001\\03rip')#,
    #steppra=c(steppra='ver001\\07pra')#, output
    bmn=T #BMN in levels means difference in reverse time and substitute diffdays with sign(diffdays), zap year1=1995 which always has zero.  equivalent used here is to assign maxdays to each non-zero period
#  ) {
    x0 <- 
      coread(
        rcx=geo[nx==nxx][,rc9],
        step=steprip,
        colClasses=list(numeric=c('retsa'),Date=c('buydate','selldate'))
      )
    pra <-
      f230508a(
        dfn=dfn, 
        x0=x0
      )
    #if(bmn) {
      x1 <- #3 non-date columns
        pra[,!grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}",names(pra)),with=F]
      x2 <- #date columns
        pra[,grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}",names(pra)),with=F]
      x3 <- as.matrix(x2)
      x3[] <- as.integer(x3>0)
      x4 <- cbind(x3[,1]*0,x3)
      x5 <- -t(apply(x4,1,diff)) #flip sign
      dim(x5)
      dim(x3)
      cbind(x3[,1],x5[,1])
      x5[1:2,]
      x5[,1:2] #this now ready to add prices as in 
      #cbind selldate back on
      #join with buy
      
      dd <- as.integer(diff(dfn))
      x3 <- copy(x2)
      
      
      #for (i in seq_along(x2)) {
      #  x3[[i]] <- as.integer(x2[[i]]>0)#*dd[i] #scaling by days is tampering with the BMN spec
      #}
      #x4 <- x3[,-1] #although tempting to leave this in, we have just biased 'days' up.  in proper BMN dummies are row-wise differenced, so when cumulated this becomes zero and must be removed
      x2
      x5 <- as.data.table(c(x1[,1],x4,x1[,-1]))
      stopifnot(all.equal(names(pra)[-2],names(x5)))
      stopifnot(all.equal(dim(pra)-c(0,1),dim(x5)))
      pra <- x5
    #}
    print(paste0('pra done, start cowrite to...',steppra))
#    cowrite(x=pra,dirnam=steppra,key='rc9',format='csv',newdir=F)
    geo[nx==nxx,rc9]%>% 
      `%in%`(.,unique(substr(dir(steppra),1,nchar(geo[1,rc9]))))%>%
      all(.)%>%
      stopifnot(.)
    print(paste0('cowrite done: ', Sys.time()))
#  }






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
    steppra1='C:\\temp\\test'
    #steppra1='now\\ver004\\07pra'  #bmn    index annual
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

