f250727aFun() #ASC  slow cowrite
f250728aFun() #SPP  slow // step






#-----------testing of f250727b
myfun <- f250727b
f250728b
f250727b()
f250728ad

#do a nice test table and nice dates
tt <- seq.Date(from=as.Date("1994-12-31"),to=as.Date("1996-12-31"),by='year')
t0 <- tt[1]+11:15
t1 <- tt[2]+11:15
p0 <- 1:5
p1 <- 11:15
idhash <- c('a','b','b','b','c')
xx <- data.table(t0,t1,p0,p1,idhash)

xx2 <- copy(xx)
xx2[3,t1:=t0+5] #now both dates fall in first date interval, 1995


x11 <- myfun(x1=xx,x2=tt)%>%setkey(.,idhash,t0,t1)
x12 <- myfun(x1=xx2,x2=tt)%>%setkey(.,idhash,t0,t1) #generates warning and incorrect result

#dropped_ids <- setdiff(xx2$id, myfun(xx2, tt)$id) #not helpful, id is not key
x11[!x12]

#--------------------------junk/broken
myfun <- 
  function(
    x1=data.table(t0=as.Date("1996-05-11"),t1=as.Date("1996-05-12"),p0=1000,p1=2000),
    x2=as.Date(c("1994-12-31", "1996-05-12", "1996-12-07", "1997-05-30", "1997-10-21"))
  ) {
    x4 <-
      1:(length(x2) - 1)
    x5 <- # Find intervals (left-open, right-closed)
      findInterval(x1$t0, x2, rightmost.closed = TRUE, left.open = TRUE)
    x6 <-
      findInterval(x1$t1, x2, rightmost.closed = TRUE, left.open = TRUE)
    x7 <- # Build long format
      rbind(
        data.table(row_id = seq_len(nrow(x1)), interval = x5, value = x1$p0),
        data.table(row_id = seq_len(nrow(x1)), interval = x6, value = x1$p1)
      )[interval > 0 & interval <= (length(x2) - 1)]
    x8 <- # Ensure all interval rows exist
      CJ(row_id = seq_len(nrow(x1)), interval = x4)
    x9 <- # place values on grid
      x7[x8, on = c(row_id = "row_id", interval = "interval")]
    lapply(lapply(x9,duplicated),any)
    x9[is.na(value), value := 0]
    x10 <- # Pivot to wide
      dcast(x9, row_id ~ interval, value.var = "value", fill = 0)
    x11 <- # rename to rh date boundary
      setnames(x10, old = as.character(x4), new = as.character(x2[-1]))
    x12 <- # join x1
      x10 %>%
      .[x1[, row_id := .I], on = c(row_id = "row_id")] %>%
      .[, -"row_id"]
    x12
  }

myfun(x1=xx,x2=tt)
myfun(x1=xx2,x2=tt) #generates warning and incorrect result





#this fails: rows 11:13 have same idhash
#row 13 has non-spanning dates
x1 <- x2[310:314,.(idhash,t0,t1,p0,p1)]
tt <- f250728b(f250723ad)[[2]]#[1:8]
f250727b(x1,tt)



f250723ad #res as a step

x2 <- f250728ad

f250727b(
  x2[310:314,.(idhash,t0,t1,p0,p1)], #SPP sale pair
  f250728b(f250723ad)[[2]] #SPE sale pair expanded
  ) #fails: both in one bin

f250727b(x2[310:314,.(t0,t1,p0,p1)],f250728b(f250723ad)[[2]]) #ok

#<<<<<<<<<<<<<<<<<<<<<here, need to fix 0727b see above


#f250727a() #prep the full 'sal' file

#is it worth breaking this join into // job?  check timings

# f250728aFun <- #SPP sale pair for pop
#   function(
#     x1=f250509cd,
#     nn='f250509cd',
#     ascpath=file.path('.','now','ver001','10asc',fsep='\\'),
#     rippath=steprip
#   ) {
#     getgd(nn)
#     x2 <- #rc3
#       x1[,sort(unique(substr(rc9,1,3)))]
#     x3 <- as.list(NULL)
#     sfInit(par=T,cpus=min(8,length(x2)))
#     x4 <- #ann-accrue
#       sfLapply(
#         x2,
#         f250728a,
#         ascpath=ascpath,
#         rippath=rippath
#       )
#     x5 <- rbindlist(x4)
#     f250728ad <<- x5
#     putt(f250728ad)
#   }
# system.time(f250728aFun())
# 
# f250728a <- #SPP worker: idhash t0 t1 p0 p1 
#   function(
#     rcx='AL-',
#     base='NOW',
#     ascpath=file.path('.',base,'ver001','10asc',fsep='\\'),
#     rippath=steprip
#   ) {
#     asc <- coread(rcx,ascpath,colClasses=c(pxsa='numeric',selldate='Date'))
#     rip <- coread(rcx,rippath,colClasses=c(buydate='Date',selldate='Date'))
#     x1 <- 
#       rip[,.(idhash=substr(idhash.selldate,1,16),buydate,selldate,retsa)]
#     x2 <- 
#       asc[,.(idhash,buydate=selldate,pxsabuy=pxsa)]%>%
#       .[x1,on=c(idhash='idhash',buydate='buydate')]%>%
#       asc[.,on=c(selldate='selldate',idhash='idhash')]%>%
#       .[,.(idhash,buydate,selldate,pxsabuy,pxsasell=pxsa)]%>%
#       .[,.(idhash,t0=buydate,t1=selldate,p0=pxsabuy,p1=pxsasell)]
#     stopifnot(
#       (!anyNA(x2)) &
#         (x2[,.N]==x1[,.N])
#     )
#     x2
#   }
f250728a()










#------junk---------------------------------------
system.time(asc <- coread('.',ascpath,colClasses=c(pxsa='numeric',selldate='Date'))) # 5 mins
system.time(rip <- coread('AL-',rippath,colClasses=c(buydate='Date',selldate='Date')))

f250727b(x2[313:313,.(t0=buydate,t1=selldate,p0=pxsabuy,p1=pxsasell)],aestdt2(resS)[[2]])

x2[1:10,.(t0=buydate,t1=selldate,p0=pxsabuy,p1=pxsasell)]





x1[x2]
x1[idhash=='b4826ec2462efcc7']
asc[idhash=='b4826ec2462efcc7']

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

