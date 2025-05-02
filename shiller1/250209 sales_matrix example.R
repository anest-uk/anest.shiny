#I supply d = cumulative binned days
dfndrc <- gett('dfndrc')
d <- 1:30
dmat

library(MASS)  # For ginv() (generalized inverse)
library(AER)   # For ivreg() if needed

### Step 1: Initial IV Estimation (Basic 2SLS)
# Compute IV coefficients using (Z'X)^(-1) Z'Y
beta_hat <- solve(t(zmat) %*% xmat) %*% (t(zmat) %*% ymat)

# Compute residuals
residuals_iv <- ymat - xmat %*% beta_hat

plot(dmat,residuals_iv)

### Step 2: Estimating the Heteroskedasticity Function s_i
# Compute squared residuals
residuals_sq <- 
  data.table(residuals_iv^2)%>%
  as.matrix(.)

# Compute the known differences in d[j]
#d_diff <- abs(d[col(xmat)] - d[row(xmat)])  # Assuming d is indexed by columns
d_diff <- 
  data.table(dmat)%>%
  as.matrix(.)

# Fit the heteroskedasticity model: residual variance as a function of d differences
het_model <- lm(residuals_sq ~ d_diff)
summary(het_model)
a_hat <- coef(het_model)[1]
b_hat <- coef(het_model)[2]

# Compute estimated variances for each observation
s_hat <- a_hat + b_hat * d_diff
s_hat[s_hat <= 0] <- min(s_hat[s_hat > 0])  # Ensure variances are positive

### Step 3: Weighted IV Estimation (Feasible GLS IV)
# Construct weighting matrix W = diag(1 / s_hat)
W <- diag(1 / s_hat)

# Compute heteroskedasticity-corrected IV estimator
beta_fgls <- solve(t(xmat) %*% W %*% zmat) %*% (t(zmat) %*% W %*% ymat)

# Compute heteroskedasticity-robust standard errors
residuals_fgls <- ymat - xmat %*% beta_fgls
XZW <- t(xmat) %*% W %*% zmat
bread <- solve(XZW)  # (X'ZW)^(-1)
meat <- t(xmat) %*% W %*% diag(residuals_fgls^2) %*% W %*% xmat
vcov_fgls <- bread %*% meat %*% bread  # Variance-covariance matrix
se_fgls <- sqrt(diag(vcov_fgls))  # Standard errors

# Print final results
list(
  beta_initial = beta_hat,
  beta_corrected = beta_fgls,
  se_corrected = se_fgls,
  residuals_final = residuals_fgls
)


#------------taking the below as given

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
rcx='NG-7--'
#rcx='S--11-'
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
  dcast(.,idhash.selldate~variable,value.var='value')%>%
  .[order(idhash.selldate)]
x3a <- #order
  x3[x4[,.(idhash.selldate)],.(idhash.selldate,days=as.numeric(as.Date(selldate)-as.Date(buydate))),on=c(idhash.selldate='idhash.selldate'),nomatch=NULL]
stopifnot(nrow(x4)==nrow(x3a))

yy <- x4[,2,with=F]%>%setnames(.,'p0')%>%.[,.(p0=abs(as.numeric(p0)))]
xx <- x4[,-(1:2),with=F]%>%
  setnames(.,paste0('p',gsub('-','',substr(names(.),1,7))))%>%
  lapply(.,`class<-`,value='numeric')%>%
  data.frame(.)
x5 <- 
  cbind(yy,xx)
zmat <- as.matrix(xx/abs(xx))
zmat[is.nan(zmat)] <- 0
xmat <- as.matrix(xx)
ymat <- as.matrix(yy)
dmat <- as.matrix(x3a[,.(years=days/365.25)])
plot(dmat,ymat)

# beta_hat <- lm.fit(t(zmat) %*% xmat, t(zmat) %*% ymat)$coefficients
# index <- 1/beta_hat
# plot(log(index))
# plot(dmat,ymat)

