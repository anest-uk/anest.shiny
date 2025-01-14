# geo0G
# estdtlG
# estdtcuG
# estdtaG
# estdtxG
geo0G
estdtlG
estdtcuG
estdtaG
estdtxG
x1[,nx]
ngrp <- function(x=x1[,nx],ndigit=5) {x%/%(10^ndigit)} 
#hypoth: only need to update 'globals'
#R        
x00R  #overcomplicated so replace -> x01
x00G

# geoplusR #abolished - only used in geo00 so substitute and remove
# geoplusG

estdtR #replace
rssR #replace
pxosrdo2ddR #ok
z110R #ok
x101R #ok
geo0R #see notes and below
dfnR #not used
dfnxR #replace
dfnxxR #replace - it is used see rsicuR


nn=c('f250111bd')
getgd(nn)

#replace geo0G[,-'gx'][order(lab,rc6)]
x0 <- copy(f250111bd)
x1 <- x0$geo%>%
  .[,.(nx,lab,rc3=substr(rc9,1,3),rc6=rc9)]%>%
  .[ngrp(nx)==4]%>%
  .[,.(nx,lab,rc3,rc6,qtile=as.numeric(substr(lab,4,4)))]%>%
  .[order(lab,rc6)]
x1
geo0G[,-'gx']

#replace estdtcuG[order(lab,rc6)]
nxx=nxqG[,nx]

x0$estdt[ngrp(nx)==4][1:4][,.(nx,date,ii,lab,rc3=substr(lab,1,3),qtile=as.numeric(substr(lab,4,4)),xdotd,days,xdot,x)][nx==nxx]

estdtaG
estdtxG
