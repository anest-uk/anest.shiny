#setwd('acycle')
source('c-cleanlib.R')
x0 <- f250111bd
geox <- x0$geo[grep('CBA-AL-1--',lab)]
dfnx <- x0$estdt[grep('^CBA',lab),sort(unique(union(as.Date('1994-12-31'),date)))]%>%as.Date(.)
x1 <- f241119a(geox[,sort(unique(nx))],geo=geox,dfn=dfnx,steprip2='/smallrip')
#with this in place:
all.equal(
x0$estdt[grep('^CBA-AL-1--',lab),xdot],
x1$estdt[,xdot]
)
all.equal(
  x0$rss[grep('^CBA-AL-1--',lab),ssek],
  x1$kfoldsse[,ssek]
)
#now just bind in any custom and that's it, bingo