#f250731b should be cowritten like pra ready for estimation
#pra: idhash.selldate [date] rc9 retsa
x1 <- copy(f250731bd)

x1[, idhash.selldate := paste0(idhash, ".", t1)]

year_cols <- grep("^y[0-9]{4}$", names(x1), value = TRUE)
ans <- x1[, cbind(
  retsa = get(year_cols[1]),
  .SD,
  rc6,
  idhash.selldate
), .SDcols = year_cols[-1]]




year_cols <- grep("^y[0-9]{4}$", names(x1), value = TRUE)
out <- x1[, {
  out_list <- .SD
  setnames(out_list, year_cols[1], "retsa")
  out_list[, idhash.selldate := paste0(idhash, ".", t1)]
  out_list[, rc6 := rc9]  # assuming rc9 exists in your source
  out_list[, c("retsa", setdiff(year_cols, year_cols[1]), "idhash.selldate", "rc6")]
}, .SDcols = c(year_cols, "idhash", "t1", "rc9")]


#
#the first obsvn in a pair must have sign flip
f250731b()
f250731bFun()
# f250731b <-
#   function( # PAS price array sparse ----
#     x0=f250731a(),
#     x1=x0[[1]],
#     x2=x0[[2]]
#     ) {
#   x2 <- 
#     x2%>%
#     as.Date(.)%>%
#     sort(.)
#   earliest_allowed <- min(x2)
#   latest_allowed <- max(x2)
#   if (any(x1$t0 < earliest_allowed, na.rm = TRUE)) {
#     stop("Some `t0` values fall before the first bin — extend `x2` earlier")
#   }
#   if (any(x1$t1 > latest_allowed, na.rm = TRUE)) {
#     stop("Some `t1` values fall after the last bin — extend `x2` later")
#   }
#   x1[, row_id := .I] # persist original row_id
# 
#   x3 <- #'buy' interval/bin
#     findInterval(x1$t0, x2, rightmost.closed = TRUE, left.open = TRUE)
#   x4 <- #'sell' interval/bin
#     findInterval(x1$t1, x2, rightmost.closed = TRUE, left.open = TRUE)
#   x5 <- #rows where sales are in different bins
#     which(x3 != x4)
#   if (length(x5) == 0L) { # no row spans bins -> stop
#     return(data.table())
#   }
#   x6 <- #exclude non-span rows from raw table
#     x1[x5]
#   x7 <- #exclude non-span rows from 'buy' bin
#     x3[x5]
#   x8 <-  #exclude non-span rows from 'sell' bin
#     x4[x5]
#   x9 <- #all bin dates ???
#     seq_len(length(x2) - 1) 
#   x10 <- #
#     rbind(
#     data.table(
#       row_id = x6$row_id, 
#       interval = x7, 
#       value = x6$p0*ifelse(x7==1,1,-1)#interval!=1, flip sign of p0
#       ), 
#     data.table(row_id = x6$row_id, interval = x8, value = x6$p1)
#   )[interval > 0 & interval <= max(x9)]
#   x11 <- #regular grid
#     CJ(row_id = x6$row_id, interval = x9)
#   x12 <- #assign price to grid
#     x11 %>%
#     .[x10, on = .(row_id, interval)] %>% 
#     .[is.na(value), value := 0] #set NA->0
#   x13 <- #cast to grid
#     dcast(
#       x12, 
#       row_id ~ interval, 
#       value.var = "value", 
#       fill = 0
#       )
#   x14 <- #populated bin numbers as character
#     intersect(as.character(x9), names(x13))
#   x15 <- 
#     x2[-1]%>% # Date : drop t=0
#     .[as.integer(x14)]%>% #Date of populated bins
#     sort(.)%>% #ordered Date
#     as.character(.)%>% 
#     substr(.,1,4)%>% #year
#     paste0('y',.) #prepend with alpha character y
#   x16 <- 
#     setnames(x13,
#     old = x14,
#     new = x15
#   )
#   x16[x1, on = .(row_id)][, !"row_id"]
# }

#---------------------scripted IV estimation
nn <- 'f250731bd'
getgd(nn)
x1 <- f250731bd
object.size(x1)/1e9 #4.5GB
x1%>%#check NA rows are intra-year (2% of non-NA)
  .[is.na(y1995)]%>%#check NA rows are intra-year
  .[,.(y0=substr(t0,1,4),y1=substr(t1,1,4))]%>%#check NA rows are intra-year
  .[y1!=y0]
x1%>%#check non-NA rows are inter-year
  .[!is.na(y1995)]%>%#check NA rows are intra-year
  .[,.(y0=substr(t0,1,4),y1=substr(t1,1,4))]%>%#check NA rows are intra-year
  .[y1==y0]
x11 <- x1[idhash==x1[,.N,idhash][order(-N)][1,idhash]]
x11
#try a ssolution
#select a geo and g
x2 <- #select an index 'nx=6' using district rcx
  geo1[nx==5]
x3 <- #select from complete dt, remove intra-year
  x1[!is.na(y1995)]
x4 <- 1e-6 #scale £m to avoid numerical probs
x5 <- #xy : prices in expanded dt
  (x3[x2,on=c(rc6='rcx')][,grep('^y[0-9]{4}$',names(x1)),with=F])*x4
x6 <- #x
  x5[,-1]%>%  #years 1996:2024
  as.matrix(.)
x7 <- #y #select first col=1994 as dep. vbl. y
  x5[,1,drop=F]%>%
  as.matrix(.)
x8 <- #z dummy instruments matrix
  (x6 != 0) * 1L
x8[1:3,]
x6[1:3,]
x9 <- solve(t(x8) %*% x6) %*% (t(x8) %*% x7)
plot(log(-1/x9)) #
x9

x5[y1996<0] #
x5[y1996>0][y1995<0] #none 
x5[y1996>0,mean(y1996/y1995)]
x5[y1996<0][y1997>0][,median(-y1997/y1996)]
x5[y1997<0][y1998>0][,median(-y1998/y1997)]
x5[y1996<0][y1997>0][,summary(-y1997/y1996)]
x5[y1997<0][y1998>0][,summary(-y1998/y1997)]
#do I save a set of trimmed ids?

#select a geo and g
x2 <- #select an index 'nx=6' using district rcx
   geo1[nx==10]
x3 <- #select from complete dt, remove intra-year
  x1[!is.na(y1995)]
x4 <- 1e-6 #scale £m to avoid numerical probs
x5 <- #xy : prices in expanded dt
  (x3[x2,on=c(rc6='rcx')][,grep('^y[0-9]{4}$',names(x1)),with=F])*x4
x6 <- #x
  x5[,-1]%>%  #years 1996:2024
  as.matrix(.)
x7 <- #y #select first col=1994 as dep. vbl. y
  x5[,1,drop=F]%>%
  as.matrix(.)
x8 <- #z dummy instruments matrix
  (x6 > 0) * 1L - (x6 < 0) * 1L
x9 <- solve(t(x8) %*% x6) %*% (t(x8) %*% x7)
plot(log(1/x9)) #
x9



#clean up
#can put into form for 'general estimator' used in BMN and 
#exclude outliers
#do it kfold

#--------------------junk from yest
f250727aFun()  #ASC 1 all sale : coco (could be step but next is //) prep for SPP
f250728aFun()  #SPP 2 sale price pair : prep for PAS
f250731a()     #test suite data = default for 31b
f250731b()     #PAS worker
f250731bFun()  #PAS 3 Price Array Sparse : step 1min



#select a geo and g
geo1
geo <- geo1[nx==9]
x1 <- #all
  f250731bd[!is.na(y1995)]
x0 <- 1e-5
x2 <- #xy
  (x1[geo,on=c(rc6='rcx')][,grep('^y',names(x1)),with=F][,1:30])*x0
x3 <- #x
  x2[,1:29]%>%
  as.matrix(.)%>%
  .[,-1] #remove first col=1995
x4 <- #y
  x2[,30]%>%
  as.matrix(.)
x5 <- #z
  (x3 != 0) * 1L

#don't know why this is not sensible 
#check whether first col is dropped in case-shiller - maybe with IV it can be left in?

#beta_hat <- solve(t(zmat) %*% xmat) %*% (t(zmat) %*% ymat)
x6 <- solve(t(x5) %*% x3) %*% (t(x5) %*% x4)
x6
plot(1/x6[,'y2024'])




x7 <- solve(t(x3) %*% x3) %*% (t(x3) %*% x4) #ols makes no sense either
plot(1/x7[,'y2024'])
det(t(x3) %*% x3)

#maybe it's outliers?  check fit, remove 10%?  or just drop long holdpers?


x6
plot(x6)
x5
x3
x4

x1
