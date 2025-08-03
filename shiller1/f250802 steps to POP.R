
#---------------------scripted IV estimation
nn <- 'f250731bd'
getgd(nn)
# x1 <- f250731bd
# object.size(x1)/1e9 #4.5GB
# x2 <- #select an index 'nx=6' using district rcx
#   geo1[nx==5]
# x3 <- #select from complete dt, remove intra-year
#   x1[!is.na(retsa)]
# 
# x3 <- 
#   coread(x2[,grepstring(rcx)],file.path('.','NOW','ver001','11pas',fsep='\\'))

# x3 <- coread(x2[1,rcx],file.path('.','NOW','ver001','11pas',fsep='\\'),colClasses=c(rc6='character'))

#x3 <- coread('CH-4--',file.path('.','NOW','ver001','11pas',fsep='\\'),colClasses=c(rc6='character'))
#x3
#dir(file.path('.','NOW','ver001','11pas',fsep='\\'))

# step <- file.path('.','NOW','ver001','11pas','CH-4--.csv',fsep='\\')
# fread(x0)
# anyNA(x3)
# lapply(lapply(x3,`==`,y=''),any)%>%
#   unlist(.)%>%
#   any()

geo=geo1
nxx=5
x2 <- coread(
        rcx = geo[nx == nxx][, rcx],
        step = steppas,
        colClasses = list(numeric = "retsa")
      )

x4 <- 1e-6 #scale £m to avoid numerical probs
x5 <- #xy : prices in expanded dt
  (x2[,grep('^y[0-9]{4}$',names(x1)),with=F])*x4
x5[,mean(is.na(y1996))]

#can read other pra
coread('AL-1--',stepprav2)
#pas
steppas <- "now\\ver001\\11pas"
coread('AL-1--',steppas)
coread('AL-1--',stepprav2)




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
