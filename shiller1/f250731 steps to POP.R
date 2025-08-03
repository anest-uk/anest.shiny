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
