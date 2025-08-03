f250728aFun()  #SPP sale price pair for pop
f250731a()     #test suite data = default for 31b
f250731b()     #PAS price array sparse
f250731bFun()  #PAS step


f250731bd






getgd('f250728ad')
x1 <- f250728ad[1:1e5,.(idhash,t0,t1,p0,p1)]
x2 <- seq.Date(from=as.Date("1994-12-31"),to=as.Date("2025-12-31"),by='year')
x3 <- f250731b(x1=x1,x2=x2)
x3
