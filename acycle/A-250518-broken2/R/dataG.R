getgd('f250509ed')
pxosrdo2dd <- getlast('pxosrdo2dd')
z110
 # x0=f250509ed,  #SOT
 #    x1=pxosrdo2dd,
 #    rc6all=z110[nchar(rcx)==6,][grep(rc3x,rcx)][rcx!=rc6x][1,c(rcx,rc6x)]
dataG <- list(f250509ed=f250509ed,pxosrdo2dd=pxosrdo2dd,z110=z110)
save(dataG,file='dataG.Rdata')
