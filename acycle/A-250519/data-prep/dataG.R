pxosrdo2dd <- getlast('pxosrdo2dd')
nn <- c('f250509ed','f250519ad')
getgd(nn)
z110

dataG <- list(f250509ed=f250509ed,pxosrdo2dd=pxosrdo2dd,z110=z110,f250519ad=f250519ad)
save(dataG,file='dataG.Rdata') #move it to data/
