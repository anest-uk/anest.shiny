#get data
nn <- c('f241021ad','f250519ad','f250509ed','f250618ad')
pxosrdo2dd <- getlast('pxosrdo2dd')
f241229bd <- getlast('f241229bd')
stopifnot(all(sapply(c('z110','x101'),exists)))
getgd(nn)

#save data
base_dir <- normalizePath(file.path(get_script_dir(), ".."))  # app folder
saveRDS(f241021ad,file=file.path(base_dir, "data", "f241021ad.rds"))
saveRDS(f250519ad,file=file.path(base_dir, "data", "f250519ad.rds"))
saveRDS(f250509ed,file=file.path(base_dir, "data", "f250509ed.rds"))
saveRDS(pxosrdo2dd,file=file.path(base_dir, "data", "pxosrdo2dd.rds"))
saveRDS(f241229bd,file=file.path(base_dir, "data", "f241229bd.rds"))
saveRDS(z110,file=file.path(base_dir, "data", "z110.rds"))
saveRDS(x101,file=file.path(base_dir, "data", "x101.rds"))

#gen2
saveRDS(f250618ad,file=file.path(base_dir, "data", "f250618ad.rds"))
