#requires first slow preamble and header, then dbg
#get data
nn <- c('f241021ad','f250519ad','f250509ed','f250618ad','f250706bd','f250713ad','f250723ad','f250924ad')
rmifgl(nn)
pxosrdo2dd <- getlast('pxosrdo2dd') #district
load('data/pxosrdo1d.Rdata')
pxosrdo1dd <- x[['a']] #area
f241229bd <- getlast('f241229bd')
stopifnot(all(sapply(c('z110','x101'),exists)))
getgd(nn)

#save data
base_dir <- normalizePath(file.path(get_script_dir(), ".."))  # app folder
saveRDS(f241021ad,file=file.path(base_dir, "data", "f241021ad.rds"))
saveRDS(f250519ad,file=file.path(base_dir, "data", "f250519ad.rds"))
saveRDS(f250509ed,file=file.path(base_dir, "data", "f250509ed.rds"))
saveRDS(pxosrdo1dd,file=file.path(base_dir, "data", "pxosrdo1dd.rds"))
saveRDS(pxosrdo2dd,file=file.path(base_dir, "data", "pxosrdo2dd.rds"))
saveRDS(f241229bd,file=file.path(base_dir, "data", "f241229bd.rds"))
saveRDS(z110,file=file.path(base_dir, "data", "z110.rds"))
saveRDS(x101,file=file.path(base_dir, "data", "x101.rds"))

#gen2
#f250713aFun() #this part of dataprep in the prod environment
#saveRDS(f250618ad,file=file.path(base_dir, "data", "f250618ad.rds")) #now includes locality
saveRDS(f250723ad,file=file.path(base_dir, "data", "f250723ad.rds")) #now includes locality
saveRDS(f250706bd,file=file.path(base_dir, "data", "f250706bd.rds"))
saveRDS(f250713ad,file=file.path(base_dir, "data", "f250713ad.rds"))
#saveRDS(f250723ad,file=file.path(base_dir, "data", "f250723ad.rds"))

