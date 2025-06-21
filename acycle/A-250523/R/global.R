app_dirG <- normalizePath("c:/users/giles/anest.repo/anest.shiny/acycle/A-250523")
setwd(app_dirG)#does nothing in the app, is here for interactive/debug
#app_dirG <- normalizePath(".") 
data_dirG <- file.path(app_dirG, "data")
print(paste0('app_dirG : ',app_dirG))
print(paste0('data_dirG : ',data_dirG))

#coltab <- data.table(light=cobalt(light=T)[c(4,3,2,2,1,5)],dark=cobalt(light=F)[c(4,3,2,2,1,5)],code=c('1.3','1.2','1.1','2.3','2.2','3.3'))

#gen1
f241021ad <- readRDS(file = file.path(app_dirG, "data", "f241021ad.rds"))
f250519ad <- readRDS(file = file.path(app_dirG, "data", "f250519ad.rds"))
pxosrdo2dd <- readRDS(file = file.path(app_dirG, "data", "pxosrdo2dd.rds"))
f241229bd <- readRDS(file = file.path(app_dirG, "data", "f241229bd.rds"))
x101 <- readRDS(file = file.path(app_dirG, "data", "x101.rds")) #date

#gen1.5
f250509ed <- readRDS(file = file.path(app_dirG, "data", "f250509ed.rds")) #res
z110 <- readRDS(file = file.path(app_dirG, "data", "z110.rds"))

#gen2
f250618ad <- readRDS(file = file.path(app_dirG, "data", "f250618ad.rds")) #res
res <- f250618ad
#gen2 gobal objects - no, unpack last
# geog2 <- f250619a('geo')
# rsig2 <- f250619a('rsi')
# dddg2 <- f250619a('ddd')
# ksrg2 <- f250619a('ksr')
# ksng2 <- f250619a('ksn')
# pvag2 <- f250619a('pva')

