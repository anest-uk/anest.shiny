app_dirG <- normalizePath("c:/users/giles/anest.repo/anest.shiny/acycle/A-250523") #normalizePath(".")  is better, but does not work for interactive debug (2 dir up)
setwd(app_dirG)#does nothing in the app, is here for interactive/debug
data_dirG <- file.path(app_dirG, "data")
print(paste0('app_dirG : ',app_dirG))
print(paste0('data_dirG : ',data_dirG))

#gen1
f241021ad <- readRDS(file = file.path(app_dirG, "data", "f241021ad.rds"))
f250519ad <- readRDS(file = file.path(app_dirG, "data", "f250519ad.rds"))
pxosrdo2dd <- readRDS(file = file.path(app_dirG, "data", "pxosrdo2dd.rds"))
f241229bd <- readRDS(file = file.path(app_dirG, "data", "f241229bd.rds"))
x101 <- readRDS(file = file.path(app_dirG, "data", "x101.rds")) #date
z110 <- readRDS(file = file.path(app_dirG, "data", "z110.rds"))

#gen1.5 <<<<<<<<<<<get rid
#f250509ed <- readRDS(file = file.path(app_dirG, "data", "f250509ed.rds")) #res

# res <- readRDS(file = file.path(app_dirG, "data", "f250618ad.rds")) #res
# dat <- list(pva=res$pva,pol=pxosrdo2dd)

resS <- readRDS(file = file.path(app_dirG, "data", "f250618ad.rds")) #res
datS <- list(pva=resS$pva,pol=pxosrdo2dd)
