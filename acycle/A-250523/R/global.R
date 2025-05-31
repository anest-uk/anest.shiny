app_dirG <- normalizePath(".") 
data_dirG <- file.path(app_dirG, "data")
print(paste0('app_dirG : ',app_dirG))
print(paste0('data_dirG : ',data_dirG))

f250509ed <- readRDS(file = file.path(app_dirG, "data", "f250509ed.rds"))
f241021ad <- readRDS(file = file.path(app_dirG, "data", "f241021ad.rds"))
f250519ad <- readRDS(file = file.path(app_dirG, "data", "f250519ad.rds"))
pxosrdo2dd <- readRDS(file = file.path(app_dirG, "data", "pxosrdo2dd.rds"))
f241229bd <- readRDS(file = file.path(app_dirG, "data", "f241229bd.rds"))
z110 <- readRDS(file = file.path(app_dirG, "data", "z110.rds"))
x101 <- readRDS(file = file.path(app_dirG, "data", "x101.rds"))

