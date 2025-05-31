f241021ad$estdt[nxaR(), on = c(nx = "nx"), .(nx, date, ii, lab, rc3, qtile, xdotd, days, xdot, x)]


geoaR()[rc6 == rc6tR()]




f250509edG nil
f241021adG common : 4x
f250519adG nil
pxosrdo2ddG common : pxosrdo2ddR() remove, in timeseries reference pxosrdo2dd
f241229bdG nil
z110G function : 3x
x101G nil


app_dirG <- file.path(normalizePath("."),"acycle","A-250523")

f250509ed <- readRDS(file = file.path(app_dirG, "data", "f250509ed.rds"))
f241021ad <- readRDS(file = file.path(app_dirG, "data", "f241021ad.rds"))
f250519ad <- readRDS(file = file.path(app_dirG, "data", "f250519ad.rds"))
pxosrdo2dd <- readRDS(file = file.path(app_dirG, "data", "pxosrdo2dd.rds"))
f241229bd <- readRDS(file = file.path(app_dirG, "data", "f241229bd.rds"))
z110 <- readRDS(file = file.path(app_dirG, "data", "z110.rds"))
x101 <- readRDS(file = file.path(app_dirG, "data", "x101.rds"))

#now can produce all page 1 displays using function lib thus:
f111D()
f112D()
f121D()
f122D()
f131D()
f132D(steprip=file.path(app_dirG, "data", "smallrip"))

#are the defaults in these functions ever used 'accidentally' i.e. the globals G instead of reactives R?  Plug this leak so 'reactive always'
#can their args be defaulted instead of 'old globals' to 'new inputs'?  This is how to do the transition.