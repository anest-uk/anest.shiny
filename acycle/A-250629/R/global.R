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

#gen1.5 <<<<<<<<<<<get rid
f250509ed <- readRDS(file = file.path(app_dirG, "data", "f250509ed.rds")) #res
z110 <- readRDS(file = file.path(app_dirG, "data", "z110.rds"))


res <- readRDS(file = file.path(app_dirG, "data", "f250618ad.rds")) #res
#res <- data.table::copy(f250618ad) # need data.table::copy

#these 2 functions load static into res - they can be run in ff250618a, therefore they are denorm 
# f250624a <- # 2274 x 3 minimisedkss, for all rc6 ----
#   function(
#     r=res#,
#     ) {
#     x3 <-
#       r$geo %>%
#       .[r$lab,on=c(nx='nx')] %>%
#       .[grep("^L", lab)] %>% # local only
#       r$kss[., on = c(nx = "nx", rc6 = "rc6"), nomatch = NULL] %>%
#       .[order(rc6, ssrk), .SD[1], rc6] %>%
#       .[, .(rc6, nx, i.n = substr(lab, 5, 7))] %>%
#       .[r$pva, on = c(rc6 = "rc6"), nomatch = NULL] %>%
#       .[
#         ,
#         .(
#           rc6, # *
#           i.n,
#           nx
#         )
#       ]
#     x3
#   }
# #f250624a()
# 
# f250624b <- # 208 x 4 extremal nx and ppm2 for all rc3
#   function( # res = f250618ad
#     r=res,
#     x0=f250624a()
#     ) {
#     x1 <-
#       r$geo %>%
#       .[x0[,.(nx)],on=c(nx='nx')]%>%
#       .[r$lab, on = c(nx = "nx")] %>% # to fileter on local
#       .[grep("^L", lab)] %>% # local
#       .[r$pva, on = c(rc6 = "rc6")] %>% # pva
#       .[, .(rc3 = substr(rc6[1], 1, 3), ppm2 = sum(pv) / sum(m2), ppm2rc6min = min(pv / m2), ppm2rc6max = max(pv / m2),i.n=substr(lab,5,7)), nx] %>% # aggregate
#       .[order(rc3, ppm2)] %>% # order
#       .[
#         ,
#         .SD[
#           c(1, .N),
#           .(
#             nx,
#             type = c("minP", "maxP"),
#             i.n,
#             ppm2agg = ppm2, # nx : aggregate
#             ppm2rc6min, # nx : lowest ppm2(rc6)
#             ppm2rc6max # nx : lowest ppm2(rc6)
#           )
#         ],
#         rc3 # per rc3
#       ]
#     x1
#   }
# #f250624b()
# 
# 
# res$f250624ad <- f250624a()
# res$f250624bd <- f250624b()
