# cols
# - 0 *rc6
# - 1 *trim = 0/1/5 = 3
# - 2 *timebin = drc/annual = 2
# - 3 *geo = m/r/a/l1/l2/l3/c = 7
# - 4 *method = accrue/bmn/cs = 3
# - 5 nobs
# - 6 R2 insample
# - 7 R2 outsample
# - 8 out-in = overfit 'variance'

# rows
# - 1 mkt
# - 2 reg
# - 3 nat
# - 4-6 local 1,2,3
# - 7 custom
# - 8 custom/annual
# - 9 custom/lo
# - 10 custom/hi


#incorporate f250723ad into resS
#make resS a step and hence simplify dataprep script


DD4411 <- function(
    statics='resS',
    x1=f250723ad,
    rc6tx=rc6tG
){
  x1[rc6==rc6tx]%>%
  gt::gt(.)
}
DD4411(rc6='NG-1--')
x1[b=='*',median(mse)]

# f250723aFun <- function(
#     statics='resS', #this should be a step output
#     x=resS
# ) {
#   x1 <- lapply(apva()[,rc6],f250723a) #2 mins
#   x2 <- rbindlist(x1)
#   f250723ad <<- x2
#   putt(f250723ad)
# }
#   
# f250723a <- function(
#     rc6tx="M--12-",
#     statics='resS'
# ) {
#   rc3tx <- substr(rc6tx, 1, 3)
#   res <- copy(resS)
#   x1 <- as.list(1:10)
#   x1[[1]] <- res$lab[substr(lab, 1, 1) == "M"] # 1 mkt
#   x1[[2]] <- res$lab[substr(lab, 1, 1) == "R", .(nx = unique(nx), lab)] %>%
#     res$geo[., on = c(nx = "nx")] %>%
#     .[rc6 == rc6tx, .(lab, nx)] # 2 reg
#   x1[[3]] <- res$lab[substr(lab, 1, 1) == "N", .(nx = unique(nx), lab)] %>% # 3 reg 10
#     res$geo[., on = c(nx = "nx")] %>%
#     .[rc6 == rc6tx, .(lab, nx)]
#   x1[[4]] <- res$lab[grep(paste0("L", rc3tx, "..1"), lab)] # 4 local q.1
#   x1[[5]] <- res$lab[grep(paste0("L", rc3tx, "..2"), lab)] %>% # 5 local q.2
#     res$geo[., on = c(nx = "nx")] %>%
#     .[rc6 == rc6tx, .(lab, nx)]
#   x1[[6]] <- res$lab[grep(paste0("L", rc3tx, "..3"), lab)] %>% # 6 local q.3
#     res$geo[., on = c(nx = "nx")] %>%
#     .[rc6 == rc6tx, .(lab, nx)]
#   x1[[7]] <- res$lab[grep(paste0("C", rc6tx, "BA"), lab)] # 7 custom - base
#   x1[[8]] <- res$lab[grep(paste0("C", rc6tx, "AN"), lab)] # 8 custom - annual
#   x1[[9]] <- res$lab[grep(paste0("C", rc6tx, "T0"), lab)] # 9 custom - trim-lo
#   x1[[10]] <- res$lab[grep(paste0("C", rc6tx, "T5"), lab)] # 10 custom - trim-hi
#   x2 <- c("market", "region", "national", "local-1bin", "local-2bin", "local-3bin", "custom-basecase", "custom-annual", "custom-alloutlier", "custom-50%outlier")
#   x3 <-
#     rbindlist(x1) %>%
#     .[, b := ifelse(grepl("C......BA", lab), "*", "")] %>%
#     .[, rc6 := rc6tx] %>%
#     .[, trim := c(rep("1", 8), "0", "5")] %>%
#     .[, time := c(rep("drc", 7), "ann", rep("drc", 2))] %>%
#     .[, geo.variant := x2] %>%
#     .[, method := "acc"] %>%
#     akss()$rc6[., on = c(nx = "nx", rc6 = "rc6")] %>% # now just calc r2 and/or mse from ss,n
#     .[, .(
#       b,
#       rc6,
#       n,
#       trim,
#       time,
#       method,
#       geo.variant,
#       rsqi = round(1 - ssrk / ssti, 4),
#       mse = round(sqrt(ssrk / n), 4),
#       insam = round(sqrt(ssri / n), 4)
#     )] %>%
#     .[, over := round((mse - insam), 4)] %>%
#     .[order(mse)] %>%
#     .[, bias := mse - mse[b == "*"]] %>%
#     .[, total := bias + over] %>%
#     .[order(mse)] %>%
#     .[]
#   x3
# }

jres$lab[substr(lab, 1, 1) == "L"] # 3 local 10
res$lab[grep("L...1.1BA", lab)] # 3 local 10
res$lab[grep("C......BA", lab)] # 3 nat 10
res$lab[, .N, substr(lab, 1, 1)]

rc6tx <- rc6tG
resS$kss

akss()$rc6[rc6 == rc6tx]
akss()
