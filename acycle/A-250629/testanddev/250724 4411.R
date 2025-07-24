#f250723ad$rsi%>%
rc6tx='S--10-'
x1 <- 
  f250723ad$f250723a%>%
  .[grepl('local',geo)|(trim!='10%'|b=='*')]%>%
  .[rc6==rc6tx,.(nx,lab=paste0(geo,gsub('%','',trim)))]
x2 <- 
  aestdt1(f250723ad)%>%
  .[x1,on=c(nx='nx')]%>%
  dcast(.,date~lab,value.var='xdot')
x2[,-'date']%>%cor(.)
x0

rc6tx <- 'NG-1--'
rc6tx <- 'B--1--'
rc6tx <- 'S--32-'
rc6tx <- 'RG-5--'
# CC4411 <- #RSI accuracy report 
#   function(rc6tx=rc6tG){
#     x1 <- resS$f250723a%>%resS$f250713a[.,on=c(rc6='rc6')]
#     x3 <- x1[rc6==rc6tx]
#     x2 <- x1[b=='*']
#     x4 <- paste0(x1[rc6==rc6tx][1,paste0(irregpcode(rc6),' (',locality,') ')],'rms error is ',round(x2[rc6==rc6tx,rmse],4),' which is below ',round(100*mean(x2[rc6==rc6tx,rmse]<x1[,rmse])), '% of districts')
#     x5 <- paste0(x1[rc6==rc6tx][1,paste0(irregpcode(rc6),' (',locality,') ')],'R-squared is ',round(x2[rc6==rc6tx,rsqk],4),' which is above ',round(100*mean(x2[rc6==rc6tx,rsqk]>x1[,rsqk])), '% of districts')
#     list(all=x1,base=x2,irc6=x3,ijselect=x3[,c('b','geo','tbin','trim','rsqk','rmse','over')],data.table(msg=c(x4,x5)))
#   }
# CC4411('NG-7--')
#CC4111()
# 
# #now do DD4411
# DD4411 <- function(
#     rc6t="TN-10-",
#     shadecol1 = "#D3D3D3" 
#     ) {
#   x1 <- 
#     CC4411(rc6t)[["ijselect"]]%>%
#     .[, b := ifelse(b == "", "", "base case")]%>%
#     gt::gt(.) %>%
#     cols_label(
#       b = gt::html(""),
#       geo = gt::html("peer group"),
#       tbin = gt::html("time bin"),
#       trim = gt::html("outlier reject"),
#       rsqk = gt::html(
#         "<div style='text-align:center; line-height:1.2;'>
#          R<sup style='font-size:70%; position:relative; top:-0.2em;'>2</sup>
#        </div>"
#       ),
#       rmse = gt::html("RMS error"),
#       over = gt::html("overfit")
#     )%>%
#   tab_style(
#     style = cell_text(align = "center"),
#     locations = cells_body(columns = trim)
#   ) %>%
#     fmt_number(
#       columns = c(rsqk, rmse, over),
#       decimals = 4
#     ) %>%
#     tab_spanner(
#       label = gt::html("parameter"),
#       columns = c(geo, tbin, trim)
#     ) %>%
#     tab_spanner(
#       label = gt::html("accuracy metric"),
#       columns = c(rsqk, rmse, over)
#     ) %>%
#     gt::tab_style(
#       style = gt::cell_fill(color = shadecol1),
#       locations = gt::cells_body(rows = (b == "base case"))
#     )
#   x1
# }
# DD4411()
#


#next put into app
#

resS$f250713a

resS$f250723a[b=='*',mean(rsqi<x1[b=='*',rsqi])]
resS$f250723a[b=='*',median(rsqi)]
resS$f250723a[b=='*',mean(rsqi)]
# f250723aFun <- #add accuracy table f250723a to 'res'
#   function(
#     nn='f250618ad', 
#     x=f250618ad #res
# ) {
#   getgd(nn)
#   x1 <- #accuracy table for each rc6
#     lapply(apva()[,rc6],f250723a) #2 mins
#   x2 <- #accuracy table for all rc6
#     rbindlist(x1)
#   x3 <- copy(x)
#   x3[['f250723a']] <- x2
#   f250723ad <<- x3
#   putt(f250723ad)
# }
#   
# f250723a <- #accuracy table for one rc6
#   function(
#     rc6tx="M--12-",
#     x0=f250618ad
# ) {
#   rc3tx <- substr(rc6tx, 1, 3)
#   res <- copy(x0)
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
#   for(i in seq_along(x1)) x1[[i]] <- x1[[i]][,.(nx,lab)]
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
