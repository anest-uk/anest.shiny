#######################transferred to dx-lib

# D211x <- #---summary called in both listings ----
#    function(
#     statics=c('resS','salS'),
#     estdtlx = estdtlG, #l=aestdt1(areso(rc6tx)) c=aestdt1(rescxG)
#     geoqx = geoqG, #l=ageo(areso(rc6tx)) c=ageo(rescxG)
#     dfnyx = dfnyG, #l=aestdt2(resS)%>%Reduce(c,.)%>%sort(.)%>%unique(.) c= same = aestdt2(resS)%>%Reduce(c,.)%>%sort(.)%>%unique(.)
#     typex = typeC, #l='L' c='C' for footnote
#     rc6tx = rc6tG,
#     salx = salS
#    ) {
#     if (verbose) print("enter R211")
#     #fpx <- file.path(data_dirG, "f241122ad.csv")
#     #print(paste0("filepath : ", fpx))
#     x1 <- 
#       salx %>%
#       .[geoqx[, .(rc6, lab, nx)], on = c(rc6 = "rc6")] %>%
#       .[,rbind(data.table(date=resS$da0,cum=0),.SD),.(nh,rc6,lab,nx)]%>%
#       .[, .(cum = sum(cum)), .(nx, lab, nh, date)] %>%
#       dcast(., date + nx + lab ~ nh, value.var = "cum") %>% #
#       .[order(lab,date), .(date, NF, NH, UF, UH),.(lab,nx)]
#      
#      
#      
#     # x1 <-
#     #   fread(fpx) %>%
#     #   .[geoqx[, .(rc6, lab, nx)], on = c(rc6 = "rc6")] %>%
#     #   .[,rbind(data.table(date=resS$da0,cum=0),.SD),.(nh,rc6,lab,nx)]%>%
#     #   .[, .(cum = sum(cum)), .(nx, lab, nh, date)] %>%
#     #   dcast(., date + nx + lab ~ nh, value.var = "cum") %>% #
#     #   .[order(lab,date), .(date, NF, NH, UF, UH),.(lab,nx)]
#     if(F) {x1[,.(tot=sum(NF+UF+UH+NH)),date][,.(days=as.integer(diff(date)),N=diff(tot),rate=diff(tot)/as.integer(diff(date)))]%>%.[,barplot(rate)]}
#     x2 <-
#       estdtlx %>%
#       x1[., on = c(date = "date",nx='nx')] %>%
#       .[, .(ii,
#         date, days, xdot, x,
#         NF = c(0, diff(NF)),
#         NH = c(0, diff(NH)),
#         UF = c(0, diff(UF)),
#         UH = c(0, diff(UH)),
#         tot = c(0, diff(NF + NH + UF + UH))
#       ),nx] %>%
#       .[-1, .(
#         nx,
#         ii,
#         date,
#         days,
#         yrs=round(days/365.25,1),
#         return = round(xdot, sf),
#         cumreturn = round(x, sf),
#         newhouse = round(NH / tot, sf),
#         usedhouse = round(UH / tot, sf),
#         newflat = round(NF / tot, sf),
#         usedflat = round(UF / tot, sf),
#         total = round(tot),
#         perday = round(tot / days, 1)
#       )
#       ]
#     x3 <- # districts footnote
#       geoqx[
#         , paste0("Districts: ", paste0(sort(irregpcode(rc6)), collapse = ", "))
#       ]
#     x <-
#       gt::gt(x2) %>%
#       gt::tab_footnote(
#         footnote = f241108a(typex, tbinC)[[1]]
#       ) %>%
#       # gt::tab_footnote(
#       #   footnote = f241108a(typex, tbinC)[[2]],
#       #   locations = NULL,
#       #   placement = c("auto", "right", "left")
#       # ) %>%
#       gt::tab_header(
#         title = x3
#       ) %>%
#       cols_label(
#         date = gt::html("end date"),
#         cumreturn = gt::html("cumulative"),
#         newhouse = gt::html("new house"),
#         usedhouse = gt::html("used house"),
#         newflat = gt::html("new flat"),
#         usedflat = gt::html("used flat"),
#         perday = gt::html("per day"),
#         total = gt::html("total")
#       ) %>%
#       tab_spanner(
#         label = gt::html("Period"),
#         columns = c(date, days)
#       ) %>%
#       tab_spanner(
#         label = gt::html("Log price"),
#         columns = c(return, cumreturn)
#       ) %>%
#       tab_spanner(
#         label = gt::html("Fraction"),
#         columns = c(newhouse, usedhouse, newflat, usedflat)
#       ) %>%
#       tab_spanner(
#         label = gt::html("Count"),
#         columns = c(total, perday)
#       ) %>%
#       tab_spanner(
#         label = gt::html("Sales Breakdown"),
#         columns = c(newhouse, usedhouse, newflat, usedflat, total, perday)
#       ) %>%
#       tab_options(
#         heading.align = "left",
#         heading.title.font.size = 12
#       )
#     x
#    }
# D211x()

D211x(
    estdtlx = aestdt1(rescxG), #l=aestdt1(areso(rc6tx)) c=aestdt1(rescxG)
    geoqx = ageo(rescxG), #l=ageo(areso(rc6tx)) c=ageo(rescxG)
    dfnyx = aestdt2(resS)%>%Reduce(c,.)%>%sort(.)%>%unique(.), #l=aestdt2(resS)%>%Reduce(c,.)%>%sort(.)%>%unique(.) c= same = aestdt2(resS)%>%Reduce(c,.)%>%sort(.)%>%unique(.)
    typex = 'C', #l='L' c='C' for footnote
    rc6tx = rc6tG
   )
