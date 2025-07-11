C211a <- #---summary called in all listings ----
function(
    statics=c('resS','salS'),
    estdtlx = estdtlG, #l=aestdt1(areso(rc6tx)) c=aestdt1(rescxG)
    geoqx = geoqG, #l=ageo(areso(rc6tx)) c=ageo(rescxG)
    salx = salS
) {
  if (verbose) print("enter R211")
  x1 <-
    salx %>%
    .[geoqx[, .(rc6, lab, nx)], on = c(rc6 = "rc6")] %>%
    .[,rbind(data.table(date=resS$da0,cum=0),.SD),.(nh,rc6,lab,nx)]%>%
    .[, .(cum = sum(cum)), .(nx, lab, nh, date)] %>%
    dcast(., date + nx + lab ~ nh, value.var = "cum") %>% #
    .[order(lab,date), .(date, NF, NH, UF, UH),.(lab,nx)]
  if(F) {x1[,.(tot=sum(NF+UF+UH+NH)),date][,.(days=as.integer(diff(date)),N=diff(tot),rate=diff(tot)/as.integer(diff(date)))]%>%.[,barplot(rate)]}
  x2 <-
    estdtlx %>%
    x1[., on = c(date = "date",nx='nx')] %>%
    .[, .(ii,
          date, days, xdot, x,
          NF = c(0, diff(NF)),
          NH = c(0, diff(NH)),
          UF = c(0, diff(UF)),
          UH = c(0, diff(UH)),
          tot = c(0, diff(NF + NH + UF + UH))
    ),nx] %>%
    .[-1, .(
      nx,
      ii,
      date,
      days,
      yrs=round(days/365.25,1),
      return = round(xdot, sf),
      cumreturn = round(x, sf),
      newhouse = round(NH / tot, sf),
      usedhouse = round(UH / tot, sf),
      newflat = round(NF / tot, sf),
      usedflat = round(UF / tot, sf),
      total = round(tot),
      perday = round(tot / days, 1)
    )
    ]
  x2
}
# 
# D211a <- #---summary called in both listings ----
# function(
#     statics=c('resS','salS'),
#     resx=aresn(resS,nx=resS$lab[grep(rc6tx,lab),nx]),
#     estdtlx = aestdt1(resx), #l=aestdt1(areso(rc6tx)) c=aestdt1(rescxG)
#     geoqx = ageo(resx),#geoqG, #l=ageo(areso(rc6tx)) c=ageo(rescxG)
#     dfnyx = aestdt2(resx)$BA,#dfnyG, #l=aestdt2(resS)%>%Reduce(c,.)%>%sort(.)%>%unique(.) c= same = aestdt2(resS)%>%Reduce(c,.)%>%sort(.)%>%unique(.)
#     typex = typeC, #l='L' c='C' for footnote
#     rc6tx = rc6tG,
#     salx = salS,
#     jlist=list(
#       return = "return",
#       #date = "end date",
#       cumreturn = "cumulative",
#       newhouse = "new house",
#       usedhouse = "used house",
#       newflat = "new flat",
#       usedflat = "used flat",
#       perday = "per day",
#       total = "total"),
#     x2=C211a(
#       statics=statics,
#       estdtlx = estdtlx, #l=aestdt1(areso(rc6tx)) c=aestdt1(rescxG)
#       geoqx = ageo(resx),#geoqx, #l=ageo(areso(rc6tx)) c=ageo(rescxG)
#       salx = salx
#     ),
#     footadd=T,
#     headadd=T
# ) {
#   x2 <- x2[,names(jlist),with=F]
#   jlist2 <- jlist[which(names(jlist)%in%names(x2))]
#   print(geoqx)
#   print(resx)
#   x3 <- # districts footnote
#     geoqx[
#       , paste0("Districts: ", paste0(sort(irregpcode(rc6)), collapse = ", "))
#     ]
#   x <-
#     gt::gt(x2) %>%
#     cols_label(
#       .list=jlist2,
#       .fn=gt::html
#     )
#   if(all(c('return','cumreturn')%in%names(jlist))){
#     x <- x%>%tab_spanner(
#       label = gt::html("Log price"),
#       columns = c(return, cumreturn)
#     )
#   }
#   if(all(c('newhouse', 'usedhouse', 'newflat', 'usedflat')%in%names(jlist))){
#     x <- x%>%
#       tab_spanner(
#         label = gt::html("Fraction"),
#         columns = c(newhouse, usedhouse, newflat, usedflat)
#       )
#   }
#   if(all(c('total', 'perday')%in%names(jlist))){
#     x <- x%>%tab_spanner(
#       label = gt::html("Count"),
#       columns = c(total, perday)
#     )
#   }
#   if(all(c('newhouse', 'usedhouse', 'newflat', 'usedflat', 'total', 'perday')%in%names(jlist))){
#     x <- x%>%tab_spanner(
#       label = gt::html("Sales Breakdown"),
#       columns = c(newhouse, usedhouse, newflat, usedflat, total, perday)
#     )
#   }
#   #print(jlist)
#   if(all(c('date','days','yrs','ii')%in%names(jlist))) {
#     print('date, days spanner')
#     x <-
#       x%>%
#       tab_spanner(
#         label = gt::html("Period"),
#         columns = c(ii, date, days, yrs)
#       )
# 
#   }
#   if(footadd){
#     x <- x%>%
#       gt::tab_footnote(
#       footnote = f241108a(typex, tbinC)[[1]]
#     )
#   }
#   if(headadd){
#     x <- x%>%
#     gt::tab_header(
#       title = x3
#     )
#   }
#   x
# }
# 
# 
# D211b <- function() {
#   list(
#     date=D211a(
#       res = areso(rc6tx),
#       jlist = list(
#         date = "date",
#         ii = "t",
#         days = "days",
#         yrs = "years"
#       ),
#       footadd=F,
#       headadd=F
#     ),
#     local=D211a(res=areso(rc6tx)),
#     custom=D211a(res=rescxG)
#   )
# }