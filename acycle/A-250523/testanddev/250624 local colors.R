#price quantile within local and national range, 0:1
#there is a local and national function mapping P to Q
#Q has entries for 0 and 1, for rc6 mapping
#for nx mapping Q is within this range

#rc6, national
#nx, national

#####superseded - see f250618a f250618b f250618c

f250623a <- function(
    resx=res
){
  x1 <- # national:  color from P
    resx$pva[order(pv / m2), .(P = log(pv / m2), rc6)] %>%
    .[, .(rc6, colnat = color_price(P, min(P), max(P)), Pn0 = min(P), Pn1 = max(P))]
  x2 <- # local:
    resx$pva[order(pv / m2), .(P = log(pv / m2), rc6), .(rc3 = substr(rc6, 1, 3))] %>%
    .[, .SD[, .(rc6, Pminrc3 = min(P), Pmaxrc3 = max(P), colloc = color_price(P, min(P), max(P)))], rc3] %>%
    .[, .(rc6, colloc, Pminrc3, Pmaxrc3)]
  # for all nx the national color
  x4 <-
    resx$pva[resx$geo, on = c(rc6 = "rc6")] %>%
    .[, .(P = log(sum(pv) / sum(m2))), nx] %>%
    .[, .(nx, P, colnatnx = color_price(P, x1[1, Pn0], x1[1, Pn1]))]
  # for local nx the local color
  x5 <-
    resx$pva[resx$geo, on = c(rc6 = "rc6")] %>%
    .[resx$lab[grep("^L", lab)], on = c(nx = "nx")] %>%
    .[, .(rc3 = substr(lab, 2, 4)[1], i.n = substr(lab, 5, 7)[1], P = log(sum(pv) / sum(m2))), nx] %>%
    x2[, .(Pminrc3 = Pminrc3[1], Pmaxrc3 = Pmaxrc3[1]), .(rc3 = substr(rc6, 1, 3))][., on = c(rc3 = "rc3")] %>%
    .[order(rc3, P), .(
      rc3, i.n, P, nx,
      col = color_price(P, Pminrc3, Pmaxrc3)
    )]
  x6 <-
    rbind(
      x1[, .(id = rc6, col = colnat, idtype = "rc6", ranktype = "nat")],
      x2[, .(id = rc6, col = colloc, idtype = "rc6", ranktype = "loc")],
      x4[, .(id = nx, col = colnatnx, idtype = "nx", ranktype = "nat")],
      x5[, .(id = nx, col = col, idtype = "nx", ranktype = "loc")]
    )
  x7 <-
    x6[idtype == "nx", .(nx = as.numeric(id), col, ranktype)] %>%
    .[resx$lab, on = c(nx = "nx"), nomatch = NULL] %>%
    .[order(ranktype,nx), .(nx, col, ranktype)]
  x7
}
f250623a()[res$lab,on=c(nx='nx'),nomatch=NULL][grep('^L',lab)]

#for local nx, the local ranking
  


#Q for all nx, rc6
x1 <- resx$pva[order(pv/m2),.(P=log(pv/m2),nidcum=cumsum(nid))][,y:=(nidcum-min(nidcum))/(diff(range(nidcum)))]
x2 <- x1[,approxfun(x=P,y=y)]
x3 <- copy(resx$pva)[,Qnat:=x2(log(pv/m2))]
x3[rc6=='NG-7--']
x3[rc6=='TN-10-']
x2(x1[,max(P)])

x4 <- resx$pva[order(pv/m2),.(P=log(pv/m2),nidcum=cumsum(nid)),.(rc3=substr(rc6,1,3))][,y:=(nidcum-min(nidcum))/(diff(range(nidcum))),rc3][]
x4[,approx(x=P,y=y,xout),rc3]

?approx
x3 <- copy(resx$pva)[,Qnat:=x2(log(pv/m2))]
x3[rc6=='NG-7--']
x3[rc6=='TN-10-']
x2(x1[,max(P)])





x1 <-
  rbind(
    resx$lab[grep("^L", lab)] %>%
      .[resx$geo, on = c(nx = "nx"), nomatch = NULL] %>%
      .[resx$pva, on = c(rc6 = "rc6"), nomatch = NULL] %>%
      .[, .(pv = sum(pv), m2 = sum(m2), nid = sum(nid), type = "nx"), .(id = nx)],
    resx$pva[, .(pv, m2, nid, type = "rc6", id = rc6)]
  )%>%
  .[,.(P=log(pv/m2),)]

x4 <- resx$pva[,.()]
