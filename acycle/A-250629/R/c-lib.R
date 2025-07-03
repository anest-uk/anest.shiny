# gen2 accessors

Ccus <- # RES for custom from rescG - this can go upstream into rescR and f241119a when switch to gen2
  function(
      rescx = rescG,
      pvax = apva(resS)) {
    list(
      lab = data.table(nx = 0, lab = "CU00"),
      geo = rescx$geo[, .(nx, lab, rc6 = rc9)],
      rsi = rescx$estdt[, .(
        nx, 
        date, 
        xdotd,
        xset,
        xset1
        )],
      da0 = as.Date("1994-12-31"),
      kss = rescx$kfoldsse[, .(
        nx,
        ssrk = ssek, # r k residual kfold
        ssri = ssei, # r i residual inlier
        ssra = sser, # r a residual all
        ssti = toti, # t i total    inlier
        ssta = sstr, # t a total    all
        n,
        rc6
      )],
      pva = rescx$geo[, .(rc6 = rc9)][pvax, on = c(rc6 = "rc6")]
    )
  }
# Ccus()

# C121a <- # {ii AN BA} dates ----
#   function(x0 = f250509ed$estdt) {
#     x1 <- f250509ed$estdt %>%
#       .[, .(ii = sort(unique(ii)), date = sort(unique(date))), .(tbin = substr(lab, 8, 9))] %>%
#       .[tbin %in% c("BA")] %>%
#       dcast(., ii ~ tbin, value.var = "date") %>%
#       rbind(., data.table(ii = 0, BA = as.Date("1994-12-31"))) %>%
#       .[order(BA)]
#     x1
#   }
#aestdt2() replaces this

# C121b <- # {rc6 ssek nx lab} 3 rows i.n ssek-ordered ----
#   function(x0 = f250509ed,
#            rc6tx = rc6tG) {
#     x0$geo %>%
#       .[grep("^L", lab)] %>%
#       .[rc9 == rc6tx, .(nx, lab)] %>%
#       x0$kfoldsse[., on = c(nx = "nx")] %>%
#       .[rc6 == rc6tx] %>%
#       .[order(ssek)] %>%
#       .[, .(rc6, ssek, nx, lab)]
#   }

# Ccus()$rsi %>%
#   aestdt1(.) %>%
#   C121c(x4 = .)
# 
# resS$rsi%>%
#   .[resS$f250618b[rc6tx == rc6, .(nx)], on = c(nx = "nx")] %>%
#   aestdt1(.) %>%
#   C121c(x4 = .)


C121c <- #----
  function(
      rc6tx = rc6tG,
      x1 = data.table(BA = aestdt2()$BA)[, ii := .I - 1][],
      x4=aestdt1(resS$rsi[resS$f250618b[rc6tx == rc6, .(nx)], on = c(nx = "nx")])
      ) {
    x2 <- # daily
      seq.Date(from = x1[1, BA], to = x1[.N, BA], by = "d")
    x3 <- # annual
      seq.Date(from = x1[1, BA], to = x1[.N, BA], by = "y") %>%
      .[-1] %>% # remove d0
      c(., x1[.N, BA]) %>% # add dmax
      unique(.)
    x5 <-
      x4 %>%
      .[.(date = x2), on = c(date = "date"), roll = -Inf, j = .(date, xdotd)] %>%
      .[, .(ii = 1:.N, date, x = cumsum(xdotd))] %>%
      .[.(date2 = x3), on = c(date = "date2")] %>%
      .[, .(date, x, xdot = c(x[1], diff(x)), ii = 1:.N)] %>%
      .[, .(ii, date, xdot, x)] %>%
      .[, .(date, xdot)] %>%
      .[date == as.Date("2009-02-28"), let(date, as.Date("2008-12-31"))] %>%
      .[, .(decade = substr(date, 1, 3), yr = substr(date, 4, 4), xdot = round(xdot, 3))] %>%
      dcast(., decade ~ yr, value.var = "xdot") %>%
      .[, decade := c(1990, 2000, 2010, 2020)] %>%
      setnames(., old = "decade", new = "decade\\year") %>%
      .[]
    x5
  }
#C121c(x4=aestdt1(x0$rsi))

C122a <- #i.n q2 nrc6.est nrc6.fit lab nid.est minppm2 maxppm2 aggppm2 col
  function(
    rc6tx = rc6tG,
    geocx = geocG, # is a georc9
    pvax  = apva(resS),
    geox  = ageo(resS)[grep("^L", lab)] # is a georc6
    ) {
  x1 <- resS$f250618b # not a geo
  geo <- rbind( # is georc6
    geox,
    geocx[, .(rc6 = rc9, nx, lab = paste0("C", substr(rc6tG, 1, 3), "0.0CU"))]
  )
  geo1 <- # is georc6 : this rc3 geo
    geo %>% # 1
    .[grep(substr(rc6tx, 1, 3), lab)]
  x2 <- #count per i.n : train
    geo1 %>%
    .[, .(nrc6.est = .N), .(i.n = substr(lab, 5, 7))]
  x3 <- # {rc6 i.n nx} 
    x1[grep(substr(rc6tx, 1, 3), rc6)] %>%
    rbind(., geocx[, .(rc6 = rc6tx, i.n = "0.0", nx = 0)])
  x4 <- # count per i.n : fit
    x3 %>%
    .[, .(nrc6.fit = .N), i.n] # %>%
  x5 <- # 3,4,5,6,7
    geo1 %>%
    pvax[., on = c(rc6 = "rc6")] %>%
    .[, .(nid.est = sum(nid), maxppm2 = max(ppm2), minppm2 = min(ppm2), aggppm2 = sum(pv) / sum(m2)), .(nx, lab)] %>%
    .[order(aggppm2), .(nx,lab, nid.est, minppm2, maxppm2, aggppm2, col = color_price(log(aggppm2), log(min(.[, minppm2])), log(max(.[, maxppm2]))), i.n = substr(lab, 5, 7))]
  x6 <-
    x2%>%
    .[x4, on = c(i.n = "i.n")]%>%
    .[x5, on = c(i.n = "i.n")]%>%
    C122b()[.,on=c(i.n='i.n')]
  x6[order(aggppm2),.(nx,lab,i.n,q2,nrc6.est,nrc6.fit,nid.est,minppm2,maxppm2,aggppm2,col)]
}
C122b <- #labelling
  function() {
  data.table(
    i = as.character(c(1, 1, 1, 2, 2, 3, 0)), 
    n = as.character(c(3, 2, 1, 3, 2, 3, 0)), 
    q2 = c("bottom tertile", "lower half", "all", "middle tertile", "upper half", "top tertile", "custom")
    )%>%
    .[, .(i, n, q2, i.n = paste0(i, ".", n))]
  }

# C122b()
# C122a()

C122 <- # combine rss and P characteristics ----
  function(rssx,
           pvax = z110 #
  ) {
    x0 <-
      pvax[rssx, on = c(rcx = "rc6")] %>%
      .[
        , .(
          frac = round(sum(nid) / pvax[nchar(rcx) == 6, sum(nid)], nfig3),
          nid = sum(nid),
          ppm2max = round(max(ppm2), nfig2),
          ppm2min = round(min(ppm2), nfig2),
          p = round(sum(pv) / sum(m2), nfig2)
        ),
        lab
      ] %>%
      .[rssx[, .(R2rsi = 1 - sum(ssek) / sum(sstr)), lab], on = c(lab = "lab")] %>%
      .[, .(
        lab = substr(lab, 1, 5),
        frac,
        R2rsi = round(R2rsi, 3),
        pnum = p,
        p = prettyNum(round(p, nfig3), big.mark = ","),
        p.cus = paste0(prettyNum(round(ppm2min, nfig2), big.mark = ","), "-", prettyNum(round(ppm2max, nfig2), big.mark = ","))
      )] %>%
      .[]
    x0
  }

C131x <- #characteristics and summary
  function(
      static='resS',
      tslidex = tslideG,
      rc6tx = rc6tG,
      rescx = copy(rescG)
      ) {
    names(rescx)[which(names(rescx) == "estdt")] <- "rsi"
    x2 <- # nx for this rc3
      resS$f250618b %>%
      .[grep(substr(rc6tx, 1, 3), rc6), unique(nx)]
    resS$rsi <-
      resS$rsi[nx %in% x2]
    x4 <-
      aestdt1(resS) %>%
      resS$lab[., on = c(nx = "nx")]
    x6 <-
      aestdt1(rescx)[, -c("col")]
    x7 <-
      rbind(x4, x6)
    #browser()
    x7 %>%
      .[ii > tslidex] %>%
      dcast(., ii ~ lab, value.var = "xdot") %>%
      .[, -"ii"] %>%
      as.matrix(.) %>%
      zoo(., x4[, sort(unique(date))]) %>%
      table.Stats(., digits = 3) %>%
      t(.) %>%
      .[, c(3, 6, 9, 14, 15, 16)] %>%
      as.data.table(., keep.rownames = T) %>%
      setnames(., c("rn", "min", "mean", "max", "stdev", "skew", "kurtosis")) %>% 
      .[unique(x7[,.(nx,lab)]),on=c(rn='lab')]%>%
      C122a()[., on = c(nx = "nx")]%>%
      .[,tot:=mean*x7[nx==min(nx)][ii > tslidex,.N]]%>%.[]%>%
      .[,pa:=round(tot/x7[ii > tslidex][nx==min(nx)][,sum(days)/365.25],3)]%>%
      .[]
  }
#C131x()

C132a <- #-----132 trade summary(2)----
  function(geox = geoqG,
           steprip = stepripG,
           estdtlx = estdtlG, # only used for its date(ii) relation
           tmin = 20 # tmin=input$tslider
  ) {
    x0 <-
      geox[, grepstring(rc6)] %>%
      coread2(., steprip) %>% # or rc6tc
      .[, .(N = .N, mean = round(mean(as.numeric(retsa)), 4)), .(buy = substr(as.Date(buydate), 1, 4), sell = substr(as.Date(selldate), 1, 4))] %>%
      .[(buy >= estdtlx[ii >= tmin, substr(min(as.character(date)), 1, 4)])]
    x1 <-
      x0 %>%
      dcast(.,
        buy ~ sell,
        value.var = "mean" # the value is unique so any aggregator function is ok
      )
    for (i in 2:length(x1)) x1[[i]] <- ifelse(is.na(x1[[i]]), "", as.character(round(x1[[i]], 3)))
    x2 <-
      x0 %>%
      dcast(.,
        buy ~ sell,
        value.var = "N"
      )
    for (i in 2:length(x2)) x2[[i]] <- ifelse(is.na(x2[[i]]), "", x2[[i]])
    x3 <- list(x1, x2)
    x3
  }


if (F) {
  C112c()
  C112d()
  C121a()
}


# geog2 <- f250619a('geo')
# rsig2 <- f250619a('rsi')
# dddg2 <- f250619a('ddd')
# ksrg2 <- f250619a('ksr')
# ksng2 <- f250619a('ksn')
# pvag2 <- f250619a('pva')



#
# C112a <- # local optimum kss for all rc6 ----
#   function(x0 = f250509ed,
#            nn = "f250509ed" # static
#   ) {
#     x1 <- # local solution for all rc6
#       x0$geo %>%
#       .[grep("^L", lab)] %>%
#       x0$kfoldsse[., on = c(nx = "nx", rc6 = "rc9"), nomatch = NULL] %>% # local only
#       .[, rc3 := substr(rc6, 1, 3)] %>%
#       .[order(rc6, ssek)] %>%
#       .[, .SD[1], rc6] %>%
#       sco(., F)
#     x1
#   }
#
# C112b <- # local lab for all rc6 in rc3tx [join on col here?] ----
#   function(nn = c("f250509ed", "coltab"), # static
#            x0 = C112a() #
#   ) {
#     x1 <- # local solution set for rc3t
#       x0 %>%
#       .[, .(rc3, nx, lab, i.n = substr(lab, 5, 7))] %>%
#       # .[,.(rc3,nx,lab,i.n=paste0(substr(lab,7,7),'.',substr(lab,5,5)))]%>%
#       unique(.) %>%
#       .[data.table(i.n = c("1.3", "1.2", "1.1", "2.3", "2.2", "3.3"), qq = c(1 / 6, 1 / 4, 1 / 2, 1 / 2, 3 / 4, 5 / 6)), on = c(i.n = "i.n"), mult = "all"] %>%
#       .[order(qq)] %>%
#       sco(., F) %>%
#       coltab[., on = c(code = "i.n")]
#     x1
#   }
#
# C112c <- # select extrema and add light dark legendlab ----
#   function(rc6tx = rc6tG,
#            coltabx = coltab) {
#     x1 <-
#       rbind(
#         C112b()[rc3 == substr(rc6tx, 1, 3)][order(qq)][c(1, .N)] # , #top and bottom
#       ) %>%
#       unique(.) %>%
#       # coltab[., on = c(code = "i.n")] %>%
#       .[, legendlab := lab] %>%
#       .[order(-qq)]
#     x1
#   }
#
#
#
# C112d <- # estdt for plot 2 local 1 custom ----
#   function(rc6tx = rc6tG, # rc6t
#            x0 = f250509ed, # kfx
#            x1 = estdtccG, # cus
#            x2 = C112c(rc6tx = rc6tx) # local for plot
#   ) {
#     x3 <-
#       intersect(names(x0$estdt), names(x1)) %>%
#       # setdiff(.,'lab')%>%
#       sort(.)
#     x4 <-
#       rbind(
#         x0$estdt[x2, on = c(nx = "nx")][, x3, with = F],
#         estdtccG[, x3, with = F]
#       )
#     x5 <-
#       rbind(
#         x2[, .(dark, lab, legendlab)],
#         data.table(dark = "brown", lab = "CU00", legendlab = "custom")
#       )
#     # print(x2)
#     x6 <-
#       x4[x5, on = c(lab = "lab")] %>%
#       .[, .(date, ii, lab, legendlab, x, col, dark)]
#     x6
#   }
#
#
# C121a <- # {ii AN BA} dates ----
#   function(x0 = f250509ed$estdt) {
#     x1 <- f250509ed$estdt %>%
#       .[, .(ii = sort(unique(ii)), date = sort(unique(date))), .(tbin = substr(lab, 8, 9))] %>%
#       .[tbin %in% c("BA")] %>%
#       dcast(., ii ~ tbin, value.var = "date") %>%
#       rbind(., data.table(ii = 0, BA = as.Date("1994-12-31"))) %>%
#       .[order(BA)]
#     x1
#   }
#
# C121b <- # {rc6 ssek nx lab} 3 rows i.n ssek-ordered ----
#   function(x0 = f250509ed,
#            rc6tx = rc6tG) {
#     x0$geo %>%
#       .[grep("^L", lab)] %>%
#       .[rc9 == rc6tx, .(nx, lab)] %>%
#       x0$kfoldsse[., on = c(nx = "nx")] %>%
#       .[rc6 == rc6tx] %>%
#       .[order(ssek)] %>%
#       .[, .(rc6, ssek, nx, lab)]
#   }
#
# C121c <- #----
#   function(
#       rc6tx = rc6tG,
#       x0 = f250509ed,
#       x1 = C121a()
#       ) {
#     x2 <- # daily
#       seq.Date(from = x1[1, BA], to = x1[.N, BA], by = "d")
#     x3 <- # annual
#       seq.Date(from = x1[1, BA], to = x1[.N, BA], by = "y") %>%
#       .[-1] %>% # remove d0
#       c(., x1[.N, BA]) %>% # add dmax
#       unique(.)
#     x4 <- x0$estdt[nx == C121b(rc6tx = rc6tx)[1, nx]]
#     x5 <-
#       x4 %>%
#       .[.(date = x2), on = c(date = "date"), roll = -Inf, j = .(date, xdotd)] %>%
#       .[, .(ii = 1:.N, date, x = cumsum(xdotd))] %>%
#       .[.(date2 = x3), on = c(date = "date2")] %>%
#       .[, .(date, x, xdot = c(x[1], diff(x)), ii = 1:.N)] %>%
#       .[, .(ii, date, xdot, x)] %>%
#       .[, .(date, xdot)] %>%
#       .[date == as.Date("2009-02-28"), let(date, as.Date("2008-12-31"))] %>%
#       .[, .(decade = substr(date, 1, 3), yr = substr(date, 4, 4), xdot = round(xdot, 3))] %>%
#       dcast(., decade ~ yr, value.var = "xdot") %>%
#       .[, decade := c(1990, 2000, 2010, 2020)] %>%
#       setnames(., old = "decade", new = "decade\\year") %>%
#       .[]
#     x5
#   }
#
# C122 <- # combine rss and P characteristics ----
#   function(rssx,
#            z110x = z110 #
#   ) {
#     x0 <-
#       z110x[rssx, on = c(rcx = "rc6")] %>%
#       .[
#         , .(
#           frac = round(sum(nid) / z110x[nchar(rcx) == 6, sum(nid)], nfig3),
#           nid = sum(nid),
#           ppm2max = round(max(ppm2), nfig2),
#           ppm2min = round(min(ppm2), nfig2),
#           p = round(sum(pv) / sum(m2), nfig2)
#         ),
#         lab
#       ] %>%
#       .[rssx[, .(R2rsi = 1 - sum(ssek) / sum(sstr)), lab], on = c(lab = "lab")] %>%
#       .[, .(
#         lab = substr(lab, 1, 5),
#         frac,
#         R2rsi = round(R2rsi, 3),
#         pnum = p,
#         p = prettyNum(round(p, nfig3), big.mark = ","),
#         p.cus = paste0(prettyNum(round(ppm2min, nfig2), big.mark = ","), "-", prettyNum(round(ppm2max, nfig2), big.mark = ","))
#       )] %>%
#       .[]
#     x0
#   }
#
#
# C132a <- #-----132 trade summary(2)----
#   function(geox = geoqG,
#            steprip = stepripG,
#            estdtlx = estdtlG, # only used for its date(ii) relation
#            tmin = 20 # tmin=input$tslider
#   ) {
#     x0 <-
#       geox[, grepstring(rc6)] %>%
#       coread2(., steprip) %>% # or rc6tC
#       .[, .(N = .N, mean = round(mean(as.numeric(retsa)), 4)), .(buy = substr(as.Date(buydate), 1, 4), sell = substr(as.Date(selldate), 1, 4))] %>%
#       .[(buy >= estdtlx[ii >= tmin, substr(min(as.character(date)), 1, 4)])]
#     x1 <-
#       x0 %>%
#       dcast(.,
#         buy ~ sell,
#         value.var = "mean" # the value is unique so any aggregator function is ok
#       )
#     for (i in 2:length(x1)) x1[[i]] <- ifelse(is.na(x1[[i]]), "", as.character(round(x1[[i]], 3)))
#     x2 <-
#       x0 %>%
#       dcast(.,
#         buy ~ sell,
#         value.var = "N"
#       )
#     for (i in 2:length(x2)) x2[[i]] <- ifelse(is.na(x2[[i]]), "", x2[[i]])
#     x3 <- list(x1, x2)
#     x3
#   }
#
#
# if (F) {
#   C112c()
#   C112d()
#   C121a()
# }