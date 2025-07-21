#-------------------------------------------------gen2 calc lib
Ccus <- #--------------RES for custom : rescxR----
  function(
      rescx = rescG,
      pvax = apva(resS)[, -"ppm2"]) {
    x1 <-
      list(
        lab = data.table(lab = "CU00", nx = 0),
        geo = rescx$geo[, .(nx, rc6 = rc9)],
        rsi = rescx$estdt[, .(
          date,
          nx,
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
        )] %>% sco(., F),
        pva = rescx$geo[, .(rc6 = rc9)][pvax, on = c(rc6 = "rc6")] %>% sco(., F)
      )
    vres(x1)
    x1
  }


CC4111 <- #-------------rc3t{} blobs : D4311a ----
  function(
      statics = "resS",
      rc6tx = rc6tG) {
    x1 <-
      resS$geo %>%
      .[resS$lab[grep("^L", lab)][grep(substr(rc6tx, 1, 3), lab)], on = c(nx = "nx")] %>%
      .[apva(resS)[, .(rc6, rc6P = log(pv / m2), pv, m2, rc6ppm2 = round(pv / m2), rc6nid = nid)], on = c(rc6 = "rc6"), nomatch = NULL] %>%
      .[, rc6col := color_price(rc6P, min(rc6P), max(rc6P))]
    x1[]
    x2 <-
      x1 %>%
      .[, .(grpppm2 = sum(pv) / sum(m2)), .(lab, nx)] %>%
      .[, .(grpppm2, lab, nx, grpP = log(grpppm2))] %>%
      .[, .(grpppm2, lab, nx, grpP, grpcol = color_price(grpP, x1[, min(rc6P)], x1[, max(rc6P)]))] %>%
      .[, .(grpppm2, lab = paste0(substr(lab, 1, 4), "xx", substr(lab, 7, 9)), nx, grpP, grpcol)]
    x2[, ]

    x3 <-
      x2[, .(nx, lab, grpcol, grpppm2)] %>%
      .[x1[, .(nx, rc6, rc6nid, rc6ppm2, rc6col)], on = c(nx = "nx"), mult = "first"] %>%
      .[order(nx, rc6ppm2)] %>%
      resS$f250713a[., on = c(rc6 = "rc6")]
    x3[]
    x4 <-
      x3 %>%
      dcast(., rc6 + rc6ppm2 + rc6nid + rc6col + locality ~ lab, value.var = "grpcol") %>%
      .[order(-rc6ppm2)]
    x4[]
    setnames(x4, c("rc6", "ppm2", "nid", "q0", "locality", "q1", "q2", "q3"))
    x4[, .(rc6, locality, ppm2, nid, q0, q3, q2, q1), with = T]
  }

# CC4131 <-  #-------------custom blobs : D4131 ----
#   function(
#     statics='resS',
#     rc6tx=rc6tG,
#     rc6cx=rc6cG,
#     x0=CC4111(rc6 = rc6tx)[,range(log(ppm2))] #P-range for color
# ) {
#   x1 <- 
#     apva(resS)%>%
#     .[data.table(rc6=rc6cx),on=c(rc6='rc6')]%>% #select custom
#     .[,.(nid,ppm2,rc6,P=log(ppm2),q0=color_price(log(ppm2),x0[1],x0[2]))]%>%
#     resS$f250713a[.,on=c(rc6='rc6')]%>% #add localiity
#     .[,
#       .(rc6,
#         locality,
#         ppm2,
#         nid,
#         q0
#         )]
#   x1
# }


CC4131 <-   #-------------custom blobs : D4131 ----
  function(
    statics='resS',
    rc6tx=rc6tG,
    rc6cx=rc6cG,
    x0=CC4111(rc6 = rc6tx)[,range(log(ppm2))] #P-range for color
) {
  x1 <- 
    apva(resS)%>%
    .[data.table(rc6=rc6cx),on=c(rc6='rc6')]%>% #select custom
    .[,.(nid,ppm2,rc6,P=log(ppm2),q0=color_price(log(ppm2),x0[1],x0[2]))]%>%
    resS$f250713a[.,on=c(rc6='rc6')]%>% #add localiity
    .[,
      .(rc6,
        locality,
        ppm2,
        nid,
        q0
        )]
  x1
}
CC4211 <- #------------------summary : D4131a #----
  function(
      static = "resS",
      tslidex = tslideG,
      rc6tx = rc6tG,
      rescxx = rescxG) {
    x1 <-
      CC4231x(
        tslidex = tslidex,
        rc6tx = rc6tx,
        rescxx = rescxx
      )[, .(
        nx, col, i.n, q2, nid.est, minppm2, maxppm2, aggppm2
      )]
    x2 <- rbind(
      aestdt3(nx = 0, resx = rescxG) %>% # custom
        .[ii > tslidex] %>%
        .[, .(year = substr(date, 1, 4), xdotd), nx] %>%
        .[, .(xdotan = sum(xdotd), ndays = .N), .(year, nx)] %>%
        .[ndays >= 365, .(meanan = mean(xdotan), minan = min(xdotan), maxan = max(xdotan)), nx], # full calendar years
      aestdt3(nx = CC4231x()[, unique(nx)], res = resS) %>% # local
        .[ii > tslidex] %>%
        .[, .(year = substr(date, 1, 4), xdotd), nx] %>%
        .[, .(xdotan = sum(xdotd), ndays = .N), .(year, nx)] %>%
        .[ndays >= 365, .(meanan = mean(xdotan), minan = min(xdotan), maxan = max(xdotan)), nx]
    )
    x1[x2, on = c(nx = "nx")][order(-aggppm2)]
  }

CC4212 <- #-------- trade summary(2) : D4132x ----
  function(
      geox = geoqG,
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

CC4221 <- # -----------------winding : D4121x ----
  function(
      rcx = rc6tG,
      x1 = data.table(BA = aestdt2()$BA)[, ii := .I - 1][, .(date = BA, ii)],
      x4 = aestdt1(areso(rcx = rcx))) {
    x2 <- # daily
      seq.Date(from = x1[1, date], to = x1[.N, date], by = "d")
    x3 <- # annual
      seq.Date(from = x1[1, date], to = x1[.N, date], by = "y") %>%
      .[-1] %>% # remove d0
      c(., x1[.N, date]) %>% # add dmax
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

CC4222a <- # -----------characteristics D4122x ----
  function(
      rc6tx = rc6tG,
      geocx = ageo(rescxG),
      pvax = apva(resS),
      geox = ageo(resS)[grep("^L", lab)] # is a georc6
      ) {
    x1 <- resS$f250618b # not a geo
    geo <- rbind( # is georc6
      geox,
      geocx[, .(rc6, nx, lab = paste0("C", substr(rc6tG, 1, 3), "0.0CU"))]
    )
    geo1 <- # is georc6 : this rc3 geo
      geo %>% # 1
      .[grep(substr(rc6tx, 1, 3), lab)]
    x2 <- # count per i.n : train
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
      .[order(aggppm2), .(nx, lab, nid.est, minppm2, maxppm2, aggppm2, col = color_price(log(aggppm2), log(min(.[, minppm2])), log(max(.[, maxppm2]))), i.n = substr(lab, 5, 7))]
    x6 <-
      x2 %>%
      .[x4, on = c(i.n = "i.n")] %>%
      .[x5, on = c(i.n = "i.n")] %>%
      CC4222b()[., on = c(i.n = "i.n")]
    x6[order(aggppm2), .(nx, lab, i.n, q2, nrc6.est, nrc6.fit, nid.est, minppm2, maxppm2, aggppm2, col)]
  }
CC4222b <- #----------------labelling : CC4222a ----
  function() {
    data.table(
      i = as.character(c(1, 1, 1, 2, 2, 3, 0)),
      n = as.character(c(3, 2, 1, 3, 2, 3, 0)),
      q2 = c("bottom tertile", "lower half", "all", "middle tertile", "upper half", "top tertile", "custom")
    ) %>%
      .[, .(i, n, q2, i.n = paste0(i, ".", n))]
  }


CC4231x <- #-characteristics and summary : D4131x ----
  function(
      static = "resS",
      tslidex = tslideG,
      rc6tx = rc6tG,
      rescxx = rescxG) {
    x2 <- # nx for this rc3
      resS$f250618b %>%
      .[grep(substr(rc6tx, 1, 3), rc6), unique(nx)]
    resS$rsi <-
      resS$rsi[nx %in% x2]
    x4 <-
      aestdt1(resS) %>%
      resS$lab[., on = c(nx = "nx")]
    x6 <-
      aestdt1(rescxx) %>% # [, -c("col")]
      rescxx$lab[., on = c(nx = "nx")]
    x7 <-
      rbind(x4, x6)
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
      .[unique(x7[, .(nx, lab)]), on = c(rn = "lab")] %>%
      CC4222a(
        rc6tx = rc6tx,
        geocx = ageo(rescxx)
      )[., on = c(nx = "nx")] %>%
      .[, tot := mean * x7[nx == min(nx)][ii > tslidex, .N]] %>%
      .[] %>%
      .[, pa := round(tot / x7[ii > tslidex][nx == min(nx)][, sum(days) / 365.25], 3)] %>%
      .[]
  }

CC4311 <- #-----summary all listings : D4211a ----
  function(
      statics = c("resS", "salS"),
      estdtlx = estdtlG, # l=aestdt1(areso(rc6tx)) c=aestdt1(rescxG)
      geoqx = geoqG, # l=ageo(areso(rc6tx)) c=ageo(rescxG)
      salx = salS) {
    if (verbose) print("enter R211")
    x1 <-
      salx %>%
      .[geoqx[, .(rc6, lab, nx)], on = c(rc6 = "rc6")] %>%
      .[, rbind(data.table(date = resS$da0, cum = 0), .SD), .(nh, rc6, lab, nx)] %>%
      .[, .(cum = sum(cum)), .(nx, lab, nh, date)] %>%
      dcast(., date + nx + lab ~ nh, value.var = "cum") %>% #
      .[order(lab, date), .(date, NF, NH, UF, UH), .(lab, nx)]
    if (F) {
      x1[, .(tot = sum(NF + UF + UH + NH)), date][, .(days = as.integer(diff(date)), N = diff(tot), rate = diff(tot) / as.integer(diff(date)))] %>% .[, barplot(rate)]
    }
    x2 <-
      estdtlx %>%
      x1[., on = c(date = "date", nx = "nx")] %>%
      .[, .(ii,
        date, days, xdot, x,
        NF = c(0, diff(NF)),
        NH = c(0, diff(NH)),
        UF = c(0, diff(UF)),
        UH = c(0, diff(UH)),
        tot = c(0, diff(NF + NH + UF + UH))
      ), nx] %>%
      .[-1, .(
        nx,
        ii,
        date,
        days,
        yrs = round(days / 365.25, 1),
        return = round(xdot, sf),
        cumreturn = round(x, sf),
        newhouse = round(NH / tot, sf),
        usedhouse = round(UH / tot, sf),
        newflat = round(NF / tot, sf),
        usedflat = round(UF / tot, sf),
        total = round(tot),
        perday = round(tot / days, 1)
      )]
    x2
  }

C4311b <- # {rc6,locality,ppm2,nid} peer*----
  function(
      rc6tx = rc6tG,
      x0 = ageo()[grep(paste0("^C", rc6tx), lab)][, .(rc6 = sort(unique(rc6)))], # dt of peers {rc6}
      statics = "resS") {
    resS$f250713a %>%
      .[x0, on = c(rc6 = "rc6")] %>%
      .[resS$pva, on = c(rc6 = "rc6"), nomatch = NULL] %>%
      .[, .(rc6, locality, ppm2 = pv / m2, nid), nomatch = NULL]
  }

# CC4112 <- # CC4112 combines this with CC4111
#   function(
#       rc6tx = rc6tG,
#       rc6cx = rc6cG,
#       x0 = data.table(rc6 = rc6cx),
#       x1 = CC4111(rc6tx = rc6tx),
#       x2 = C4311b(rc6tx = rc6tx, x0 = x0)[!(rc6 %in% x1[, rc6])] # out of area peers
#       ) {
#     x3 <- x1[, log(range(ppm2))] %>% setNames(., c("minP", "maxP"))
#     x2[, .(
#       rc6,
#       locality,
#       ppm2,
#       nid,
#       q0 = color_price(log(ppm2), x3["minP"], x3["maxP"]),
#       q3 = "",
#       q2 = "",
#       q1 = ""
#     )] %>%
#       rbind(x1, .)
#   }

# CC4131 <- # C4311b: a second function that just returns rc6,locality,ppm2,nid for the exotic peers
#   function(
#       rc6cx = rc6cG,
#       rc6tx = rc6tG,
#       statics = "resS") {
#     CC4112(rc6cx = rc6cx, rc6tx = rc6tx) %>%
#       .[, .(rc6, locality, ppm2, nid, q0)]
#   }
