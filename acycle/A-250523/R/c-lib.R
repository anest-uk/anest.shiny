#gen2 accessors
#lowercase c <<<<<<<<<<<<<<<

B112a <- #2274 x 3 minimisedkss, for all rc6 ----
  function(
    lab=res$lab,
    geo=res$geo,
    kss=res$kss[,.(nx,ssrk,rc6)], #some denormalised stuff also
    pva=res$pva[,.(rc6,pv,m2,ppm2=pv/m2)]
  ) {
    x3 <- 
      geo %>%
      .[lab,on=c(nx='nx'),nomatch=NULL]%>%
      .[grep("^L", lab)] %>% # local only
      kss[., on = c(nx = "nx", rc6 = "rc6"), nomatch = NULL] %>%
      .[order(rc6, ssrk), .SD[1], rc6] %>%
      .[,.(rc6,nx,i.n=substr(lab,5,7))]%>%
      .[pva,on=c(rc6='rc6'),nomatch=NULL]%>%
      .[,
        .(
          rc6,# *
          i.n,
          nx,
          ppm2rc6=ppm2 #not needed, not used
        )]
    x3
  }
B112a()

B112b <- # 208 x 4 extremal nx and ppm2 for all rc3
  function(
      # res = f250618ad
      geo = res$geo,
      lab = res$lab,
      pva = res$pva) {
    x1 <-
      geo %>%
      .[lab, on = c(nx = "nx")] %>% # to fileter on local
      .[grep("^L", lab)] %>% # local
      .[pva, on = c(rc6 = "rc6")] %>% # pva
      .[, .(rc3 = substr(rc6[1], 1, 3), ppm2 = sum(pv) / sum(m2), ppm2rc6min = min(pv / m2), ppm2rc6max = max(pv / m2)), nx] %>% # aggregate
      .[order(rc3, ppm2)] %>% # order
      .[
        ,
        .SD[
          c(1, .N),
          .(
            nx,
            type = c("minP", "maxP"),
            ppm2agg = ppm2, # nx : aggregate
            ppm2rc6min, # nx : lowest ppm2(rc6)
            ppm2rc6max # nx : lowest ppm2(rc6)
          )
        ],
        rc3 # per rc3
      ]
    x1
  }
B112b()
B112a()





# this is national so could be B112c
B112c <-
  function(
      pva = res$pva,
      geo = res$geo,
      x0 = B112a()) {
    pva %>%
      .[geo, on = c(rc6 = "rc6")] %>%
      .[, .(ppm2nx = sum(pv) / sum(m2)), nx] %>%
      .[x0[, .(i.n, nx, rc6, rc3=substr(rc6,1,3))], on = c(nx = "nx")] %>%
      .[order(rc3,ppm2nx), .(i.n, nx, ppm2nx, rc6, rc3, P = log(ppm2nx))]
  }
x1 <- B112c()
x1[,.SD[,.(i.n,nx,P,rc6,color_price(P,min(P),max(P)))],rc3]

# color_show <- function(colors, ncol = 40) {
#   n <- length(colors)
#   nrow <- ceiling(n / ncol)
#   grid::grid.newpage()
#   grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow, ncol)))
# 
#   for (i in seq_along(colors)) {
#     row <- ceiling(i / ncol)
#     col <- i %% ncol
#     col <- ifelse(col == 0, ncol, col)
#     grid::grid.rect(gp = grid::gpar(fill = colors[i], col = NA),
#                     vp = grid::viewport(layout.pos.row = row, layout.pos.col = col))
#   }
# }
# 
# #
# x1[col==color_price(1,0,1)]
# color_show(color_price(0,0,100))

x1 <-
  B112c() %>%
  .[, P := log(ppm2nx)] %>%
  .[, .SD[, .(nx,i.n, P, ppm2nx, col = color_price(P, min(P), max(P)), rc6)], rc3] %>%
  .[]
#x1[,.(i.n,col)]%>%unique(.)%>%.[i.n=='1.3']
#x1[,.(i.n,col)]%>%unique(.)%>%.[i.n=='3.3']
x2 <-
  x1[rc3%in% c('NG-')] %>%
  .[, .(col, rc6)]
x2

minzoom <- 7
maxzoom <- 12
f240810b( #->leaflet, colours for areas-to-shade in column 'col'
  x1[, .(col, rc6)],
  x2 = pxosrdo2dd, # map polygons
  pva = pva[,.(rcx=rc6,ppm2=pv/m2)], # for tooltip
  minzoom = minzoom,
  maxzoom = maxzoom
)



+x2 <- x1[order(P),sort(unique(col))]
color_show(x2)
#but probably want to use 'within-rc3' colors


#
rc6tx=rc6tG
B112a()%>%
  .[grep(paste0('^',substr(rc6tx,1,3)),rc6),.(rc6,nx)]%>%
  B112b()[,.(nx,ppm2agg)]
  .[,.()]

#need also for this rc3 all the rc6 and their optimum nx
C112a()[,.(rc6,nx)] #add ppm2nx to C112a 
geo=res$geo[res$lab,on=c(nx='nx'),nomatch=NULL][grep('^L',lab)]
pva[geo,on=c(rc6='rc6')]


#add P, color
x1 <-  #what is this for?
  rbind(
  res$pva%>%
  .[geoccG,on=c(rc6='rc9')]%>%
  .[,.(P=log(sum(pv)/sum(m2))),nx]
  ,
  B112b()%>%
    .[,.(nx,P=log(ppm2agg))]
)%>%
  .[,.(nx,P,col=color_price(P,Pmin=min(P),Pmax=max(P)))]
x1[]

#combine cus, loc* 
x2 <- rbind(
  estdtccG[,.(nx,date,xdotd,type='cus')],
  res$rsi[C112b(),on=c(nx='nx')][,.(nx,date,xdotd,type)]
)[,.(nx,date,xdotd,col=as.factor(nx))]
x2

#layer to denormalise and add color




ggplot(x2,aes())


x1
x2




# x1 <- C112a()%>%
#       .[,.(nx,rc6,rc3=substr(rc6,1,3))] %>%
#       unique(.)

x2 <-   #6628 x 4
  x1%>%
  #.[res$geo,on=c(nx='nx'),nomatch=NULL]%>%
  .[res$pva,on=c(rc6='rc6'),nomatch=NULL]%>%
  .[,.(nx,rc6,rc3,ppm2=pv/m2,pv,m2)]%>%
  .[order(rc3,ppm2)]
x2
x3 <- 
  x2%>%
  #.[,.(ppm2=sum(pv)/sum(m2)),.(nx)]%>%
  #.[C112b(),on=c(nx='nx'),nomatch=NULL]%>%  x1
  .[,.(nx,ppm2=sum(pv)/sum(m2)),.(rc3=substr(rc6,1,3))]%>%
  .[,dark:=color_price(P, Pmin=x1[,log(min(pv/m2))], Pmax=x1[,log(max(pv/m2))], light = FALSE)]%>%
  .[,light:=color_price(P, Pmin=x1[,log(min(pv/m2))], Pmax=x1[,log(max(pv/m2))], light = TRUE)]%>%
  .[]
x2

#this could be d
C112c <- # select extrema ----
  function(
      rc6tx = rc6tG,
      lab = res$lab,
      #coltabx = coltab,
      x1 = C112a()[C112b(), on = c(nx = "nx")]) {
    x2 <-
      x1 %>%
      .[substr(rc6, 1, 3) == substr(rc6tx, 1, 3)] %>%
      #.[order(qq)] %>%
      .[c(1, .N)] %>%
      .[lab, on = c(nx = "nx"), nomatch = NULL] %>%
      #.[order(-qq)]%>%
      .[,.(
        nx, # * two nx
        lab
        #code,lab
        #code,qq,light,dark,lab
        )]
    x2
  }
#C112c()
# 
# 
#     x5 <-
#       rbind(
#         x3[x4, on = c(nx = "nx")][, x3, with = F],
#         estdtccG[, x3, with = F]
#       )
#     x5 <-
#       rbind(
#         x2[, .(dark, lab, legendlab)],
#         data.table(dark = "brown", lab = "CU00", legendlab = "custom")
#       )

    
#########<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<working here    
#rsi4:  combine local extrema and custom rsi3
C112c <- function(
    rsi=res$rsi,
    rsiC=estdtccG
    ) {
  x1 <- 
    rbind(
      rsi[C112c(),on=c(nx='nx')],
      estdtccG[,.(nx,date,xdotd,lab='UU00')]
    )
  x1
}
C112c()

C112d <- # estdt for plot 2 local 1 custom ----
  function(rc6tx = rc6tG, # rc6t
           rsi=res$rsi,
           lab = f250618ad$lab,
           x1 = estdtccG[,.(nx,date,xdotd)], # cus x1
           x2 = C112c(rc6tx = rc6tx) # local for plot x2
  ) {
    x4 <-
      rbind(
        rsi[x2[,.(nx)], on = c(nx = "nx")],
        x1
      )%>%
      lab[.,on=c(nx='nx')]
    x5 <-
      rbind(
        x2[, .(dark, lab, legendlab)],
        data.table(dark = "brown", lab = "CU00", legendlab = "custom")
      )
    # print(x2)
    x6 <-
      x4[x5, on = c(lab = "lab")] %>%
      .[, .(date, ii, lab, legendlab, x, col, dark)]
    x6
  }


C121a <- # {ii AN BA} dates ----
  function(x0 = f250509ed$estdt) {
    x1 <- f250509ed$estdt %>%
      .[, .(ii = sort(unique(ii)), date = sort(unique(date))), .(tbin = substr(lab, 8, 9))] %>%
      .[tbin %in% c("BA")] %>%
      dcast(., ii ~ tbin, value.var = "date") %>%
      rbind(., data.table(ii = 0, BA = as.Date("1994-12-31"))) %>%
      .[order(BA)]
    x1
  }

C121b <- # {rc6 ssek nx lab} 3 rows i.n ssek-ordered ----
  function(x0 = f250509ed,
           rc6tx = rc6tG) {
    x0$geo %>%
      .[grep("^L", lab)] %>%
      .[rc9 == rc6tx, .(nx, lab)] %>%
      x0$kfoldsse[., on = c(nx = "nx")] %>%
      .[rc6 == rc6tx] %>%
      .[order(ssek)] %>%
      .[, .(rc6, ssek, nx, lab)]
  }

C121c <- #----
  function(
      rc6tx = rc6tG,
      x0 = f250509ed,
      x1 = C121a()
      ) {
    x2 <- # daily
      seq.Date(from = x1[1, BA], to = x1[.N, BA], by = "d")
    x3 <- # annual
      seq.Date(from = x1[1, BA], to = x1[.N, BA], by = "y") %>%
      .[-1] %>% # remove d0
      c(., x1[.N, BA]) %>% # add dmax
      unique(.)
    x4 <- x0$estdt[nx == C121b(rc6tx = rc6tx)[1, nx]]
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

C122 <- # combine rss and P characteristics ----
  function(rssx,
           z110x = z110 #
  ) {
    x0 <-
      z110x[rssx, on = c(rcx = "rc6")] %>%
      .[
        , .(
          frac = round(sum(nid) / z110x[nchar(rcx) == 6, sum(nid)], nfig3),
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