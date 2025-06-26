r=res
setkey(r$lab,nx)
setkey(r$geo,nx)
setkey(r$rsi,nx)
setkey(r$kss,nx)
setkey(r$pva,nx)
rc6tx=rc6tG

#these moved to step2 as f250618b and f250618c
#they are called in f250618aFun and append to the results list

# f250624a <- # 2274 x 3 minimisedkss, nx*(rc6) ----
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
res$f250624ad <- f250624a()
#f250624a()
#f250624a()[,unique(nx)] #587 of these
#extremal ppm2rc6 per rc3
#r$pva[,.(minPrc6=min(log(pv/m2)),maxPrc6=max(log(pv/m2))),.(rc3=substr(rc6,1,3))]
#ppm2nx per unique nx*

#----f250624b start : 
# f250624b <- #col(rc6) (static)
#   function() {
#     x1 <- f250624a()[, .(nx = unique(nx))][r$geo, on = c(nx = "nx"), nomatch = NULL][r$pva, on = c(rc6 = "rc6")][, .(Pnx = log(sum(pv) / sum(m2))), nx][f250624a(), on = c(nx = "nx")][, .(Pnx, rc6, rc3 = substr(rc6, 1, 3))]
#     x2 <- r$pva[, .(minPrc6 = min(log(pv / m2)), maxPrc6 = max(log(pv / m2))), .(rc3 = substr(rc6, 1, 3))]
#     x3 <- x1[x2, on = c(rc3 = "rc3")][, col := color_price(Pnx, minPrc6, maxPrc6)][, .(rc6, Pnx, col)]
#     x3
#   }
res$f250624bd <- f250624b()

#now res can be mapped, plotted, ...


#-------------------no - want local colors for all rc6
f250624b <- # 208 x 4 extremal nx and ppm2 for all rc3
  function( # res = f250618ad
    r=res,
    x0=f250624a()
    ) {
    x1 <-
      r$geo %>%
      .[x0[,.(nx)],on=c(nx='nx')]%>%
      .[r$lab, on = c(nx = "nx")] %>% # to filter on local
      .[grep("^L", lab)] %>% # local
      .[r$pva, on = c(rc6 = "rc6")] %>% # pva
      .[, .(rc3 = substr(rc6[1], 1, 3), ppm2 = sum(pv) / sum(m2), ppm2rc6min = min(pv / m2), ppm2rc6max = max(pv / m2),i.n=substr(lab,5,7)), nx] %>% # aggregate
      .[order(rc3, ppm2)] %>% # order
      .[
        ,
        .SD[
          c(1, .N),
          .(
            nx,
            type = c("minP", "maxP"),
            i.n,
            ppm2agg = ppm2, # nx : aggregate
            ppm2rc6min, # nx : lowest ppm2(rc6)
            ppm2rc6max # nx : lowest ppm2(rc6)
          )
        ],
        rc3 # per rc3
      ]
    x1
  }
#f250624b()


#names(resx) # "lab" "geo" "rsi" "da0" "kss" "pva"
C112x <- 
  function(
    rc6tx=rc6tG,
    r=res
){
  x1 <- #select nx referencing rc6t 
    r$geo[rc6==rc6tx,.(nx)]
  x2 <- list(
    lab=r$lab[x1],
    geo=r$geo[x1],
    rsi=r$rsi[x1],
    da0=r$da0,
    kss=r$kss[x1],
    pva=r$pva
  )
  x2
}
C112x()

