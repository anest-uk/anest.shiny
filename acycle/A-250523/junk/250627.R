f250624a <- # 2274 x 3 minimisedkss, nx*(rc6) ----
  function(
    r=res,
    type="^L"
    ) {
    x3 <-
      r$geo %>%
      .[r$lab,on=c(nx='nx')] %>%
      .[grep(type, lab)] %>% # local only
      r$kss[., on = c(nx = "nx", rc6 = "rc6"), nomatch = NULL] %>%
      .[order(rc6, ssrk), .SD[1], rc6] %>%
      .[, .(rc6, nx, i.n = substr(lab, 5, 7))] %>%
      .[r$pva, on = c(rc6 = "rc6"), nomatch = NULL] %>%
      .[
        ,
        .(
          rc6, # *
          i.n,
          nx
        )
      ]
    x3
  }

#----f250624b start : 
f250624b <- #col(rc6) (static)
  function() {
    x1 <- f250624a()[, .(nx = unique(nx))][r$geo, on = c(nx = "nx"), nomatch = NULL][r$pva, on = c(rc6 = "rc6")][, .(Pnx = log(sum(pv) / sum(m2))), nx][f250624a(), on = c(nx = "nx")][, .(Pnx, rc6, rc3 = substr(rc6, 1, 3))]
    x2 <- r$pva[, .(minPrc6 = min(log(pv / m2)), maxPrc6 = max(log(pv / m2))), .(rc3 = substr(rc6, 1, 3))]
    x3 <- x1[x2, on = c(rc3 = "rc3")][, col := color_price(Pnx, minPrc6, maxPrc6)][, .(rc6, Pnx, col)]
    x3
  }


# this is national so could be C111c
C111c <-
  function(pva = res$pva,
           geo = res$geo,
           x0 = C111a()) {
    pva %>%
      .[geo, on = c(rc6 = "rc6")] %>%
      .[, .(ppm2nx = sum(pv) / sum(m2)), nx] %>%
      .[x0[, .(i.n, nx, rc6, rc3 = substr(rc6, 1, 3))], on = c(nx = "nx")] %>%
      .[order(rc3, ppm2nx), .(i.n, nx, ppm2nx, rc6, rc3, P = log(ppm2nx))] %>%
      .[, P := log(ppm2nx)] %>%
      .[, .SD[, .(nx, i.n, P, ppm2nx, col = color_price(P, min(P), max(P), light = F), rc6)], rc3] %>%
      .[]
  }
# C111c()

# combine RES
C111e <-
  function(
      res1 = C111d(),
      res2 = res) {
    list(
      lab = rbind(res1$lab, res2$lab)[order(nx)],
      geo = rbind(res1$geo[, .(nx, rc6)], res2$geo[, .(nx, rc6)])[order(nx)],
      rsi = rbind(res1$rsi, res2$rsi)[order(nx,date)],
      da0 = res1$da0,
      kss = rbind(res1$kss, res2$kss)[order(nx)],
      pva = unique(rbind(res1$pva, res2$pva))[order(rc6)]
    )
  }
# C111e()



# this could be d
C112c <- # select extrema ----
  function(rc6tx = rc6tG,
           lab = res$lab,
           # coltabx = coltab,
           x1 = C112a()[C112b(), on = c(nx = "nx")]) {
    x2 <-
      x1 %>%
      .[substr(rc6, 1, 3) == substr(rc6tx, 1, 3)] %>%
      # .[order(qq)] %>%
      .[c(1, .N)] %>%
      .[lab, on = c(nx = "nx"), nomatch = NULL] %>%
      # .[order(-qq)]%>%
      .[, .(
        nx, # * two nx
        lab
        # code,lab
        # code,qq,light,dark,lab
      )]
    x2
  }
# C112c()
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


######### <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<working here
# rsi4:  combine local extrema and custom rsi3
C112c <- function(rsi = res$rsi,
                  rsiC = estdtccG) {
  x1 <-
    rbind(
      rsi[C112c(), on = c(nx = "nx")],
      estdtccG[, .(nx, date, xdotd, lab = "UU00")]
    )
  x1
}
# C112c()

C112d <- # estdt for plot 2 local 1 custom ----
  function(rc6tx = rc6tG, # rc6t
           rsi = res$rsi,
           lab = f250618ad$lab,
           x1 = estdtccG[, .(nx, date, xdotd)], # cus x1
           x2 = C112c(rc6tx = rc6tx) # local for plot x2
  ) {
    x4 <-
      rbind(
        rsi[x2[, .(nx)], on = c(nx = "nx")],
        x1
      ) %>%
      lab[., on = c(nx = "nx")]
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
}