
rc6tx=rc6tG
pvax=apva(resS) 
geocx=geocG#is a georc9
geox=ageo(resS)[grep('^L',lab)] #is a georc6

x1 <- resS$f250618b # not a geo
geo <- rbind( # is georc6
  geox,
  geocx[, .(rc6 = rc9, nx, lab = paste0("C", substr(rc6tG, 1, 3), "0.0CU"))]
)
geo1 <- # is georc6
  geo %>% # 1
  .[grep(substr(rc6tx, 1, 3), lab)]
x2 <-
  geo1 %>%
  .[, .(nrc6.est = .N), .(i.n = substr(lab, 5, 7))]
x3 <- # is not a geo
  x1[grep(substr(rc6tx, 1, 3), rc6)] %>%
  rbind(., geocx[, .(rc6 = rc6tx, i.n = "0.0", nx = 0)])
x4 <- # 2
  x3 %>%
  .[, .(nrc6.fit = .N), i.n] # %>%
x5 <- # 3,4,5,6,7
  geo1 %>%
  pvax[., on = c(rc6 = "rc6")] %>%
  .[, .(nid.est = sum(nid), maxppm2 = max(ppm2), minppm2 = min(ppm2), aggppm2 = sum(pv) / sum(m2)), .(nx, lab)] %>%
  .[order(aggppm2), .(lab, nid.est, minppm2, maxppm2, aggppm2, col = color_price(log(aggppm2), log(min(.[,minppm2])), log(max(.[,maxppm2]))), i.n = substr(lab, 5, 7))]
x6 <-
  x2[x4, on = c(i.n = "i.n")][x5, on = c(i.n = "i.n")] %>%
  .[, .(i.n, col, nrc6.est, nrc6.fit)]
x7 <- # 5,6,7
  pvax %>%
  .[geo1, on = c(rc6 = "rc6")] %>%
  .[, .(agg = sum(pv) / sum(m2), max = max(pv / m2), min = min(pv / m2), nid = sum(nid)), .(nx, i.n = substr(lab, 5, 7))] %>%
  .[order(agg)]
x8 <- 
  x6[x7, on = c(i.n = "i.n")][, .(i.n, col, nrc6.est, nrc6.fit, nid, min, max, agg)]
x9 <-
  x8[, .(
    i = substr(i.n, 1, 1),
    n = substr(i.n, 3, 3),
    col,
    nrc6.est,
    nrc6.fit,
    nid = prettyNum(nid, big.mark = ","),
    pmin = prettyNum(round(min), big.mark = ","),
    pmax = prettyNum(round(max), big.mark = ","),
    agg = prettyNum(round(agg), big.mark = ","),
    key = "\u2589" #square symbol for color key
  )]

x10 <- 
  data.table(x9[,.(i,n,c('bottom tertile','bottom half','all','middle tertile','upper half','top tertile','custom'))])[x9,on=c(i='i',n='n')]

x10%>%
      gt::gt(.) %>%
      cols_label(
        i = gt::html("rank"),
        n = gt::html("out of"),
        col = gt::html(""),
        nrc6.est = gt::html("train"),
        nrc6.fit = gt::html("fit"),
        nid = gt::html("properties"),
        pmin = gt::html("min"),
        pmax = gt::html("max"),
        agg = gt::html("aggregate"),
        key = gt::html(""),
      ) %>%
      tab_spanner(
        label = gt::html("£/m<sup>2</sup>"),
        columns = c(pmin,pmax, agg)
        )%>%
      tab_spanner(
        label = gt::html("quantiles"),
        columns = c(i,n)
        )%>%
      tab_spanner(
        label = gt::html("districts"),
        columns = c(nrc6.est,nrc6.fit)
      )%>%
      text_transform(
        locations = cells_body(columns = key),
        fn = function(codes) {
          purrr::map2_chr(codes, x9[,col], ~ paste0(
            "<span style='color:", .y, "; font-weight:bold;'>", .x, "</span>"
          )) %>%
            purrr::map(htmltools::HTML)
        }
      ) %>%
      cols_hide(columns = col) %>%
      cols_label(key = "")

# C122 <- # combine rss and P characteristics ----
#   function(rssx,
           pvax = z110 #
#  ) {
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
#  }
