server_accuracy <- function(input, output, session, common) {
  f411D <- function( #-------411 accuracy tbin----
                    geoqX = geoqG,
                    rc6tX = rc6tG,
                    rssX = rssG) {
    if (verbose) print("enter f411G")
    x1 <-
      data.table(tbin = 1:3, freq = c("lo", "hi", "an"))
    x2 <-
      rssX %>% # use global no filters
      .[geoqX, on = c(rc6 = "rc6")] %>%
      .[type == "L"] %>%
      .[itrim == itriC] %>%
      .[, .(n = sum(n), ssek = sum(ssek)), .(tbin, rc6)]
    x3 <-
      rbind(
        x2[, .(span = "index.average", mse = round(sqrt(sum(ssek) / sum(n)), 4)), tbin],
        x2[rc6 == rc6tX, .(span = rc6tX, mse = round(sqrt(sum(ssek) / sum(n)), 4)), tbin]
      ) %>%
      dcast(., tbin ~ span, value.var = "mse") %>%
      x1[., on = c(tbin = "tbin")] %>%
      .[, -"tbin"]
    x <-
      gt::gt(x3) %>%
      gt::tab_footnote(
        footnote = f241108a(typeC, tbinC)[[1]]
      )
    x411G <<- copy(x)
    x
  }


  f412D <- function( #--------412 accuracy tbin----
                    geoccX = geoccG,
                    rc6tX = rc6tG,
                    rssccX = rssccG) {
    x1 <-
      data.table(tbin = 1:3, freq = c("lo", "hi", "an"))
    x2 <-
      rssccX %>% # use global no filters
      .[geoccX, on = c(rc6 = "rc9")] %>%
      .[, .(n, ssek, tbin = tbinC, rc6)]
    x3 <-
      rbind(
        x2[, .(span = "index.average", mse = round(sqrt(sum(ssek) / sum(n)), 4)), tbin],
        x2[rc6 == rc6tX, .(span = rc6tX, mse = round(sqrt(sum(ssek) / sum(n)), 4)), tbin]
      ) %>%
      dcast(., tbin ~ span, value.var = "mse") %>%
      x1[., on = c(tbin = "tbin")] %>%
      .[, -"tbin"]
    x <-
      gt::gt(x3) %>%
      gt::tab_footnote(footnote = f241108a(tc = "C", tbinC)[[1]]) %>%
      gt::tab_footnote(footnote = paste0("only freq=hi is computed for custom"))
    x412G <<- copy(x)
    x
  }


  f421D <- function( #--------421 accuracy trim----
                    geoqX = geoqG,
                    rc6tX = rc6tG,
                    rssX = rssG) {
    x1 <-
      data.table(itrim = 1:3, threshold = c("0.0", "0.1", "0.5"))
    x2 <-
      rssX %>%
      .[geoqX, on = c(rc6 = "rc6")] %>%
      .[type == "L"] %>%
      .[tbin == tbinC] %>%
      .[, .(n = sum(n), ssek = sum(ssek)), .(itrim, rc6)]
    x3 <- rbind(
      x2[, .(span = "index.average", mse = round(sqrt(sum(ssek) / sum(n)), 4)), itrim],
      x2[rc6 == rc6tX, .(span = rc6tX, mse = round(sqrt(sum(ssek) / sum(n)), 4)), itrim]
    ) %>%
      dcast(., itrim ~ span, value.var = "mse") %>%
      x1[., on = c(itrim = "itrim")] %>%
      .[, -"itrim"]
    x <-
      gt::gt(x3) %>%
      gt::tab_footnote(
        footnote = f241108a(typeC, tbinC)[[1]]
      ) %>%
      gt::tab_footnote(
        footnote = f241108a(typeC, tbinC)[[2]]
      )
    x421G <<- copy(x)
    x
  }

  f422D <- function( # #--------422 accuracy trim----
                    geoccX = geoccG,
                    rc6tX = rc6tG,
                    rssccX = rssccG) {
    x1 <-
      data.table(itrim = 1:3, threshold = c("0.0", "0.1", "0.5"))
    x2 <-
      rssccX %>%
      .[geoccX, on = c(rc6 = "rc9")] %>%
      .[, .(n, ssek, itrim = itriC, rc6)]
    x3 <- rbind(
      x2[, .(span = "index.average", mse = round(sqrt(sum(ssek) / sum(n)), 4)), itrim],
      x2[rc6 == rc6tX, .(span = rc6tX, mse = round(sqrt(sum(ssek) / sum(n)), 4)), itrim]
    ) %>%
      dcast(., itrim ~ span, value.var = "mse") %>%
      x1[., on = c(itrim = "itrim")] %>%
      .[, -"itrim"]
    x <-
      gt::gt(x3) %>%
      gt::tab_footnote(footnote = f241108a(tc = "C", tbinC)[[1]]) %>%
      gt::tab_footnote(footnote = paste0("only threshold=0.1 is computed for custom"))
    x432G <<- copy(x)
    x
  }


  f431D <- function( #------431 accuracy in/out----
                    geoqX = geoqG,
                    rc6tX = rc6tG,
                    rssX = rssG) {
    x1 <-
      rssX %>%
      .[geoqX, on = c(rc6 = "rc6")] %>%
      .[type == "L"] %>%
      .[tbin == tbinC] %>%
      .[itrim == itriC] %>%
      .[, .(n = sum(n), ssek = sum(ssek), ssei = sum(ssei)), .(itrim, rc6)]
    x2 <-
      rbind(
        x1[, .(outsamp = round(sqrt(sum(ssek) / sum(n)), 4), insamp = round(sqrt(sum(ssei) / sum(n)), 4))],
        x1[rc6 == rc6tX, .(outsamp = round(sqrt(sum(ssek) / sum(n)), 4), insamp = round(sqrt(sum(ssei) / sum(n)), 4))]
      ) %>%
      as.matrix(.) %>%
      t(.) %>%
      as.data.table(., keep.rownames = T)
    setnames(x2, c("domain", "index.average", rc6tX)[1:ncol(x2)])
    if (ncol(x2) == 3) x2 <- x2[, c(1, 3, 2)]
    x <-
      gt::gt(x2) %>%
      gt::tab_footnote(
        footnote = f241108a(typeC, tbinC)[[1]]
      ) %>%
      gt::tab_footnote(
        footnote = f241108a(typeC, tbinC)[[2]]
      )
    x431G <<- copy(x)
    x
  }


  f432D <- function( #------432 accuracy in/out----
                    geoccX = geoccG,
                    rc6tX = rc6tG,
                    rssccX = rssccG) {
    x1 <-
      rssccX %>%
      .[geoccX, on = c(rc6 = "rc9")] %>%
      .[, .(n, ssek, ssei, itrim = itriC, rc6)]
    x2 <-
      rbind(
        x1[, .(outsamp = round(sqrt(sum(ssek) / sum(n)), 4), insamp = round(sqrt(sum(ssei) / sum(n)), 4))],
        x1[rc6 == rc6tX, .(outsamp = round(sqrt(sum(ssek) / sum(n)), 4), insamp = round(sqrt(sum(ssei) / sum(n)), 4))]
      ) %>%
      as.matrix(.) %>%
      t(.) %>%
      as.data.table(., keep.rownames = T)
    setnames(x2, c("domain", "index.average", rc6tX)[1:ncol(x2)])
    if (ncol(x2) == 3) x2 <- x2[, c(1, 3, 2)]
    x <-
      gt::gt(x2) %>%
      gt::tab_footnote(footnote = f241108a(tc = "C", tbinC)[[1]])
    x432G <<- copy(x)
    x
  }

  x411D <- eventReactive( #-----------------411----
    list(common$geoqR(), common$rc6tR(), common$rssR()),
    {
      if (verbose) print("enter x411G")
      x <- f411D(geoqX = common$geoqR(), rc6tX = common$rc6tR(), rssX = common$rssR())
      x411G <<- copy(x)
      x
    }
  )

  x412D <- eventReactive( #-----------------412----
    list(common$geoccR(), common$rc6tR(), common$rssccR()),
    {
      if (verbose) print("enter x412D")
      x <- f412D(geoccX = common$geoccR(), rc6tX = common$rc6tR(), rssccX = common$rssccR())
      x412G <<- copy(x)
      x
    }
  )

  x421D <- eventReactive( #-----------------421----
    list(common$geoqR(), common$rc6tR(), common$rssR()),
    {
      if (verbose) print("enter x421D")
      x <- f421D(geoqX = common$geoqR(), rc6tX = common$rc6tR(), rssX = common$rssR())
      x421G <<- copy(x)
      x
    }
  )

  x422D <- eventReactive( #-----------------422----
    list(common$geoccR(), common$rc6tR(), common$rssccR()),
    {
      if (verbose) print("enter x422D")
      x <- f422D(geoccX = common$geoccR(), rc6tX = common$rc6tR(), rssccX = common$rssccR())
      x422G <<- copy(x)
      x
    }
  )

  x431D <- eventReactive( #-----------------431----
    list(common$geoqR(), common$rc6tR(), common$rssR()),
    {
      if (verbose) print("enter x431D")
      x <- f431D(geoqX = common$geoqR(), rc6tX = common$rc6tR(), rssX = common$rssR())
      x431G <<- copy(x)
      x
    }
  )

  x432D <- eventReactive( #-----------------432----
    list(common$geoccR(), common$rc6tR(), common$rssccR()),
    {
      if (verbose) print("enter x432D")
      x <- f432D(geoccX = common$geoccR(), rc6tX = common$rc6tR(), rssccX = common$rssccR())
      x432G <<- copy(x)
      x
    }
  )

  output$x411 <- gt::render_gt(x411D()) #-----.----
  output$x421 <- gt::render_gt(x421D()) #-----.----
  output$x431 <- gt::render_gt(x431D()) #-----.----
  output$x412 <- gt::render_gt(x412D()) #-----.----
  output$x422 <- gt::render_gt(x422D()) #-----.----
  output$x432 <- gt::render_gt(x432D()) #-----.----
}
