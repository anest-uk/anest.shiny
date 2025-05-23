  #------------------ accuracy
  
  f411D <- function(
    geoqX = geoqG, 
    rc6tX = rc6tG, 
    rssX = rssG
  ) { # 2x11 accuracy----tbin----
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
  
  
  f412D <- function(
    geocuX = geocuG, 
    rc6tX = rc6tG, 
    rsscuX = rsscuG
  ) { # 2x11cu accuracy--custom--tbin----
    x1 <-
      data.table(tbin = 1:3, freq = c("lo", "hi", "an"))
    x2 <-
      rsscuX %>% # use global no filters
      .[geocuX, on = c(rc6 = "rc9")] %>%
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
  
  
  f421D <- function(
    geoqX = geoqG, 
    rc6tX = rc6tG, 
    rssX = rssG
  ) { # 221 accuracy----trim----
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
  
  f422D <- function(
    geocuX = geocuG, 
    rc6tX = rc6tG, 
    rsscuX = rsscuG
  ) { # 221cu accuracy----trim----
    x1 <-
      data.table(itrim = 1:3, threshold = c("0.0", "0.1", "0.5"))
    x2 <-
      rsscuX %>%
      .[geocuX, on = c(rc6 = "rc9")] %>%
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
  
  
  f431D <- function(
    geoqX = geoqG, 
    rc6tX = rc6tG, 
    rssX = rssG
  ) { # 231 accuracy----in/out----
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
  
  
  f432D <- function(
    geocuX = geocuG, 
    rc6tX = rc6tG, 
    rsscuX = rsscuG
  ) { # 231cu accuracy----in/out----
    x1 <-
      rsscuX %>%
      .[geocuX, on = c(rc6 = "rc9")] %>%
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
  
  #-----------------------------reactive
  x411D <- eventReactive(
    list(geoqR(), rc6tR(), rssR()), # 2x11 accuracy----tbin----
    {
      if (verbose) print("enter x411G")
      x <- f411D(geoqX = geoqR(), rc6tX = rc6tR(), rssX = rssR())
      x411G <<- copy(x)
      x
    }
  )
  
  x412D <- eventReactive(
    list(geocuR(), rc6tR(), rsscuR()), # 2x11cu accuracy--custom--tbin----
    {
      if (verbose) print("enter x411Gcu")
      x <- f412D(geocuX = geocuR(), rc6tX = rc6tR(), rsscuX = rsscuR())
      x412G <<- copy(x)
      x
    }
  )
  
  x421D <- eventReactive(
    list(geoqR(), rc6tR(), rssR()), # 221 accuracy----trim----
    {
      if (verbose) print("enter x421D")
      x <- f421D(geoqX = geoqR(), rc6tX = rc6tR(), rssX = rssR())
      x421G <<- copy(x)
      x
    }
  )
  
  x422D <- eventReactive(
    list(geocuR(), rc6tR(), rsscuR()), # 221cu accuracy----trim----
    {
      if (verbose) print("enter x422D")
      x <- f422D(geocuX = geocuR(), rc6tX = rc6tR(), rsscuX = rsscuR())
      x422G <<- copy(x)
      x
    }
  )
  
  x431D <- eventReactive(
    list(geoqR(), rc6tR(), rssR()), # 231 accuracy----in/out----
    {
      if (verbose) print("enter x431D")
      x <- f431D(geoqX = geoqR(), rc6tX = rc6tR(), rssX = rssR())
      x431G <<- copy(x)
      x
    }
  )
  
  x432D <- eventReactive(
    list(geocuR(), rc6tR(), rsscuR()), # 231cu accuracy----in/out----
    {
      if (verbose) print("enter x432D")
      x <- f432D(geocuX = geocuR(), rc6tX = rc6tR(), rsscuX = rsscuR())
      x432G <<- copy(x)
      x
    }
  )
  