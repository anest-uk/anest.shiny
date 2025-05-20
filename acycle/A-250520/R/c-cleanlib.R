abbrev <-
  function(
      # abbrev - abbreviate and remove forbidden characters
      x,
      len = 30,
      rep = "",
      patt = list("\\.", "/", "&", "\\*", ":", ","),
      nospace = TRUE) {
    if (nospace) {
      patt <- union(patt, " ")
    }
    x <- abbreviate(x, minl = len)
    x <- gsub(x = x, patt = grepstring(patt, caret = F), rep = rep)
    x
  }
accrue2 <-
  function(
      # for vector of dates, accrue days from buydate to selldate
      pdate = round(seq(from = fur[, min(buydate)], to = fur[, max(selldate)], length.out = 10)),
      fur = f221029bd # has Date fields buydate,selldate
      ) {
    x1 <- pdate <- round(sort(unique(pdate)))
    x1[length(x1)] <- fur[, max(selldate)] # always accrue all dates at the end
    x2 <- structure(outer(x1, fur[, selldate], `-`),
      class = "numeric"
    )
    x2[] <- pmax(0, x2[]) # numeric matrix
    x3 <- structure(outer(x1, fur[, buydate], `-`),
      class = "numeric"
    )
    x3[] <- pmax(0, x3[])
    structure(
      cbind(
        t(x3[1, , drop = F]),
        t(diff(x3 - x2))
      ), # accrue all days up to pdate[1]
      dimnames = list(
        fur[, idhash.selldate],
        as.character(pdate[])
      )
    )
  }
cardinalgeo <-
  function( # 10 *representative areas*
  ) {
    structure(
      list(
        rc9 =
          c(
            "TS-", "L--", "S--", "M--", "LS-", "B--", "BS-", # 1:7 : metro areas
            "AL-", "HP-10-", "HP-16-", "HP-23-", # 8 : AL&HP-peers
            "HP-4--", "HP-6--", "HP-7--", "HP-8--", "HP-9--",
            "N--", # 9 : N for non-prime London
            "EC-4A-", "W--1H-", # 10 : top 34==q10
            "WC-1V-", "WC-1A-", "SW-5--", "W--1W-",
            "WC-2H-", "N--1C-", "SW-1H-", "SW-10-", "WC-2B-", "SW-1E-", "WC-2N-",
            "W--1T-", "EC-3R-", "W--1D-", "W--1U-", "W--1G-", "W--1F-", "W--11-",
            "WC-2E-", "SW-7--", "W--8--", "SW-1W-", "SW-3--", "WC-2R-", "SW-1X-",
            "SW-1A-", "W--1B-", "W--1J-", "SW-1Y-", "W--1K-", "WC-2A-", "W--1S-"
          ),
        nx = c(
          1, 2, 3, 4, 5, 6, 7, # 1:7 : metro areas
          8, 8, 8, 8, 8, 8, 8, 8, 8, # 8 : AL&HP-peers
          9, # 9 : N for non-prime London
          10, 10, 10, 10, 10, 10, 10, 10, 10, # 10 : top £/m2 PCL districts
          10, 10, 10, 10, 10, 10, 10, 10, 10,
          10, 10, 10, 10, 10, 10, 10, 10, 10,
          10, 10, 10, 10, 10, 10, 10
        )
      ),
      class = "data.frame",
      row.names = c(NA, -50L)
    ) %>% data.table(.)
  }
chkpcode <-
  function(
      pc = "EC2R 8AH") {
    nch <- sapply(pc, nchar)
    stopifnot(all(nch <= 8)) # right length
    stopifnot(unlist(lapply(gregexpr(patt = " ", pc), length)) == 1) # max one space
    x <- strsplit(pc, split = " ")
    if (length(x[[1]]) == 1) {
      stopifnot(all(unlist(gregexpr(pc, patt = "^[A-Z,a-z]")) == 1)) # 1-part always starts with alpha cap
    }
    if (length(x[[1]]) == 2) {
      pcin <- lapply(x, "[[", 2)
      if (!all(unlist(gregexpr(pcin, patt = "^[0-9]")) == 1)) stop("postcode malformed") # 2-part always starts with number [never fails]
      stopifnot(all(unlist(gregexpr(pcin, patt = "^[0-9]")) == 1))
    }
    TRUE
  }
circlefit <-
  function(
      # from package pracma modified: silent, returns rms DEPRECATED due to shared name :(
      xp,
      yp) {
    if (!is.vector(xp, mode = "numeric") || !is.vector(yp, mode = "numeric")) {
      stop("Arguments 'xp' and 'yp' must be numeric vectors.")
    }
    if (length(xp) != length(yp)) {
      stop("Vectors 'xp' and 'yp' must be of the same length.")
    }
    n <- length(xp)
    p <- qr.solve(cbind(xp, yp, 1), matrix(xp^2 + yp^2, ncol = 1))
    v <- c(p[1] / 2, p[2] / 2, sqrt((p[1]^2 + p[2]^2) / 4 + p[3]))
    rms <- sqrt(sum((sqrt((xp - v[1])^2 + (yp - v[2])^2) - v[3])^2) / n)
    x <- c(x = v[1], y = v[2], r = v[3], rms = rms)
    return(x)
  }
cobalt <-
  function() {
    c(
      blue = "#0082F4",
      green = "#35CA05",
      onch = "#ED9304",
      punk = "#FF628C",
      midnight = "#002170"
    )
  }
cocomkd <-
  function(
      # make subdir for csv(postcode) WINDOWS
      type = "coco") {
    shell(paste0("rd /s /q .\\", type), intern = T)
    mkdirn(type)
  }
coread <-
  function(
      # read data from csv in subdir
      rcx = "AL-", # postcodes to read
      step = "nac", # subdir name
      colClasses = NULL, # e.g. colClasses=c(deed_date='character'),nrow=10
      nrows = Inf) {
    x1 <- dir(step)
    if (length(x1) == 0) {
      stop(paste0("step: ", step, " no files found"))
    }
    x2 <- grep(grepstring(rcx, caret = T), x1)
    x3 <- x1[x2]
    ext <- tolower(unique(unlist(lapply(strsplit(x3, split = "\\."), `[`, i = 2))))
    stopifnot(length(ext) == 1) # csv or rdata
    x4 <- list(NULL)
    i <- 1
    for (i in seq_along(x3)) {
      fp <- paste0(step, "/", x3[i])
      if (ext == "rdata") {
        load(file = fp)
      } else if (ext == "csv") {
        x <- fread(file = fp, colClasses = colClasses, nrows = nrows)
        if (is.data.table(x) & is.null(colClasses)) { # undo autorecognition
          for (i2 in seq_along(x)) {
            x[[i2]] <- as.character(x[[i2]])
          }
        }
      } else {
        stop(paste0("file not found step=", step, "rcx=", paste0(rcx, collapse = ",")))
      }
      x4[[i]] <- x
    }
    rbindlist(x4)
  }
coread2 <-
  function(
      # read data from csv in subdir
      rcx = "AL-", # postcodes to read
      step = "app13\\test1", # subdir name
      colClasses = c(retsa = "double"), # e.g. colClasses=c(deed_date='character'),nrow=10
      nrows = Inf,
      addrcode = c(rc9 = "rcx"),
      datetochar = F) {
    x1 <- dir(step)
    if (length(x1) == 0) {
      stop(paste0("step: ", step, " no files found"))
    }
    x2 <- grep(grepstring(rcx, caret = T), x1)
    x3 <- x1[x2]
    ext <- tolower(unique(unlist(lapply(strsplit(x3, split = "\\."), `[`, i = 2))))
    stopifnot(length(ext) == 1) # csv or rdata
    x4 <- list(NULL)
    i <- 1
    for (i in seq_along(x3)) { #
      fp <- paste0(step, "/", x3[i])
      if (ext == "rdata") {
        load(file = fp)
      } else if (ext == "csv") {
        x <- fread(file = fp, colClasses = colClasses, nrows = nrows)
        jdate <- grep("date", colnames(x))
        jname <- colnames(x)[jdate]
        for (j in seq_along(jdate)) {
          # txt <-
          #   ifelse(datetochar,
          #   paste0(jname[j],':=as.character(as.Date(',jname[j],'))'),
          #   paste0(jname[j],':=as.Date(',jname[j],')')
          #   )
          txt <- # the returned values are either character or date, starting from integer
            ifelse(datetochar,
              paste0(jname[j], ":=as.character(as.Date(as.numeric(", jname[j], ")))"),
              paste0(jname[j], ":=as.Date(as.numeric(", jname[j], "))")
            )
          x[, eval(parse(text = txt))]
        }
      } else {
        stop(paste0("file not found step=", step, "rcx=", paste0(rcx, collapse = ",")))
      }
      if (!is.null(addrcode)) {
        x[, let(names(addrcode), gsub(".csv", "", x3[i]))]
      }
      x4[[i]] <- copy(x)
    }
    rbindlist(x4)
  }
cowrite <-
  function(
      # split table object on keyx, write to a set of rdata or csv in dirnam
      x0 = f201203dd,
      dirnam = "f201203d",
      newdir = T,
      keyx = "rc3", # file per unique value of keyx
      nokey = F, # remove column keyx before write
      format = c("rdata", "csv"),
      csv = F # in addition write all.csv
      ) {
    format <- match.arg(format)
    if (newdir) {
      suppressWarnings(cocomkd(dirnam))
    }
    stopifnot(keyx %in% names(x0))
    x1 <- setkeyv(copy(x0), keyx)
    x2 <- sort(unique(x1[, eval(parse(text = keyx))]))
    for (i in seq_along(x2)) {
      x <- x1[x2[i]]
      if (nokey) {
        x <- x[, which(grepl(keyx, colnames(x))) := NULL] # remove keyx : SO 9202413
      }
      if (format == "rdata") {
        save(x, file = paste0(dirnam, "/", x2[i], ".RData"))
      } else {
        fwrite(x, file = paste0(dirnam, "/", x2[i], ".csv"))
      }
    }
    if (csv) {
      fwrite(x0, file = paste0(dirnam, "/all.csv"))
    }
    x4 <- data.table(file.info(paste0(dirnam, "/", dir(paste0(dirnam, "/")))), keep.rownames = T)
    x4
  }
das <-
  function(
      # das = days after sale = days subsequent to saleda within refda[i]:refda[i+1]
      refda,
      saleda) {
    x1 <- structure(outer(refda, saleda, `-`), class = "numeric")
    x1[] <- pmax(0, x1[])
    x2 <- cbind(t(x1[1, , drop = F]), t(diff(x1)))
    setnames(as.data.table(x2), as.character(refda))
  }
f221206a <-
  function(
      # SES : soar estdt stat
      bso, # list 'big solution' record-level RSI results
      geoin = f221121bd[, .(date = pvdate, rc6, nx = ceiling(qai * 5))]) {
    jrc <- names(geoin)[grep("^rc", names(geoin))] %>% setNames(., .) # now bso has rc6 and rc9 but only one is returned
    x1 <- copy(bso)
    x5 <- as.list(NULL) # stat
    for (i in seq_along(x1)) {
      x5[[i]] <- x1[[i]][["stat"]] %>%
        .[, .(rsq, nsam, sigma, type, nx = x1[[i]][["nx"]], update = strsplit(names(x1), split = "\\.")[[1]][1])]
    }
    x6 <- rbindlist(x5)
    x3 <- # estdt
      x1 %>%
      lapply(., `[[`, i = "estdt") %>%
      rbindlist(.)
    x2 <- # soar=solve.reprice.aggregate.rank
      x1 %>% # bso
      lapply(., `[[`, i = "pvid") %>%
      rbindlist(.) %>% # combine
      setkey(., idhash, selldate) %>%
      .[.[, .(idhash = unique(idhash))], mult = "last"] %>% # last sale per id
      .[, .(nid = .N, pv = sum(pv), m2 = sum(m2), ppm2 = sum(pv) / sum(m2)), jrc] %>% # aggregate
      .[order(ppm2), c("nid", "pv", "m2", "ppm2", jrc), with = F] %>%
      .[, xrnk := rank(ppm2)] %>%
      .[, qai := cumsum(nid) / sum(nid)]
    x7 <- x2 %>% # soar with terminal rank
      .[, c("pv", "m2", "nid", "ppm2", "xrnk", "qai", jrc), with = F] %>%
      setnames(., old = "xrnk", new = "rnk")
    x5 <- list(soar = x7, estdt = x3, stat = x6)
    x5
  }
f221209a <-
  function(
      # wrapper for accrue2
      geo = f221230ad$geo,
      dfn = f230215c(), # yearend series
      fur = f221029bd,
      applygeo = T) {
    dfn <- sort(unique(dfn))
    if (applygeo) {
      x0 <- fur[geo, on = names(geo)[grep("^rc.", names(geo))] %>% setNames(., .), allow = T]
    } else {
      x0 <- fur
    }
    x1 <- accrue2( # accrue
      fur = x0,
      pdate = dfn[-1]
    ) %>%
      data.table(., keep.rownames = T) %>%
      .[
        x0[, .(idhash.selldate, rc9, retsa)],
        on = c(rn = "idhash.selldate")
      ] %>%
      setnames(., old = "rn", new = "idhash.selldate")
    x1
  }
f230311a <-
  function(
      # RSI from pra x6
      nxx = 1,
      steppra = "now\\ver002\\07pra",
      stepsip = "now\\ver001\\02sip",
      steprip = "now\\ver001\\03rip", # needed for buydate which is reported in fit
      d0 = "1994-12-31",
      geo,
      onepass = F,
      q1 = .1 # outlier fraction
      ) {
    stopifnot(
      "nx" %in% names(geo) &
        "rc9" %in% names(geo) &
        length(nxx) == 1 &
        nxx %in% geo[, nx]
    )
    geox <- geo[nx == nxx]
    x1 <- coread(rcx = geox[, rc9], step = stepsip, colClasses = list(Date = "selldate"))[, rc6 := substr(rc9, 1, 6)][, rc3 := substr(rc9, 1, 3)]
    x2 <- coread(rcx = geox[, rc9], step = steppra, colClasses = list(numeric = "retsa")) %>%
      .[, !grepl("^rc", names(.)), with = F] %>% # more than one
      .[, order(nchar(names(.)), names(.)), with = F] %>%
      .[order(idhash.selldate)]
    x2a <- coread(rcx = geox[, rc9], step = steprip, colClasses = list(numeric = "retsa")) %>%
      .[x2[, .(idhash.selldate)], on = c(idhash.selldate = "idhash.selldate")] %>%
      .[, .(idhash.selldate, buydate, rc9)]
    stopifnot(all.equal(x2[, idhash.selldate], x2a[, idhash.selldate]))
    tdate <- as.Date(sort(unique(c(d0, names(x2)[grep("[0-9]{4}-", names(x2))]))))
    ddate <- diff(as.integer(tdate))
    x3 <- lm(retsa ~ . - 1, x2[, -"idhash.selldate"])
    x4 <- residuals(x3) # residuals all
    x5 <- summary(x3) # summary all
    if (onepass) { # outlier rejection already applied
      x7 <- x3 # no stats for 'with outliers'
      x6 <- x2
    } else { # 2nd pass excluding outliers
      x6a <- x2a[, .(idhash.selldate, buydate, rc6 = substr(rc9, 1, 6), res = x4)] # per rc6 231210
      x6b <- x6a[, .(lo = quantile(res, q1 / 2), hi = quantile(res, (1 - q1 / 2))), rc6] # apply thresholds
      x6c <- x6b[x6a, on = c(rc6 = "rc6")][(lo < res & res < hi), .(rc6, idhash.selldate, buydate)] # select inlier
      x6 <- x2[x6c[, .(idhash.selldate)], on = c(idhash.selldate = "idhash.selldate")]
      x7 <- lm(retsa ~ . - 1, data = x6[, -"idhash.selldate"]) # regression inlier
    }
    x8 <- summary(x7)
    x9 <- tcrossprod(ddate) * x8$cov.unscaled * x8$sigma^2 # estimator covariance
    x10 <- nrow(x9)
    x11 <- rep(0, x10)
    for (i in 1:x10) {
      x11[i] <- # cumulative lookback estimation s.e.
        sqrt(sum(x9[i:x10, i:x10]))
    }
    jnam <- # allow for geox may have a misnamed rcn (ie rc6/9) col, so detect the correct join field name
      names(geox)[grep("^rc", names(geox))] %>%
      `names<-`(., value = paste0("rc", nchar(geox[1, ., with = F])))
    x12 <- #---estdt---1/4
      data.table(as.matrix(x7$coefficients), keep.rownames = T) %>%
      .[, type := "inlier"] %>%
      .[, rn := gsub("\`", "", rn)] %>%
      .[, .(
        date0 = tdate[-length(tdate)],
        date1 = as.Date(rn),
        xdot.daily = V1,
        type,
        days = ddate
      )] %>%
      .[, xdot := xdot.daily * days] %>%
      .[, x := cumsum(xdot), type] %>%
      .[, xdotse := x8$coefficients[, 2] * days] %>%
      .[, xse := x11] %>%
      .[, nx := nxx] %>%
      unique(geo[, .(nx, rc3 = lab)])[., on = c(nx = "nx"), allow = T]
    x13 <- x1[geox[, .(rc9)], on = jnam] # sale (no outlier reject)
    x14 <- das( # accrue current holding period 'days after sale'
      refda = tdate[-1],
      saleda = x13[, selldate]
    )
    x15 <- predict( # estimated post-purchase return
      x7,
      newdata = as.data.table(x14),
      se.fit = T
    ) %>%
      .[c("fit", "se.fit")] %>%
      as.data.table(.) %>%
      setnames(., c("post", "se.post"))
    x16 <- data.table( #---fit---2/4
      x2[, .(idhash.selldate)],
      x2a[, .(buydate, rc6 = substr(rc9, 1, 6))], # ordering checked already
      fit = fitted(x3),
      res = residuals(x3)
    )
    x17 <- #---pvid---3/4 (no outlier reject)
      data.table(cbind(x13[, -c("pxraw", "rc3")], x15)) %>%
      .[, pv := round(pxsa * exp(post), -2)] %>%
      .[, se.pv := round(sqrt(exp((se.post^2)) - 1) * pv, -2)] %>% # present value
      .[, se.px := round(sqrt(exp((se.post^2 + x8$sigma^2)) - 1) * pv, -2)] # present price
    x18 <- cbind( #---stat---3/4 (r-squared for uncentred regression)
      data.table(
        rsq = c(
          rsq0 = 1 - crossprod(residuals(x3)) / crossprod(x2[, retsa]),
          rsq1 = 1 - crossprod(residuals(x7)) / crossprod(x6[, retsa])
        ),
        tss = c(crossprod(x2[, retsa]), crossprod(x6[, retsa])),
        nsam = c(nrow(x2), nrow(x6)),
        sigma = c(x5$sigma, x8$sigma),
        type = c("all", "inlier")
      )
    )
    x19 <- list(
      estdt = x12,
      pvid = x17,
      fit = x16,
      stat = x18,
      nx = nxx
    )
    x19
  }
f230311b <-
  function(
      # FIS: parallel wrapper to f230311a
      geo,
      steppra = "now\\ver002\\07pra",
      stepsip = "now\\ver001\\02sip",
      steprip = "now\\ver001\\03rip",
      parx = (3 < geo[, length(unique(nx))]),
      tssdo = T,
      onepass = F,
      q1 = .1) {
    sfInit(par = parx, cpus = min(ncpus(), geo[, length(sort(unique(nx)))]))
    x1 <- sfLapply(
      geo[, sort(unique(nx))],
      f230311a,
      geo = geo,
      steppra = steppra,
      steprip = steprip,
      stepsip = stepsip,
      onepass = onepass,
      q1 = q1
    )
    sfStop()
    t1 <- max(names(coread(step = steppra)) %>% .[grep("....-..-..", .)]) # parse out final date
    names(x1) <- paste0(t1, ".", zeroprepend(seq_along(x1), 3)) # list label for 1206a
    x2 <- f221206a(bso = x1, geo = geo)
    x2a <- NULL
    if (tssdo) {
      x2a <- f230531a(x2) # timeseries summary
    }
    x4 <- list(
      geo = geo,
      ses = x2, # soar estdt summary
      bso = x1, # big solution = record level
      tss = x2a
    )
    x4
  }
f230312a <-
  function(
      # solve single nx -> estdt with no pra
      nxx = geo[, min(nx)],
      stepdfn = "ver001\\06dfn",
      stepgeo = "ver001\\04geo",
      steprip = "ver001\\03rip",
      dfn = dfnx,
      geo = geo0,
      outthresh = .1,
      newused = c(".", "N", "U"),
      houseflat = c(".", "H", "F") # typex field added to rip 240826, values UH/NH/UF/NF for new/house
      ) {
    newused <- match.arg(newused)
    houseflat <- match.arg(houseflat)
    x1 <- # rip read
      coread(
        rcx = geo[nx == nxx][, rc9],
        step = steprip,
        colClasses = list(numeric = c("retsa"), Date = c("buydate", "selldate")) #' retraw',
      )
    if (
      "type" %in% names(x1) & # for backward compat.
        (paste0(newused, houseflat) != "..") # default=no screen
    ) {
      x1 <- x1[grep(paste0("^", newused, houseflat, "$"), type)]
    }
    x2 <- f221209a(
      geo = geo[nx == nxx],
      fur = x1,
      dfn = dfn,
      applygeo = F
    )
    x4 <- lm(
      retsa ~ . - 1,
      x2[, !c("idhash.selldate", "rc9")] # all
    )
    x5 <- residuals(x4)
    x6a <- x2[, .(idhash.selldate, rc6 = substr(rc9, 1, 6), res = x5)] # per rc6
    x6b <- x6a[, .(lo = quantile(res, outthresh / 2), hi = quantile(res, (1 - outthresh / 2))), rc6] # apply thresholds
    x6c <- x6b[x6a, on = c(rc6 = "rc6")][(lo < res & res < hi), .(rc6, idhash.selldate)] # select inlier
    x6d <- x2[x6c[, .(idhash.selldate)], on = c(idhash.selldate = "idhash.selldate")]
    x6e <- x6d[, !c("idhash.selldate", "rc9")]
    x6 <- lm(
      retsa ~ . - 1,
      x6e
    ) %>%
      .[["coefficients"]] %>%
      data.table(
        xdotd = as.numeric(.),
        date = as.Date(substr(names(.), 2, 11))
      ) %>%
      .[, days := as.numeric(diff(c(min(dfn), date)))] %>%
      .[, xdot := as.numeric(xdotd * days)] %>%
      .[, x := cumsum(xdot)] %>%
      .[, .(nx = nxx, rsqraw = summary(x4)$r.squared, date, xdotd, days, xdot, x, lab = geo[nx == nxx][1, lab])] %>%
      .[, ii := 1:.N, lab] %>%
      .[, col := as.factor(lab)]
    x6
  }
f230312x <-
  function(
      ...,
      d0 = as.Date("1994-12-31"),
      newused = c(".", "N", "U"),
      houseflat = c(".", "H", "F") # typex field added to rip 240826, values UH/NH/UF/NF for new/house
      ) {
    newused <- match.arg(newused)
    houseflat <- match.arg(houseflat)
    x1 <- f230312a(..., newused = newused, houseflat = houseflat)
    x2 <- copy(x1)[, date1 := date][, date0 := c(as.Date(d0), date[-.N])][, xdot.daily := xdotd]
    x2
  }
f230506a <-
  function(
      # PRJ  geo_nx -> x_nx -> PRJ_nx|pca={beta,theta|pca}(nx)
      nxx = 3,
      estdtx = x141$fis$ses$estdt,
      pcax = x141$rib$pca,
      kbar = 3) {
    x1 <- estdtx[nx == nxx][, lab := rc3]
    x2 <- pcaz(pcax)[, 1:kbar, drop = F]
    x3 <- data.table(x = x1[, xdot], x2)
    x4 <- lm(x ~ ., x3)
    x5 <- summary(x4)
    x6 <- x4$coefficients[(1 + (1:kbar))]
    x7 <- atan2(x6[3], x6[2]) %>% # put into range (-1/2 to +3/2)pi
      `+`(x = ., y = ifelse(. < (-pi / 2), 2 * pi, 0))
    lapply(as.data.table(as.list(x6)), class)
    x8 <- as.data.table(as.list(c(as.list(x6), x7, nx = nxx, lab = x1[1, rc3]))) %>%
      setnames(., c(paste0("b", 1:kbar), "theta", "nx", "lab")) %>% # beta
      .[, r := sqrt(b2^2 + b3^2)] %>% # a=polar radius
      .[, rsq := x5$r.squared] %>% # LFM
      .[, rbarsq := x5$adj.r.squared] %>% # LFM
      .[, a := x5$coefficients[1]] %>% # a = intercept
      .[, at := x5$coefficients[1, 3]] # t(a)
    x9 <- list(
      pca = pcax, # LFM
      x = x1, # x(t)
      beta = x8
    )
    x9
  }
f230531a <-
  function(
      # timeseries summary for fis$tss
      ses = x103$ses, # fis 230311b
      meanwin = Inf) {
    x2 <- # join parts of 311b for stats
      ses$estdt %>%
      .[ses$stat[type == "all", .(nx, rsq, nsam)], on = c(nx = "nx"), allow = T, nomatch = NULL]
    x3 <- #' beta1'
      x2[, .(date = date1, lab = rc3, xdot)] %>%
      dcast(., date ~ lab, value.var = "xdot") %>%
      pcaest(., rotate = T) %>% # rotate for rewarded beta
      pcajscale(beta = T) %>% # capm style
      pcab(.) %>%
      .[, 1, drop = F] %>%
      data.table(., keep.rownames = T) %>%
      setnames(., c("rc3", "beta"))
    x4 <- # tseries properties
      x2[, .(
        mean = mean(tail(xdot, meanwin)),
        sigma = sd(xdot),
        rho = acf(xdot, lag.max = 1, plot = F)$acf[2],
        rsqraw = rsq[1],
        dd = min(xdot)
      ), nx] %>% # join fundamentals
      as.data.table(.) %>%
      .[unique(x2[, .(nx, lab = rc3)]), on = c(nx = "nx")] %>%
      .[x3, on = c(lab = "rc3")] %>%
      .[order(nx), .(rc3 = lab, mean, sigma, rho, dd, beta, rsqraw)] %>%
      .[, .(rcx = rc3, mean, sigma, rho, dd, beta = beta / mean(beta), rsqraw)]
    x4
  }
f230602a <-
  function(
      # pva 'present value' aggregation up postcode tree
      fis = x103 # fis 230311b  'solution 1'
      ) {
    x0 <- fis$ses$soar[, log(sum(pv) / sum(m2))]
    x1 <- fis$ses$soar %>% # soar: SOlve+reprice/Aggregate/Rank
      .[, .(
        nid,
        m2,
        pv,
        pt = log(ppm2) - x0,
        ppm2,
        rcx = rc9 #---------------------rc9=sector
      )] %>%
      rbind(
        .,
        .[
          , .(
            nid = sum(nid),
            m2 = sum(m2),
            pv = sum(pv),
            pt = log(sum(pv) / sum(m2)) - x0,
            ppm2 = sum(pv) / sum(m2)
          ),
          .(
            rcx = substr(rcx, 1, 6) #-rc6=district
          )
        ]
      ) %>%
      rbind(
        .,
        .[
          nchar(rcx) == 9, .(
            nid = sum(nid),
            m2 = sum(m2),
            pv = sum(pv),
            pt = log(sum(pv) / sum(m2)) - x0,
            ppm2 = sum(pv) / sum(m2)
          ),
          .(
            rcx = substr(rcx, 1, 3) #--rc3=area
          )
        ]
      )
    x1[]
  }
f230603a <-
  function(
      # attribute r to mkt,cyc,res
      nx = 1,
      t0 = as.Date("1994-12-31"),
      fis = x141a,
      kat = list(m = 1, c = 2:3),
      rib = x142) {
    x0 <- fis$bso[[nx]][["fit"]][, buydate := as.Date(buydate)]
    x1 <- pcaz(rib$pca)[, unlist(kat)]
    x2 <- # zdot daily rate calced by zdot/days
      x1 %>%
      sweep(., STAT = as.numeric(diff(c(t0, index(x1)))), MAR = 1, FUN = `/`) %>%
      data.table(.) %>%
      setnames(., paste0("z", 1:3)) %>%
      .[, .(date = index(x1), days = as.numeric(diff(c(t0, index(x1)))), z1, z2, z3)]
    x3 <- # assigned daily, cumulative
      data.table(daily = seq.Date(from = t0, to = max(index(x1)), by = "d")) %>%
      x2[., on = c(date = "daily"), roll = -Inf] %>%
      .[, .(date, days, z1 = cumsum(z1), z2 = cumsum(z2), z3 = cumsum(z3))]
    x4 <- # product with beta
      x3[, -c("date", "days")] %>%
      sweep(., STAT = as.matrix(rib$beta[, unlist(kat), with = F])[nx, , drop = T], MAR = 2, FUN = `*`) %>%
      data.table(.) %>%
      .[, .(m = z1, c = z2 + z3)] %>%
      cbind(x3[, .(date, days)], .)
    x5 <- # join buy, sell, and collate
      x4[, .(date, mbuy = m, cbuy = c)][x0, on = c(date = "buydate")] %>%
      .[, selldate := as.Date(substr(idhash.selldate, 18, 30))] %>%
      .[x4[, .(date, msell = m, csell = c)], on = c(selldate = "date"), nomatch = NULL] %>% # ,days=selldate-date
      .[, .(
        idhash.selldate,
        buydate = date,
        days = as.integer(selldate - date),
        fit2.m = msell - mbuy,
        fit2.c = csell - cbuy,
        fit2.r = fit - ((msell - mbuy) + (csell - cbuy)),
        fit2.t = fit,
        res1 = res,
        tot = fit + res
      )]
    x5
  }
f230603b <-
  function(
      # summary of attribution
      fis = x141$fis,
      rib = x141$rib,
      t0 = as.Date("1994-12-31"),
      kat = list(m = 1, c = 2:3)) {
    x1 <- as.list(seq_along(fis$bso))
    x2 <- lapply(x1, f230603a, fis = fis, kat = kat, rib = rib)
    x3 <- rbindlist(x2)
    x5 <- x3[, .(fit2.m, fit2.c, fit2.r, fit2.t, res1, tot, days)]
    x6 <- rbind(
      as.data.table(lapply(x5, mean)),
      as.data.table(lapply(x5, sd)),
      as.data.table(lapply(x5^2, sum)),
      as.data.table(lapply(x5, mean))
    ) %>%
      cbind(data.table(sum = c("mean", "sd", "sse", "mse")), .)
    x8 <- x9 <- as.list(NULL)
    ix <- 6
    x <- lm(fit2.t ~ days + fit2.m + fit2.c, x5)
    x8[[ix]] <- broom::glance(x) %>%
      data.table(.) %>%
      .[, lm := ix]
    x9[[ix]] <- broom::tidy(x) %>%
      data.table(.) %>%
      .[, lm := ix]
    ix <- ix - 1
    x <- lm(tot ~ days + fit2.m + fit2.c, x5)
    x8[[ix]] <- broom::glance(x) %>%
      data.table(.) %>%
      .[, lm := ix]
    x9[[ix]] <- broom::tidy(x) %>%
      data.table(.) %>%
      .[, lm := ix]
    ix <- ix - 1
    x <- lm(tot ~ days + fit2.m, x5)
    x8[[ix]] <- broom::glance(x) %>%
      data.table(.) %>%
      .[, lm := ix]
    x9[[ix]] <- broom::tidy(x) %>%
      data.table(.) %>%
      .[, lm := ix]
    ix <- ix - 1
    x <- lm(tot ~ fit2.m, x5)
    x8[[ix]] <- broom::glance(x) %>%
      data.table(.) %>%
      .[, lm := ix]
    x9[[ix]] <- broom::tidy(x) %>%
      data.table(.) %>%
      .[, lm := ix]
    ix <- ix - 1
    x <- lm(tot ~ days, x5)
    x8[[ix]] <- broom::glance(x) %>%
      data.table(.) %>%
      .[, lm := ix]
    x9[[ix]] <- broom::tidy(x) %>%
      data.table(.) %>%
      .[, lm := ix]
    ix <- ix - 1
    x <- lm(tot ~ fit2.m + fit2.c, x5) # new
    x8[[ix]] <- broom::glance(x) %>%
      data.table(.) %>%
      .[, lm := ix]
    x9[[ix]] <- broom::tidy(x) %>%
      data.table(.) %>%
      .[, lm := ix]
    x10 <- list(
      summary = x6,
      glance = rbindlist(rev(x8)),
      tidy = rbindlist(rev(x9))
    )
    x10
  }
f230605a <-
  function(
      # AIO : regress r on const,days,mkt,c; also regress fit=(m+c+r) on d,m,c - used in DTC
      geo = x131[, .(rc9, nx, lab = labxnnn(nx))],
      steppra = "ver002\\07pra",
      stepsip = "ver001\\02sip",
      steprip = "ver001\\03rip",
      pcax = x133,
      parx = (3 < geo[, length(unique(nx))]), # added 231028 // execution
      exbso = T, # exclude bso='big solution'
      circlep = NULL, # NULL for recalc, else x142
      outthreshx = .1) {
    print("fis")
    x1 <- # FIS
      f230311b( # //parallel
        geo = geo,
        steppra = steppra, # ver002=drc
        steprip = steprip,
        stepsip = stepsip,
        parx = parx,
        q1 = outthreshx
      ) # //
    x1$fis$soar <- NULL # because should only use soar from pva
    print("prj")
    # x2 <- #PRJ #replaced 231106, should be back-compat
    #   f230506b(
    #     estdtx=x1$ses$estdt, #regressand x
    #     pcax=pcax #regressor z
    #   )
    x2 <- # PRJ
      f231106a(
        estdtx = x1$ses$estdt, # regressand x
        pcax = pcax, # regressor z
        circlep = circlep
      )
    print("att")
    x3 <- # ATT
      f230603b(
        fis = x1,
        rib = x2
      )
    print("combo")
    x4 <- # COMBO
      f230605b(
        x3 = x3
      )
    print("sse")
    x5 <- f231105a(x1$bso)
    if (exbso) {
      x1 <- x1[names(x1) != "bso"] # bso is big
    }
    x6 <- list(
      fis = x1, # bso is big 231028: allowed bso -> big object, use with caution
      rib = x2,
      att = x3,
      combo = x4,
      sse = x5
    )
    print("cpars")
    x7 <- # CPARS
      f230928c(
        aio = x6 # aio but without cpars - cpars can be overwritten in another function
      )
    x6$cpars <- x7 # add cpars w phi r c2 c3
    x6[c("fis", "rib", "att", "combo", "sse", "cpars")]
  }
f230605b <-
  function(
      # COMBO regression test results reorg
      x3) {
    coname <- c("a0", "ad", "am", "ac")
    x4 <- # COMBO : a0=intercept; ad=days, am=market k=1, ac=cycle k=2,3, ay=ad*365.25
      rbind( # all have constant labelled a0 ; first lm=5 is 'special' with fit on lhs
        cbind(
          x3$glance[lm == 6, .(r.squared, sigma)],
          as.data.table(
            as.list(
              c(
                x3$tidy[lm == 6, estimate],
                x3$tidy[lm == 6, std.error]
              )
            )
          ) %>%
            setnames(., c(coname, paste0(coname, ".se"))) %>% .[, lm := 6] # fit~0,d,m,c
        ) #  0,d,m,c row6
        ,
        cbind(
          x3$glance[lm == 1, .(r.squared, sigma)],
          as.data.table(
            as.list(
              c(
                x3$tidy[lm == 1, estimate][1], NA, x3$tidy[lm == 1, estimate][2:3],
                x3$tidy[lm == 1, std.error][1], NA, x3$tidy[lm == 1, std.error][2:3]
              )
            )
          ) %>%
            setnames(., c(coname, paste0(coname, ".se"))) %>% .[, lm := 1] # tot~0,m,c
        ) # 0, ,m,c row1
        ,
        cbind(
          x3$glance[lm == 2, .(r.squared, sigma)],
          as.data.table(
            as.list(
              c(
                x3$tidy[lm == 2, estimate], NA, NA,
                x3$tidy[lm == 2, std.error], NA, NA
              )
            )
          ) %>%
            setnames(., c(coname, paste0(coname, ".se"))) %>% .[, lm := 2] # tot~0,d
        ) #  0,d,, row 2
        ,
        cbind(
          x3$glance[lm == 3, .(r.squared, sigma)],
          as.data.table(
            as.list(
              c(
                x3$tidy[lm == 3, estimate][1], NA, x3$tidy[lm == 3, estimate][2], NA,
                x3$tidy[lm == 3, std.error][1], NA, x3$tidy[lm == 3, std.error][2], NA
              )
            )
          ) %>%
            setnames(., c(coname, paste0(coname, ".se"))) %>% .[, lm := 3]
        ) #  0,,m, row 3
        ,
        cbind(
          x3$glance[lm == 4, .(r.squared, sigma)],
          as.data.table(
            as.list(
              c(
                x3$tidy[lm == 4, estimate], NA,
                x3$tidy[lm == 4, std.error], NA
              )
            )
          ) %>%
            setnames(., c(coname, paste0(coname, ".se"))) %>% .[, lm := 4]
        ) #  0,d,m, row 4
        ,
        cbind(
          x3$glance[lm == 5, .(r.squared, sigma)],
          as.data.table(
            as.list(
              c(
                x3$tidy[lm == 5, estimate],
                x3$tidy[lm == 5, std.error]
              )
            )
          ) %>%
            setnames(., c(coname, paste0(coname, ".se"))) %>% .[, lm := 5]
        ) #  0,d,m,c row 5
      ) %>%
      .[order(lm)] %>%
      .[, .(
        r2 = r.squared,
        sigma,
        a0,
        a0.se,
        am,
        am.se,
        ac,
        ac.se,
        ay = ad * 365.25,
        ay.se = ad.se * 365.25
      )]
    x4
  }
f230616a <-
  function( # load and return selected objects from pit, now
  ) {
    mnem <- "now"
    f1 <- function(mnem = "now") {
      x0 <- dir(paste0(mnem, "/")) %>% .[grep("Rdata$", .)]
      if (length(x0) == 0) {
        x1 <- NULL
        print(paste0("last dump type=", mnem, " rdata not found "))
      } else {
        rdatafile <- file.path(mnem, max(x0))
        print(paste0("last dump type=", mnem, " rdata: ", rdatafile))
        load(rdatafile) # to local namespace
        x1 <- list( # this hard-coded
          car = x135, # aiopca
          var = x161, # var
          aio = x180, # aionuts
          qnut = x190 # aioqnut
        )
      }
      x1
    }
    x2 <- f1("pit")
    x3 <- f1("now")
    list(pit = x2, now = x3)
  }
f230616b <-
  function(
      # grid PIT forecast onto NOW periods
      pitnow = f230616a()) {
    mnow.d <- # mnow(date) using linear interp
      pitnow$now$car$fis$ses$estdt[, .(date0, date1)] %>%
      unique(.) %>%
      .[, m := -(.N - 1):0] %>%
      .[, approxfun(x = date1, y = m)] # m(date)
    dnowvec <- pitnow$now$car$fis$ses$estdt[, sort(unique(date1))]
    mnow.d(dnowvec) # check: is integer vector ending zero
    dpit <- max(pitnow$pit$car$rib$pca$date)
    mnowpit <- mnow.d(v = dpit) # dpit on now grid
    mneeded <- ceiling(-mnowpit) # first step from mnowpit with mnow>0
    x2 <-
      pitnow$pit$var$var0p3 %>%
      predict(., n.ahead = mneeded)
    x3 <- # forecast zusing pit-var
      rbind(
        pitnow$pit$var$z[nrow(pitnow$pit$var$z), -"ii"], # z(pit); initial value
        cbind(
          x2$fcst$z002[, "fcst"],
          x2$fcst$z003[, "fcst"]
        ),
        use.names = F
      ) %>% # deduct z(pit) to rebase z(pit=0)
      sweep(., MAR = 2, FUN = `-`, STAT = unlist(pitnow$pit$var$z[nrow(pitnow$pit$var$z)][, -"ii"]))
    stopifnot(all.equal(as.numeric(unlist(x3[1, ])), rep(0, 2))) # check
    x4 <- # regrid
      data.table(x3) %>% # training set
      # .[,-'ii']%>%
      setnames(., c("z2", "z3")) %>%
      .[, .(z2, z3, mnow = seq(from = mnowpit, to = 1, by = 1))]
    z2.mnow <- x4[, approxfun(x = mnow, y = z2)]
    z3.mnow <- x4[, approxfun(x = mnow, y = z3)]
    x5 <- c(mnowpit, ceiling(mnowpit):0)
    x6 <- c(dpit, dnowvec[dnowvec > dpit])
    x7 <- # only z are returned
      data.table(
        date = x6,
        mnow = x5,
        z2 = z2.mnow(x5),
        z3 = z3.mnow(x5)
      ) %>%
      .[]
    x7[]
  }
f230616c <-
  function(
      # actuals since dpit
      dpit = as.Date("2016-05-31"),
      d0 = estdt[, max(date1)],
      estdt = x141$fis$ses$estdt,
      histperiods = 6) {
    stopifnot(as.Date("2023-01-01") < estdt[, max(date1)]) # dirty 'now' check
    x1 <-
      data.table(
        daily = seq.Date(from = dpit, to = d0, by = 1)
      )
    nxx <- estdt[, sort(unique(nx))]
    ll <- as.list(NULL)
    i <- 1
    for (i in seq_along(nxx)) {
      ll[[i]] <-
        estdt[nx == nxx[i]] %>%
        .[, .(nx, date0, xdot.daily)] %>%
        .[x1, on = c(date0 = "daily"), roll = Inf] %>%
        .[, .(date0, xdot.daily, x = cumsum(xdot.daily)), nx]
    }
    x2 <- rbindlist(ll) %>%
      .[, .(nx, date = date0, xdot.daily, x, prev6 = estdt[date1 < dpit, .SD[.N + 1 - (histperiods:1)][, sum(xdot)], nx][, V1])]
    x2
  }
f230618a <-
  function(
      # nuts qspread forc, act, median(r); forc is k=2,3 others total
      estdt = pitnow$now$qnut$fis$ses$estdt,
      pitnow = f230616a(), # pitnow (fast)
      fppit = file.path("pit", "ver001", "03rip"),
      fpnow = file.path("now", "ver001", "03rip")) {
    x1 <- # pit z23 forecasts
      f230616b(pitnow = pitnow)
    x2 <- # lut for 1:50
      geo2[, .(nx, code)] %>%
      unique(.) %>%
      .[unique(geo3[, .(n0, qq, nx)]), on = c(nx = "n0")] %>%
      .[, .(nx, code, qq, ix = i.nx)]
    x3 <- # pit rib(qnuts)
      pitnow$pit$qnut$rib$beta %>%
      .[, .(nx, b2, b3)]
    x4 <- # pit xhat(k=2,3)
      as.matrix(x1[.N, .(z2, z3)]) %*% t(x3[, .(b2, b3)]) %>%
      as.numeric(.) %>%
      data.table(nx = 1:50, xhat = .)
    x5 <- # forc + key
      x2[x4, on = c(ix = "nx")] %>%
      .[, .SD[, .(q5 = xhat[5], q1 = xhat[1])], .(nx, code)] %>%
      .[, .(nx, code, q1, q5, qs = q1 - q5, type = "forc")]
    x6 <- # actual xtot since dpit
      f230616c(
        dpit = as.Date("2016-03-31"),
        estdt = estdt,
      )[date == max(date)]
    x7 <- # actual + key
      x2[x6, on = c(ix = "nx")] %>%
      .[order(nx, qq)] %>%
      .[, .SD[, .(q5 = x[5], q1 = x[1], h5 = prev6[5], h1 = prev6[1])], .(nx, code)] %>%
      .[, .(nx, code, q1, q5, qs = q1 - q5, hs = h1 - h5, type = "actual")]
    x8 <- # q1-q5 tot r spread and key
      x5[, .(forc = qs, nx)] %>%
      .[x7[, .(nx, code, act = qs, hist = hs)], on = c(nx = "nx")] # forc x23 and act xtot
    x9 <- geo3 %>%
      .[qq %in% c(1, 5)] %>%
      .[, coread(rcx = rcx, step = fpnow), .(qq, n0)] %>%
      .[buydate > as.Date("2016-06-30")] %>%
      .[buydate < as.Date("2016-12-31")] %>%
      .[selldate > as.Date("2022-09-30")] %>%
      .[
        , .( # r_tot r metric by quintile
          mu = mean(as.numeric(retsa)),
          med = median(as.numeric(retsa)),
          muun = mean(as.numeric(retraw))
        ),
        .(qq = labxnnn(qq), n0)
      ] %>%
      dcast(., n0 ~ qq, value.var = "med") %>%
      .[, .(
        nx = n0,
        r1 = x001, # q1 median
        r5 = x005, # q5 median
        xmed = x001 - x005 # spread
      )]
    x10 <- x9 %>% # join with forc, act
      .[, .(nx, xmed)] %>%
      .[x8, on = c(nx = "nx")] %>%
      .[, .(nx, code, xforc = forc, xact = act, xmed, hist)]
    x10
  }
f230703b <-
  function( # NUTS2 codes A-H
  ) {
    geo2 <-
      structure(list(rc6 = c(
        "AL-1--", "AL-10-", "AL-2--", "AL-3--",
        "AL-4--", "AL-5--", "AL-6--", "AL-7--", "AL-8--", "AL-9--", "B--1--",
        "B--10-", "B--11-", "B--12-", "B--13-", "B--14-", "B--15-", "B--16-",
        "B--17-", "B--18-", "B--19-", "B--2--", "B--20-", "B--21-", "B--23-",
        "B--24-", "B--25-", "B--26-", "B--27-", "B--28-", "B--29-", "B--3--",
        "B--30-", "B--31-", "B--32-", "B--33-", "B--34-", "B--35-", "B--36-",
        "B--37-", "B--38-", "B--4--", "B--40-", "B--42-", "B--43-", "B--44-",
        "B--45-", "B--46-", "B--47-", "B--48-", "B--49-", "B--5--", "B--50-",
        "B--6--", "B--60-", "B--61-", "B--62-", "B--63-", "B--64-", "B--65-",
        "B--66-", "B--67-", "B--68-", "B--69-", "B--7--", "B--70-", "B--71-",
        "B--72-", "B--73-", "B--74-", "B--75-", "B--76-", "B--77-", "B--78-",
        "B--79-", "B--79-", "B--8--", "B--80-", "B--9--", "B--90-", "B--91-",
        "B--92-", "B--93-", "B--94-", "B--95-", "B--96-", "B--97-", "B--98-",
        "B--99-", "BA-1--", "BA-10-", "BA-11-", "BA-12-", "BA-13-", "BA-14-",
        "BA-15-", "BA-16-", "BA-2--", "BA-20-", "BA-21-", "BA-22-", "BA-3--",
        "BA-4--", "BA-5--", "BA-6--", "BA-7--", "BA-8--", "BA-9--", "BB-1--",
        "BB-10-", "BB-11-", "BB-12-", "BB-18-", "BB-18-", "BB-2--", "BB-3--",
        "BB-4--", "BB-5--", "BB-6--", "BB-7--", "BB-8--", "BB-9--", "BB-94-",
        "BD-1--", "BD-10-", "BD-11-", "BD-12-", "BD-13-", "BD-14-", "BD-15-",
        "BD-16-", "BD-17-", "BD-18-", "BD-19-", "BD-2--", "BD-20-", "BD-21-",
        "BD-22-", "BD-23-", "BD-23-", "BD-24-", "BD-3--", "BD-4--", "BD-5--",
        "BD-6--", "BD-7--", "BD-8--", "BD-9--", "BD-97-", "BD-98-", "BD-99-",
        "BH-1--", "BH-10-", "BH-11-", "BH-12-", "BH-13-", "BH-14-", "BH-15-",
        "BH-16-", "BH-17-", "BH-18-", "BH-19-", "BH-2--", "BH-20-", "BH-21-",
        "BH-22-", "BH-23-", "BH-23-", "BH-24-", "BH-24-", "BH-25-", "BH-3--",
        "BH-31-", "BH-31-", "BH-4--", "BH-5--", "BH-6--", "BH-7--", "BH-8--",
        "BH-9--", "BL-0--", "BL-1--", "BL-11-", "BL-2--", "BL-3--", "BL-4--",
        "BL-5--", "BL-6--", "BL-7--", "BL-78-", "BL-8--", "BL-9--", "BN-1--",
        "BN-10-", "BN-11-", "BN-12-", "BN-13-", "BN-14-", "BN-15-", "BN-16-",
        "BN-17-", "BN-18-", "BN-2--", "BN-20-", "BN-21-", "BN-22-", "BN-23-",
        "BN-24-", "BN-25-", "BN-26-", "BN-27-", "BN-3--", "BN-41-", "BN-42-",
        "BN-43-", "BN-44-", "BN-45-", "BN-5--", "BN-50-", "BN-51-", "BN-52-",
        "BN-6--", "BN-7--", "BN-8--", "BN-88-", "BN-9--", "BN-95-", "BN-99-",
        "BR-1--", "BR-2--", "BR-3--", "BR-4--", "BR-5--", "BR-6--", "BR-6--",
        "BR-7--", "BR-8--", "BR-8--", "BS-1--", "BS-10-", "BS-11-", "BS-13-",
        "BS-14-", "BS-15-", "BS-16-", "BS-2--", "BS-20-", "BS-21-", "BS-22-",
        "BS-23-", "BS-24-", "BS-25-", "BS-26-", "BS-27-", "BS-28-", "BS-29-",
        "BS-3--", "BS-30-", "BS-31-", "BS-32-", "BS-34-", "BS-35-", "BS-36-",
        "BS-37-", "BS-39-", "BS-4--", "BS-40-", "BS-41-", "BS-48-", "BS-49-",
        "BS-5--", "BS-6--", "BS-7--", "BS-8--", "BS-9--", "BS-98-", "BS-99-",
        "CA-1--", "CA-10-", "CA-11-", "CA-12-", "CA-13-", "CA-14-", "CA-15-",
        "CA-16-", "CA-17-", "CA-18-", "CA-19-", "CA-2--", "CA-20-", "CA-21-",
        "CA-22-", "CA-23-", "CA-24-", "CA-25-", "CA-26-", "CA-27-", "CA-28-",
        "CA-3--", "CA-4--", "CA-5--", "CA-6--", "CA-7--", "CA-8--", "CA-8--",
        "CA-9--", "CA-9--", "CA-95-", "CB-1--", "CB-10-", "CB-11-", "CB-2--",
        "CB-21-", "CB-22-", "CB-23-", "CB-24-", "CB-25-", "CB-3--", "CB-4--",
        "CB-5--", "CB-6--", "CB-7--", "CB-8--", "CB-9--", "CF-10-", "CF-11-",
        "CF-14-", "CF-15-", "CF-23-", "CF-24-", "CF-3--", "CF-30-", "CF-31-",
        "CF-32-", "CF-33-", "CF-34-", "CF-35-", "CF-36-", "CF-37-", "CF-38-",
        "CF-39-", "CF-40-", "CF-41-", "CF-42-", "CF-43-", "CF-44-", "CF-45-",
        "CF-46-", "CF-47-", "CF-48-", "CF-5--", "CF-61-", "CF-62-", "CF-63-",
        "CF-64-", "SO-21-", "SO-22-", "SO-23-", "SO-24-", "SO-25-", "SO-30-",
        "SO-31-", "SO-32-", "SO-40-", "SO-41-", "SO-42-", "SO-43-", "SO-45-",
        "SO-50-", "SO-51-", "SO-51-", "SO-52-", "SO-53-", "SO-97-", "SP-1--",
        "SP-10-", "SP-11-", "SP-11-", "SP-2--", "SP-3--", "SP-4--", "SP-4--",
        "SP-5--", "SP-5--", "SP-6--", "SP-6--", "SP-7--", "SP-8--", "SP-9--",
        "SP-9--", "SR-1--", "SR-2--", "SR-3--", "SR-4--", "SR-5--", "SR-6--",
        "SR-7--", "SR-8--", "SS-0--", "SS-1--", "SS-11-", "SS-12-", "SS-13-",
        "SS-14-", "SS-15-", "SS-16-", "SS-17-", "SS-2--", "SS-22-", "SS-3--",
        "SS-4--", "SS-5--", "SS-6--", "SS-7--", "SS-8--", "SS-9--", "SS-99-",
        "ST-1--", "ST-10-", "ST-11-", "ST-12-", "ST-13-", "ST-14-", "ST-14-",
        "ST-15-", "ST-16-", "ST-17-", "ST-18-", "ST-19-", "ST-2--", "ST-20-",
        "ST-21-", "ST-3--", "ST-4--", "ST-5--", "ST-55-", "ST-6--", "ST-7--",
        "ST-7--", "ST-8--", "ST-9--", "SW-10-", "SW-11-", "SW-12-", "SW-13-",
        "SW-14-", "SW-15-", "SW-16-", "SW-17-", "SW-18-", "SW-19-", "SW-1A-",
        "SW-1E-", "SW-1H-", "SW-1P-", "SW-1V-", "SW-1W-", "SW-1X-", "SW-1Y-",
        "SW-2--", "SW-20-", "SW-3--", "SW-4--", "SW-5--", "SW-6--", "SW-7--",
        "SW-8--", "SW-9--", "SW-95-", "SY-1--", "SY-10-", "SY-10-", "SY-11-",
        "SY-12-", "SY-13-", "SY-13-", "SY-13-", "SY-14-", "SY-14-", "SY-14-",
        "SY-15-", "SY-15-", "SY-16-", "SY-17-", "SY-18-", "SY-19-", "SY-2--",
        "SY-20-", "SY-21-", "SY-21-", "SY-22-", "SY-22-", "SY-23-", "SY-24-",
        "SY-25-", "SY-3--", "SY-4--", "SY-5--", "SY-5--", "SY-6--", "SY-7--",
        "SY-7--", "SY-8--", "SY-9--", "SY-9--", "SY-99-", "TA-1--", "TA-10-",
        "TA-11-", "TA-12-", "TA-13-", "TA-14-", "TA-15-", "TA-16-", "TA-17-",
        "TA-18-", "TA-19-", "TA-2--", "TA-20-", "TA-21-", "TA-22-", "TA-23-",
        "TA-24-", "TA-3--", "TA-4--", "TA-5--", "TA-6--", "TA-7--", "TA-8--",
        "TA-9--", "TD-12-", "TD-15-", "TD-5--", "TD-9--", "TF-1--", "TF-10-",
        "TF-11-", "TF-12-", "TF-13-", "TF-2--", "TF-3--", "TF-4--", "TF-5--",
        "TF-6--", "TF-7--", "TF-8--", "TF-9--", "TF-9--", "TN-1--", "TN-10-",
        "TN-11-", "TN-12-", "TN-13-", "TN-14-", "TN-14-", "TN-15-", "TN-16-",
        "TN-16-", "TN-17-", "TN-18-", "TN-19-", "TN-2--", "TN-20-", "TN-21-",
        "TN-22-", "TN-23-", "TN-24-", "TN-25-", "TN-26-", "TN-27-", "TN-28-",
        "TN-29-", "TN-3--", "TN-30-", "TN-31-", "TN-32-", "TN-33-", "TN-34-",
        "TN-35-", "TN-36-", "TN-37-", "TN-38-", "TN-39-", "TN-4--", "TN-40-",
        "TN-5--", "TN-6--", "TN-7--", "TN-8--", "TN-9--", "TQ-1--", "TQ-10-",
        "TQ-11-", "TQ-12-", "TQ-13-", "TQ-14-", "TQ-2--", "TQ-3--", "TQ-4--",
        "TQ-5--", "TQ-6--", "TQ-7--", "TQ-8--", "TQ-9--", "TR-1--", "TR-10-",
        "TR-11-", "TR-12-", "TR-13-", "TR-14-", "TR-15-", "TR-16-", "TR-17-",
        "TR-18-", "TR-19-", "TR-2--", "TR-20-", "TR-21-", "TR-22-", "TR-23-",
        "TR-24-", "TR-25-", "TR-26-", "TR-27-", "TR-3--", "TR-4--", "TR-5--",
        "TR-6--", "TR-7--", "TR-8--", "TR-9--", "TS-1--", "TS-10-", "TS-11-",
        "TS-12-", "TS-13-", "TS-13-", "TS-14-", "TS-15-", "TS-15-", "TS-16-",
        "TS-17-", "TS-18-", "TS-19-", "TS-2--", "TS-20-", "TS-21-", "TS-22-",
        "TS-23-", "TS-24-", "TS-25-", "TS-26-", "TS-27-", "TS-28-", "TS-29-",
        "TS-3--", "TS-4--", "TS-5--", "TS-6--", "TS-7--", "TS-7--", "TS-8--",
        "TS-8--", "TS-9--", "TS-9--", "TW-1--", "TW-10-", "TW-11-", "TW-12-",
        "TW-12-", "TW-13-", "TW-14-", "TW-14-", "TW-15-", "TW-15-", "TW-16-",
        "TW-17-", "TW-18-", "TW-19-", "TW-19-", "TW-2--", "TW-20-", "TW-3--",
        "TW-4--", "TW-5--", "TW-6--", "TW-6--", "TW-7--", "TW-8--", "TW-9--",
        "UB-1--", "UB-10-", "UB-11-", "UB-18-", "UB-2--", "UB-3--", "UB-4--",
        "UB-5--", "UB-6--", "UB-7--", "UB-8--", "UB-8--", "UB-9--", "UB-9--",
        "UB-9--", "W--10-", "W--11-", "W--12-", "W--13-", "W--14-", "W--1A-",
        "W--1B-", "W--1C-", "W--1D-", "W--1F-", "W--1G-", "W--1H-", "W--1J-",
        "W--1K-", "W--1S-", "W--1T-", "W--1U-", "W--1W-", "W--2--", "W--3--",
        "W--4--", "W--5--", "W--6--", "W--7--", "W--8--", "W--9--", "WA-1--",
        "WA-10-", "WA-11-", "WA-12-", "WA-13-", "WA-14-", "WA-15-", "WA-16-",
        "WA-2--", "WA-3--", "WA-4--", "WA-5--", "WA-55-", "WA-6--", "WA-7--",
        "WA-8--", "WA-88-", "WA-9--", "WC-1A-", "WC-1B-", "WC-1E-", "WC-1H-",
        "WC-1N-", "WC-1R-", "WC-1V-", "WC-1X-", "WC-2A-", "WC-2B-", "WC-2E-",
        "WC-2H-", "WC-2N-", "WC-2R-", "WD-17-", "WD-18-", "WD-19-", "WD-23-",
        "WD-23-", "WD-24-", "WD-25-", "WD-3--", "WD-3--", "WD-3--", "WD-4--",
        "WD-5--", "WD-6--", "WD-6--", "WD-7--", "WD-99-", "WF-1--", "WF-10-",
        "WF-11-", "WF-12-", "WF-13-", "WF-14-", "WF-15-", "WF-16-", "WF-17-",
        "WF-2--", "WF-3--", "WF-4--", "WF-5--", "WF-6--", "WF-7--", "WF-8--",
        "WF-9--", "WF-90-", "WN-1--", "WN-2--", "WN-3--", "WN-4--", "WN-5--",
        "WN-6--", "WN-7--", "WN-8--", "WR-1--", "WR-10-", "WR-11-", "WR-11-",
        "WR-12-", "WR-12-", "WR-13-", "WR-13-", "WR-14-", "WR-15-", "WR-2--",
        "WR-3--", "WR-4--", "WR-5--", "WR-6--", "WR-7--", "WR-8--", "WR-8--",
        "WR-9--", "WR-99-", "WS-1--", "WS-10-", "WS-11-", "WS-12-", "WS-13-",
        "WS-14-", "WS-15-", "WS-2--", "WS-3--", "WS-4--", "WS-5--", "WS-6--",
        "WS-7--", "WS-8--", "WS-9--", "WV-1--", "WV-10-", "WV-11-", "WV-12-",
        "WV-13-", "WV-14-", "WV-15-", "WV-16-", "WV-2--", "WV-3--", "WV-4--",
        "WV-5--", "WV-6--", "WV-7--", "WV-8--", "WV-9--", "WV-98-", "WV-99-",
        "YO-1--", "YO-10-", "YO-11-", "YO-12-", "YO-13-", "YO-14-", "YO-15-",
        "YO-16-", "YO-17-", "YO-18-", "YO-19-", "YO-21-", "YO-22-", "YO-23-",
        "YO-24-", "YO-25-", "YO-26-", "YO-30-", "YO-31-", "YO-32-", "YO-41-",
        "YO-42-", "YO-43-", "YO-51-", "YO-60-", "YO-61-", "YO-62-", "YO-7--",
        "YO-8--", "YO-90-", "CF-71-", "CF-72-", "CF-81-", "CF-82-", "CF-83-",
        "CF-91-", "CF-95-", "CF-99-", "CH-1--", "CH-1--", "CH-2--", "CH-25-",
        "CH-26-", "CH-27-", "CH-28-", "CH-29-", "CH-3--", "CH-30-", "CH-31-",
        "CH-32-", "CH-33-", "CH-34-", "CH-4--", "CH-4--", "CH-41-", "CH-42-",
        "CH-43-", "CH-44-", "CH-45-", "CH-46-", "CH-47-", "CH-48-", "CH-49-",
        "CH-5--", "CH-6--", "CH-60-", "CH-61-", "CH-62-", "CH-63-", "CH-64-",
        "CH-65-", "CH-66-", "CH-7--", "CH-70-", "CH-8--", "CH-88-", "CH-99-",
        "CM-0--", "CM-1--", "CM-11-", "CM-12-", "CM-13-", "CM-13-", "CM-14-",
        "CM-14-", "CM-15-", "CM-16-", "CM-17-", "CM-18-", "CM-19-", "CM-2--",
        "CM-20-", "CM-21-", "CM-22-", "CM-23-", "CM-24-", "CM-3--", "CM-4--",
        "CM-5--", "CM-6--", "CM-7--", "CM-77-", "CM-8--", "CM-9--", "CM-92-",
        "CM-98-", "CM-99-", "CO-1--", "CO-10-", "CO-11-", "CO-12-", "CO-13-",
        "CO-14-", "CO-15-", "CO-16-", "CO-2--", "CO-3--", "CO-4--", "CO-5--",
        "CO-6--", "CO-7--", "CO-8--", "CO-9--", "CR-0--", "CR-2--", "CR-3--",
        "CR-3--", "CR-4--", "CR-44-", "CR-5--", "CR-5--", "CR-6--", "CR-6--",
        "CR-7--", "CR-8--", "CR-8--", "CR-9--", "CR-90-", "CT-1--", "CT-10-",
        "CT-11-", "CT-12-", "CT-13-", "CT-14-", "CT-15-", "CT-16-", "CT-17-",
        "CT-18-", "CT-19-", "CT-2--", "CT-20-", "CT-21-", "CT-3--", "CT-4--",
        "CT-5--", "CT-50-", "CT-6--", "CT-7--", "CT-8--", "CT-9--", "CV-1--",
        "CV-10-", "CV-10-", "CV-11-", "CV-11-", "CV-12-", "CV-13-", "CV-13-",
        "CV-2--", "CV-21-", "CV-22-", "CV-23-", "CV-23-", "CV-3--", "CV-31-",
        "CV-32-", "CV-33-", "CV-34-", "CV-35-", "CV-36-", "CV-37-", "CV-4--",
        "CV-47-", "CV-5--", "CV-6--", "CV-7--", "CV-8--", "CV-9--", "CV-9--",
        "CW-1--", "CW-10-", "CW-11-", "CW-12-", "CW-12-", "CW-2--", "CW-2--",
        "CW-3--", "CW-3--", "CW-4--", "CW-5--", "CW-6--", "CW-7--", "CW-8--",
        "CW-9--", "CW-98-", "DA-1--", "DA-1--", "DA-10-", "DA-11-", "DA-12-",
        "DA-13-", "DA-14-", "DA-14-", "DA-15-", "DA-16-", "DA-17-", "DA-17-",
        "DA-18-", "DA-2--", "DA-3--", "DA-4--", "DA-5--", "DA-5--", "DA-6--",
        "DA-7--", "DA-8--", "DA-9--", "DE-1--", "DE-11-", "DE-12-", "DE-13-",
        "DE-13-", "DE-14-", "DE-15-", "DE-15-", "DE-21-", "DE-22-", "DE-23-",
        "DE-24-", "DE-3--", "DE-4--", "DE-45-", "DE-5--", "DE-55-", "DE-56-",
        "DE-6--", "DE-6--", "DE-65-", "DE-7--", "DE-72-", "DE-73-", "DE-74-",
        "DE-75-", "DE-99-", "DG-16-", "DH-1--", "DH-2--", "DH-3--", "DH-4--",
        "DH-5--", "DH-6--", "DH-7--", "DH-8--", "DH-9--", "DH-97-", "DH-98-",
        "DH-99-", "DL-1--", "DL-10-", "DL-11-", "DL-11-", "DL-12-", "DL-12-",
        "DL-12-", "DL-13-", "DL-14-", "DL-15-", "DL-16-", "DL-17-", "DL-2--",
        "DL-2--", "DL-3--", "DL-4--", "DL-5--", "DL-6--", "DL-7--", "DL-8--",
        "DL-9--", "DL-98-", "DN-1--", "DN-10-", "DN-10-", "DN-11-", "DN-11-",
        "DN-12-", "DN-14-", "DN-15-", "DN-16-", "DN-17-", "DN-17-", "DN-18-",
        "DN-19-", "DN-2--", "DN-20-", "DN-20-", "DN-21-", "DN-21-", "DN-22-",
        "DN-3--", "DN-31-", "DN-32-", "DN-33-", "DN-34-", "DN-35-", "DN-36-",
        "DN-36-", "DN-37-", "DN-37-", "DN-38-", "DN-38-", "DN-39-", "DN-39-",
        "DN-4--", "DN-40-", "DN-41-", "DN-41-", "DN-5--", "DN-55-", "DN-6--",
        "DN-7--", "DN-8--", "DN-9--", "DN-9--", "DT-1--", "DT-10-", "DT-11-",
        "DT-2--", "DT-3--", "DT-4--", "DT-5--", "DT-6--", "DT-7--", "DT-8--",
        "DT-9--", "DY-1--", "DY-10-", "DY-11-", "DY-12-", "DY-13-", "DY-14-",
        "DY-2--", "DY-3--", "DY-4--", "DY-5--", "DY-6--", "DY-7--", "DY-8--",
        "DY-9--", "E--1--", "E--10-", "E--11-", "E--12-", "E--13-", "E--14-",
        "E--15-", "E--16-", "E--17-", "E--18-", "E--1W-", "E--2--", "E--20-",
        "E--3--", "E--4--", "E--4--", "E--5--", "E--6--", "E--7--", "E--8--",
        "E--9--", "E--98-", "EC-1A-", "EC-1M-", "EC-1N-", "EC-1P-", "EC-1R-",
        "EC-1V-", "EC-1Y-", "EC-2A-", "EC-2M-", "EC-2N-", "EC-2P-", "EC-2R-",
        "EC-2V-", "EC-2Y-", "EC-3A-", "EC-3M-", "EC-3N-", "EC-3P-", "EC-3R-",
        "EC-3V-", "EC-4A-", "EC-4M-", "EC-4N-", "EC-4P-", "EC-4R-", "EC-4V-",
        "EC-4Y-", "EN-1--", "EN-10-", "EN-11-", "EN-2--", "EN-2--", "EN-3--",
        "EN-4--", "EN-4--", "EN-5--", "EN-5--", "EN-6--", "EN-6--", "EN-7--",
        "EN-7--", "EN-8--", "EN-8--", "EN-9--", "EN-9--", "EX-1--", "EX-10-",
        "EX-11-", "EX-12-", "EX-13-", "EX-14-", "EX-15-", "EX-16-", "EX-17-",
        "EX-18-", "EX-19-", "EX-2--", "EX-20-", "EX-21-", "EX-22-", "EX-23-",
        "EX-24-", "EX-3--", "EX-31-", "EX-32-", "EX-33-", "EX-34-", "EX-35-",
        "EX-36-", "EX-37-", "EX-38-", "EX-39-", "EX-4--", "EX-5--", "EX-6--",
        "EX-7--", "EX-8--", "EX-9--", "FY-1--", "FY-2--", "FY-3--", "FY-4--",
        "FY-5--", "FY-6--", "FY-7--", "FY-8--", "GL-1--", "GL-10-", "GL-11-",
        "GL-12-", "GL-13-", "GL-14-", "GL-15-", "GL-16-", "GL-16-", "GL-17-",
        "GL-17-", "GL-18-", "GL-18-", "GL-19-", "GL-19-", "GL-2--", "GL-20-",
        "GL-20-", "GL-3--", "GL-4--", "GL-5--", "GL-50-", "GL-51-", "GL-52-",
        "GL-53-", "GL-54-", "GL-55-", "GL-55-", "GL-56-", "GL-56-", "GL-56-",
        "GL-6--", "GL-7--", "GL-7--", "GL-8--", "GL-9--", "GU-1--", "GU-10-",
        "GU-11-", "GU-12-", "GU-14-", "GU-15-", "GU-16-", "GU-17-", "GU-18-",
        "GU-19-", "GU-2--", "GU-20-", "GU-21-", "GU-22-", "GU-23-", "GU-24-",
        "GU-25-", "GU-26-", "GU-27-", "GU-28-", "GU-29-", "GU-3--", "GU-30-",
        "GU-31-", "GU-32-", "GU-33-", "GU-34-", "GU-35-", "GU-4--", "GU-46-",
        "GU-47-", "GU-5--", "GU-51-", "GU-52-", "GU-6--", "GU-7--", "GU-8--",
        "GU-9--", "HA-0--", "HA-1--", "HA-2--", "HA-3--", "HA-4--", "HA-5--",
        "HA-5--", "HA-6--", "HA-6--", "HA-7--", "HA-7--", "HA-8--", "HA-9--",
        "HD-1--", "HD-2--", "HD-3--", "HD-4--", "HD-5--", "HD-6--", "HD-7--",
        "HD-8--", "HD-9--", "HG-1--", "HG-2--", "HG-3--", "HG-4--", "HG-5--",
        "HP-1--", "HP-10-", "HP-11-", "HP-12-", "HP-13-", "HP-14-", "HP-15-",
        "HP-16-", "HP-17-", "HP-18-", "HP-19-", "HP-2--", "HP-20-", "HP-21-",
        "HP-22-", "HP-23-", "HP-23-", "HP-27-", "HP-3--", "HP-4--", "HP-4--",
        "HP-5--", "HP-5--", "HP-6--", "HP-7--", "HP-8--", "HP-8--", "HP-9--",
        "HR-1--", "HR-2--", "HR-3--", "HR-3--", "HR-4--", "HR-5--", "HR-5--",
        "HR-6--", "HR-7--", "HR-8--", "HR-8--", "HR-9--", "HR-9--", "HU-1--",
        "HU-10-", "HU-11-", "HU-12-", "HU-13-", "HU-14-", "HU-15-", "HU-16-",
        "HU-17-", "HU-18-", "HU-19-", "HU-2--", "HU-20-", "HU-3--", "HU-4--",
        "HU-5--", "HU-6--", "HU-7--", "HU-8--", "HU-9--", "HX-1--", "HX-2--",
        "HX-3--", "HX-4--", "HX-5--", "HX-6--", "HX-7--", "IG-1--", "IG-10-",
        "IG-11-", "IG-2--", "IG-3--", "IG-4--", "IG-5--", "IG-6--", "IG-7--",
        "IG-7--", "IG-8--", "IG-8--", "IG-9--", "IG-9--", "IP-1--", "IP-10-",
        "IP-11-", "IP-12-", "IP-13-", "IP-14-", "IP-15-", "IP-16-", "IP-17-",
        "IP-18-", "IP-19-", "IP-2--", "IP-20-", "IP-21-", "IP-22-", "IP-23-",
        "IP-24-", "IP-25-", "IP-26-", "IP-27-", "IP-28-", "IP-29-", "IP-3--",
        "IP-30-", "IP-31-", "IP-32-", "IP-33-", "IP-4--", "IP-5--", "IP-6--",
        "IP-7--", "IP-8--", "IP-9--", "IP-98-", "KT-1--", "KT-10-", "KT-11-",
        "KT-12-", "KT-13-", "KT-14-", "KT-15-", "KT-16-", "KT-17-", "KT-17-",
        "KT-18-", "KT-18-", "KT-19-", "KT-19-", "KT-2--", "KT-20-", "KT-21-",
        "KT-22-", "KT-22-", "KT-23-", "KT-24-", "KT-3--", "KT-4--", "KT-4--",
        "KT-5--", "KT-6--", "KT-6--", "KT-7--", "KT-7--", "KT-8--", "KT-8--",
        "KT-9--", "KT-9--", "L--1--", "L--10-", "L--11-", "L--12-", "L--13-",
        "L--14-", "L--15-", "L--16-", "L--17-", "L--18-", "L--19-", "L--2--",
        "L--20-", "L--21-", "L--22-", "L--23-", "L--24-", "L--25-", "L--26-",
        "L--27-", "L--28-", "L--29-", "L--3--", "L--30-", "L--31-", "L--32-",
        "L--33-", "L--34-", "L--35-", "L--36-", "L--37-", "L--38-", "L--39-",
        "L--4--", "L--40-", "L--5--", "L--6--", "L--67-", "L--68-", "L--69-",
        "L--7--", "L--70-", "L--71-", "L--72-", "L--74-", "L--75-", "L--8--",
        "L--80-", "L--9--", "LA-1--", "LA-10-", "LA-10-", "LA-11-", "LA-12-",
        "LA-13-", "LA-14-", "LA-15-", "LA-16-", "LA-17-", "LA-18-", "LA-19-",
        "LA-2--", "LA-2--", "LA-20-", "LA-21-", "LA-22-", "LA-23-", "LA-3--",
        "LA-4--", "LA-5--", "LA-6--", "LA-6--", "LA-7--", "LA-8--", "LA-9--",
        "LD-1--", "LD-2--", "LD-3--", "LD-4--", "LD-5--", "LD-6--", "LD-7--",
        "LD-7--", "LD-8--", "LD-8--", "LE-1--", "LE-10-", "LE-10-", "LE-11-",
        "LE-12-", "LE-13-", "LE-14-", "LE-15-", "LE-16-", "LE-17-", "LE-17-",
        "LE-18-", "LE-19-", "LE-2--", "LE-21-", "LE-3--", "LE-4--", "LE-41-",
        "LE-5--", "LE-55-", "LE-6--", "LE-65-", "LE-67-", "LE-7--", "LE-8--",
        "LE-87-", "LE-9--", "LE-94-", "LE-95-", "LL-11-", "LL-12-", "LL-13-",
        "LL-14-", "LL-14-", "LL-15-", "LL-16-", "LL-17-", "LL-18-", "LL-19-",
        "LL-20-", "LL-21-", "LL-22-", "LL-23-", "LL-24-", "LL-25-", "LL-26-",
        "LL-27-", "LL-28-", "LL-29-", "LL-30-", "LL-31-", "LL-32-", "LL-33-",
        "LL-34-", "LL-35-", "LL-36-", "LL-37-", "LL-38-", "LL-39-", "LL-40-",
        "LL-41-", "LL-42-", "LL-43-", "LL-44-", "LL-45-", "LL-46-", "LL-47-",
        "LL-48-", "LL-49-", "LL-51-", "LL-52-", "LL-53-", "LL-54-", "LL-55-",
        "LL-56-", "LL-57-", "LL-58-", "LL-59-", "LL-60-", "LL-61-", "LL-62-",
        "LL-63-", "LL-64-", "LL-65-", "LL-66-", "LL-67-", "LL-68-", "LL-69-",
        "LL-70-", "LL-71-", "LL-72-", "LL-73-", "LL-74-", "LL-75-", "LL-76-",
        "LL-77-", "LL-78-", "LN-1--", "LN-10-", "LN-11-", "LN-12-", "LN-13-",
        "LN-2--", "LN-3--", "LN-4--", "LN-5--", "LN-6--", "LN-7--", "LN-7--",
        "LN-8--", "LN-8--", "LN-9--", "LS-1--", "LS-10-", "LS-11-", "LS-12-",
        "LS-13-", "LS-14-", "LS-15-", "LS-16-", "LS-17-", "LS-18-", "LS-19-",
        "LS-2--", "LS-20-", "LS-21-", "LS-22-", "LS-23-", "LS-24-", "LS-25-",
        "LS-26-", "LS-27-", "LS-28-", "LS-29-", "LS-3--", "LS-4--", "LS-5--",
        "LS-6--", "LS-7--", "LS-8--", "LS-88-", "LS-9--", "LS-98-", "LS-99-",
        "LU-1--", "LU-2--", "LU-3--", "LU-4--", "LU-5--", "LU-6--", "LU-6--",
        "LU-7--", "LU-7--", "M--1--", "M--11-", "M--12-", "M--13-", "M--14-",
        "M--15-", "M--16-", "M--17-", "M--18-", "M--19-", "M--2--", "M--20-",
        "M--21-", "M--22-", "M--23-", "M--24-", "M--25-", "M--26-", "M--27-",
        "M--28-", "M--29-", "M--3--", "M--30-", "M--31-", "M--32-", "M--33-",
        "M--34-", "M--35-", "M--38-", "M--4--", "M--40-", "M--41-", "M--43-",
        "M--44-", "M--45-", "M--46-", "M--5--", "M--50-", "M--6--", "M--60-",
        "M--61-", "M--7--", "M--8--", "M--9--", "M--90-", "M--99-", "ME-1--",
        "ME-10-", "ME-11-", "ME-12-", "ME-13-", "ME-14-", "ME-15-", "ME-16-",
        "ME-17-", "ME-18-", "ME-19-", "ME-2--", "ME-20-", "ME-3--", "ME-4--",
        "ME-5--", "ME-6--", "ME-7--", "ME-8--", "ME-9--", "ME-99-", "MK-1--",
        "MK-10-", "MK-11-", "MK-12-", "MK-13-", "MK-14-", "MK-15-", "MK-16-",
        "MK-16-", "MK-17-", "MK-17-", "MK-18-", "MK-19-", "MK-19-", "MK-2--",
        "MK-3--", "MK-4--", "MK-40-", "MK-41-", "MK-42-", "MK-43-", "MK-43-",
        "MK-44-", "MK-45-", "MK-46-", "MK-5--", "MK-6--", "MK-7--", "MK-77-",
        "MK-8--", "MK-9--", "N--1--", "N--10-", "N--11-", "N--12-", "N--13-",
        "N--14-", "N--15-", "N--16-", "N--17-", "N--18-", "N--19-", "N--1C-",
        "N--1P-", "N--2--", "N--20-", "N--21-", "N--22-", "N--3--", "N--4--",
        "N--5--", "N--6--", "N--7--", "N--8--", "N--81-", "N--9--", "NE-1--",
        "NE-10-", "NE-11-", "NE-12-", "NE-13-", "NE-15-", "NE-16-", "NE-17-",
        "NE-18-", "NE-19-", "NE-2--", "NE-20-", "NE-21-", "NE-22-", "NE-23-",
        "NE-24-", "NE-25-", "NE-26-", "NE-27-", "NE-28-", "NE-29-", "NE-3--",
        "NE-30-", "NE-31-", "NE-32-", "NE-33-", "NE-34-", "NE-35-", "NE-36-",
        "NE-37-", "NE-38-", "NE-39-", "NE-4--", "NE-40-", "NE-41-", "NE-42-",
        "NE-43-", "NE-44-", "NE-45-", "NE-46-", "NE-47-", "NE-48-", "NE-49-",
        "NE-5--", "NE-6--", "NE-61-", "NE-62-", "NE-63-", "NE-64-", "NE-65-",
        "NE-66-", "NE-67-", "NE-68-", "NE-69-", "NE-7--", "NE-70-", "NE-71-",
        "NE-8--", "NE-82-", "NE-83-", "NE-85-", "NE-88-", "NE-9--", "NE-92-",
        "NE-98-", "NE-99-", "NG-1--", "NG-10-", "NG-11-", "NG-12-", "NG-13-",
        "NG-14-", "NG-15-", "NG-16-", "NG-17-", "NG-18-", "NG-19-", "NG-2--",
        "NG-20-", "NG-21-", "NG-22-", "NG-23-", "NG-24-", "NG-25-", "NG-3--",
        "NG-31-", "NG-32-", "NG-33-", "NG-34-", "NG-4--", "NG-5--", "NG-6--",
        "NG-7--", "NG-8--", "NG-80-", "NG-9--", "NG-90-", "NN-1--", "NN-10-",
        "NN-10-", "NN-11-", "NN-11-", "NN-12-", "NN-12-", "NN-13-", "NN-13-",
        "NN-14-", "NN-15-", "NN-16-", "NN-17-", "NN-18-", "NN-2--", "NN-29-",
        "NN-29-", "NN-3--", "NN-4--", "NN-5--", "NN-6--", "NN-7--", "NN-7--",
        "NN-8--", "NN-9--", "NP-10-", "NP-11-", "NP-12-", "NP-13-", "NP-15-",
        "NP-16-", "NP-16-", "NP-18-", "NP-19-", "NP-20-", "NP-22-", "NP-23-",
        "NP-24-", "NP-25-", "NP-25-", "NP-25-", "NP-26-", "NP-4--", "NP-44-",
        "NP-7--", "NP-7--", "NP-8--", "NR-1--", "NR-10-", "NR-11-", "NR-12-",
        "NR-13-", "NR-14-", "NR-15-", "NR-16-", "NR-17-", "NR-18-", "NR-19-",
        "NR-2--", "NR-20-", "NR-21-", "NR-22-", "NR-23-", "NR-24-", "NR-25-",
        "NR-26-", "NR-27-", "NR-28-", "NR-29-", "NR-3--", "NR-30-", "NR-31-",
        "NR-32-", "NR-33-", "NR-34-", "NR-35-", "NR-4--", "NR-5--", "NR-6--",
        "NR-7--", "NR-8--", "NR-9--", "NR-99-", "NW-1--", "NW-10-", "NW-11-",
        "NW-1W-", "NW-2--", "NW-26-", "NW-3--", "NW-4--", "NW-5--", "NW-6--",
        "NW-7--", "NW-8--", "NW-9--", "OL-1--", "OL-10-", "OL-11-", "OL-12-",
        "OL-13-", "OL-14-", "OL-14-", "OL-15-", "OL-15-", "OL-16-", "OL-2--",
        "OL-3--", "OL-4--", "OL-5--", "OL-6--", "OL-7--", "OL-8--", "OL-9--",
        "OL-95-", "OX-1--", "OX-10-", "OX-11-", "OX-12-", "OX-13-", "OX-14-",
        "OX-15-", "OX-15-", "OX-16-", "OX-17-", "OX-17-", "OX-17-", "OX-18-",
        "OX-18-", "OX-2--", "OX-20-", "OX-25-", "OX-26-", "OX-27-", "OX-27-",
        "OX-28-", "OX-29-", "OX-3--", "OX-33-", "OX-39-", "OX-4--", "OX-44-",
        "OX-49-", "OX-5--", "OX-7--", "OX-7--", "OX-7--", "OX-9--", "PE-1--",
        "PE-10-", "PE-11-", "PE-12-", "PE-12-", "PE-13-", "PE-13-", "PE-14-",
        "PE-14-", "PE-15-", "PE-16-", "PE-19-", "PE-2--", "PE-20-", "PE-21-",
        "PE-22-", "PE-23-", "PE-24-", "PE-25-", "PE-26-", "PE-27-", "PE-28-",
        "PE-29-", "PE-3--", "PE-30-", "PE-31-", "PE-32-", "PE-33-", "PE-34-",
        "PE-34-", "PE-35-", "PE-36-", "PE-37-", "PE-38-", "PE-4--", "PE-5--",
        "PE-6--", "PE-6--", "PE-7--", "PE-8--", "PE-8--", "PE-9--", "PE-9--",
        "PL-1--", "PL-10-", "PL-11-", "PL-12-", "PL-13-", "PL-14-", "PL-15-",
        "PL-16-", "PL-17-", "PL-18-", "PL-19-", "PL-2--", "PL-20-", "PL-21-",
        "PL-22-", "PL-23-", "PL-24-", "PL-25-", "PL-26-", "PL-27-", "PL-28-",
        "PL-29-", "PL-3--", "PL-30-", "PL-31-", "PL-32-", "PL-33-", "PL-34-",
        "PL-35-", "PL-4--", "PL-5--", "PL-6--", "PL-7--", "PL-8--", "PL-9--",
        "PL-95-", "PO-1--", "PO-10-", "PO-11-", "PO-12-", "PO-13-", "PO-14-",
        "PO-15-", "PO-16-", "PO-17-", "PO-18-", "PO-19-", "PO-2--", "PO-20-",
        "PO-21-", "PO-22-", "PO-3--", "PO-30-", "PO-31-", "PO-32-", "PO-33-",
        "PO-34-", "PO-35-", "PO-36-", "PO-37-", "PO-38-", "PO-39-", "PO-4--",
        "PO-40-", "PO-41-", "PO-5--", "PO-6--", "PO-7--", "PO-8--", "PO-9--",
        "PR-0--", "PR-1--", "PR-11-", "PR-2--", "PR-25-", "PR-26-", "PR-3--",
        "PR-4--", "PR-5--", "PR-6--", "PR-7--", "PR-8--", "PR-9--", "RG-1--",
        "RG-10-", "RG-12-", "RG-14-", "RG-17-", "RG-17-", "RG-18-", "RG-19-",
        "RG-2--", "RG-20-", "RG-21-", "RG-22-", "RG-23-", "RG-24-", "RG-25-",
        "RG-26-", "RG-27-", "RG-28-", "RG-29-", "RG-30-", "RG-31-", "RG-4--",
        "RG-40-", "RG-41-", "RG-42-", "RG-45-", "RG-5--", "RG-6--", "RG-7--",
        "RG-8--", "RG-9--", "RH-1--", "RH-10-", "RH-11-", "RH-12-", "RH-13-",
        "RH-14-", "RH-15-", "RH-16-", "RH-17-", "RH-18-", "RH-19-", "RH-2--",
        "RH-20-", "RH-3--", "RH-4--", "RH-5--", "RH-6--", "RH-7--", "RH-77-",
        "RH-8--", "RH-9--", "RM-1--", "RM-10-", "RM-11-", "RM-12-", "RM-13-",
        "RM-14-", "RM-14-", "RM-15-", "RM-15-", "RM-16-", "RM-17-", "RM-18-",
        "RM-19-", "RM-2--", "RM-20-", "RM-3--", "RM-4--", "RM-4--", "RM-5--",
        "RM-6--", "RM-7--", "RM-8--", "RM-9--", "S--1--", "S--10-", "S--11-",
        "S--11-", "S--12-", "S--12-", "S--13-", "S--14-", "S--17-", "S--17-",
        "S--18-", "S--2--", "S--20-", "S--21-", "S--25-", "S--26-", "S--3--",
        "S--32-", "S--33-", "S--35-", "S--36-", "S--4--", "S--40-", "S--41-",
        "S--42-", "S--43-", "S--44-", "S--45-", "S--49-", "S--5--", "S--6--",
        "S--60-", "S--61-", "S--62-", "S--63-", "S--64-", "S--65-", "S--66-",
        "S--7--", "S--70-", "S--71-", "S--72-", "S--73-", "S--74-", "S--75-",
        "S--8--", "S--8--", "S--80-", "S--80-", "S--81-", "S--81-", "S--9--",
        "S--95-", "S--96-", "S--97-", "S--98-", "S--99-", "SA-1--", "SA-10-",
        "SA-11-", "SA-12-", "SA-13-", "SA-14-", "SA-15-", "SA-16-", "SA-17-",
        "SA-18-", "SA-19-", "SA-2--", "SA-20-", "SA-3--", "SA-31-", "SA-32-",
        "SA-33-", "SA-34-", "SA-35-", "SA-36-", "SA-37-", "SA-38-", "SA-39-",
        "SA-4--", "SA-40-", "SA-41-", "SA-42-", "SA-43-", "SA-44-", "SA-45-",
        "SA-46-", "SA-47-", "SA-48-", "SA-5--", "SA-6--", "SA-61-", "SA-62-",
        "SA-63-", "SA-64-", "SA-65-", "SA-66-", "SA-67-", "SA-68-", "SA-69-",
        "SA-7--", "SA-70-", "SA-71-", "SA-72-", "SA-73-", "SA-8--", "SA-80-",
        "SA-9--", "SA-99-", "SE-1--", "SE-10-", "SE-11-", "SE-12-", "SE-13-",
        "SE-14-", "SE-15-", "SE-16-", "SE-17-", "SE-18-", "SE-19-", "SE-1P-",
        "SE-2--", "SE-20-", "SE-21-", "SE-22-", "SE-23-", "SE-24-", "SE-25-",
        "SE-26-", "SE-27-", "SE-28-", "SE-3--", "SE-4--", "SE-5--", "SE-6--",
        "SE-7--", "SE-8--", "SE-9--", "SG-1--", "SG-10-", "SG-11-", "SG-12-",
        "SG-13-", "SG-14-", "SG-15-", "SG-16-", "SG-17-", "SG-18-", "SG-19-",
        "SG-2--", "SG-3--", "SG-4--", "SG-5--", "SG-6--", "SG-7--", "SG-8--",
        "SG-9--", "SK-1--", "SK-10-", "SK-11-", "SK-11-", "SK-12-", "SK-13-",
        "SK-13-", "SK-14-", "SK-14-", "SK-15-", "SK-16-", "SK-17-", "SK-17-",
        "SK-2--", "SK-22-", "SK-22-", "SK-23-", "SK-23-", "SK-3--", "SK-4--",
        "SK-5--", "SK-6--", "SK-7--", "SK-8--", "SK-9--", "SL-0--", "SL-1--",
        "SL-2--", "SL-3--", "SL-4--", "SL-5--", "SL-6--", "SL-60-", "SL-7--",
        "SL-8--", "SL-9--", "SL-9--", "SL-95-", "SM-1--", "SM-2--", "SM-2--",
        "SM-3--", "SM-3--", "SM-4--", "SM-5--", "SM-6--", "SM-7--", "SM-7--",
        "SN-1--", "SN-10-", "SN-11-", "SN-12-", "SN-13-", "SN-14-", "SN-15-",
        "SN-16-", "SN-2--", "SN-25-", "SN-26-", "SN-3--", "SN-38-", "SN-4--",
        "SN-5--", "SN-6--", "SN-6--", "SN-7--", "SN-8--", "SN-8--", "SN-9--",
        "SN-99-", "SO-14-", "SO-15-", "SO-16-", "SO-17-", "SO-18-", "SO-19-",
        "SO-20-"
      ), ltr = c(
        "H", "H", "H", "H", "H", "H", "H", "H", "H",
        "H", "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", "G",
        "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", "G",
        "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", "G",
        "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", "G",
        "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", "G",
        "G", "F", "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", "G",
        "G", "G", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K",
        "K", "K", "K", "K", "K", "K", "K", "K", "D", "D", "D", "D", "D",
        "E", "D", "D", "D", "D", "D", "D", "D", "D", "D", "E", "E", "E",
        "E", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E",
        "D", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E", "K",
        "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K",
        "K", "K", "J", "J", "K", "J", "K", "K", "J", "K", "K", "K", "K",
        "K", "K", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D",
        "D", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J",
        "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J",
        "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "I", "I",
        "I", "I", "I", "I", "J", "I", "J", "I", "K", "K", "K", "K", "K",
        "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K",
        "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K",
        "K", "K", "K", "K", "K", "K", "K", "K", "D", "D", "D", "D", "D",
        "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D",
        "D", "D", "D", "D", "D", "D", "D", "D", "D", "C", "D", "C", "D",
        "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H",
        "H", "H", "H", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L",
        "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L",
        "L", "L", "L", "L", "L", "L", "L", "L", "J", "J", "J", "J", "J",
        "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "K", "J", "J",
        "J", "K", "J", "J", "K", "K", "K", "K", "J", "K", "J", "J", "K",
        "K", "K", "K", "J", "C", "C", "C", "C", "C", "C", "C", "C", "H",
        "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H",
        "H", "H", "H", "H", "H", "G", "G", "G", "G", "G", "G", "F", "G",
        "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", "G",
        "D", "G", "G", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I",
        "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I",
        "I", "I", "I", "I", "I", "G", "L", "G", "G", "G", "G", "L", "D",
        "D", "L", "G", "L", "G", "L", "L", "L", "L", "G", "L", "L", "G",
        "L", "G", "L", "L", "L", "G", "G", "G", "L", "G", "G", "L", "G",
        "G", "L", "G", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K",
        "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K",
        "K", "C", "C", "C", "D", "G", "G", "G", "G", "G", "G", "G", "G",
        "G", "G", "G", "G", "G", "D", "J", "J", "J", "J", "J", "J", "I",
        "J", "J", "I", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J",
        "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J",
        "J", "J", "J", "J", "J", "J", "J", "J", "J", "K", "K", "K", "K",
        "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K",
        "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K",
        "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "C", "C",
        "C", "C", "C", "E", "C", "E", "C", "C", "C", "C", "C", "C", "C",
        "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C",
        "C", "E", "E", "C", "E", "C", "I", "I", "I", "I", "J", "I", "I",
        "J", "J", "I", "J", "J", "J", "J", "I", "I", "J", "I", "I", "I",
        "I", "J", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I",
        "I", "I", "I", "J", "J", "I", "H", "I", "I", "I", "I", "I", "I",
        "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I",
        "I", "I", "I", "I", "I", "I", "I", "D", "D", "D", "D", "D", "D",
        "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "I",
        "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I",
        "H", "H", "H", "H", "I", "H", "H", "H", "I", "J", "H", "H", "H",
        "I", "H", "H", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E",
        "E", "E", "E", "E", "E", "E", "E", "E", "D", "D", "D", "D", "D",
        "D", "D", "D", "G", "G", "G", "K", "G", "K", "G", "K", "G", "G",
        "G", "G", "G", "G", "G", "G", "G", "K", "G", "G", "G", "G", "G",
        "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", "G",
        "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", "G",
        "G", "G", "G", "G", "E", "E", "E", "E", "E", "E", "E", "E", "E",
        "E", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E",
        "E", "E", "E", "E", "E", "E", "E", "E", "L", "L", "L", "L", "L",
        "L", "L", "L", "D", "L", "D", "D", "D", "D", "D", "D", "D", "D",
        "D", "D", "D", "D", "L", "D", "D", "D", "D", "D", "D", "D", "D",
        "D", "D", "L", "L", "D", "D", "D", "D", "D", "D", "D", "L", "D",
        "L", "D", "D", "H", "H", "H", "H", "H", "I", "H", "I", "H", "H",
        "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H",
        "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H",
        "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "I", "I", "J",
        "I", "I", "I", "I", "J", "J", "I", "I", "I", "J", "I", "I", "J",
        "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J",
        "J", "J", "J", "J", "J", "J", "J", "J", "G", "G", "F", "G", "F",
        "G", "F", "G", "G", "G", "G", "G", "F", "G", "G", "G", "G", "G",
        "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", "F", "D", "D",
        "D", "D", "G", "D", "G", "D", "G", "D", "D", "D", "D", "D", "D",
        "D", "J", "I", "J", "J", "J", "J", "I", "J", "I", "I", "I", "J",
        "I", "J", "J", "J", "I", "J", "I", "I", "I", "J", "F", "F", "F",
        "G", "F", "G", "G", "F", "F", "F", "F", "F", "F", "F", "F", "F",
        "F", "F", "F", "G", "F", "F", "F", "F", "F", "F", "F", "D", "C",
        "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "E",
        "E", "C", "C", "D", "E", "C", "C", "C", "C", "C", "C", "E", "C",
        "C", "C", "E", "E", "E", "E", "C", "E", "F", "E", "E", "F", "E",
        "E", "E", "E", "E", "F", "E", "E", "E", "E", "F", "F", "E", "F",
        "E", "E", "E", "E", "E", "E", "E", "F", "E", "F", "E", "F", "E",
        "F", "E", "E", "E", "F", "E", "E", "E", "E", "E", "E", "F", "K",
        "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "G", "G", "G",
        "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", "I", "I",
        "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I",
        "H", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I",
        "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I",
        "I", "I", "I", "I", "I", "I", "I", "I", "I", "H", "H", "I", "H",
        "I", "I", "H", "I", "H", "H", "I", "H", "I", "H", "I", "H", "I",
        "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K",
        "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K",
        "K", "K", "K", "K", "K", "K", "K", "D", "D", "D", "D", "D", "D",
        "D", "D", "K", "K", "K", "K", "K", "K", "K", "K", "L", "K", "G",
        "K", "G", "K", "G", "K", "K", "G", "K", "K", "K", "K", "K", "K",
        "K", "K", "K", "G", "K", "G", "J", "K", "K", "J", "K", "K", "J",
        "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J",
        "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J",
        "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "I", "I",
        "I", "I", "I", "I", "H", "I", "H", "I", "H", "I", "I", "E", "E",
        "E", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E", "H",
        "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "H", "J", "J",
        "J", "H", "J", "J", "H", "H", "J", "J", "H", "J", "J", "J", "H",
        "J", "G", "G", "L", "G", "G", "G", "L", "G", "G", "G", "K", "G",
        "K", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E",
        "E", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E",
        "E", "E", "I", "H", "I", "I", "I", "I", "I", "I", "I", "H", "I",
        "H", "H", "I", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H",
        "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H",
        "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "I", "J",
        "J", "J", "J", "J", "J", "J", "J", "I", "J", "I", "J", "I", "I",
        "J", "J", "J", "I", "J", "J", "I", "I", "J", "I", "I", "J", "J",
        "I", "J", "I", "I", "J", "D", "D", "D", "D", "D", "D", "D", "D",
        "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D",
        "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D",
        "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D",
        "D", "D", "D", "D", "E", "D", "D", "D", "D", "D", "D", "D", "D",
        "D", "D", "E", "D", "D", "D", "D", "D", "D", "D", "D", "E", "D",
        "D", "D", "L", "L", "L", "L", "L", "L", "L", "G", "L", "G", "F",
        "F", "G", "F", "F", "F", "F", "F", "F", "F", "G", "F", "F", "F",
        "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F",
        "F", "F", "L", "L", "L", "L", "G", "L", "L", "L", "L", "L", "L",
        "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L",
        "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L",
        "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L",
        "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L",
        "L", "L", "L", "L", "L", "F", "F", "F", "F", "F", "F", "F", "F",
        "F", "F", "F", "E", "F", "E", "F", "E", "E", "E", "E", "E", "E",
        "E", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E",
        "E", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E",
        "H", "H", "H", "H", "H", "H", "J", "H", "J", "D", "D", "D", "D",
        "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D",
        "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D",
        "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D",
        "D", "D", "D", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J",
        "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J",
        "J", "J", "J", "J", "J", "J", "F", "J", "H", "J", "F", "J", "J",
        "J", "J", "H", "H", "H", "H", "J", "H", "H", "J", "J", "J", "J",
        "J", "J", "J", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I",
        "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I",
        "I", "I", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C",
        "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C",
        "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C",
        "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C",
        "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C",
        "C", "C", "C", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F",
        "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F",
        "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "H", "F", "G",
        "F", "J", "F", "J", "F", "F", "F", "F", "F", "F", "F", "H", "F",
        "F", "F", "F", "F", "J", "F", "F", "L", "L", "L", "L", "L", "L",
        "K", "L", "L", "L", "L", "L", "L", "L", "G", "K", "L", "L", "L",
        "L", "G", "L", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H",
        "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H",
        "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H",
        "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I",
        "D", "D", "D", "D", "D", "E", "D", "D", "E", "D", "D", "D", "D",
        "D", "D", "D", "D", "D", "D", "J", "J", "J", "J", "J", "J", "J",
        "G", "J", "J", "F", "G", "J", "K", "J", "J", "J", "J", "J", "F",
        "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "G", "K", "J",
        "H", "F", "F", "F", "H", "H", "F", "H", "F", "H", "H", "H", "H",
        "F", "F", "F", "F", "F", "F", "H", "H", "H", "H", "H", "H", "H",
        "H", "H", "H", "F", "H", "H", "H", "H", "H", "H", "F", "H", "H",
        "F", "H", "F", "H", "K", "K", "K", "K", "K", "K", "K", "K", "K",
        "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K",
        "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "K",
        "K", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J",
        "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J",
        "J", "J", "J", "J", "J", "J", "J", "J", "J", "D", "D", "D", "D",
        "D", "D", "D", "D", "D", "D", "D", "D", "D", "J", "J", "J", "J",
        "J", "K", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J",
        "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J",
        "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J", "J",
        "J", "J", "J", "J", "J", "J", "J", "J", "J", "I", "I", "I", "I",
        "I", "I", "H", "H", "I", "H", "H", "H", "H", "I", "H", "I", "H",
        "I", "I", "I", "I", "I", "I", "E", "E", "E", "F", "E", "F", "E",
        "E", "E", "F", "F", "E", "E", "F", "E", "E", "E", "F", "F", "E",
        "E", "E", "F", "F", "F", "F", "F", "F", "F", "E", "E", "E", "E",
        "E", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E",
        "F", "F", "E", "F", "E", "E", "E", "E", "E", "E", "E", "L", "L",
        "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L",
        "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L",
        "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L",
        "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "I",
        "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I",
        "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I",
        "I", "I", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H",
        "H", "H", "H", "H", "H", "H", "H", "H", "D", "D", "D", "G", "D",
        "F", "D", "D", "F", "D", "D", "F", "G", "D", "F", "D", "F", "D",
        "D", "D", "D", "D", "D", "D", "D", "J", "J", "J", "J", "J", "J",
        "J", "J", "J", "J", "J", "H", "J", "I", "I", "J", "I", "J", "I",
        "I", "I", "J", "I", "K", "K", "K", "K", "K", "K", "K", "K", "K",
        "K", "K", "K", "K", "K", "K", "K", "J", "J", "K", "J", "K", "K",
        "J", "J", "J", "J", "J", "J", "J"
      )), class = "data.frame", row.names = c(
        NA,
        -2616L
      )) %>%
      data.table(.) %>%
      .[, .(rc6, NUTS = ltr)]
    geo2
  }
f230703c <-
  function( # NUTS2 names
  ) {
    nname <-
      structure(
        list(
          X1 = c("L", "K", "J", "I", "H", "G", "F", "E", "D", "C"),
          X2 =
            c(
              "Wales", "South West", "South East", "London", "East of England",
              "West Midlands", "East Midlands", "Yorkshire and Humber", "North West",
              "North East"
            ),
          X3 = c(
            "Wales", "Southwest", "Southeast", "London",
            "East", "W Midlands", "E Midlands", "Yorkshire",
            "Northwest", "Northeast"
          ),
          nx = c(4, 7, 9, 10, 8, 5, 6, 3, 2, 1)
        ),
        class = "data.frame",
        row.names = c(
          NA,
          -10L
        )
      ) %>%
      data.table(.) %>%
      setnames(., c("code", "name", "abbrev", "nx")) %>%
      .[order(-nx)]
    nname
  }
f230703d <-
  function( # all E&W 104 areas (exclude only tweedside)
  ) {
    geo0 <-
      c(
        "AL-", "B--", "BA-", "BB-", "BD-", "BH-", "BL-", "BN-", "BR-",
        "BS-", "CA-", "CB-", "CF-", "CH-", "CM-", "CO-", "CR-", "CT-",
        "CV-", "CW-", "DA-", "DE-", "DH-", "DL-", "DN-", "DT-", "DY-",
        "E--", "EC-", "EN-", "EX-", "FY-", "GL-", "GU-", "HA-", "HD-",
        "HG-", "HP-", "HR-", "HU-", "HX-", "IG-", "IP-", "KT-", "L--",
        "LA-", "LD-", "LE-", "LL-", "LN-", "LS-", "LU-", "M--", "ME-",
        "MK-", "N--", "NE-", "NG-", "NN-", "NP-", "NR-", "NW-", "OL-",
        "OX-", "PE-", "PL-", "PO-", "PR-", "RG-", "RH-", "RM-", "S--",
        "SA-", "SE-", "SG-", "SK-", "SL-", "SM-", "SN-", "SO-", "SP-",
        "SR-", "SS-", "ST-", "SW-", "SY-", "TA-", "TF-", "TN-", "TQ-",
        "TR-", "TS-", "TW-", "UB-", "W--", "WA-", "WC-", "WD-", "WF-",
        "WN-", "WR-", "WS-", "WV-", "YO-"
      ) %>%
      data.table(rc9 = ., nx = seq_along(.), lab = .)
    geo0
  }
f230810a <-
  function(
      # PRJ aio$rib: (1) translate +/- cen, (2) scale .5, (3) *rotate theta*
      aio = x141, # aio only used in next 3 args
      rib = aio$rib,
      theta = rib$circlep["rottheta"], #* not* atan(rib$pca$tantheta)
      b23shift = rib$circlep[c("x", "y")]) {
    trans.rot <-
      function(rib, fun, theta, b23shift) {
        rib$beta[, 2:3] %>%
          as.matrix(.) %>%
          sweep(., FUN = fun, STAT = b23shift, MAR = 2) %>% # (1)translate
          `/`(x = ., y = 2) %>% # (2)scale
          `%*%`(x = ., y = rrr2(tantheta = tan(theta))) # (3)rotate
      }
    b1 <- rib$beta[, 1]
    ba <- trans.rot(rib = rib, fun = `-`, theta = theta, b23shift = b23shift) # a=above
    bb <- trans.rot(rib = rib, fun = `+`, theta = theta, b23shift = b23shift) # b=below
    bc <- ba + bb # c=a+b => no translate, no scale, yes rotate = 'rotate only'
    bd <- 2 * ba # translate only
    x2 <- data.table(b1, ba, bb, bc, bd) %>%
      setnames(., c(
        "b1",
        "b2a", # a = (c-O)/2 (no longer used)
        "b3a",
        "b2b", # b = (c+O)/2 (no longer used)
        "b3b",
        "b2c", # c = total
        "b3c",
        "b2d", # d = b~ = c-_ 'centred'
        "b3d"
      )) %>% # O is not stored but can be recovered from c-d
      .[, thetad := signal::unwrap(atan2(b3a, b2a))] %>%
      .[, ii := 1:.N] %>%
      as.matrix(.)
    x2
  }
f230810c <-
  function(
      # Z rotated, zdot0 = {0,zdot} accessor with cumulative z0=cum{zdot0} option
      aio = x141, # aio
      docum = T # option to return z0 not zdot0
      ) {
    x1 <- pcaz(aio$rib$pca)
    x2 <- x1[, 2:3] %*%
      rrr2(tantheta = aio$rib$circlep["rottheta"]) %>%
      data.table(.) %>%
      as.matrix(.) %>%
      zoo(., aio$rib$pca$date) %>%
      cbind(x1[, 1, drop = F], .) %>%
      cbind(., x1[, -(1:3)]) %>%
      `colnames<-`(x = ., value = labxnnn(n = 1:ncol(.), char = "z")) %>%
      pre0(., docum = docum) %>%
      cbind(data.table(date = c(aio$rib$estdt[, min(date0)], aio$rib$pca$date)), .)
    x2
  }
f230810f <-
  function(
      # beta accessor from aio
      aio = x141) {
    x1 <- f230810a(aio = aio) %>%
      data.table(.)
    x2 <- aio$rib$pca %>%
      pcab(.) %>%
      data.table(.) %>%
      setnames(., labxnnn(1:ncol(.), ch = "b")) %>%
      .[, b002 := x1[, b2c]] %>% # use c because this is the entire beta
      .[, b003 := x1[, b3c]]
    x2[] # 10-col data.table
  }
f230903a <-
  function(
      # quantiles of distribution for fanchart
      x1 = x161$var0p3, # varest
      j = 1,
      vbl = colnames(x4$fcst)[j],
      ci = (1:9) / 10, # confint in 0,1
      nahead = 20) {
    x3 <- as.list(ci)
    i <- 1
    for (i in seq_along(ci)) {
      cix <- ci[i]
      qqx <- c(.5 - cix / 2, .5, .5 + cix / 2)
      x4 <- predict(x1, n.ahead = nahead, ci = ci[i])
      x3[[i]] <- data.table(x4$fcst[[j]][, c("lower", "fcst", "upper")]) %>%
        .[, ii := (1:.N) + (nrow(x1$y) - 1)] %>% #-1 because x1$y starts at 0
        melt(., id.vars = "ii") %>%
        data.table(bound = c("lower", "fcst", "upper"), qq = qqx)[., on = c(bound = "variable")]
      if (i > 1) {
        x3[[i]] <- x3[[i]][qq != .5]
      }
      x6 <- # include final datapoint in the fan
        rbind(
          data.table(bound = "upper", qq = qqx[1], ii = nrow(x1$y) - 1, value = x1$y[nrow(x1$y), j] - 1e-10), #-1 because x1$y starts at 0
          data.table(bound = "lower", qq = qqx[3], ii = nrow(x1$y) - 1, value = x1$y[nrow(x1$y), j] + 1e-10)
        )
      x3[[i]] <- rbind(x3[[i]], x6)
    }
    x5 <- rbindlist(x3)
    x5
  }
f230903b <-
  function(
      # prep var for geom_fan
      x1 = x161$var0p3, # varest
      ci = (1:9) / 10, # confint in 0,1
      nahead = 20) {
    x2 <- colnames(x1$y)
    x3 <- as.list(x2)
    j <- 2
    for (j in seq_along(x2)) {
      x3[[j]] <- f230903a(x1 = x1, j = j, ci = ci, nahead = nahead)
    }
    x3[[j + 1]] <- melt(data.table(x1$y)[, ii := 0:(.N - 1)], id.vars = "ii")[, qq := 1 * NA][]
    names(x3) <- c(colnames(x1$y), "y")
    x3
  }
f230903c <-
  function(
      # fanchart
      x0 = x161$var0p3,
      desat = "DDDD",
      cols = data.table(hi = c("red", "blue"), lo = c(paste0("#FF", desat), paste0("#", desat, "FF"))),
      i = 2,
      x1 = f230903b(x1 = x0, nahead = naheadx),
      naheadx = 20) {
    ggplot(x1[[i]], aes(ii, value, quantile = qq)) +
      ggfan::geom_fan() +
      geom_line(data = x1[[3]][variable == colnames(x0$y)[i]], aes(ii, value), color = cols[i, hi]) +
      scale_fill_gradient(low = cols[i, hi], high = cols[i, lo]) +
      theme_bw() +
      theme(
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(size = .4, linetype = "dotted", color = "grey80"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 7, face = "plain"),
        axis.line.y.left = element_line(size = .1),
        axis.line.x.bottom = element_line(size = .1),
        axis.text = element_text(size = 6, face = "plain"),
        legend.position = "none"
      )
  }
f230907a <-
  function(
      # utility relating date,ii,theta
      aio = z141) {
    x1 <- f230810c(aio = aio, docum = F) %>%
      .[, days := c(NA, diff(date))] %>%
      .[, thetadot := signal::unwrap(atan2(z003, z002) + 4 * pi)] %>%
      .[-1, .(ii, date, date, imd = as.numeric(date), days, thetadot)]
    x2 <- lm(thetadot ~ ii, x1)
    x3 <- copy(x1) %>%
      .[, trendtheta := predict(x2, newdata = x1[, .(ii)])]
    x4 <- # date(trendtheta) #mi
      x3[, approxfun(x = trendtheta, y = date)]
    x5 <- # ii(trendtheta)
      x3[, approxfun(x = trendtheta, y = ii)]
    x6 <- # theta for 'Xn=1 goes -ve'
      f230810f(aio = aio) %>% # total beta
      .[1, atan2(b003, b002)] - # n=1
      pi / 2 + # pi/2 -> 'orthogonal to' and  the negative means 'lower angle' -> later
      2 * pi
    x7 <- # theta t.p. i.e every pi/2
      pi * seq(from = 3, to = 0, by = -.5)
    x8 <- # theta crit : combine 'flip date' with t.p. in sequence
      rev(sort(c(x6, x7)))
    x9 <- # t.p. dates
      as.Date(round(x4(x8)))
    x10 <- rbind(
      data.table(date = x9, thetapi = x8 / pi),
      x1[.N, .(date, thetapi = coefficients(x2)["ii"] / 2)] # theta/ii = -0.2451187 = .078pi = pi/12.817, cycle is 25.63 bins
    ) %>%
      .[, .(
        date,
        ii = c(x5(thetapi * pi)[-length(date)], x1[, max(ii)]),
        type = "endbin", # no adjustment made for 'theta refers to middle of timebin'
        thetapi = thetapi
      )]
    list(
      tab = x10,
      middateoftheta = x4,
      iioftheta = x5
    )
  }
f230928a <-
  function(
      # FIT harmonic used in AIO
      pars = f230928c(),
      ii # no default, must be supplied
      ) {
    pars <- setNames(pars, c("w", "phi", "r", "c2", "c3"))
    addm <- T
    x1 <- data.table(
      ii,
      z002 = pars["r"] * (sin(pars["w"] * ii - pars["phi"])) + pars["c2"],
      z003 = pars["r"] * (cos(pars["w"] * ii - pars["phi"])) + pars["c3"]
    )
    x1
  }
f230928c <-
  function(
      # SOL sinusoid parameter solvef23[0-9]{4} used in FIS
      aio = x141,
      iix = 0:length(aio$rib$pca$date),
      pars0 = c(w = .25 * length(iix) / 40, phi = -3, r = 5, c2 = -.2, c3 = 4)) {
    x2 <- # start0 actuals
      f230810c(aio = aio)[, .(z002, z003)] # ACT
    f1 <- # error sse
      function(
          pars = pars0,
          x = x2,
          iix) {
        (x - f230928a(
          pars = pars,
          ii = iix
        )[, .(z002, z003)]) %>% # ii from x FIT
          unlist(.) %>%
          `^`(., i = 2) %>%
          sum(.)
      }
    ccon <- F # convergence
    pars1 <- pars0 # working copy
    trycount <- 0
    while (!ccon & (trycount < 100)) { # attempt different starting values
      trycount <- trycount + 1
      print(paste0("try: ", trycount))
      pars1[1] <- pars0[1] + .01 * rnorm(1) # try another freq
      pars1[2] <- pars1[2] + .1 #* rnorm(1) #try another phase
      x5 <- # solve
        nlm( # numerical fit error minimisation
          f = f1, # error sse
          p = pars1, # starting values
          x = x2, # actuals
          ii = iix
        )
      pars.sol <- # label solution
        setNames(x5$estimate, names(pars1))
      pars.sol["w"] <- # replace w with principal value
        pars.sol["w"] - floor(pars.sol["w"] / pi) * pi
      pars.sol["phi"] <- # replace phi with principal value
        pars.sol["phi"] - round(pars.sol["phi"] / (2 * pi)) * 2 * pi
      ccon <- # test convergence
        (x5[[4]] <= 2) & # convergence code ok
          (pars.sol["w"] > .2) & # freq in range
          (pars.sol["w"] < .3)
    }
    pars <- c(
      pars.sol # numerical solution
    )
    stopifnot(all.equal(pars[c("w", "phi", "r", "c2", "c3")], pars))
    pars[c("w", "phi", "r", "c2", "c3")]
  }
f231105a <-
  function(
      # SSE from bso, tabulated
      x1 = x141$fis$bso) {
    lapply(seq_along(x1),
      function(i, x) {
        x[[i]][["fit"]][, data.table(sse = sum(res^2), N = .N)]
      },
      x = x1
    ) %>%
      rbindlist(.) %>%
      .[, .(sse = sum(sse), N = sum(N))] %>%
      .[]
  }
f231106a <-
  function(
      # PRJ,R2 and rottheta replacing f230506b
      nxx = estdtx[, sort(unique(nx))],
      estdtx = x141$fis$ses$estdt,
      pcax = x133,
      kbar = 3,
      circlep = x142 # if NULL, recalc
      ) {
    x2 <- x3 <- as.list(NULL)
    for (i in seq_along(nxx)) {
      x1 <- # PRJ
        f230506a(nxx = nxx[i], estdtx = estdtx, pcax = pcax, kbar = kbar)
      x2[[i]] <- x1$x
      x3[[i]] <- x1$beta
    }
    x6 <- rbindlist(x3)
    if (is.null(circlep)) {
      print("recalc circle")
      x4 <- x6[, circlefit(b2, b3)] # current set define circle
    } else {
      print("precomputed circle")
      stopifnot(all.equal(sort(names(circlep)), sort(c("r", "rms", "rottheta", "x", "y"))))
      x4 <- copy(circlep)
    }
    x5 <- x6[, .(atan2(y = b3 - x4["y"], x = b2 - x4["x"]))] %>% # put into range (-1/2 to +3/2)pi
      `+`(x = ., y = ifelse(. < (-pi / 2), 2 * pi, 0))
    x6[, theta := x5] %>%
      .[, dt := c(NA, diff(theta))]
    x7 <- # translate betas to (0,0) centre b-bCEN = 2*bA
      x6[, .(b2 = b2 - x4["x"], b3 = b3 - x4["y"])] %>%
      .[, .(b2, b3, theta = atan2(b3, b2))]
    x7[theta < (-pi / 2), theta := theta + 2 * pi] # 3rd quadrant is +ve theta
    if (is.null(circlep)) { # if this is the calibration set then update rottheta
      x8 <- # additive theta adjustment to maximise minimum theta(bA)
        x7 %>%
        .[c(1, .N), .(diff(b2), diff(b3), tgttheta = (pi - diff(theta)) / 2)] %>%
        .[, tgttheta - x7[1, theta]]
      stopifnot( # check centring
        !is.null(circlep) || # precomputed => no check needed/relevant
          all.equal( # test equality of theta 'up from b3=0'   removed 230916
            as.numeric(x7[1, theta + x8]), # up from x so following right hand rule
            -(as.numeric(x7[.N, theta + x8]) - pi) # up from x so reversed sign
          )
      )
      x4["rottheta"] <- x8 # update rottheta in local circlep R2
    }
    x9 <- list(
      circlep = x4,
      estdt = rbindlist(x2),
      pca = pcax,
      beta = x6
    )
    x9
  }
f231113a <-
  function(
      # timeseries summary for rsi$tss called by f231113b
      x0 = rsi) {
    x2 <- # join parts of 311b for stats
      x0$ses$estdt %>%
      .[x0$ses$stat[type == "all", .(nx, rsq, nsam)], on = c(nx = "nx"), allow = T, nomatch = NULL]
    x3 <- #' beta1'
      x2[, .(date = date1, lab = rc3, xdot)] %>%
      dcast(., date ~ lab, value.var = "xdot") %>%
      pcaest(., rotate = T) %>% # rotate for rewarded beta
      pcajscale(beta = T) %>% # capm style
      pcab(.) %>%
      .[, 1, drop = F] %>%
      data.table(., keep.rownames = T) %>%
      setnames(., c("rc3", "beta"))
    x4 <- # tseries properties
      x2[, .(
        mean = mean(xdot),
        sigma = sd(xdot),
        rho = acf(xdot, lag.max = 1, plot = F)$acf[2],
        rsqrsitot = rsq[1],
        dd = min(xdot)
      ), .(nx, rc3)] %>% # join fundamentals
      as.data.table(.) %>%
      .[unique(x2[, .(nx, lab = rc3)]), on = c(nx = "nx")] %>%
      .[x3, on = c(lab = "rc3")] %>%
      .[order(nx), .(nx, rc3, mean, sigma, rho, dd, beta, rsqrsitot)] %>%
      .[, .(nx, rc3, mean, sigma, rho, dd, beta = beta / mean(beta), rsqrsitot)]
    x5 <-
      lapply(seq_along(x0$bso),
        function(i, x) {
          x[[i]][["fit"]][, data.table(sse = sum(res^2), N = .N)]
        },
        x = x0$bso
      ) %>%
      rbindlist(.) %>%
      .[, nx := as.integer(substr(names(x0$bso), nchar(names(x0$bso)) - 2, 20))] %>%
      .[x4, on = c(nx = "nx")] %>%
      .[, .(nx, rc3, sse, N, mean, sigma, rho, dd, beta, rsqrsitot)]
    x5
  }
f231113b <-
  function(
      # RSI //
      geo = geo1[nx %in% c(1, 2, 10)],
      steppra = "now\\ver001\\07pra",
      stepsip = "now\\ver001\\02sip",
      steprip = "now\\ver001\\03rip",
      parx = (3 < geo[, length(unique(nx))]),
      soarkeep = F, # revaluation info (LFM1 only)
      bsokeep = F, # individual record fit (large)
      onepass = F,
      q1 = .1) {
    # stopifnot(geo[,length(unique(nx))>2]) #add 240310, found needed
    force(q1) # validity check
    sfInit(par = parx, cpus = min(ncpus(), geo[, length(sort(unique(nx)))]))
    x1 <- sfLapply(
      geo[, sort(unique(nx))],
      f230311a,
      geo = geo,
      steppra = steppra,
      steprip = steprip,
      stepsip = stepsip,
      onepass = onepass,
      q1 = q1
    )
    sfStop()
    t1 <- # final date
      max(names(coread(step = steppra)) %>% .[grep("....-..-..", .)])
    names(x1) <- # list label for 1206a
      paste0(t1, ".", zeroprepend(seq_along(x1), 3))
    x2 <- # SES
      f221206a(bso = x1, geo = geo)
    if (geo[, length(unique(nx)) > 2]) { # can only do pca/lfm on n>2
      x3 <- # timeseries summary
        f231113a(list(geo = geo, ses = x2, bso = x1))
    } else {
      x3 <- NA
    }
    x4 <-
      x2$estdt[, .(date = date1, lab = rc3, xdot)] %>%
      dcast(., date ~ lab, value.var = "xdot")
    if (!soarkeep) {
      x2$soar <- NA
    }
    if (!bsokeep) {
      x1 <- NA
    }
    x4 <- list(
      geo = geo, # GEO
      ses = x2, # SES soar estdt summary, soar optional:soarkeep
      bso = x1, # BSO big solution = record level, optional:bsokeep
      tss = x3,
      pan = x4 # panel for pcaest
    )
    x4
  }
f231113c <-
  function(
      # PRJ replacing 0506b
      nxx = estdtx[, sort(unique(nx))],
      estdtx = rsi$ses$estdt,
      pcax = x133,
      kbar = 3) {
    x2 <- as.list(NULL)
    i <- 1
    for (i in seq_along(nxx)) {
      x1 <- # PRJ uses lm() for
        f230506a(nxx = nxx[i], estdtx = estdtx, pcax = pcax, kbar = kbar)
      x2[[i]] <- x1$beta[, .(nx, b1, b2, b3, rsqprj = rsq, rbarsqprj = rbarsq, aprj = a, atprj = at)]
    }
    rbindlist(x2)
  }
f231114a <-
  function(
      # BCE f231106a beta centre/cycle extract from prj
      prjx = prj) {
    x1 <- # b2c, b3c
      prjx[, circlefit(xp = b2, yp = b3)] %>%
      as.list(.) %>%
      as.data.table(.) %>%
      .[, tbc := atan2(y = y, x = x)] %>%
      .[, tbc := tbc + ifelse(tbc < (-pi / 2), 2 * pi, 0)] %>% # put into range (-1/2 to +3/2)pi
      .[, abc := sqrt(y^2 + x^2)] %>%
      .[, .(
        b2c = x, # betac k=2
        b3c = y, # betac k=3
        tbc, # betac theta
        abc, # betac radius
        rbw = r, # betaw estimated radius of betaw circle
        rmsw = rms # betaw rms betaw deviation from circle
      )]
    x1
  }
f231114b <-
  function(
      # BWE beta wave extract from PRJ
      prjx = prj,
      bcex = bce) {
    prjx[, .(
      nx,
      b1,
      b2w = b2 - bcex[, b2c],
      b3w = b3 - bcex[, b3c]
    )] %>%
      .[, .(
        nx, b1, b2w, b3w,
        tbw = unwrap(atan2(b3w, b2w)),
        abw = sqrt(b2w^2 + b3w^2)
      )]
  }
f231121a <-
  function(
      # local geo rc6 ppm2 nid nx des split ix
      x1 = c(
        "^L--",
        "^M--",
        "^B--",
        "^BS-|^BA-|^GL-",
        "^AL-|^LU-|^HP",
        "^E--|SE-",
        "^NW-|^N--"
      ),
      pva = x121,
      geotest = geo1,
      npcl = 22) {
    x6 <- pva[nchar(rcx) == 6]
    x0 <- # NP=0 - don't use this, instead concentrate x2
      x6 %>%
      .[order(ppm2)] %>%
      .[1:7, rcx] # geo1[nx==1,rcx]
    all.equal(x0, geotest[nx == 1, rcx])
    xx <- as.list(NULL)
    xx[[1]] <- # 1 x1 plus rc6 from same rc3 up to rank 20 : split 2
      x6 %>%
      .[, .(rc3 = substr(rcx, 1, 3), rc6 = rcx, ppm2, nid)] %>%
      .[rc3 %in% substr(x0, 1, 3)] %>% # select rc3 in np=1
      .[order(rc3, ppm2)] %>%
      .[, .SD[1:4], rc3] %>% # first 3 per rc3
      .[order(ppm2)] %>%
      .[ppm2 < 1250] %>%
      .[, .(rc6, ppm2, nid, nx = 1, des = "np1plus", split = 2)]
    xx[[2]] <- # 2 SR  : split 2
      x6 %>%
      .[grep("^SR-", rcx)] %>%
      .[order(-ppm2)] %>%
      .[1:7, .(rc6 = rcx, ppm2, nid, nx = 2, des = "SR", split = 2)] #' ^SR-[1,2,3,4,5,7,8]--$'
    for (i in seq_along(x1)) {
      xx[[2 + i]] <- x6[grep(x1[i], rcx), .(rc6 = rcx, ppm2, nid, nx = 2 + i, des = substr(x1[i], 1, 3), split = 3)]
    }
    ix <- length(xx)
    xx[[ix + 2]] <- # top 20 not in geo-bins AKA PCL; true PCL is top 10 : split 2
      x6 %>%
      .[!grepl("^NW-|^N--", rcx)] %>%
      .[order(-ppm2)] %>%
      .[1:npcl, .(rc6 = rcx, ppm2, nid, nx = ix + 2, des = "pclplus", split = 2)] # 29k
    xx[[ix + 1]] <- # next layer 20 not in geo-bins AKA NEAR-PCL : split 2
      x6 %>%
      .[!grepl("^NW-|^N--", rcx)] %>%
      .[, .(rc3 = substr(rcx, 1, 3), rc6 = rcx, ppm2, nid)] %>%
      .[rc3 %in% substr(xx[[ix + 2]][, rc6], 1, 3)] %>% # np
      .[!rc6 %in% xx[[ix + 2]][, rc6]] %>% # np exclude prev bin rc6
      .[order(-ppm2)] %>%
      .[1:npcl, .(rc6, ppm2, nid, nx = ix + 1, des = "near-pcl", split = 2)] # 29k #was 20
    x7 <- as.list(NULL)
    for (i in seq_along(xx)) {
      isplit <- xx[[i]][1, split]
      x7[[i]] <-
        xx[[i]] %>%
        .[order(ppm2)] %>%
        .[, ix := ceiling(isplit * cumsum(nid) / sum(nid))] # %>%
    }
    x7a <- rbindlist(x7)
    x7a
  }
f231204a <-
  function(
      # generate table 4, 'geo comparison' combining P, RSI, LFM, CIRC
      ipanel = 3, # note this function *breaks* the universal rule: it has global references xnnn which are not passed
      cardinal = c("TS-", "L--", "S--", "M--", "LS-", "B--", "BS-", "AL-", "N--", "WC-")) {
    rsi <- list(z221, z321, z421)[[ipanel]] #---global
    prj <- list(z223, z323, z423)[[ipanel]] #---global
    bwe <- list(z224, z324, z424)[[ipanel]] #---global
    pva <- z110 #-----------------------------global
    stat <- rsi$ses$stat
    geo <- rsi$geo
    x0 <- # expand rc3 parts of geo into rc6
      pva[nchar(rcx) == 6] %>%
      .[, rc3 := substr(rcx, 1, 3)] %>%
      .[rc3 %in% geo[, rc9], .(rc6 = rcx, rc3)] %>%
      geo[., on = c(rc9 = "rc3")] %>%
      .[, .(rc9 = rc6, nx, lab)] %>%
      rbind(., geo[nchar(rc9) == 6])
    cname <- rbind(
      data.table(panel = 1, nx = 1:10, lab = gsub("-", "", cardinal)),
      data.table(panel = 2, nx = 1:10, lab = paste0("np", 1:10)),
      data.table(panel = 3, nx = 1:10, lab = f230703c()[order(nx), abbrev])
    ) %>%
      .[panel == ipanel, .(ipanel, nx, lab)]
    x1 <-
      stat[type == "all", .(nx, rsq)] %>%
      .[prj[, .(nx, rbarsqprj, aprj, ase = aprj / atprj)], on = c(nx = "nx")] %>%
      .[bwe[, .(nx, b1, tbw = atan2(b3w, b2w), dtbw = c(NA, diff(atan2(b3w, b2w))))], on = c(nx = "nx")] %>%
      .[pva[x0, on = c(rcx = "rc9")][, .(nid = sum(nid), ppm2min = min(ppm2), ppm2max = max(ppm2), ppm2 = sum(pv) / sum(m2)), nx], on = c(nx = "nx")] %>%
      .[, .(nx, ppm2min, ppm2max, ppm2, fid = nid / sum(nid), r2rsi = rsq, rbar2prj = rbarsqprj, b1, tbw, dtbw, aprj, ase)] %>%
      cname[., on = c(nx = "nx")] %>%
      .[order(-nx)]
    x1
  }
f231207a <-
  function(
      # zdot daily rate calced by zdot/days
      pcax = z222,
      t0 = as.Date("1994-12-31"),
      kat = list(m = 1, c = 2:3),
      x0 = pcaz(pcax)[, unlist(kat)] # z1,2,3
      ) {
    x1 <-
      x0 %>%
      sweep(., STAT = as.numeric(diff(c(t0, index(x0)))), MAR = 1, FUN = `/`) %>%
      data.table(.) %>%
      setnames(., paste0("z", seq_along(.))) %>%
      cbind(data.table(date = index(x0), days = as.numeric(diff(c(t0, index(x0))))), .) %>%
      .[data.table(daily = seq.Date(from = t0, to = max(index(x0)), by = "d")), on = c(date = "daily"), roll = -Inf] %>%
      .[, .(date, days, z1 = c(0, z1[-1]), z2 = c(0, z2[-1]), z3 = c(0, z3[-1]))] %>% # start at 0 at t0
      .[, .(date, days, z1 = cumsum(z1), z2 = cumsum(z2), z3 = cumsum(z3))]
    x1
  }
f231207b <-
  function(
      # solve geolist; select common records for att; tabulate sse
      geolist =
        list(
          geo0[, .(rc9, nx, lab = labxnnn(nx))], #   GEO(RC3) 1:104
          geo2[, .(rc9 = rc6, nx, lab)], #           GEO(REG) 1:10
          geo1[, .(rc9 = rcx, nx, lab = labxnnn(nx))], # GEO(nP) 1:10
          z502 #                                GEO(LOC) 1:29
        ),
      stepprax = stepprav1,
      nref = 2, # geo which defines scaling of sseret
      ncommon = 4 # geo which defines common subset of records
      ) {
    x5 <- x4 <- x2 <- as.list(NULL)
    for (i in seq_along(geolist)) {
      print(i)
      x2[[i]] <- # solve
        f231113b(
          geo = geolist[[i]],
          steppra = stepprax,
          bsokeep = T
        )
    }
    for (i in seq_along(geolist)) { # collate geo-bins within scheme
      x3 <- as.list(seq_along(x2[[i]][["bso"]])) # detect n within scheme
      for (nbin in seq_along(x3)) {
        x3[[nbin]] <-
          x2[[i]][["bso"]][[nbin]][["fit"]] %>%
          .[, .( # fields for att
            idhash.selldate,
            buydate = as.Date(buydate),
            rc6,
            fit,
            res,
            nscheme = i
          )]
      }
      x4[[i]] <- rbindlist(x3)
    }
    for (i in seq_along(geolist)) { # x4[[nref]] is reference solution for norm.
      x5[[i]] <- # scheme
        x4[[i]] %>% # raw records
        .[
          i = x4[[ncommon]][, .(idhash.selldate)], # select using id(ncommon)
          on = c(idhash.selldate = "idhash.selldate")
        ]
    }
    x6 <- # records
      rbindlist(x5) %>%
      .[, selldate := as.Date(substr(idhash.selldate, 18, 30))] %>%
      .[, buydate := as.Date(buydate)]
    x7 <- # summary table
      x6[, .(sse = sum(res^2), nsam = .N), nscheme] %>%
      .[, df := unlist(lapply(geolist, function(x) {
        x[, length(unique(nx))]
      })) * nrow(x2[[1]][["pan"]])] %>%
      .[, relsse := sse / sse[nref]]
    x8 <-
      list(
        bsocommon = x6, # fields for att
        sse = x7 # nscheme, sse, nsam, df, relsse
      )
    x8
  }
f231208a <-
  function(
      # ATT on 5 schemas prepared in solver
      nscx = 1, # one schema/geo
      geox = # geo for schema nsc=1:4
        rbind(
          geo0[, .(nsc = 1, nx, rc9, lab)], # rc3
          geo1[, .(nsc = 2, rc9 = rcx, nx, lab = labxnnn(nx))], # nP  price
          geo2[, .(nsc = 3, rc9 = rc6, nx, lab = labxnnn(nx))], # REG region
          z502[, .(nsc = 4, nx, rc9, lab = labxnnn(nx))] # LOC locality/area+
        ),
      prjx = # b(k=123|drc) for schema nsc=1:4
        rbind(
          z123[, .(nx, nsc = 1, b1, b2, b3)], # rc3
          z323[, .(nx, nsc = 2, b1, b2, b3)], # np
          z423[, .(nx, nsc = 3, b1, b2, b3)], # reg
          z523[, .(nx, nsc = 4, b1, b2, b3)] # loc
        ),
      zx = f231207a(),
      bsox = z602$bsocommon, # common records for schema 1:4
      t0 = as.Date("1994-12-31"),
      kat = list(m = 1, c = 2:3),
      pcax = z222, # common
      lhs = c(
        "tot", # RSI fit of return
        "fit" # total return
      )) {
    lhs <- match.arg(lhs)
    print(lhs)
    if (nscx == 1) {
      x1 <- bsox[, rcx := substring(rc6, 1, 3)] # this just for schema 1
    } else {
      x1 <- bsox[, rcx := rc6]
    }
    x2 <- geox[nsc == nscx][x1[nscheme == nscx], on = c(rc9 = "rcx")] # gives nx as required
    x3 <- prjx[nsc == nscx, .(nx, b1, b2, b3)][x2, on = c(nx = "nx")] %>% # beta as required
      .[, .(rc6, idhash.selldate, buydate, selldate, b1, b2, b3, fit, res)]
    x4 <- zx[x3, on = c(date = "buydate")][, .(rc6, idhash.selldate, buydate = date, selldate, z1buy = z1, z2buy = z2, z3buy = z3, b1, b2, b3, fit, res)]
    x5 <- zx[x4, on = c(date = "selldate")][, .(rc6, idhash.selldate, buydate, selldate = date, dz1 = (z1 - z1buy), dz2 = z2 - z2buy, dz3 = z3 - z3buy, b1, b2, b3, fit, res)]
    x6 <- x5[, .(rc6, idhash.selldate, buydate, selldate, xm = (dz1 * b1), xc = dz2 * b2 + dz3 * b3, fit, res, tot = fit + res)]
    summary(lm(as.formula(paste0(lhs, "~xm+xc")), x6))
  }
f231214b <-
  function(
      # ACT z23
      pcax = z222,
      cpars = z222$zcycle[c("c2", "c3")]) {
    x1 <-
      pcaz(pcax) %>%
      coredata(.) %>%
      .[, 2:3] %>%
      rbind(rep(0, 2), .) %>%
      apply(., 2, cumsum) %>%
      sweep(., STAT = cpars, MAR = 2, FUN = `-`) %>%
      as.data.table(.) %>%
      setnames(., c("z2", "z3")) %>%
      .[, ii := (1:.N) - 1] %>%
      melt(., id.vars = "ii") %>%
      .[, type := "actual"]
    x1[]
  }
f231221a <-
  function(
      # tabulate factor cycle pars now and pit
      mnem2 = c(setdiff(c("now", "pit"), mnem), mnem),
      x0 = c("z110", "z111", "z221", "z222", "z500", "z500", "z501", "z502", "z506", "z507", "z521", "z523", "z524", "z610", "z620")) {
    for (i in seq_along(mnem2)) {
      for (j in seq_along(x0)) {
        rmifgl(
          paste0(x0[j], mnem2[i])
        )
      }
    }
    x1 <- as.list(c(setdiff(mnem2, mnem), mnem))
    for (i in seq_along(mnem2)) {
      rdatafile <- dir(paste0(mnem2[i], "/")) %>% .[grep("Rdata$", .)]
      if (length(rdatafile) > 0) {
        rdpath <- rdatafile %>%
          max() %>%
          file.path(mnem2[i], .)
        print(mnem2[i])
        print(paste0("load latest rdata ", rdpath))
        load(rdpath)
      } else {
        print(paste0("no rdata loaded for mnem=", mnem2[i]))
      }
      for (j in seq_along(x0)) {
        x <- get(x0[j])
        assign(x = paste0(mnem2[i], x0[j]), value = x, envir = globalenv())
      }
    }
  }
f231224a <-
  function(
      # s.e.(z) from s.e.(x)
      rsi = y201,
      pca = y202,
      addsummary = F #
      ) {
    x0 <- rsi$ses$estdt # x205$fis$ses$estdt
    x1 <- pca # x205$rib$pca
    x2 <- x1$date
    x3 <- pcah(x1)[, 1:3]
    x4 <- as.list(NULL)
    i <- 1
    for (i in seq_along(x2)) {
      x5 <- x0[date1 == x2[i]] # cross-section of x
      x6 <- diag(x5[, xdotse^2])
      x7 <- sqrt(diag(t(x3) %*% x6 %*% x3)) # s.e.
      x8 <- pcaz(x1)[x2[i], 1:3] # cross-section of z
      x4[[i]] <- as.data.table(
        as.list(c(
          as.numeric(coredata(x8)), # z
          x7
        )) # zse
      ) %>%
        setnames(., c(
          paste0("z", 1:3), # z block of 3
          paste0("zse", 1:3) # se(z) block of 3
        )) %>%
        .[, r1 := z1 / zse1] %>%
        .[, r2 := z2 / zse2] %>%
        .[, r3 := z3 / zse3] %>% # tstat=z/zse block of 3
        .[]
    }
    x9 <- # dates
      as.character(x2)
    if (addsummary) { # additional row, median value
      x4[[i + 1]] <- as.data.table(rbindlist(x4) %>% lapply(., abs) %>% lapply(., median))
      x9 <- c(x9, "summary")
    }
    x10 <- cbind(
      data.table(date = x9), # 1 col
      rbindlist(x4) # 9 col
    )
    x10
  }
f240115a <-
  function(
      ii = seq(from = -4, to = length(z222$date) + 4, by = 1),
      pars = z222$zcycle) {
    pars <- setNames(as.numeric(pars), c("w", "phi", "r", "c2", "c3"))
    addm <- T
    x1 <- data.table(
      ii,
      z2 =
        pars["r"] *
          cos(
            -pars["w"] * ii - pars["phi"] # a . cos(-w.t -phi)
          ),
      zdot2 = # added on 241027, also zdot3
        -1 * -1 * pars["w"] *
          pars["r"] *
          sin(
            -pars["w"] * ii - pars["phi"] # a . cos(-w.t -phi)
          ),
      z3 =
        pars["r"] *
          sin(
            -pars["w"] * ii - pars["phi"] # a . sin(-w.t -phi)
          ),
      zdot3 =
        -1 * pars["w"] *
          pars["r"] *
          cos(
            -pars["w"] * ii - pars["phi"] # a . sin(-w.t -phi)
          )
    )
    x1
  }
f240115b <-
  function(
      # SOL sinusoid parameter solve
      pcax = z222,
      iix = 0:length(pcax$date),
      pars0 = c(w = .25 * length(iix) / 40, phi = 3 - pi / 2, r = 5, c2 = -.2, c3 = 4),
      wmin = .2,
      wmax = .3,
      perturb = .1 # try different starting values (or set =0)
      ) {
    x2 <-
      f240115f(pca = pcax) %>%
      .[, .(z002, z003)]
    f1 <- # error sse
      function(
          pars = pars0,
          x = x2,
          iix) {
        x1 <- (x - f240115a(
          pars = pars,
          ii = iix
        )[, .(z002, z003)]) %>% # ii from x FIT
          unlist(.) %>%
          `^`(., i = 2) %>%
          sum(.)
        x1
      }
    ccon <- F # convergence status
    pars1 <- pars0 # working copy
    trycount <- 0
    while (!ccon & (trycount < 100)) { # attempt different starting values
      trycount <- trycount + 1
      print(paste0("try: ", trycount))
      pars1[1] <- pars0[1] + perturb * rnorm(1) # try another freq
      pars1[2] <- pars1[2] + perturb #* rnorm(1) #try another phase - but not right, it's a const
      x5 <- # solve
        nlm( # numerical fit error minimisation
          f = f1, # error sse
          p = pars1, # starting values
          x = x2, # actuals
          ii = iix
        )
      #
      pars.sol <- # label solution
        setNames(x5$estimate, names(pars1))
      pars.sol["w"] <- # replace w with principal value
        pars.sol["w"] - floor(pars.sol["w"] / (pi)) * pi
      pars.sol["phi"] <- # replace phi with principal value
        pars.sol["phi"] - round(pars.sol["phi"] / (2 * pi)) * 2 * pi
      ccon <- # test convergence
        (x5[[4]] <= 2) & # convergence code ok
          (pars.sol["w"] > wmin) & # freq in range
          (pars.sol["w"] < wmax) &
          pars.sol["r"] > 0
      # browser()
    }
    pars <- c(
      pars.sol # numerical solution
    )
    stopifnot(all.equal(pars[c("w", "phi", "r", "c2", "c3")], pars))
    pars[c("w", "phi", "r", "c2", "c3")]
  }
f240115c <-
  function(
      # FIT harmonic without initial values c2,c3 (=analytical fit)
      # pars=c(w=0.2207136,phi=2.8165764-pi/2,r=5.7421196 ,c2=-3.3197401  ,c3=5.1958021  ),
      pars,
      ii = 0:50) {
    pars <- # set additive constants zero
      setNames(pars, c("w", "phi", "r", "c2", "c3")) *
        c(1, 1, 1, 0, 0)
    x1 <- # evaluate wavefunction
      f240115a(
        pars = pars,
        ii = ii
      )
    x1
  }
f240115d <-
  function(
      # SIN solves/adds sinusoid parameters to pcaest
      pcax = z222,
      d0 = as.Date("1994-12-31"),
      pars0 = c(w = .25 * length(pcax$date) / 40, phi = -3, r = 5, c2 = -.2, c3 = 4),
      wmin = .2,
      wmax = .3,
      perturb = .1 # try different starting values (or set =0)
      ) {
    set.seed(1)
    x1 <- f240115b( # SOL sinusoid parameter solve
      pcax = pcax,
      iix = 0:length(pcax$date),
      pars0 = pars0,
      wmin = wmin,
      wmax = wmax,
      perturb = perturb # try different starting values (or set =0)
    )
    x2 <- copy(pcax)
    x2$zcycle <- x1[c("w", "phi", "r", "c2", "c3")]
    x2
  }
f240115e <-
  function(
      # AIO : regress r on const,days,mkt,c; also regress fit=(m+c+r) on d,m,c
      geo = x131[, .(rc9, nx, lab = labxnnn(nx))],
      steppra = "ver002\\07pra",
      stepsip = "ver001\\02sip",
      steprip = "ver001\\03rip",
      pcax = x133,
      parx = (3 < geo[, length(unique(nx))]), # added 231028 // execution
      exbso = T, # exclude bso='big solution'
      circlep = NULL, # NULL for recalc, else x142
      outthreshx = .1) { # this 'all in one' function uses duplicate code :( - used only for 'table 5'
    print("fis")
    x1 <- # FIS
      f230311b( # //parallel
        geo = geo,
        steppra = steppra, # ver002=drc
        steprip = steprip,
        stepsip = stepsip,
        parx = parx,
        q1 = outthreshx
      ) # //
    x1$fis$soar <- NULL # because should only use soar from pva
    print("prj")
    x2 <- # PRJ
      f231106a(
        estdtx = x1$ses$estdt, # regressand x
        pcax = pcax, # regressor z
        circlep = circlep
      )
    x2$circlep["rottheta"] <- 0 # deprecated additional rotation
    print("att")
    x3 <- # ATT
      f230603b(
        fis = x1,
        rib = x2
      )
    print("combo")
    x4 <- # COMBO
      f230605b(
        x3 = x3
      )
    print("sse")
    x5 <- f231105a(x1$bso)
    if (exbso) {
      x1 <- x1[names(x1) != "bso"] # bso is big
    }
    x6 <- list(
      fis = x1, # bso is big 231028: allowed bso -> big object, use with caution
      rib = x2,
      att = x3,
      combo = x4,
      sse = x5
    )
    print("cpars")
    x7 <- f240115d(pca = pcax) # newstyle 0115 protocol
    x6$cpars <- x7 # add cpars w phi r c2 c3
    x6[c("fis", "rib", "att", "combo", "sse", "cpars")]
  }
f240115f <-
  function(
      # Z accessor with cumulative option called in SOL
      pcax = z222, # aio
      docum = T, # option to return z0 not zdot0
      d0 = as.Date("1994-12-31")) {
    x1 <- pcaz(pcax) %>%
      `colnames<-`(x = ., value = labxnnn(n = 1:ncol(.), char = "z")) %>%
      pre0(., docum = docum) %>%
      .[, -"ii"] %>%
      data.table(date = c(d0, pcax$date), .)
    x1
  }
f240710a <-
  function(
      # solve single nx -> estdt with no pra
      nxx = 1,
      stepripx = steprip,
      stepprax = stepprav2,
      dfn = dfnx,
      geo = geo0,
      outthresh = .1,
      kfold = 5,
      randomise = F, # for testing
      sectorwise = F, # flag to split folds on complete sectors<<<<<<<<<<<<< T
      usepra = T, # optional<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< F
      newused = c(".", "N", "U"),
      houseflat = c(".", "H", "F") # typex field added to rip 240826, values UH/NH/UF/NF for new/house
      ) {
    stopifnot(nxx %in% geo[, nx]) # add 241116
    newused <- match.arg(newused)
    houseflat <- match.arg(houseflat)
    if (usepra) {
      x2 <- coread(
        rcx = geo[nx == nxx][, rc9],
        step = stepprax,
        colClasses = list(numeric = "retsa")
      )
    } else {
      x1 <- # rip read
        coread(
          rcx = geo[nx == nxx][, rc9],
          step = stepripx,
          colClasses = list(numeric = c("retsa"), Date = c("buydate", "selldate")) # c('retraw','retsa')
        )
      if (paste0(newused, houseflat) != "..") {
        x1 <- x1[grep(paste0("^", newused, houseflat, "$"), type)]
      }
      x2 <- # accrue
        f221209a(
          geo = geo[nx == nxx],
          fur = x1,
          dfn = dfn,
          applygeo = F
        )
    }
    x4 <- lm(
      retsa ~ . - 1,
      x2[, !c("idhash.selldate", "rc9")] # all, no outlier reject
    )
    x5 <- residuals(x4)
    x6a <- x2[, .(idhash.selldate, rc6 = substr(rc9, 1, 6), res = x5)] # per rc6
    x6b <- x6a[, .(lo = quantile(res, outthresh / 2), hi = quantile(res, (1 - outthresh / 2))), rc6] # apply thresholds *by rc6*
    x6c <- x6b[x6a, on = c(rc6 = "rc6")][(lo <= res & res <= hi), .(rc6, idhash.selldate)] # select inlier
    x6d <- x2[x6c[, .(idhash.selldate)], on = c(idhash.selldate = "idhash.selldate")] %>%
      .[order(rc9, idhash.selldate)]
    if (randomise) { # to test it's doing something
      x6d <- x6d[sample(.N)]
    } else if (
      sectorwise &
        x6d[, length(unique(rc9)) * (kfold - 1) / kfold] >= 1 # rc9 [these really are sectors!] in each training set>=1
    ) {
      x6d[, ktile := (as.integer(as.factor(rc9)) %% kfold) %% kfold + 1]
    } else {
      x6d[, ktile := ((1:.N)) %% kfold + 1]
    }
    x10 <- as.list(NULL)
    for (i in 1:kfold) {
      x7 <- x6d[ktile == i, ] # target ktile inlier
      x8 <- x6d[ktile != i, ] # train on other ktiles
      x9 <- lm(
        retsa ~ . - 1,
        x8[, !c("idhash.selldate", "rc9", "ktile")] # train
      )
      x10[[i]] <- x7[, .(sse = sum((
        .SD[ktile == i, retsa] -
          predict(x9, newdata = .SD[ktile == i])
      )^2), n = .N, i), rc9]
    }
    x11 <- # here sum by rc6
      rbindlist(x10) %>%
      .[, .(sse = sum(sse), n = sum(n)), .(rc6 = substr(rc9, 1, 6))]
    x12 <- lm(
      retsa ~ . - 1,
      x6d[, !c("idhash.selldate", "rc9", "ktile")] # all inlier
    )

    x11a <- # Inlier
      data.table(x6d[, .(rc6 = substr(rc9, 1, 6))], res = residuals(x12)) %>%
      .[, .(ssei = sum(res^2), n = .N), rc6]
    x11b <- # kfold
      x11[, .(rc6, ssek = sse, n)]
    x11c <- # Raw
      data.table(x2[, .(rc6 = substr(rc9, 1, 6))], res = residuals(x4)) %>%
      .[, .(sser = sum(res^2), n = .N), rc6]
    x11d <- # tss called ssrt 'sum square total return'
      x2[, .(rc6 = substr(rc9, 1, 6), retsa)] %>%
      .[, .(sstr = sum(retsa^2), n = .N), rc6]
    x12a <- # combine 4 ss
      x11a[x11b, on = c(rc6 = "rc6")][x11c, on = c(rc6 = "rc6")][x11d, on = c(rc6 = "rc6")] %>%
      .[, .(rc6, ssei, ssek, sser, sstr, n, nx = nxx)]
    x13 <-
      x12 %>%
      .[["coefficients"]] %>%
      data.table(
        xdotd = as.numeric(.),
        date = as.Date(substr(names(.), 2, 11))
      ) %>%
      .[, days := as.numeric(diff(c(min(dfn), date)))] %>%
      .[, xdot := as.numeric(xdotd * days)] %>%
      .[, x := cumsum(xdot)] %>%
      .[, .(
        nx = nxx,
        date,
        xdotd,
        days,
        xdot,
        # xdotse,for this would need to do summary(lm)
        x,
        lab = geo[nx == nxx][1, lab]
      )] %>%
      .[, ii := 1:.N, lab] %>%
      .[, col := as.factor(lab)] %>%
      .[]
    x13a <-
      data.table( # these are totals
        allsse.insam = sum(residuals(x12)^2), # rss all inlier, single lm
        allsse.osam = x11[, sum(sse)], # rss kxv out-sample
        allsse.raw = sum(residuals(x4)^2), # rss all in/outlier, single lm
        allsse.tot = x2[, sum(retsa^2)], # tss
        rsqraw = summary(x4)$r.squared # no outlier rejection, all obs
      )
    x14 <- list(
      estdt = x13, # all obsvns
      kfoldsse = x12a, # x11, #kfold sse(rc6)
      all = x13a # rsq is literally all; alltilesse is single solution i/s sse on all inliers/tiles; sumtilesse is sum of o/s sse on all inliers/tiles
    )
    x14
  }
f240710b <-
  function(
      # // rsi kfold with/without pra
      nxx = geo[, sort(unique(nx))],
      stepripx = steprip,
      stepprax = stepprav2,
      dfn = dfnx,
      geo = geo0,
      outthresh = .1,
      kfold = 5,
      randomise = F,
      sectorwise = F, # flag to split folds on complete sectors
      usepra = T,
      ncpu = ncpus(),
      returnestdt = F, # for backward compatibility
      parallel = 1 < length(nxx)) {
    sfInit(
      cpus = min(ncpus(), ncpu),
      parallel = parallel
    )
    x1 <-
      sfLapply(
        fun = f240710a,
        x = nxx,
        stepripx = stepripx,
        stepprax = stepprax,
        dfn = dfn,
        geo = geo,
        outthresh = outthresh,
        kfold = kfold,
        randomise = randomise,
        sectorwise = sectorwise,
        usepra = usepra
      )
    x2a <- x2 <-
      list(NULL)
    for (i in seq_along(nxx)) {
      x2[[i]] <-
        x1[[i]]$kfoldsse # [,nx:=i]
      x2a[[i]] <-
        x1[[i]]$estdt
    }
    x3 <-
      unique(geo[, .(nx, lab)]) %>%
      .[rbindlist(x2), on = c(nx = "nx")]
    x3a <-
      unique(geo[, .(nx, lab)]) %>%
      .[rbindlist(x2a), on = c(nx = "nx")]
    x4 <- x3 # for backward compatibility
    if (returnestdt == T) {
      x4 <- list(
        rss = x3,
        estdt = x3a
      )
    }
    x4
  }
f240715a <-
  function(
      # rc3x='AL-', #it is national
      maxrad = 50, # km radius
      maxpeer = 50, # max group - 50 gives mean(narank)=0
      x0 = z110, # pva
      x1 = getlast("f201203fd") # rc6 coordinates
      ) {
    x2 <-
      x0 %>%
      .[nchar(rcx) == 6, .(pv, m2, ppm2, nid, rc6 = rcx)]
    x3 <-
      x1 %>%
      .[nchar(rc) == 6, .(rc6 = rc, eex, nnx)] %>%
      .[x2[, .(rc6)], on = c(rc6 = "rc6")]
    x4 <- # neighbours
      x3[, .(rc6, eex, nnx, one = 1)] %>%
      .[., mult = "all", on = c(one = "one"), allow = T] %>%
      .[, .(rc6, other = i.rc6, r = round(sqrt((eex - i.eex)^2 + (nnx - i.nnx)^2) / 1000, 2))] %>%
      .[substr(rc6, 1, 3) != substr(other, 1, 3)] %>%
      .[r < maxrad] %>%
      .[order(rc6, r)] %>%
      .[, data.table(.SD, dr = 1:.N), rc6] %>%
      .[dr <= maxpeer] %>%
      .[, rc3 := substr(rc6, 1, 3)] %>%
      .[, .(rc3, rc6, other, r, dr)]
    x4
  }
f240715b <-
  function(
      rc3x = # target rc3
        "AL-",
      rc6aug = # rc6 to test for augmentation
        c("N--17-", "HP-7--", "LU-7--", "SG-10-", "WD-18-", "SW-7--", "SR-1--", "SE-5--"),
      x0 = # pva
        z110,
      nmin = # minimum target rc6 per 'tertile' group
        2) {
    x6 <-
      x0 %>% # pva
      .[nchar(rcx) == 6] %>%
      .[grep(rc3x, rcx), ]
    # stopifnot(x6[,.N]>=3*nmin) #test for sufficient rc6
    x7 <- # aug set
      x0[rcx %in% rc6aug][order(ppm2)]
    x8 <-
      rbind(x6, x7) %>%
      .[order(ppm2)]
    ix <- # cumulative # rcx in 'x8-index space'
      x8[, cumsum(substr(rcx, 1, 3) == rc3x)] #
    ix1 <- # perm-assign these 1 in 'x8-index space' ####
      1:min(which(ix == nmin))
    ix1max <- max(which(ix == (x6[, .N] - 2 * nmin))) # ix1add in the range max(ix1):(max(ix1):ix1max)
    # stopifnot(x8[ix1,sum(substr(rcx,1,3)==rc3x)==nmin]) #at least 2 in rc3x always assigned 1
    ix3 <- # assign these 3 in 'x1-index space' ####
      min(which(ix == (x6[, .N] - nmin + 1))):x8[, .N] # assign these 3
    # stopifnot(x8[ix3,sum(substr(rcx,1,3)==rc3x)==nmin]) #check at least 2 in rc3x always assigned 3
    x9 <- as.list(NULL)
    i3 <- 0
    x10 <- setNames(rep(NA, x8[, .N]), x8[, rcx])
    x10[ix1] <- 1 # always
    x10[ix3] <- 3 # always
    for (i1 in max(ix1):ix1max) { # range that can additionally be assigned 1
      x10[max(ix1):(i1)] <- 1 # assign 1
      x11 <- min(which(ix == (ix[i1] + nmin))) # min index that assigns nmin to 2
      for (i2 in x11:(min(ix3) - 1)) { # assign 2 any from min index up to the 3-reserved range
        x10[(i1 + 1):i2] <- 2 # assign 2
        x10[(i2 + 1):min(ix3)] <- 3 # remainder assign 3
        i3 <- i3 + 1
        x9[[i3]] <- x10 # store it
      }
    }
    x12 <- # convert list to data.table
      as.data.table(x9)[, rc6 := x8[, rcx]]
    for (i in seq_along(x12[, -"rc6"])) {
      for (j in 1:3) { # check: number of rcx in each tert
        # stopifnot(x8[,.(rcx)][which(x12[[i]]==j),sum(grepl(rc3x,rcx))]>=nmin)
      }
    }
    x12[]
  }
f240716a <-
  function(
      rc3x = "AL-", # target rc3
      maxp = 0, # don't augment this way
      nmin = 3) {
    if (z110[nchar(rcx) == 6][, .N, substr(rcx, 1, 3)][substr == rc3x, N] < 8) {
      nmin <- 2 # only happens for HG
    }
    x1 <- # aug: rc3 rc6 other r dr
      f240715a(maxpeer = maxp) %>% # national rc6aug(rc3)
      # .[rc3==rc3x]%>%
      .[grep(rc3x, rc3)] %>%
      .[, unique(other)] %>%
      f240715b(rc3 = rc3x, rc6aug = ., nmin = nmin)
    f1 <- function(
        x2 = x1[, -"rc6"], # each col is 1,2,3 attribute of rc6
        rc6 = x1[, rc6] # length=nrow(x)
        ) {
      x3 <- as.list(1:3)
      x4 <- as.list(seq_along(x2))
      for (j in seq_along(x2)) {
        for (i in 1:3) {
          x3[[i]] <- paste0(rc6[x2[, j, with = F] == i], collapse = "")
        }
        x4[[j]] <- x3
      }
      x4 # a list of 3-lists, each element a pasteup of rc6
    }
    x5 <- f1(x = x1[, -"rc6"])
    stopifnot(!unlist(lapply(x5, function(x) {
      nchar(Reduce(paste0, x)) / 6
    })) %>%
      range(.) %>%
      diff(.)) # must be zero
    x5
  }
f240719aFun <-
  function(
      # best 3-partition using kfold
      ...) {
    x11 <- f240719a(...)
    f240719ad <<- x11
    putt(f240719ad)
  }
f240810a <-
  function(
      # leaflet special/custom function for index app, copied from anest.shiny/acycle/applib.R
      zoomx = 6.5,
      x3a = pxosrdo2dd, # rc6 polygons
      pva = z110, # pva: for labelling with £/m2
      rcx = regpcode(pxosrdo2dd$name[grep("^NG", pxosrdo2dd$name)]), # vector(rc6) - these get shaded
      targetrcx = rcx[1], # rc6 - gets shaded differently
      minzoom = 6,
      maxzoom = 11,
      palx = leaflet::colorNumeric(palette = cobalt()[c(2, 4)], domain = 0:1) # only target gets special shading 4='punk', rest is green
      ) {
    gsx <- grepstring(rcx, caret = T, dollar = T)
    width <- 1000 * .7
    height <- 1000 * .7
    w1 <- 1
    l1 <- .08
    addlegend <- F
    uproj3 <- # desired longlat
      "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" %>%
      CRS(.)
    x3 <- # data slot is pc
      x3a[grep(gsx, regpcode(x3a@data$name)), ] # selected polygons named irreg
    x4 <- # add rc6 and vv
      data.table(x3@data) %>%
      .[, rc6 := regpcode(name)] %>%
      .[, .(name, rc6, col = as.numeric(rc6 == targetrcx))]
    x3@data <- data.frame(x4)
    x5 <- pva[grep(gsx, rcx), .(rcx = sort(rcx))]
    labels <-
      paste0(
        x5[, irregpcode(rcx)],
        " ",
        prettyNum(as.integer(pva[x5, round(ppm2, -1), on = c(rcx = "rcx")]), big.mark = ",", scientific = FALSE)
      ) %>%
      lapply(htmltools::HTML)
    x7 <- leaflet(
      x3,
      width = width,
      height = height,
      options = leafletOptions(
        zoomControl = F,
        minZoom = minzoom,
        maxZoom = maxzoom,
        zoomDelta = 2
      )
    )
    x8 <- x7 %>%
      addPolygons(
        stroke = T,
        fillColor = ~ palx(col),
        smoothFactor = 0, # remove white slivers
        weight = 1,
        opacity = 1,
        color = "transparent", # "steelblue",
        dashArray = "2",
        fillOpacity = .5,
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "12px",
          direction = "auto"
        )
      ) %>%
      addProviderTiles(providers$CartoDB.Positron)
    x8
  }
f240810b <-
  function(
      # leaflet special/custom function for index app, copied from anest.shiny/acycle/applib.R
      x1 =
        data.table(
          rc6 = sort(rc6),
          col = rep(mycols, length(rc6))[1:length(rc6)] # should be meaningful
        ),
      rc6 = c("NG-1--", "S--10-", "SE-25-"),
      mycols = cobalt(), # only to generate x1
      x2 = pxosrdo2dd, # rc6 SPDF
      pva = z110, # pva: for labelling with £/m2
      zoomx = 6.5,
      minzoom = 6,
      maxzoom = 11,
      width = 1000 * .7,
      height = 1000 * .7,
      w1 = 1,
      l1 = .08,
      addlegend = F,
      uproj3 = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" %>% CRS(.),
      palx = leaflet::colorFactor(
        palette = x1[, col],
        domain = x1[, rc6]
      )) {
    x1[, stopifnot(all(nchar(rc6) == 6))]
    x1 <- # alpha sort rc
      x1[order(rc6)]
    x2 <- # alpha sort pc
      x2[order(x2@data$name), ]
    x3 <- # SPDF subset using [
      x1[, grepstring(rc6, caret = T, dollar = T)] %>%
      grep(., regpcode(x2@data$name)) %>%
      sort(.) %>% # retain alpha sort
      x2[., ]
    { # check
      all.equal(
        x1[, rc6], # requested
        regpcode(x3@data$name) # spdf
      ) %>%
        stopifnot(.) # or fail
    }
    x3@data <- # assign cols
      x1[, .(rc6)] %>%
      data.frame(.)
    x4 <- # tooltip labels
      x1[
        ,
        paste0(
          x1[, irregpcode(rc6)],
          " ",
          prettyNum(as.integer(pva[x1, round(ppm2, -1), on = c(rcx = "rc6")]), big.mark = ",", scientific = FALSE)
        )
      ] %>%
      lapply(., htmltools::HTML)
    x5 <- # leaflet
      leaflet(
        x3,
        width = width,
        height = height,
        options = leafletOptions(
          zoomControl = F,
          minZoom = minzoom,
          maxZoom = maxzoom,
          zoomDelta = 2
        )
      )
    x6 <- # colour it
      x5 %>%
      addPolygons(
        stroke = T,
        fillColor = ~ palx(rc6), # ~palx(col),
        smoothFactor = 0, # remove white slivers
        weight = 1,
        opacity = 1,
        color = "transparent", # "steelblue",
        dashArray = "2",
        fillOpacity = .5,
        label = x4,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "12px",
          direction = "auto"
        )
      ) %>%
      addProviderTiles(providers$CartoDB.Positron)
    x6
  }
f240823a <-
  function(
      # winding performance table
      x0 = z321,
      x1 =
        x0$ses$estdt %>%
          .[order(nx, date0)],
      nx = 1) {
    x2 <-
      x1 %>%
      .[, .(ddate = seq.Date(from = date0[1], to = date1[.N], by = 1))]
    x3 <-
      x2[, .(mdate = seq.Date(from = min(ddate) + 1, to = max(ddate) + 1, by = "m") - 1)]
    x4 <-
      x1[, sort(unique(nx))]
    x5 <-
      setNames(as.list(x4), x4)
    for (i in seq_along(x4)) {
      x5[[i]] <-
        x1[nx == x4[i]][x2, on = c(date0 = "ddate"), roll = Inf] %>%
        .[, .(date0, nx, x = cumsum(c(0, xdot.daily[-1])))] %>% # start cumulation day 1, not day 0
        .[x3, on = c(date0 = "mdate")] %>%
        .[, .(date = date0[-1], nx[-1], xdot = diff(x))]
    }
    x6 <-
      zoo::zoo(x5[[nx]][, .(r = exp(xdot) - 1)], x5[[i]][, date])
    x7 <-
      PerformanceAnalytics::table.CalendarReturns(x6, digits = 2)
    x7
  }
f240915b <-
  function(
      rc6x = "W--2--",
      outthreshx = c("0.0", "0.1", "0.5"), # outthreshx='zero'
      rtk = c("raw", "trim", "kfx"), # rtk='raw'
      stat = c("r2", "rmse") # , #stat='r2'
      # nn='f240915ad'
      ) {
    print(outthreshx)
    # getgd(nn)
    outthreshx <- match.arg(outthreshx)
    rtk <- match.arg(rtk)
    stat <- match.arg(stat)
    x1 <- c(0, .1, .5)[match(outthreshx, c("0.0", "0.1", "0.5"))]
    x2 <- c("sser", "ssei", "ssek")[match(rtk, c("raw", "trim", "kfx"))]
    x3 <- c("lab", x2)
    x4 <- f240915ad
    x5 <-
      x4[rc6 == rc6x][outthresh == x1][, x3, with = F] %>%
      setnames(., c("lab", rtk)) %>%
      .[] %>%
      .[, lab := substr(lab, 1, 1)] %>%
      .[, lab := ifelse(lab == "C", "L", lab)]
    if (stat == "r2") {
      x5[[2]] <- 1 - x5[[2]] / x4[rc6 == rc6x][outthresh == x1][, sstr]
    }
    x5
  }
f240915c <-
  function(
      rc6x = "W--2--",
      outthreshx = c("0.0", "0.1", "0.5"),
      stat = c("r2", "rmse"), # stat='r2'
      # nn='f240915ad',
      digit = 4) {
    if (!exists("f240915ad")) {
      print("data.table f240915ad needs to be loaded")
    }
    x6 <-
      f240915b(rtk = "k", rc6x = rc6x, out = outthreshx, stat = stat) %>%
      .[f240915b(rtk = "t", rc6x = rc6x, out = outthreshx, stat = stat), on = c(lab = "lab")] %>%
      .[f240915b(rtk = "r", rc6x = rc6x, out = outthreshx, stat = stat), on = c(lab = "lab")]
    labs <- data.table(lab = c("L", "N", "X", "R", "A"), relab = c("London", "National", "Local", "Region", "Area"))
    x7 <- labs[x6, on = c(lab = "lab")]
    x8 <-
      x7[order(-kfx)] %>%
      .[, .(relab, kfx = round(kfx, digit), trim = round(trim, digit), raw = round(raw, digit))]
    x8
  }
f240920a <-
  function(
      # split ordered rc3 into all feasible rc6 3-groups
      rc3x = "NG-",
      stepripx = steprip,
      hmin = 20000 # min rip datapoints
      ) {
    stopifnot(z110[, rc3x %in% rcx])
    x0 <-
      z110 %>%
      .[nchar(rcx) == 6] %>%
      .[grep(rc3x, rcx)] %>%
      .[order(ppm2)]
    x1 <- # rc6 ordered
      x0[, rcx]
    x2 <- # cumulative ntrans
      x0 %>%
      .[, .(rcx, pv, m2, ppm2, nh = 0 * NA, ii = 1:.N)] %>%
      .[, let(nh = nrow(coread(rcx, stepripx))), ii] %>%
      .[, let(cumh = cumsum(nh))] %>%
      .[, .(ii, cumh)]
    if (max(x2) < 3.5 * hmin) {
      return(NULL)
    } # strictly just 3*, but scrape pass will fail on next line
    x3 <-
      x2[-.N, CJ(ii, ii)] %>%
      setnames(., c("end1", "end2")) %>%
      .[(end1 < end2)] %>%
      .[x2, on = c(end1 = "ii"), nomatch = NULL] %>%
      .[, .(end1, end2, cumh1 = cumh)] %>%
      .[x2, on = c(end2 = "ii"), nomatch = NULL] %>%
      .[, .(end1, end2, cumh1, cumh2 = cumh)] %>%
      .[, .(end1, end2, end3 = length(x1), n1 = cumh1, n2 = cumh2 - cumh1, n3 = x2[, max(cumh)] - cumh2)] %>%
      .[n1 > hmin & n2 > hmin & n3 > hmin] %>%
      .[, ii := 1:.N] %>%
      .[, hmin := pmin(n1, n2, n3)] %>%
      .[, .(
        s1 = paste0(x1[1:.BY[[1]]], collapse = ""),
        s2 = paste0(x1[(1 + .BY[[1]]):.BY[[2]]], collapse = ""),
        s3 = paste0(x1[(1 + .BY[[2]]):.BY[[3]]], collapse = "")
      ), .(end1, end2, end3, ii, hmin)]
    x3
  }
f240920b <-
  function(
      # split text string into blocks of n characters, fast
      text,
      n = 6) {
    sst <- strsplit(text, "")[[1]]
    do.call(paste0, lapply(seq_len(n), function(i) {
      idx <- rep(FALSE, n)
      idx[i] <- TRUE
      sst[idx]
    }))
  }
f240920c <-
  function(
      # split ordered rc3 into all feasible rc6 2-groups
      rc3x = "NG-",
      stepripx = steprip,
      hmin = 20000 # min rip datapoints
      ) {
    stopifnot(z110[, rc3x %in% rcx])
    x0 <-
      z110 %>%
      .[nchar(rcx) == 6] %>%
      .[grep(rc3x, rcx)] %>%
      .[order(ppm2)]
    x1 <- # rc6 ordered
      x0[, rcx]
    x2 <- # cumulative ntrans
      x0 %>%
      .[, .(rcx, pv, m2, ppm2, nh = 0 * NA, ii = 1:.N)] %>%
      .[, let(nh = nrow(coread(rcx, stepripx))), ii] %>%
      .[, let(cumh = cumsum(nh))] %>%
      .[, .(ii, cumh)]
    if (max(x2) < 2.5 * hmin) {
      return(NULL)
    } # strictly just 2*, but scrape pass will fail on next line
    x3 <- # feasible break range
      x2[, c(min(which(cumh > hmin)), min(which((max(cumh) - cumh) < hmin) - 1))] # first, final end for group 1
    if (diff(x3) < 0) {
      return(NULL)
    }
    x4 <-
      data.table(
        ii = x3[1]:x3[2] # valid range of end for group 1
      ) %>%
      .[,
        j = .(
          s1 = paste0(x1[1:.BY[[1]]], collapse = ""), # all in valid range
          s2 = paste0(x1[(.BY[[1]] + 1):length(x1)], collapse = "")
        ),
        by = ii
      ]
    x4
  }
f241108a <-
  function(tc = typeC, tb = tbinC) {
    x1 <-
      list(
        paste0("Geographic bins: ", c(A = "All", L = "Local", N = "National", C = "Custom")[tc]),
        paste0("Time sampling: ", c("Low Frequency", "High Frequency", "Annual")[tb])
      )
    x1
  }
f241119a <-
  function(
      # solve single nx -> estdt with no pra; replaces f240710a
      nxx = 1,
      # steprip1=steprip, #old format
      steprip2 = "c:\\users\\giles\\anest.repo\\anest.shiny\\acycle\\app241119\\smallrip", # 1/3 size format
      stepprax = stepprav2,
      dfn = dfnG[, date],
      geo = geoaG[, .(nx = nx - min(nx) + 1, lab, rc9 = rc6)],
      outthresh = .1,
      kfold = 5,
      randomise = F, # for testing
      sectorwise = F, # flag to split folds on complete sectors<<<<<<<<<<<<< T
      usepra = F, # optional<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< F
      newused = c(".", "N", "U"),
      houseflat = c(".", "H", "F") # typex field added to rip 240826, values UH/NH/UF/NF for new/house
      ) {
    stopifnot(nxx %in% geo[, nx]) # add 241116
    newused <- match.arg(newused)
    houseflat <- match.arg(houseflat)
    if (usepra) {
      x2 <- coread(
        rcx = geo[nx == nxx][, rc9],
        step = stepprax,
        colClasses = list(numeric = "retsa")
      )
    } else {
      # browser()
      # x1 <- #rip read
      #   coread(
      #     rcx=geo[nx==nxx][,rc9],
      #     step=steprip1,
      #     colClasses=list(numeric=c('retsa'),Date=c('buydate','selldate')) #c('retraw','retsa')
      #   )
      x1 <- # rip read coread2 steprip2
        coread2(
          rcx = geo[nx == nxx][, rc9],
          step = steprip2,
          colClasses = list(numeric = c("retsa"), integer = c("buydate", "selldate")) # c('retraw','retsa')
        )
      if (paste0(newused, houseflat) != "..") {
        x1 <- x1[grep(paste0("^", newused, houseflat, "$"), type)]
      }
      x1[, idhash.selldate := as.character(1:.N)]
      x2 <- # accrue
        f221209a(
          geo = geo[nx == nxx],
          fur = x1,
          dfn = dfn,
          applygeo = F
        )
    }
    x4 <- lm(
      retsa ~ . - 1,
      x2[, !c("idhash.selldate", "rc9")] # all, no outlier reject
    )
    x5 <- residuals(x4)
    x6a <- x2[, .(idhash.selldate, rc6 = substr(rc9, 1, 6), res = x5)] # per rc6
    x6b <- x6a[, .(lo = quantile(res, outthresh / 2), hi = quantile(res, (1 - outthresh / 2))), rc6] # apply thresholds *by rc6*
    x6c <- x6b[x6a, on = c(rc6 = "rc6")][(lo <= res & res <= hi), .(rc6, idhash.selldate)] # select inlier
    x6d <- x2[x6c[, .(idhash.selldate)], on = c(idhash.selldate = "idhash.selldate")] %>%
      .[order(rc9, idhash.selldate)]
    if (randomise) { # to test it's doing something
      x6d <- x6d[sample(.N)]
    } else if (
      sectorwise &
        x6d[, length(unique(rc9)) * (kfold - 1) / kfold] >= 1 # rc9 [these really are sectors!] in each training set>=1
    ) {
      x6d[, ktile := (as.integer(as.factor(rc9)) %% kfold) %% kfold + 1]
    } else {
      x6d[, ktile := ((1:.N)) %% kfold + 1]
    }
    x10 <- as.list(NULL)
    for (i in 1:kfold) {
      x7 <- x6d[ktile == i, ] # target ktile inlier
      x8 <- x6d[ktile != i, ] # train on other ktiles
      x9 <- lm(
        retsa ~ . - 1,
        x8[, !c("idhash.selldate", "rc9", "ktile")] # train
      )
      x10[[i]] <- x7[, .(sse = sum((
        .SD[ktile == i, retsa] -
          predict(x9, newdata = .SD[ktile == i])
      )^2), n = .N, i), rc9]
    }
    x11 <- # here sum by rc6
      rbindlist(x10) %>%
      .[, .(sse = sum(sse), n = sum(n)), .(rc6 = substr(rc9, 1, 6))]
    x12 <- lm(
      retsa ~ . - 1,
      x6d[, !c("idhash.selldate", "rc9", "ktile")] # all inlier
    )

    x11a <- # Inlier
      data.table(x6d[, .(rc6 = substr(rc9, 1, 6))], res = residuals(x12)) %>%
      .[, .(ssei = sum(res^2), n = .N), rc6]
    x11b <- # kfold
      x11[, .(rc6, ssek = sse, n)]
    x11c <- # Raw
      data.table(x2[, .(rc6 = substr(rc9, 1, 6))], res = residuals(x4)) %>%
      .[, .(sser = sum(res^2), n = .N), rc6]
    x11d <- # tss called ssrt 'sum square total return'
      x2[, .(rc6 = substr(rc9, 1, 6), retsa)] %>%
      .[, .(sstr = sum(retsa^2), n = .N), rc6]
    x12a <- # combine 4 ss
      x11a[x11b, on = c(rc6 = "rc6")][x11c, on = c(rc6 = "rc6")][x11d, on = c(rc6 = "rc6")] %>%
      .[, .(rc6, ssei, ssek, sser, sstr, n, nx = nxx)]
    x13 <-
      x12 %>%
      .[["coefficients"]] %>%
      data.table(
        xdotd = as.numeric(.),
        date = as.Date(substr(names(.), 2, 11))
      ) %>%
      .[, days := as.numeric(diff(c(min(dfn), date)))] %>%
      .[, xdot := as.numeric(xdotd * days)] %>%
      .[, x := cumsum(xdot)] %>%
      .[, .(
        nx = nxx,
        date,
        xdotd,
        days,
        xdot,
        # xdotse,for this would need to do summary(lm)
        x,
        lab = geo[nx == nxx][1, lab]
      )] %>%
      .[, ii := 1:.N, lab] %>%
      .[, col := as.factor(lab)] %>%
      .[]
    x13a <-
      data.table( # these are totals
        allsse.insam = sum(residuals(x12)^2), # rss all inlier, single lm
        allsse.osam = x11[, sum(sse)], # rss kxv out-sample
        allsse.raw = sum(residuals(x4)^2), # rss all in/outlier, single lm
        allsse.tot = x2[, sum(retsa^2)], # tss
        rsqraw = summary(x4)$r.squared # no outlier rejection, all obs
      )
    x14 <- list(
      estdt = x13, # all obsvns
      kfoldsse = x12a, # x11, #kfold sse(rc6)
      all = x13a # rsq is literally all; alltilesse is single solution i/s sse on all inliers/tiles; sumtilesse is sum of o/s sse on all inliers/tiles
    )
    x14
  }
gg1 <-
  function(
      # discrete hue-spaced color scheme
      n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
grepstring <-
  function(
      # grep for any in x
      x = regpcode(metro()), # character vector
      dollar = F,
      caret = T) {
    if (caret) x <- paste0("^", x)
    if (dollar) x <- paste0(x, "$")
    paste(x, collapse = "|") # OR function does the work
  }
irregpcode <-
  function(
      # convert regular (area,district,sector,unit) 12 char to 'normal' postcode
      x) {
    x1 <- substr(x, 1, pmin(6, nchar(x)))
    x2 <- substr(x, pmin(7, nchar(x)), nchar(x))
    gsub(patt = " $", rep = "", x = paste(gsub(patt = "\\-", rep = "", x = x1), gsub(patt = "\\-", rep = "", x = x2)))
  }
labxnnn <-
  function(
      # convenience labeller
      n,
      len = 3,
      char = "x") {
    paste0(char, zeroprepend(n, len))
  }
mkdirn <-
  function(
      # make dir (WINDOWS)
      dd) {
    if (all(is.na(file.info(dd)))) {
      suppressWarnings(shell(paste0("mkdir ", dd)))
    }
  }
ncpus <-
  function( # number of CPUs for snow
  ) {
    shell(
      "wmic cpu get NumberOfCores,NumberOfLogicalProcessors",
      intern = TRUE
    ) %>%
      strsplit(., " ") %>%
      `[[`(., i = 2) %>%
      as.numeric(.) %>%
      max(., na.rm = T) %>%
      min(., 8)
  }
pad1 <-
  function(x) {
    n1 <- nchar(x)
    x[n1 == 1] <- paste0(x[n1 == 1], paste(collapse = "", rep(rcs(), 2)))
    x[n1 == 2] <- paste0(x[n1 == 2], rcs())
    x
  }
parsepcode <-
  function(
      # parse a vector of 'irregular' (normal) postcode
      pc = c("AL1 1AD", "AL1 1BD", "AL1 1CD")) {
    x <- lapply(pc, ppc) %>%
      lapply(., data.table) %>%
      lapply(., t) %>%
      Reduce(rbind, .) %>%
      data.frame(.) %>%
      lapply(., unlist) %>%
      suppressWarnings(.)
    x <- lapply(x, `names<-`, NULL)
    names(x) <- names(ppc(pc[1]))
    x
  }
pcab <-
  function(
      # beta : only when g=1 are these covariance-related, otherwise 'sensitivity' to a scaled factor
      xest) {
    nbar <- ncol(xest$x[, -"date"])
    kbar <- ncol(xest$eig$vectors)
    if (xest$par$iscale == "cov") {
      sigma <- rep(1, nbar)
    } else if (xest$par$iscale == "cor") {
      sigma <- xest$sigma
    }
    x3 <- diag(sigma) %*%
      xest$eig$vectors %*%
      diag(sqrt(xest$eig$values)) %*%
      tanrot(xest$tantheta, jbar = kbar) %*%
      diag(xest$g[1:kbar])
    rownames(x3) <- names(xest$x[, -"date"])
    colnames(x3) <- zeroprepend(1:ncol(x3), 3)
    x3
  }
pcadrc <-
  function(
      # class='pcaest' | min-range-rotator taking pcaest as input
      pca,
      date0 = as.Date("1994-12-31"),
      kbar = 3) {
    dates <- c(date0, pca$date)
    x2 <- x1 <- NA
    for (i in 1:nrow(pca$x)) {
      x0 <- data.table(
        y = unlist(t(pca$x[i, -1])),
        x = pcab(pca)[, 1:kbar]
      ) %>%
        setnames(., c("y", paste0("b", 1:kbar))) %>%
        lm(y ~ ., .) %>%
        summary(.)
      x1[i] <- x0[["adj.r.squared"]]
      x2[i] <- x0[["r.squared"]]
    }
    x3 <- data.table(
      days = as.numeric(diff(dates)), # days
      rbarsq = x1, # rbarsq
      rsq = x2,
      r = apply(pcaz(pca)[, 2:kbar]^2, 1, sum), # k=2,3 variance
      start = dates[-length(dates)], # period start
      end = dates[-1], # period end
      pcaz(pca)[, 2:3] %>%
        coredata(.) %>%
        data.table(.) %>%
        setnames(., c("z2", "z3"))
    )
    x3
  }
pcaest <-
  function(
      # local version with tantheta[1]!=0 derived from symmetry in 2/3 plane (see last lines)
      x = data.table(date, retmat),
      iscale = c("cov", "cor"),
      method = c("ML", "unbiased"),
      signmethod = c("ordered", "reference"),
      rcref = "WC-3", # reference series name
      refpol = c(1, -1, -1), # polarity associated
      rollsum = 1, # apply rollsum - not used
      verbose = T,
      center = T,
      pcawin = x[, range(date)],
      rotwin = c(x[, sort(date)[2]], pcawin[2]), # default to eliminate first period (SNR motive)
      rotate = T, # always T
      krot = 2:3,
      doplot = F) {
    iscale <- match.arg(iscale)
    method <- match.arg(method)
    signmethod <- match.arg(signmethod)
    pcawin <- sort(round(as.Date(pcawin)))
    stopifnot(is.Date(pcawin) & all(pcawin %in% x[, date]) & length(pcawin) == 2)
    rotwin <- sort(round(as.Date(rotwin)))
    stopifnot(is.Date(rotwin) & all(rotwin %in% x[, date]) & length(rotwin) == 2)
    ipcawin <- setNames(match(pcawin, x[, date]), pcawin)
    irotwin <- setNames(match(rotwin, x[, date]), rotwin)
    nbar <- ncol(x) - 1
    x0 <- rollapply(x[, -"date"], width = rollsum, FUN = sum, partial = T, align = "right")
    if (rollsum != 1 & verbose) {
      print(paste0("rollsum=", rollsum, " in pcaest"))
    }
    x1 <- cov.wt(x0[ipcawin[1]:ipcawin[2], ],
      method = method,
      center = center,
      cor = T
    )
    x2 <- x1[[iscale]]
    x3 <- eigen(x = x2)
    dimnames(x3$vectors) <- dimnames(x1$cov)
    thresh <- sqrt(.Machine$double.eps)
    if (any(x3$values < thresh)) {
      kbar <- max(which(x3$values > thresh))
      x3$values <- x3$values[1:kbar]
      x3$vectors <- x3$vectors[, 1:kbar, drop = F]
    } else {
      kbar <- ncol(x2)
    }
    if (signmethod == "ordered") {
      print("ordered method")
      signfinder <-
        function(evec, pola = c(1, -1, -1)[1:min(3, length(evec))]) {
          # sign by regression on 1st 3 even zero phase cos(x.centred); pola is tgt for last
          n <- length(evec) - 1
          x <-
            data.table(
              y = evec,
              f0 = rep(1, n + 1),
              f1 = -cos(pi * (0:n) / n),
              f3 = cos(-2 * pi * (0:n) / n)
            )
          x1 <- summary(lm(y ~ . - 1, x))$coefficients
          if (any(is.na(x1[, 2]))) {
            x2 <- sign(sum(x1[, 1] * pola))
          } else {
            x2 <- sign(sum(x1[, 3] * pola)) # sum of t-stats (scale-invariant)
          }
          x2
        }
      x4 <- unlist(lapply(data.table(x3$vectors), signfinder))
      if (any(is.na(x4))) {
        x4 <- rep(1, length(x4))
      }
    } else {
      stopifnot(rcref %in% rownames(x3$vectors))
      iref <- match(rcref, rownames(x3$vectors))
      jref <- seq_along(refpol)
      x4 <- c(refpol * sign(x3$vectors[iref, jref]), rep(1, ncol(x3$vectors) - length(jref)))
    }
    x3$vectors <- sweep(x3$vectors,
      STAT = x4,
      MAR = 2,
      FUN = `/`
    )
    x4 <- list(
      x = x,
      xrs = x0, # rs 'rollsum applied'
      date = x[, date],
      sigma = sqrt(diag(x1$cov)),
      xx = x2,
      eig = x3,
      tantheta =
        c(
          .13 # base rotation for 240502 run
          - 0.01008349, # adjustment applied 240503
          rep(0, nbar - 1)
        ),
      g = rep(1, nbar),
      par = list(
        method = method,
        iscale = iscale,
        xsect = "",
        rollsum = rollsum,
        kbar = kbar,
        ipcawin = ipcawin,
        irotwin = irotwin
      )
    )
    if (rotate) { # solve theta to minimise range(z(k=2,3)), update tantheta
      x4 <- pcarot0(x4)
    }
    if (doplot) {
      plot(cumsum(pcaz(x4))[, 1:3], scr = 1, col = 1:3)
    }
    x4
  }
pcah <-
  function(
      # h holdings (unit variance factor portfolios)
      xest) {
    nbar <- ncol(xest$x[, -"date"])
    kbar <- ncol(xest$eig$vectors)
    if (xest$par$iscale == "cov") {
      sigmainv <- rep(1, nbar)
    } else if (xest$par$iscale == "cor") {
      sigmainv <- 1 / xest$sigma
    }
    x3 <- diag(sigmainv) %*%
      xest$eig$vectors %*%
      diag(1 / sqrt(xest$eig$values)) %*%
      tanrot(xest$tantheta, jbar = kbar) %*%
      diag(1 / xest$g[1:kbar])
    rownames(x3) <- names(xest$x[, -"date"])
    colnames(x3) <- zeroprepend(1:ncol(x3), 3)
    x3
  }
pcajscale <-
  function(
      # column scalar
      xest,
      jscale = c("var", "gross", "long", "short"),
      beta1 = F # modified default to FALSE
      ) {
    jscale <- match.arg(jscale)
    x1 <- copy(xest)
    x1$g <- rep(1, ncol(xest$x) - 1) # unscaled (=varscaled)
    x2 <- pcah(x1)
    if (jscale == "gross") {
      x1$g <- apply(abs(x2), 2, sum) * .5
    } else if (jscale == "long") {
      x1$g <- apply(x2 * (x2 > 0), 2, sum)
    } else if (jscale == "short") {
      x1$g <- apply(abs(x2) * (x2 < 0), 2, sum)
    }
    x1$par$jscale <- jscale
    if (beta1) { # mean(beta)=1
      print(paste0("mean pcab=", mean(pcab(x1)[, 1])))
      x1$g[1] <- x1$g[1] / mean(pcab(x1)[, 1])
      x1$par$beta1 <- T
    } else {
      x1$par$beta1 <- F
    }
    x1
  }
pcaobj <-
  function(
      # objective function for rotation
      tantheta = 0,
      xest = y2a,
      j = 2,
      years = 20) {
    stopifnot(1 < j) # see definition of rotation
    dstart <- Sys.Date() - years * 365.25
    xest$tantheta[j] <- tantheta
    diff(range(cumsum(pcaz(xest)[dstart <= xest$date, j])))
  }
pcarot0 <-
  function(
      # class='pcaest' | min-range-rotator taking pcaest as input
      x1, # pcaest object
      krot = 2:3,
      irotwin = x1$par$irotwin[1]:x1$par$irotwin[2]) {
    krot <- setdiff(krot, 1)
    krot <- krot[krot <= ncol(x1$x[, -"date"])]
    f1 <- function(rr = 0, jj = 2, xm, irotwin) { # apply rr with jj
      x1 <- copy(xm)
      x1$tantheta[jj] <- rr
      xx <- diff(range(cumsum(pcaz(xest = x1)[irotwin])[, jj, drop = F]))
      xx
    }
    kbar <- x1$par$kbar
    initialgrid <- ((-20:20) / 21) + .01
    kset <- sort(unique(pmin(krot, kbar)))
    for (i in seq_along(kset)) {
      k <- kset[i]
      start <- initialgrid[which.min(sapply(initialgrid, f1, xm = x1, jj = k, irotwin = irotwin))]
      x1$tantheta[k] <- nlm(f = f1, p = start, j = k, xm = x1, irotwin = irotwin)$estimate
    }
    x1 # with updated tantheta
  }
pcaz <-
  function(
      # factor timeseries
      xest = pcaestd,
      x = xest$x,
      h = pcah(xest)) {
    zoo(as.matrix(x[, -"date"]) %*% h, x[, date])
  }
ppc <-
  function(pc = "EC2R 8AH") {
    if (nchar(pc) < 2) {
      return(list(area = ifelse(grepl("[A-Z,a-z]", pc), paste0(toupper(pc), "--"), ""), district = "", sector = "", unit = ""))
    }
    chkpcode(pc)
    pc <- toupper(pc)
    gg <- gregexpr(patt = " ", pc)
    x <- strsplit(pc, split = " ")
    out <- unlist(lapply(x, "[[", 1))
    nout <- nchar(out)
    inum <- as.numeric(regexpr("[0-9]", out))
    area <- pc
    sector <- unit <- district <- rep("", length(pc))
    area[inum == 2] <- substr(out[inum == 2], 1, 1)
    area[inum == 3] <- substr(out[inum == 3], 1, 2)
    district[inum == 2] <- substring(out[inum == 2], 2)
    district[inum == 3] <- substring(out[inum == 3], 3)
    if (any(lapply(x, length) > 1)) { # inbound code exists
      stopifnot(all(lapply(x, length) == 2)) # exists for all
      inb <- unlist(lapply(x, "[[", 2))
      nin <- nchar(inb)
      sector <- substr(inb, 1, 1)
      unit <- substring(inb, 2, nin)
    }
    list(area = area, district = district, sector = sector, unit = unit)
  }
pre0 <-
  function(
      # utility prepends zero row -> tab and optionally cumulate
      zoox = zoo(f230810c(aio = aio)[, -c("date", "ii")], f230810c(aio = aio)[, date]),
      charx = substr(colnames(zoox)[1], 1, 1),
      docum = T,
      len = 3) {
    x1 <- # z23
      zoox %>%
      `dimnames<-`(., NULL) %>%
      rbind(matrix(0, 1, ncol(zoox)), .)
    if (docum) {
      x1 <- cumsum(x1)
    }
    x1 <- x1 %>%
      data.table(.) %>%
      setnames(., labxnnn(n = 1:ncol(.), char = charx, len = len)) %>%
      cbind(data.table(ii = 0:(nrow(.) - 1)), .)
    x1
  }
pxmoreg1 <-
  function() {
    x <-
      structure(
        list(
          area = c(
            "GY", "JE", "AL", "CB", "CM", "CO", "HP",
            "IP", "LU", "NR", "PE", "SG", "SS", "DE", "DN", "LE", "LN", "NG",
            "S", "BR", "CR", "DA", "EN", "HA", "IG", "KT", "RM", "SM", "TW",
            "UB", "WD", "IM", "E", "EC", "N", "NW", "SE", "SW", "W", "WC",
            "BF", "BX", "GIR", "QC", "XX", "DH", "DL", "HG", "HU", "LS",
            "NE", "SR", "TS", "WF", "YO", "BT", "BB", "BD", "BL", "CA", "CH",
            "CW", "FY", "HD", "HX", "L", "LA", "M", "OL", "PR", "SK", "WA",
            "WN", "AB", "DD", "DG", "EH", "FK", "G", "HS", "IV", "KA", "KW",
            "KY", "ML", "PA", "PH", "TD", "ZE", "BN", "CT", "GU", "ME", "MK",
            "OX", "PO", "RG", "RH", "SL", "SO", "TN", "BA", "BH", "BS", "DT",
            "EX", "GL", "PL", "SN", "SP", "TA", "TQ", "TR", "CF", "LD", "LL",
            "NP", "SA", "SY", "B", "CV", "DY", "HR", "NN", "ST", "TF", "WR",
            "WS", "WV"
          ),
          name = c(
            "Guernsey", "Jersey", "St. Albans", "Cambridge",
            "Chelmsford", "Colchester", "Hemel", "Ipswich", "Luton", "Norwich",
            "Peterborough", "Stevenage", "Southend", "Derby", "Doncaster",
            "Leicester", "Lincoln", "Nottingham", "Sheffield", "Bromley",
            "Croydon", "Dartford", "Enfield", "Harrow", "Ilford", "Kingston",
            "Romford", "Sutton", "Twickenham", "Southall", "Watford", "Isle of Man",
            "London", "London", "London", "London", "London", "London", "London",
            "London", "British Forces", "Non-geographic", "Girobank HQ, Bootle",
            "Awarding Bodies", "Amazon.com returns", "Durham", "Darlington",
            "Harrogate", "Hull", "Leeds", "Newcastle", "Sunderland", "Cleveland",
            "Wakefield", "York", "Belfast", "Blackburn", "Bradford", "Bolton",
            "Carlisle", "Chester", "Crewe", "Blackpool", "Huddersfield",
            "Halifax", "Liverpool", "Lancaster", "Manchester", "Oldham",
            "Preston", "Stockport", "Warrington", "Wigan", "Aberdeen", "Dundee",
            "Dumfries", "Edinburgh", "Falkirk", "Glasgow", "Comhairle nan Eilean Siar",
            "Inverness", "Kilmarnock", "Kirkwall", "Kirkaldy", "Motherwell",
            "Paisley", "Perth", "Galashiels", "Shetland", "Brighton", "Canterbury",
            "Guildford", "Medway", "Milton Keynes", "Oxford", "Portsmouth",
            "Reading", "Redhill", "Slough", "Southampton", "Tonbridge", "Bath",
            "Bournemouth", "Bristol", "Dorchester", "Exeter", "Gloucester",
            "Plymouth", "Swindon", "Salisbury", "Taunton", "Torquay", "Truro",
            "Cardiff", "Llandrindod", "Llandudno", "Newport", "Swansea",
            "Shrewsbury", "Birmingham", "Coventry", "Dudley", "Hereford",
            "Northampton", "Stoke on Trent", "Telford", "Worcester", "Walsall",
            "Wolverhampton"
          ),
          rc = c(
            "GY-", "JE-", "AL-", "CB-", "CM-", "CO-",
            "HP-", "IP-", "LU-", "NR-", "PE-", "SG-", "SS-", "DE-", "DN-",
            "LE-", "LN-", "NG-", "S--", "BR-", "CR-", "DA-", "EN-", "HA-",
            "IG-", "KT-", "RM-", "SM-", "TW-", "UB-", "WD-", "IM-", "E--",
            "EC-", "N--", "NW-", "SE-", "SW-", "W--", "WC-", "BF-", "BX-",
            "GIR", "QC-", "XX-", "DH-", "DL-", "HG-", "HU-", "LS-", "NE-",
            "SR-", "TS-", "WF-", "YO-", "BT-", "BB-", "BD-", "BL-", "CA-",
            "CH-", "CW-", "FY-", "HD-", "HX-", "L--", "LA-", "M--", "OL-",
            "PR-", "SK-", "WA-", "WN-", "AB-", "DD-", "DG-", "EH-", "FK-",
            "G--", "HS-", "IV-", "KA-", "KW-", "KY-", "ML-", "PA-", "PH-",
            "TD-", "ZE-", "BN-", "CT-", "GU-", "ME-", "MK-", "OX-", "PO-",
            "RG-", "RH-", "SL-", "SO-", "TN-", "BA-", "BH-", "BS-", "DT-",
            "EX-", "GL-", "PL-", "SN-", "SP-", "TA-", "TQ-", "TR-", "CF-",
            "LD-", "LL-", "NP-", "SA-", "SY-", "B--", "CV-", "DY-", "HR-",
            "NN-", "ST-", "TF-", "WR-", "WS-", "WV-"
          )
        ),
        .Names = c(
          "area",
          "name", "rc"
        ), row.names = c(NA, -129L), class = "data.frame"
      )
    data.table(x)
  }
rcs <-
  function() {
    "-"
  }
regpcode <-
  function(
      # parse irregular postcode to regular 12-char (area,district,sector,unit)
      rawcode = c("AL1 1AD", "AL1 1BD", "AL1 1CD"), x = parsepcode(rawcode)) {
    rawcode <- gsub(patt = "  ", rep = " ", rawcode)
    Reduce(paste0, lapply(x, pad1))
  }
rmifgl <-
  function(
      # remove if global
      x # character=names of non-function objects in .GlobalEnv
      ) {
    for (i in seq_along(x)) {
      if (
        exists(x[i], envir = globalenv()) &&
          mode(get(x[i], envir = globalenv())) != "function"
      ) {
        rm(list = x[i], envir = globalenv())
      }
    }
  }
rr3 <-
  function(
      # rotate 2 out of jbar in the (x1,xjrot) plane
      tantheta = 1,
      jrot = 3,
      jbar = 3) {
    x1 <- diag(jbar)
    if (1 < jrot) {
      x1[matrix(c(
        c(1, jrot, 1, jrot),
        c(1, 1, jrot, jrot)
      ), 4, 2)] <- rrr2(tantheta)
    }
    x1
  }
rrr2 <-
  function(
      # rotate 2D from a real number
      tantheta = .1) {
    th <- atan(tantheta)
    matrix(c(cos(th), -sin(th), sin(th), cos(th)), 2, 2)
  }
tanrot <-
  function(
      # rowvector1 = rowvector0 x tanrot(tantheta); rotate coordinates not axes
      tantheta = rep(0, 3), # rotation angle: 2->3,2->1,3->1
      jbar = 5, # , #should be kbar (backward-compatibility issue)
      reorder = T,
      kord = c(3, 2, 1) # normal-to-1 is done last, normal-to-3 is first
      ) {
    # this is the legacy convention:
    # 1 rotation angles are stored in order 2->3,1->2,1->3 so was (0,+,+) with 0 enforced
    # 2 rotation order was and is 2->1,3->1,2->3
    if (reorder) { # reorder legacy-order -> by-normal order
      tti <- # by-normal-order
        c( # this is information: where to find the angle for a particular normal, using legacy convention
          1, # 1 means normal-to-1 is found in index 1 [applied third but this info found in kord, not tti]
          3, # 3 means normal-to-3 is found in index 2 [applied first]
          2 # 2 means normal-to-2 is found in index 3 [applied second]
        )
    } else {
      tti <- # unch
        c(1, 2, 3)
    }
    x0 <- # tantheta in order of normals
      tantheta[tti]
    x1 <- # tantheta in order of normals augmented with from and to
      data.table(
        x = c(2, 1, 1), # from dimension->
        y = c(3, 3, 2), #->to dimension
        tantheta = x0 # delta positive will increase to-dimension
      )
    x2 <- as.list(NULL)
    i <- 1
    for (i in 1:3) { # i is jnormal
      x3 <- # compute a 2d rotation moving coordinates, not axes
        rrr2(tantheta = x1[i, tantheta])
      x4 <- # assign to the to/from columns
        diag(jbar)
      jx <- x1[i, x] # from i.e. dx[,jx]/dx[theta] < 0
      jy <- x1[i, y] # to i.e. >0
      x4[x1[i, x], x1[i, x]] <- x3[1, 1]
      x4[x1[i, x], x1[i, y]] <- x3[1, 2]
      x4[x1[i, y], x1[i, x]] <- x3[2, 1]
      x4[x1[i, y], x1[i, y]] <- x3[2, 2]
      x2[[i]] <- x4
    }
    x5 <- x2[kord] %>% # composite rotation
      Reduce(`%*%`, .) # same as x2[[3]]%*%x2[[2]]%*%x2[[1]], which is correct because rotate in reverse order of normals
    x5
  }
zeroprepend <-
  function(
      # left-pad integer with zeros
      x,
      ntotal) {
    x <- as.character(x)
    stopifnot(all(nchar(x) <= ntotal)) # otherwise x is right-truncated
    z <- paste(rep("0", ntotal), collapse = "")
    zz <- rep(z, length(x))
    substr(zz, 1 + nchar(zz) - nchar(x), nchar(zz)) <- x
    zz
  }
