f250727b <- # PAS price array sparse
  function(
    x1 = data.table( # written by cgpt on 250728, has test suite etc
      t0 = as.Date("1996-05-11"),
      t1 = as.Date("1996-05-12"),
      p0 = 1000, p1 = 2000, id = "y"
    ),
    x2 = as.Date(c( # define bins with right-closed/left-open i.e. equality on the right
      "1994-12-31", "1996-05-12",
      "1996-12-07", "1997-05-30", "1997-10-21"
    ))) {
  earliest_allowed <- min(x2)
  latest_allowed <- max(x2)
  if (any(x1$t0 < earliest_allowed, na.rm = TRUE)) {
    stop("Some `t0` values fall before the first bin — extend `x2` earlier")
  }
  if (any(x1$t1 > latest_allowed, na.rm = TRUE)) {
    stop("Some `t1` values fall after the last bin — extend `x2` later")
  }
  x1[, row_id := .I] # persist original row_id

  x3 <- findInterval(x1$t0, x2, rightmost.closed = TRUE, left.open = TRUE)
  x4 <- findInterval(x1$t1, x2, rightmost.closed = TRUE, left.open = TRUE)

  x5 <- which(x3 != x4)
  if (length(x5) == 0L) {
    return(data.table())
  } # nothing spans bins
  x6 <- #
    x1[x5]
  x7 <- #
    x3[x5]
  x8 <- #
    x4[x5]
  x9 <- #
    seq_len(length(x2) - 1) # all bin IDs
  x10 <- #
    rbind(
    data.table(row_id = x6$row_id, interval = x7, value = x6$p0),
    data.table(row_id = x6$row_id, interval = x8, value = x6$p1)
  )[interval > 0 & interval <= max(x9)]
  x11 <- #
    CJ(row_id = x6$row_id, interval = x9)
  x12 <- #
    x11 %>%
    .[x10, on = .(row_id, interval)] %>%
    .[is.na(value), value := 0]
  x13 <- #
    dcast(x12, row_id ~ interval, value.var = "value", fill = 0)
  x14 <- #existing bins
    intersect(as.character(x9), names(x13))
  setnames(x13,
    old = x14,
    new = as.character(x2[-1][as.integer(x14)])
  )
  x13[x1, on = .(row_id)][, !"row_id"]
}
