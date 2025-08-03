f250727b <- # PAS price array sparse
  function(# written by cgpt on 250728, has test suite etc
      x1 = data.table( # this 'non-spanning' row silently is dropped as required
        t0 = as.Date("1996-05-11"),
        t1 = as.Date("1996-05-12"),
        p0 = 1000, p1 = 2000, id = "y"
      ),
      x2 = as.Date(c( # define bins with right-closed/left-open i.e. equality on the right
        "1994-12-31", "1996-05-12",
        "1996-12-07", "1997-05-30", "1997-10-21"
      ))) {
    x3 <- # interval indices (bins) for t0 and t1
      findInterval(x1$t0, x2, rightmost.closed = TRUE, left.open = TRUE)
    x4 <- 
      findInterval(x1$t1, x2, rightmost.closed = TRUE, left.open = TRUE)
    x5 <- # Drop anx rows where both dates fall in the same bin
      which(x3 != x4)
    if (length(x5) == 0L) {
      return(data.table())
    } # return empty if nothing spans
    x6 <- x1[x5]
    x7 <- x3[x5]
    x8 <- x4[x5]
    x9 <- # Interval IDs for the grid
      seq_len(length(x2) - 1)
    x10 <- # Stack into long format: (row_id, interval, value)
      rbind(
      data.table(row_id = seq_len(nrow(x6)), interval = x7, value = x6$p0),
      data.table(row_id = seq_len(nrow(x6)), interval = x8, value = x6$p1)
    )[interval > 0 & interval <= (length(x2) - 1)]
    x11 <- # Full row_id × interval grid
      CJ(row_id = seq_len(nrow(x6)), interval = x9)
    x12 <- # Place values, fill missing with 0
      x11[x10, on = .(row_id, interval)]%>%
      .[is.na(value), value := 0]
    x13 <- # Pivot to wide
      dcast(x12, row_id ~ interval, value.var = "value", fill = 0)
    setnames(x13, old = as.character(x9), new = as.character(x2[-1]))
    x13%>% # Join back to x1 (keep id, t0, t1, p0, p1)
      .[x1[, row_id := .I], on = .(row_id)]%>%
      .[, -"row_id"]
  }
f250727b()
