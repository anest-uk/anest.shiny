# Date cut points (x2)
tt <- as.Date(c(
  "1994-12-31", "1995-12-31", "1996-12-31", "1997-12-31"
))

# Construct test table (x1)
x_test <- data.table(
  id  = c("a","a","b","b","b","c","c","d","d","e","f","g","h","i","j"),
  t0  = as.Date(c(
    "1995-01-10", 
    "1995-12-31", 
    "1996-01-01", 
    "1996-05-01", 
    "1996-12-31", 
    "1994-12-31", 
    "1995-06-01", 
    "1997-01-01", 
    "1996-01-10", 
    "1995-01-01", 
    "1997-12-31", 
    "1995-07-01", 
    "1995-07-01", 
    "1996-12-30", 
    "1995-11-15"
  )),
  t1  = as.Date(c(
    "1996-01-10", 
    "1996-01-15", 
    "1996-02-01", 
    "1997-01-10", 
    "1997-01-05", 
    "1995-01-10", 
    "1995-07-01", 
    "1997-02-01", 
    "1997-01-01", 
    "1996-02-02", 
    "1997-12-31", 
    "1995-08-01", 
    "1995-08-01", 
    "1997-01-05", 
    "1996-01-05"
  )),
  p0  = 1:15,
  p1  = 101:115,
  comment = c(
    "spans bin 1→2",
    "starts on boundary (bin 1→2)",
    "within bin 2 (drop case)",
    "spans bin 2→3",
    "t1 on boundary (bin 2→3)",
    "t0 on first boundary (bin 0→1)",
    "both dates in same bin (drop)",
    "spans bin 3 only (drop)",
    "spans bin 2→3",
    "spans bin 1→2",
    "edge case (final date)",
    "duplicate row (key)",
    "duplicate row (key)",
    "spans bin 2→3",
    "spans bin 1→2"
  )
)


x1 <- x_test
x2 <- tt



f250727b(x1,x2)


#f250727b <- #PAS price array sparse 
#  function( #written by cgpt on 250728, has test suite etc
      # x1 = data.table(#this 'non-spanning' row silently is dropped as required
      #   t0 = as.Date("1996-05-11"),
      #   t1 = as.Date("1996-05-12"),
      #   p0 = 1000, p1 = 2000, id = "x"
      # )#,
      # x2 = as.Date(c( #define bins with right-closed/left-open i.e. equality on the right
      #   "1994-12-31", "1996-05-12",
      #   "1996-12-07", "1997-05-30", "1997-10-21"
      # ))
      # 
      #) {
    #browser()
    # interval indices (bins) for t0 and t1
    x5 <- findInterval(x1$t0, x2, rightmost.closed = TRUE, left.open = TRUE)
    x6 <- findInterval(x1$t1, x2, rightmost.closed = TRUE, left.open = TRUE)

    # Drop any rows where both dates fall in the same bin
    keep <- which(x5 != x6)
    if (length(keep) == 0L) {
      return(data.table())
    } # return empty if nothing spans
    x11 <- x1[keep]
    x12 <- x5[keep]
    x13 <- x6[keep]

    # Interval IDs for the grid
    x4 <- seq_len(length(x2) - 1)

    # Stack into long format: (row_id, interval, value)
    x7 <- rbind(
      data.table(row_id = seq_len(nrow(x11)), interval = x12, value = x11$p0),
      data.table(row_id = seq_len(nrow(x11)), interval = x13, value = x11$p1)
    )[interval > 0 & interval <= (length(x2) - 1)]

    # Full row_id × interval grid
    x8 <- CJ(row_id = seq_len(nrow(x11)), interval = x4)

    # Place values, fill missing with 0
    x9 <- x8[x7, on = .(row_id, interval)]
    x9[is.na(value), value := 0]

    # Pivot to wide
    x10 <- dcast(x9, row_id ~ interval, value.var = "value", fill = 0)
    setnames(x10, old = as.character(x4), new = as.character(x2[-1]))

    # Join back to x1 (keep id, t0, t1, p0, p1)
    x10[x1[, row_id := .I], on = .(row_id)][, -"row_id"]
  }



f250727b(x1=x_test[1,],x2=tt)
f250727b()




#f250727b <- #PAS price array sparse 
#  function( #written by cgpt on 250728, has test suite etc
      # x1 = data.table(#this 'non-spanning' row silently is dropped as required
      #   t0 = as.Date("1996-05-11"),
      #   t1 = as.Date("1996-05-12"),
      #   p0 = 1000, p1 = 2000, id = "x"
      # )#,
      # x2 = as.Date(c( #define bins with right-closed/left-open i.e. equality on the right
      #   "1994-12-31", "1996-05-12",
      #   "1996-12-07", "1997-05-30", "1997-10-21"
      # ))
      # 
      #) {
    #browser()
    # interval indices (bins) for t0 and t1
    x5 <- findInterval(x1$t0, x2, rightmost.closed = TRUE, left.open = TRUE)
    x6 <- findInterval(x1$t1, x2, rightmost.closed = TRUE, left.open = TRUE)

    # Drop any rows where both dates fall in the same bin
    keep <- which(x5 != x6)
    if (length(keep) == 0L) {
      return(data.table())
    } # return empty if nothing spans
    x11 <- x1[keep]
    x12 <- x5[keep]
    x13 <- x6[keep]

    # Interval IDs for the grid
    x4 <- seq_len(length(x2) - 1)

    # Stack into long format: (row_id, interval, value)
    x7 <- rbind(
      data.table(row_id = seq_len(nrow(x11)), interval = x12, value = x11$p0),
      data.table(row_id = seq_len(nrow(x11)), interval = x13, value = x11$p1)
    )[interval > 0 & interval <= (length(x2) - 1)]

    # Full row_id × interval grid
    x8 <- CJ(row_id = seq_len(nrow(x11)), interval = x4)

    # Place values, fill missing with 0
    x9 <- x8[x7, on = .(row_id, interval)]
    x9[is.na(value), value := 0]

    # Pivot to wide
    x10 <- dcast(x9, row_id ~ interval, value.var = "value", fill = 0)
    setnames(x10, old = as.character(x4), new = as.character(x2[-1]))

    # Join back to x1 (keep id, t0, t1, p0, p1)
    x10[x1[, row_id := .I], on = .(row_id)][, -"row_id"]
  }
