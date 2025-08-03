# Define bin edges
tt <- as.Date(c("1994-12-31", "1995-12-31", "1996-12-31", "1997-12-31"))

# Define x_test excluding error cases
x_test <- data.table(
  id = c("a", "a", "b", "b", "b", "c", "c", "d", "d", "e", 
         "f", "g", "h", "i", "j", "m", "n"),
  t0 = as.Date(c(
    "1995-01-10", "1995-12-31", "1996-01-01", "1996-05-01", "1996-12-31",
    "1994-12-31", "1995-06-01", "1997-01-01", "1996-01-10", "1995-01-01",
    "1997-12-31", "1995-07-01", "1995-07-01", "1996-12-30", "1995-11-15",
    "1996-06-01", "1997-10-01"
  )),
  t1 = as.Date(c(
    "1996-01-10", "1996-01-15", "1996-02-01", "1997-01-10", "1997-01-05",
    "1995-01-10", "1995-07-01", "1997-02-01", "1997-01-01", "1996-02-02",
    "1997-12-31", "1995-08-01", "1995-08-01", "1997-01-05", "1996-01-05",
    "1996-06-10", "1997-10-21"
  )),
  p0 = c(1:15, 18, 19),
  p1 = c(101:115, 118, 119),
  comment = c(
    "spans bin 1→2", "starts on boundary (bin 1→2)", "within bin 2 (drop case)",
    "spans bin 2→3", "t1 on boundary (bin 2→3)", "t0 on first boundary (bin 0→1)",
    "both dates in same bin (drop)", "spans bin 3 only (drop)", "spans bin 2→3",
    "spans bin 1→2", "edge case (final date)", "duplicate row (key)",
    "duplicate row (key)", "spans bin 2→3", "spans bin 1→2",
    "within bin 2 (drop case)", "t1 on final bin boundary"
  ),
  action = c(
    "keep", "keep", "drop", "keep", "keep", "keep", "drop", "drop", "keep",
    "keep", "drop", "drop", "drop", "keep", "keep", "drop", "keep"
  )
)

# Compute bin index for t0 and t1
x_test[, bin_t0 := findInterval(t0, tt, rightmost.closed = TRUE, left.open = TRUE)]
x_test[, bin_t1 := findInterval(t1, tt, rightmost.closed = TRUE, left.open = TRUE)]

# Display diagnostic table
print(x_test[, .(id, t0, t1, bin_t0, bin_t1, action, comment)])
