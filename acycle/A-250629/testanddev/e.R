x_test <- data.table(
  id = c("a", "a", "b", "b", "b", "c", "c", "d", "d", "e", 
         "f", "g", "h", "i", "j", "k", "l", "m", "n"),
  t0 = as.Date(c(
    "1995-01-10",  # 1: spans bin 1→2
    "1995-12-31",  # 2: starts on boundary (bin 1→2)
    "1996-01-01",  # 3: within bin 2 (drop case)
    "1996-05-01",  # 4: spans bin 2→3
    "1996-12-31",  # 5: t1 on boundary (bin 2→3)
    "1994-12-31",  # 6: t0 on first boundary (bin 0→1)
    "1995-06-01",  # 7: both dates in same bin (drop)
    "1997-01-01",  # 8: spans bin 3 only (drop)
    "1996-01-10",  # 9: spans bin 2→3
    "1995-01-01",  #10: spans bin 1→2
    "1997-12-31",  #11: edge case (final date)
    "1995-07-01",  #12: duplicate row (key)
    "1995-07-01",  #13: duplicate row (key)
    "1996-12-30",  #14: spans bin 2→3
    "1995-11-15",  #15: spans bin 1→2
    "1993-12-30",  #16: before first bin (error)
    "2025-01-01",  #17: after last bin (error)
    "1996-06-01",  #18: within bin 2 (drop case)
    "1997-10-01"   #19: t1 on final bin boundary
  )),
  t1 = as.Date(c(
    "1996-01-10",  # 1
    "1996-01-15",  # 2
    "1996-02-01",  # 3
    "1997-01-10",  # 4
    "1997-01-05",  # 5
    "1995-01-10",  # 6
    "1995-07-01",  # 7
    "1997-02-01",  # 8
    "1997-01-01",  # 9
    "1996-02-02",  #10
    "1997-12-31",  #11
    "1995-08-01",  #12
    "1995-08-01",  #13
    "1997-01-05",  #14
    "1996-01-05",  #15
    "1994-01-15",  #16
    "2025-02-01",  #17
    "1996-06-10",  #18
    "1997-10-21"   #19
  )),
  p0 = 1:19,
  p1 = 101:119,
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
    "spans bin 1→2",
    "before first bin (error)",
    "after last bin (error)",
    "within bin 2 (drop case)",
    "t1 on final bin boundary"
  ),
  action = c(
    "keep", "keep", "drop", "keep", "keep",
    "keep", "drop", "drop", "keep", "keep",
    "drop", "drop", "drop", "keep", "keep",
    "drop", "drop", "drop", "keep"
  )
)[-1,]
