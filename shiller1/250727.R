f250727aFun() #cowrite SAL - all sales



#---------------------------------------------------junk
X <- fread('
t0,t1,p0,p1
1997-01-30,2000-09-01,157136,263773
1997-06-06,2000-05-26,119356,184720
2000-05-26,2005-03-11,184720,284395
2005-03-11,2020-04-03,284395,517146
2020-04-03,2025-02-07,517146,631571
1997-06-20,1999-12-22,119356,163156
1999-12-22,2010-07-28,163156,352509
2010-07-28,2017-08-11,352509,549781
1997-06-20,2000-06-16,141239,241208
1997-07-31,2003-02-17,146957,328417
')
X[, `:=`(t0 = as.IDate(t0), t1 = as.IDate(t1),
         p0 = as.numeric(p0), p1 = as.numeric(p1))]

# Boundaries T (ordered)
T <- as.IDate(c(
  "1994-12-31", "1996-05-12", "1996-12-07", "1997-05-30", "1997-10-21", "1998-07-01",
  "1999-04-16", "1999-08-23", "1999-12-19", "2000-02-22", "2001-09-02", "2002-05-01",
  "2002-08-31", "2002-12-17", "2003-04-05", "2003-06-14", "2003-08-23", "2003-11-07",
  "2004-01-27", "2004-04-21", "2004-06-10", "2004-11-06", "2005-05-26", "2006-07-11",
  "2007-01-30", "2007-05-17", "2007-12-31", "2009-02-28", "2009-06-09", "2010-02-16",
  "2011-03-28", "2011-10-11", "2012-08-04", "2013-10-08", "2014-05-26", "2014-08-27",
  "2015-04-08", "2015-10-18", "2016-01-27", "2016-05-29", "2016-10-27", "2017-07-16",
  "2018-05-07", "2020-01-30", "2021-07-08", "2022-08-04", "2025-02-28"
))

# There are length(T)-1 intervals, define their names as strings
interval_names <- paste0("int", seq_len(length(T)-1))

# Find which interval each t0 and t1 falls into
idx_t0 <- findInterval(X$t0, T, rightmost.closed = TRUE)
idx_t1 <- findInterval(X$t1, T, rightmost.closed = TRUE)

# Create a long table of (row_id, interval, value)
DT_long <- rbind(
  data.table(row_id = seq_len(nrow(X)), interval = idx_t0, value = X$p0),
  data.table(row_id = seq_len(nrow(X)), interval = idx_t1, value = X$p1)
)

# Drop any rows where the date is before first or after last (interval=0 or > length-1)
DT_long <- DT_long[interval > 0 & interval <= (length(T)-1)]

# Wide format: N × (length(T)-1), zeros by default
Y <- dcast(DT_long, row_id ~ interval, value.var = "value", fill = 0)

# Clean up names
setnames(Y, old = setdiff(names(Y),'row_id'), new = interval_names)
setcolorder(Y, c("row_id", interval_names))

# Merge back with X if needed
result <- merge(X[, row_id := .I], Y, by = "row_id")
result[, row_id := NULL]  # drop row_id if not needed