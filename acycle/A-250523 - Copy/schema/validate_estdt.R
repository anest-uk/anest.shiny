# -------------------------------------------------------------------
# @datatype estdt
# @columns
#   ii     : integer
#   nx     : integer
#   date   : Date
#   xdotd  : numeric
#   xdot   : numeric
#   x      : numeric
#   days   : integer
# @key {nx, date}
# @constraints:
#   - no duplicated (nx, date)
#   - days ≥ 0
# -------------------------------------------------------------------

validate_estdt <- function(dt) {
  stopifnot(data.table::is.data.table(dt))
  stopifnot(all(c("ii", "nx", "date", "xdotd", "xdot", "x", "days") %in% names(dt)))

  stopifnot(
    is.integer(dt$ii),
    is.integer(dt$nx),
    inherits(dt$date, "Date"),
    is.numeric(dt$xdotd),
    is.numeric(dt$xdot),
    is.numeric(dt$x),
    is.integer(dt$days),
    !anyDuplicated(dt[, .(nx, date)]),
    all(dt$days >= 0)
  )
}
