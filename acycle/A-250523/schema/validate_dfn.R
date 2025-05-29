# -------------------------------------------------------------------
# @datatype dfn
# @columns
#   date : Date
#   ii   : integer
# @key {date}
# -------------------------------------------------------------------

validate_dfn <- function(dt) {
  stopifnot(data.table::is.data.table(dt))
  stopifnot(all(c("date", "ii") %in% names(dt)))

  stopifnot(
    inherits(dt$date, "Date"),
    is.integer(dt$ii),
    !anyDuplicated(dt[, .(date)])
  )
}
