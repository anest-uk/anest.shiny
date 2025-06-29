# -------------------------------------------------------------------
# @datatype sse
# @columns
#   rc9    : character
#   ssei   : numeric
#   toti   : numeric
#   ssek   : numeric
#   sser   : numeric
#   ssetr  : numeric
#   n      : integer
#   nx     : integer
# @key {nx, rc9}
# -------------------------------------------------------------------

validate_sse <- function(dt) {
  stopifnot(data.table::is.data.table(dt))
  stopifnot(all(c("rc9", "ssei", "toti", "ssek", "sser", "ssetr", "n", "nx") %in% names(dt)))

  stopifnot(
    is.character(dt$rc9),
    is.numeric(dt$ssei),
    is.numeric(dt$toti),
    is.numeric(dt$ssek),
    is.numeric(dt$sser),
    is.numeric(dt$ssetr),
    is.integer(dt$n),
    is.integer(dt$nx),
    !anyDuplicated(dt[, .(nx, rc9)])
  )
}
