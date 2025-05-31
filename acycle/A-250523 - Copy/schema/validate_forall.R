# -------------------------------------------------------------------
# @datatype forall
# @columns
#   insam  : numeric
#   outsam : numeric
#   raw    : numeric
#   tot    : numeric
#   rsq    : numeric
#   nx     : integer
# @key {nx}
# -------------------------------------------------------------------

validate_forall <- function(dt) {
  stopifnot(data.table::is.data.table(dt))
  stopifnot(all(c("insam", "outsam", "raw", "tot", "rsq", "nx") %in% names(dt)))

  stopifnot(
    is.numeric(dt$insam),
    is.numeric(dt$outsam),
    is.numeric(dt$raw),
    is.numeric(dt$tot),
    is.numeric(dt$rsq),
    is.integer(dt$nx),
    !anyDuplicated(dt[, .(nx)])
  )
}
