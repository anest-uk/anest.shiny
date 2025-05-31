# -------------------------------------------------------------------
# @datatype geo
# @columns
#   nx   : integer
#   rc9  : character
#   lab  : character
# @key {nx, rc9}
# @foreign_keys:
#   - {nx} → forall
#   - {nx, date} ← geo × dfn → estdt
#   - {nx, rc9} ← geo × dfn → sse
# @rows: one per (nx, rc9)
# @constraints:
#   - rc9 values ∈ GLOBAL_RC9
#   - rc9 prefix ∈ GLOBAL_RC9 with same prefix as first row
# -------------------------------------------------------------------

validate_geo <- function(dt, dfn = NULL, estdt = NULL, sse = NULL, forall = NULL, GLOBAL_RC9 = NULL) {
  stopifnot(
    data.table::is.data.table(dt),
    setequal(names(dt), c("nx", "rc9", "lab")),
    is.integer(dt$nx),
    is.character(dt$rc9),
    is.character(dt$lab),
    !anyDuplicated(dt[, .(nx, rc9)])
  )

  if (!is.null(forall)) {
    stopifnot(all(dt[, unique(nx)] %in% forall$nx))
  }

  if (!is.null(GLOBAL_RC9)) {
    stopifnot(setequal(dt[, unique(rc9)], GLOBAL_RC9))
    prefix <- substr(dt[1, rc9], 1, 3)
    stopifnot(setequal(
      dt[, unique(rc9)],
      GLOBAL_RC9[substr(GLOBAL_RC9, 1, 3) == prefix]
    ))
  }

  if (!is.null(dfn) && !is.null(estdt)) {
    combined <- merge(dt, dfn, allow.cartesian = TRUE)[, .(nx, date)]
    stopifnot(nrow(merge(combined, estdt, by = c("nx", "date"), nomatch = 0L)) == nrow(estdt))
  }

  if (!is.null(dfn) && !is.null(sse)) {
    combined <- merge(dt, dfn, allow.cartesian = TRUE)
    stopifnot(nrow(merge(combined, sse, by = c("nx", "rc9"), nomatch = 0L)) == nrow(sse))
  }
}
