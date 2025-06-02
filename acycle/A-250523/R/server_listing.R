server_listing <- function(input, output, session, common) {
  x211D <- # ---listing-1-local-estdtl,qeoq,dfn----
    eventReactive(
      list(
        common$estdtlR(),
        common$geoqR(),
        common$dfnxxR()
      ),
      {
        if (verbose) print("enter x211D")
        x <- f211D(
          estdtlx = common$estdtlR(),
          geoqx = common$geoqR(),
          dfnxxx = common$dfnxxR()
        )
        x211G <<- copy(x)
        x
      }
    )
  x211ccD <- # listing-2-custom-estdtcu,qeocu,dfn----
    eventReactive(
      list(
        common$estdtccR(),
        common$geoccR(),
        common$dfnxxR()
      ),
      {
        if (verbose) print("enter x211D")
        geox <- copy(common$geoccR())[, let(rc6, rc9)] # used for aggregation and label
        x <- f211D(
          estdtlx = common$estdtccR(),
          geoqx = common$geoccR()[, .(nx, lab, rc6 = rc9)], # non-standard geo
          dfnxxx = common$dfnxxR(),
          typex = "C"
        )
        x211ccG <<- copy(x)
        x
      }
    )
  output$x211 <- gt::render_gt(x211D()) #-----.#----
  output$x211cu <- gt::render_gt(x211ccD()) #--.#----
}
