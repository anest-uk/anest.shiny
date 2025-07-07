server_lis1 <- function(input, output, session, common) {
  R211x <- # ---listing-1-local-estdtl,qeoq,dfn----
    eventReactive(
      list(
        common$estdtlR(),
        common$geoqR(),
        common$dfnyR()
      ),
      {
        if (verbose) print("enter R211x")
        x <- D211x(
          estdtlx = common$estdtlR(),
          geoqx = common$geoqR(),
          dfnyx = common$dfnyR()
        )
        G211x <<- copy(x)
        x
      }
    )
  R212x <- # listing-2-custom-estdtcu,qeocu,dfn----
    eventReactive(
      list(
        common$estdtcR(),
        common$geocR(),
        common$dfnyR()
      ),
      {
        if (verbose) print("enter R211x")
        geox <- copy(common$geocR())[, let(rc6, rc9)] # used for aggregation and label
        x <- D211x(
          estdtlx = common$estdtcR(),
          geoqx = common$geocR()[, .(nx, lab, rc6 = rc9)], # non-standard geo
          dfnyx = common$dfnyR(),
          typex = "C"
        )
        G212x <<- copy(x)
        x
      }
    )
  output$O211x <- gt::render_gt(R211x()) #-----.#----
  output$O212x <- gt::render_gt(R212x()) #--.#----
}
