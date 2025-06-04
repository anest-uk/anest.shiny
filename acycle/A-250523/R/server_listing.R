server_listing <- function(input, output, session, common) {
  R211 <- # ---listing-1-local-estdtl,qeoq,dfn----
    eventReactive(
      list(
        common$estdtlR(),
        common$geoqR(),
        common$dfnyR()
      ),
      {
        if (verbose) print("enter R211")
        x <- D211(
          estdtlx = common$estdtlR(),
          geoqx = common$geoqR(),
          dfnz = common$dfnyR()
        )
        G211 <<- copy(x)
        x
      }
    )
  R212 <- # listing-2-custom-estdtcu,qeocu,dfn----
    eventReactive(
      list(
        common$estdtccR(),
        common$geoccR(),
        common$dfnyR()
      ),
      {
        if (verbose) print("enter R211")
        geox <- copy(common$geoccR())[, let(rc6, rc9)] # used for aggregation and label
        x <- D211(
          estdtlx = common$estdtccR(),
          geoqx = common$geoccR()[, .(nx, lab, rc6 = rc9)], # non-standard geo
          dfnz = common$dfnyR(),
          typex = "C"
        )
        G212 <<- copy(x)
        x
      }
    )
  output$O211 <- gt::render_gt(R211()) #-----.#----
  output$O212 <- gt::render_gt(R212()) #--.#----
}
