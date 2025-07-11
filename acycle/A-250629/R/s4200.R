server_lis1 <- function(input, output, session, common) {
  R211a <- # ---timebin table----
    eventReactive(
      list(
          rescx = common$rescxR(),
          rc6tx = common$rc6tR(),
          tslidex = common$tslideR()
      ),
      {
        if (verbose) print("enter R211a<<<<<<<<<<<<<<<")
        x <- 
          D211b(
            rescx = common$rescxR(),
            rc6tx = common$rc6tR(),
            tslidex = common$tslideR()
            )
        G211a <<- copy(x)
        x
      }
    )

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
        common$rescR(),
        common$estdtcR(),
        common$geocR(),
        common$dfnyR()
      ),
      {
        if (verbose) print("enter R211x<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<")
        print(common$geocR())
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
  R213x <- # listing-2-custom-estdtcu,qeocu,dfn----
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
  output$O211a <- gt::render_gt(R211a()[[1]]) #-----.#----
  output$O211b <- gt::render_gt(R211a()[[2]]) #-----.#----
  output$O211c <- gt::render_gt(R211a()[[3]]) #-----.#----
  
  output$O211x <- gt::render_gt(R211x()) #-----.#----
  output$O212x <- gt::render_gt(R212x()) #--.#----
}
