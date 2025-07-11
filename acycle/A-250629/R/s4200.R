s4200 <- function(input, output, session, common) {
  R4211a <- # ---timebin table----
    eventReactive(
      list(
          rescx = common$rescxR(),
          rc6tx = common$rc6tR(),
          tslidex = common$tslideR()
      ),
      {
        if (verbose) print("enter R211a")
        x <- 
          D4211b(
            rescx = common$rescxR(),
            rc6tx = common$rc6tR(),
            tslidex = common$tslideR()
            )
        G4211a <<- copy(x)
        x
      }
    )

  R4211x <- # ---listing-1-local-estdtl,qeoq,dfn----
    eventReactive(
      list(
        common$estdtlR(),
        common$geoqR(),
        common$dfnyR()
      ),
      {
        if (verbose) print("enter R211x")
        x <- D4211x(
          estdtlx = common$estdtlR(),
          geoqx = common$geoqR(),
          dfnyx = common$dfnyR()
        )
        G4211x <<- copy(x)
        x
      }
    )
  R4212x <- # listing-2-custom-estdtcu,qeocu,dfn----
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
        x <- D4211x(
          estdtlx = common$estdtcR(),
          geoqx = common$geocR()[, .(nx, lab, rc6 = rc9)], # non-standard geo
          dfnyx = common$dfnyR(),
          typex = "C"
        )
        G4212x <<- copy(x)
        x
      }
    )
  R4213x <- # listing-2-custom-estdtcu,qeocu,dfn----
    eventReactive(
      list(
        common$estdtcR(),
        common$geocR(),
        common$dfnyR()
      ),
      {
        if (verbose) print("enter R211x")
        geox <- copy(common$geocR())[, let(rc6, rc9)] # used for aggregation and label
        x <- D4211x(
          estdtlx = common$estdtcR(),
          geoqx = common$geocR()[, .(nx, lab, rc6 = rc9)], # non-standard geo
          dfnyx = common$dfnyR(),
          typex = "C"
        )
        G4213x <<- copy(x)
        x
      }
    )
  output$O4211a <- gt::render_gt(R4211a()[[1]]) #-----.#----
  output$O4211b <- gt::render_gt(R4211a()[[2]]) #-----.#----
  output$O4211c <- gt::render_gt(R4211a()[[3]]) #-----.#----
  
  output$O4211x <- gt::render_gt(R4211x()) #-----.#----
  output$O4212x <- gt::render_gt(R4212x()) #--.#----
}
