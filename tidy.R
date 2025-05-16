estdtxR <- eventReactive( #---------112 x(t)----
  list(common$estdtcuR(), common$estdtaR(), common$geocuR()),
  {
    festdtxX <- function( #-------------112 x(t)----
                         estdtcuX = estdtcuG, estdtaX = estdtaG, geocuX = geocuG) {
      x <-
        rbind(
          # estdtcuX[,.(nx,date,xdotd,days,xdot,x,lab,ii,qtile=0,rc3=geocuX[,substr(rc9,1,3)])],
          estdtcuX[, .(nx, date, xdotd, days, xdot, x, lab, ii, qtile = 0, rc3 = lab)],
          estdtaX[, .(nx, date, xdotd, days, xdot, x, lab, ii, qtile, rc3)]
        )[, qq := as.factor(qtile)]
      x
    }



    print("enter estdtxR")
    x <-
      festdtxX(estdtcuX = common$estdtcuR(), estdtaX = common$estdtaR(), geocuX = common$geocuR())
    estdtxG <<- copy(x)
    print("exit estdtxR")
    x
  }
)
