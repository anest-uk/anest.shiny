server_timeseries <- function(input, output, session, common) {
  x111D <- eventReactive( # -------111 leaflet----
    list(
      common$rc6tR(),
      common$rc6ccR(),
      common$geoaR()#,
      #common$pxosrdo2ddR(),
      #common$z110R()
    ),
    {
      if (verbose) {
        print("enter x111D xxx")
      }
      x <- f111D(
        rc6tX = common$rc6tR(),
        rc6ccX = common$rc6ccR(),
        geoaX = common$geoaR(),
        pxosrdo2ddX = pxosrdo2dd, #common$pxosrdo2ddR(),
        z110X = z110,#common$z110R(),
        colX = colx # punk green blue
      )
      x111G <<- copy(x)
      if (verbose) {
        print("exit x111D")
      }

      x
    }
  )

  x112D <- eventReactive( # ----------112 x(t)----
    list(common$tslideR(), common$estdtxR(), common$ylimR()),
    {
      if (verbose) print("enter x112D")
      # browser()
      x <-
        f112D(
          tslideX = common$tslideR(),
          estdtxX = common$estdtxR(),
          ylimX = common$ylimR(),
          geoccX = common$geoccR()
        )
      x112G <<- copy(x)
      x
    }
  )


  x121D <- eventReactive( #--------121 winding----
    list(common$estdtlR(), common$estdtccR(), common$dfnxxR()),
    {
      if (verbose) print("enter x121D")
      x2 <- f121D(
        estdt = common$estdtlR(),
        dfnxX = common$dfnxR()
      )
      x4 <- f121D(
        estdt = common$estdtccR(),
        dfnxX = common$dfnxR(),
        typeX = "C"
      )
      x <- list(x2, x4)
      x121G <<- copy(x)
      x
    }
  )


  x122D <- eventReactive( #------------------.#----
    list(common$rc6tR(), common$rssaR(), common$rssccR()), #, common$z110R()
    {
      if (verbose) print("enter x122D")
      x <- f122D(
        rc6tX = common$rc6tR(),
        rssaX = common$rssaR(),
        rssccX = common$rssccR(),
        z110X = z110#common$z110R()
      )
      x122G <<- copy(x)
      if (verbose) print("exit x122D")
      x
    }
  )


  x131D <- eventReactive( #------------------.#----
    list(common$tslideR(), common$estdtxR()),
    {
      if (verbose) print("enter x131D")
      x <- f131D(
        estdtxX = common$estdtxR(),
        tslideX = common$tslideR()
      )
      x131G <<- copy(x)
      x
    }
  )


  x132D <- eventReactive( # 132 trade summary(2)----
    list(common$tslideR(), common$geoqR(), common$estdtlR()),
    {
      if (verbose) print("enter x132D")
      x <- f132D(
        tslideX = common$tslideR(),
        geoqX = common$geoqR(),
        geoccX = common$geoccR(),
        estdtlX = common$estdtlR()
      )
      x132G <<- copy(x)
      if (verbose) print("exit x132D")
      x
    }
  )

  output$x111 <- renderLeaflet(x111D()) # render----
  output$x112 <- renderPlot(x112D()) #---render----

  output$x121a <- gt::render_gt(x121D()[[1]]) # render----
  output$x121b <- gt::render_gt(x121D()[[2]]) # render----
  output$x122 <- gt::render_gt(x122D()) # render----
  output$x131 <- gt::render_gt(x131D()) # render----
  output$x132a <- gt::render_gt(x132D()[["local"]][[1]]) # render----
  output$x132b <- gt::render_gt(x132D()[["local"]][[2]]) # render----
  output$x132c <- gt::render_gt(x132D()[["custom"]][[1]]) # render----
  output$x132d <- gt::render_gt(x132D()[["custom"]][[2]]) # render----
  print("Leaving server_timeseries()...")
}
