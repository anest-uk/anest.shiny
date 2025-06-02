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
        print("enter x111D z")
      }
      x <- f111D(
        rc6tx = common$rc6tR(),
        rc6ccx = common$rc6ccR(),
        geoax = common$geoaR(),
        pxosrdo2ddx = pxosrdo2dd, #common$pxosrdo2ddR(),
        z110x = z110,#common$z110R(),
        colx = colx # punk green blue
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
          tslidex = common$tslideR(),
          estdty = common$estdtxR(),
          ylimx = common$ylimR(),
          geoccx = common$geoccR()
        )
      x112G <<- copy(x)
      x
    }
  )


  x121D <- eventReactive( #--------121 winding----
    list(common$estdtlR(), common$estdtccR(), common$dfnyR()),
    {
      if (verbose) print("enter x121D")
      x2 <- f121D(
        estdt = common$estdtlR(),
        dfny = common$dfnxR()
      )
      x4 <- f121D(
        estdt = common$estdtccR(),
        dfny = common$dfnxR(),
        typex = "C"
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
        rc6tx = common$rc6tR(),
        rssax = common$rssaR(),
        rssccx = common$rssccR(),
        z110x = z110#common$z110R()
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
        estdty = common$estdtxR(),
        tslidex = common$tslideR()
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
        tslidex = common$tslideR(),
        geoqx = common$geoqR(),
        geoccx = common$geoccR(),
        estdtlx = common$estdtlR()
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
