server_timeseries <- function(input, output, session, common) {
  R111 <- eventReactive( # -------111 leaflet----
    list(
      common$rc6tR(),
      common$rc6ccR(),
      common$geoaR()
    ),
    {
      if (verbose) {
        print("enter R111 z")
      }
      x <- D111(
        rc6tx = common$rc6tR(),
        rc6ccx = common$rc6ccR(),
        geoax = common$geoaR(),
        pxosrdo2ddx = pxosrdo2dd, 
        z110x = z110,
        colx = colx # punk green blue
      )
      G111 <<- copy(x)
      if (verbose) {
        print("exit R111")
      }

      x
    }
  )

  R112 <- eventReactive( # ----------112 x(t)----
    list(common$tslideR(), common$estdtxR(), common$ylimR()),
    {
      if (verbose) print("enter R112")
      # browser()
      x <-
        D112(
          tslidex = common$tslideR(),
          estdty = common$estdtxR(),
          ylimx = common$ylimR(),
          geoccx = common$geoccR()
        )
      G112 <<- copy(x)
      x
    }
  )


  R121 <- eventReactive( #--------121 winding----
    list(common$estdtlR(), common$estdtccR(), common$dfnyR()),
    {
      if (verbose) print("enter R121")
      x2 <- D121(
        estdt = common$estdtlR(),
        dfny = common$dfnxR()
      )
      x4 <- D121(
        estdt = common$estdtccR(),
        dfny = common$dfnxR(),
        typex = "C"
      )
      x <- list(x2, x4)
      G121 <<- copy(x)
      x
    }
  )


  R122 <- eventReactive( #------------------.#----
    list(common$rc6tR(), common$rssaR(), common$rssccR()), #, common$z110R()
    {
      if (verbose) print("enter R122")
      x <- D122(
        rc6tx = common$rc6tR(),
        rssax = common$rssaR(),
        rssccx = common$rssccR(),
        z110x = z110#common$z110R()
      )
      G122 <<- copy(x)
      if (verbose) print("exit R122")
      x
    }
  )


  R131 <- eventReactive( #------------------.#----
    list(common$tslideR(), common$estdtxR()),
    {
      if (verbose) print("enter R131")
      x <- D131(
        estdty = common$estdtxR(),
        tslidex = common$tslideR()
      )
      G131 <<- copy(x)
      x
    }
  )


  R132 <- eventReactive( # 132 trade summary(2)----
    list(common$tslideR(), common$geoqR(), common$estdtlR()),
    {
      if (verbose) print("enter R132")
      x <- D132(
        tslidex = common$tslideR(),
        geoqx = common$geoqR(),
        geoccx = common$geoccR(),
        estdtlx = common$estdtlR()
      )
      G132 <<- copy(x)
      if (verbose) print("exit R132")
      x
    }
  )

  output$O111 <- renderLeaflet(R111()) # render----
  output$O112 <- renderPlot(R112()) #---render----

  output$O121a <- gt::render_gt(R121()[[1]]) # render----
  output$O121b <- gt::render_gt(R121()[[2]]) # render----
  output$O122 <- gt::render_gt(R122()) # render----
  output$O131 <- gt::render_gt(R131()) # render----
  output$O132a <- gt::render_gt(R132()[["local"]][[1]]) # render----
  output$O132b <- gt::render_gt(R132()[["local"]][[2]]) # render----
  output$O132c <- gt::render_gt(R132()[["custom"]][[1]]) # render----
  output$O132d <- gt::render_gt(R132()[["custom"]][[2]]) # render----
  print("Leaving server_timeseries()...")
}
