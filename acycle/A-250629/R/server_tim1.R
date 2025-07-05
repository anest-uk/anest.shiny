server_tim1 <- function(input, output, session, common) {
  R111x <- eventReactive( # 111 leaflet---- 
    list(
      common$rc6tR(), # C
      common$rc6cR() # C
    ),
    {
      print("enter server_tim1")
      x <- D111x(
        rc6tx = common$rc6tR(), # target
        rc6cx = common$rc6cR() #    #custom
      )
      x
    }
  )

  R112x <- eventReactive( # 112 x(t) ----
    list(
      common$rc6tR(), # C
      common$rescxR(), # U
      common$tslideR() # C
    ),
    {
      x <-
        D112x(
          rc6tx = common$rc6tR(),
          rescxx = common$rescxR(),
          tslidex = common$tslideR()
        )
      x
    }
  )


  R121x <- eventReactive( # 121 winding ----
    list(
      common$rc6tR(),
      common$rescxR()
    ),
    {
      x <- D121x(
        rc6tx = common$rc6tR(),
        rescx = common$rescxR()
      )
      x
    }
  )

  R122x <- eventReactive( # 122 trade ----
    list(
      common$rc6tR(),
      rescxx = common$rescxR()
    ),
    {
      x <- D122x(
        rc6tx = common$rc6tR(),
        rescxx = common$rescxR()
      )
      x
    }
  )

  R131x <- eventReactive( # 131 summary ----
    list(
      common$rescxR(),
      common$rc6tR(),
      common$tslideR()
    ),
    {
      x <- D131x(
        rescx = common$rescxR(),
        rc6tx = common$rc6tR(),
        tslidex = common$tslideR()
      )
      x
    }
  )

  R132x <- eventReactive( # 132 trade ----
    list(
      common$rescxR(),
      common$rc6tR(),
      common$tslideR()
    ),
    {
      x <- D132x(
        rescxx = common$rescxR(),
        rc6tx = common$rc6tR(),
        tslidex = common$tslideR()
      )
      x
    }
  )



  output$O111x <- renderLeaflet(R111x()) # render----
  output$O112x <- renderPlot(R112x()) # x(t)
  output$O121xa <- gt::render_gt(R121x()[[1]]) # wind gt - custom
  output$O121xb <- gt::render_gt(R121x()[[2]]) # wind gt - local
  output$O122x <- gt::render_gt(R122x()) # char gt

  output$O131x <- gt::render_gt(R131x()) # summ gt

  output$O132xa <- gt::render_gt(R132x()[["local"]][[1]]) # render----
  output$O132xb <- gt::render_gt(R132x()[["local"]][[2]]) # render----
  output$O132xc <- gt::render_gt(R132x()[["custom"]][[1]]) # render----
  output$O132xd <- gt::render_gt(R132x()[["custom"]][[2]]) # render----

  print("Leaving server_R1()...")
}
