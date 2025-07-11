#-------------------------------------------------4100 timeseries
s4100 <- 
  function(input, output, session, common) {
  R4111x <- eventReactive( # 111 leaflet---- 
    list(
      common$rc6tR(), # C
      common$rc6cR() # C
    ),
    {
      print("enter R4111x")
      x <- D4111x(
        rc6tx = common$rc6tR(), # target
        rc6cx = common$rc6cR() #    #custom
      )
      G4111x <<- copy(x)
      x
    }
  )

  R4112x <- eventReactive( # 112 x(t) ----
    list(
      common$rc6tR(), # C
      common$rescxR(), # U
      common$tslideR() # C
    ),
    {
      x <-
        D4112x(
          rc6tx = common$rc6tR(),
          rescxx = common$rescxR(),
          tslidex = common$tslideR()
        )
      G4112x <<- copy(x)
      x
    }
  )

  R4121x <- eventReactive( # 121 winding ----
    list(
      common$rc6tR(),
      common$rescxR()
    ),
    {
      x <- D4121x(
        rc6tx = common$rc6tR(),
        rescx = common$rescxR()
      )
      G4121x <<- copy(x)
      x
    }
  )

  R4122x <- eventReactive( # 122 trade ----
    list(
      common$rc6tR(),
      rescxx = common$rescxR()
    ),
    {
      x <- D4122x(
        rc6tx = common$rc6tR(),
        rescxx = common$rescxR()
      )
      G4122x <<- copy(x)
      x
    }
  )

  R4131x <- eventReactive( # 131 summary ----
    list(
      common$rescxR(),
      common$rc6tR(),
      common$tslideR()
    ),
    {
      x <- D4131x(
        rescx = common$rescxR(),
        rc6tx = common$rc6tR(),
        tslidex = common$tslideR()
      )
      G4131x <<- copy(x)
      x
    }
  )

  R4132x <- eventReactive( # 132 trade ----
    list(
      common$rescxR(),
      common$rc6tR(),
      common$tslideR()
    ),
    {
      x <- D4132x(
        rescxx = common$rescxR(),
        rc6tx = common$rc6tR(),
        tslidex = common$tslideR()
      )
      G4132x <<- copy(x)
      x
    }
  )

  output$O4111x <- renderLeaflet(R4111x()) # render----
  output$O4112x <- renderPlot(R4112x()) # x(t)
  output$O4121xa <- gt::render_gt(R4121x()[[1]]) # wind gt - custom
  output$O4121xb <- gt::render_gt(R4121x()[[2]]) # wind gt - local
  output$O4122x <- gt::render_gt(R4122x()) # char gt

  output$O4131x <- gt::render_gt(R4131x()) # summ gt

  output$O4132xa <- gt::render_gt(R4132x()[["local"]][[1]]) # render----
  output$O4132xb <- gt::render_gt(R4132x()[["local"]][[2]]) # render----
  output$O4132xc <- gt::render_gt(R4132x()[["custom"]][[1]]) # render----
  output$O4132xd <- gt::render_gt(R4132x()[["custom"]][[2]]) # render----

  print("Leaving s4100")
}
