
s4100 <-#-------------------- 4100 timeseries ----
  function(input, output, session, common) {
    
    RR4211x <- eventReactive( # -- 4111 leaflet----
      list(
        common$rc6tR(), # C
        common$rc6cR() # C
      ),
      {
        print("enter RR4211x")
        x <- DD4211x(
          rc6tx = common$rc6tR(), # target
          rc6cx = common$rc6cR()  # custom
        )
        G4111x <<- copy(x)
        x
      }
    )

    RR4212x <- eventReactive( #--- 4112 ggplot ----
      list(
        common$rc6tR(), # C
        common$rescxR(), # U
        common$tslideR() # C
      ),
      {
        x <-
          DD4212x(
            rc6tx = common$rc6tR(),
            rescxx = common$rescxR(),
            tslidex = common$tslideR()
          )
        G4112x <<- copy(x)
        x
      }
    )

    RR4221x <- eventReactive( #-- 4121 winding ----
      list(
        common$rc6tR(),
        common$rescxR()
      ),
      {
        x <- DD4221x(
          rc6tx = common$rc6tR(),
          rescx = common$rescxR()
        )
        G4121x <<- copy(x)
        x
      }
    )

    RR4222x <- eventReactive( #---- 4122 trade ----
      list(
        common$rc6tR(),
        rescxx = common$rescxR()
      ),
      {
        x <- DD4222x(
          rc6tx = common$rc6tR(),
          rescxx = common$rescxR()
        )
        G4122x <<- copy(x)
        x
      }
    )

    RR4231x <- eventReactive( #-- 4131 summary ----
      list(
        common$rescxR(),
        common$rc6tR(),
        common$tslideR()
      ),
      {
        browser()
        x <- DD4231a(
          rescx = common$rescxR(),
          rc6tx = common$rc6tR(),
          tslidex = common$tslideR()
        )
        G4131x <<- copy(x)
        x
      }
    )

    RR4232x <- eventReactive( #----- 132 trade ----
      list(
        common$rescxR(),
        common$rc6tR(),
        common$tslideR()
      ),
      {
        x <- #list local/custom return/number
          DD4232x(
          rescxx = common$rescxR(),
          rc6tx = common$rc6tR(),
          tslidex = common$tslideR()
        )
        G4132x <<- copy(x)
        x
      }
    )
    output$OO4211 <- gt::render_gt(RR4231x()) # summ gt ----
    output$OO4212 <- leaflet::renderLeaflet(RR4211x()) # leaflet ----
    output$OO4221a <- gt::render_gt(RR4221x()[[1]]) # wind gt - custom ----
    output$OO4221b <- gt::render_gt(RR4221x()[[2]]) # wind gt - local ----
    output$OO4222 <- renderPlot(RR4212x()) # ggplot ----
    output$OO4222x <- gt::render_gt(RR4222x()) # char gt ----
    output$OO4231a <- gt::render_gt(RR4232x()[["local"]][[1]]) # local-return ----
    output$OO4231b <- gt::render_gt(RR4232x()[["local"]][[2]]) # local-number ----
    output$OO4231c <- gt::render_gt(RR4232x()[["custom"]][[1]]) # custom-return ----
    output$OO4231d <- gt::render_gt(RR4232x()[["custom"]][[2]]) # custom-number-----

    print("Leaving s4100")
  }
