
s4200 <-#-------------------- 4100 timeseries ----
  function(input, output, session, common) {
    
    RR4212 <- eventReactive( # -- 4111 leaflet----
      list(
        common$rc6tR(), # C
        common$rc6cR() # C
      ),
      {
        print("enter RR4212")
        x <- DD4212(
          rc6tx = common$rc6tR(), # target
          rc6cx = common$rc6cR()  # custom
        )
        G4111x <<- copy(x)
        x
      }
    )

    RR4222 <- eventReactive( #--- 4112 ggplot ----
      list(
        common$rc6tR(), # C
        common$rescxR(), # U
        common$tslideR() # C
      ),
      {
        x <-
          DD4222(
            rc6tx = common$rc6tR(),
            rescxx = common$rescxR(),
            tslidex = common$tslideR()
          )
        G4112x <<- copy(x)
        x
      }
    )

    RR4221 <- eventReactive( #-- 4121 winding ----
      list(
        common$rc6tR(),
        common$rescxR()
      ),
      {
        x <- DD4221(
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

    RR4211 <- eventReactive( #-- 4131 summary ----
      list(
        common$rescxR(),
        common$rc6tR(),
        common$tslideR()
      ),
      {
        x <- DD4211(
          rescx = common$rescxR(),
          rc6tx = common$rc6tR(),
          tslidex = common$tslideR()
        )
        G4131x <<- copy(x)
        x
      }
    )

    RR4231 <- eventReactive( #----- 132 trade ----
      list(
        common$rescxR(),
        common$rc6tR(),
        common$tslideR()
      ),
      {
        x <- #list local/custom return/number
          DD4231(
          rescxx = common$rescxR(),
          rc6tx = common$rc6tR(),
          tslidex = common$tslideR()
        )
        G4132x <<- copy(x)
        x
      }
    )
    output$OO4211 <- gt::render_gt(RR4211()) # summ gt ----
    output$OO4212 <- leaflet::renderLeaflet(RR4212()) # leaflet ----
    output$OO4221a <- gt::render_gt(RR4221()[[1]]) # wind gt - custom ----
    output$OO4221b <- gt::render_gt(RR4221()[[2]]) # wind gt - local ----
    output$OO4222 <- renderPlot(RR4222()) # ggplot ----
    output$OO4222x <- gt::render_gt(RR4222x()) # char gt ----
    output$OO4231a <- gt::render_gt(RR4231()[["local"]][[1]]) # local-return ----
    output$OO4231b <- gt::render_gt(RR4231()[["local"]][[2]]) # local-number ----
    output$OO4231c <- gt::render_gt(RR4231()[["custom"]][[1]]) # custom-return ----
    output$OO4231d <- gt::render_gt(RR4231()[["custom"]][[2]]) # custom-number-----

    print("Leaving s4100")
  }
