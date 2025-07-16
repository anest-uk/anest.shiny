
s4100 <-#-------------------- 4100 timeseries ----
  function(input, output, session, common) {
    
    R4111x <- eventReactive( # -- 4111 leaflet----
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

    R4112x <- eventReactive( #--- 4112 ggplot ----
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

    R4121x <- eventReactive( #-- 4121 winding ----
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

    R4122x <- eventReactive( #---- 4122 trade ----
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

    R4131x <- eventReactive( #-- 4131 summary ----
      list(
        common$rescxR(),
        common$rc6tR(),
        common$tslideR()
      ),
      {
        x <- D4131a(
          rescx = common$rescxR(),
          rc6tx = common$rc6tR(),
          tslidex = common$tslideR()
        )
        G4131x <<- copy(x)
        x
      }
    )

    R4132x <- eventReactive( #----- 132 trade ----
      list(
        common$rescxR(),
        common$rc6tR(),
        common$tslideR()
      ),
      {
        x <- #list local/custom return/number
          D4132x(
          rescxx = common$rescxR(),
          rc6tx = common$rc6tR(),
          tslidex = common$tslideR()
        )
        G4132x <<- copy(x)
        x
      }
    )

    output$O4111x <- renderLeaflet(R4111x()) # leaflet ----
    output$O4112x <- renderPlot(R4112x()) # ggplot ----
    output$O4121xa <- gt::render_gt(R4121x()[[1]]) # wind gt - custom ----
    output$O4121xb <- gt::render_gt(R4121x()[[2]]) # wind gt - local ----
    output$O4122x <- gt::render_gt(R4122x()) # char gt ----
    output$O4131x <- gt::render_gt(R4131x()) # summ gt ----
    output$O4132xa <- gt::render_gt(R4132x()[["local"]][[1]]) # local-return ----
    output$O4132xb <- gt::render_gt(R4132x()[["local"]][[2]]) # local-number ----
    output$O4132xc <- gt::render_gt(R4132x()[["custom"]][[1]]) # custom-return ----
    output$O4132xd <- gt::render_gt(R4132x()[["custom"]][[2]]) # custom-number-----

    print("Leaving s4100")
  }
