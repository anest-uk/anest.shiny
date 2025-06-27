server_tim1 <- function(input, output, session, common) {
  R111x <- eventReactive( # 111 leaflet----
    list(
      common$rc6tR(),
      common$rc6ccR(),
      common$tslideR()
    ),
    {
      print('enter server_tim1')
      x <- D111x(
        rc6x = common$rc6tR(),
        rc6ccx=common$rc6ccR(),
        x1 = pxosrdo2dd, 
        x2 = z110, 
        x3 = f250519ad
        )
      x
    }
  )

  # rc6tx = rc6tG,
  #     x0 = C111d(cus=rsiccG)
  
  R112x <- eventReactive( # 112 x(t) ----
    list(
      common$rc6tR(),
      common$estdtccR(),
      common$tslideR()
    ),
    {
      x <-
        D112x(
          rc6tx = common$rc6tR(),
          x0 = C111d(cus=common$rsiccR()),
          # x1 = common$estdtccR(),
          tslidex = common$tslideR()
        )
      x
    }
  )

  R121x <- eventReactive( # 121 winding ----
    list(
      common$rc6tR()
    ),
    {
      x <- D121x(rc6t=common$rc6tR())
      x
    }
  )
  
   R122x <- eventReactive( # 121 winding ----
    list(
      common$rc6tR(),
      common$rssaR(),
      common$rssccR()
    ),
    {
      #browser()
      x <- D122x(
        rc6t=common$rc6tR(),
        rssax=common$rssaR(),
        rssccx=common$rssccR(),
        z110=z110
        )
      x
    }
  )

  # R122x <- eventReactive( # 122 charac ----
  #   list(
  #     common$rc6tR()
  #   ),
  #   {
  #     x <- ggplot(mtcars, aes(drat, wt)) +
  #       geom_point()
  #     x
  #   }
  # )

  R131x <- eventReactive( # 131 summary ----
    list(
      common$rc6tR()
    ),
    {
      x <- ggplot(mtcars, aes(drat, wt)) +
        geom_point()
      x
    }
  )

  R132x <- eventReactive( # 132 trade ----
    list(
      common$rc6tR()
    ),
    {
      x <- ggplot(mtcars, aes(drat, wt)) +
        geom_point()
      x
    }
  )


  
  output$O111x <- renderLeaflet(R111x()) # render----
  output$O112x <- renderPlot(R112x()) #x(t)
  output$O121x <- gt::render_gt(R121x()) #wind gt
  output$O122x <- gt::render_gt(R122x()) #char gt
  
  output$O131x <- gt::render_gt(R131x()) #summ gt
  output$O132x <- gt::render_gt(R132x()) #trede gt
  print("Leaving server_R1()...")
}

