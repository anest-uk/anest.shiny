server_tim1 <- function(input, output, session, common) {
  R111x <- eventReactive( # 111 leaflet----
    list(
      common$rc6tR(), #C
      common$rc6cR() #C
    ),
    {
      print('enter server_tim1')
      x <- D111x(
        rc6tx = common$rc6tR(),
        rc6cx=common$rc6cR(),
        x1 = apol(datS), 
        x2 = apva(resS)
        )
      x
    }
  )

  # rc6tx = rc6tG,
  #     x0 = Ccus(cus=rescG)
  
  R112x <- eventReactive( # 112 x(t) ----
    list(
      common$rc6tR(),   #C
      common$tslideR(), #C
      common$estdtccR() #U switch this to resccR
    ),
    {
      #browser()
      x <-
        D112x(
          rc6tx = common$rc6tR(),
          x0 = Ccus(rescx=common$rescR()),
          # x1 = common$estdtccR(),
          tslidex = common$tslideR()
        )
      x
    }
  )
  

  R121x <- eventReactive( # 121 winding ----
    list(
      common$rc6tR(),
      common$estdtccR()
    ),
    {
      x <- D121x(
        rc6t=common$rc6tR(),
        cus=common$R111dx()
        )
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
  output$O121xa <- gt::render_gt(R121x()[[1]]) #wind gt - custom
  output$O121xb <- gt::render_gt(R121x()[[2]]) #wind gt - local
  output$O122x <- gt::render_gt(R122x()) #char gt
  
  output$O131x <- gt::render_gt(R131x()) #summ gt
  output$O132x <- gt::render_gt(R132x()) #trede gt
  print("Leaving server_R1()...")
}

