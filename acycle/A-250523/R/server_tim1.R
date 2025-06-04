server_tim1 <- function(input, output, session, common) {
  R111x <- eventReactive( # -------111 leaflet----
    list(
      common$rc6tR()
    ),
    {
      x <- f250517a(rc6x = common$rc6tR(), x1 = pxosrdo2dd, x2 = z110, x3 = f250519ad)
      x
    }
  )

  R112x <- eventReactive( # -------112 ----
    list(
      common$rc6tR()
    ),
    {
      x <-
        f250509ed$estdt[lab == common$labxR()[1, lab]] %>%
        ggplot(., aes(date, x)) +
        geom_point()
      x
    }
  )

  R121x <- eventReactive( # -------121 ----
    list(
      common$rc6tR()
    ),
    {
      x <- ggplot(mtcars, aes(drat, wt)) +
        geom_point()
      x
    }
  )

  R122x <- eventReactive( # -------122 ----
    list(
      common$rc6tR()
    ),
    {
      x <- ggplot(mtcars, aes(drat, wt)) +
        geom_point()
      x
    }
  )

  R131x <- eventReactive( # -------131 ----
    list(
      common$rc6tR()
    ),
    {
      x <- ggplot(mtcars, aes(drat, wt)) +
        geom_point()
      x
    }
  )

  R132x <- eventReactive( # -------132 ----
    list(
      common$rc6tR()
    ),
    {
      x <- ggplot(mtcars, aes(drat, wt)) +
        geom_point()
      x
    }
  )


  output$O111x <- renderLeaflet(R111x()) #---render----
  output$O112x <- renderPlot(R112x())
  output$O121x <- renderPlot(R121x())
  output$O122x <- renderPlot(R122x())
  output$O131x <- renderPlot(R131x())
  output$O132x <- renderPlot(R132x())
  print("Leaving server_R1()...")
}
