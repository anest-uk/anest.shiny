server_tim1 <- function(input, output, session, common) {
  tim111D <- eventReactive( # -------111 leaflet----
    list(
      common$rc6tR()
    ),
    {
      print("enter tim111")
      x <- f250517a(rc6x = common$rc6tR(), x1 = pxosrdo2dd, x2 = z110, x3 = f250519ad)
      x
    }
  )

  tim112D <- eventReactive( # -------112 ----
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

  tim121D <- eventReactive( # -------121 ----
    list(
      common$rc6tR()
    ),
    {
      x <- ggplot(mtcars, aes(drat, wt)) +
        geom_point()
      x
    }
  )

  tim122D <- eventReactive( # -------122 ----
    list(
      common$rc6tR()
    ),
    {
      x <- ggplot(mtcars, aes(drat, wt)) +
        geom_point()
      x
    }
  )

  tim131D <- eventReactive( # -------131 ----
    list(
      common$rc6tR()
    ),
    {
      x <- ggplot(mtcars, aes(drat, wt)) +
        geom_point()
      x
    }
  )

  tim132D <- eventReactive( # -------132 ----
    list(
      common$rc6tR()
    ),
    {
      x <- ggplot(mtcars, aes(drat, wt)) +
        geom_point()
      x
    }
  )


  output$tim111 <- renderLeaflet(tim111D()) #---render----
  output$tim112 <- renderPlot(tim112D())
  output$tim121 <- renderPlot(tim121D())
  output$tim122 <- renderPlot(tim122D())
  output$tim131 <- renderPlot(tim131D())
  output$tim132 <- renderPlot(tim132D())
  print("Leaving server_tim1()...")
}
