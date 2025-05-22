server_tim1 <- function(input, output, session, common) {
  
  tim111D <- eventReactive( # -------111 leaflet----
    list(
      common$rc6tR()
    ),
    {
      print("enter tim111")
      #x <- ggplot(mtcars, aes(drat, wt)) + geom_point()
      #x <- ggplot(dataG$f250509ed$estdt[nx==min(nx)], aes(date, x)) + geom_point()
  #     x <- f250517a <- #map 1/2/3 colour + 'specials'=peers+
  # function(
  #   rc6x='SW-8--', #target
  #   x0=f250509ed,  #SOT
  #   x1=pxosrdo2dd,
  #   rc6all=z110[nchar(rcx)==6,][grep(rc3x,rcx)][rcx!=rc6x][1,c(rcx,rc6x)], #peers to highlight
  #   rc3x=substr(rc6x,1,3) #target area
  # )
  #     x
  #   }
      x <- f250517a(rc6x=common$rc6tR(),x1=pxosrdo2dd,x2=z110,x3=f250519ad)
      x
    }
  )#eventreactive

   tim112D <- eventReactive( # -------112 ----
    list(
      common$rc6tR()
    ),
    {
      x <- 
        f250509ed$estdt[lab==common$labxR()[1,lab]]%>%
        ggplot(., aes(date, x)) + geom_point()
      x
    }
  )  
  # tim112D <- eventReactive( # -------112 ----
  #   list(
  #     common$rc6tR()
  #   ),
  #   {
  #     x <- ggplot(mtcars, aes(drat, wt)) + geom_point()
  #     x
  #   }
  # )
  
  tim121D <- eventReactive( # -------121 ----
    list(
      common$rc6tR()
    ),
    {
      x <- ggplot(mtcars, aes(drat, wt)) + geom_point()
      x
    }
  )
  
  tim122D <- eventReactive( # -------122 ----
    list(
      common$rc6tR()
    ),
    {
      x <- ggplot(mtcars, aes(drat, wt)) + geom_point()
      x
    }
  )
  
  tim131D <- eventReactive( # -------131 ----
    list(
      common$rc6tR()
    ),
    {
      x <- ggplot(mtcars, aes(drat, wt)) + geom_point()
      x
    }
  )
  
  tim132D <- eventReactive( # -------132 ----
    list(
      common$rc6tR()
    ),
    {
      x <- ggplot(mtcars, aes(drat, wt)) + geom_point()
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
