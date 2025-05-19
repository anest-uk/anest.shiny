server_1tss <- function(input, output, session, common) {
  f1tssD <- function(
  )
    
    tss12D <- eventReactive( #
      list(rc6tX = common$rc6tR()),
      {
        if (verbose) print("enter tss12D")
        x <- 
          f250517a(
            rc6x=common$rc6tR(), #target
            x0=dataG$f250509ed,  #SOT
            x1=dataG$pxosrdo2dd
          )
      }
    )
  output$tss12 <- renderLeaflet(tss12D()) 
  
  
}
print("exit tss12")
