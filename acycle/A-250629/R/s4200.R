#-------------------------------------------------4200 listing

s4200 <- 
  function(input, output, session, common) {
  R4211a <-  #-4211a timebin,local, custom table list----
    eventReactive(
      list(
          rescx = common$rescxR(),
          rc6tx = common$rc6tR(),
          tslidex = common$tslideR()
      ),
      {
        if (verbose) print("enter R4211a")
        x <- 
          D4211b(
            rescx = common$rescxR(),
            rc6tx = common$rc6tR(),
            tslidex = common$tslideR()
            )
        G4211a <<- copy(x)
        x
      }
    )

  output$O4211 <- gt::render_gt(R4211a()[[1]]) #timebins #----
  output$O4212 <- gt::render_gt(R4211a()[[2]]) #local    #----
  output$O4213 <- gt::render_gt(R4211a()[[3]]) #custom   #----
  
}
