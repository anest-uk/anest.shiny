#-------------------------------------------------4200 listing

s4300 <-
  function(input, output, session, common) {
    RR4311 <- #-4211a timebin,local, custom table list----
      eventReactive(
        list(
          rescx = common$rescxR(),
          rc6tx = common$rc6tR(),
          tslidex = common$tslideR()
        ),
        {
          if (verbose) print("enter RR4311")
          x <-
            DD4311(
              rescx = common$rescxR(),
              rc6tx = common$rc6tR(),
              tslidex = common$tslideR()
            )
          G4211a <<- copy(x)
          x
        }
      )

    output$OO4311 <- gt::render_gt(RR4311()[[1]]) # timebins #----
    output$OO4312 <- gt::render_gt(RR4311()[[2]]) # local    #----
    output$OO4313 <- gt::render_gt(RR4311()[[3]]) # custom   #----
  }
