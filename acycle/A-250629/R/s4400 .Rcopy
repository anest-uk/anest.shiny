#-------------------------------------------------4400 listing
print('<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< s4400 ')
s4400 <-
  function(input, output, session, common) {
    RR4411 <- #-  ----
      eventReactive(
        list(
          rc6tx = common$rc6tR()
        ),
        {
          if (verbose) print("enter RR4411")
          x1 <- DD4411( rc6t = common$rc6tR() )
          x2 <- DD4412( rc6t = common$rc6tR() )
          x <- 
            list(
              tab=x1,
              sum=x2
            )
          G4411 <<- copy(x)
          x
        }
      )

    output$OO4411a <- gt::render_gt(RR4411()[['tab']]) # table    #----
    output$OO4411b <- gt::render_gt(RR4411()[['sum']]) # summary    #----
  }
