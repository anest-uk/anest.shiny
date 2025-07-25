
s3000 <-#-------------------------3000 sidebar----
  function(input, output, session, common) {
    R3001x <- #---------- slider date readback----
      eventReactive(
      list(
        rc6tx = common$tslideR()
      ),
      {
        if (verbose) print("enter R3001x")
        x <-
          common$dfnyR()[input$tslider + 1] %>%
          as.Date() %>%
          as.character(.)
        G3001x <<- copy(x)
        x
      }
    )
    R3002x <- #------ target locality readback----
      eventReactive(
      list(
        rc6tx = common$rc6tR()
      ),
      {
        if (verbose) print("enter R3002x")
        x <-
          paste0(
            irregpcode(common$rc6tR()),
            " ",
            resS$f250713a[rc6 == common$rc6tR(), locality]
          )
        G3002x <<- copy(x)
        x
      }
    )

    output$O3001x <- renderText(R3001x())
    output$O3002x <- renderText(R3002x())
  }

