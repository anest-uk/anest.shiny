server_accuracy <- function(input, output, session, common) {
  R411 <- eventReactive( #-----------------411----
    list(common$geoqR(), common$rc6tR(), common$rssR()),
    {
      if (verbose) print("enter G411")
      x <- D411(geoqx = common$geoqR(), rc6tx = common$rc6tR(), rssx = common$rssR())
      G411 <<- copy(x)
      x
    }
  )

  R412 <- eventReactive( #-----------------412----
    list(common$geocR(), common$rc6tR(), common$rsscR()),
    {
      if (verbose) print("enter R412")
      x <- D412(geocx = common$geocR(), rc6tx = common$rc6tR(), rsscx = common$rsscR())
      G412 <<- copy(x)
      x
    }
  )

  R421 <- eventReactive( #-----------------421----
    list(common$geoqR(), common$rc6tR(), common$rssR()),
    {
      if (verbose) print("enter R421")
      x <- D421(geoqx = common$geoqR(), rc6tx = common$rc6tR(), rssx = common$rssR())
      G421 <<- copy(x)
      x
    }
  )

  R422 <- eventReactive( #-----------------422----
    list(common$geocR(), common$rc6tR(), common$rsscR()),
    {
      if (verbose) print("enter R422")
      x <- D422(geocx = common$geocR(), rc6tx = common$rc6tR(), rsscx = common$rsscR())
      G422 <<- copy(x)
      x
    }
  )

  R431 <- eventReactive( #-----------------431----
    list(common$geoqR(), common$rc6tR(), common$rssR()),
    {
      if (verbose) print("enter R431")
      x <- D431(geoqx = common$geoqR(), rc6tx = common$rc6tR(), rssx = common$rssR())
      G431 <<- copy(x)
      x
    }
  )

  R432 <- eventReactive( #-----------------432----
    list(common$geocR(), common$rc6tR(), common$rsscR()),
    {
      if (verbose) print("enter R432")
      x <- D432(geocx = common$geocR(), rc6tx = common$rc6tR(), rsscx = common$rsscR())
      G432 <<- copy(x)
      x
    }
  )

  # output$O411 <- gt::render_gt(R411()) #-----.----
  # output$O412 <- gt::render_gt(R412()) #-----.----
  # output$O421 <- gt::render_gt(R421()) #-----.----
  # output$O422 <- gt::render_gt(R422()) #-----.----
  # output$O431 <- gt::render_gt(R431()) #-----.----
  # output$O432 <- gt::render_gt(R432()) #-----.----
}
