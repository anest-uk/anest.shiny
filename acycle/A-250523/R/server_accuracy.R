server_accuracy <- function(input, output, session, common) {
  x411D <- eventReactive( #-----------------411----
    list(common$geoqR(), common$rc6tR(), common$rssR()),
    {
      if (verbose) print("enter x411G")
      x <- f411D(geoqx = common$geoqR(), rc6tx = common$rc6tR(), rssx = common$rssR())
      x411G <<- copy(x)
      x
    }
  )

  x412D <- eventReactive( #-----------------412----
    list(common$geoccR(), common$rc6tR(), common$rssccR()),
    {
      if (verbose) print("enter x412D")
      x <- f412D(geoccx = common$geoccR(), rc6tx = common$rc6tR(), rssccx = common$rssccR())
      x412G <<- copy(x)
      x
    }
  )

  x421D <- eventReactive( #-----------------421----
    list(common$geoqR(), common$rc6tR(), common$rssR()),
    {
      if (verbose) print("enter x421D")
      x <- f421D(geoqx = common$geoqR(), rc6tx = common$rc6tR(), rssx = common$rssR())
      x421G <<- copy(x)
      x
    }
  )

  x422D <- eventReactive( #-----------------422----
    list(common$geoccR(), common$rc6tR(), common$rssccR()),
    {
      if (verbose) print("enter x422D")
      x <- f422D(geoccx = common$geoccR(), rc6tx = common$rc6tR(), rssccx = common$rssccR())
      x422G <<- copy(x)
      x
    }
  )

  x431D <- eventReactive( #-----------------431----
    list(common$geoqR(), common$rc6tR(), common$rssR()),
    {
      if (verbose) print("enter x431D")
      x <- f431D(geoqx = common$geoqR(), rc6tx = common$rc6tR(), rssx = common$rssR())
      x431G <<- copy(x)
      x
    }
  )

  x432D <- eventReactive( #-----------------432----
    list(common$geoccR(), common$rc6tR(), common$rssccR()),
    {
      if (verbose) print("enter x432D")
      x <- f432D(geoccx = common$geoccR(), rc6tx = common$rc6tR(), rssccx = common$rssccR())
      x432G <<- copy(x)
      x
    }
  )

  output$x411 <- gt::render_gt(x411D()) #-----.----
  output$x421 <- gt::render_gt(x421D()) #-----.----
  output$x431 <- gt::render_gt(x431D()) #-----.----
  output$x412 <- gt::render_gt(x412D()) #-----.----
  output$x422 <- gt::render_gt(x422D()) #-----.----
  output$x432 <- gt::render_gt(x432D()) #-----.----
}
