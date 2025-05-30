server_constituents <- function(input, output, session, common) {
  x311D <- eventReactive( #-------'-311 custom----
    list(common$geoaR(), common$z110R(), rc6tX = common$rc6tR()),
    {
      if (verbose) print("enter 311")
      x <- f311D(
        geo0X = common$geoaR(),
        z110X = common$z110R(),
        rc6tX = substr(common$rc6tR(), 1, 3) # arbitrarily initialise it to the area
      )
      x311G <<- copy(x)
      x
    }
  )
  output$x311 <- DT::renderDT(x311D()) # output----
}
print("exit ss")
