server_constituents <- function(input, output, session, common) {
  x311D <- eventReactive( #-------'-311 custom----
    list(common$geoaR(), rc6tx = common$rc6tR()), #, common$z110R()
    {
      if (verbose) print("enter 311")
      x <- f311D(
        geo0x = common$geoaR(),
        z110x = z110,#common$z110R(),
        rc6tx = substr(common$rc6tR(), 1, 3) # arbitrarily initialise it to the area
      )
      x311G <<- copy(x)
      x
    }
  )
  output$x311 <- DT::renderDT(x311D()) # output----
}
print("exit ss")
