s4300 <- function(input, output, session, common) {
  R4311x <- eventReactive( #-------'-311 custom----
    list(common$geoaR(), rc6tx = common$rc6tR()), #, common$z110R()
    {
      if (verbose) print("enter 311")
      x <- D311(
        geo0x = common$geoaR(),
        z110x = z110,#common$z110R(),
        rc6tx = substr(common$rc6tR(), 1, 3) # arbitrarily initialise it to the area
      )
      G311 <<- copy(x)
      x
    }
  )
  output$O4311x <- DT::renderDT(R4311x()) # output----
}
print("exit s4300")
