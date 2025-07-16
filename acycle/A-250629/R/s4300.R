#-------------------------------------------------4300 constituents
s4300 <- 
  function(input, output, session, common) {
  R4311x <- eventReactive( #-------4311 blobs----
    list(
      common$geoaR(), 
      rc6tx = common$rc6tR(),
      rc6cx = common$rc6cR()
      ),
    {
      if (verbose) print("enter R4311a")
      x <- D4311a(
        rc6tx = common$rc6tR(), #control: target
        rc6cx = common$rc6cR()  #control: custom
      )
      G4311 <<- copy(x)
      x
    }
  )
  R4321x <- eventReactive( #4321 'identifies as' message----
    list(
      common$geoaR(), 
      rc6tx = common$rc6tR(),
      rc6cx = common$rc6cR()
      ),
    {
      if (verbose) print("enter R4321x")
      x <- D4321x(
        rc6tx = common$rc6tR()
      )
      G4321 <<- copy(x)
      x
    }
  )
     

  output$O4311x <-gt::render_gt(R4311x()) #constuents #----
  output$O4321x <- renderUI({ HTML(markdown::renderMarkdown(text = R4321x())) })
}
print("exit s4300")
