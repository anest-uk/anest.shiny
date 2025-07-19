#-------------------------------------------------4300 constituents
s4300 <- 
  function(input, output, session, common) {
  R4311x <- eventReactive( #-4311 local blobs----
    list(
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
  R4331x <- eventReactive( #-------4331 custom blobs----
  list(
    common$geoaR(),
    rc6tx = common$rc6tR(),
    rc6cx = common$rc6cR()
  ),
  {
    if (verbose) print("enter R4331a")
    x <- D4331a(
      rc6tx = common$rc6tR(), # control: target
      rc6cx = common$rc6cR() # control: custom
    )
    G4331 <<- copy(x)
    x
  }
)
  R4312x <- eventReactive( #-------4312 augmented leaflet----
    list(
      common$geoaR(), 
      rc6tx = common$rc6tR()
      ),
    {
      if (verbose) print("enter R4312x")
      x <- D4312a(
        rc6tx = common$rc6tR() #control: target
      )
      G4312 <<- copy(x)
      x
    }
  )
  R4300x <- eventReactive( #-------4300 title for table----
    list(
      common$geoaR(), 
      rc6tx = common$rc6tR()
      ),
    {
      if (verbose) print("enter R4300x")
      x <- 
        which(f230306a()==substr(common$rc6tR(),1,3))%>%
        names(.)%>% #string
        gsub(':',': ',.)#%>%
        #paste0(.,'  plus peers')
      G4300 <<- copy(x)
      x
    }
  )
  output$O4300x <-renderUI({ HTML(markdown::renderMarkdown(text = R4300x())) })
  output$O4311x <-gt::render_gt(R4311x())
  output$O4321x <- renderUI({ HTML(markdown::renderMarkdown(text = R4321x())) })
  output$O4331x <-gt::render_gt(R4331x())
  output$O4312x <-leaflet::renderLeaflet(R4312x()) #constuents #----
}
print("exit s4300")
