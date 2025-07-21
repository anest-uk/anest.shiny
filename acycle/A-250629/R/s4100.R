#-------------------------------------------------4300 constituents
s4100 <-
  function(input, output, session, common) {
    RR4111 <- eventReactive( #-4311 local blobs----
      list(
        rc6tx = common$rc6tR(),
        rc6cx = common$rc6cR()
      ),
      {
        if (verbose) print("enter R4111")
        x <- DD4111(
          rc6tx = common$rc6tR(), # control: target
          rc6cx = common$rc6cR() # control: custom
        )
        G4311 <<- copy(x)
        x
      }
    )
    RR4121 <- eventReactive( # 4321 'identifies as' message----
      list(
        common$geoaR(),
        rc6tx = common$rc6tR(),
        rc6cx = common$rc6cR()
      ),
      {
        if (verbose) print("enter RR4121")
        x <- DD4121(
          rc6tx = common$rc6tR()
        )
        G4321 <<- copy(x)
        x
      }
    )
    RR4131 <- eventReactive( #-------4331 custom blobs----
      list(
        common$geoaR(),
        rc6tx = common$rc6tR(),
        rc6cx = common$rc6cR()
      ),
      {
        if (verbose) print("enter R4131")
        x <- DD4131(
          rc6tx = common$rc6tR(), # control: target
          rc6cx = common$rc6cR() # control: custom
        )
        G4331 <<- copy(x)
        x
      }
    )
    RR4112 <- eventReactive( #-------4312 augmented leaflet----
      list(
        common$geoaR(),
        rc6tx = common$rc6tR(),
        rc6cx = common$rc6cR()
      ),
      {
        if (verbose) print("enter RR4112")
        x <- DD4112(
          rc6tx = common$rc6tR(), # control: target
          rc6cx = common$rc6cR()
        )
        G4312 <<- copy(x)
        x
      }
    )
    RR4113 <- eventReactive( #-------4300 title for table----
      list(
        common$geoaR(),
        rc6tx = common$rc6tR()
      ),
      {
        if (verbose) print("enter RR4113")
        x <-
          which(f230306a() == substr(common$rc6tR(), 1, 3)) %>%
          names(.) %>% # string
          gsub(":", ": ", .) # %>%
        # paste0(.,'  plus peers')
        G4300 <<- copy(x)
        x
      }
    )
    output$OO4113 <- renderUI({
      HTML(markdown::renderMarkdown(text = RR4113()))
    })
    output$OO4111 <- gt::render_gt(RR4111())
    output$OO4121 <- renderUI({
      HTML(markdown::renderMarkdown(text = RR4121()))
    })
    output$OO4131 <- gt::render_gt(RR4131())
    output$OO4112 <- leaflet::renderLeaflet(RR4112()) # constuents #----
  }
print("exit s4100")
