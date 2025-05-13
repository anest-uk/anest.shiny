server_common <- 
  function(
    input, 
    output, 
    session
  ) {
    geoaR <- #---area geo compute       ----
    eventReactive(
      rc6tR(),
      {
        if (verbose) print("enter geoaR")
        x <- geo0R() %>%
          .[rc3 == substr(rc6tR(), 1, 3)]
        geoaG <<- copy(x)
        x
      }
    )
    
    geotR <- #---target geo compute     ----
    eventReactive(
      rc6tR(),
      {
        if (verbose) print("enter geotR")
        x <-
          geoaR() %>%
          .[rc6 == rc6tR()]
        geotG <<- copy(x)
        if (verbose) print("exit geotR")
        x
      }
    )
    
    nxqR <- #---qtile nx compute        ----
    eventReactive(
      geoqR(),
      {
        if (verbose) print("enter nxqR")
        x <-
          geoqR()[, .(nx, rc3, qtile, lab)] %>%
          unique(.)
        nxqG <<- copy(x)
        x
      }
    )
    
    dfnxR <- reactive({ # 4-col table with NA
      x <-
        dcast(f241021adG$estdt[, .(tbin, date)] %>% unique(.) %>% .[order(tbin, date)], date ~ tbin, value.var = "date") %>% # lo, hi, an
        rbind(as.data.table(as.list(rep(as.Date("1994-12-31"), 4))), ., use.names = F) %>%
        setnames(., c("date", "tbin1", "tbin2", "tbin3"))
      dfnxG <<- copy(x)
      x
    })
    
    rsicuR <- #---custom rsi compute    ----
    eventReactive(
      list(
        input$docusabC
      ),
      {
        if (verbose) print("enter rsicuR<<<<<<<<<<<<<<<<<<<<<<<<<<")
        geox <- isolate(geocuR())
        dfnx <- isolate(dfnxxR()) # source of truth
        rc6tx <- toupper(isolate(irregpcode(input$rc6tC[1])))
        rc6valid <- isolate(geo0R()[, rc6])
        if (
          (irregpcode(regpcode(rc6tx)) == rc6tx) &
          (nchar(regpcode(rc6tx)) == 6) &
          (regpcode(rc6tx) %in% rc6valid)
        ) {
          print("recalc accepted in rsicuR")
          x <-
            f241119a( # returns estdt, kfoldsse, all
              nxx = 0,
              steprip2 = stepripG, # smaller format
              dfn = dfnx, # R
              geo = geox, # R
              outthresh = .1,
              kfold = 5,
              sectorwise = T,
              usepra = F,
              newused = c("."),
              houseflat = c(".")
            )
          rsicuG <<- copy(x)
        } else {
          print("recalc rejected in rsicuR")
          x <- copy(rsicuG)
        }
        x
      }
    )
    
    rc6cuR <- #---custom rc6 control    ----
    eventReactive(
      list(rc6tR(), input$rctreeC), #+control
      {
        if (verbose) print("enter rc6cuR")
        x <- sort(unique(c(rc6tR(), input$rctreeC))) %>% .[nchar(.) == 6]
        rc6cuG <<- copy(x)
        x
      }
    )
    
    rc6tR <- #---target rc6 reformat    ----
    eventReactive(
      input$rc6tC,
      {
        if (verbose) print("enter rc6tR")
        x <-
          regpcode(input$rc6tC)[1]
        rc6tG <<- copy(x)
        x
      }
    )
    
    geo0R <-    #----
    reactive({
      x <-
        geoplusR() %>%
        .[type == "L"] %>%
        .[itrim == itriC] %>%
        .[tbin == tbinC] %>%
        .[, .(
          nx,
          gx,
          lab = des,
          rc6 = rc9,
          rc3 = substr(rc9, 1, 3),
          qtile = as.numeric(substr(des, 4, 4))
        )] %>%
        z110R()[., on = c(rcx = "rc6")] %>%
        .[, .(nx, gx, lab, rc3, rc6 = rcx, qtile)]
      geo0G <<- copy(x)
      x
    })
    
    geoplusR <- #----
    reactive({
      x <- copy(f241021adG$geoplus)[, let(lab, des)]
      geoplusG <<- copy(x)
      x
    })
    
    # x00R <- reactive({
    #   x <- copy(f241021ad)
    #   x00G <<- copy(x)
    #   x
    # })
    
    z110R <-    #----
    reactive({
      x <- copy(z110)
      z110G <<- copy(x)
      x
    })
    
    estdtR <-   #----
    reactive({
      x <- copy(f241021adG$estdt)[, .(nx, ii, date, xdotd, days, xdot, x)]
      estdtG <<- copy(x)
      x
    })
    
    estdtlR <- #---local estdt compute  ----
    eventReactive(
      nxqR(),
      {
        if (verbose) print("enter estdtlR")
        x <-
          estdtR()[nxqR(), on = c(nx = "nx")] %>%
          .[, .(nx, date, ii, lab, rc3, qtile, xdotd, days, xdot, x)]
        estdtlG <<- copy(x)
        x
      }
    )
    
    geoqR <- #---qtile geo select       ----
    eventReactive(
      list(
        geoaR(), geotR() # ,
      ),
      {
        if (verbose) print("enter geoqR")
        x <- geoaR() %>%
          .[geotR()[, .(qtile)],
            on = c(qtile = "qtile")
          ]
        geoqG <<- copy(x)
        x
      }
    )
    
    dfnxxR <-   #----
    reactive({ # vector of current date
      x <-
        dfnxR()[, paste0("tbin", tbinC), with = F] %>%
        setnames(., "x") %>%
        .[, sort(unique(x))]
      dfnxxG <<- copy(x)
      x
    })
    
    estdtcuR <- #---custom estdt select ----
    eventReactive(
      list(
        rsicuR()
      ),
      {
        if (verbose) print("enter estdtcuR")
        x <- rsicuR()$estdt
        estdtcuG <<- copy(x)
        x
      }
    )
    
    geocuR <- #---custom geo compute    -----
    eventReactive(
      rc6cuR(),
      {
        if (verbose) print("enter geocuR")
        x <-
          fgeocuX(rc6cuX = rc6cuR())
        geocuG <<- copy(x)
        x
      }
    )
    
    fgeocuX <- #---custom geo compute   -----
    function(
    rc6cuX = rc6cuG) {
      x <-
        data.table(rc9 = rc6cuX, nx = 0, lab = "CU00")
      x
    }
    
    tslideR <- reactive({
      x <- input$tslider
      tslideG <<- copy(x)
      x
    })
    
    
    list( #common list #-----
          estdtR=estdtR,
          estdtlR=estdtlR,
          geoqR=geoqR,
          dfnxxR=dfnxxR,
          estdtcuR=estdtcuR,
          geocuR=geocuR,
          geoaR=geoaR,
          geotR=geotR,
          nxqR=nxqR,
          dfnxR=dfnxR,
          rsicuR=rsicuR,
          rc6cuR=rc6cuR,
          rc6tR=rc6tR,
          geo0R=geo0R,
          geoplusR=geoplusR,
          z110R=z110R,
          tslideR=tslideR
    )
  }
