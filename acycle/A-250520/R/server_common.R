server_common <-
  function(input,
           output,
           session) {
    dfnxR <- # -----------4-col table with NA----
      reactive({
        x <-
          dcast(f241021adG$estdt[, .(tbin, date)] %>% unique(.) %>% .[order(tbin, date)], date ~ tbin, value.var = "date") %>% # lo, hi, an
          rbind(as.data.table(as.list(rep(as.Date("1994-12-31"), 4))), ., use.names = F) %>%
          setnames(., c("date", "tbin1", "tbin2", "tbin3"))
        dfnxG <<- copy(x)
        x
      })

    dfnxxR <- #---------vector of current date----
      reactive({
        x <-
          dfnxR()[, paste0("tbin", tbinC), with = F] %>%
          setnames(., "x") %>%
          .[, sort(unique(x))]
        dfnxxG <<- copy(x)
        x
      })

    estdtaR <- #---------- area estdt compute----
      eventReactive(
        list(
          nxaR(),
          estdtR()
        ),
        {
          if (verbose) print("enter estdtaR")
          x <-
            estdtR()[nxaR(), on = c(nx = "nx")] %>%
            .[, .(nx, date, ii, lab, rc3, qtile, xdotd, days, xdot, x)]
          estdtaG <<- copy(x)
          x
        }
      )

    estdtcuR <- #---------custom estdt select ----
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

    estdtlR <- #---------local estdt compute  ----
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

    estdtlnewR <- #------local estdt compute  ----
      eventReactive(
        nxqR(),
        {
          if (verbose) print("enter estdtlnewR")
          x <-
            copy(f250509ed$estdt)[, .(nx, ii, date, xdotd, days, xdot, x)] %>%
            .[nxqR(), on = c(nx = "nx")] %>%
            .[, .(nx, date, ii, lab, rc3, qtile, xdotd, days, xdot, x)]
          estdtlnewG <<- copy(x)
          x
        }
      )
    
    estdtR <- #-------------------------estdtR----
      reactive({
        x <- copy(f241021adG$estdt)[, .(nx, ii, date, xdotd, days, xdot, x)]
        # x <- copy(f250509ed$estdt)[, .(nx, ii, date, xdotd, days, xdot, x)]
        estdtG <<- copy(x)
        x
      })

    estdtxR <- #---------------------112 x(t)----
      eventReactive(
        list(estdtcuR(), estdtaR(), geocuR()),
        {
          festdtxX <- function(estdtcuX = estdtcuG, estdtaX = estdtaG, geocuX = geocuG) {
            x <-
              rbind(
                estdtcuX[, .(nx, date, xdotd, days, xdot, x, lab, ii, qtile = 0, rc3 = lab)],
                estdtaX[, .(nx, date, xdotd, days, xdot, x, lab, ii, qtile, rc3)]
              )[, qq := as.factor(qtile)]
            x
          }
          print("enter estdtxR")
          x <-
            festdtxX(estdtcuX = estdtcuR(), estdtaX = estdtaR(), geocuX = geocuR())
          estdtxG <<- copy(x)
          print("exit estdtxR")
          x
        }
      )
    
    geo0R <- #----------------------------geo0----
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
    
    geoaR <- #----------------area geo compute----
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
    
    geocuR <- #-------------custom geo compute----
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
    
    geoplusR <- #----------------------geoplus----
      reactive({
        x <- copy(f241021adG$geoplus)[, let(lab, des)]
        geoplusG <<- copy(x)
        x
      })
    
    geoqR <- #----------------qtile geo select----
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
    
    geotR <- #--------------target geo compute----
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
    
    labxR <- #-------------optimum index label----
      eventReactive(
        rc6tR(),
        {
          if (verbose) print("enter labxR")
          x <-
            f250509ed$geo %>%
            .[rc9 %in% rc6tR()] %>%
            .[grep("^L", lab)] %>%
            .[f250509ed$kfoldsse, on = c(nx = "nx"), nomatch = NULL] %>% # local only
            .[rc6 == rc6tR()] %>% # target rc6
            .[order(ssek), .(rc6, ssek, n, nx, lab)]
          labxG <<- copy(x)
          x
        }
      )

    nxaR <- #------------------area nx select----
      eventReactive(
        geoaR(),
        {
          if (verbose) print("enter nxaR")
          x <-
            geoaR()[, .(nx, rc3, qtile, lab)] %>%
            unique(.)
          nxaG <<- copy(x)
          if (verbose) print("exit nxaR")
          x
        }
      )
    
    nxqR <- #-----------------qtile nx compute----
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
    
    pxosrdo2ddR <- #---------------pxosrdo2dd----
      reactive({
        x <- copy(pxosrdo2dd)
        pxosrdo2ddG <<- copy(x)
        x
      })
    
    rc6cuR <- #-------------custom rc6 control----
      eventReactive(
        list(rc6tR(), input$rctreeC), #+control
        {
          if (verbose) print("enter rc6cuR")
          x <- sort(unique(c(rc6tR(), input$rctreeC))) %>% .[nchar(.) == 6]
          rc6cuG <<- copy(x)
          x
        }
      )
    
    rc6tR <- #-------------target rc6 reformat----
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
    
    rsicuR <- #-------------custom rsi compute----
      eventReactive(
        list(
          input$docusabC
        ),
        {
          if (verbose) print("enter rsicuR")
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
    
    rssaR <- #---------------area rss compute----
      eventReactive(
        nxaR(),
        {
          if (verbose) print("enter rssaR")
          x <-
            rssR()[nxaR(), on = c(nx = "nx")]
          rssaG <<- copy(x)
          x
        }
      )
    
    rsscuR <- # ------------custom rss select----
      eventReactive(
        list(
          rsicuR()
        ),
        {
          if (verbose) print("enter rsscuR")
          x <- cbind(rsicuR()$kfoldsse, rsicuR()$all)
          rsscuG <<- copy(x)
          x
        }
      )
    
    rssR <- #-----------------------------rss----
      reactive({
        x <- copy(f241021ad$rss)
        rssG <<- copy(x)
        x
      })
    
    tslideR <- # --------------control-slider----
      reactive({
        x <- input$tslider
        tslideG <<- copy(x)
        x
      })
    
    ylimR <- #--------------------------ylim ----
      eventReactive(
        estdtxR(),
        {
          x <-
            estdtxR()[, range(x)] * 1.1
          ylimG <<- copy(x)
          x
        }
      )
    
    z110R <- #---------------------------z110R----
      reactive({
        x <- copy(z110)
        z110G <<- copy(x)
        x
      })
    
    fgeocuX <- #------------custom geo compute----
      function(rc6cuX = rc6cuG) {
        x <-
          data.table(rc9 = rc6cuX, nx = 0, lab = "CU00")
        x
      }

    list( # ---------------------common list #----
      dfnxR = dfnxR,
      dfnxxR = dfnxxR,
      estdtaR = estdtaR,
      estdtcuR = estdtcuR,
      estdtlR = estdtlR,
      estdtlnewR = estdtlnewR,
      estdtR = estdtR,
      estdtxR = estdtxR,
      geo0R = geo0R,
      geoaR = geoaR,
      geocuR = geocuR,
      geoplusR = geoplusR,
      geoqR = geoqR,
      geotR = geotR,
      labxR = labxR,
      nxaR = nxaR,
      nxqR = nxqR,
      pxosrdo2ddR = pxosrdo2ddR,
      rc6cuR = rc6cuR,
      rc6tR = rc6tR,
      rsicuR = rsicuR,
      rssaR = rssaR,
      rsscuR = rsscuR,
      rssR = rssR,
      tslideR = tslideR,
      ylimR = ylimR,
      z110R = z110R
    )
  }
