server_common <-
  function(input,
           output,
           session) {
    
    # ----lib functions called in common only----
    festdty <- #------------rbind estdtcc,estdta----
    function(estdtccx = estdtccG, estdtax = estdtaG, geoccx = geoccG) {
      x <-
        rbind(
          estdtccx[, .(nx, date, xdotd, days, xdot, x, lab, ii, qtile = 0, rc3 = lab)],
          estdtax[, .(nx, date, xdotd, days, xdot, x, lab, ii, qtile, rc3)]
        )#[, qq := as.factor(qtile)]
      x
    }
    
    fgeoccx <- #------------custom geo compute----
    function(rc6ccx = rc6ccG) {
      x <-
        data.table(rc9 = rc6ccx, nx = 0, lab = "CU00")
      x
    }
    
    #----reactive----
    dfnxR <- # -----------4-col table with NA----
      reactive({
        x <-
          dcast(f241021ad$estdt[, .(tbin, date)] %>% unique(.) %>% .[order(tbin, date)], date ~ tbin, value.var = "date") %>% # lo, hi, an
          rbind(as.data.table(as.list(rep(as.Date("1994-12-31"), 4))), ., use.names = F) %>%
          setnames(., c("date", "tbin1", "tbin2", "tbin3"))
        dfnxG <<- copy(x)
        x
      })

    dfnyR <- #---------vector of current date----
      reactive({
        x <-
          dfnxR()[, paste0("tbin", tbinC), with = F] %>%
          setnames(., "x") %>%
          .[, sort(unique(x))]
        dfnyG <<- copy(x)
        x
      })

    estdtccR <- #---------custom estdt select ----
      eventReactive(
        list(
          rsiccR()
        ),
        {
          if (verbose) print("enter estdtccR")
          x <- rsiccR()$estdt
          estdtccG <<- copy(x)
          x
        }
      )

    estdtlR <- #---------local estdt compute  ----
      eventReactive(
        nxqR(),
        {
          if (verbose) print("enter estdtlR")
          x <-
            copy(f241021ad$estdt)[, .(nx, ii, date, xdotd, days, xdot, x)]%>%
            .[nxqR(), on = c(nx = "nx")] %>%
             .[, .(nx, date, ii, lab, rc3, qtile, xdotd, days, xdot, x)]
          estdtlG <<- copy(x)
          x
        }
      )

    estdtxR <- #---------------------112 x(t)----
      eventReactive(
         list(estdtccR(), nxaR(), geoccR()),
        {
          print("enter estdtxR")
          x <-
            festdty(
              estdtccx = estdtccR(), 
              estdtax = f241021ad$estdt[nxaR(), on = c(nx = "nx"), .(nx, date, ii, lab, rc3, qtile, xdotd, days, xdot, x)], #was estdtaR() 
              geoccx = geoccR()
              )
          estdtxG <<- copy(x)
          print("exit estdtxR")
          x
        }
      )

    geoaR <- #----------------area geo compute----
      eventReactive(
        rc6tR(),
        {
          if (verbose) print("enter geoaR")
          x <-
          copy(f241021ad$geoplus)%>%
          .[type == "L"] %>%
          .[itrim == itriC] %>%
          .[tbin == tbinC] %>%
          .[, .(
            nx,
            gx,
            lab,
            rc6 = rc9,
            rc3 = substr(rc9, 1, 3),
            qtile = as.numeric(substr(des, 4, 4))
          )] %>%
          z110[., on = c(rcx = "rc6")] %>%
          .[, .(nx, gx, lab, rc3, rc6 = rcx, qtile)]%>%
            .[rc3 == substr(rc6tR(), 1, 3)]
          geoaG <<- copy(x)
          x
        }
      )

    geoccR <- #-------------custom geo compute----
      eventReactive(
        rc6ccR(),
        {
          if (verbose) print("enter geoccR")
          x <-
            fgeoccx(rc6ccx = rc6ccR())
          geoccG <<- copy(x)
          x
        }
      )

    geoqR <- #----------------qtile geo select----
      eventReactive(
        list(
          geoaR(), rc6tR() # ,
        ),
        {
          if (verbose) print("enter geoqR")
          x <- geoaR() %>%
            #.[geotR()[, .(qtile)],
            .[geoaR()[rc6 == rc6tR()][, .(qtile)],
              on = c(qtile = "qtile")
            ]
          geoqG <<- copy(x)
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

    rc6ccR <- #-------------custom rc6 control----
      eventReactive(
        list(rc6tR(), input$rctreeC), #+control
        {
          if (verbose) print("enter rc6ccR")
          x <- sort(unique(c(rc6tR(), input$rctreeC))) %>% .[nchar(.) == 6]
          rc6ccG <<- copy(x)
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

    rsiccR <- #-------------custom rsi compute----
      eventReactive(
        list(
          input$docusabC
        ),
        {
          if (verbose) print("enter rsiccR")
          geox <- isolate(geoccR())
          dfnx <- isolate(dfnyR()) # source of truth
          rc6tx <- toupper(isolate(irregpcode(input$rc6tC[1])))
          rc6valid <- f241021ad$geoplus[,unique(rc9)]
          if (
            (irregpcode(regpcode(rc6tx)) == rc6tx) &
              (nchar(regpcode(rc6tx)) == 6) &
              (regpcode(rc6tx) %in% rc6valid)
          ) {
            print("recalc accepted in rsiccR")
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
            rsiccG <<- copy(x)
          } else {
            print("recalc rejected in rsiccR")
            x <- copy(rsiccG)
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

    rssccR <- # ------------custom rss select----
      eventReactive(
        list(
          rsiccR()
        ),
        {
          if (verbose) print("enter rssccR")
          x <- cbind(rsiccR()$kfoldsse, rsiccR()$all)
          rssccG <<- copy(x)
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


    list( # ---------------------common list #----
      dfnxR = dfnxR,
      dfnyR = dfnyR,
      estdtccR = estdtccR,
      estdtlR = estdtlR,
      estdtxR = estdtxR,
      geoaR = geoaR,
      geoccR = geoccR,
      geoqR = geoqR,
      labxR = labxR,
      nxaR = nxaR,
      nxqR = nxqR,
      rc6ccR = rc6ccR,
      rc6tR = rc6tR,
      rsiccR = rsiccR,
      rssaR = rssaR,
      rssccR = rssccR,
      rssR = rssR,
      tslideR = tslideR,
      ylimR = ylimR
    )
  }
