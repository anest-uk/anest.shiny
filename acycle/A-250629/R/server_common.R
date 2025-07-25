server_common <-
  function(input,
           output,
           session) {
    verbose <- T

    # ----lib functions called in common only----
    festdty <- #------------rbind estdtc,estdta----
      function(estdtcx = estdtcG, estdtax = estdtaG, geocx = geocG) {
        x <-
          rbind(
            estdtcx[, .(nx, date, xdotd, days, xdot, x, lab, ii, qtile = 0, rc3 = lab)],
            estdtax[, .(nx, date, xdotd, days, xdot, x, lab, ii, qtile, rc3)]
          ) # [, qq := as.factor(qtile)]
        x
      }

    fgeocx <- #------------custom geo compute----
      function(rc6cx = rc6cG) {
        x <-
          data.table(rc9 = rc6cx, nx = 0, lab = "CU00")
        x
      }


    #----reactive----
    dfnxR <- # -----------4-col table with NA----
      reactive({
        if (verbose) print("enter common-dfnxR")
        x <-
          dcast(f241021ad$estdt[, .(tbin, date)] %>% unique(.) %>% .[order(tbin, date)], date ~ tbin, value.var = "date") %>% # lo, hi, an
          rbind(as.data.table(as.list(rep(as.Date("1994-12-31"), 4))), ., use.names = F) %>%
          setnames(., c("date", "tbin1", "tbin2", "tbin3"))
        dfnxG <<- copy(x)
        x
      })

    dfnyR <- #---------vector of current date----
      reactive({
        if (verbose) print("enter common-dfnyR")
        x <-
          dfnxR()[, paste0("tbin", tbinC), with = F] %>%
          setnames(., "x") %>%
          .[, sort(unique(x))]
        dfnyG <<- copy(x)
        x
      })

    geoaR <- #----------------area geo compute----
      eventReactive(
        rc6tR(),
        {
          if (verbose) print("enter common-geoaR")
          x <-
            copy(f241021ad$geoplus) %>%
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
            .[, .(nx, gx, lab, rc3, rc6 = rcx, qtile)] %>%
            .[rc3 == substr(rc6tR(), 1, 3)]
          geoaG <<- copy(x)
          x
        }
      )

    geocR <- #-------------custom geo compute----
      eventReactive(
        rc6cR(),
        {
          if (verbose) print("enter common-geocR")
          x <-
            fgeocx(rc6cx = rc6cR())
          geocG <<- copy(x)
          if (verbose) print("exit common-geocR")
          x
        }
      )

    rc6cR <- #-------------custom rc6 accessor----
      eventReactive(
        list(rc6tR(), input$rc6cC), #+control
        {
          if (verbose) print("enter common-rc6cR")
          x <-
            sort(unique(c(rc6tR(), input$rc6cC))) %>% # combine
            .[ # test
              (nchar(.) == 6) &
                (substr(., 3, 3) == "-") &
                (substr(., 6, 6) == "-")
            ] # return all
          rc6cG <<- copy(x)
          x
        }
      )

    rc6tR <- #-------------target rc6 accessor----
      eventReactive(
        input$rc6tC,
        {
          if (verbose) print("enter common-rc6tR")
          x <-
            input$rc6tC %>%
            .[ # test
              (nchar(.) == 6) &
                (substr(., 3, 3) == "-") &
                (substr(., 6, 6) == "-")
            ] %>%
            .[1] # return first
          rc6tG <<- copy(x)
          x
        }
      )

    rescR <- #-------------custom rsi compute----
      eventReactive(
        list(
          input$docusabC
        ),
        {
          if (verbose) print("enter common-rescR")
          geox <- isolate(geocR())
          dfnx <- isolate(dfnyR()) # source of truth
          # rc6tx <- toupper(isolate(irregpcode(input$rc6tC[1])))
          rc6tx <- toupper(isolate(irregpcode(rc6tR())))
          rc6valid <- f241021ad$geoplus[, unique(rc9)]
          if (
            (irregpcode(regpcode(rc6tx)) == rc6tx) &
              (nchar(regpcode(rc6tx)) == 6) &
              (regpcode(rc6tx) %in% rc6valid)
          ) {
            print("recalc accepted in rescR")
            x1 <-
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
            x <- copy(x1)
            rescG <<- copy(x)
          } else {
            print("recalc rejected in rescR")
            x <- copy(rescG)
          }

          x
        }
      )

    rescxR <- # custom result gen2
      eventReactive(
        list(
          rescR()
        ),
        {
          if (verbose) print("enter common-rescxR")
          x <-
            Ccus(
              rescx = rescR(),
              pvax = resS$pva
            )
          rescxG <<- x
          x
        }
      )

    tslideR <- # --------------control-slider----
      reactive({
        if (verbose) print("enter common-tslideR")
        x <- input$tslider
        tslideG <<- copy(x)
        x
      })

    #---- gen2 accessors ----

    list( # ---------------------common list #----
      dfnxR = dfnxR,
      dfnyR = dfnyR,
      geoaR = geoaR,
      geocR = geocR,
      rescxR = rescxR,
      rc6cR = rc6cR,
      rc6tR = rc6tR,
      rescR = rescR,
      tslideR = tslideR 
    )
  }
