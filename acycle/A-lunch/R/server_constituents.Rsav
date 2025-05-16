server_constitutents <-
  function(
      input, output, session, common
      ) {
    f311D <- function(
        geo0X = geo0G,
        z110X = z110G,
        rc6tX = rc6tG) # 311 constituents----
    {
      if (verbose) print("enter 311")
      x1 <-
        geo0X[, .(rc3, rc6, qtile)] %>%
        z110X[., on = c(rcx = "rc6")] %>%
        .[, .(rc3, rc6 = rcx, nid, ppm2 = round(ppm2), quantile = paste0("local-", qtile))]
      x <-
        DT::datatable(
          x1,
          options = list(
            search = list(search = rc6tX),
            columnDefs = list(list(className = "dt-center", targets = 1:4, searchable = F, targets = 3:5)),
            paging = T,
            pageLength = 100,
            initComplete = JS(
              "function(settings, json) {",
              "$('body').css({'font-family': 'Calibri'});",
              "}"
            )
          ),
          rownames = F
        ) %>%
        DT::formatStyle(0, target = "row", lineHeight = "70%")
      x311G <<- copy(x)
      x
    }

    x311D <- eventReactive(
      list(common$geo0R(), common$z110R(), rc6tX = common$rc6tR()), # 311 custom constituents output table----
      {
        if (verbose) print("enter 311")
        x <- f311D(
          geo0X = common$geo0R(),
          z110X = common$z110R(),
          rc6tX = substr(common$rc6tR(), 1, 3) # arbitrarily initialise it to the area
        )
        x311G <<- copy(x)
        x
      }
    )
    output$x311 <- DT::renderDT(x311D())
  }
