
ageo(resS)
apva(resS)


D311x <- function( #--'----311 constituents----
                  geoax = geoaG, # add to this to give more tiles
                  pvax = apva(resS),
                  rc6tx = rc6tG) {
  if (verbose) print("enter 311")
  x1 <-
    geoax[, .(rc3, rc6, qtile)] %>%
    pvax[., on = c(rc6 = "rc6")] %>%
    .[, .(rc3, rc6, nid, ppm2 = round(ppm2), quantile = paste0("local-", qtile))]
  x <-
    gt(x1)
    # DT::datatable(
    #   x1,
    #   options = list(
    #   #    search = list(search = rc6tx),
    #   #   columnDefs = list(list(className = "dt-center", targets = 1:4, searchable = F, targets = 3:5)),
    #   #   paging = T,
    #   #   pageLength = 100,
    #   #   initComplete = JS(
    #   #     "function(settings, json) {",
    #   #     "$('body').css({'font-family': 'Calibri'});",
    #   #     "}"
    #   #   )
    #   #),
    #   rownames = F
    # ) %>%
    # DT::formatStyle(0, target = "row", lineHeight = "70%")
  #G311 <<- copy(x)
  x
}
D311x()
C311a <-
  function(
      statics = "resS",
      rc6tx=rc6tG
      ) {
    x1 <-
      resS$geo %>%
      .[resS$lab[grep("^L", lab)][grep(substr(rc6tx, 1, 3), lab)], on = c(nx = "nx")] %>%
      .[apva(resS)[, .(rc6, rc6P = log(pv / m2), pv, m2, rc6ppm2=round(pv/m2),rc6nid=nid)], on = c(rc6 = "rc6"), nomatch = NULL]%>%
      .[,rc6col:=color_price(rc6P,min(rc6P),max(rc6P))]
    x1[]
    x2 <- 
      x1%>%
      .[,.(grpppm2=sum(pv)/sum(m2)),.(lab,nx)]%>%
      .[,.(grpppm2,lab,nx,grpP=log(grpppm2))]%>%
      .[,.(grpppm2,lab,nx,grpP,grpcol=color_price(grpP,x1[,min(rc6P)],x1[,max(rc6P)]))]%>%
      .[,.(grpppm2,lab=paste0(substr(lab,1,4),'xx',substr(lab,7,9)),nx,grpP,grpcol)]
    x2[,]
    
    x3 <- 
      x2[,.(nx,lab,grpcol,grpppm2)]%>%
      .[x1[,.(nx,rc6,rc6nid,rc6ppm2,rc6col)],on=c(nx='nx'),mult='first']%>%
      .[order(nx,rc6ppm2)]
    x3[]
    x4 <- 
      x3 %>%
      dcast(., rc6+rc6ppm2+rc6nid+rc6col ~ lab, value.var = "grpcol")%>%
      .[order(-rc6ppm2)]#%>%
      #.[,c('rc6','rc6ppm2','rc6nid','rc6col',x2[order(grpppm2),lab]),with=F]
    x4[]
    
    
    #x5 <- data.table(old=names(x4),new=letters[1:ncol(x4)])
    #setnames(x4,x5[,old],x5[,new])
    setnames(x4,c('rc6','ppm2','nid','none','1','2','3'))
    x4
  }
rc6tx <- 'NG-7--'
x <- C311a(rc6=rc6tx)
x
#x <- as.data.frame(D311y())
#dump('x',file='')


D411b <- function(
    rc6tx = rc6tG,
    x1 = C311a(rc6=rc6tx),
    cols_to_paint = names(x1)[4:7],
    shadecol1 = "#DDDDFF",
    shadecolblock = "#EEEEFF",
    symbolsize = ".8em"
) {
  x1 %>%
    gt::gt() %>%
    # Show colored disks in specified columns
    gt::text_transform(
  locations = gt::cells_body(columns = all_of(cols_to_paint)),
  fn = function(hexvec) {
    vapply(hexvec, function(val) {
      paste0(
        "<div style='display:inline-block; width:", symbolsize, 
        "; height:", symbolsize, 
        "; background-color:", val, 
        "; border-radius:50%;'></div>"
      )
    }, character(1))
  }
    )%>%
    # Highlight selected row by rc6 value
    gt::tab_style(
      style = gt::cell_fill(color = shadecol1),
      locations = gt::cells_body(
        rows = x[, rc6 %in% ageo(areso(rc6tx))[, rc6]],
        columns = areso(rc6tx)$lab[, substr(lab, 7, 7)]
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_fill(color = shadecol1),
      locations = gt::cells_body(
        rows = rc6 == rc6tx
      )
    )
  # Highlight other related cells based on areso logic
}
D411b(rc6=rc6tx,symbolsize='1em')


        columns = areso(rc6tx)$lab[, substr(lab, 7, 7)]



D411a <- function(
    x1 = C311a(),
    rc6tx = rc6tG,
    cols_to_paint = names(x1)[4:7],
    shadecol = "grey92",
    symbolsize = ".8em"
) {
  x1 %>%
    gt::gt() %>%
    # Show colored disks in specified columns
    gt::text_transform(
  locations = gt::cells_body(columns = all_of(cols_to_paint)),
  fn = function(hexvec) {
    vapply(hexvec, function(val) {
      paste0(
        "<div style='display:inline-block; width:", symbolsize, 
        "; height:", symbolsize, 
        "; background-color:", val, 
        "; border-radius:50%;'></div>"
      )
    }, character(1))
  }
)%>%
    # Highlight selected row by rc6 value
    gt::tab_style(
      style = gt::cell_fill(color = shadecol),
      locations = gt::cells_body(rows = rc6 == rc6tx)
    ) %>%
    # Highlight other related cells based on areso logic
    gt::tab_style(
      style = gt::cell_fill(color = shadecol),
      locations = gt::cells_body(
        rows = x[, rc6 %in% ageo(areso(rc6tx))[, rc6]],
        columns = areso(rc6tx)$lab[, substr(lab, 7, 7)]
      )
    )
}
D411a(symbolsize='.5em')

##<<not working
D411a <- 
  function(
    x1=C311a(),
    rc6tx=rc6tG,
    cols_to_paint=names(x1)[4:7],
    shadecol="grey92",
    squaresize=".8em"
  ) {
    x1 %>%
  gt::gt(.) %>%
  gt::text_transform(
    locations = cells_body(columns = all_of(cols_to_paint)),
    fn = function(hexvec) {
      htmltools::HTML(paste0(
        "<div style='display:inline-block; width:", squaresize, 
        "; height:", squaresize, 
        "; background-color:", hexvec, 
        "; border-radius:50%;'></div>"
      ))
    }
  ) %>%
      gt::tab_style(
        style = cell_fill(color = shadecol),
        locations = cells_body(rows = rc6==rc6tx)
      ) %>%
      gt::tab_style(
        style = cell_fill(color = shadecol),
        locations = cells_body(rows = x[,rc6%in%ageo(areso(rc6tx))[,rc6]], columns = areso(rc6tx)$lab[,substr(lab,7,7)])
      )
  }
D411a()





D411a <- 
  function(
    x1=C311a(),
    rc6tx=rc6tG,
    cols_to_paint=names(x1)[4:7],
    shadecol="grey92",
    squaresize=".8em"
  ) {
    x1 %>%
      gt::gt() %>%
      gt::text_transform(
        locations = cells_body(columns = all_of(cols_to_paint)),
        fn = function(hexvec) {
          purrr::map_chr(hexvec, function(val) {
            if (!is.na(val)) {
              paste0(
                "<div style='display:inline-block; width:1em; height:",squaresize,"; width:",squaresize,"; background-color:", val, 
                "; border:0px solid black;'></div>"
              )
            } else {
              ""
            }
          }) %>% 
            purrr::map(HTML)
        }
      ) %>%
      gt::tab_style(
        style = cell_fill(color = shadecol),
        locations = cells_body(rows = rc6==rc6tx)
      ) %>%
      gt::tab_style(
        style = cell_fill(color = shadecol),
        locations = cells_body(rows = x[,rc6%in%ageo(areso(rc6tx))[,rc6]], columns = areso(rc6tx)$lab[,substr(lab,7,7)])
      )
  }
D411a(square='.6em')
#---------------junk
names(resS)
resS$geo[resS$]


      x11 <- 
      resS$geo %>%
      .[resS$lab[grep("^L", lab)][grep(substr(rc6tG, 1, 3), lab)], on = c(nx = "nx")] %>%
        dcast(.,rc6~lab,value.var='nx')%>%
        setnames(.,c('rc6',letters[1:(ncol(.)-1)]))
      
      x12 <- 
      x2[,grep('^L',names(x2))[1],with=F]%>%unlist(.)
      x13 <- 
        color_price(x12,min(x12),max(x12))
      x11%>%
        .[,col:=x13]
      x11
      
    
      
x1 <- copy(x11)%>%
      .[apva(resS)[, .(rc6, ppm2 = round(pv / m2, -1), pv, m2)], on = c(rc6 = "rc6"), nomatch = NULL]%>%
  .[order(pv/m2)]

x1 %>%
  gt::gt() %>%
  text_transform(
    locations = cells_body(columns = a:f),
    fn = function(value_vec) {
      purrr::map2_chr(value_vec, seq_along(value_vec), function(val, i) {
        if (!is.na(val) && val != "NA") {
          colval <- x1$col[i]
          paste0(
            "<div style='display:inline-block; width:1em; height:1em; background-color:", colval, ";'></div>"
          )
        } else {
          ""
        }
      }) %>%
        purrr::map(htmltools::HTML)
    }
  )



###################################
x1 %>%
  gt::gt(.) %>%
  text_transform(
    locations = cells_body(columns = a:f),
    fn = function(value_vec) {
      purrr::imap_chr(value_vec, function(val, i) {
        colval <- x1$col[i]
        if (!is.na(val)) {
          paste0(
            "<div style='display:inline-block; width:1em; height:1em; background-color:", colval, ";'></div>"
          )
        } else {
          ""  # empty cell
        }
      }) %>% 
        purrr::map(htmltools::HTML)
    }
  )        


x1 %>%
  gt::gt(.) %>%
  text_transform(
    locations = cells_body(columns = a:f),
    fn = function(value_vec) {
      row_nums <- seq_along(value_vec)
      purrr::map_chr(row_nums, function(i) {
        val <- value_vec[i]
        colval <- x1$col[i]
        if (!is.na(val)) {
          paste0(
            "<div style='display:inline-block; width:1em; height:1em; background-color:", colval, ";'></div>"
          )
        } else {
          ""
        }
      }) %>%
        purrr::map(htmltools::HTML)
    }
  )
        

outline <- "border:1px solid black"

x1 %>%
  gt::gt() %>%
  text_transform(
    locations = cells_body(columns = a:f),
    fn = function(value_vec) {
      # value_vec is the column's values (1 value per row)
      purrr::map2_chr(value_vec, seq_along(value_vec), function(val, i) {
        if (!is.na(val)) {
          colval <- x1$col[i]
          paste0(
            "<div style='display:inline-block; width:1em; height:1em; background-color:", colval, "; ", outline, ";'></div>"
          )
        } else {
          ""  # blank cell
        }
      }) %>%
        purrr::map(htmltools::HTML)
    }
  )

x1 %>%
  gt::gt() %>%
  text_transform(
    locations = cells_body(columns = a:f),
    fn = function(value_vec) {
      print(value_vec)  # <- check what’s really coming in
      purrr::map2_chr(value_vec, seq_along(value_vec), function(val, i) {
        if (!is.na(val) && val != "") {
          colval <- x1$col[i]
          paste0(
            "<div style='display:inline-block; width:1em; height:1em; background-color:", colval, ";'></div>"
          )
        } else {
          ""
        }
      }) %>%
        purrr::map(htmltools::HTML)
    }
  )


        
      cols_label(
        q1 = gt::html("rank"),
        q2 = gt::html("segment"),
        col = gt::html(""),
        min = gt::html("min"),
        mean = gt::html("mean"),
        tot = gt::html("total"),
        pa = gt::html("p.a."),
        max = gt::html("max"),
        stdev = gt::html("volatility"),
        skew = gt::html("skew"),
        kurtosis = gt::html("kurtosis"),
        agg = gt::html("£/m<sup>2</sup>"),
        key = gt::html(""),
      ) %>%
      fmt_number(
        columns = agg,
        decimals = 0,
        sep_mark = "," # Thousands separator
      ) %>%
      fmt_number(
        columns = c("skew", "kurtosis"),
        decimals = 2,
        sep_mark = "," # Thousands separator
      ) %>%
      tab_spanner(
        label = gt::html(aestdt2(resS)$BA %>% .[c(tslidex + 1, length(.))] %>% paste0(., collapse = " - ") %>% paste0("log returns : ", .)),
        columns = c(min, mean, tot, pa, max, stdev, skew, kurtosis)
      ) %>%
      tab_spanner(
        label = gt::html("quantiles"),
        columns = c(q1, q2)
      ) %>%
      text_transform(
        locations = cells_body(columns = key),
        fn = function(codes) {
          purrr::imap_chr(codes, function(code, i) {
            colval <- x1[i, col]
            is_target <- (x1[i, q2 == "custom"]) # <- change this condition as needed
            outline <- if (is_target) "border:4px solid black;" else ""
            paste0(
              "<div style='display:inline-block; width:1em; height:1em; background-color:", colval, "; ",
              outline, "'></div>"
            )
          }) %>%
            purrr::map(htmltools::HTML)
        }
      ) %>%
      cols_hide(columns = col) %>%
      cols_label(key = "") %>%
      cols_move_to_start(
        columns = c("key")
      )
      
      
    x2 <-
      x1 %>%
      dcast(., rc6 ~ lab, value.var = "ppm2")
    x3 <-
      x1[, .(ppm2 = sum(pv) / sum(m2)), lab] %>%
      .[order(ppm2), lab]%>%
      c('rc6',.)
    x4 <-
      x2 %>%
      .[order(x2[, 2])] %>%
      .[,x3, with = F]
    x4

