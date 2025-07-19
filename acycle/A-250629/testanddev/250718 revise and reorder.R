#add custom blobs under local, same gridcard or add one

D4311b <- #--------blobs for rc3/rc6 : R4311x ----
  function(
      rc6tx = rc6tG,
      rc6cx = rc6cG,
      x1 = C4311d(rc6 = rc6tx), # returns custom
      cols_to_paint = 'q0',
       shadecol1 = "#D3D3D3", # matches grid
      symbolsize = ".8em") {
    x1[, blobsuppress := F] # all get blobs
    x1 %>%
      .[order(-ppm2)]%>%
      gt::gt() %>%
      # ---- q0 with conditional outline ----
      gt::text_transform(
        locations = gt::cells_body(columns = "q0"),
        fn = function(hexvec) {
          vapply(seq_along(hexvec), function(i) {
            val <- hexvec[i]
            row_rc6 <- x1$rc6[i]
            outline <- "" #if (row_rc6 %in% rc6cx) "border:4px solid black;" else ""
            paste0(
              "<div style='display:inline-block; width:", symbolsize,
              "; height:", symbolsize,
              "; background-color:", val,
              "; border-radius:50%; ", outline, "'></div>"
            )
          }, character(1))
        }
      ) %>%
      cols_label(
        rc6 = gt::html("postcode"),
        locality = gt::html("name"),
        ppm2 = gt::html("£/m<sup>2</sup>"),
        nid = gt::html("properties"),
        q0 = gt::html("")
      ) %>%
      tab_spanner(
        label = gt::html("custom"),
        columns = c(rc6, locality, ppm2, nid)
      ) %>%
      cols_move_to_start(
        columns = c("q0")
      ) %>%
      cols_move_to_start(
        columns = c("locality")
      ) %>%
      cols_move_to_start(
        columns = c("rc6")
      ) %>%
      cols_move_to_start(
        columns = c("nid")
      ) %>%
      cols_move_to_start(
        columns = c("ppm2")
      ) %>%
      fmt_number(
        columns = c(ppm2, nid),
        decimals = 0,
        sep_mark = "," # Thousands separator
      ) %>%
      cols_hide(columns = blobsuppress) %>%
      gt::tab_style(
        style = gt::cell_fill(color = shadecol1),
        locations = gt::cells_body(rows = rc6 == rc6tx)
      )
  }
D4311b()

#---------------------------------------junk







rc6tG <- 'SW-7--'
D4311a( #--------blobs for rc3/rc6 : R4311x ----
      rc6tx = rc6tG,
      rc6cx = rc6cG,
      x1 = C4311b(rc6 = rc6tx), # it returns entire rc3  <<<<replace this with a call to generalised (C4311b) which exposes geo,and adapt the idiom:
      # ageo()[grep(paste0('^C',substr(rc6tx,1,3)),lab)][,.(rc3tpeer=sort(unique(rc6)))]
      # actually, have C4311b a second function that just returns rc6,locality,ppm2,nid for the exotic peers
      # then C4311c combines this with C4311a, adding the colors in q0 using the range in C4311a, setting q1,2,3 blank and a logical column 'blobsuppress'
      # the argument x1 above is defaulted to C4311c() instead of C4311a()
      
      
      #cols_to_paint = names(x1)[5:8],
      shadecol1 = "#D3D3D3", # matches grid
      shadecolblock = "#D3D3D3", ## EEEEFF nice pale blue
      symbolsize = ".8em")


#---------------------------------------junk

#table x3 from C4311c() is : all this rc3t, plus custom added peers - no longer want this

#instead : all this rc3t, plus rc3 it references in custom peers
#restrict scope to custom for now, then do local

#custom 
#fill all rc3 referenced in custom
#4100-district - dot-outline rc3t, black-outline rc6t, gray-outline rc6c, color=intrinsic P(rc6) (all different)
#4200-index - dot-outline nil, black-outline rc6t, gray-outline rc6c, color=custom-aggregate P(rc6) (all different)

#local
#4100-district - dot-outline rc3t, black-outline rc6t, gray-outline rc6-qtile, color=intrinsic P(rc6)  (all different)
#4200-index - dot-outline nil, black-outline rc6t, gray-outline rc6c, color= P(rc6) (6 only)

#total 4 maps
#supply f240810b with: x1[,.(col,rc6)]
#add dotoutline, blackoutline, grayoutline

#calc C functions return data:
  #for 4 perms: (custom, local) / (district, index)
    # 4 objects for map
    # f240810b(x1) where colors(x1)
    # black solid data
    # black dash data
    # gray solid data
#display D function returns leaflet

#C1000(index=c('custom','local'), aggreg=('district','index'))


C1000 <-
  function(
    rc6tx=rc6tG, 
    x1=apol(datS), #static rc6 polygon
    x2=apva(resS), #static pva
    x3=C4311c(rc6tx=rc6tx), #{rc6 locality  ppm2 nid q0 q3 q2 q1} blobs data
    x4=pxosrdo1dd, # rc3 polygon
    minzoom=9,
    maxzoom=13,
      index = c("custom", "local"),
      aggreg = c("district", "index")) {
    if (index == "custom") {
      if (aggreg == "district") {
        #colors
               
        #black solid
        #black dash
        #gray solid
      }
    }
  }

D1000 <-
  function(
    rc6tx=rc6tG, 
    x1=apol(datS), #static rc6 polygon
    x2=apva(resS), #static pva
    x3=C4311c(rc6tx=rc6tx), #{rc6 locality  ppm2 nid q0 q3 q2 q1} blobs data
    x4=pxosrdo1dd, # rc3 polygon
    minzoom=9,
    maxzoom=13,
      index = c("custom", "local"),
      aggreg = c("district", "index")) {
    if (index == "custom") {
      if (aggreg == "district") {
        #colors
        
        #black solid
        #black dash
        #gray solid
      }
    }
  }



#----------------------------------------------------junk
D4312a <-  #--------leaflet for rc3  : R4311x ----
  function(
    rc6tx=rc6tG, 
    x1=apol(datS), #static rc6 polygon
    x2=apva(resS), #static pva
    x3=O(rc6tx=rc6tx), #{rc6 locality  ppm2 nid q0 q3 q2 q1} blobs data
    x4=pxosrdo1dd, # rc3 polygon
    minzoom=9,
    maxzoom=13
  ){
    x5 <- 
      x4[which(x4@data$name == irregpcode(substr(rc6tx,1,3))),]
    x6 <- 
      x1[which(x1@data$name == irregpcode(rc6tx)),]
    x7 <- 
      f240810b( #->leaflet, colours for areas-to-shade in column 'col'
        x3[,.(col=q0,rc6)],
        x2 = x1, # map polygons
        pva = resS$pva[, .(rcx = rc6, ppm2 = pv / m2)], # for tooltip
        minzoom = minzoom,
        maxzoom = maxzoom
      )%>%
      addPolygons( # outline custom districts
        data = x5,
        fill = F,
        color='black',
        dashArray = "5,5",
        weight = 1,
        opacity = 1
      )%>%
      addPolygons( # outline custom districts
        data = x6,
        fill = F,
        color='#888888',
        #dashArray = "5,5",
        weight = 1,
        opacity = 1
      )
    x7
  }
C


f240810b <-
  function( #leaflet special/custom function for index app, copied from anest.shiny/acycle/applib.R
    x1=
      data.table(
        rc6=sort(rc6),
        col=rep(mycols,length(rc6))[1:length(rc6)] #should be meaningful
      ),
    rc6=c('NG-1--','S--10-','SE-25-'),
    mycols=cobalt(),#only to generate x1
    x2=pxosrdo2dd,#rc6 SPDF
    pva=z110, #pva: for labelling with £/m2
    zoomx=6.5,
    minzoom=6,
    maxzoom=11,
    width=1000*.7,
    height=1000*.7,
    w1=1,
    l1=.08,
    addlegend=F,
    uproj3= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"%>%CRS(.),
    palx=leaflet::colorFactor(
      palette=x1[,col],
      domain=x1[,rc6]
    ) 
  ){
    x1[,stopifnot(all(nchar(rc6)==6))]
    x1 <- #alpha sort rc
      x1[order(rc6)] 
    x2 <- #alpha sort pc
      x2[order(x2@data$name),] 
    x3 <- #SPDF subset using [
      x1[,grepstring(rc6,caret=T,dollar=T)]%>%
      grep(.,regpcode(x2@data$name))%>%
      sort(.)%>%#retain alpha sort
      x2[.,]
    { #check
      all.equal(
        x1[,rc6], #requested
        regpcode(x3@data$name) #spdf
      )%>%
        stopifnot(.) #or fail
    }
    x3@data <- #assign cols
      x1[,.(rc6)]%>%
      data.frame(.)
    x4 <- #tooltip labels
      x1[,
         paste0(
           x1[,irregpcode(rc6)],
           ' ',
           prettyNum(as.integer(pva[x1,round(ppm2,-1),on=c(rcx='rc6')]), big.mark=",", scientific=FALSE)
         )
      ]%>%
      lapply(.,htmltools::HTML)
    x5 <- #leaflet
      leaflet(
        x3,
        width=width,
        height=height,
        options=leafletOptions(
          zoomControl=F,
          minZoom=minzoom,
          maxZoom=maxzoom,
          zoomDelta=2
        )
      )
    x6 <- #colour it
      x5 %>% 
      addPolygons(
        stroke=T,
        fillColor = ~palx(rc6),#~palx(col),
        smoothFactor = 0, #remove white slivers
        weight = 1,
        opacity = 1,
        color = "transparent",#"steelblue",
        dashArray = "2",
        fillOpacity = .5,
        label = x4,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "12px",
          direction = "auto")
      )%>%
      addProviderTiles(providers$CartoDB.Positron)
    x6
  }






#--------------------------------junk

#new numbering
#4100 rc6 level map and blobs
#4200 index 'timeseries'
#4300 listing


#4111 leaflet
rc6tx=rc6tG



D4312a(   #--------leaflet for rc3  : R4311x ----
    rc6tx=rc6tG,
    x1=apol(datS), # rc6 polygon
    x2=apva(resS), # pva
    x3=C4311c(rc6tx=rc6tx), #rc6 for this rc3, augmented with out of area peers
    x4=pxosrdo1dd, # rc3 polygon
    minzoom=9,
    maxzoom=13
  )
#C4311c calls C4311b and C4311a
#can generalise to take rc3?  then almost done in one
#remember it all is reactive - much updates with rc6t


#4112 blobs

