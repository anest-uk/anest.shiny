#---server   section
server <-
  function(input, output) {
    #behaviour of JS testing depends on location of these lines so do not move without testing
    #--------------------------------------output
    output$selrc6 <- renderText({
      paste0('selected:   ', selxrc6stringR())
    })
    
    output$comrc6 <- renderText({
      paste0('computed: ', compxrc6R())
    })
    output$defrc6 <- renderText({
      paste0(c('suggested: ', rc6deR()), collapse = '')
    })
    output$selrc6forjstest <- renderText({
      selxrc6stringR()
    })
    output$comrc6forjstest <- renderText({
      compxrc6R()
    })
    output$cuseqcom <- renderText({
      'bespoke index matches selection'
    })
    output$cusnecom <- renderText({
      'Recalc for selected districts'
    })
    
    output$x111  <- renderLeaflet(x111D())
    output$x112  <- renderPlot(x112D())
    output$x121a <- gt::render_gt(x121D()[[1]])
    output$x121b <- gt::render_gt(x121D()[[2]])
    output$x122  <- gt::render_gt(x122D())
    output$x131  <- gt::render_gt(x131D())
    output$x132a <- gt::render_gt(x132D()[['local']][[1]])
    output$x132b <- gt::render_gt(x132D()[['local']][[2]])
    output$x132c <- gt::render_gt(x132D()[['custom']][[1]])
    output$x132d <- gt::render_gt(x132D()[['custom']][[2]])
    output$x411  <- gt::render_gt(x411D())
    output$x421  <- gt::render_gt(x421D())
    output$x431  <- gt::render_gt(x431D())
    output$x412  <- gt::render_gt(x412D())
    output$x422  <- gt::render_gt(x422D())
    output$x432  <- gt::render_gt(x432D())
    output$x211  <- gt::render_gt(x211D())
    output$x211cu <- gt::render_gt(x211cuD())
    output$x311  <- DT::renderDT(x311D())
    
    
    #output$x4xx <- gt::render_gt(x4xxD())
    #--------------------------------------control
    
    
    #-------------------------------------control cleaned 
    #---select=target+bespoke
    #selxrc6R <-  eventReactive(list(rccuR(), rctaR()), {--------------------------------
    selxrc6R <-  eventReactive(list(rccuR(), rctaR()), {
      if (verbose) {print('X - enter selxrc6R')   }
      x0 <-   #target (always must be included)
        union(rccuR(), rctaR()) %>%
        unique(.) %>%
        sort(.)
      x1 <- #exclude non-rc6 higher tree nodes
        x0[which((nchar(x0) == 6) &
                   (substr(x0, 3, 3) == '-'))]
      selxrc6G <<- copy(x1)
      x1 #vector of union(tree1,tree2)
    })
    
    #--------------------------------------control message pasteup
    #--- computed
    #compxrc6R <-  eventReactive(list(rsicuR()), {--------------------------------
    compxrc6R <-  eventReactive(list(rsicuR()), {
      if (verbose) {print('X - enter compxrc6R')}
      rsicuX <- rsicuR()
      x1 <- rsicuX$kfoldsse[, rc6] %>% .[nchar(.) == 6]
      x <- paste0(paste0(sort(unique(x1)), collapse = ','))
      compxrc6G <<- copy(x)
      x
    })
    #--- recommended
    #--- selected
    #selxrc6stringR <- eventReactive(list(selxrc6R()), {-----------------------------------------
    selxrc6stringR <- eventReactive(list(selxrc6R()), {
      if (verbose) {  print('X - enter selxrc6stringR')  }
      x1 <- selxrc6R()
      x <- paste0(paste0(x1, collapse =
                           ',')) 
      selxrc6stringG <<- copy(x)
      x
    })
    
    #-------------------------------------table from list - not needed
    #rssR  <-     eventReactive(list(x00R()), {-----------------------------------
    rssR  <-     eventReactive(list(x00R()), {
      if (verbose) {print('X - enter rssR') }
      x <- copy(x00R()$rss)
      rssG <<- copy(x)
      x
    })
    
    #------------------------------------reactive from global - not needed
    #pxosrdo2ddR <- reactive({-------------------------------------------------------
    pxosrdo2ddR <- reactive({
      if (verbose) {print('X - enter pxosrdo2ddR') }
      x <- copy(pxosrdo2dd)
      pxosrdo2ddG <<- copy(x)
      x
    })
    
    #------------------------------------display function
    #---bespoke: geo
    #fgeocuX <-  function(rccuX = rccuG) {---------------------------------------------
    fgeocuX <-  function(rccuX = rccuG) {
      if (verbose) {print('enter fgeocuX')}
      x <-
        data.table(rc9 = rccuX, nx = 0, lab = 'CU00')
      x
    } #construct geo from bespoke
    #---
    #festdtxX <- function(estdtcuX = estdtcuG,-------------------------------------
    festdtxX <- function(estdtcuX = estdtcuG,
                         estdtaX = estdtaG,
                         geocuX = geocuG) {
      if (verbose) {print('X - enter festdtxX') }
      x <-
        rbind(estdtcuX[, .(nx,
                           date,
                           xdotd,
                           days,
                           xdot,
                           x,
                           lab,
                           ii,
                           qtile = 0,
                           rc3 = lab)]
              , estdtaX [, .(nx, date, xdotd, days, xdot, x, lab, ii, qtile, rc3)])[, qq := as.factor(qtile)]
      x
    } #rbind estdt ter and cus - not needed
    
    #f111D <- function(rctaX = rctaG--------------------------------------
    f111D <- function(rctaX = rctaG,
                      rccuX = rccuG,
                      geoaX = geoaG,
                      pxosrdo2ddX = pxosrdo2ddG,
                      z110X = z110G,
                      colX = colx,
                      minzoom = 9,
                      lightx = .7) {
      if (verbose) {print('X - enter f111D') }
      x <-
        geoaX %>%
        .[, .(rc6, col = lighten(colX, lightx)[.BY[[1]]], ### capital in colX <<<<<<<<<<<<<<<<<<<<
              qtile, #shade tiles light
              lab), by = .(qtile)] %>%
        .[rc6 == rctaX, #with target district darker
          col := colX[.BY[[1]]], by = .(qtile)] %>% .[] %>%
        f240810b(
          #->leaflet, colours for areas-to-shade in column 'col'
          .,
          x2 = pxosrdo2ddX,
          pva = z110X,
          minzoom = minzoom,
          maxzoom = 12
        ) %>%
        addPolygons(
          #outline bespoke districts
          data = pxosrdo2ddX[which(pxosrdo2ddX@data$name %in% irregpcode(rccuX)), ],
          fill = F,
          color = "orange",
          weight = 1,
          opacity = 1
        )
      x
    } #map(tar, ter, cus/bes)
    
    #f112D <- function(tslideX = tslideG,-------------------------------------------
    f112D <- function(tslideX = tslideG,
                      estdtxX = estdtxG,
                      ylimX = ylimG,
                      geocuX = geocuxG) {
      if (verbose) {print('X - enter f112D')}
      
      x2c <-
        estdtxX %>%
        .[, .SD[, .(ii, date, lab, x = x - ifelse(tslideX == 0, 0, x[tslideX]))], .(qtile)] %>%
        .[, .SD[, .(ii, date, lab, x)], .(qtile)] %>%
        .[, qq := as.factor(qtile)] %>%
        .[, labx := ifelse(date == max(date), lab, NA)]
      x0 <- setNames(cobalt()[c('punk', 'green', 'blue')], as.character(1:3))
      x3 <- estdtxX[, .SD[, .(ifelse(tslideX == 0, 0, x[tslideX]))], .(qtile)][, mean(V1, na.rm =
                                                                                        T)]#base value for rebase level
      x2 <-
        estdtxX[, .SD[, .(ii, date, lab, x = x - ifelse(tslideX == 0, 0, x[tslideX]))], .(qtile)] %>%
        .[, qq := as.factor(qtile)] %>%
        .[, labx := ifelse(date == max(date), lab, NA)] %>%
        .[!is.na(x)]
      x <- x2 %>%
        ggplot(., aes(date, x, color = qq, label = labx)) +
        geom_hline(
          yintercept = 0,
          linewidth = .4,
          linetype = "dotted",
          color = 'grey40'
        ) +
        geom_line() +
        geom_point(size = .3) +
        geom_text_repel() +
        ylim(ylimX - x3) +
        labs(caption = geocuX[, paste0('bespoke districts: ', paste0(sort(irregpcode(rc9)), collapse =
                                                                      ', '))]) +
        xlab('') +
        ylab(bquote(Delta ~ P ~ log ~ price ~ change)) +
        theme_bw() +
        theme(
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(
            size = .2,
            linetype = 'dotted',
            color = pgmc
          ),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          text = element_text(size = 16, face = 'plain'),
          axis.line.y.left = element_line(size = .1),
          axis.line.x.bottom = element_line(size = .1),
          legend.position = 'none'
        ) +
        scale_color_manual(values = x0) +
        scale_x_date(
          breaks = as.Date(
            c(
              '1995-01-01',
              '2000-01-01',
              '2010-01-01',
              '2020-01-01',
              '2024-01-01'
            )
          ),
          date_labels = "%Y",
          limits = c(as.Date(c(
            '1994-12-31', '2027-12-31'
          )))
        )
      x
    } #plot x(t) ter, cus/bes
    
    #f411D <- function(x0=f250110ad,rctaX = rctaG)----------------------------------------
    f411D <- function(x0=f250110ad,rctaX = rctaG) {
      if (verbose) {print('X - enter f411D');print(names(x0)) }
      x1 <-
        copy(x0) %>%
        .[rc6 == rctaX] %>%
        #.[,V1:=ngrp]%>%
        .[order(ngrp)] %>%
        .[, target := irregpcode(rc6)] %>%
        #.[, -c('ngrp', 'rc6')] %>%
        .[, version := c(
          
          'Market',
          'Region',
          'Area',
          'Area-tertile',
          'Custom default',
          'Custom annual',
          'Custom all outliers',
          'Custom 50% outliers',
          'Bespoke'
        )%>%
          .[1:nrow(x0[rc6 == rctaX])] #handle 'no bespoke' initial condition
        ] %>%
        .[,c('target','version','n','rmsei','rmseo','delta','rsqi','rsqo','deltarsq','rmsei.avg','rmseo.avg','delta.avg','rsqi.avg','rsqo.avg','deltarsq.avg'),with=F]
      x2 <-
        gt::gt(x1) %>%
        cols_label(
          n = 'datapoints',
          version = 'index span',
          
          rmsei = 'training',
          rmseo = 'testing',
          delta = 'delta',
          
          rsqi = 'training',
          rsqo = 'testing',
          deltarsq = 'delta',
          
          rmsei.avg = 'training',
          rmseo.avg = 'testing',
          delta.avg = 'delta',
          
          rsqi.avg = 'training',
          rsqo.avg = 'testing',
          deltarsq.avg = 'delta'
        ) %>%
        tab_spanner(
          label = 'RMS error',
          columns = c(
            'rmsei',
            'rmseo',
            'delta',
            'rmsei.avg',
            'rmseo.avg',
            'delta.avg'
          ),
          level = 1
        ) %>%
        tab_spanner(
          label = 'R-squared',
          columns = c(
            'rsqi',
            'rsqo',
            'deltarsq',
            'rsqi.avg',
            'rsqo.avg',
            'deltarsq.avg'
          ),
          level = 1
        ) %>%
        tab_spanner(
          label = 'Target',
          columns = c('rmsei', 'rmseo', 'delta', 'rsqi', 'rsqo', 'deltarsq'),
          level = 2,
          gather = F
        ) %>%
        tab_spanner(
          label = 'Index average',
          columns = c(
            'rmsei.avg',
            'rmseo.avg',
            'delta.avg',
            'rsqi.avg',
            'rsqo.avg',
            'deltarsq.avg'
          ),
          level = 2,
          gather = F
        ) %>%
        fmt_number(
          columns = c(
            'rmsei',
            'rmseo',
            'delta',
            'rmsei.avg',
            'rmseo.avg',
            'delta.avg',
            'rsqi',
            'rsqo',
            'deltarsq',
            'rsqi.avg',
            'rsqo.avg',
            'deltarsq.avg'
          ),
          use_seps = TRUE,
          decimals = 4
        ) %>%
        fmt_number(columns = c('n'),
                   use_seps = TRUE,
                   decimals = 0) %>%
        gtExtras::gt_highlight_rows(rows = 5, fill = "#EEFFEE") %>%
        tab_options(
          table.font.size = 13,
          column_labels.font.size = 13,
          #row.padding=1.1,
          row.striping.include_table_body = F,
          table.align = 'center'
        )#%>%
      x411G <<- copy(x2)
      x2
    } 
    
    #f4xxD <- function(x0=rsicuG,geoxx=geocuG,rctax=rctaG){-----------------------
    f4xxD <- function(x0=rsicuG,geoxx=geocuG,rctax=rctaG){
      x1 <- #gep
        geoxx%>% #if targetonly then this is the targetonly geo
        x0$kfoldsse[.,on=c(nx='nx',rc6='rc9')]%>%
        .[,.(lab,rc6,ssek,ssei,sstr,n)]%>%
        .[,.(rmsei=sqrt(ssei/n),rmseo=sqrt(ssek/n),rsqi=1-ssei/sstr,rsqo=1-ssek/sstr,n,ssei,ssek),.(lab,rc6)]%>%
        .[,.(
          rmsei,
          rmseo,
          delta=rmseo-rmsei,
          rsqi,
          rsqo,
          deltarsq=rsqi-rsqo,
          n,
          ssei,
          ssek,
          lab,
          rc6
        )]%>%
        .[order(rc6)]
      x3 <-  #aggregate for this nx
        x0$kfoldsse[geoxx,on=c(nx='nx',rc6='rc9')]%>%
        .[,.(ssek=sum(ssek,na.rm=T),ssei=sum(ssei,na.rm=T),sstr=sum(sstr,na.rm=T),n=sum(n,na.rm=T)),lab]%>%  #additional line: sum by lab
        .[,.(rmsei=sqrt(sum(ssei,na.rm=T)/sum(n,na.rm=T)),rmseo=sqrt(sum(ssek,na.rm=T)/sum(n,na.rm=T)),rsqi=1-ssei/sstr,rsqo=1-ssek/sstr,n,ssei,ssek),lab]%>%
        .[,.(
          rmsei.avg=rmsei,
          rmseo.avg=rmseo,
          delta.avg=rmseo-rmsei,
          rsqi.avg=rsqi,
          rsqo.avg=rsqo,
          deltarsq.avg=rsqi-rsqo,
          lab)
        ]
      x4 <- 
        x1[x3,on=c(lab='lab')]%>%
        .[rc6==rctax]
      x4
    }
    
    
    #dfnxxR   <-      reactive({----------------------------------------------------
    dfnxxR   <-      reactive({
        if (verbose) {print('X - enter dfnxxR') }
        #x <- dfnxR()[, date] #vector of current date, now always drc
        x <- c(as.Date('1994-12-31'), x00R()$estdt[grep('^MKT', col), date])
        dfnxxG <<- copy(x)
        x
      })
    
    #estdtaR <-         eventReactive(nxaR(), {#---area estdt compute--------------------
    estdtaR <-         eventReactive(nxaR(), {#---area estdt compute
        if (verbose) {print('X - enter estdtaR')}
        x <-
          estdtsR()[nxaR(), on = c(nx = 'nx')] %>%
          .[, .(nx, date, ii, lab, rc3, qtile, xdotd, days, xdot, x)]
        estdtaG <<- copy(x)
        x
      })
    
    
    #estdtxR <-      eventReactive(list(estdtcuR(), estdtaR(), geocuX = geocuR()) #----112 x(t)-------
    estdtxR <-      eventReactive(list(estdtcuR(), estdtaR(), geocuX = geocuR()) #----112 x(t)
                    , {
                      if (verbose) {print('X - enter estdtxR')}
                      
                      x <-
                        festdtxX(estdtcuX = estdtcuR(),
                                 estdtaX = estdtaR(),
                                 geocuX = geocuR())
                      estdtxG <<- copy(x)
                      x
                    })
    #estdtcuR <-  eventReactive(list(rsicuR()), {  #---bespoke estdt select----------
    estdtcuR <-  eventReactive(list(rsicuR()), {  #---bespoke estdt select
        if (verbose) {print('X - enter estdtcuR')}
        x <- rsicuR()$estdt
        estdtcuG <<- copy(x)
        x
      })
    
    #estdtsR   <- reactive({--------------------------------------------------------
    estdtsR   <- reactive({
        if (verbose) {print('X - enter estdtsR')}
        x <- copy(x00R()$estdt)[, .(nx, ii, date, xdotd, days, xdot, x)]
        estdtsG <<- copy(x)
        x
      })
    
    #-------------------------------------------------------------------------------
    geo0R    <-
      reactive({
        
        if (verbose) {print('X - enter geo0R')}
        x <-
          x00R()$geo %>%
          .[(nx %/% 100000) == 4] %>%
          .[, .(
            nx,
            lab,
            rc6 = rc9,
            rc3 = substr(rc9, 1, 3),
            qtile = as.numeric(substr(lab, 4, 4))
          )] %>%
          z110R()[., on = c(rcx = 'rc6')] %>%
          .[, .(nx, lab, rc3, rc6 = rcx, qtile)]
        geo0G <<- copy(x)
        x
      })
    
    #-------------------------------------------------------------------------------
    geoaR <-
      eventReactive(rctaR(), {
        #---area tertile geo
        if (verbose) {print('X - enter geoaR')}
        # x <- geo0R()[rc3==substr(rctaR(),1,3)]
        x <-
          f250111bd$geo[(nx %/% 100000) == 4] %>%
          .[grep(substr(rctaR(), 1, 3), rc9)] %>%
          .[, .(
            nx,
            rc6 = rc9,
            rc3 = substr(rc9, 1, 3),
            qtile = as.numeric(substr(lab, 4, 4)),
            #
            lab
          )]
        geoaG <<- copy(x)
        x
      })
    
    #-------------------------------------------------------------------------------
    geocuR <-
      eventReactive(rccuR(), {
        #---bespoke geo compute
        if (verbose) {print('X - enter geocuR')}
        x <-
          fgeocuX(rccuX = rccuR())
        geocuG <<- copy(x)
        x
      })
    
    geoqR <-
      eventReactive(list(geoaR(), rctaR()), {
        #---qtile geo select
        if (verbose) {print('X - enter geoqR')}
        x <- geoaR() %>%
          .[geoaR()[rc6 == rctaR()][, .(qtile)], on = c(qtile = 'qtile')]
        geoqG <<- copy(x)
        x
      })
    
    #-------------------------------------------------------------------------------
    nxaR <-
      eventReactive(geoaR(), {
        #---area nx select
        if (verbose) {print('X - enter nxaR')}
        x <-
          geoaR()[, .(nx, rc3, qtile, lab)] %>%
          unique(.)
        nxaG <<- copy(x)
        x
      })
    
    fcheckrc6 <- function(x) {
      x[(nchar(x) == 6) & (substr(x, 3, 3) == '-')]
    }
    
    #-------------------------------------------------------------------------------
    rccuR <-  eventReactive(list(rctaR(), input$rccuC), {
      #-bespoke rc6
      if (verbose) {print('X - enter rccuR')}
      x <- sort(unique(c(rctaR(), input$rccuC))) %>%
        fcheckrc6(.) #valid
      rccuG <<- copy(x)
      x
    })
    
    #-------------------------------------------------------------------------------
    rctaR <-  eventReactive(input$rctaC, {
      #-target rc6
      if (verbose) {print('X - enter rctaR')}
      x <- input$rctaC[(nchar(input$rctaC) == 6)] %>%
        fcheckrc6(.) %>% #valid
        `[`(., y = 1) #first only
      shinyWidgets::updateTreeInput('rctaC', selected =
                                      x)  #update: first only
      rctaG <<- copy(x)
      x
    })
    
    #-------------------------------------------------------------------------------
    rc6deR <-
      eventReactive(rctaR(), {
        #eR-default bespoke rc6 <<<<<<
        if (verbose) {print('X - enter rc6deR')}
        if ((!is.null(rctaR())) &
            (all(nchar(rctaR()) == 6))) {
          x <-
            f241229bd[rctaR() == target, sort(f240920b(id))]
          rc6deG <<- copy(x)
          x
        }
      })
    
    #rsicuR <-    #---bespoke rsi compute...--------------------------------------------
    rsicuR <-    #---bespoke rsi compute
      eventReactive(list(input$docusabC), {
        if (verbose){print('X - enter rsicuR')}
        geox <- isolate(geocuR())
        dfnx <- isolate(dfnxxR()) #source of truth
        shinyWidgets::updateTreeInput('rccuC', selected = unique(union(rctaR()[1], rccuR())))
        rctax <- toupper(isolate(irregpcode(rctaR()[1])))
        rc6valid <- isolate(geo0R()[, rc6])
        if ((irregpcode(regpcode(rctax)) == rctax)
            &
            (nchar(regpcode(rctax)) == 6)
            &
            (regpcode(rctax) %in% rc6valid))  {
          print('recalc accepted in rsicuR')
          x <-
            f241119a(
              #returns estdt, kfoldsse, all
              nxx = 0,
              steprip2 = 'smallrip/',
              #smaller format
              dfn = dfnx,
              #R
              geo = geox,
              #R
              outthresh = .1,
              kfold = 5,
              sectorwise = T,
              usepra = F,
              newused = c('.'),
              houseflat = c('.')
            )
          rsicuG <<- copy(x)
        } else
        {
          print('recalc rejected in rsicuR')
          x <- copy(rsicuG)
        }
        x
      })
    
    #-------------------------------------------------------------------------------
    tslideR  <-
      reactive({
        if (verbose){ print('X - enter tslideR')}
        x <- input$tslider
        tslideG <<- copy(x)
        x
      })
    
    #-------------------------------------------------------------------------------
    x00R     <-
      reactive({
        if (verbose){print('X - enter x00R')}
        
        x <- copy(f250111bd)
        x00G <<- copy(x)
        x
      })
    
    #-------------------------------------------------------------------------------
    x111D <- eventReactive(list(rccuR(), geoaR(), pxosrdo2ddR(), rctaR(), z110R()), #111 map
                           {
                             if (verbose){ print('X - enter x111D')}
                             print(rccuR())
                             x <-   f111D(
                               rctaX = rctaR(),
                               rccuX = rccuR(),
                               geoaX = geoaR(),
                               pxosrdo2ddX = pxosrdo2ddR(),
                               z110X = z110R(),
                               colX = colx #punk green blue
                             )
                             x111G <<- copy(x)
                             x
                           })
    
    #-------------------------------------------------------------------------------
    x112D <- eventReactive(list(tslideR(), estdtxR(), ylimR(), geocuR()), #112 x(t)
                           {
                             if (verbose){ print('X - enter x112D')}
                             print(geocuR())
                             x <-
                               f112D(
                                 tslideX = tslideR(),
                                 estdtxX = estdtxR(),
                                 ylimX = ylimR(),
                                 geocuX = geocuR()
                               )
                             x112G <<- copy(x)
                             x
                           })
    
    #-------------------------------------------------------------------------------
    x411D <- eventReactive(list(
      geoqR(), 
      rctaR(), #target
      rssR(), #not needed
      x4xxD() #bespoke row
      ), #8x13 numerics plus one for bespoke; accuracy----tbin
                           {
                             if (verbose){ print('X - enter x411D')}
                             x0 <- #composite of 1-row bespoke and library
                               rbind(
                                 f250110ad, #is a global
                                 x4xxD()[,-c('ssei','ssek','lab')][,ngrp:=Inf] #bespoke
                                 )
                             print(x0)
                             x <- f411D(x0=x0,rctaX = rctaR())
                             x411G <<- copy(x)
                             x
                           })
    
    x4xxD <-    #---bespoke accuracy compute
      eventReactive(list(rsicuR()), {
        if (verbose){print('X - enter x4xxD')}
        geox <- isolate(geocuR())
        rsix <- isolate(rsicuR()) 
        rctax <- rctaR()
        x <- f4xxD(x0=rsix,geox=geox,rcta=rctax)
        x4xxG <<- copy(x)
        x
      })
    
    
    #-------------------------------------------------------------------------------
    ylimR <-       #ylim
      eventReactive(estdtxR(), {
        if (verbose){ print('X - enter ylimR')}
        x <-
          estdtxR()[, range(x, na.rm = T) + diff(range(x, na.rm = T)) *
                      c(-.1, .1)]#*1.1
        print(x)
        ylimG <<- copy(x)
        x
      })
    
    #-------------------------------------------------------------------------------
    z110R    <-
      reactive({
        if (verbose){ print('X - enter z110R')}
        x <- copy(z110)
        z110G <<- copy(x)
        x
      })
    
    #-------------------------------------------------------------------------------
    observe({
      #o-update tree with bespoke peers ----
      if (verbose){ print('X - enter observe updateTreeInput')}
      if (#guard against invalid selection
        (!is.null(rctaR())) &
        (all(nchar(rctaR()) == 6))) {
        rc6c <-
          rc6deR()
        shinyWidgets::updateTreeInput(
          inputId = 'rccuC',
          label = NULL,
          selected = rc6c,
          session = shiny::getDefaultReactiveDomain()
        )
      } else {
        print('no updateTreeInput')
      }
      if (verbose) {print('X - exit observe updateTreeInput') }
      
    })
    
  }
