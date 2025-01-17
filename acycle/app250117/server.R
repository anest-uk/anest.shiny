#-----UNCH-----



#---server   section
server <- 
function(input, output) {
  
  #behaviour of JS testing depends on location of these lines so do not move without testing
  output$selrc6 <- renderText({ paste0('selected:   ',selectedrc6stringR()) })
  output$comrc6 <- renderText({ paste0('computed: ',computedrc6R()) })
  output$defrc6 <- renderText({ paste0(c('suggested: ',rc6deR()),collapse='') })
  output$selrc6forjstest <- renderText({ selectedrc6stringR() })
  output$comrc6forjstest <- renderText({ computedrc6R() })
  output$cuseqcom <- renderText({'Custom index matches selection'})
  output$cusnecom <- renderText({'Recalc for selected districts'})
  output$x111 <- renderLeaflet(x111D())
  output$x112 <- renderPlot(x112D())
  output$x121a <- gt::render_gt(x121D()[[1]])
  output$x121b <- gt::render_gt(x121D()[[2]])
  output$x122 <- gt::render_gt(x122D())
  output$x131 <- gt::render_gt(x131D())
  output$x132a <- gt::render_gt(x132D()[['local']][[1]])
  output$x132b <- gt::render_gt(x132D()[['local']][[2]])
  output$x132c <- gt::render_gt(x132D()[['custom']][[1]])
  output$x132d <- gt::render_gt(x132D()[['custom']][[2]])
  output$x411 <- gt::render_gt(x411D())
  output$x421 <- gt::render_gt(x421D())
  output$x431 <- gt::render_gt(x431D())
  output$x412 <- gt::render_gt(x412D())
  output$x422 <- gt::render_gt(x422D())
  output$x432 <- gt::render_gt(x432D())
  output$x211 <- gt::render_gt(x211D())
  output$x211cu <- gt::render_gt(x211cuD())
  output$x311 <- DT::renderDT(x311D())
  
  selectedrc6R <-  eventReactive(list(input$rctreeC,input$rc6tC),
  {
  x0 <- union(
      input$rctreeC, #custom
      input$rc6tC     #target (always must be included)
    )%>%
      unique(.)%>%
      sort(.)
    x1 <- #exclude non-rc6 higher tree nodes
      x0[
        which(
          (nchar(x0)==6) &
            (substr(x0,3,3)=='-')
        )
      ]
    print(x0)
    print(paste0('selectedrc6R: ',paste0(x1,collapse=',')))
    selectedrc6G <<- copy(x1)
    x1 #return vector of union(tree1,tree2)
  })
  selectedrc6stringR <- eventReactive(list(selectedrc6R()), #r----
  {
    print('-----------------*******************______________________***********************')
    x1 <- selectedrc6R()
    x <- paste0(paste0(x1,collapse=',')) #for messages
    print(paste0('selectedrc6stringR: ',x))
    selectedrc6stringG <<- copy(x)
    x
  })
  computedrc6R<-   #r----
  eventReactive(list(rsicuR()),
  { 
    rsicuX <- rsicuR()
    x1 <- rsicuX$kfoldsse[,rc6]%>%.[nchar(.)==6]
    x <- paste0(paste0(sort(unique(x1)),collapse=','))
    print(paste0('computedrc6R: ',x))
    computedrc6G <<- copy(x)
    x
  })
  
  #---global   section---- UNCH
  rssR     <-    #r----
    reactive({ 
    x <- copy(x00R()$rss)
    rssG <<- copy(x)
    x
  })
  pxosrdo2ddR <- #r----
    reactive({ 
    x <- copy(pxosrdo2dd)
    pxosrdo2ddG <<- copy(x)
    x
  })
  x101R    <-    #r----
    reactive({ 
      print('enter x101R')
      x <- copy(x101) #initial dates
    x101G <<- copy(x)
    x
  })
  dfnF <- #date extractor using globals----
    function(nn=c('tbinC','dfnxG')) {
      x <- dfnxR()[,paste0('tbin',tbinC),with=F]%>%setnames(.,'x')%>%.[,sort(unique(x))]
      x #vector of Date 0:tbar
    }
  
  #------------------------reactive + global
  
  #---target section----
  #---custom   section----
  
  
  
  fgeocuX <-    #---custom geo compute   -----
  function( 
    rc6cuX=rc6cuG
  ){
    x <- 
      data.table(rc9=rc6cuX,nx=0,lab='CU00') 
    x
  }
  
  nxcuR <-     #---custom nx compute    ----
  eventReactive( 
    geocuR(),
    {
      if(verbose) print('enter nxcuR')
      x <- 
        geocuR()[,.(nx,rc3,qtile,lab)]%>%
        unique(.)
      nxcuG <<- copy(x)
      x
    }
  )

  #---qtile    section----
  nxqR <-      #---qtile nx compute     ----
  eventReactive( 
    geoqR(),
    {
      if(verbose) print('enter nxqR')
      x<- 
        geoqR()[,.(nx,rc3,qtile,lab)]%>%
        unique(.)
      nxqG <<- copy(x)
      x
    }
  )
  estdtlR <-   #---local estdt compute  ----
  eventReactive( 
    nxqR(),
    {
      if(verbose) print('enter estdtlR')
      x <- 
        estdtR()[nxqR(),on=c(nx='nx')]%>%
        .[,.(nx,date,ii,lab,rc3,qtile,xdotd,days,xdot,x)]
      estdtlG <<- copy(x)
      x
    }
  )
  #---area     section----
  rssaR <-     #---area rss compute     ----
  eventReactive( 
    nxaR(),
    {
      if(verbose) print('enter rssaR')
      x <- 
        rssR()[nxaR(),on=c(nx='nx')]
      rssaG <<- copy(x)
      x
    }
  )
  #---combo    section                  ----
  festdtxX <- #----112 x(t)              ----
  function(
    estdtcuX=estdtcuG,estdtaX=estdtaG,geocuX=geocuG
  ) {
    x <- 
      rbind(
        estdtcuX[,.(nx,date,xdotd,days,xdot,x,lab,ii,qtile=0,rc3=lab)],
        estdtaX [,.(nx,date,xdotd,days,xdot,x,lab,ii,qtile,rc3)]
      )[,qq:=as.factor(qtile)]
    x
  }
  #festdtxX()
  
  #revised---------------------
  f111D <- function( #-leaflet----
    rc6tX=rc6tG,
    rc6cuX=rc6cuG,
    geoaX=geoaG,
    pxosrdo2ddX=pxosrdo2ddG,
    z110X=z110G,
    colX=colx, #punk green blue
    minzoom=9,#7 for national
    lightx=.7#higher is lighter
  ) {
    x <-
      geoaX%>%
      .[,.(
        rc6,
        col=lighten(colX,lightx)[.BY[[1]]],   ### capital in colX <<<<<<<<<<<<<<<<<<<<
        qtile, #shade tiles light
        lab
      ),by=.(qtile)]%>%
      .[
        rc6==rc6tX, #with target district darker
        col:=colX[.BY[[1]]],
        by=.(qtile)
      ]%>%
      f240810b( #->leaflet, colours for areas-to-shade in column 'col'
        .,
        x2=pxosrdo2ddX,
        pva=z110X,
        minzoom=minzoom,
        maxzoom=12
      )%>%
      addPolygons( #outline custom districts
        data=pxosrdo2ddX[which(pxosrdo2ddX@data$name%in%irregpcode(rc6cuX)),],
        fill=F,
        color="orange",
        weight=1,
        opacity=1
      )
    x
  }
  f112D <- function(#-timeseries ----
    tslideX=tslideG,
    estdtxX=estdtxG,
    ylimX=ylimG,
    geocuX=geocuxxG
  )
  {
    #browser()
    print('X - enter f112D')
    x2c <- estdtxX%>%
      .[,.SD[,.(ii,date,lab,x=x-ifelse(tslideX==0,0,x[tslideX]))],.(qtile)]%>%
      .[,.SD[,.(ii,date,lab,x)],.(qtile)]%>%
      .[,qq:=as.factor(qtile)]%>%
      .[,labx:=ifelse(date==max(date),lab,NA)]
    x0 <- setNames(cobalt()[c('punk','green','blue')],as.character(1:3))
    x3 <- estdtxX[,.SD[,.(ifelse(tslideX==0,0,x[tslideX]))],.(qtile)][,mean(V1)]#base value for rebase level
    x2 <-
      estdtxX[,.SD[,.(ii,date,lab,x=x-ifelse(tslideX==0,0,x[tslideX]))],.(qtile)]%>%
      .[,qq:=as.factor(qtile)]%>%
      .[,labx:=ifelse(date==max(date),lab,NA)]
    x <- x2%>%
      ggplot(.,aes(date,x,color=qq,label=labx))+
      geom_hline(yintercept=0,linewidth=.4,linetype = "dotted",color='grey40')+
      geom_line()+
      geom_point(size=.3)+
      geom_text_repel()+
      ylim(ylimX-x3)+
      labs(caption = geocuX[,paste0('Custom districts: ',paste0(sort(irregpcode(rc9)),collapse=', '))])+
      xlab('')+
      ylab(bquote(Delta~P~log~price~change))+
      theme_bw() +
      theme(
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(size=.2,linetype = 'dotted',color=pgmc),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text=element_text(size=16,face='plain'),
        axis.line.y.left=element_line(size=.1),
        axis.line.x.bottom=element_line(size=.1),
        legend.position='none')+
      scale_color_manual(values=x0)+
      scale_x_date(
        breaks = as.Date(c('1995-01-01','2000-01-01','2010-01-01','2020-01-01','2024-01-01')),
        date_labels = "%Y",
        limits=c(as.Date(c('1994-12-31','2027-12-31')))
      )
    x
  }
  #f112D()
  
  f411D <- function(geoqX=geoqG,rc6tX=rc6tG,rssX=rssG) {        #2x11 accuracy----tbin----
    x1 <- 
      copy(f250107bd)%>%
      .[rc6==rc6tX]%>%
      .[order(V1)]%>%
      .[,target:=irregpcode(rc6)]%>%
      .[,-c('V1','rc6')]%>%
      .[,version:=c('Market','Region','Area','Area-tertile','Custom default','Custom annual','Custom all outliers','Custom 50% outliers')]%>%
      .[,c(14,15,1:13)]
    
    x2 <-   
      gt::gt(x1)%>%
      cols_label(
        n='datapoints',
        version='index span',
        
        rmsei='training',
        rmseo='testing',
        delta='delta',
        
        rsqi='training',
        rsqo='testing',
        deltarsq='delta',
        
        rmsei.avg='training',
        rmseo.avg='testing',
        delta.avg='delta',
        
        rsqi.avg='training',
        rsqo.avg='testing',
        deltarsq.avg='delta'
      )%>%
      tab_spanner(label='RMS error',columns=c('rmsei','rmseo','delta','rmsei.avg','rmseo.avg','delta.avg'),level=1)%>%
      tab_spanner(label='R-squared',columns=c('rsqi','rsqo','deltarsq','rsqi.avg','rsqo.avg','deltarsq.avg'),level=1)%>%
      tab_spanner(label='Target',columns=c('rmsei','rmseo','delta','rsqi','rsqo','deltarsq'),level=2,gather=F)%>%
      tab_spanner(label='Index average',columns=c('rmsei.avg','rmseo.avg','delta.avg','rsqi.avg','rsqo.avg','deltarsq.avg'),level=2,gather=F)%>%
      fmt_number(columns=c('rmsei','rmseo','delta','rmsei.avg','rmseo.avg','delta.avg','rsqi','rsqo','deltarsq','rsqi.avg','rsqo.avg','deltarsq.avg'),
                 use_seps = TRUE,
                 decimals = 4
      )%>%
      fmt_number(columns=c('n'),
                 use_seps = TRUE,
                 decimals = 0
      )%>%
      gtExtras::gt_highlight_rows(rows = 5,
                                  fill = "#EEFFEE")%>%
      tab_options(table.font.size = 13,column_labels.font.size = 13,#row.padding=1.1,
                  row.striping.include_table_body = F,table.align='center')#%>%
    x411G <<- copy(x2)
    x2
  }
  
  #################################################################################
  dfnxR    <-    #r----
  reactive({     
    print('X - enter dfnxR')
    x <-   #4-col table with NA
      dcast(x00R()$estdt[,.(tbin,date)]%>%unique(.)%>%.[order(tbin,date)],date~tbin,value.var='date')%>% #lo, hi, an
      rbind(as.data.table(as.list(rep(as.Date('1994-12-31'),4))),.,use.names=F)%>%
      setnames(.,c('date','tbin1','tbin2','tbin3'))
    dfnxG <<- copy(x)
    x
  })
  
  dfnxxR   <-    #r----
  reactive({ 
    print('X - enter dfnxxR')
    x <- #vector of current date
      dfnxR()[,paste0('tbin',tbinC),with=F]%>%setnames(.,'x')%>%.[,sort(unique(x))]
    dfnxxG <<- copy(x)
    x
  })
  
  estdtaR <-   #---area estdt compute   ----
  eventReactive( 
    nxaR(),
    {
      if(verbose) print('X - enter estdtaR')
      x <- 
        estdtR()[nxaR(),on=c(nx='nx')]%>%
        .[,.(nx,date,ii,lab,rc3,qtile,xdotd,days,xdot,x)]
      estdtaG <<- copy(x)
      x
    }
  )
  
  estdtcuR <-  #---custom estdt select  ----
  eventReactive( 
    list(
      rsicuR()
    ),
    {
      if(verbose) print('X - enter estdtcuR')
      x <- rsicuR()$estdt
      estdtcuG <<- copy(x)
      x
    }
  )
  
  estdtR   <-    #r----
  reactive({ 
    print('X - enter estdtR')
    x <- copy(x00R()$estdt)[,.(nx,ii,date,xdotd,days,xdot,x)]
    estdtG <<- copy(x)
    x
  })
  
  estdtxR <- #----112 x(t)              ----
  eventReactive(
    list(estdtcuR(),estdtaR(),geocuX=geocuR())
    ,
    {
      print('X - enter estdtxR')
      x <- 
        festdtxX(estdtcuX=estdtcuR(),estdtaX=estdtaR(),geocuX=geocuR())
      estdtxG <<- copy(x)
      x
    }
  )
  
  geo0R    <-    #r----
  reactive({ 
    print('X - enter geo0R')
    x <- 
      geoplusR()%>%
      .[type=='L']%>%
      .[itrim==itriC]%>%
      .[tbin==tbinC]%>%
      .[,.(
        nx,
        gx,
        lab=des,
        rc6=rc9,
        rc3=substr(rc9,1,3),
        qtile=as.numeric(substr(des,4,4)))
      ]%>%
      z110R()[.,on=c(rcx='rc6')]%>%
      .[,.(nx,gx,lab,rc3,rc6=rcx,qtile)]
    geo0G <<- copy(x)
    x
  })
  # dfnR <- reactive({ #not used?
  #   x <- data.table(date=c(as.Date('1994-12-31'),estdtR()[,sort(unique(date))]))[,let(i,(0:(.N-1)))]
  #   dfnG <<- copy(x)
  #   x
  # })
  
  geoaR <-     #---area tertile geo  ----
  eventReactive( 
    rc6tR(),
    {
      if(verbose) print('X - enter geoaR')
      # x <- geo0R()[rc3==substr(rc6tR(),1,3)] 
      #browser()
      x <- 
        f250111bd$geo[(nx%/%100000)==4]%>%
        .[grep(substr(rc6tR(),1,3),rc9)]%>%
        .[,.(
        nx,
        rc6=rc9,
        rc3=substr(rc9,1,3),
        qtile=as.numeric(substr(lab,4,4)),
        lab
      )]
      geoaG <<- copy(x)
      x
    }
  )
  
  geocuxxR <-    #---custom geo compute   -----
  eventReactive( 
    rc6cuR(), #from control
    {
      if(verbose) print('X - enter geocuxxR')
      #browser()
      x <- data.table(rc9=rc6cuR(),nx=0,lab='CU00') 
      geocuxxG <<- copy(x)
      x
    }
  )
  
  geocuR <-    #---custom geo compute   -----
  eventReactive( 
    rc6cuR(),
    {
      if(verbose) print('X - enter geocuR')
      x <- 
        fgeocuX(rc6cuX=rc6cuR())
      geocuG <<- copy(x)
      x
    }
  )
  
  rsscuR <-    #---custom rss select    ----
  eventReactive( 
    list(
      rsicuR()
    ),
    {
      if(verbose) print('X - enter rsscuR')
      x <- cbind(rsicuR()$kfoldsse,rsicuR()$all)
      rsscuG <<- copy(x)
      x
    }
  )
  
  geoplusR <-    #r----
  reactive({
    print('X - enter geoplusR')
    x <- copy(x00R()$geoplus)[,let(lab,des)]
    geoplusG <<- copy(x)
    x
  })
  
  geoqR <-     #---qtile geo select     ----
  eventReactive( 
    list(geoaR(),geotR()#,
    ),
    {
      if(verbose) print('X - enter geoqR')
      x <- geoaR()%>% 
        .[geotR()[,.(qtile)],
          on=c(qtile='qtile')]
      geoqG <<- copy(x)
      x
    }
  )
  
  geotR <-  eventReactive(rc6tR(),         #-target geo compute   ----
                          {
                            if(verbose) print('X - enter geotR')
                            x <- 
                              geoaR()%>%
                              .[rc6==rc6tR()]
                            geotG <<- copy(x)
                            x
                          }
  )
  
  nxaR <-      #---area nx select       ----
  eventReactive( 
    geoaR(),
    {
      if(verbose) print('X - enter nxaR')
      x <- 
        geoaR()[,.(nx,rc3,qtile,lab)]%>%
        unique(.)
      nxaG <<- copy(x)
      x
    }
  )
  
  rc6cuR <-    #---custom rc6 control   ----
  eventReactive( 
    list(rc6tR(),input$rctreeC), #+control
    {
      if(verbose) print('X - enter rc6cuR')
      x <- sort(unique(c(rc6tR(),input$rctreeC)))%>%.[nchar(.)==6]
      rc6cuG <<- copy(x)
      x
    }
  )
  
  rc6tR <-  eventReactive(input$rc6tC,     #-target rc6 reformat  ----
                          {
                            if(verbose) print('X - enter rc6tR')
                            x <- 
                              regpcode(input$rc6tC)[1]
                            rc6tG <<- copy(x)
                            x
                          })
  
  rc6deR <-    #eR-default custom rc6 <<<<<<   ----
  eventReactive( 
    input$rc6tC, #target 
    {
      if(verbose) {
        print('X - enter updateTreeInput')
      }
      if(#guard against invalid selection
        (!is.null(input$rc6tC))&#empty tree
        (all(nchar(input$rc6tC)==6))#non-leaf selection
      ) {
        x <- 
          f241229bd[rc6tR()==target,sort(f240920b(id))]
        if(verbose) {
          print('custom peers:')
          print(x)
        }
        rc6deG <<- copy(x)
        x
      }
    }
  )
  
  rsicuR <-    #---custom rsi compute   ----
  eventReactive(
    list(
      input$docusabC
    ),
    {
      if(verbose) print('X - enter rsicuR')
      geox <- isolate(geocuR())
      dfnx <- isolate(dfnxxR()) #source of truth
      if(verbose) isolate(print(paste0('set/reset custom ',paste0(input$rctreeC,collapse=','),' to include target ',input$rc6tC[1])))
      shinyWidgets::updateTreeInput('rctreeC',selected=unique(union(input$rc6tC[1],input$rctreeC)))
      rc6tx <- toupper(isolate(irregpcode(input$rc6tC[1])))
      rc6valid <- isolate(geo0R()[,rc6])
      if(
        (irregpcode(regpcode(rc6tx))==rc6tx)
        &
        (nchar(regpcode(rc6tx))==6)
        &
        (regpcode(rc6tx)%in%rc6valid)
      )  {
        print('recalc accepted in rsicuR')
        x <-
          f241119a(  #returns estdt, kfoldsse, all
            nxx=0,
            steprip2='smallrip/',  #smaller format
            dfn=dfnx,    #R
            geo=geox, #R
            outthresh=.1,
            kfold=5,
            sectorwise=T,
            usepra=F,
            newused=c('.'),
            houseflat=c('.')
          )
        rsicuG <<- copy(x)
      } else {
        print('recalc rejected in rsicuR')
        x <- copy(rsicuG)
      }
      x
    }
  )
  
  tslideR  <-    #r----
  reactive({ 
    print('X - enter tslideR')
    x <- input$tslider
    tslideG <<- copy(x)
    x
  })
  
  x00R     <-    #r----
  reactive({ 
    print('X - enter x00R')
    x <- copy(f241021ad)
    x00G <<- copy(x)
    x
  })
  
  x111D <- eventReactive(list(rc6tR(),rc6cuR(),geoaR(),pxosrdo2ddR(),z110R()),       #111 map ----
                         {
                           if(verbose) print('X - enter x111D')
                           x <-   f111D(
                             rc6tX=rc6tR(),
                             rc6cuX=rc6cuR(),
                             geoaX=geoaR(),
                             pxosrdo2ddX=pxosrdo2ddR(),
                             z110X=z110R(),
                             colX=colx #punk green blue
                           )
                           x111G <<- copy(x)
                           x
                         }
  )
  
  x112D <- eventReactive(list(input$tslider,estdtxR(),ylimR()),#112 x(t)----
                         {
                           if(verbose) print('X - enter x112D')
                           x <- 
                             f112D(tslideX=tslideR(),
                                   estdtxX=estdtxR(),
                                   ylimX=ylimR(),
                                   geocuX=geocuR()
                             )
                           x112G <<- copy(x)
                           x
                         }
  )
  
  x411D <- eventReactive(list(geoqR(),rc6tR(),rssR()),        #2x11 accuracy----tbin----
                         {
                           if(verbose) print('X - enter x411D')
                           x <- f411D(geoqX=geoqR(),rc6tX=rc6tR(),rssX=rssR())
                           x411G <<- copy(x)
                           x
                         }
  )
  
  ylimR <-       #ylim                    ----
  eventReactive( 
    estdtxR(),
    {
      print('X - enter ylimR')
      x <- 
        estdtxR()[,range(x)]*1.1
      ylimG <<- copy(x)
      x
    }
  )
  
  z110R    <-    #r----
  reactive({ 
    print('X - enter z110R')
    x <- copy(z110)
    z110G <<- copy(x)
    x
  })
  
  observe({     #o-update tree with custom peers ----
    if(verbose) {
      print('X - enter observe updateTreeInput')
    }
    if(#guard against invalid selection
      (!is.null(input$rc6tC))&#empty tree
      (all(nchar(input$rc6tC)==6))#non-leaf selection
    ) {
      rc6c <- 
        rc6deR()
      if(verbose) {
        print('treeInput update with custom peers:')
        print(rc6c)
      }
      shinyWidgets::updateTreeInput(
        inputId='rctreeC',
        label = NULL,
        selected = rc6c,
        session = shiny::getDefaultReactiveDomain()
      )
      if(verbose) {
        print('updateTreeInput complete')
      }
    } else {
      print('no updateTreeInput')
    }  
  }
  )
  
}
