#--------------------------reactives not in essentials----
if(F) {
  x121D <- eventReactive(list(estdtlR(),estdtcuR(),dfnxxR()),  #121 winding----
                         {
                           if(verbose) print('enter x121D')
                           x2 <- f121D(estdt=estdtlR(),
                                       dfnxX=dfnxR())
                           x4 <- f121D(estdt=estdtcuR(),
                                       dfnxX=dfnxR(),
                                       typeX='C')
                           x <- list(x2,x4)
                           x121G <<- copy(x)
                           x
                         }
  )
  
  
  
  x122D <- eventReactive(list(rc6tR(),rssaR(),rsscuR(),z110R()), #122 characteristics---- 
                         {
                           if(verbose) print('enter x122D')
                           x <- f122D(
                             rc6tX=rc6tR(),
                             rssaX=rssaR(),
                             rsscuX=rsscuR(),
                             z110X=z110R()
                           )
                           x122G <<- copy(x)
                           x
                         }
  )
  
  x131D <- eventReactive(list(tslideR(),estdtxR()),#131 summary----
                         {
                           if(verbose) print('enter x131D')
                           x <-  f131D(
                             estdtxX=estdtxR(),
                             tslideX=tslideR()
                           )
                           x131G <<- copy(x)
                           x
                         }
  )
  
  
  x132D <- eventReactive(list(tslideR(),geoqR(),estdtlR()),#132 trade summary(2)----
                         {
                           if(verbose) print('enter x132D')
                           x <- f132D(
                             tslideX=tslideR(),
                             geoqX=geoqR(),
                             geocuX=geocuR(),
                             estdtlX=estdtlR()
                           )
                           x132G <<- copy(x)
                           if(verbose) print('exit x132D')
                           x
                         }
  )
  
  
  #211 listing----
  x211D <- eventReactive(list(estdtlR(),geoqR(),dfnxxR()),       #211 listing----
                         {
                           if(verbose) print('enter x211D')
                           x <- f211D(
                             estdtlX=estdtlR(),
                             geoqX=geoqR(), 
                             dfnxxX=dfnxxR()
                           )
                           x211G <<- copy(x)
                           x
                         }
  )
  x211cuD <- eventReactive(list(estdtcuR(),geocuR(),dfnxxR()),           #211cu listing----
                           {
                             if(verbose) print('enter x211D')
                             geox <- copy(geocuR())[,let(rc6,rc9)] #used for aggregation and label
                             x <- f211D(
                               estdtlX=estdtcuR(),
                               geoqX=geocuR()[,.(nx,lab,rc6=rc9)], #non-standard geo
                               dfnxxX=dfnxxR(),
                               typeX='C'
                             )
                             x211cuG <<- copy(x)
                             x
                           }
  )
  
  x311D <- eventReactive(list(geo0R(),z110R(),rc6tX=rc6tR()),     #311 custom constituents output table----
                         {
                           if(verbose) print('enter 311')
                           x <- f311D(
                             geo0X=geo0R(),
                             z110X=z110R(),
                             rc6tX=substr(rc6tR(),1,3) #arbitrarily initialise it to the area
                           )
                           x311G <<- copy(x)
                           x
                         }
  )
  
  x412D <- eventReactive(list(geocuR(),rc6tR(),rsscuR()),      #2x11cu accuracy--custom--tbin----
                         {
                           if(verbose) print('enter x411Gcu')
                           x <-  f412D(geocuX=geocuR(),rc6tX=rc6tR(),rsscuX=rsscuR())
                           x412G <<- copy(x)
                           x
                         }
  )
  
  x421D <- eventReactive(list(geoqR(),rc6tR(),rssR()),        #221 accuracy----trim----
                         {
                           if(verbose) print('enter x421D')
                           x <-  f421D(geoqX=geoqR(),rc6tX=rc6tR(),rssX=rssR())
                           x421G <<- copy(x)
                           x
                         }
  )
  
  x422D <- eventReactive(list(geocuR(),rc6tR(),rsscuR()),      #221cu accuracy----trim----
                         {
                           if(verbose) print('enter x422D')
                           x <- f422D(geocuX=geocuR(),rc6tX=rc6tR(),rsscuX=rsscuR())
                           x422G <<- copy(x)
                           x
                         }
  )
  
  x431D <- eventReactive(list(geoqR(),rc6tR(),rssR()),        #231 accuracy----in/out----
                         {
                           if(verbose) print('enter x431D')
                           x <- f431D(geoqX=geoqR(),rc6tX=rc6tR(),rssX=rssR())
                           x431G <<- copy(x)
                           x
                         }
  )
  
  x432D <- eventReactive(list(geocuR(),rc6tR(),rsscuR()),      #231cu accuracy----in/out----
                         {
                           if(verbose) print('enter x432D')
                           x <- f432D(geocuX=geocuR(),rc6tX=rc6tR(),rsscuX=rsscuR())
                           x432G <<- copy(x)
                           x
                         }
  )
  
}
