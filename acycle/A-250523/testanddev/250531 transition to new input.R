f241021ad$estdt[nxaR(), on = c(nx = "nx"), .(nx, date, ii, lab, rc3, qtile, xdotd, days, xdot, x)]


geoaR()[rc6 == rc6tR()]


# f250509edG nil
# f241021adG common : 4x
# f250519adG nil
# pxosrdo2ddG common : pxosrdo2ddR() remove, in timeseries reference pxosrdo2dd
# f241229bdG nil
# z110G function : 3x
# x101G nil


app_dirG <- file.path(normalizePath("."),"acycle","A-250523")

f250509ed <- readRDS(file = file.path(app_dirG, "data", "f250509ed.rds"))
f241021ad <- readRDS(file = file.path(app_dirG, "data", "f241021ad.rds"))
f250519ad <- readRDS(file = file.path(app_dirG, "data", "f250519ad.rds"))
pxosrdo2dd <- readRDS(file = file.path(app_dirG, "data", "pxosrdo2dd.rds"))
f241229bd <- readRDS(file = file.path(app_dirG, "data", "f241229bd.rds"))
z110 <- readRDS(file = file.path(app_dirG, "data", "z110.rds"))
x101 <- readRDS(file = file.path(app_dirG, "data", "x101.rds"))

#now can produce all page 1 displays using function lib thus:
f111D()
f112D()
f121D()
f122D()
f131D()
f132D(steprip=file.path(app_dirG, "data", "smallrip"))

#are the defaults in these functions ever used 'accidentally' i.e. the globals G instead of reactives R?  Plug this leak so 'reactive always'
#can their args be defaulted instead of 'old globals' to 'new inputs'?  This is how to do the transition.



f111D( #leaflet
  rc6tX = rc6tG,                  #scalar: target rc6 that implicitly defines rc3
  rc6ccX = rc6ccG,                #vector: to outline
  geoaX = geoaG[,.(rc6,qtile)],   #geo {nx    gx    lab    rc3    rc6 qtile} : shade by qtile this-rc3-geo
  pxosrdo2ddX = pxosrdo2dd,       #global
  z110X = z110[,.(rcx,ppm2)],     #global {rcx ppm2} : pva
  colX = colx,                    #global named vector : punk green blue 
  minzoom = 9,                    #7 for national
  lightx = .7                     #higher is lighter
)
#could generalise to 5?

f112D( #x(t) 4 timeseries
  tslideX = tslideG,              #integer: zero index
  estdtxX = estdtxG[,.(ii,date,lab,qtile,x,xdotd)],              #estdt where qtile in 0:3, 0=custom
  ylimX = ylimG,                  #vector: ylim
  geoccX = geoccG #rc9 nx=0 lab - only for annotation footnote with custom districts
)
#could generalise to 5?

f121D( #-------------121 winding
  estdtX = estdtlG[,.(ii,date,lab,qtile,x,xdotd)],               #estdt {date days ii lab nx qtile rc3 x xdot xdotd}
  dfnxX = dfnxG,                  #date {tbin1 tbin2 tbin3}
  #drangeX = range(dfnxxX),
  typeX = typeC,                  #'L' always
  tbinX = tbinC,                  #'hi' always, tbinC=2 always
  dfnxxX =                        #vector: drc dates excluding date0
    dfnxG[-1, tbinC + 1, with = F] %>% 
    setnames(., "x") %>%
    .[, sort(unique(x))], 
  d2X =                           #vector: annual dates excluding date0
    dfnxG[-1, tbinC + 2, with = F] %>%
    setnames(., "x") %>%
    .[, sort(unique(x))]          
)
#ok

f122D( # ----122 characteristics
  rc6tX = rc6tG,                  #scalar: target
  rssaX = rssaG[,.(rc6,ssek,sstr,lab)],                  #rss { itrim lab n nx qtile rc3 rc6 ssei ssek sser sstr tbin type } : for area
  rssccX = rssccG[,.(rc6,ssek,sstr)],                #rss { itrim lab n nx qtile rc3 rc6 ssei ssek sser sstr tbin type } : for custom geo
  z110X = z110
)
#don't know why rssaG[,lab] has 'B' at the start

f131D( #-------------131 summary
  estdtxX = estdtxG[,.(date,ii,lab,xdot)],
  tslideX = tslideG)
#don't know why estdtxG[,lab] has 'B' at the start

f132D ( # ---132 trade summary(2)
  tslideX = tslideG,
  geoqX = geoqG[,.(rc6)],
  geoccX = geoccG[,.(rc9)], 
  estdtlX = estdtlG[,.(ii,date)], 
  steprip = file.path(app_dirG, "data", "smallrip"))

#---------------------now switch to feeding the same functions with input from f250509ed
x1  <- f250509ed$estdt
x2 <- labxG[1,lab]


#111 - one I did earlier!
f250517a(
  rc6x = rc6tG, 
  x1 = pxosrdo2dd, 
  x2 = z110, 
  x3 = f250519ad
  )


#112 x(t) for five and custom [still lacking custom; merge 1.1 and 2.3]
#think hard about what to show - probably have to show the five
#
#where is the rebasing done?
x4 <- 
  f250519ad[grep(substr(rc6tG,1,3),rc6)][,.(lab,col)]%>%
  .[order(lab)]%>%
  unique(.)%>%
  .[,.(lab,col)]
x5 <- 
  x1[x4,on=c(lab='lab')][!grepl('2\\.3|\\.2',lab)]
# x5 <- 
#   x1[x4,on=c(lab='lab')][grepl('2\\.3|1\\.1',lab)]
# x5 <- 
#   x1[x4,on=c(lab='lab')][!grepl('1\\.1|\\.2',lab)]
x6 <- 
  ggplot(x5, aes(ii, x, group = i.col)) + 
  geom_line(aes(color = i.col)) + 
  geom_point(size=1,aes(color = i.col))+
  scale_color_identity()
x6

121
f121D( #-------------121 winding - this has problem: blank 2024<<<<<<<<<<<<<<
  estdtX = x1[lab==x2,.(ii,date,lab,qtile=substr(lab,5,7),x,xdotd)],
  dfnxX = dfnxG,                  #date {tbin1 tbin2 tbin3}
  #drangeX = range(dfnxxX),
  typeX = typeC,                  #'L' always
  tbinX = tbinC,                  #'hi' always, tbinC=2 always
  dfnxxX =                        #vector: drc dates excluding date0
    dfnxG[, tbinC + 1, with = F] %>% 
    setnames(., "x") %>%
    .[, sort(unique(x))], 
  d2X =                           #vector: annual dates excluding date0
    dfnxG[, tbinC + 2, with = F] %>%
    setnames(., "x") %>%
    .[, sort(unique(x))]          
)
#how is dfnxG compatible with estdt - are they from same ver?  I guess so


#122
f122D( # ----122 characteristics
  rc6tX = rc6tG,                  #scalar: target
  rssaX = rssaG[,.(rc6,ssek,sstr,lab)],                  #rss { itrim lab n nx qtile rc3 rc6 ssei ssek sser sstr tbin type } : for area
  rssccX = rssccG[,.(rc6,ssek,sstr)],                #rss { itrim lab n nx qtile rc3 rc6 ssei ssek sser sstr tbin type } : for custom geo
  z110X = z110
)


#131
f131D( #-------------131 summary
  estdtxX = x1[lab==x2,.(date,ii,lab=col,xdot)],
  tslideX = tslideG)

#132
x3 <- f250509ed$geo[nx==labxG[1,nx]]
f132D ( # ---132 trade summary(2)
  tslideX = tslideG,
  geoqX = x3[,.(rc6=rc9)],
  geoccX = geoccG[,.(rc9)], 
  estdtlX = x1[nx==labxG[1,nx],.(ii,date)], 
  steprip = file.path(app_dirG, "data", "smallrip"))

