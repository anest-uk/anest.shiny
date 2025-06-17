#f241021ad$estdt[nxaR(), on = c(nx = "nx"), .(nx, date, ii, lab, rc3, qtile, xdotd, days, xdot, x)]


#geoaR()[rc6 == rc6tR()]


# f250509edG nil
# f241021adG common : 4x
# f250519adG nil
# pxosrdo2ddG common : pxosrdo2ddR() remove, in timeseries reference pxosrdo2dd
# f241229bdG nil
# z110G function : 3x
# x101G nil

#load static
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
f121D() # missing yeards 95 96 24
f122D() # B at start of area-band
f131D() # B at start of colname
f132D(steprip=file.path(app_dirG, "data", "smallrip"))

#are the defaults in these functions ever used 'accidentally' i.e. the globals G instead of reactives R?  Plug this leak so 'reactive always'
#can their args be defaulted instead of 'old globals' to 'new inputs'?  This is how to do the transition.
rc6tG <-
"W--8--"
rc6ccG <-
c("SW-1W-", "SW-3--", "SW-7--", "W--8--")
geoaG <-
structure(list(nx = c(2273L, 2273L, 2273L, 2273L, 2282L, 2282L, 
2282L, 2282L, 2282L, 2282L, 2291L, 2291L, 2291L, 2291L, 2291L, 
2291L, 2291L, 2291L, 2291L, 2291L, 2291L, 2291L, 2291L, 2291L
), gx = c(253L, 253L, 253L, 253L, 254L, 254L, 254L, 254L, 254L, 
254L, 255L, 255L, 255L, 255L, 255L, 255L, 255L, 255L, 255L, 255L, 
255L, 255L, 255L, 255L), lab = c("BW--1", "BW--1", "BW--1", "BW--1", 
"BW--2", "BW--2", "BW--2", "BW--2", "BW--2", "BW--2", "BW--3", 
"BW--3", "BW--3", "BW--3", "BW--3", "BW--3", "BW--3", "BW--3", 
"BW--3", "BW--3", "BW--3", "BW--3", "BW--3", "BW--3"), rc3 = c("W--", 
"W--", "W--", "W--", "W--", "W--", "W--", "W--", "W--", "W--", 
"W--", "W--", "W--", "W--", "W--", "W--", "W--", "W--", "W--", 
"W--", "W--", "W--", "W--", "W--"), rc6 = c("W--13-", "W--3--", 
"W--5--", "W--7--", "W--10-", "W--12-", "W--14-", "W--4--", "W--6--", 
"W--9--", "W--11-", "W--1B-", "W--1D-", "W--1F-", "W--1G-", "W--1H-", 
"W--1J-", "W--1K-", "W--1S-", "W--1T-", "W--1U-", "W--1W-", "W--2--", 
"W--8--"), qtile = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 
3, 3, 3, 3, 3, 3, 3, 3, 3, 3)), row.names = c(NA, -24L), class = "data.frame")%>%data.table(.)


#---------------------------------------------gen1
#111 ok
#112 ok
#121 missing years ->  ok
#122 B at start : old system of labelling A/B/C found in f241021ad$geoplus - not important, will be replace in gen2
#131 B at start : see above
#132 2025 missing : smallrip is not up-to-date - will get fixed

#111
f111D( #leaflet 8 args
  rc6tX = rc6tG,                  #scalar: target rc6 that implicitly defines rc3
  rc6ccX = rc6ccG,                #vector: to outline
  geoaX = geoaG[,.(rc6,qtile)],   #geo {nx    gx    lab    rc3    rc6 qtile} : shade by qtile this-rc3-geo
  pxosrdo2ddX = pxosrdo2dd,       #global
  z110X = z110[,.(rcx,ppm2)],     #global {rcx ppm2} : pva
  colX = colx,                    #global named vector : punk green blue 
  minzoom = 9,                    #7 for national
  lightx = .7                     #higher is lighter
)

#112
f112D( #x(t) 4 timeseries |4 args
  tslideX = tslideG,              #integer: zero index
  estdtxX = estdtxG[,.(ii,date,lab,qtile,x,xdotd)],              #estdt where qtile in 0:3, 0=custom
  ylimX = ylimG,                  #vector: ylim
  geoccX = geoccG #rc9 nx=0 lab - only for annotation footnote with custom districts
)
#select the right 3 o4 4

#121
f121D( #-------------121 winding |7 args ok
  estdtX = estdtlG[,.(ii,date,lab,qtile,x,xdotd)],               #estdt {date days ii lab nx qtile rc3 x xdot xdotd}
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

#122
f122D( # ----122 characteristics|4 args
  rc6tX = rc6tG,                  #scalar: target
  rssaX = rssaG[,.(rc6,ssek,sstr,lab)],                  #rss { itrim lab n nx qtile rc3 rc6 ssei ssek sser sstr tbin type } : for area
  rssccX = rssccG[,.(rc6,ssek,sstr)],                #rss { itrim lab n nx qtile rc3 rc6 ssei ssek sser sstr tbin type } : for custom geo
  z110X = z110
)
#don't know why rssaG[,lab] has 'B' at the start

#131
f131D( #-------------131 summary|2 args
  estdtxX = estdtxG[,.(date,ii,lab,xdot)],
  tslideX = tslideG)
#don't know why estdtxG[,lab] has 'B' at the start

#132
f132D ( # ---132 trade summary(2)|5 args
  tslideX = tslideG,
  geoqX = geoqG[,.(rc6)],
  geoccX = geoccG[,.(rc9)], 
  estdtlX = estdtlG[,.(ii,date)], 
  steprip = file.path(app_dirG, "data", "smallrip"))

#---------------------------------------------gen2
# f250509ed or
# hidden layer R/G
#static--------------


#leaflet (one I did earlier) new function-leaf-111
x11 <- 
  f250517a( 
  rc6x = rc6tG, 
  x1 = pxosrdo2dd, 
  x2 = z110, 
  x3 = f250519ad
  )
x11

#timeseries x(t)--------------------------tser-112
#where is the rebasing done?
#is it too noisy? option to do 1.2,2.2 instead of 1.3,3.3?
#does it handle the ones where
x12 <- 
  f112g2(
  x1 = labxG, #input : this rc6,ordered by ssek
  x2 = rc6tG, #input : target
)
x12
ggplot(x12, aes(ii, x, group = col)) +
  geom_line(aes(color = col)) +
  geom_point(size = 1, aes(color = col)) +
  scale_color_identity()

#   
# 
# x4 <- 
#   f250519ad[grep(substr(rc6tG,1,3),rc6)][,.(lab,col)]%>%
#   .[order(lab)]%>%
#   unique(.)%>%
#   .[,.(lab,col)] #normally 6 rows 2 cols: all tile labs selected, this rc3
# x4
# x5 <- 
#   x1%>% #estdt
#   .[x4,on=c(lab='lab')]%>%
#   .[!grepl('2\\.3|\\.2',lab)] #exclude 2.3, 1.2, 2.2
# # x5 <- 
# #   x1[x4,on=c(lab='lab')][grepl('2\\.3|1\\.1',lab)]
# # x5 <- 
# #   x1[x4,on=c(lab='lab')][!grepl('1\\.1|\\.2',lab)]
# x6 <- 
#   ggplot(x5, aes(ii, x, group = i.col)) + 
#   geom_line(aes(color = i.col)) + 
#   geom_point(size=1,aes(color = i.col))+
#   scale_color_identity()
# x6


# 121
# f121g3 <- function(
#     x1 = f250509ed$estdt,
#     x2 = labxG[1, lab],
#     xdfnxG = dfnxG,
#     xtypeC = typeC,
#     xtbinC = tbinC
# ) {
#   x3 <- 
#     f121D( #-------------121 winding--------------wind
#     estdtX = x1[lab == x2, .(ii, date, lab, qtile = substr(lab, 5, 7), x, xdotd)],
#     dfnxX = dfnxG, # date {tbin1 tbin2 tbin3}
#     typeX = typeC, #' L' always
#     tbinX = tbinC, #' hi' always, tbinC=2 always
#     dfnxxX = # vector: drc dates excluding date0
#       dfnxG[, tbinC + 1, with = F] %>%
#         setnames(., "x") %>%
#         .[, sort(unique(x))],
#     d2X = # vector: annual dates excluding date0
#       dfnxG[, tbinC + 2, with = F] %>%
#         setnames(., "x") %>%
#         .[, sort(unique(x))]
#   )
#   x3
# }

#-------------121 winding--------------wind
x21 <- 
  f121g3(
    x1 = f250509ed$estdt,
    x2 = labxG[1, lab], #labxG/labxR best local lab
    dfnxx = dfnxG, #dfnxG/dfnxR table {date tbin1 tbin2 tbin3}
    typex = typeC, #'L' local
    tbinx = tbinC  #'hi' freq
  )
x21

#122
x21 <-
  f122D( # ----122 characteristics
    rc6tX = rc6tG,                  #scalar: target
    rssaX = rssaG[,.(rc6,ssek,sstr,lab)],              #rss { itrim lab n nx qtile rc3 rc6 ssei ssek sser sstr tbin type } : for area
    rssccX = rssccG[,.(rc6,ssek,sstr)],                #rss { itrim lab n nx qtile rc3 rc6 ssei ssek sser sstr tbin type } : for custom geo
    z110X = z110
  )
x21

#for rc6t
nxx   <- labxG[1,nx]         #nxx   = nx  for 'best local lab' 
xlabx <- labxG[1,lab]        #xlabx = lab for 'best local lab'
xlabg <- paste0(substr(xlabx,1,4),'.\\.',substr(xlabx,7,10)) #xlabg = grepstring for labels in .n (all i, same n)
xgeon <- f250509ed$geo[grep(xlabg,lab)] #xgeon = geo for ..n this rc3   #'geo for the local peers which may be 1/2/3'
xgeoi <- f250509ed$geo[nx==labxG[1,nx]] #xgeoi = geo for i.n this rc3

geoccG #custom geo
xgeon  #i.n    geo

#using these geo compute stats for 122 charac as in f122D
#do this by applying f122() to 'a table with fields like rssccG : { allsse.insam allsse.osam allsse.raw allsse.tot n nx nx rc6 rsqraw ssei ssek sser sstr toti }
#are these fields in f250509ed$kfoldsse, $all? yes:                 x            x           x          x          x x  x  x   x      x    x    x    x    x
#so how to join the 2 tables ready for f122?
sco(f250509ed$kfoldsse) #"{ n nx rc6 ssei ssek sser sstr toti }"
sco(f250509ed$all) #{ n nx rc6 ssei ssek sser sstr toti }

x1 <- f250509ed$kfoldsse[nx%in%xgeon[,unique(nx)]][] #add tbin=2 itrim=2 type=L rc3 qtile=1/2/2 lab=BW==1/2/3
x2 <- f250509ed$all[nx%in%xgeon[,unique(nx)]] #for *.n
#x3 <- f250509ed$kfoldsse[nx%in%xgeon[,unique(nx)]][,.(n=sum(n),ssei=sum(ssei),ssek=sum(ssek),sser=sum(sser),sstr=sum(sstr),toti=sum(toti)),.(nx)] #for *.n
x3 <- f250509ed$kfoldsse[xgeon,on=c(nx='nx',rc6='rc9')]#[,.(n=sum(n),ssei=sum(ssei),ssek=sum(ssek),sser=sum(sser),sstr=sum(sstr),toti=sum(toti)),.(nx)] #for *.n
x4 <- x2[x3,on=c(nx='nx')]
z110[,.(rc6=rcx,ppm2)][x4,on=c(rc6='rc6')][,.(min(ppm2),max(ppm2)),.(lab)]
f122(rss=x4,z110=z110)[]


#this now fixedvvvvv
# #seems that there is ppm2 overlap in 1,2 - where is the geo designed?  check W2 which  is in LW--2.2BA
# getgd(c('f250509cd','f250509ed'))
# f250509cd[grep('LW--',lab)][rc9=='W--2--'] #geo - confirm W2 is in 2.2
# f250509bd[rc9=='W--2--'] #w2 in 2.2
# #now look at f250509bFun
# #ok got it, problem was failure to reorder by ppm2 in f250509bFun so redo that, f250509cFun(), then redo the rsi:
# f250509dFun() #rsi
# f250509eFun() #collate
# 
# 
# f250509ed

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

