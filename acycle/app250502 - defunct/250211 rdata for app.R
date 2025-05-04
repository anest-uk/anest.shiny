f250110ad <- getlast('f250110ad') #RSQ collation rss
f250111bd <- getlast('f250111bd') #RSI collation geo and estdt
f241021ad <- getlast('f241021ad') #1/6 to replace
f241229bd <- getlast('f241229bd') #2/6 tertile encoded geo
getgd(               'f250107bd') #3/6 rms(8 nxgroups)
getgd(               'f250111bd') #replaces f241021ad
pxosrdo2dd <-getlast('pxosrdo2dd')#4/6 shapefile
exists(              'x101')      #5/6  dfn drc
exists(              'z110')      #6/6  pva
save(list=c(
  'f250110ad', #rsq
  'f250111bd', #goe, estdt
  'f241021ad', #1/6
  'f241229bd', #2/6
  'f250107bd', #3/6
  'f250111bd',
  'x101',      #4/6
  'z110',      #5/6
  'pxosrdo2dd' #6/6
),file='250211app.Rdata')
