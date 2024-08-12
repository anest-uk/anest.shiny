#1 run HEADER NOW or HEADER PIT
#2 run IMAGELOAD for pre-solved tables
#3 run SOLVE to solve 
#4 run TAB for tables .csv
#5 run FIG for figures .tif
rdatafile <- dir(paste0(mnem,'/'))%>%.[grep('Rdata$',.)]%>%sort(.) #all Rdata
  if(length(rdatafile)>0) {
    rdpath <- rdatafile%>%max()%>%file.path(mnem,.) #max(filename)!=latest
    print(mnem)
    print(paste0('load latest rdata ',rdpath))
    load(rdpath)
  } else {
    print(paste0('no rdata loaded for mnem=',mnem))
  }
