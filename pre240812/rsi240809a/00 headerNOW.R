#1 run HEADER NOW or HEADER PIT
#  run DATAUNPACK to unpack csv files to subfolders (needed once only)
#2 run IMAGELOAD for pre-solved tables
#3 run SOLVE to solve 
#4 run TAB for tables .csv
#5 run FIG for figures .tif
# x101= #annual, tweaked
#   seq.Date(
#     from=as.Date('1994-12-31'),
#     to=as.Date('2024-03-31'),
#     by='y'
#   )+
#   c(
#     rep(0,14),
#     59, #extend 2008 2 months to 2009-02-28 (GFC end)
#     rep(0,14),
#     +31+29+31) #240129 : 231131
#    -184) #in 2023 end 2023-05-31 and discard
x1= #annual, tweaked
  seq.Date(
    from=as.Date('1994-12-31'),
    to=as.Date('2023-12-31'),
    by='y'
  )+
  c(
    rep(0,14),
    59, #extend 2008 2 months to 2009-02-28 (GFC end)
    rep(0,15)
    )
x101  <- c(x1,as.Date('2024-05-31'))
steprip <- 'now\\ver001\\03rip'
stepsip <- 'now\\ver001\\02sip'
stepprav1 <- 'now\\ver001\\07pra'
stepprav2 <- 'now\\ver002\\07pra'
stepprav3 <- 'now\\ver003\\07pra' #bmn
mnem <- 'now'
#source('c-cleanlib.r')
