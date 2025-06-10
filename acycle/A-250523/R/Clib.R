C112a <- #local optimum kss for all rc6
  function(
    x0=f250509ed,
    nn='f250509ed'#static
    ) {
    x1 <-  #local solution for all rc6
      x0$geo %>%
      .[grep("^L", lab)] %>%
      x0$kfoldsse[., on = c(nx = "nx",rc6='rc9'), nomatch = NULL] %>% # local only
      .[,rc3:=substr(rc6,1,3)]%>%
      .[order(rc6,ssek)]%>%
      .[,.SD[1],rc6]
    x1
  }

C112b <- #local lab for all rc6 in rc3tx [join on col here?]
  function(
    nn='f250509ed', #static
    x0=C112a() #over
  ){
    x1 <- #local solution set for rc3t
      x0%>%
      .[,.(rc3,nx,lab,i.n=substr(lab,5,7))]%>%
      #.[,.(rc3,nx,lab,i.n=paste0(substr(lab,7,7),'.',substr(lab,5,5)))]%>%
      unique(.)%>%
      data.table(i.n=c('1.3','1.2','1.1','2.3','2.2','3.3'),qq=c(1/6,1/4,1/2,1/2,3/4,5/6))[.,on=c(i.n='i.n'),mult='all']%>%
      .[order(qq)]
    x1
  }

C112c <-
  function(
    rc6tx=rc6tG,
    coltabx=coltab
  ) {
    x1 <- 
      rbind(
        C112b()[rc3==substr(rc6tx,1,3)][order(qq)][c(1,.N)]#, #top and bottom
        #C112b()[lab==C112a()[rc6==rc6tx,lab]] #target #clutter/confused if not a tertile-optimum
      )%>%
      unique(.)%>%
      coltab[.,on=c(code='i.n')]%>%
      #.[,legendlab:=ifelse(lab==C112a()[rc6==rc6tx,lab],'target',lab)]%>%
      .[,legendlab:=lab]%>%
      .[order(-qq)]
    x1
  }

C112d <- 
  function(
    rc6tx=rc6tG, #rc6t
    x0=f250509ed,#kfx
    x1=estdtccG, #cus
    x2=C112c(rc6tx=rc6tx)#local for plot
  ) {
    x3 <- 
      intersect(names(x0$estdt),names(x1))%>%
      #setdiff(.,'lab')%>%
      sort(.)
    x4 <- 
      rbind(
        x0$estdt[x2,on=c(nx='nx')][,x3,with=F],
        estdtccG[,x3,with=F]
      )
    x5 <- 
      rbind(
      x2[,.(dark,lab,legendlab)],
      data.table(dark='brown',lab='CU00',legendlab='custom'))
    #print(x2)
    x6 <-
      x4[x5,on=c(lab='lab')]%>%
      .[,.(date,ii,lab,legendlab,x,col,dark)]
    x6
  }

#C112d()[,.N,legendlab]
C122 <- # combine rss and P characteristics
  function(rssx, z110x) {
    x0 <-
      z110x[rssx, on = c(rcx = "rc6")] %>%
      .[
        , .(
          frac = round(sum(nid) / z110x[nchar(rcx) == 6, sum(nid)], nfig3),
          nid = sum(nid),
          ppm2max = round(max(ppm2), nfig2),
          ppm2min = round(min(ppm2), nfig2),
          p = round(sum(pv) / sum(m2), nfig2)
        ),
        lab
      ] %>%
      .[rssx[, .(R2rsi = 1 - sum(ssek) / sum(sstr)), lab], on = c(lab = "lab")] %>%
      .[, .(
        lab = substr(lab, 1, 5),
        frac,
        R2rsi = round(R2rsi, 3),
        pnum = p,
        p = prettyNum(round(p, nfig3), big.mark = ","),
        p.cus = paste0(prettyNum(round(ppm2min, nfig2), big.mark = ","), "-", prettyNum(round(ppm2max, nfig2), big.mark = ","))
      )]
  }

# C132a <- function( #-------------------------.#----
#                  geox = geoqG,                    #estdt { gx lab nx qtile rc3 rc6 }
#                  steprip = stepripG,
#                  estdtlx = estdtlG, # only used for its date(ii) relation
#                  tmin = 20) { # tmin=input$tslider
#   x0 <-
#     geox[, grepstring(rc6)] %>%
#     coread2(., steprip) %>% # or rc6tC
#     .[, .(N = .N, mean = round(mean(as.numeric(retsa)), 4)), .(buy = substr(as.Date(buydate), 1, 4), sell = substr(as.Date(selldate), 1, 4))] %>%
#     .[(buy >= estdtlx[ii >= tmin, substr(min(as.character(date)), 1, 4)])]
#   x1 <-
#     x0 %>%
#     dcast(.,
#       buy ~ sell,
#       value.var = "mean" # the value is unique so any aggregator function is ok
#     )
#   for (i in 2:length(x1)) x1[[i]] <- ifelse(is.na(x1[[i]]), "", as.character(round(x1[[i]], 3)))
#   x2 <-
#     x0 %>%
#     dcast(.,
#       buy ~ sell,
#       value.var = "N"
#     )
#   for (i in 2:length(x2)) x2[[i]] <- ifelse(is.na(x2[[i]]), "", x2[[i]])
#   x3 <- list(x1, x2)
#   x3
# }
# 

C132a <- function( #-----132 trade summary(2)----
                 geox = geoqG,
                 steprip = stepripG,
                 estdtlx = estdtlG, # only used for its date(ii) relation
                 tmin = 20) { # tmin=input$tslider
  x0 <-
    geox[, grepstring(rc6)] %>%
    coread2(., steprip) %>% # or rc6tC
    .[, .(N = .N, mean = round(mean(as.numeric(retsa)), 4)), .(buy = substr(as.Date(buydate), 1, 4), sell = substr(as.Date(selldate), 1, 4))] %>%
    .[(buy >= estdtlx[ii >= tmin, substr(min(as.character(date)), 1, 4)])]
  x1 <-
    x0 %>%
    dcast(.,
      buy ~ sell,
      value.var = "mean" # the value is unique so any aggregator function is ok
    )
  for (i in 2:length(x1)) x1[[i]] <- ifelse(is.na(x1[[i]]), "", as.character(round(x1[[i]], 3)))
  x2 <-
    x0 %>%
    dcast(.,
      buy ~ sell,
      value.var = "N"
    )
  for (i in 2:length(x2)) x2[[i]] <- ifelse(is.na(x2[[i]]), "", x2[[i]])
  x3 <- list(x1, x2)
  x3
}

