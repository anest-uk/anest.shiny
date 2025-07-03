#x0=resS

D131x <- 
  function(
    x3=copy(resS),
    x1=copy(rescG)
  ){
    names(x1)[which(names(x1)=='estdt')] <- 'rsi'
    #from d122x
    x2 <-  #nx for this rc3
      x3$f250618b%>%
      .[grep(substr(rc6tx,1,3),rc6),unique(nx)]
    x3$rsi <- 
      x3$rsi[nx%in%x2]
    x4 <- 
      aestdt1(x3)%>%
      x3$lab[.,on=c(nx='nx')]
    x5 <- 
      copy(rescG)
    names(x5)[which(names(x5)=='estdt')] <- 
      'rsi'
    x6 <- 
      aestdt1(x1)[,-c('col')]
    x7 <- 
      rbind(x4,x6)
    x7
  }
estdty <- D131x()



# x6[,plot(x)]
# x6[,summary(xdot)]

#D131 <- function( #-------------131 summary----
                  #estdty = estdtxG#,
                  tslidex = tslideG#) #{
  x8 <-
    x7 %>%
    .[ii >= tslidex] %>%
    dcast(., ii ~ lab, value.var = "xdot") %>%
    .[, -"ii"] %>%
    as.matrix(.) %>%
    zoo(., estdty[, sort(unique(date))]) %>%
    table.Stats(., digits = 3) 
  
  x8 #transpose and apply rename/reorder used for 122
  t(x8)
  
  %>%
    data.table(., keep.rownames = T) %>%
    `[`(., i = -c(1, 2, 7, 11, 12, 13)) %>%
    gt::gt(.) %>%
    cols_label(
      rn = gt::html("Log return<br>summary")
    ) %>%
    gt::tab_footnote(
      footnote = f241108a(typeC, tbinC)[[1]]
    )# %>%
    # gt::tab_footnote(
    #   footnote = f241108a(typeC, tbinC)[[2]]
    # )
  x
#}
