# x0=resS

# C131x <- #characteristics and summary
#   function(
#       static='resS',
#       rc6tx = rc6tG,
#       rescx = copy(rescG)
#       ) {
#     names(rescx)[which(names(rescx) == "estdt")] <- "rsi"
#     x2 <- # nx for this rc3
#       resS$f250618b %>%
#       .[grep(substr(rc6tx, 1, 3), rc6), unique(nx)]
#     resS$rsi <-
#       resS$rsi[nx %in% x2]
#     x4 <-
#       aestdt1(resS) %>%
#       resS$lab[., on = c(nx = "nx")]
#     x6 <-
#       aestdt1(rescx)[, -c("col")]
#     x7 <-
#       rbind(x4, x6)
#     x7 %>%
#       .[ii >= tslidex] %>%
#       dcast(., ii ~ lab, value.var = "xdot") %>%
#       .[, -"ii"] %>%
#       as.matrix(.) %>%
#       zoo(., estdty[, sort(unique(date))]) %>%
#       table.Stats(., digits = 3) %>%
#       t(.) %>%
#       .[, c(3, 6, 9, 16 , 14, 15, 16)] %>%
#       as.data.table(., keep.rownames = T) %>%
#       setnames(., c("rn", "min", "mean","tot", "max", "stdev", "skew", "kurtosis")) %>% 
#       .[unique(x7[,.(nx,lab)]),on=c(rn='lab')]%>%
#       C122a()[., on = c(nx = "nx")]
#   }
# C131x()
# 

x7 <- D131x()

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
  
   #transpose and apply rename/reorder used for 122
  t(x8)%>%
  .[,c(3,6,9,14,15,16)]%>%
  as.data.table(.,keep.rownames=T)%>%
    setnames(.,c('rn','min','mean','max','stdev','skew','kurtosis'))%>%
    C122a()[.,on=c(lab='rn')]
  
  
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
