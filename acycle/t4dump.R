setwd("C:/Users/Giles/anest.repo/anest.shiny/acycle")
ipanel <- 2
rsi=list(z221,z321,z421)[[ipanel]] #---global 
prj=list(z223,z323,z423)[[ipanel]] #---global
bwe=list(z224,z324,z424)[[ipanel]] #---global
pva=z110 #-----------------------------global
pxosrdo2dd=getlast('pxosrdo2dd')

nn<-c('geon','z221','z321','z321a','z321b','z421','z223','z323','z423','z224','z324','z424','z110','pxosrdo2dd') #z321a is np, annual  321b is annual more outlier removal .3
nn <- c(nn,c(
  'z321c',
  'z321d', #local tertile ann RSI
  'z323c',  #local tertile drc PRJ
  'f240915ad'
  ))
save(list=nn,file='t4dump.Rdata')
