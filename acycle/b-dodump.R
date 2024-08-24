# ver <- 153
# setwd(paste0("c:/users/giles/anest.repo/anest.prod/v",ver))
# getwd() #ok
# dir() #ok
source('C:/Users/Giles/anest.repo/anest.lib1/R/aa010util.r') #note: in anest.lib1
source('C:/Users/Giles/anest.repo/anest.lib2/R/lib2.r')
source('C:/Users/Giles/anest.repo/anest.step2/R/step2.r') #add nname() as a function
source('C:/Users/Giles/anest.repo/anest.lfm2/R/aapca.r')
print(paste0('In ',getwd(),' overloading pcaest function using /pcaest.r TAKE CARE'))
source('pcaest.r') #local copy, initialises pcaest()$tantheta
x1 <- fread('a-funlist.R',header=F)[,V1]%>%
  sort(.)
x1 <- intersect(x1,ls())
dump( #ok
  list=x1,
  file='c-cleanlib.r'
  )

