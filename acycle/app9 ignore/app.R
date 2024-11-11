#---libraries from app4
#source('CRANlibload.R') #causes failure once deployed
# library               ----
LIBRARY_CALL #at start to avoid overload
LIBRARY_CALL #linear hypothesis test
library(magic)
library(broom)
library(bslib)
library(car)
library(colorspace)
library(data.table)
library(devtools)
library(ggplot2)
library(ggrepel)
library(grid)
library(gridlayout)
library(gt)
library(gtExtras)
library(htmltools)
library(leaflet)
library(lubridate)
library(magrittr)
library(PerformanceAnalytics)
library(plotly)
library(scales)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(sp)
library(zoo)
library(shiny)
library(ggplot2)
library(bslib)
library(gridlayout)
library(DT)
#---



#local files            -------------------------------------------------------------------
#rdata
load('t4dump.Rdata',envir=globalenv())
#r data
source('f240915ad.R') #manually generated and data.table(.) added
source('rctree.R') #f240824b() : rctree
source('headerscript.R') #geo and dfn
#r code
source('../c-cleanlib.R')
print(getwd())

#from app4              ---------------------------------------------------------------------
print(getwd())
pgmt='dotted'
pgmc='grey50'
pgms=.2
lightenx <- .7
gridheight="630px"

dfnx <- seq.Date( #same as t4dump but define in code
  from=as.Date('1994-12-31'),
  to=as.Date('2024-12-31'),
  by='y')
dfnx[length(dfnx)] <- 
  as.Date('2024-07-31')
steprip <- '03rip/'
nfig1 <- 3 #for T2
nfig2 <- -1 #for ppm2
nfig3 <- 4 #for frac
options(scipen=999) #no scientific format
#function lib           ------------------------------------------------------------------
rcx <<- c('SW-','AL-','M--')
#rc6x <- "SW-3--"
treex <- dir('03rip/')[1]%>%sort(.)%>%substr(.,1,6)#''

tabwihead <- "Index return"
tabchhead <- "Characteristics"
tabsuhead <- "Returns summary"
tabachead <- "Accuracy"

#---function: map colours
lightenx=.4
palna <- #national
  c(lightenx,0)%>% #for non-target, lighten
  lighten('green',.)%>%
  leaflet::colorNumeric(palette=.,domain=0:1)
# palcu <- #custom
#   lightenx%>% #lighten
#   lighten('green',.)%>%
#   leaflet::colorNumeric(palette=.,domain=0:1)

ui <- page_navbar()
#--------options 
#1 replace the reactives that compute results for rendering, so they take inputs from f14a
#2 extract the code from here to run as a script to avoid all the shiny loading
#3 split the reactives here into (a) data prep (b) display and focus on the data prep
#next: try an example of extracting to a script the input-read, generating output
#then replace with equivalent using f14a
#server ------------------------------------------------------------------------
server <- function(input, output) {#server                                   -------------------------------------
  #Index
  #Reactive
  #--Reactive
  #input-read
 
  
  #--Render

  # local----
  output$inloma <- renderLeaflet(#local map                           1.1    ----
                                 z321d$geo%>% #lab=rc3-q and both parts are used here
                                   .[substr(rc9,1,3)==substr(Rintarc(),1,3)]%>% #rc3 match
                                   .[,.(
                                     rc6=rc9, #name it correctly
                                     col=
                                       cobalt()[c(1,2,4)]%>% #tertile colours ascending freq
                                       lighten(.,lightenx*1.5)%>% #lighten all
                                       .[4-as.numeric(substr(lab,4,4))], #high
                                     lab #include label 
                                   )]%>% 
                                   .[
                                     rc6==Rintarc(), #target district
                                     col:=cobalt()[c(4,2,1)][as.numeric(substr(lab,4,4))]  #overwrite target: colour without lightening
                                   ]%>% 
                                   f240810b(.) #fields rc6,col
  )
  
  #custom

 
}

shinyApp(ui, server)

