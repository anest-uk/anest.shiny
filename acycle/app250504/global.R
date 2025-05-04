library(broom)
library(bslib)
library(car) #linear hypothesis test
library(colorspace)
library(data.table)
library(devtools)
library(DT)
library(ggplot2)    
library(ggrepel)   
library(grid)
library(gt)
library(gtExtras)
library(leaflet)
library(lubridate)
library(magrittr)   
library(PerformanceAnalytics)
library(scales)
library(shinyvalidate)
library(shinyWidgets)
library(sp)
library(zoo)
library(shiny)
library(plotly)
library(gridlayout)
source('c-cleanlib.R')      
source('rctree.R')
load('250503app.RData')


#--------------------------------ui
gridheight="630px"
gridheight2="830px"
gridheight3="1020px"
colx <<- cobalt()[c(4,2,1)]
sf <<- 3
pgmc <<- 'grey50'

#--------------------------------Pseudo=Control----2----
hoflC   <<- c('house','flat','all')[3]#,
itriC   <<- c('.0'=1,'.1'=2,'.5'=3)[2]#, #Trim ---
neusC   <<- c('new','used','all')[3]#,
rc3coC  <<- c('B--','E--','AL-')#,  #comp
rc6cuC  <<- c('SW-1Y-','SW-7--','W--8--','WC-2E-')
tbinC   <<- c(lo=1,hi=2,an=3)[2]#,  #lo hi an ---
typeC   <<- c('A','L','N','C')[2]#, #All Local National ---
typerC  <<- typeC
nfig2   <<- -1 #for ppm2
nfig3   <<- 4 #for frac
verbose <<- T

zerorefC =F#, #set reference asset NULL
showtradetriangle=F


