app_dirG <- normalizePath("c:/users/giles/anest.repo/anest.shiny/acycle/A-250523") #normalizePath(".")  is better, but does not work for interactive debug (2 dir up)
setwd(app_dirG)#does nothing in the app, is here for interactive/debug
source(file.path(app_dirG, "R", "global.R"))
source(file.path(app_dirG, "R", "a-lib.R"))
