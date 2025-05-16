library(shiny)
#-------------------------------load data, dataprep is in v164/250503 rdata for app.R (probably...)
load("data/.RData") # contains "f241021ad"  "f241208fd"  "pxosrdo2dd" "x101"       "z110"

print("app.R global env:")
print(globalenv())
print(ls(globalenv()))

f241021adG <- f241021ad
pxosrdo2ddG <- pxosrdo2dd
x101G <- x101
z110G <- z110