library(shiny)
library(leaflet)
library(bslib)
library(gt)
library(ggplot2)
library(shinyWidgets)

#---fail test box
library(sp)
library(sf)
library(data.table)
library(magrittr)
library(gt)
source('c-cleanlib.r')
#source("helpers.R")
load('pxosrdo2dd.Rdata')
#-----

#source('153 DINO SLOW PREAMBLE.R')
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

cities <- data.frame(
  continent = c("America", "America", "America", "Africa",
                "Africa", "Africa", "Africa", "Africa",
                "Europe", "Europe", "Europe", "Antarctica"),
  country = c("Canada", "Canada", "USA", "Tunisia", "Tunisia",
              "Tunisia", "Algeria", "Algeria", "Italy", "Germany", "Spain", NA),
  city = c("Trois-Rivières", "Québec", "San Francisco", "Tunis",
           "Monastir", "Sousse", "Alger", "Oran", "Rome", "Berlin", "Madrid", NA),
  stringsAsFactors = FALSE
)

# app
ui <- fluidPage(
  tags$h2("treeInput() example"),
  fluidRow(
    column(
      width = 4,
      treeInput(
        inputId = "ID1",
        label = "Select cities:",
        choices = create_tree(cities),
        selected = "San Francisco",
        returnValue = "text",
        closeDepth = 0
      ),
      verbatimTextOutput("res1")
    ),
    column(
      width = 4,
      treeInput(
        inputId = "ID2",
        label = "Select cities:",
        choices = create_tree(cities),
        selected = "San Francisco",
        returnValue = "text",
        closeDepth = 1
      ),
      verbatimTextOutput("res2")
    ),
    column(
      width = 4,
      treeInput(
        inputId = "ID3",
        label = "Select cities:",
        choices = create_tree(cities),
        selected = c("San Francisco", "Monastir"),
        returnValue = "text",
        closeDepth = 2
      ),
      verbatimTextOutput("res3")
    )
  )
)

server <- function(input, output, session) {

  output$res1 <- renderPrint(input$ID1)
  output$res2 <- renderPrint(input$ID2)
  output$res3 <- renderPrint(input$ID3)

}


shinyApp(ui, server)
