#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#----------------------------------shared across sessions
setwd("C:/Users/Giles/anest.repo/anest.shiny/acycle")

#---------------------CRAN package
library(shiny)
library(bslib)
library(gridlayout)
library(shinyWidgets)
library(ggplot2)
library(data.table)
library(magrittr)

#---------------------function lib
source('applib.R')

#---------------------data
vbl <- #saved in rdata with own name
  c(
    'z421',
    'z321'
    )
rmifgl(vbl)
for(i in seq_along(vbl)) {load(paste0(vbl[i],'.Rdata'))}


#-------------------------------------------------------------------------------ui
ui <- page_sidebar(
  title = "Index",
  sidebar = sidebar(
    textInput("tgtrc6",label='Target RC6',value='WV-1--'),
    width=140
    ),
  
  navset_card_underline(
    nav_panel(
      title = "National", 
      #grid_page(
      card(
        p("graphic"),
        plotOutput('estdt.nat.p')
        ),
      card(p(
        "map")
        ),
      card(
        p("table"),
        tableOutput('estdt.nat.t')
        )
      #)
    ),
    nav_panel(
      title = "Local", 
      p("graphic"),
      card(p("graphic")),
      p("map"),
      card(p("map")),
      p("table")
    ),
    nav_panel(title = "Custom", 
              sidebarLayout(
                sidebarPanel(
                  card(
                    sliderInput("bins",
                                "Placeholder",
                                min = 1,
                                max = 50,
                                value = 30)
                  ),
                  width=2
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                  p("map")
                )#,
                # tableOutput('table')
              )
    )
    
  )
)
#-------------------------------------------------------------------------------server
server <- function(input, output) {
  #----------------------------------per-session
  #national estdt
      output$estdt.nat.t <- renderTable(
        z321$ses$estdt[nx==z321$geo[rc9==input$tgtrc6,nx]][,.(nx,date=as.Date(date1),days,xdot,xdotse,x,xse)]
      )
  #national timeseries
      output$estdt.nat.p <- renderPlot(
        z321$ses$estdt[nx==z321$geo[rc9==inputtgtrc6,nx]]%>%
          ggplot(.,aes(date1,x))+
          geom_line()
      )
      
}

# Run the application 
shinyApp(ui = ui, server = server)
