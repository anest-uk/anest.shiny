
library(shiny)
library(data.table)

x1 <- data.table(
  typical.area=c(
    'Kensington, W8',
    'Bayswater, W2',
    'Wimbledon, SW19',
    'Kingston',
    'Guildford',
    'Bristol',
    'Birmingham',
    'Doncaster',
    'Liverpool',
    'Middlesborough, TS1'
  )
)[,n:=1:10]

x3 <-
  structure(c(9625, 9835, 10010, 10148, 10403, 10684, 10822, 10937, 
              10993, 11552, 11799, 11926, 12028, 12141, 12210, 12278, 12356, 
              12433, 12526, 12576, 12719, 12916, 13337, 13538, 13655, 13878, 
              14303, 14418, 14682, 15066, 15285, 15640, 16075, 16237, 16412, 
              16635, 16780, 16887, 17035, 17247, 17548, 18061, 18761, 19054, 
              19691+31), class = "Date")
x3 <- as.character(x3)
x3a <- c('1994-12-31',x3[-length(x3)])

xtab3 <-
  structure(
    list(t = 1:45, 
         period.end = structure(
           c(9625, 9835, 
             10010, 10148, 10403, 10684, 10822, 10937, 10993, 11552, 11799, 
             11926, 12028, 12141, 12210, 12278, 12356, 12433, 12526, 12576, 
             12719, 12916, 13337, 13538, 13655, 13878, 14303, 14418, 14682, 
             15066, 15285, 15640, 16075, 16237, 16412, 16635, 16780, 16887, 
             17035, 17247, 17548, 18061, 18761, 19054, 19691+31), 
           class = "Date"), 
         days = c(495, 210, 175, 138, 255, 281, 138, 115, 56, 559, 
                  247, 127, 102, 113, 69, 68, 78, 77, 93, 50, 143, 197, 421, 
                  201, 117, 223, 425, 115, 264, 384, 219, 355, 435, 162, 175, 
                  223, 145, 107, 148, 212, 301, 513, 700, 293, 637)
    ), class = "data.frame", 
    row.names = c(NA,-45L)
  )
xtab3[,'period.end'] <- as.character(xtab3[,'period.end'])
names(xtab3) <- c('t','interval end','days')

x2a <-
  structure(
    list(np = 
           c(paste0('PCL=',10),9:1), 
         ppm2.2024 = c("19,900-28,100", "13,500-19,400", "7,500-13,400",
                       "6,600-7,500", "4,600-6,600", "3,200-4,600", "2,500-3,200", 
                       "1,600-2,500", "1,100-1,600", "800-1,100"), 
         fraction = c(0.19, 0.39, 
                      3.69, 1.92, 17.00, 
                      28.78, 21.27, 22.78, 3.78, 0.19)
    ), 
    class = "data.frame", 
    row.names = c(NA,-10L)
  )
x2 <- cbind(x2a[,1],x1[,1],x2a[,-1])
names(x2) <- c('Price bin','Typical area','£/m2 2024','% of properties')

ui <- fluidPage(
  tags$head(tags$link(rel="shortcut icon", href="fav.ico")),
  
  titlePanel(# Application title
    title=htmltools::div(
      htmltools::p(),
      htmltools::img(src="logo.png",style="width:42px; height:42px"),
      htmltools::p(),
      htmltools::p(),
      "The Residential Wave, 16-20 years period"
    ),
    windowTitle = "Resi Wave"
  ),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", # slider input for number of bins 
                  "Time interval:", 
                  min = 1, 
                  max = 45, 
                  value = 1, 
                  animate = animationOptions(interval = 1000)
      ),
      span(textOutput("date"),style = " font-size:16px"),
      hr(),
      textOutput("thetarad"),
      textOutput("thetadeg"),
      hr(),
      h5("Slider selects time interval 1 to 45"),
      h5("Map shows the model return spread in selected time interval; spread = return minus national index"),
      h5("Bar chart shows same data as map"),
      h5("Line chart shows same data cumulated"),
      h5("Radial chart shows computation: factor as arrow, index betas as points"),
      h5("Value/momentum chart shows cyclical factor return and cumulative return on local £/m2 tertiles (T1=low, T3=high)"),
      h5("Model returns are for factors 2,3"),
      hr(),
      h4("Price bins for 10 repeat sales indices"),
      tableOutput("table4"),
      h5("All properties seasonally adjusted and repriced from last sale to present using repeat sales index"),
      h5("Present value and floor area are aggregates for all tracked units in a postcode district"),
      h5("Breakpoints are chosen to space beta equally on the semicircle"),
      hr(),
      h4("Time intervals"),
      tableOutput("table5")
    ),
    mainPanel(
      fluidPage(
        fluidRow(
          column(imageOutput("leafplot"), width = 7),
          column(imageOutput("fig"), width = 3)
        )
      )
    )
  )
)

server <- function(input, output) {
  
  output$thetarad <- 
    renderText(
      paste0(
        'Cycle Phase = ',
        as.character(round((-0.2211737*input$n-1.2370456+4*pi)-pi/2,2)),
        ' radians  (',
        as.character(round(((-0.2211737*input$n-1.2370456+4*pi)-pi/2)*180/pi)),
        ' degrees)'
      )
    )
  output$date <- renderText(paste0('Time interval ',input$n,' is from ',x3a[input$n],' to ',x3[input$n]))
  output$table4 <- 
    renderTable(
      {x2},
      digits=2,
      comma=T,
      hover=T,
      auto=T
    )
  output$table5 <- 
    renderTable(
      {xtab3},
      hover=T,
      auto=T
    )
  plotdim <- '90%'
  output$leafplot <- 
    renderImage({
      filename <- 
        normalizePath(
          file.path(
            'leaf/',
            paste('leaf', input$n, '.png', sep='')
          )
        )
      list(
        src = filename,
        width=plotdim
      )
    }, deleteFile = FALSE)
  output$fig <- 
    renderImage({
      filename <- 
        normalizePath(
          file.path(
            'leaf/',
            paste('pan', input$n, '.png', sep='')
          )
        )
      list(
        src = filename,
        width='130%'
      )
    }, deleteFile = FALSE)
  
}

shinyApp(
  ui = ui, 
  server = server
)
