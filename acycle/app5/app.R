#---libraries from app4
#source('CRANlibload.R') #causes failure once deployed
library(magic) #at start to avoid overload

library(broom)
library(bslib)
library(car) #linear hypothesis test
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
#---

library(shiny)
library(ggplot2)
library(bslib)
library(gridlayout)
library(DT)

#---------------------------------------from app4
pgmt='dotted'
pgmc='grey50'
pgms=.2
lightenx <- .7
load('t4dump.Rdata',envir=globalenv())
# dfnx <- seq.Date(from=as.Date('1994-12-31'),to=as.Date('2024-12-31'),by='y')
# dfnx[length(dfnx)] <- as.Date('2024-07-31')
#---function lib
source('c-cleanlib.R')
source('rctree.R') #f240824b() : rctree
source('headerscript.R') #geo and dfn
rcx <<- c('SW-','AL-','M--')
rc6x <- "SW-3--"
#---function: map colours
palna <- 
  c('red','magenta')%>%
  lighten(.,lightenx)%>%
  leaflet::colorNumeric(palette=.,domain=0:1)
palna <- 
  c(.3,0)%>%
  lighten('green',.)%>%
  leaflet::colorNumeric(palette=.,domain=0:1)
palcu <- 
  c('green')%>%
  lighten(.,.4)%>%
  leaflet::colorNumeric(palette=.,domain=0:1)
#---------------------------------------
ui <- 
  page_navbar(##################################################################
              sidebar = sidebar(
                Title="Sidebar",
                layout = c("customrc6",
                           "nationalrc6",
                           "localrc3",
                           "nationalnp"
                ),
                grid_card(
                  area="nationalrc6",
                  card_body(
                    textInput(
                      inputId = "innata",#innata
                      label = "Target district",
                      value = "SW3"
                    )
                  )
                )
                ,
                conditionalPanel(condition="input.tabs == 'Custom'",
                                 grid_card(
                                   area="customrc6",
                                   card_body(
                                     actionButton(inputId = "sicobu", label = "Compute")
                                     ,
                                     treeInput( #districts
                                       inputId = "incusetr",
                                       label = "Select peers:",
                                       choices = create_tree(f240824b(unique(substr(dir('03rip/'),1,3)))),
                                       selected = "rc6x", #this
                                       returnValue = "text",
                                       closeDepth = 1
                                     )
                                     
                                   )
                                 )
                )
                ,
                #conditionalPanel(condition="input.tabs == 'National' || input.tabs == 'Local'",
                conditionalPanel(condition="input.tabs == 'Local'",
                                 grid_card(
                                   area="localrc3",
                                   card_body(
                                     #textOutput("rc3"),
                                     textOutput("loter")
                                   )
                                 )
                ),
                conditionalPanel(condition="input.tabs == 'National'",
                                 grid_card(
                                   area="nationalnp",
                                   card_body(
                                     textOutput("nationalnp")
                                   )
                                 )
                )
                
              ),
              footer = tags$div("powered by Acycle"),
              title = "Acycle",
              selected = "Index",
              collapsible = TRUE,
              theme = bslib::bs_theme(),
              nav_panel(#--------------------------------------------------------------index
                title = "Index",
                tabsetPanel(
                  id="tabs",
                  
                  
                  nav_panel(#---------------------------------------------------index custom
                    title = "Custom",
                    tabsetPanel(
                      nav_panel(#-----------------------------------------index custom index
                        title = "Index",
                        grid_container(
                          layout = c(
                            "incuinti incuinma",
                            "incuinwi incuinch",
                            "incuinsu incuinbl"
                          ),
                          row_sizes = c(
                            "1fr",
                            "1fr",
                            "1fr"
                          ),
                          col_sizes = c(
                            "1fr",
                            "1fr"
                          ),
                          gap_size = "10px",
                          grid_card(
                            area = "incuinti", #time-series
                            full_screen = TRUE,
                            card_header("Index time-series"),
                            card_body(
                              plotOutput("incuinti"),
                            )
                          ),
                          grid_card(
                            area = "incuinma", #map 
                            full_screen = TRUE,
                            card_header("Map"),
                            card_body(
                              leafletOutput('incuinma'),
                              max_height="500px"
                            )
                          ),
                          grid_card(
                            area = "incuinwi", #winding
                            full_screen = TRUE,
                            card_header("Index return"),
                            card_body(
                              gt_output('incuinwi')
                            )
                          ),
                          grid_card(
                            area = "incuinch", #characteristics
                            full_screen = TRUE,
                            card_header(
                              "Characteristics"
                            ),
                            card_body(
                              gt_output(outputId = "incuinch")
                            )
                          ),
                          grid_card(
                            area = "incuinsu", #summary
                            full_screen = TRUE,
                            card_header("Time-series summary"),
                            card_body(
                              gt_output(outputId = "incuinsu")
                            )
                          ),
                          grid_card(
                            area = "incuinbl", #blank-unused
                            full_screen = TRUE
                          )
                        )
                      ),
                      nav_panel(#--------------------------------------index custom settings
                        title = "Settings",
                        grid_container(
                          layout = c(
                            "incusetb", 
                            "incusege", 
                            "incusefi", 
                            "incuseou",
                            "incusety", 
                            "incusecr",
                            "incuseag"
                          ),
                          row_sizes = c(
                            "1fr",
                            "1fr",
                            "1fr",
                            "1fr",
                            "1fr",
                            "1fr",
                            "1fr"
                          ),
                          col_sizes = c(
                            ".3fr"
                          ),
                          gap_size = "10px",
                          grid_card(
                            area = "incusetb", #time bin 
                            card_body(
                              radioButtons(
                                inputId = "incusetb",
                                label = "Time interval",
                                choices = list(
                                  "Annual" = "a" #only one for now
                                ),
                                selected='a',
                                width = "100%"
                              )
                            )
                          ),
                          grid_card(
                            area = "incusety", #type
                            card_body(
                              radioButtons(
                                inputId = "incusety",
                                label = "Type",
                                choices = list(
                                  "All" = "a", #this
                                  "House" = "h", 
                                  "Flat" = "f"
                                ),
                                width = "100%"
                              )
                            )
                          ),
                          grid_card(
                            area = "incuseag", #age
                            card_body(
                              radioButtons(
                                inputId = "incuseag",
                                label = "Age",
                                choices = list(
                                  "All"="a", #age
                                  "New" = "n", 
                                  "Used" = ""
                                ),
                                width = "100%"
                              )
                            )
                          ),
                          grid_card(
                            area = "incusege", #gep
                            card_body(
                              radioButtons(
                                inputId = "incusege",
                                label = "Geo definition",
                                choices = list(
                                  "Tree picker" = "t", #this
                                  "Custom file" = "c"
                                ),
                                width = "100%"
                              )
                            )
                          ),
                          
                          grid_card(
                            area = "incusefi", #file
                            full_screen = TRUE,
                            card_header(
                              "File upload"
                            ),
                            card_body(
                              conditionalPanel(condition = "input.incusege == 'c'" ,
                                               fileInput("incusefi", "Upload a file")
                              )
                            )
                          ),
                          grid_card(
                            area = "incuseou", #outlier
                            card_body(
                              radioButtons(
                                inputId = "incuseou",
                                label = "Outlier reject %",
                                choices = list(
                                  "0" = "b", #this
                                  "10" = "a", 
                                  "30" = "value3"
                                ),
                                width = "100%"
                              )
                            )
                          ),
                          grid_card(
                            area = "incusecr",
                            card_body(
                              radioButtons(
                                inputId = "incusecr",
                                label = "Cross-validation",
                                choices = list("none" = "a", "5-fold" = "b"),
                                width = "100%"
                              )
                            )
                          )
                          
                          
                        )
                      )
                    )
                  ),#navpan 
                  nav_panel(#-------------------------------------------------index national
                    title = "National",
                    grid_container(
                      layout = c(
                        "innati innama",
                        "innawi innach ",
                        "innasu .      " #innata
                      ),
                      row_sizes = c(
                        "1fr",
                        "1fr",
                        "1fr"
                      ),
                      col_sizes = c(
                        "1fr",
                        "1fr"
                      ),
                      gap_size = "10px",
                      grid_card( #1,1
                        area = "innati",
                        full_screen = TRUE,
                        card_header("Index time-series"),
                        card_body(
                          plotOutput('innati'),
                          max_height="500px"
                        )
                      ),
                      grid_card(#1,2
                        area = "innama",
                        full_screen = TRUE,
                        card_header("Map"),
                        card_body(
                          leafletOutput('innama'),
                          max_height="500px"
                        )
                      ),
                      grid_card(#2,1
                        area = "innawi",
                        full_screen = TRUE,
                        card_header("Winding"),
                        div(
                          gt_output('innawi'),
                          max_height="400px"
                        )
                      ),
                      grid_card(#2,2
                        area='innach',
                        full_screen = TRUE,
                        card_header("Price band characteristics"),
                        card_body(
                          gt_output('innach')
                        )
                      ),
                      grid_card(#3,1
                        area = "innasu",
                        full_screen = TRUE,
                        card_header("Time-series summary"),
                        card_body(
                          gt_output('innasu')
                        )
                      )
                      
                    )
                  ),
                  nav_panel(#----------------------------------------------------index local
                    title = "Local",
                    grid_container(
                      layout = c(
                        "inloti inloma",
                        "inlowi inloch",
                        "inlosu inlobl"
                      ),
                      row_sizes = c(
                        "1fr",
                        "1fr",
                        "1fr"
                      ),
                      col_sizes = c(
                        "1fr",
                        "1fr"
                      ),
                      gap_size = "10px",
                      grid_card(#1,1
                        area = "inloti",
                        full_screen = TRUE,
                        card_header(
                          "Index time-series                                                                                                                                                                                                                                                                                                  "
                        ),
                        card_body(##################################
                                  plotOutput('inloti'),
                                  max_height="500px"
                        )
                      ),
                      grid_card(#1,2
                        area = "inloma",
                        full_screen = TRUE,
                        card_header("Map"),
                        card_body(##################################
                                  leafletOutput('inloma'),
                                  max_height="500px"
                        )
                      ),
                      grid_card(#2,1
                        area = "inlowi",
                        full_screen = TRUE,
                        card_header("Winding"),
                        card_body(##################################
                                  div(
                                    gt_output('inlowi'),
                                    max_height="400px"
                                  )
                        )
                      ),
                      grid_card(#2,2
                        area = "inloch",
                        full_screen = TRUE,
                        card_header(
                          "Geo-bin characteristics table"
                        ),
                        card_body(##################################
                                  gt_output('inloch')
                        )
                      ),
                      grid_card(#3,1
                        area = "inlosu",
                        full_screen = TRUE,
                        card_header("Time-series summary"),
                        card_body(##################################
                                  gt_output('inlosu')
                        )
                      ),
                      grid_card(
                        area = "inlobl", #blank-unused
                        full_screen = TRUE
                      )
                    )
                  ),
                  nav_panel(#-------------------------------------------------index accuracy
                    title = "Accuracy",
                    grid_container(
                      layout = c(
                        "inacth area1 area2",
                        "area4 .     .    ",
                        "area5 .     .    ",
                        "area6 .     .    ",
                        ".     .     .    "
                      ),
                      row_sizes = c(
                        "0.93fr",
                        "0.49fr",
                        "0.45fr",
                        "0.46fr",
                        "2.67fr"
                      ),
                      col_sizes = c(
                        "0.9fr",
                        "1.1fr",
                        "1fr"
                      ),
                      gap_size = "10px",
                      grid_card(
                        area = "area1",
                        card_body(
                          "R2",
                          grid_container(
                            layout = c(
                              "area0 area1 area2"
                            ),
                            gap_size = "10px",
                            col_sizes = c(
                              "1fr",
                              "1fr",
                              "1fr"
                            ),
                            row_sizes = c(
                              "0.7000000000000001fr"
                            ),
                            grid_card(area = "area0", card_body("All")),
                            grid_card(
                              area = "area1",
                              card_body(
                                "Outlier trim
                                                                                                                                                                                                                                                                                                                                                                                                                                                        "
                              )
                            ),
                            grid_card(area = "area2", card_body("k-fold"))
                          )
                        )
                      ),
                      grid_card(
                        area = "area2",
                        card_body(
                          "MSE
                                                                                                                                                                                                                                                                                                                    ",
                          grid_container(
                            layout = c(
                              "area0 area1 area2"
                            ),
                            gap_size = "10px",
                            col_sizes = c(
                              "1fr",
                              "1fr",
                              "1fr"
                            ),
                            row_sizes = c(
                              "0.8fr"
                            ),
                            grid_card(area = "area0", card_body("All")),
                            grid_card(area = "area1", card_body("Outlier trim")),
                            grid_card(
                              area = "area2",
                              card_body(
                                "k-fold
                                                                                                                                                                                                                                                                                                                                                                                                                                                        "
                              )
                            )
                          )
                        )
                      ),
                      grid_card(
                        area = "area4",
                        card_body(
                          "Custom
                                                                                                                                                                                                                                                                                                                    "
                        )
                      ),
                      grid_card(area = "area5", card_body("National")),
                      grid_card(area = "area6", card_body("Local")),
                      grid_card(
                        area = "inacth",
                        card_body("this is just a table!")
                      )
                    )
                  ),
                  nav_panel(#-------------------------------------------------index download
                    title = "Download",
                    grid_container(
                      layout = c(
                        "indocu",
                        "indona",
                        "indolo",
                        ".    "
                      ),
                      row_sizes = c(
                        "1fr",
                        "1fr",
                        "1fr",
                        "1fr"
                      ),
                      col_sizes = c(
                        "1fr"
                      ),
                      gap_size = "10px",
                      grid_card(
                        area = "indocu",
                        card_body(
                          card(
                            full_screen = TRUE,
                            card_header(
                              "Custom
                                                                                                                                                                                                                                                                                                                                                                        "
                            ),
                            card_body(
                              grid_container(
                                layout = c(
                                  "area0 area1 ."
                                ),
                                row_sizes = c(
                                  "1fr"
                                ),
                                col_sizes = c(
                                  "0.5fr",
                                  "0.5fr",
                                  "2fr"
                                ),
                                gap_size = "10px",
                                grid_card(
                                  area = "area0",
                                  full_screen = TRUE,
                                  card_header("csv"),
                                  card_body(
                                    actionButton(inputId = "myButton", label = "Data")
                                  )
                                ),
                                grid_card(
                                  area = "area1",
                                  full_screen = TRUE,
                                  card_header("html"),
                                  card_body(
                                    actionButton(inputId = "myButton", label = "Report")
                                  )
                                )
                              )
                            )
                          )
                        )
                      ),
                      grid_card(
                        area = "indona",
                        card_body(
                          card(
                            full_screen = TRUE,
                            card_header("National"),
                            card_body(
                              grid_container(
                                layout = c(
                                  "area0 area1 ."
                                ),
                                row_sizes = c(
                                  "1fr"
                                ),
                                col_sizes = c(
                                  "0.5fr",
                                  "0.5fr",
                                  "2fr"
                                ),
                                gap_size = "10px",
                                grid_card(
                                  area = "area0",
                                  full_screen = TRUE,
                                  card_header("csv"),
                                  card_body(
                                    actionButton(inputId = "myButton", label = "Data")
                                  )
                                ),
                                grid_card(
                                  area = "area1",
                                  full_screen = TRUE,
                                  card_header("html"),
                                  card_body(
                                    actionButton(inputId = "myButton", label = "Report")
                                  )
                                )
                              )
                            )
                          )
                        )
                      ),
                      grid_card(
                        area = "indolo",
                        card_body(
                          card(
                            full_screen = TRUE,
                            card_header("Local"),
                            card_body(
                              grid_container(
                                layout = c(
                                  "area0 area1 ."
                                ),
                                row_sizes = c(
                                  "1fr"
                                ),
                                col_sizes = c(
                                  "0.5fr",
                                  "0.5fr",
                                  "2fr"
                                ),
                                gap_size = "10px",
                                grid_card(
                                  area = "area0",
                                  full_screen = TRUE,
                                  card_header("csv"),
                                  card_body(
                                    actionButton(inputId = "myButton", label = "Data")
                                  )
                                ),
                                grid_card(
                                  area = "area1",
                                  full_screen = TRUE,
                                  card_header("html"),
                                  card_body(
                                    actionButton(inputId = "myButton", label = "Report")
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ),
              nav_panel(#--------------------------------------------------------------cycle
                title = "Cycle",
                grid_container(
                  layout = c(
                    "cyse cyta"
                  ),
                  row_sizes = c(
                    "1fr"
                  ),
                  col_sizes = c(
                    "0.28fr",
                    "1.72fr"
                  ),
                  gap_size = "10px",
                  grid_card(
                    area = "cyse",
                    full_screen = TRUE,
                    card_header(
                      "Target"
                    ),
                    card_body(
                      textInput(
                        inputId = "myTextInput",
                        label = "Reference/benchmark",
                        value = "SW3"
                      ),
                      sliderInput(
                        inputId = "inputId",
                        label = "Date",
                        min = 1995,
                        max = 2025,
                        value = 2024,
                        width = "100%",
                        step = 1
                      ),
                      checkboxGroupInput(
                        inputId = "myCheckboxGroup",
                        label = "Local index (treeselect)",
                        choices = list("choice a" = "a", "choice b" = "b")
                      ),
                      checkboxGroupInput(
                        inputId = "myCheckboxGroup",
                        label = "National index ",
                        choices = list(
                          "1" = "a",
                          "2" = "b",
                          "3" = "value3",
                          "4" = "value4",
                          "5" = "value5",
                          "6" = "value6",
                          "7" = "value7",
                          "8" = "value8",
                          "9" = "value9",
                          "10" = "value10"
                        )
                      ),
                      checkboxGroupInput(
                        inputId = "myCheckboxGroup",
                        label = "Custom",
                        choices = list(" " = "a")
                      )
                    )
                  ),
                  grid_card(
                    area = "cyta",
                    card_body(
                      tabsetPanel(
                        nav_panel(#-------------------------------------------------cycle Xx
                          title = "Value/Momentum",
                          grid_container(
                            layout = c(
                              "cyvmcc cyvmcr",
                              "cyvmci cyvmct"
                            ),
                            row_sizes = c(
                              "0.97fr",
                              "1.03fr"
                            ),
                            col_sizes = c(
                              "1fr",
                              "1fr"
                            ),
                            gap_size = "10px",
                            grid_card(
                              area = "cyvmcc",
                              full_screen = TRUE,
                              card_header(
                                "C/c = Value/Momentum = Dog/star
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "
                              ),
                              card_footer(
                                DTOutput(outputId = "myTable", width = "100%")
                              )
                            ),
                            grid_card(
                              area = "cyvmcr",
                              full_screen = TRUE,
                              card_header(
                                "c/return/y-axis/momentum  barchart
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "
                              )
                            ),
                            grid_card(
                              area = "cyvmci",
                              full_screen = TRUE,
                              card_header(
                                "C/index/x-axis/value-flipped  barchart
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "
                              )
                            ),
                            grid_card(
                              area = "cyvmct",
                              full_screen = TRUE,
                              card_header(
                                "Scatter c/total or c/(c+s
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "
                              ),
                              card_body(DTOutput(outputId = "myTable", width = "100%"))
                            )
                          )
                        ),
                        nav_panel(#-------------------------------------------------cycle zb
                          title = "Factor/beta",
                          grid_container(
                            layout = c(
                              "cyfbar cyfbxt",
                              "cyfbce cyfbwc"
                            ),
                            row_sizes = c(
                              "1fr",
                              "1fr"
                            ),
                            col_sizes = c(
                              "1fr",
                              "1fr"
                            ),
                            gap_size = "10px",
                            grid_card(
                              area = "cyfbar",
                              full_screen = TRUE,
                              card_header(
                                "z23 b23 points
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "
                              ),
                              card_footer(
                                DTOutput(outputId = "myTable", width = "100%")
                              )
                            ),
                            grid_card(
                              area = "cyfbxt",
                              full_screen = TRUE,
                              card_header(
                                "x23 timeseries
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "
                              )
                            ),
                            grid_card(
                              area = "cyfbce",
                              full_screen = TRUE,
                              card_header("z23 b23-centred")
                            ),
                            grid_card(
                              area = "cyfbwc",
                              full_screen = TRUE,
                              card_header(
                                "xwave and xcycle
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "
                              ),
                              card_body(DTOutput(outputId = "myTable", width = "100%"))
                            )
                          )
                        ),
                        nav_panel(#----------------------------------------------cycle xsect
                          title = "Cross-section",
                          grid_container(
                            layout = c(
                              "area0",
                              "area1"
                            ),
                            row_sizes = c(
                              "1fr",
                              "1fr"
                            ),
                            col_sizes = c(
                              "1fr"
                            ),
                            gap_size = "10px",
                            grid_card(
                              area = "area0",
                              full_screen = TRUE,
                              card_header(
                                "Index/level
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "
                              ),
                              card_body(
                                grid_container(
                                  layout = c(
                                    "cycrix cycrim cycric cycris cycrir"
                                  ),
                                  row_sizes = c(
                                    "1fr"
                                  ),
                                  col_sizes = c(
                                    "1fr",
                                    "1fr",
                                    "1fr",
                                    "1fr",
                                    "1fr"
                                  ),
                                  gap_size = "10px",
                                  grid_card(
                                    area = "cycrix",
                                    full_screen = TRUE,
                                    card_header(
                                      "X Index total
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "
                                    ),
                                    card_body(DTOutput(outputId = "myTable", width = "100%"))
                                  ),
                                  grid_card(
                                    area = "cycric",
                                    full_screen = TRUE,
                                    card_header(
                                      "C cyclical
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "
                                    )
                                  ),
                                  grid_card(
                                    area = "cycris",
                                    full_screen = TRUE,
                                    card_header(
                                      "S systematic
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "
                                    )
                                  ),
                                  grid_card(
                                    area = "cycrir",
                                    full_screen = TRUE,
                                    card_header("R residual")
                                  ),
                                  grid_card(
                                    area = "cycrim",
                                    full_screen = TRUE,
                                    card_header(
                                      "M market
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "
                                    )
                                  )
                                )
                              )
                            ),
                            grid_card(
                              area = "area1",
                              card_body(
                                card(
                                  full_screen = TRUE,
                                  card_header("Return/change"),
                                  card_body(
                                    grid_container(
                                      layout = c(
                                        "cycrrx cycrrm cycrrc cycrrs cycrr"
                                      ),
                                      row_sizes = c(
                                        "1fr"
                                      ),
                                      col_sizes = c(
                                        "1fr",
                                        "1fr",
                                        "1fr",
                                        "1fr",
                                        "1fr"
                                      ),
                                      gap_size = "10px",
                                      grid_card(
                                        area = "cycrrx",
                                        full_screen = TRUE,
                                        card_header(
                                          "x return total
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "
                                        )
                                      ),
                                      grid_card(
                                        area = "cycrrm",
                                        full_screen = TRUE,
                                        card_header(
                                          "m market
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "
                                        )
                                      ),
                                      grid_card(
                                        area = "cycrrc",
                                        full_screen = TRUE,
                                        card_header(
                                          "c cyclical
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "
                                        )
                                      ),
                                      grid_card(
                                        area = "cycrrs",
                                        full_screen = TRUE,
                                        card_header(
                                          "s systematic
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "
                                        )
                                      ),
                                      grid_card(
                                        area = "cycrr",
                                        full_screen = TRUE,
                                        card_header(
                                          "r residual
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "
                                        ),
                                        card_body(DTOutput(outputId = "myTable", width = "100%"))
                                      )
                                    )
                                  ),
                                  card_body()
                                )
                              )
                            )
                          )
                        ),
                        nav_panel(#--------------------------------------------cycle tseries
                          title = "Time-series",
                          grid_container(
                            layout = c(
                              "area0",
                              "area1"
                            ),
                            row_sizes = c(
                              "1fr",
                              "1fr"
                            ),
                            col_sizes = c(
                              "1fr"
                            ),
                            gap_size = "10px",
                            grid_card(
                              area = "area0",
                              full_screen = TRUE,
                              card_header(
                                "Index/level
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "
                              ),
                              card_body(
                                grid_container(
                                  layout = c(
                                    "cytsix cytsim cytsic cytsis cytsir"
                                  ),
                                  row_sizes = c(
                                    "1fr"
                                  ),
                                  col_sizes = c(
                                    "1fr",
                                    "1fr",
                                    "1fr",
                                    "1fr",
                                    "1fr"
                                  ),
                                  gap_size = "10px",
                                  grid_card(
                                    area = "cytsix",
                                    full_screen = TRUE,
                                    card_header("X "),
                                    card_body(DTOutput(outputId = "myTable", width = "100%"))
                                  ),
                                  grid_card(
                                    area = "cytsim",
                                    full_screen = TRUE,
                                    card_header("M")
                                  ),
                                  grid_card(
                                    area = "cytsic",
                                    full_screen = TRUE,
                                    card_header("C")
                                  ),
                                  grid_card(
                                    area = "cytsis",
                                    full_screen = TRUE,
                                    card_header("S")
                                  ),
                                  grid_card(
                                    area = "cytsir",
                                    full_screen = TRUE,
                                    card_header("R")
                                  )
                                )
                              )
                            ),
                            grid_card(
                              area = "area1",
                              full_screen = TRUE,
                              card_header(
                                "Retrun/change
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "
                              ),
                              card_body(
                                grid_container(
                                  layout = c(
                                    "cytsrx cytsrm cytsrc cytsrs cytsrr"
                                  ),
                                  row_sizes = c(
                                    "1fr"
                                  ),
                                  col_sizes = c(
                                    "1fr",
                                    "1fr",
                                    "1fr",
                                    "1fr",
                                    "1fr"
                                  ),
                                  gap_size = "10px",
                                  grid_card(
                                    area = "cytsrx",
                                    full_screen = TRUE,
                                    card_header("x")
                                  ),
                                  grid_card(
                                    area = "cytsrm",
                                    full_screen = TRUE,
                                    card_header("m")
                                  ),
                                  grid_card(
                                    area = "cytsrc",
                                    full_screen = TRUE,
                                    card_header("c")
                                  ),
                                  grid_card(
                                    area = "cytsrs",
                                    full_screen = TRUE,
                                    card_header("s")
                                  ),
                                  grid_card(
                                    area = "cytsrr",
                                    full_screen = TRUE,
                                    card_header("r"),
                                    card_body(DTOutput(outputId = "myTable", width = "100%"))
                                  )
                                )
                              )
                            )
                          )
                        ),
                        nav_panel(#-------------------------------------------cycle download
                          title = "Download",
                          grid_container(
                            layout = c(
                              "cydofa",
                              "cydobe",
                              "cydoco"
                            ),
                            gap_size = "10px",
                            col_sizes = c(
                              "1fr"
                            ),
                            row_sizes = c(
                              "1fr",
                              "1fr",
                              "1fr"
                            ),
                            grid_card(
                              area = "cydofa",
                              full_screen = TRUE,
                              card_header(
                                "Factor
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "
                              ),
                              card_body(
                                grid_container(
                                  layout = c(
                                    "area0 area1 ."
                                  ),
                                  row_sizes = c(
                                    "1fr"
                                  ),
                                  col_sizes = c(
                                    "0.5fr",
                                    "0.5fr",
                                    "2fr"
                                  ),
                                  gap_size = "10px",
                                  grid_card(
                                    area = "area0",
                                    full_screen = TRUE,
                                    card_header("csv"),
                                    card_body(
                                      actionButton(inputId = "button1", label = "Data")
                                    )
                                  ),
                                  grid_card(
                                    area = "area1",
                                    full_screen = TRUE,
                                    card_header("html"),
                                    card_body(
                                      actionButton(inputId = "myButton", label = "Report")
                                    )
                                  )
                                )
                              )
                            ),
                            grid_card(
                              area = "cydobe",
                              full_screen = TRUE,
                              card_header("Beta"),
                              card_body(
                                grid_container(
                                  layout = c(
                                    "area0 area1 ."
                                  ),
                                  row_sizes = c(
                                    "1fr"
                                  ),
                                  col_sizes = c(
                                    "0.5fr",
                                    "0.5fr",
                                    "2fr"
                                  ),
                                  gap_size = "10px",
                                  grid_card(
                                    area = "area0",
                                    full_screen = TRUE,
                                    card_header("csv"),
                                    card_body(
                                      actionButton(inputId = "myButton", label = "Data")
                                    )
                                  ),
                                  grid_card(
                                    area = "area1",
                                    full_screen = TRUE,
                                    card_header("html"),
                                    card_body(
                                      actionButton(inputId = "myButton", label = "Report")
                                    )
                                  )
                                )
                              )
                            ),
                            grid_card(
                              area = "cydoco",
                              full_screen = TRUE,
                              card_header(
                                "Covariance
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "
                              ),
                              card_body(
                                grid_container(
                                  layout = c(
                                    "area0 area1 ."
                                  ),
                                  row_sizes = c(
                                    "1fr"
                                  ),
                                  col_sizes = c(
                                    "0.5fr",
                                    "0.5fr",
                                    "2fr"
                                  ),
                                  gap_size = "10px",
                                  grid_card(
                                    area = "area0",
                                    full_screen = TRUE,
                                    card_header("csv"),
                                    card_body(
                                      actionButton(inputId = "myButton", label = "Data")
                                    )
                                  ),
                                  grid_card(
                                    area = "area1",
                                    full_screen = TRUE,
                                    card_header("html"),
                                    card_body(
                                      actionButton(inputId = "myButton", label = "Report")
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ),
              nav_panel(#---------------------------------------------------------------data
                title = "Data",
                grid_container(
                  layout = c(
                    "daipbu daipsu  daipfi",
                    "dappbu dappsa  dappfi",
                    "dasebu dasesa  dasefi",
                    "dasabu dasasa  dasafi",
                    "darebu daresa darefi"
                  ),
                  row_sizes = c(
                    "1fr",
                    "1fr",
                    "1fr",
                    "1fr",
                    "1fr"
                  ),
                  col_sizes = c(
                    "0.5fr",
                    "1.5fr",
                    "1fr"
                  ),
                  gap_size = "10px",
                  grid_card(
                    area = "dappbu",
                    full_screen = TRUE,
                    card_header("PPD-id"),
                    card_body(
                      actionButton(inputId = "myButton", label = "Download")
                    )
                  ),
                  grid_card(
                    area = "daipbu",
                    full_screen = TRUE,
                    card_header("EPC-id"),
                    card_body(
                      actionButton(inputId = "myButton", label = "Download")
                    )
                  ),
                  grid_card(
                    area = "dasebu",
                    full_screen = TRUE,
                    card_header(
                      "Seasonal
                                                                                                                                                                                                                                                                                        "
                    ),
                    card_body(
                      actionButton(inputId = "myButton", label = "Download")
                    )
                  ),
                  grid_card(
                    area = "dasabu",
                    full_screen = TRUE,
                    card_header(
                      "Sale
                                                                                                                                                                                                                                                                                        "
                    ),
                    card_body(
                      actionButton(inputId = "myButton", label = "Download")
                    )
                  ),
                  grid_card(
                    area = "darebu",
                    full_screen = TRUE,
                    card_header(
                      "Repeat
                                                                                                                                                                                                                                                                                        "
                    ),
                    card_body(
                      actionButton(inputId = "myButton", label = "Download")
                    )
                  ),
                  grid_card(
                    area = "daipsu",
                    full_screen = TRUE,
                    card_header(
                      "Sample
                                                                                                                                                                                                                                                                              "
                    )
                  ),
                  grid_card(
                    area = "dappsa",
                    full_screen = TRUE,
                    card_header("Sample")
                  ),
                  grid_card(
                    area = "dasesa",
                    full_screen = TRUE,
                    card_header("Sample")
                  ),
                  grid_card(
                    area = "dasasa",
                    full_screen = TRUE,
                    card_header("Sample")
                  ),
                  grid_card(
                    area = "daresa",
                    full_screen = TRUE,
                    card_header("Sample")
                  ),
                  grid_card(
                    area = "daipfi",
                    full_screen = TRUE,
                    card_header(
                      "Fields
                                                                                                                                                                                                                                                                              "
                    )
                  ),
                  grid_card(
                    area = "dappfi",
                    full_screen = TRUE,
                    card_header("Fields")
                  ),
                  grid_card(
                    area = "dasefi",
                    full_screen = TRUE,
                    card_header("Fields")
                  ),
                  grid_card(
                    area = "dasafi",
                    full_screen = TRUE,
                    card_header("Fields")
                  ),
                  grid_card(
                    area = "darefi",
                    full_screen = TRUE,
                    card_header("Fields")
                  )
                )
              )
  )#end page_navbar#############################################################
#========================================================================-server
server <- function(input, output) {
  steprip <- '03rip/'
  nfig1 <- 3 #for T2
  nfig2 <- -1 #for ppm2
  nfig3 <- 4 #for frac
  
  options(scipen=999) #no scientific format
  
  #----------------------------------------------------------------render output
  
  #-----------------------------------------------------------------index custom
  # 1/6 index   2/6 map
  # 3/6 winding 4/6 charac
  # 5/6 timeser 6/6 blank
  
  # 1/6 incuinti 
  output$incuinti <- #solve rsi
    renderPlot({
      Rincuinti()#+
    })
  
  
  # 2/6 incuinma
  # output$incuinma <- #custom map 
  #   renderLeaflet(
  #     f240810a(
  #         rcx=Rsitr(),
  #         x3a=pxosrdo2dd,
  #         target=isolate(Rsirc()),
  #         pva=z110,
  #         palx=palcu,
  #         maxzoom=12
  #       )
  #   )
  
  
  output$incuinma <- #custom map 
    renderLeaflet(
      Rincuinma()
    )
  Rincuinma <- 
    eventReactive(
      eventExpr=
        list(
          input$innata, #sidepanel target
          input$incusetr #sidepanel tree
        ),
      valueExpr={
        f240810a(
          rcx=input$incusetr[which(nchar(input$incusetr)==6)],
          x3a=pxosrdo2dd,
          target=input$innata,
          pva=z110,
          palx=palcu,
          maxzoom=12
        )
      }
    )  
  
  output$incuinwi <- #custom winding
    render_gt(
      Rincuinwi() #index custom index summary
    )
  Rincuinwi <- 
    eventReactive(
      eventExpr=
        list(
          Rsirc(), #sidepanel target
          input$sicobu #sidepanel compute button
        ),
      valueExpr={
        isolate(Rincuin())%>%
          .[,.(date=as.character(date),xdot)]%>%
          .[,.(decade=substr(date,1,3),yr=substr(date,4,4),xdot=round(xdot,3))]%>%
          dcast(.,decade~yr,value.var='xdot')%>%
          .[,decade:=c(1990,2000,2010,2020)]
      }
    )
  
  
  # 4/6 incuinch
  output$incuinch <- 
    render_gt(
      Rincuinch() #index custom index summary
    )
  Rincuinch <- 
    eventReactive(
      eventExpr=
        list(
          Rsirc(), #sidepanel target
          input$sicobu #sidepanel compute button
        ),
      valueExpr={
        z110%>%.[
          rcx%in%isolate(Rsitr())#]%>%#.[1:10,]%>%
          ,
          .(#no beta1
            frac=round(sum(nid)/z110[nchar(rcx==3),sum(nid)],nfig3),
            ppm2max=round(max(ppm2),nfig2),
            ppm2min=round(min(ppm2),nfig2),
            p=round(sum(pv)/sum(m2),nfig2),
            R2rsi=Rincuin()[1,round(rsqraw,nfig1)]
          )
        ]%>%
          .[,.(
            frac,
            R2rsi,
            p,
            p.cus=paste0(round(ppm2min,nfig2),'-',round(ppm2max,nfig2))
          )]%>%
          gt::gt(.)%>%
          cols_label(
            frac = gt::html('Fraction<br>properties'),
            R2rsi = gt::html("RSI R<sup>2</sup>"),
            p = gt::html("Aggregate"),
            p.cus=gt::html("Range")
          )%>%
          tab_spanner(
            label = gt::html("Custom £/m<sup>2</sup>"),
            columns = c(p.cus, p)
          )
      }
    )
  
  # 5/6 incuinsu 
  output$incuinsu <- 
    render_gt(
      Rincuinsu() #index custom index summary
    )
  Rincuinsu <- 
    eventReactive(
      eventExpr=
        list(
          Rsirc(), #sidepanel target
          input$sicobu #sidepanel compute button
        ),
      valueExpr={
        x1 <- Rincuin()[,.(date,xdot)] #index custom index (update rsi)
        x2 <- zoo(as.matrix(x1[,xdot]),as.matrix(x1[,date]))
        x3 <- table.Stats(x2,digits=3)
        x4 <- data.table(x=rownames(x3),stat=x3[[1]])%>%
          setnames(.,c(' ','  '))%>%
          .[-c(1,2,7),]
        
        x4
      }
    )
  
  # 6/6 incuinbl BLANK
  
  #---------------------------------------------------------------index national
  # 1/6 index   2/6 map
  # 3/6 winding 4/6 charac
  # 5/6 summary 6/6 target
  
  Rinnati <- #1/6 innati 
    eventReactive(
      list(
        Rsirc(),
        input$sicobu
      )
      ,
      valueExpr={
        x <- z321a$ses$estdt[nx==z321a$geo[rc9==Rsirc(),nx]]%>%
          ggplot(.,aes(date1,x))+
          geom_line()+
          geom_point(size=.3)+
          xlab('')+
          ylab(bquote(Delta~P~log~price~change))+
          theme_bw() +
          theme(
            axis.line = element_line(colour = "black"),
            panel.grid.major = element_line(size=pgms,linetype = pgmt,color=pgmc),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            text=element_text(size=16,face='plain'),
            axis.line.y.left=element_line(size=.1),
            axis.line.x.bottom=element_line(size=.1),
            legend.position='none')+
          scale_x_date(
            breaks = as.Date(c('1995-01-01','2000-01-01','2010-01-01','2020-01-01','2024-01-01')),
            date_labels = "%Y",
            limits=c(as.Date(c('1994-12-31','2027-12-31')))
          )
        print('Rinnati')
        x
      }
    )
  output$innati <- 
    renderPlot(
      Rinnati()
    )
  
  output$innama <- #2/6 innama
    renderLeaflet(
      z321a$geo[nx==z321a$geo[rc9==Rsirc(),nx],rc9]%>%
        f240810a(rcx=.,x3a=pxosrdo2dd,target=Rsirc(),pva=z110,palx=palna,maxzoom=12) 
    )
  
  output$innawi <- #3/6 innawi
    render_gt(
      x3 <- z321a$pan[,date:=as.character(date)][,c(1,z321a$geo[rc9==Rsirc(),nx]+1),with=F][date=='2009-02-28',date:='2008-12-31']%>%
        setnames(.,c('date','xdot'))%>%
        .[,.(decade=substr(date,1,3),yr=substr(date,4,4),xdot=round(xdot,3))]%>%
        dcast(.,decade~yr,value.var='xdot')%>%
        .[,decade:=c(1990,2000,2010,2020)]
    )
  
  x.nat.t4 <- #4/6 innach
    f231204a(2)%>%
    .[,.(
      p.bin=paste0(
        formatC(round(ppm2min,-1), format="f", big.mark=",", digits=0),'-',
        formatC(round(ppm2max,-1), format="f", big.mark=",", digits=0)
      ),
      p=formatC(round(ppm2,-1), format="f", big.mark=",", digits=0),
      np=nx,
      frac=round(fid,4),
      R2rsi=round(r2rsi,3),
      beta=round(b1/mean(b1),2)
    )]
  output$innach <- 
    render_gt(
      gt::gt(x.nat.t4)%>%
        cols_label(
          frac = html('Fraction<br>properties'),
          R2rsi = html("RSI R<sup>2</sup>"),
          p = html("Aggregate"),
          p.bin=html("Range")
        )%>%
        tab_spanner(
          label = html("Band £/m<sup>2</sup>"),
          columns = c(p.bin, p)
        )%>%
        gt_highlight_rows(
          .,
          columns = gt::everything(),
          rows = z321a$geo[rc9==Rsirc(),11-nx], #reversed order
          fill = cobalt()['green'], #"#80bcd8"
          alpha = 0.1, #v pale
          font_weight = "normal",
          target_col = c()
        )
      
    )
  
  output$innasu <- #5/6 innasu 
    render_gt(
      zoo(z321$pan[,-'date'],z321$pan[,date])%>%
        table.Stats(.,digits=3)%>%
        data.table(.,keep.rownames = T)%>%
        `[`(.,i=-c(1,2,7))%>%
        setnames(.,c('.',paste0('np=',1:10)))
    ) 
  
  # 6/6 innata - target, input
  
  #-------------------------------------------------------------------index local
  #nb this all copy/paste from inna
  Rinloti <- #1/6 inloti 
    eventReactive(
      list(
        Rsirc(),
        input$sicobu
      )
      ,
      valueExpr={
        #x0 <- rep(cobalt()['green'],3)
        x0 <- rep('#00FF00',3)
        x0[sort(setdiff(1:3,z321d$geo[rc9==Rsirc(),substr(lab,4,4)]))] <- 
          cobalt()[c('onch','blue')]
        #as.character(.)%>%
        x0 <- setNames(x0,as.character(1:3))
        #as.factor(.)
        print(x0)
        x1 <- 
          Rsirc()%>%
          substr(.,1,3)
        print(x1)
        x2 <- 
          z321d$ses$estdt[substr(rc3,1,3)==x1]%>%
          .[,qq:=as.factor(substr(rc3,4,4))]%>%
          .[,rc3:=as.factor(substr(rc3,1,3))]
        print(x2)
        x <- x2%>%
          ggplot(.,aes(date1,x,color=qq))+
          geom_line()+
          geom_point(size=.3)+
          xlab('')+
          ylab(bquote(Delta~P~log~price~change))+
          theme_bw() +
          theme(
            axis.line = element_line(colour = "black"),
            panel.grid.major = element_line(size=pgms,linetype = pgmt,color=pgmc),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            text=element_text(size=16,face='plain'),
            axis.line.y.left=element_line(size=.1),
            axis.line.x.bottom=element_line(size=.1),
            legend.position='none')+
          scale_color_manual(values=x0)+#cobalt()[2:4]  as.character(cobalt())
          scale_x_date(
            breaks = as.Date(c('1995-01-01','2000-01-01','2010-01-01','2020-01-01','2024-01-01')),
            date_labels = "%Y",
            limits=c(as.Date(c('1994-12-31','2027-12-31')))
          )
        #print('Rinloti')
        x
      }
    )
  output$inloti <- 
    renderPlot(
      Rinloti()
    )
  z321d$geo
  output$inloma <- #2/6 inloma
    
    
    renderLeaflet(
      z321d$geo%>%
        .[substr(rc9,1,3)==substr(Rsirc(),1,3)]%>%
        .[,.(rc6=rc9,col=lighten(cobalt(),lightenx)[as.numeric(substr(lab,4,4))])]%>%
        f240810b(.)
    )
  # z321d$geo[nx==z321d$geo[rc9==Rsirc(),nx],rc9]%>%
  #   f240810b(rcx=.,x3a=pxosrdo2dd,target=Rsirc(),pva=z110,palx=palna,maxzoom=12)
  # z321a$geo[nx==z321a$geo[rc9==Rsirc(),nx],rc9]%>%
  #   f240810a(rcx=.,x3a=pxosrdo2dd,target=Rsirc(),pva=z110,palx=palcu,maxzoom=12)
  #)
  
  output$inlowi <- #3/6 inlowi
    render_gt(
      x3 <- z321a$pan[,date:=as.character(date)][,c(1,z321a$geo[rc9==Rsirc(),nx]+1),with=F][date=='2009-02-28',date:='2008-12-31']%>%
        setnames(.,c('date','xdot'))%>%
        .[,.(decade=substr(date,1,3),yr=substr(date,4,4),xdot=round(xdot,3))]%>%
        dcast(.,decade~yr,value.var='xdot')%>%
        .[,decade:=c(1990,2000,2010,2020)]
    )
  
  x.nat.t4 <- #4/6 inloch
    f231204a(2)%>%
    .[,.(
      p.bin=paste0(
        formatC(round(ppm2min,-1), format="f", big.mark=",", digits=0),'-',
        formatC(round(ppm2max,-1), format="f", big.mark=",", digits=0)
      ),
      p=formatC(round(ppm2,-1), format="f", big.mark=",", digits=0),
      np=nx,
      frac=round(fid,4),
      R2rsi=round(r2rsi,3),
      beta=round(b1/mean(b1),2)
    )]
  output$inloch <- 
    render_gt(
      gt::gt(x.nat.t4)%>%
        cols_label(
          frac = html('Fraction<br>properties'),
          R2rsi = html("RSI R<sup>2</sup>"),
          p = html("Aggregate"),
          p.bin=html("Range")
        )%>%
        tab_spanner(
          label = html("Band £/m<sup>2</sup>"),
          columns = c(p.bin, p)
        )%>%
        gt_highlight_rows(
          .,
          columns = gt::everything(),
          rows = z321a$geo[rc9==Rsirc(),11-nx], #reversed order
          fill = cobalt()['green'], #"#80bcd8"
          alpha = 0.1, #v pale
          font_weight = "normal",
          target_col = c()
        )
      
    )
  
  output$loter <- renderText(
    paste0(
      Rsirc(),
      ' is in ',irregpcode(substr(Rsirc(),1,3)),' tertile ',
      z321d$geo[rc9==Rsirc(),substr(lab,4,4)]
    )
  )
  output$inlosu <- #5/6 inlosu 
    render_gt(
      zoo(z321$pan[,-'date'],z321$pan[,date])%>%
        table.Stats(.,digits=3)%>%
        data.table(.,keep.rownames = T)%>%
        `[`(.,i=-c(1,2,7))%>%
        setnames(.,c('.',paste0('np=',1:10)))
    ) 
  
  # output$rc3 <- 
  #   renderText(
  #     paste0("P-tertiles of ",irregpcode(substr(regpcode(Rsita()),1,3)))
  #   )
  
  output$nationalnp <- 
    renderText(
      paste0(
        Rsirc(),
        " is in national P-band ",
        z321a$geo[rc9==Rsirc(),nx]
      )
    )
  
  
  #---------------------------------------------------------------------not used
  #---not used
  output$perfnatt1 <-  #triangular counts
    render_gt(
      x1 <- dcast(coread(geon[nx==z321a$geo[rc9==Rsirc(),nx],rc9],steprip)[,.N,.(buy=substr(buydate,1,4),sell=substr(selldate,1,4))],buy~sell,value.var='N') #
    )
  #---not used
  output$perfnatt2 <-  #triangular mean
    render_gt(
      x2 <-  
        dcast(
          coread(
            geon[nx==z321a$geo[rc9==Rsirc(),nx],rc9],
            steprip
          )[,
            j=.(r=round(mean(as.numeric(retsa)),3)),
            by=.(
              buy=substr(buydate,1,4),
              sell=substr(selldate,1,4)
            )
          ],
          buy~sell,value.var='r')
    )
  #---not used
  output$binchacus <-  #pva 
    render_gt(
      z110[rcx%in%input$customtree[which(nchar(input$customtree)==6)],.(pc=irregpcode(rcx),ppm2=round(ppm2,-1),nid,m2bar=round(m2/nid),pvbar=round(pv/(1000*nid)))]%>%
        gt::gt(.)%>%
        fmt_number(columns = 1:4,sep_mark = ",",decimals=0) %>% 
        cols_label(
          pc = '',
          nid = html("Tracked properties"),
          ppm2 = html("£/m<sup>2</sup>"),
          m2bar = html("Floor area (m<sup>2</sup>)"),
          pvbar = html("Present value (£000)")
        )%>%
        tab_spanner(
          label = html("Average"),
          columns = c(m2bar, pvbar)
        )%>%
        tab_options(
          table.align = "left"
        )
    )
  #---not used
  output$geocusl <- #leaflet cust
    renderLeaflet(
      input$customtree[which(nchar(input$customtree)==6)]%>%
        f240810a(rcx=.,x3a=pxosrdo2dd,target=Rsirc(),pva=z110,palx=pal,maxzoom=12)
    )
  
  o <- observe({
    shinyjs::click("sicobu")
    o$destroy() # destroy observer as it has no use after initial button click
  })

  #======================================================================-reactive   for readability these should list out what they return 
  Rsita <-     
    eventReactive(input$innata,toupper(input$innata))
  
  Rsirc <- 
    eventReactive(
      list(
        Rsita()
      )
      ,
      valueExpr={
        print(regpcode(Rsita()))
        if(irregpcode(regpcode(Rsita()))==Rsita()) {
          print(regpcode(Rsita()))
          updateTreeInput(
            inputId='incusetr',
            selected = regpcode(Rsita())
          )
        }
        x <- regpcode(Rsita())
        x
      }
    )
  
  #---------custom
  Rincurc <- #vector character rc6
    eventReactive(
      eventExpr= 
        input$sicobu,#-sicobu *compute*
      valueExpr=
        {
          if(input$incusege=='t') {
            x <- Rsitr()
          } else if(input$incusege=='c') {
            x <- Rincusefi()
          }
          x
        }
    )
  
  Rsitr <- #vector character rc6 [treeselect]
    eventReactive(
      eventExpr= 
        input$sicobu,#-sicobu *compute*
      valueExpr=
        {
          print('Rsitr:')
          print(input$incusetr[which(nchar(input$incusetr)==6)])
          print('Rsirc:')
          print(Rsirc())
          input$incusetr[which(nchar(input$incusetr)==6)]
        }
    )
  
  Rincusefi <- # vector character rc6 [file]
    eventReactive(
      eventExpr=
        input$sicobu,#-sicobu *compute*
      valueExpr=
        {
          req(input$incusefi)
          if(!grepl('csv$',input$incusefi)) {validate('Invalid: select csv file')}
          print(fread(input$incusefi$datapath))
          fread(input$incusefi$datapath)
        }
    )
  
  Rincuin <- # RSI solve: dt estdt incuin
    eventReactive(
      eventExpr=#-sicobu index custom setting *compute*
        list(
          input$sicobu,#-sicobu *compute*
          input$Used, 
          input$Type,
          input$incusetr
        ),
      valueExpr=
        { 
          x0 <- list(
            input$sicobu,
            input$Used,
            input$Type,
            input$incusetr
          )
          x <- f230312x(  #solve single nx -> estdt with no pra
            nxx=1,
            steprip='03rip/',
            dfn=dfnx,
            geo=data.table(
              rc9=Rsitr(),
              nx=1,
              lab='lab001'
            ),
            houseflat=input$Type,
            newused=input$Used
          )
          if(any(x[,is.na(x)])) {
            xna <<- x; print('na found');x <- x[is.na(xdot),xdot:=0][,x:=cumsum(xdot)][xdot==0,x:=NA];print(x)
          }
          x
        }
    )
  
  Rincuinti <- # plot(incuin) 
    eventReactive(
      eventExpr=#-sicobu *compute*
        input$sicobu
      ,
      valueExpr=
        {
          print('Rincuinti')
          x <- Rincuin()
          foo <<- Rincuin()
          x1 <- 
            ggplot(
              x,
              aes(date,x)
            )+
            geom_line()+
            geom_point(size=.3)+
            xlab('')+
            ylab(bquote(Delta~P~log~price~change))+
            theme_bw() +
            theme(
              axis.line = element_line(colour = "black"),
              panel.grid.major = element_line(size=pgms,linetype = pgmt,color=pgmc),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              text=element_text(size=16,face='plain'),
              axis.line.y.left=element_line(size=.1),
              axis.line.x.bottom=element_line(size=.1),
              legend.position='none')+
            scale_x_date(
              breaks = as.Date(c('1995-01-01','2000-01-01','2010-01-01','2020-01-01','2024-01-01')),
              date_labels = "%Y",
              limits=c(as.Date(c('1994-12-31','2027-12-31')))
            )
          x1
        }
    )
  
  
}


shinyApp(ui, server)

