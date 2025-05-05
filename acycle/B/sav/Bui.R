
#---ui   section
ui <-      #ui----
grid_page( ###grid page ----
           layout = c(
             "action  header",
             "sidebar  area2 "
           ),
           row_sizes = c(
             "200px",
             "1fr"
           ),
           col_sizes = c(
             "250px",
             "1fr"
           ),
           gap_size = ".1rem",
           
           grid_card(
             area = "action",
             card_header("Custom"),
             card_body(
               div(style = "padding:4px",
                   actionButton("docusabC", "Recompute bespoke"),
                   conditionalPanel( #bespoke == computed
                     condition="output.comrc6forjstest==output.selrc6forjstest",#"all.equal('A','B')",
                     span(textOutput('cuseqcom'), style="color:black;font-size:12px")
                   ),
                   conditionalPanel(#bespoke != computed
                     condition="output.comrc6forjstest!=output.selrc6forjstest",#"all.equal('A','B')",
                     span(textOutput('cusnecom'), style="color:red;font-size:12px")
                   ),
                   span(textOutput('comrc6'), style="color:black;font-size:12px"),
                   span(textOutput('selrc6'), style="color:black;font-size:12px"),
                   span(textOutput('defrc6'), style="color:black;font-size:12px")
               ),#div
             )#card_body
           ),#grid_card
           grid_card(
             area = "sidebar",
             card_header("Selection"),
             card_body(
               shinyWidgets::treeInput( #districts
                 inputId = "rctaC",
                 label = "Target district",
                 choices = create_tree(f240824b(rev(sort(unique(substr(dir('smallrip/'),1,3)))))[(order(regionx,lab))]),
                 selected = (rc6cuC), 
                 returnValue = "text",
                 closeDepth = 0
               ),
               
               sliderInput(
                 inputId = "tslider",
                 label = "Datum period",
                 min=0,
                 max=45,
                 value=34
               ),
               shinyWidgets::treeInput( #districts
                 inputId = "rccuC",
                 label = "Bespoke selection",
                 choices = create_tree(f240824b(rev(sort(unique(substr(dir('smallrip/'),1,3)))))[(order(regionx,lab))]),
                 selected = rc6cuC, 
                 returnValue = "text",
                 closeDepth = 0
               ),
               span(textOutput('selrc6forjstest'), style="color:white"), #white - these must exist for conditionalpanel logic
               span(textOutput('comrc6forjstest'), style="color:white") #white
             )
           ),#grid card end
           grid_card_text(
             area = "header",
             content = "Local and custom index",
             alignment = "start",
             is_title = FALSE
           ),
           grid_card(
             area = "area2",
             card_body(
               page_navbar(#tabsetPanel(#----
                           fillable=F,
                           position='fixed-top',
                           nav_panel(title = "Time-series summary",#----
                                     
                                     grid_container(
                                       layout = c(
                                         "x111     xtimeseries    "#,
                                         #"Winding characteristics",
                                         #"summary tradesummary   "
                                       ),
                                       row_sizes = c(
                                         "1fr"#,
                                         #".7fr",
                                         #"1fr"
                                       ),
                                       col_sizes = c(
                                         "1fr",
                                         "1fr"
                                       ),
                                       gap_size = "10px",
                                       grid_card( #1,1-------------card
                                         area = "x111", 
                                         full_screen = TRUE,
                                         card_header(
                                           "Postcode area map"
                                         ),
                                         card_body(#
                                           leaflet::leafletOutput('x111'),
                                           height=gridheight
                                         )
                                       ),
                                       grid_card(
                                         area = "xtimeseries", #1.1 -------x112
                                         full_screen = TRUE,
                                         card_header(
                                           "Indices"
                                         ),
                                         card_body(#--------------x112
                                           plotOutput('x112'),
                                           height=gridheight
                                         )
                                         
                                       )#,

                                     ) #grid_container for whole tss page
                           ),
                           nav_panel(title = "Listing",#----
                                     
                           ),
                           nav_panel(title = "Constituents"#,#----
                           ),
                           nav_panel(title = "Accuracy", #----
                                     card(
                                       full_screen = TRUE,
                                       card_header("Accuracy metrics"),
                                       card_body(
                                         grid_container(
                                           layout = c(
                                             "trial"
                                           ),
                                           row_sizes = c(
                                             "1fr"
                                           ),
                                           col_sizes = c(
                                             "1fr"
                                           ),
                                           gap_size = "10px",
                                           grid_card(
                                             area = "trial",
                                             full_screen = TRUE,
                                             card_body(#--------------x411
                                               gt::gt_output('x411')#,
                                             )
                                           )
                                           
                                         )
                                       )
                                     )
                           ),
                           nav_panel(title = "Notes",#----
                                     
                                     card(
                                       full_screen = TRUE,
                                       card_body(
                                         htmltools::includeMarkdown("notes.Rmd")
                                       )
                                     )
                           )#,
                           #widths=c(2,8)
               )#-navset_pill_list----
             )#card_body
           )#grid_card
)###----

