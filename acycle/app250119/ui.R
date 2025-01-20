
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
                   actionButton("docusabC", "Recompute custom"),
                   conditionalPanel( #custom == computed
                     condition="output.comrc6forjstest==output.selrc6forjstest",#"all.equal('A','B')",
                     span(textOutput('cuseqcom'), style="color:black;font-size:12px")
                   ),
                   conditionalPanel(#custom != computed
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
                 selected = (rc6cuC[1]), 
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
                 label = "Custom selection",
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
                                         "yyy111     xtimeseries    "#,
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
                                         area = "yyy111", 
                                         full_screen = TRUE,
                                         card_header(
                                           "Postcode area map"
                                         ),
                                         card_body(#
                                           leaflet::leafletOutput('yyy111'),
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
                                           plotOutput('yyy112'),
                                           height=gridheight
                                         )
                                         
                                       )#,
                                       # grid_card(
                                       #   area = "Winding",
                                       #   full_screen = TRUE,
                                       #   card_header(
                                       #     "Price return summary"
                                       #   ),
                                       #   card_body(#--------------x121
                                       #     gt::gt_output('x121a'),
                                       #     gt::gt_output('x121b'),
                                       #     height=gridheight
                                       #   )
                                       #   
                                       # ),
                                       # grid_card(
                                       #   area = "characteristics",
                                       #   full_screen = TRUE,
                                       #   card_header(
                                       #     "Index characteristics"
                                       #   ),
                                       #   card_body(#--------------x122
                                       #     gt::gt_output('x122')#,
                                       #     #height=gridheight
                                       #   )
                                       # ),
                                       # grid_card(
                                       #   area = "summary",
                                       #   full_screen = TRUE,
                                       #   card_header(
                                       #     "Summary"
                                       #   ),
                                       #   card_body(#--------------x131
                                       #     gt::gt_output('x131'),
                                       #     height=gridheight
                                       #   )
                                       # )
                                       # ,
                                       # grid_card(
                                       #   area = "tradesummary",
                                       #   full_screen = TRUE,
                                       #   card_header(
                                       #     "Trade recap"
                                       #   ),
                                       #   card_body(#--------------x132
                                       #     gt::gt_output('x132a'),
                                       #     gt::gt_output('x132b'),
                                       #     gt::gt_output('x132c'),
                                       #     gt::gt_output('x132d'),
                                       #     height=gridheight2
                                       #   )
                                       # )
                                     ) #grid_container for whole tss page
                           ),
                           
                           
                           
                           
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
                                       # grid_card(
                                       #   area = "Winding",
                                       #   full_screen = TRUE,
                                       #   card_header(
                                       #     "Price return summary"
                                       #   ),
                                       #   card_body(#--------------x121
                                       #     gt::gt_output('x121a'),
                                       #     gt::gt_output('x121b'),
                                       #     height=gridheight
                                       #   )
                                       #   
                                       # ),
                                       # grid_card(
                                       #   area = "characteristics",
                                       #   full_screen = TRUE,
                                       #   card_header(
                                       #     "Index characteristics"
                                       #   ),
                                       #   card_body(#--------------x122
                                       #     gt::gt_output('x122')#,
                                       #     #height=gridheight
                                       #   )
                                       # ),
                                       # grid_card(
                                       #   area = "summary",
                                       #   full_screen = TRUE,
                                       #   card_header(
                                       #     "Summary"
                                       #   ),
                                       #   card_body(#--------------x131
                                       #     gt::gt_output('x131'),
                                       #     height=gridheight
                                       #   )
                                       # )
                                       # ,
                                       # grid_card(
                                       #   area = "tradesummary",
                                       #   full_screen = TRUE,
                                       #   card_header(
                                       #     "Trade recap"
                                       #   ),
                                       #   card_body(#--------------x132
                                       #     gt::gt_output('x132a'),
                                       #     gt::gt_output('x132b'),
                                       #     gt::gt_output('x132c'),
                                       #     gt::gt_output('x132d'),
                                       #     height=gridheight2
                                       #   )
                                       # )
                                     ) #grid_container for whole tss page
                           ),
                           nav_panel(title = "Listing",#----
                                     
                                     # grid_container(
                                     #   layout = c(
                                     #     "locallist  customlist"
                                     #   ),
                                     #   row_sizes = c(
                                     #     "1fr"
                                     #   ),
                                     #   col_sizes = c(
                                     #     "1fr",
                                     #     "1fr"
                                     #   ),
                                     #   grid_card(
                                     #     area='locallist',
                                     #     full_screen = TRUE,
                                     #     card_header(
                                     #       "Local"
                                     #     ),
                                     #     card_body(#--------------x211
                                     #       gt::gt_output('x211')
                                     #     )
                                     #   ),
                                     #   grid_card(
                                     #     area='customlist',
                                     #     full_screen = TRUE,
                                     #     card_header(
                                     #       "Custom"
                                     #     ),
                                     #     card_body(#--------------x211cu
                                     #       gt::gt_output('x211cu')
                                     #     )
                                     #   )
                                     # )#grid_container
                           ),
                           nav_panel(title = "Constituents"#,#----
                                     # card(
                                     #   full_screen = TRUE,
                                     #   card_header(
                                     #     "Constituent districts"
                                     #   ),
                                     #   card_body(#--------------x311
                                     #     DT::DTOutput('x311'),
                                     #     height=gridheight3
                                     #   ),
                                     #   height=gridheight3
                                     # )
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
                                             # card_header(
                                             #   "Parameter sets" #does not need header until have more cards e.g. correlation
                                             # ),
                                             card_body(#--------------x411
                                               gt::gt_output('x411')
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

