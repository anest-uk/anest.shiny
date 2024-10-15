

############################## 1

                conditionalPanel(condition="input.intybu == 'c'",
                                 grid_card(
                                   area="customrc6",
                                   card_body(
                                     actionButton(inputId = "sicobu", label = "Compute")
                                     ,
                                     treeInput( #districts
                                       inputId = "incusetr",
                                       label = "Select peers:",
                                       choices = create_tree(f240824b(unique(substr(dir('03rip/'),1,3)))),
                                       selected = treex, #this
                                       returnValue = "text",
                                       closeDepth = 0
                                     )
                                     
                                   )
                                 )
                                 ,
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
                                         "All" = ".", #this
                                         "House" = "H",
                                         "Flat" = "F"
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
                                       label = "Used",
                                       choices = list(
                                         "All"=".", #used
                                         "New" = "N",
                                         "Used" = "U"
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
                                 
                                 
                                 
                                 
                )######################
                ,

#####################2