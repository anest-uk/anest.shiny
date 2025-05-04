ui_navpanel_tss <- function(id=NULL){
  nav_panel(
    title = "Time-series summary",#--------------------------Time-series summary
    grid_container(
      layout = c(
        "x111     xtimeseries    ",
        "Winding characteristics",
        "summary tradesummary   "
      ),
      row_sizes = c(
        "1fr",
        ".7fr",
        "1fr"
      ),
      col_sizes = c(
        "1fr",
        "1fr"
      ),
      gap_size = "10px",
      grid_card( 
        area = "x111", 
        full_screen = TRUE,
        card_header(
          "Postcode area map"
        ),
        card_body(
          leaflet::leafletOutput('x111'),#-----------leaflet x111
          height=gridheight
        )
      ),
      grid_card(
        area = "xtimeseries",
        full_screen = TRUE,
        card_header(
          "Indices"
        ),
        card_body(
          plotOutput('x112'), #----------------------plot x112
          height=gridheight
        )
        
      ),
      grid_card(
        area = "Winding",
        full_screen = TRUE,
        card_header(
          "Price return summary"
        ),
        card_body(#----------------------------------winding table 121
          gt::gt_output('x121a'),
          gt::gt_output('x121b'),
          height=gridheight
        )
        
      ),
      grid_card(
        area = "characteristics",
        full_screen = TRUE,
        card_header(
          "Index characteristics"
        ),
        card_body(#---------------------------------characteristics table x122
          gt::gt_output('x122')
        )
      ),
      grid_card(
        area = "summary",
        full_screen = TRUE,
        card_header(
          "Summary"
        ),
        card_body(#--------------------------------return summary table x131
          gt::gt_output('x131'),
          height=gridheight
        )
      )
      ,
      grid_card(
        area = "tradesummary",
        full_screen = TRUE,
        card_header(
          "Trade recap"
        ),
        card_body(#--------------------------------trade recap table x132
          gt::gt_output('x132a'),
          gt::gt_output('x132b'),
          gt::gt_output('x132c'),
          gt::gt_output('x132d'),
          height=gridheight2
        )
      )
    )
  )
}

ui_navpanel_listing <- function(id=NULL) {
  nav_panel(title = "Listing",#------------------------------------------Listing
            
            grid_container(
              layout = c(
                "locallist  customlist"
              ),
              row_sizes = c(
                "1fr"
              ),
              col_sizes = c(
                "1fr",
                "1fr"
              ),
              grid_card(
                area='locallist',
                full_screen = TRUE,
                card_header(
                  "Local"
                ),
                card_body(#-------------- listing table 211
                  gt::gt_output('x211')
                )
              ),
              grid_card(
                area='customlist',
                full_screen = TRUE,
                card_header(
                  "Custom"
                ),
                card_body(#-------------- listing table 211cu
                  gt::gt_output('x211cu')
                )
              )
            )
  )}

ui_navpanel_constituents <- function(id=NULL) {
  nav_panel(title = "Constituents",#--------------------------------Constituents
            card(
              full_screen = TRUE,
              card_header(
                "Constituent districts"
              ),
              card_body(#-------------- constituent table 311
                DT::DTOutput('x311'),
                height=gridheight3
              ),
              height=gridheight3
            )
  )}

ui_navpanel_accuracy <- function(id=NULL) {
  nav_panel(title = "Accuracy",#----------------------------------------Accuracy
            card(
              full_screen = TRUE,
              card_header("RMS error sensitivity to key parameters"),
              card_body(
                grid_container(
                  layout = c(
                    "timesamplinglocal      timesamplingcustom",
                    "outlierrejectionlocal  outlierrejectioncustom",
                    "crossvalidationlocal   crossvalidationcustom"
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
                    area = "timesamplinglocal",
                    full_screen = TRUE,
                    card_header(
                      "Time Sampling"
                    ),
                    card_body(#--------------accuracy 411
                      gt::gt_output('x411')
                    )
                  ),
                  grid_card(
                    area = "timesamplingcustom",
                    full_screen = TRUE,
                    card_header(
                      "."
                    ),
                    card_body(#--------------accuracy 412
                      gt::gt_output('x412')
                    )
                  ),
                  grid_card(
                    area = "outlierrejectionlocal",
                    full_screen = TRUE,
                    card_header(
                      "Outlier Rejection"
                    ),
                    card_body(#--------------accuracy 421
                      gt::gt_output('x421')
                    )
                  ),
                  
                  grid_card(
                    area = "outlierrejectioncustom",
                    full_screen = TRUE,
                    card_header(
                      "."
                    ),
                    card_body(#--------------accuracy 412
                      gt::gt_output('x422')
                    )
                  ),
                  grid_card(
                    area = "crossvalidationlocal",
                    full_screen = TRUE,
                    card_header(
                      "Cross Validation"
                    ),
                    card_body(#--------------accuracy 431
                      gt::gt_output('x431')
                    )
                  ),
                  grid_card(
                    area = "crossvalidationcustom",
                    full_screen = TRUE,
                    card_header(
                      "Cross Validation"
                    ),
                    card_body(#--------------accuracy 432
                      gt::gt_output('x432')
                    )
                  )
                )
              )
            )
  )
}

ui_navpanel_notes <- function(id=NULL) {
  nav_panel(title = "Notes",#----------------------------------------------Notes
            
            card(
              full_screen = TRUE,
              card_body(
                htmltools::includeMarkdown("notes.Rmd")
              )
            )
  )
}

ui_card_action <- function(id=NULL) {
  grid_card(
    area = "action",  #--------------------------------------------action button
    card_header("Custom"),
    card_body(
      div(style = "padding:4px",
          span(
            textOutput('defrc6'), 
            style="padding:0px;color:black;font-size:12px"
          ),
          span(
            textOutput('selrc6'), 
            style="padding:0px;color:black;font-size:12px"
          ),
          span(
            textOutput('comrc6'), 
            style="padding:0px;color:black;font-size:12px"
          ),
          conditionalPanel(
            condition="output.comrc6forjstest==output.selrc6forjstest",
            span(
              textOutput('cuseqcom'), 
              style="color:black;font-size:12px"),
          ),
          conditionalPanel(
            condition="output.comrc6forjstest!=output.selrc6forjstest",
            span(
              textOutput('cusnecom'), 
              style="color:red;font-size:12px"
            ),
          ),
          actionButton("docusabC", "Recompute custom"),
      ),
    )
  )
}

ui_card_sidebar <- function(id=NULL) {
  grid_card(
    area = "sidebar", #--------------------------------------------------sidebar
    card_header("Selection"),
    card_body(
      treeInput( #district---target
        inputId = "rc6tC",
        label = "Target district",
        choices = create_tree(
          f240824b(rev(sort(unique(
            substr(dir('smallrip/'),1,3)
            ))))[(order(regionx,lab))]),
        selected = (rc6cuC[1]), 
        returnValue = "text",
        closeDepth = 0
      )
      ,
      sliderInput(
        inputId = "tslider",
        label = "Datum period",
        min=0,
        max=45,
        value=34
      )
      ,
      treeInput( #district---custom
        inputId = "rctreeC",
        label = "Custom selection",
        choices = create_tree(f240824b(rev(sort(unique(
          substr(dir('smallrip/'),1,3)
          ))))[(order(regionx,lab))]),
        selected = rc6cuC, 
        returnValue = "text",
        closeDepth = 0
      )
      ,
      span(
        textOutput('selrc6forjstest'), 
        style="color:white"
        )
      ,
      span(
        textOutput('comrc6forjstest'), 
        style="color:white"
        ) 
    )
  )
}

ui_card_header <- function(id=NULL) {
  grid_card_text(#--------------------------------------------------------header
    area = "header",
    content = "Local and custom index",
    alignment = "start",
    is_title = FALSE
  )    
}

ui_area2 <- function(id=NULL) {
  grid_card(
    area = "area2", #-----------------------------------------area2: page_navbar
    card_body(
      page_navbar(
        fillable=F,
        position='fixed-top'
        ,
        ui_navpanel_tss()#--------------------------Time-series summary
        ,
        ui_navpanel_listing()#----------------------Listing
        ,
        ui_navpanel_constituents()#-----------------Constituents
        ,
        ui_navpanel_accuracy()#---------------------Accuracy
        ,
        ui_navpanel_notes()#------------------------Notes
      )
    )
  )
}
