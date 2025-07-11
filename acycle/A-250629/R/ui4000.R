ui4000 <- function(id=NULL) {
  grid_card(
    area = "a4000", #-----------------------------page_navbar 4000
    card_body(
      page_navbar(
        fillable=F,
        position='fixed-top'
        ,
        ui_navpanel_tss()#-----Time-series summary
        ,
        ui_navpanel_listing()#-------------Listing
        ,
        ui_navpanel_constituents()#---Constituents
        ,
        ui_navpanel_accuracy()#-----------Accuracy
        ,
        ui_navpanel_notes()#-----------------Notes
        ,
        ui4100()#-timeseries gen2-------
        ,
        ui4200()#-listing gen2----------
        ,
        ui4300()#-constituents gen2-----
     )
    )
  )
}
