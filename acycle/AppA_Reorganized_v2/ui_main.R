

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
