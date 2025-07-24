ui4000 <- function(id=NULL) {
  grid_card(
    area = "A4000", #-----------------------------page_navbar 4000
    card_body(
      page_navbar(
        fillable=F,
        position='fixed-top'
        #,
        # ui_navpanel_tss()#-----Time-series summary gen1
        # ,
        # ui_navpanel_listing()#-------------Listing gen1
        # ,
        # ui_navpanel_constituents()#---Constituents gen1
        ,
        ui4100()#-timeseries ---------------------gen2----
        ,
        ui4200()#-listing ------------------------gen2----
        ,
        ui4300()#-constituents -------------------gen2----
         ,
        ui4400()#-accuracy -------------------gen2----
       ,
        ui_navpanel_accuracy()#-----------Accuracy gen1
        ,
        ui_navpanel_notes()#-----------------Notes gen1
     )
    )
  )
}
