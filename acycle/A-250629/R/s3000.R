server_sidebar <- function(input, output, session, common) {
   output$datet0 <- # this does not really belong here but a separate file seems excessive
      renderText({
        common$dfnyR()[input$tslider+1]%>%
          as.Date()%>%
          as.character(.)
      })
}