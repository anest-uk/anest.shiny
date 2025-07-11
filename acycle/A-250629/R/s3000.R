s3000 <- function(input, output, session, common) {
   output$datet0 <- 
      renderText({
        common$dfnyR()[input$tslider+1]%>%
          as.Date()%>%
          as.character(.)
      })
}