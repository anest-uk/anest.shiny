  output$perfcust <-  #gt winding
    render_gt(
      f240823a(x1=Rrsi0())%>%
        .[]%>%
        gt::gt(. ,rownames_to_stub = T)
      
    )
  
          grid_container(
            layout = c(
              "custom_control xchartcus leafletcus",
              "custom_control perfcust binchacus "
            ),
            grid_card(
              area = "perfcust",
              div(
                gt_output('perfcust'),
                style = "font-size:25%"
              )
            ),

            
            
              
  # Rselectedrc <- #rc
  # eventReactive(
  #   input$go.custom.b,
  #   {
  #     input$ID1[which(nchar(input$ID1)==6)]
  #   }
  # )
  
  
  # output$estdt.nat.t <-  #gt estdt - decided not to show this
  #   render_gt(
  #     z321$ses$estdt[nx==z321$geo[rc9==regpcode(input$tgtrc6),nx]]%>%
  #       .[,.(np=nx,date=as.Date(date1),days,xdot=round(xdot,4),xdotse=round(xdotse,4),x=round(x,4),xse=round(xse,4))]#,
  #   )

  
  
        #.[]%>%
      #gt(. ,rownames_to_stub = T)
      
  # Rrdt <- #returns
  #   eventReactive(
  #     input$go.custom.b,
  #     {
  #       Rselectedrow()
  #       coread(Rselectedrc(),'03rip/')[]
  #     }
  #   )
  
# Rrsi <- #Rgeo -> RSI -> ggplot
  #   eventReactive(
  #     input$go.custom.b,
  #     {
  #       x <- f230312a(  #solve single nx -> estdt with no pra
  #         nxx=1,
  #         steprip='03rip/',
  #         dfn=dfnx,
  #         geo=Rgeo()
  #       )
  #       rsi.g <<- x
  #       print(x)
  #       ggplot(
  #         x,
  #         aes(date,x)
  #       )+
  #         geom_line()+
  #         xlab('')+
  #         ylab(bquote(Delta~P~log~price~change))+
  #         theme_bw() +
  #         theme(
  #           axis.line = element_line(colour = "black"),
  #           panel.grid.major = element_line(size=pgms,linetype = pgmt,color=pgmc),
  #           panel.grid.minor = element_blank(),
  #           panel.border = element_blank(),
  #           panel.background = element_blank(),
  #           text=element_text(size=16,face='plain'),
  #           axis.line.y.left=element_line(size=.1),
  #           axis.line.x.bottom=element_line(size=.1),
  #           legend.position='none')+
  #         scale_x_date(
  #           breaks = as.Date(c('1995-01-01','2000-01-01','2010-01-01','2020-01-01','2024-01-01')),
  #           date_labels = "%Y",
  #           limits=c(as.Date(c('1994-12-31','2027-12-31')))
  #         )
  #     }
  #   )
  # 
