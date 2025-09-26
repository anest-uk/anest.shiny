#-------------------------------------------------4400 listing
print('<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< s4400 ')
s4400 <-
  function(input, output, session, common) {
    RR4411 <- #-  ----
      eventReactive(
        list(
          rc6tx = common$rc6tR(),
          bespx = RR4413() #bespoke row
        ),
        {
          if (verbose) print("enter RR4411")
          x0 <- RR4413()[,.(b,geo,tbin,trim,method,rsqk,rmse,over)] #bespoke
          x1 <- DD4411( rc6t = common$rc6tR() , x0=x0)
          x2 <- DD4412( rc6t = common$rc6tR() )
          #x3 <- rbind(x2,RR4412())
          x <- 
            list(
              tab=x1,
              sum=x2
            )
          G4411 <<- copy(x)
          x
        }
      )
    
    RR4413 <- #-bespoke row depends rescR()  ----
    eventReactive(
      list(
        rescx = common$rescR(),
        rc6tx = common$rc6tR()
      ),
      {
        if (verbose) print("enter RR4412")
        x <-
          common$rescR()$kfoldsse%>% #from bespoke
          .[,.(nx=0,ssek,ssei,ssra=sser,toti,ssta=sstr,n=n,ninl=n,nraw=n,rc6=rc6)]%>%
          .[,.( #fields returned by f250723a('SW-10-',xx)
            b='',
            rc6,
            n,
            nx=0,
            time='drc',
            method='acc',
            geo='custom',
            tbin='variance',
            trim='10%',
            rsqk = round(1 - ssek / toti, 4), 
            rmse = round(sqrt(ssek / n), 4),
            insam = round(sqrt(ssei / n), 4)
          )] %>%
          .[rc6==common$rc6tR()]%>%
          .[, over := round((rmse - insam), 4)] %>%
          .[order(rmse)] %>%
          .[, bias := rmse - rmse[b == "*"]] %>%
          .[, total := bias + over] %>%
          .[order(rmse)] %>%
          .[]%>%
          sco(.,F)
        G4413 <<- copy(x)
        x
      }
    )

    output$OO4411a <- gt::render_gt(RR4411()[['tab']]) # table    #----
    output$OO4411b <- gt::render_gt(RR4411()[['sum']]) # summary    #----
  }
