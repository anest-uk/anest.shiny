library(shiny)
library(data.table)
library(DT)
#library(pxmo)
regpcode <- function(rawcode='W1U 4RL',x=parsepcode(rawcode)) {
  rawcode <- gsub(patt='  ',rep=' ',rawcode)
  Reduce(paste0,lapply(x,pad1))
}

parsepcode <- function(pc) {
  x <- suppressWarnings(lapply(data.frame(Reduce(rbind,lapply(lapply(lapply(pc,ppc),data.table),t))),unlist))
  x <- lapply(x,`names<-`,NULL)
  names(x) <- names(ppc(pc[1]))
  x
}

ppc <- function(pc='EC2R 8AH') {
  if(nchar(pc)<2) return(list(area=ifelse(grepl('[A-Z,a-z]',pc),paste0(toupper(pc),'--'),''),district='',sector='',unit=''))
  chkpcode(pc)
  pc <- toupper(pc)
  gg <- gregexpr(patt=' ',pc)
  x <- strsplit(pc,split=' ')
  out <- unlist(lapply(x,'[[',1))
  nout <- nchar(out)
  inum <- as.numeric(regexpr("[0-9]",out))
  area <- pc
  sector <- unit <- district <- rep('',length(pc))
  area[inum==2] <- substr(out[inum==2],1,1)
  area[inum==3] <- substr(out[inum==3],1,2)
  district[inum==2] <- substring(out[inum==2],2)
  district[inum==3] <- substring(out[inum==3],3)
  if(any(lapply(x,length)>1)) { #inbound code exists
    stopifnot(all(lapply(x,length)==2)) #exists for all
    inb <- unlist(lapply(x,'[[',2))
    nin <- nchar(inb)
    sector <- substr(inb,1,1)
    unit <- substring(inb,2,nin)
  }
  list(area=area,district=district,sector=sector,unit=unit)
}

chkpcode <- function(pc='EC2R 8AH') {
  #composed of correct chars
  #grepl(patt='[^a-zA-Z0-9]/',x=pc,perl=TRUE)
  #right length
  nch <- sapply(pc,nchar)
  stopifnot(all(nch<=8))
  #max one space
  stopifnot(unlist(lapply(gregexpr(patt=' ',pc),length))==1)
  #is either 1-part or 2-part
  x <- strsplit(pc,split=' ')
  #stopifnot(all(lapply(x,length)==1)||all(lapply(x,length)==2))
  #1-part always starts with alpha cap
  if(length(x[[1]])==1) {
    stopifnot(all(unlist(gregexpr(pc,patt='^[A-Z,a-z]'))==1))
  }
  #2-part always starts with number
  if(length(x[[1]])==2) {
    pcin <- lapply(x,'[[',2)
  }
  TRUE
}

pad1 <- function(x) {
  n1 <- nchar(x)
  x[n1==1] <- paste0(x[n1==1],paste(collapse ='',rep(rcs(),2)))
  x[n1==2] <- paste0(x[n1==2],rcs())
  x
}

rcs <- function(){'-'}


# Define UI for dataset viewer app ----
ui <- fluidPage(
  tags$head(tags$link(rel="shortcut icon", href="Favicon Transparent.ico")),
  # App title ----#"width:104px; height:77px"
  titlePanel(
    title=htmltools::div(
      htmltools::p(),
      #htmltools::img(src="Transparent Logo No Slogan.png",style="width:72px; height:52px"),
      htmltools::img(src="Transparent Logo.png",style="width:42px; height:42px"),
      htmltools::p(),
      htmltools::p(),
      "Identifier lookup from address"
    ),
    windowTitle = "idhash(address)"
  ),

  # Sidebar layout with a input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
       # Input: Selector for choosing dataset ----
      textInput(inputId = "postcode",
                  label = "postcode",
                value="WC2A"
                )
      ,
      textInput(inputId = "saon",
                label = "flat (saon)")
      ,
      textInput(inputId = "paon",
                label = "building (paon)")
      ,
      textInput(inputId = "street",
                label = "street")
      ,
      h6("
         Note:
         "),
      h6("
         This browser is an audit tool to look up idhash from a single unique postal address.
         "),
      h6("
         Matching is permissive so supplying partial address will lead to multiple matches and matching across fields is also permitted.
         "),
      h6("
         Only properties sold since 1995 are present.
         "),
      h6("
         Presence of an identifier does not mean the transactions are present in the analysis dataset after screening.
         "),
      h6(paste0("
         If incomplete address input causes multiple matches then a subset is tabulated.
         ")),
      h6(paste0("
         The table can be reordered and searched.
         ")),
      h6("
         Definitions of saon, paon can be found at https://www.gov.uk/guidance/about-the-price-paid-data
         "),
      # h6("
      #    This browser can be used in conjunction with the repeat-sales-from-id app here: https://anest-uk.shinyapps.io/repeat-sale-return/
      #    "),
      h6("
         Source: Land Registry Price Paid Data
         "),
      h6("
         Driven by R and shinyapps.io
         "),
      h6("
         Beta test
         ")

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: HTML table with requested number of observations ----
      div(dataTableOutput("view"), style = "font-family:consolas")

    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {


  # Return the requested dataset ----
  datasetInput <- reactive({
    x1a <- regpcode(input$postcode)
    x1b <- ifelse(nchar(x1a)==12,'',paste0('.{',12-nchar(x1a),'}'))
    x1 <- paste0(x1a,x1b) #regex for rc12
    x2 <- substr(x1,1,3) #rc3
    x3 <- paste0('scs/',x2,'.csv') #fpath
    if(file.exists(x3)) {
      x4 <- fread(x3)
      x6 <- ifelse(nchar(input$saon)>0,paste0(gsub(' ','-',toupper(input$saon)),'.*'),'.*')
      x7 <- ifelse(nchar(input$paon)>0,paste0(gsub(' ','-',toupper(input$paon)),'.*'),'.*')
      x8 <- ifelse(nchar(input$street)>0,paste0(gsub(' ','-',toupper(input$street)),'.*'),'')
      x9 <- paste0(x6,
                   x7,
                   x8
      )
      x10 <- paste0('^',x1,x9)
      x5 <- head(x4[grep(x10,id),.(n.transactions=ntr,id,idhash)][order(id)],Inf)
    } else {
      x5 <- NULL
    }
    x5
  })

  output$view <- renderDT({datasetInput()},options = list(dom = 'ft'))

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

