
#college explorer UI file

library(shiny)
library(shinyjs)
library(leaflet)

#basically, I have one page filled with a leaflet map and one absolutePanel filled with rows and columns

shinyUI(bootstrapPage(theme="bootstrap.css",
                      shinyjs::useShinyjs(),                      
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  tags$head(includeScript("google_analytics.js")),
  
  leafletOutput("map", width = "100%", height = "100%"),
  
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 50, left = "auto", right = 20, bottom = "auto",
                width = 490, height = "auto",
  ##################check boxes to select types of schools
  fluidRow(
    column(7,div(style="display:inline-block",h3("College Explorer v0.3"))),
    column(5,h6("(", textOutput("num_matching", inline=TRUE),"schools selected)",style="padding:20px 0 0 0;"))),
  fluidRow(
    column(4,checkboxGroupInput("ownershiptype",label=NULL,
                                 c("Public"=1,"Private non-profit"=2, "Private for-profit"=3),
                                 selected=c(1,2,3))),
    column(5, checkboxGroupInput("type", label=NULL,
                                c("Doctoral Universities","Master's Colleges","Baccalaureate Colleges", "Associate's Colleges"),
                                selected=c("Doctoral Universities","Master's Colleges","Baccalaureate Colleges", "Associate's Colleges"))),
    column(3, checkboxInput("online", "Online Only", TRUE))
    ),
  
  ####################sliders to set various criteria
  fluidRow(
    column(6,
    sliderInput("selectivity", "Selectivity (rejection rate):",
              min=0,max=100, step=1, value=c(0,100), dragRange=FALSE, ticks=FALSE, post="%")),
    column(2,checkboxInput("selectivityNA", label = "Include NAs?", TRUE))),
   
  #the UI below uses shinyjs which allows the UI (sliders) to "unfold" when the title is clicked
  h5("Filter on size", a(id = "togglesize", "show/hide")),
  shinyjs::hidden(div(id = "filtersize",
      fluidRow(
       column(4, sliderInput("size","Undergrad size",min = 0, max = 35000, value = c(0,35000), step=1000, ticks=FALSE)),
       column(1,h6("NAs?"), checkboxInput("sizeNA",label=NULL, value=TRUE)),
       column(4, offset = 1, sliderInput("totsize","Total size",min = 0, max = 35000, value = c(0,35000), step=1000, ticks=FALSE)),
       column(1,h6("NAs?"), checkboxInput("totsizeNA",label=NULL, value=TRUE))))),
  
  h5("Filter on SAT scores", a(id = "toggleSAT", "show/hide")),
  shinyjs::hidden(div(id = "filterSAT",
      fluidRow(
        column(4, sliderInput("satvr","75th Pctile Verbal SAT",min = 300, max = 800, value = c(300,800), step=10, ticks=FALSE)),
        column(1,h6("NAs?"), checkboxInput("satvrNA",label=NULL, value=TRUE)),
        column(4, offset = 1, sliderInput("satmt","75th Pctile Math SAT",min = 300, max = 800, value = c(300,800), step=10, ticks=FALSE)),
        column(1,h6("NAs?"), checkboxInput("satmtNA",label=NULL, value=TRUE))))),
  
  h5("Filter on price", a(id = "toggleprice", "show/hide")),
  shinyjs::hidden(div(id = "filterprice",
      fluidRow(
        column(4,sliderInput("tuition", "Tuition and fees", min=0, max=60000, step=1000,ticks=FALSE, pre="$", value=c(0,60000))),
        column(1,h6("NAs?"), checkboxInput("tuitionNA",label=NULL, value=TRUE)),
        column(4, offset = 1, sliderInput("netprice", "Average Net Price", min=0,max=50000, step=1000, value=c(0,50000), ticks=FALSE, pre="$")),
        column(1,h6("NAs?"), checkboxInput("netpriceNA",label=NULL, value=TRUE))))),
  
  h5("Filter on outcomes", a(id = "toggleoutcomes", "show/hide")),
  shinyjs::hidden(div(id = "filteroutcomes",
      fluidRow(
        column(4,sliderInput("gr", "Graduation Rate", min=0, max=100, step=1,ticks=FALSE, post="%", value=c(0,100))),
        column(1,h6("NAs?"), checkboxInput("grNA",label=NULL, value=TRUE)),
        column(4, offset = 1, sliderInput("retention", "Retention Rate", min=0,max=100, step=1, value=c(0,100), ticks=FALSE, post="%")),
        column(1,h6("NAs?"), checkboxInput("retentionNA",label=NULL, value=TRUE))))),
  
  h5("Filter on financial outcomes", a(id = "togglefinoutcomes", "show/hide")),
  shinyjs::hidden(div(id = "filterfinoutcomes",
                      fluidRow(
                        column(4,sliderInput("salary", "Med Salary (6yrs)", min=5000, max=125000, step=5000,ticks=FALSE, pre="$", value=c(5000,125000))),
                        column(1,h6("NAs?"), checkboxInput("salaryNA",label=NULL, value=TRUE)),
                        column(4, offset = 1, sliderInput("default", "Default Rate", min=0,max=100, step=1, value=c(0,100), ticks=FALSE, post="%")),
                        column(1,h6("NAs?"), checkboxInput("defaultNA",label=NULL, value=TRUE))))),
  
  #######################################graph controls
  tags$hr(),
  fluidRow(
   column(6, selectInput("sizevar", "Size variable:", 
              choices = c("Constant"="constant","Undergraduate size"="size_trimmed", 
                          "Selectivity"="selectivity", "Tuition and fees"="CHG1AY3",
                          "Net Price"="netprice", "Med Salary (6yrs after)"="salary", "Graduation rate" = "gr",
                          "Retention rate"= "RET_PCF"))),
   column(6, selectInput("colorvar", "Color variable:", 
              choices = c("Type of ownership"="ownership","Level of instruction"="type", "Testing"="testing")))),
  
  #######################################about this app
  h6(a(id="toggleabout","About this app")),
  shinyjs::hidden(div(id="about",
  tags$div(HTML("<h6> This app is inspired <a href='https://shiny.rstudio.com/gallery/superzip-example.html' 
        target='_blank'> ZipExplorer </a>. It uses data from <a href='https://nces.ed.gov/ipeds/Home/UseTheData' 
        target='_blank'> IPEDS</a> and <a href='https://collegescorecard.ed.gov/data/' target='_blank'>
                College Scorecard</a>. If you have any questions and suggestions email me at 
                <a href='mailto:dvorakt@union.edu' target='_top'>dvorakt@union.edu.</a> <br><br>
                - Clicking on a college will display more info and a link to the college's website.<br><br>
                - NA stands for 'not available.' If the 'Include NA?' box is checked, colleges that do not provide 
                info on a particular variable are <i>included</i>.</h6>"))))
)))
