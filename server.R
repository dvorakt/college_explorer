
#college explorer server.R

library(shiny)
library(shinyjs)
library(leaflet)
library(tidyverse)

#plot.csv is generated using program called "get using full ipeds datafiles.R"
# it has cleaned up IPEDS and College Scorecard data
plot <- read_csv("www/plot.csv")

shinyServer(function(input, output) {
  
  # create dataframe 'data' to be plotted starts with dataframe 'plot', filtering depends on inputs from the UI
  data <- reactive({
    data <- plot
    
    data <- filter(data, CONTROL %in% input$ownershiptype)
    data <- filter(data, type %in% input$type)
    if(!input$online){data <- filter(data, is.na(online))}
    
    if(input$satvrNA){data <- filter(data, (SATVR75 >= input$satvr[1] & SATVR75 <= input$satvr[2]) | is.na(SATVR75))}
    else{data <- filter(data, (SATVR75 >= input$satvr[1] & SATVR75 <= input$satvr[2]))}
    
    if(input$satmtNA){data <- filter(data, (SATMT75 >= input$satmt[1] & SATMT75 <= input$satmt[2]) | is.na(SATMT75))}
    else{data <- filter(data, (SATMT75 >= input$satmt[1] & SATMT75 <= input$satmt[2]))}
    
    if(input$sizeNA){data <- filter(data, (size_trimmed >= input$size[1] & size_trimmed <= input$size[2]) | is.na(EFYTOTLT))}
    else{data <- filter(data, (size_trimmed >= input$size[1] & size_trimmed <= input$size[2]))}
    
    if(input$totsizeNA){data <- filter(data, (totsize_trimmed >= input$totsize[1] & totsize_trimmed <= input$totsize[2]) | is.na(totsize))}
    else{data <- filter(data, (totsize_trimmed >= input$totsize[1] & totsize_trimmed <= input$totsize[2]))}
    
    if(input$netpriceNA){data <- filter(data, (netprice_trimmed>= input$netprice[1] & netprice_trimmed<= input$netprice[2]) | is.na(netprice_trimmed))}
    else{data <- filter(data, (netprice_trimmed>= input$netprice[1] & netprice_trimmed<= input$netprice[2]))}
      
    if(input$tuitionNA){data <- filter(data, (tuition>= input$tuition[1] & tuition<= input$tuition[2]) | is.na(tuition))}
    else{data <- filter(data, (tuition>= input$tuition[1] & tuition<= input$tuition[2]))}
    
    if(input$selectivityNA){data <- filter(data, (selectivity>= input$selectivity[1] & selectivity<= input$selectivity[2]) | is.na(selectivity))}
    else{data <- filter(data, (selectivity>= input$selectivity[1] & selectivity<= input$selectivity[2]))}
    
    if(input$grNA){data <- filter(data, (gr>= input$gr[1] & gr<= input$gr[2]) | is.na(gr))}
    else{data <- filter(data, (gr>= input$gr[1] & gr<= input$gr[2]))}
    
    if(input$retentionNA){data <- filter(data, (RET_PCF>= input$retention[1] & RET_PCF<= input$retention[2]) | is.na(RET_PCF))}
    else{data <- filter(data, (RET_PCF>= input$retention[1] & RET_PCF<= input$retention[2]))}
    
    if(input$salaryNA){data <- filter(data, (salary>= input$salary[1] & salary<= input$salary[2]) | is.na(salary))}
    else{data <- filter(data, (salary>= input$salary[1] & salary<= input$salary[2]))}
    
    if(input$defaultNA){data <- filter(data, (default>= input$default[1] & default<= input$default[2]) | is.na(default))}
    else{data <- filter(data, (default>= input$default[1] & default<= input$default[2]))} 
    
  })
  
  # create a basic map without any markers, (the markers are added later using leafletProxy)
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      setView(lng = -73.85, lat = 37.45, zoom = 4)
      })

  # observe is a function that can modify objects based on changes in inputs
  # I need observe to run leafletProxy when I change my data()
  observe({if(nrow(data())!=0){   #some of the functions below break down if data() is empty so I need this if statement
    #below is how the colors and the legend work, each categorical variable has a sibling that contains color
    #associated with each value of the variable, e.g. type has typecolor
    #below I find the position of the colorvar and the colors are in the column right next to it
    #I am not using a pallete because when I filter data and the set of possible values changes
    #the colors would change, e.g. public is green and then blue, that is why I want to "hard code" the colors
    pos <- match(input$colorvar,names(plot)) # find the column number of the colorvariable
    colors <- data()[[pos+1]] #this contains vector of colors for each datapoint that will be plotted
    colors_list <- unique(plot[[pos+1]]) #this gives vector of all possible colors(it uses plot, because I want the legend to be full even if not all values are displayed)
    values_list <- unique(plot[[pos]]) #this gives vector of all possible values - used for legend
    # below is how I control size of the marker - 
    # the size variable has to be "standartized/scaled" because depending on what is chosen as the size variable, these variables could have totally different scale
    x <- data()[[input$sizevar]] #put the size var in vector x
    # I take the 95th percentile of the x (size variable) to scale all of the values in x
    # I took 95th percentile instead of maximum because there are some outlier and they may make the majority of markers tiny
    # make that proportional to the sqrt of size and multiply by 440 - a constant that makes the size ok (not too big or too small)
    size <- sqrt(x/quantile(x,0.95,na.rm=TRUE)*440) 
    leafletProxy("map", data= data()) %>%
        clearMarkers() %>% #you have to clear previously drawn markers
        addCircleMarkers(lng=~lon, lat=~lat, stroke = FALSE, popup=~popup, 
                         label= ~INSTNM, radius= size, fillOpacity = 0.5,
                         fillColor = colors) %>%
        addLegend("bottomleft",  colors=colors_list, labels=values_list, layerId="legend")
    }
    else{leafletProxy("map") %>% clearMarkers()} #clear the map if the data() is empty
    })
  
  output$num_matching <- renderText({format(nrow(data()),big.mark = ",")}) #display how many schools match the criteria
   
  # output$list <- renderDataTable(select(data(), linked_name, adm_rate, SATVR75),options = list(
  #   pageLength = 25,spacing="xs"),escape = FALSE)
  
  #below is what is needed for the "unfolding" UI
  shinyjs::onclick("toggleprice",
                   shinyjs::toggle(id = "filterprice", anim = TRUE))
  shinyjs::onclick("toggleSAT",
                   shinyjs::toggle(id = "filterSAT", anim = TRUE))
  shinyjs::onclick("toggleoutcomes",
                   shinyjs::toggle(id = "filteroutcomes", anim = TRUE))
  shinyjs::onclick("togglefinoutcomes",
                   shinyjs::toggle(id = "filterfinoutcomes", anim = TRUE))
  shinyjs::onclick("togglesize",
                   shinyjs::toggle(id = "filtersize", anim = TRUE))
  shinyjs::onclick("toggleabout",
                   shinyjs::toggle(id = "about", anim = TRUE))
 
})