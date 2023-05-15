# Libraries, setting the working directory to the directory of the file
# Note: If you want to upload the script to Shiny, you should turn the setwd(...) into comment
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(BAMMtools)
library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
library(shiny)

df <- read.csv("Data/szurt.csv")|> as.data.frame()
szja <- read.csv2("Data/szja.csv")
maphu = st_read("alapterkep/telepules.shp")
maphu = st_make_valid(maphu)
szjamap <- merge(maphu,szja, by.y="ELEM_KOD",by.x="TELKOD")
szjamap$JARAS_NEV <- paste(szjamap$JARAS_NEV," járás")
szjamap[is.na(szjamap$JARASNEV),]$JARAS_NEV <- "Budapest"
# Reading the necessary data
df$kikialtasi_ar <- as.numeric(gsub(" ","",df$kikialtasi_ar))
df$minimal_ar<- as.numeric(gsub(" ","",df$minimal_ar))
df$aktualis_licit<- as.numeric(gsub(" ","",df$aktualis_licit))
df$haz <- 1
df <- df[df$kikialtasi_ar <= quantile(df$kikialtasi_ar,0.95),]
# setting up the getInput() function the be able to choose data in the app
# getInput = function(var,ymin,ymax){
#   inputdf <- df[df$ev >= ymin&df$ev <= ymax,]
#   return(inputdf[,var])
# }

# The ui to the app
ui = fluidPage(
  fluidRow(
    column(3,
           selectInput("var","Heatmap változója:", choices = c( "Helyzet"="haz",
           																						"Kikiáltási ár" = "kikialtasi_ar",
                                                      "Aktuális licit" = "aktualis_licit",
                                                      "Minimál ár" = "minimal_ar"),
                                                      selected = "haz"),
           # selectInput("year","Év:", choices = c(1995:2020), selected = 2018)),
				    sliderInput("year","Évek:",min = 1995, max = 2023, value = c(1995, 2023)),
				    sliderInput("ar","Kikiáltási ár:",min = min(df$kikialtasi_ar), max = max(df$kikialtasi_ar),
				    						value = c(min(df$kikialtasi_ar), max(df$kikialtasi_ar)))),
    column(9,
           leafletOutput("mymap", height = 600),
    			 h3("SZJA alap/állandó lakos településenként (eFt)"),
           leafletOutput("mymap2", height = 600))
))

# The server to the app
server = function(input, output, session){

  #getting the data based on what we've set to be shown
  # dataInput = reactive({
    # getInput(input$var,input$year[1],input$year[2])
  # })
  # dfs <- df[df$ev >= input$year[1]&df$ev <= input$year[2],]
  bins = c(0,quantile(szjamap$VALUE, probs = c(.2,.4,.6,.8)), Inf)
  pal = colorBin("YlGnBu", domain = szjamap$VALUE, bins = bins)

  output$mymap = renderLeaflet({
	print(input)
    # Basically, this is the map, we're showing
  dfs <- df[df$ev >= input$year[1]&df$ev <= input$year[2]&df$kikialtasi_ar>=input$ar[1]&df$kikialtasi_ar<=input$ar[2],]
    leaflet(dfs) %>%
      addTiles() %>%
  		 addHeatmap(lng=dfs$lon,lat=dfs$lat,intensity=dfs[,c(input$var)],max=ifelse(input$var =="haz",100,0.8*10^9),radius=15,blur=10,group = "Heatmap")
  })
  #településenkénti
  output$mymap2 = renderLeaflet({
    leaflet(szjamap) %>%
      addTiles() %>%
    	addPolygons(
    		group = "Településenkénti SZJA alap",
    		weight = 1,
    		opacity = 1, color = "white",
    		dashArray = "3",
    		highlightOptions = highlightOptions(
    			weight = 5, color = "orange",
    			dashArray = "", fillOpacity = 1),
    		fillColor = ~pal(szjamap$VALUE), fillOpacity = 1,
    		popup = paste0(szjamap$JARAS_NEV, "<br>", round(szjamap$VALUE,2)," eFt"))%>%
    	 addLegend(pal=pal, values = szjamap$VALUE, position = "topright",title = "SZJA alap/bejegyzett lakos (EFt)",group = "Településenkénti SZJA alap")
  })

}

# running the app
shinyApp(ui,server)
