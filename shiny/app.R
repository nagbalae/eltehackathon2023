# Libraries, setting the working directory to the directory of the file
# Note: If you want to upload the script to Shiny, you should turn the setwd(...) into comment
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# library(BAMMtools)
library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
library(shiny)
library(viridis)
# devtools::install_version("MASS","7.3-58.4")

df <- read.csv("Data/szurt.csv")|> as.data.frame()
szja <- read.csv2("Data/szja.csv")
maphu = st_read("alapterkep/telepules.shp")
maphu = st_make_valid(maphu)
szjamap <- merge(maphu,szja, by.y="ELEM_KOD",by.x="TELKOD")
szjamap$JARAS_NEV <- paste(szjamap$JARAS_NEV," járás")
szjamap[is.na(szjamap$JARASNEV),]$JARAS_NEV <- "Budapest"
#%>% Reading the necessary data
df$kikialtasi_ar <- as.numeric(gsub(" ","",df$kikialtasi_ar))
df$minimal_ar<- as.numeric(gsub(" ","",df$minimal_ar))
df$aktualis_licit<- as.numeric(gsub(" ","",df$aktualis_licit))
df$haz <- 1
df <- df[df$kikialtasi_ar <= quantile(df$kikialtasi_ar,0.95),]
df$kat<-NA
df$kat <- ifelse(df$aktualis_licit/df$kikialtasi_ar>1, "felett","alatt")
df$kat <- ifelse(df$aktualis_licit/df$kikialtasi_ar==1,"megegyezik",df$kat)
df$kat <- as.factor(df$kat)
summary(df$kat)
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
                                                      "Minimál ár" = "minimal_ar",
           																						"Kikáltásihoz képest akt. lic"="kat"),
                                                      selected = "haz"),
           # selectInput("year","Év:", choices = c(1995:2020), selected = 2018),
	    checkboxInput("licit", "Csak aktuális licites", value = F, width = NULL),
	    "(Kiszűri azokat, amelyekre nem érkezett licit)",
	   # checkboxInput("heat", "Licitáraránynál heatmap", value = T, width = NULL),
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

  output$mymap = renderLeaflet({
  dfs <- df[df$ev >= input$year[1]&df$ev <= input$year[2]&df$kikialtasi_ar>=input$ar[1]&df$kikialtasi_ar<=input$ar[2],]
  if(input$licit){
  dfs <- na.omit(dfs)
  }
  if(unique(dfs[,c(input$var)])|>length() > 100)
	{
  bins = c(0,quantile(dfs[,c(input$var)], probs = c(.2,.4,.6,.8),na.rm=T), Inf)
  }else{
      bins = c(0,1)
  }
  pal = colorBin("magma", domain = dfs[,c(input$var)], bins = bins,reverse = T)
  ################ ha kategoria
  if(input$var == "kat"){
  	# if(!input$heat){
  	if(T){
    leaflet(dfs) %>%
      addTiles() %>%
    	addCircleMarkers(lng=dfs[dfs$kat =="alatt",]$lon,lat=dfs[dfs$kat =="alatt",]$lat,group="Alatt",color = "blue",fillColor = "blue") %>%
    	addCircleMarkers(lng=dfs[dfs$kat =="felett",]$lon,lat=dfs[dfs$kat =="felett",]$lat,group="Felett",color = "red",fillColor = "red") %>%
    	addCircleMarkers(lng=dfs[dfs$kat =="megegyezik",]$lon,lat=dfs[dfs$kat =="megegyezik",]$lat,group="Megegyezik",color = "darkgreen",fillColor = "darkgreen") %>%
    	addCircleMarkers(lng=dfs[is.na(dfs$kat),]$lon,lat=dfs[is.na(dfs$kat),]$lat,group="Nincs licit",color = "black",fillColor = "black") %>%
  		addLayersControl(
  			#baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
  			overlayGroups = c("Alatt","Megegyezik","Felett","Nincs licit"),
  			options = layersControlOptions(collapsed = FALSE))
  }else{
    leaflet() %>%
      addTiles() %>%
    	addHeatmap(lng=na.omit(dfs[dfs$kat =="alatt",]$lon),lat=na.omit(dfs[dfs$kat =="alatt",]$lat),group="Alatt",intensity = 1,max=100,radius = 15,blur = 10) %>%
    	addHeatmap(lng=na.omit(dfs[dfs$kat =="felett",]$lon),lat=na.omit(dfs[dfs$kat =="felett",]$lat),group="Felett",intensity = 1,max=100,radius = 15,blur = 10) %>%
    	addHeatmap(lng=na.omit(dfs[dfs$kat =="megegyezik",]$lon),lat=na.omit(dfs[dfs$kat =="megegyezik",]$lat),group="Megegyezik",intensity = 1,max=100,radius = 15,blur = 10) %>%
    	addHeatmap(lng=na.omit(dfs[is.na(dfs$kat),]$lon),lat=na.omit(dfs[is.na(dfs$kat),]$lat),group="Nincs licit",intensity = 1,max=100,radius = 15,blur = 10) %>%
  		addLayersControl(
  			#baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
  			baseGroups = c("Alatt","Megegyezik","Felett","Nincs licit"),
  			options = layersControlOptions(collapsed = FALSE))
  }
  }else{
  if(nrow(dfs)< 400){
    leaflet(dfs) %>%
      addTiles() %>%
  		 addHeatmap(lng=dfs$lon,lat=dfs$lat,intensity=dfs[,c(input$var)],max=ifelse(input$var =="haz",100,0.8*10^9),radius=15,blur=10,group = "Heatmap")%>%
    	addCircleMarkers(lng=dfs$lon,lat=dfs$lat,group="Marker",color = ~pal(dfs[,c(input$var)]),fillColor = ~pal(dfs[,c(input$var)]),
    									 popup = paste0(dfs[,c(input$var)]))%>%
    	 addLegend(pal=pal, values = dfs[,c(input$var)], position = "bottomright",group = "Marker")%>%
    	addLayersControl(
    		baseGroups = c("Heatmap","Marker"),
    		options = layersControlOptions(collapsed = FALSE))
  }else{
    leaflet(dfs) %>%
      addTiles() %>%
  		 addHeatmap(lng=dfs$lon,lat=dfs$lat,intensity=dfs[,c(input$var)],max=ifelse(input$var =="haz",100,0.8*10^9),radius=15,blur=10,group = "Heatmap")
  }}
  })
  #településenkénti
  output$mymap2 = renderLeaflet({
  bins2 = c(0,quantile(szjamap$VALUE, probs = c(.2,.4,.6,.75,.9)), Inf)
  pal2 = colorBin("magma", domain = szjamap$VALUE, bins = bins2,reverse = T)
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
    		fillColor = ~pal2(szjamap$VALUE), fillOpacity = 1,
    		popup = paste0(szjamap$TELEP_NEV, "<br>", round(szjamap$VALUE,2)," eFt"))%>%
    	 addLegend(pal=pal2, values = szjamap$VALUE, position = "topright",title = "SZJA alap/bejegyzett lakos (EFt)",group = "Településenkénti SZJA alap")
  })

}

# running the app
shinyApp(ui,server)
