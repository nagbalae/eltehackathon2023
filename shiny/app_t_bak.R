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
# Reading the necessary data
df |> filter(ev == 2010)

# Merging Budapest's districts into one Budapest object(Thanks, Balázs!) and sorting the shape file to be compatible with the other variables with accents

# setting up the getInput() function the be able to choose data in the app
# getInput = function(var,ymin,ymax){
#   inputdf <- df[df$ev >= ymin&df$ev <= ymax,]
#   return(inputdf[,var])
# }

# The ui to the app
ui = fluidPage(
  fluidRow(
    column(3,
           selectInput("var","Változó:", choices = c("Kikiáltási ár" = "kikialtasi_ar",
                                                      "Aktuális licit" = "aktualis_licit",
                                                      "Minimál ár" = "minimal_ar"),
                                                      selected = "kikialtasi_ar"),
           # selectInput("year","Év:", choices = c(1995:2020), selected = 2018)),
				    sliderInput("year","Évek:",min = 1995, max = 2023, value = c(1995, 2023))),
				    # sliderInput("jov",":",min = 1995, max = 2023, value = c(1995, 2023))),
    column(10,
           leafletOutput("mymap", height = 1000))
))

# The server to the app
server = function(input, output, session){

  # getting the data based on what we've set to be shown
  # dataInput = reactive({
  #   getInput(input$var,input$year[1],input$year[2])
  # })

  output$mymap = renderLeaflet({
	print(input)
    # Basically, this is the map, we're showing
    leaflet(df[df$ev >= input$year[1]&df$ev <= input$year[2],]) %>%
      addTiles() %>%
  		addHeatmap(lng=~lon,lat=~lat,intensity=1,max=100,radius=15,blur=10)
  })

}

# running the app
shinyApp(ui,server)
