# Libraries, setting the working directory to the directory of the file
# Note: If you want to upload the script to Shiny, you should turn the setwd(...) into comment
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(BAMMtools)
library(tidyverse)
library(sf)
library(leaflet)
library(shiny)

# Reading the necessary data
vardf = read.csv("Data/FullDatasetUnpredicted")
jaras_nev = read.csv("Data/jarasnev.csv")[,1]
df = st_read("HunMap/hun_admbnda_adm2_osm_20220720.shp")
df = st_make_valid(df)

# Merging Budapest's districts into one Budapest object(Thanks, Balázs!) and sorting the shape file to be compatible with the other variables with accents
df$jaras_wBP <- ifelse(grepl("district",df$ADM2_HU),"Budapest",df$ADM2_HU)
df2 <- rbind(df[!grepl("district",df$ADM2_HU),],head(df[df$jaras_wBP == "Budapest",],n=1))
bpshape <- st_union(df[df$jaras_wBP == "Budapest",])
df2[df2$jaras_wBP == "Budapest",]$geometry <- bpshape
jarasdf = data.frame("JARAS_NEV" = jaras_nev, "jaras_wBP" = stringi::stri_trans_general(str = jaras_nev, id = "Latin-ASCII"))
jarasdf = jarasdf[order(jarasdf$jaras_wBP),]
JARAS_NEV = jarasdf$JARAS_NEV
df2 <- df2[order(df2$jaras_wBP),]
df2 = cbind(df2, JARAS_NEV)
df2 <- df2[order(df2$JARAS_NEV),]
rm(df, bpshape, jaras_nev, jarasdf, JARAS_NEV)

# setting up the getInput() function the be able to choose data in the app
getInput = function(var, year){
  inputdf = vardf %>%
    filter(EV == year)
  return(inputdf[,var])
}

# The ui to the app
ui = fluidPage(
  fluidRow(
    column(2,
           selectInput("var","Változó:", choices = c("Tízezer lakosra jutó lakásépítés" = "LAKAS",
                                                      "CSOK keresési trend" = "CSOK",
                                                      "Személyi jövedelemadóalapot képező jövedelem egy állandó lakosra" = "SZJA" ,
                                                      "Foglalkoztatottsági ráta" = "MUNKA" ,
                                                      "Beruházási teljesítményérték/lakosok száma" = "BERUHAZAS",
                                                      "X koordináta" = "X",
                                                      "Y koordináta" = "Y"), selected = "Lakas"),
           selectInput("year","Év:", choices = c(2012:2021), selected = 2018)),
    column(10,
           leafletOutput("mymap", height = 1000))
))

# The server to the app
server = function(input, output, session){

  # getting the data based on what we've set to be shown
  dataInput = reactive({
    getInput(input$var,input$year)
  })


  output$mymap = renderLeaflet({

    # Creating the bins, if all the values are 0, we create only one bin
    if(sum(dataInput(), na.rm = TRUE) == 0){
      bins = c(0,1)
    }else{
      bins = c(0,quantile(na.omit(dataInput()), probs = c(.2,.4,.6,.8)), Inf)
    }

    # Creating the colorpalette based on the bins
    pal = colorBin("YlGnBu", domain = dataInput(), bins = bins)

    # Basically, this is the map, we're showing
    leaflet(df2) %>%
      addTiles() %>%
      addPolygons(
        weight = 1,
        opacity = 1, color = "white",
        dashArray = "3",
        highlightOptions = highlightOptions(
          weight = 5, color = "grey",
          dashArray = "", fillOpacity = .7),
        fillColor = ~pal(dataInput()), fillOpacity = .7,
        popup = paste0(df2$JARAS_NEV, "<br>", round(dataInput(),2))
      ) %>%
      addLegend(pal=pal, values = dataInput(), position = "topright")
  })

}

# runnung the app
shinyApp(ui,server)
