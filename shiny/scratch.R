  output$mymap = renderLeaflet({
    # Basically, this is the map, we're showing
  dfs <- df[df$ev >= input$year[1]&df$ev <= input$year[2]&df$kikialtasi_ar>=input$ar[1]&df$kikialtasi_ar<=input$ar[2],]
    leaflet(szjamap) %>%
      addTiles() %>%
  		 addHeatmap(lng=dfs$lon,lat=dfs$lat,intensity=1,max=100,radius=15,blur=10,group = "Heatmap")%>%
    	addPolygons(
    		group = "Településenkénti SZJA alap",
    		weight = 1,
    		opacity = 1, color = "white",
    		dashArray = "3",
    		highlightOptions = highlightOptions(
    			weight = 5, color = "grey",
    			dashArray = "", fillOpacity = .7),
    		fillColor = ~pal(szjamap$VALUE), fillOpacity = 1,
    		popup = paste0(szjamap$JARAS_NEV, "<br>", round(szjamap$VALUE,2)))%>%
    	addLayersControl(
    		#baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
    		#overlayGroups = c("Településenkénti SZJA alap", "Heatmap"),
    		baseGroups = c("Heatmap","Településenkénti SZJA alap"),
    		options = layersControlOptions(collapsed = FALSE)
    	)%>%
    	 addLegend(pal=pal, values = szjamap$VALUE, position = "topright",group = "Településenkénti SZJA alap")
  })
