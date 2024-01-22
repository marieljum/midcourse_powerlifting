function(input, output, session) {
    
    ## Interactive Map ##########################################
    
    output$map <- renderLeaflet({
        leaflet() |> 
            addTiles() |> 
            setView(lng = -71, lat = 40, zoom = 4)
    })
    
    observe({
        input_type <- input$maptype

        if (input_type == "Count") {
            # Map for meet counts
            leafletProxy("map") |>
                clearShapes() |>
                addPolygons(
                    data = meet_counts,
                    fillColor = ~colorQuantile("YlOrRd", meet_count)(meet_count),
                    fillOpacity = 0.7,
                    weight = 1.5,
                    label = labels
                )
        } else if (input_type == "Change in count") {
            # Map for meet count change over years
            leafletProxy("map") |>
                clearShapes() |>
                addPolygons(
                    data = meet_counts_change,
                    fillColor = ~colorQuantile("Blues", meet_count)(meet_count),
                    fillOpacity = 0.5,
                    weight = 1.5,
                    label = labels_change
                )
        }
    })
    
    output$uniquemeetTable <- renderTable({
        selected_state <- input$state
        
        if (selected_state != "") {
            filtered_data <- all_competitions_by_month |> 
                filter(State == selected_state)
            return(filtered_data)
        } else {
            return(NULL)
        }
    })
    
    output$allmeets_time <- renderPlot({
        selected_state <- input$state
        
        if (selected_state != "") {
            filtered_data <- meet_counts_by_year |> 
                filter(MetLctn == selected_state)
            
            min_year <- min(filtered_data$Year)
            max_year <- max(filtered_data$Year)
            max_meet_count <- max(filtered_data$meet_count)
            
            ggplot(filtered_data, aes(x = Year, y = meet_count)) + 
                geom_line() + geom_point(color = "orange", size = 1.5)+ theme_minimal() + 
                labs(x = "Year", y = "Number of Meets") + 
                scale_x_continuous(breaks = seq(from = min_year, to = max_year, by = 2), limits = c(min_year, max_year))
        }
    })

    
}
