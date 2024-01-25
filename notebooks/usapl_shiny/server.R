function(input, output, session) {
    
    ## Interactive Map ##########################################
    
    output$map <- renderLeaflet({
        leaflet() |>
            addTiles() |>
            setView(lng = -68, lat = 40, zoom = 4)
    })

    # observe({
    #     input_type <- input$maptype
    # 
    #     if (input_type == "Count") {
    #         # Map for meet counts
    #         leafletProxy("map") |>
    #             clearShapes() |>
    #             addPolygons(
    #                 data = meet_counts,
    #                 fillColor = ~colorQuantile("Greens", meet_count)(meet_count),
    #                 fillOpacity = 0.7,
    #                 weight = 1.5,
    #                 label = labels
    #             )
    #     } else if (input_type == "Change in count") {
    #         # Map for meet count change over years
    #         leafletProxy("map") |>
    #             clearShapes() |>
    #             addPolygons(
    #                 data = all_competitions_change_thrutime,
    #                 fillColor = ~colorQuantile("Oranges", `Avg Growth Per Year`)(`Avg Growth Per Year`),
    #                 fillOpacity = 0.7,
    #                 weight = 1.5,
    #                 label = labels_change
    #             )
    #     }
    # })
    
    ## Table of Populous Meets In State
    
    output$uniquemeetTable <- renderTable({
        selected_state <- input$state
        
        if (selected_state != "") {
            filtered_data <- all_competitions_bymonth_count |> 
                filter(State == selected_state)
            return(filtered_data)
        } else {
            return(NULL)
        }
    })
    
    ## 
    
    output$allmeets_time <- renderPlotly({
        selected_state <- input$state
        
        if (selected_state != "") {
            filtered_data <- meet_counts_by_year |> 
                filter(MetLctn == selected_state)
            
            min_year <- min(filtered_data$Year)
            max_year <- max(filtered_data$Year)
            max_meet_count <- max(filtered_data$Count)
            
            gg <- ggplot(filtered_data, aes(x = Year, y = Count)) + 
                geom_line(color = "#596FB7", size = 0.8) + 
                geom_point(color = "#11235A", size = 1.5) + 
                theme_minimal() + 
                labs(x = "Year", y = "Number of Meets") + 
                scale_x_continuous(breaks = seq(from = min_year, to = max_year, by = 2), 
                                   limits = c(min_year, max_year)) + 
                theme(axis.text.x = element_text(size = 8),  
                      axis.text.y = element_text(size = 8))
            
            p <- ggplotly(gg)
            
            # p <- ggplot(df, aes(x = Name, y = Value, fill = Lift, 
            #                     text = paste("Competitor: ", Name, "<br>",
            #                                  Lift, ": ", Value, "<br>Total: ", Total))) + 
            #     geom_col() + 
            #     theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
            #     scale_fill_manual(values = c("Squat" = "#213555", "Bench Press" = "#4F709C", "Deadlift" = "#D8C4B6"))
            # 
            # p <- ggplotly(p, tooltip = "text")
            
        }
    })
    
    output$allmeets_type <- renderHighchart({
        selected_state <- input$state

        if (selected_state != "") {
            filtered_data <- all_competitions_bytype |>
                filter(MetLctn == selected_state)

            hchart(
                filtered_data,
                "pie", hcaes(x = MeetTyp, y = Count)
            ) |>
            hc_tooltip(
                pointFormat = "<b>Count</b>: {point.y} meets ({point.percentage:.1f}%)")
        }
    })

    
    ## Meet Results Tab ##########################################
    
    ## Meet Results Table
    output$usaplmeetsTable <- renderTable({

        df <- usapl_meets |>
            filter(
                `Meet Location` == input$states, 
                if (input$year == "All") {
                    TRUE
                } else {
                    Year == input$year
                },
                if (input$type == "All") {
                    TRUE
                } else {
                    `Meet Type` == input$type
                },
                `Meet Name` == input$meet
            ) |> 
            select(-c(`Date Format`, `Meet Location`, `Meet Name`, Team, Lot, Year, State)) |> 
            mutate(`Year of Birth` = as.character(`Year of Birth`))

    })
    

    ## Update the list of choices for meets in Meet Results Table
    observe({
        
        choices <- usapl_meets |>
            filter(
                `Meet Location` == input$states, 
                if (input$year == "All") {
                    TRUE
                } else {
                    Year == input$year
                },
                if (input$type == "All") {
                    TRUE
                } else {
                    `Meet Type` == input$type
                }
                ) |> 
            pull(`Meet Name`) |>
            unique() |>
            sort()
        
        if (length(choices) == 0) {
            choices <- c("No results")
        }
        
        updateSelectInput(session, "meet", choices = c("", choices))
    })
    
    ## Meet Results Graph
    
    output$usaplmeetsGraph <- renderPlotly({
        
        df <- usapl_meets |>
            filter(
                `Meet Location` == input$states, 
                if (input$year == "All") {
                    TRUE
                } else {
                    Year == input$year
                },
                if (input$type == "All") {
                    TRUE
                } else {
                    `Meet Type` == input$type
                },
                `Meet Name` == input$meet
            ) |> 
            select(-(`Date Format`)) |> 
            mutate(`Year of Birth` = as.character(`Year of Birth`))
        
        df <- df |> 
            rowwise() |> 
            mutate(Name, Squat = max(0, `Squat 1`, `Squat 2`, `Squat 3`, na.rm = TRUE), 
                   `Bench Press` = max(0, `Bench Press 1`, `Bench Press 2`, `Bench Press 3`, na.rm = TRUE),
                   Deadlift = max(0, `Deadlift 1`, `Deadlift 2`, `Deadlift 3`, na.rm = TRUE)) |> 
            pivot_longer(names_to = "Lift", values_to = "Value", cols = c("Squat", "Bench Press", "Deadlift")) |> 
            select(Name, Lift, Value) |> 
            ungroup() |> 
            distinct(Name, Lift, .keep_all = TRUE)
        
        # Calculate Total for Tooltip 
        df_totals <- df |> 
            group_by(Name) |> 
            summarise(Total = sum(Value))
        
        df <- left_join(df, df_totals, by = "Name")
            

        p <- ggplot(df, aes(x = Name, y = Value, fill = Lift, 
                            text = paste("Competitor: ", Name, "<br>",
                                         Lift, ": ", Value, "<br>Total: ", Total))) + 
            geom_col() + 
            theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
            scale_fill_manual(values = c("Squat" = "#394867", "Bench Press" = "#4F709C", "Deadlift" = "#D8C4B6"))
        
        p <- ggplotly(p, tooltip = "text")
    })
    

}
