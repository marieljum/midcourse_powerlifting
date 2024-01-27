function(input, output, session) {

    ## Interactive Map ##########################################

    output$map <- renderLeaflet({
        leaflet() |>
            addTiles() |>
            setView(lng = -68, lat = 40, zoom = 4)
    })

    observe({
        input_type <- input$maptype

        if (input_type == "Meet Count") {
            # Map for meet counts
            leafletProxy("map") |>
                clearShapes() |>
                addPolygons(
                    data = meet_counts,
                    fillColor = ~colorQuantile("Greens", meet_count)(meet_count),
                    fillOpacity = 0.7,
                    weight = 1.5,
                    label = labels
                )
        } else if (input_type == "Member Count") {
            # Map for meet count change over years
            leafletProxy("map") |>
                clearShapes() |>
                addPolygons(
                    data = membercounttotal,
                    fillColor = ~colorQuantile("Blues", total)(total),
                    fillOpacity = 0.7,
                    weight = 1.5,
                    label = labels_member
                )
        } else if (input_type == "Change in meet count") {
            # Map for meet count change over years
            leafletProxy("map") |>
                clearShapes() |>
                addPolygons(
                    data = meetcountchange,
                    fillColor = ~colorQuantile("Oranges", `Avg Change Per Year`)(`Avg Change Per Year`),
                    fillOpacity = 0.7,
                    weight = 1.5,
                    label = labels_change
                )
        } else if (input_type == "Change in membership count") {
            # Map for meet count change over years
            leafletProxy("map") |>
                clearShapes() |>
                addPolygons(
                    data = membercountchange,
                    fillColor = ~colorQuantile("Purples", `Avg Change Per Year`)(`Avg Change Per Year`),
                    fillOpacity = 0.7,
                    weight = 1.5,
                    label = labels_memberchange
                )
        }
    })

    ## Table of Populous Meets In State

    output$uniquemeetTable <- renderTable({
        selected_state <- input$state

        if (selected_state != "") {
            filtered_data <- all_competitions_bymonth_count |>
                filter(State == selected_state) |>
                select(-(State)) |>
                arrange(desc(Count))
            return(filtered_data)
        } else {
            return(NULL)
        }
    })


    ## Meet Count over time
    output$meetcount <- renderPlotly({
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
                theme_classic() +
                labs(x = "Year", y = "Number of Meets") +
                scale_x_continuous(breaks = seq(from = min_year, to = max_year, by = 2),
                                   limits = c(min_year, max_year)) +
                theme(axis.text.x = element_text(size = 8),
                      axis.text.y = element_text(size = 8))

            p <- ggplotly(gg, tooltip = "text")
        }
    })


    ## Meet Type Pie Chart
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

    ## Member Count Bar Graph
    output$membercount <- renderPlotly({

        df <- membercount |>
            filter(
                if (input$state == "") {
                    TRUE
                } else {
                    `Meet Location` == input$state
                })

            gg <- ggplot(df, aes(x = factor(Year), y = MemberCount, text = paste(
                MemberCount
            ))) + geom_col(fill = "#DF826C") +
                theme_classic() +
                labs(
                    # title = "Total Member Count Over the Years by Meet Location",
                    x = "Year",
                    y = "Member Count",
                    color = "Meet Location")

            p <- ggplotly(gg, tooltip = "text")


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

        # Filter
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

        # Reshape df and convert to lbs
        df <- df |>
            rowwise() |>
            mutate(Squat = max(0, `Squat 1`, `Squat 2`, `Squat 3`, na.rm = TRUE),
                   `Bench Press` = max(0, `Bench Press 1`, `Bench Press 2`, `Bench Press 3`, na.rm = TRUE),
                   Deadlift = max(0, `Deadlift 1`, `Deadlift 2`, `Deadlift 3`, na.rm = TRUE)) |>
            pivot_longer(names_to = "Lift", values_to = "Value", cols = c("Squat", "Bench Press", "Deadlift")) |>
            select(Name, Lift, Value, Total, `Weight Class`, Category) |>
            distinct(Name, Lift, .keep_all = TRUE) |>
            mutate(`Weight Class` = as.integer(gsub("\\+", "", `Weight Class`))) |>
            # If statement that calculates values to lbs if lbs unit is chosen
            mutate(
                Value = case_when(input$lift_units == "lbs" ~ round(Value*2.20462, 2), TRUE ~ Value),
                Total = case_when(input$lift_units == "lbs" ~ round(Total*2.20462, 2), TRUE ~ Total),
                `Weight Class` = case_when(input$lift_units == "lbs" ~ round(`Weight Class`*2.20462, 2), TRUE ~ `Weight Class`)
                )

        ## ggplot
        p <- ggplot(df, aes(x = Name, y = Value, fill = Lift, text = paste(
            Name, "<br>",
            Lift, ": ", Value, "<br>",
            "Total: ", Total, "<br>",
            "Weight Class: ", `Weight Class`, "<br>",
            "Category: ", Category
        ))) +
            geom_col() +
            labs(x = "Competitor Name", y = "Weight Lifted") +
            theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_fill_manual(values = c("Squat" = "#394867", "Bench Press" = "#4F709C", "Deadlift" = "#D8C4B6"))

        p <- ggplotly(p, tooltip = "text") |>
            layout(legend = list(x = 0.4, y = 1.3, orientation = "h"))
    })

    # ## Meet Summary Table
    # output$summarytable <- renderTable({
    #
    #     df <- usapl_meets |>
    #         filter(
    #             `Meet Location` == input$states,
    #             if (input$year == "All") TRUE else Year == input$year,
    #             if (input$type == "All") TRUE else `Meet Type` == input$type,
    #             `Meet Name` == input$meet
    #         ) |>
    #         filter(Placement == "1") |>
    #         select(Category, `Weight Class`, Name, `Squat 1`:Total) |>
    #         mutate(`Weight Class` = as.integer(gsub("\\+", "", `Weight Class`))) |>
    #         arrange(`Weight Class`)
    #
    #
    #
    # })

    ## Meet Summary/ First Place Graph
    output$firstplaceGraph <- renderPlotly({

        # Filtering the data
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
            filter(Placement == "1") |>
            select(Category, `Weight Class`, Name, `Squat 1`:Total)

        # Change to long format
        df <- df |>
            rowwise() |>
            mutate(Squat = max(0, `Squat 1`, `Squat 2`, `Squat 3`, na.rm = TRUE),
                   `Bench Press` = max(0, `Bench Press 1`, `Bench Press 2`, `Bench Press 3`, na.rm = TRUE),
                   Deadlift = max(0, `Deadlift 1`, `Deadlift 2`, `Deadlift 3`, na.rm = TRUE)) |>
            pivot_longer(names_to = "Lift", values_to = "Value", cols = c("Squat", "Bench Press", "Deadlift")) |>
            select(Name, Lift, Value, Total, `Weight Class`, Category) |>
            distinct(Name, Lift, .keep_all = TRUE) |>
            mutate(`Weight Class` = as.integer(gsub("\\+", "", `Weight Class`))) |>
                # If statement that calculates values to lbs if lbs unit is chosen
                mutate(
                    Value = case_when(input$lift_units == "lbs" ~ round(Value*2.20462, 2), TRUE ~ Value),
                    Total = case_when(input$lift_units == "lbs" ~ round(Total*2.20462, 2), TRUE ~ Total),
                    `Weight Class` = case_when(input$lift_units == "lbs" ~ round(`Weight Class`*2.20462, 2), TRUE ~ `Weight Class`)
                )

        ## ggplot
        p <- ggplot(df, aes(x = Name, y = Value, fill = Lift, text = paste(
            Name, "<br>",
            Lift, ": ", Value, "<br>",
            "Total: ", Total, "<br>",
            "Weight Class: ", `Weight Class`, "<br>",
            "Category: ", Category
        ))) +
            geom_col() +
            labs(x = "Competitor Name", y = "Weight Lifted") +
            theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_fill_manual(values = c("Squat" = "#9A3B3B", "Bench Press" = "#C08261", "Deadlift" = "#D8C4B6"))

        p <- ggplotly(p, tooltip = "text")|>
            layout(legend = list(x = 0.2, y = 1.3, orientation = "h"))

    })

    output$competitor_count <- renderValueBox({

        competitor_num <- usapl_meets |>
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
            distinct(Name) |>
            n_distinct()

        valueBox(
            value = competitor_num,
            subtitle = "Number of competitors"
            )
    })

    output$averageSquat <- renderValueBox({

        averages <- usapl_meets |>
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
            distinct(Name, .keep_all = TRUE) |>
            rowwise() |>
            mutate(max_squat = max(0, `Squat 1`, `Squat 2`, `Squat 3`, na.rm = TRUE)) |>
            mutate(
                max_squat = case_when(input$lift_units == "lbs" ~ round(max_squat*2.20462, 2), TRUE ~ max_squat))

        avg_maxsquat <- mean(averages$max_squat)

        valueBox(
            value = round(avg_maxsquat, 2),
            subtitle = "Average Max Squat"
            )
    })

    output$averageBenchpress <- renderValueBox({

        averages <- usapl_meets |>
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
            distinct(Name, .keep_all = TRUE) |>
            rowwise() |>
            mutate(max_bp = max(0, `Bench Press 1`, `Bench Press 2`, `Bench Press 3`, na.rm = TRUE)) |>
            mutate(
                max_bp = case_when(input$lift_units == "lbs" ~ round(max_bp*2.20462, 2), TRUE ~ max_bp))

        avg_max_bp <- mean(averages$max_bp)

        valueBox(
            value = round(avg_max_bp, 2),
            subtitle = "Average Max Benchpress"
        )
    })

    output$averageDeadlift <- renderValueBox({

        averages <- usapl_meets |>
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
            distinct(Name, .keep_all = TRUE) |>
            rowwise() |>
            mutate(max_dead = max(0, `Deadlift 1`, `Deadlift 2`, `Deadlift 3`, na.rm = TRUE)) |>
            mutate(
                max_dead = case_when(input$lift_units == "lbs" ~ round(max_dead*2.20462, 2), TRUE ~ max_dead))

        avg_max_dead <- mean(averages$max_dead)

        valueBox(
            value = round(avg_max_dead, 2),
            subtitle = "Average Max Deadlift"
        )
    })

    output$averageTotal <- renderValueBox({

        totals <- usapl_meets |>
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
                `Meet Name` == input$meet,
                (Placement == "1")
            ) |>
            distinct(Name, .keep_all = TRUE) |>
            mutate(
                Total = case_when(input$lift_units == "lbs" ~ round(Total*2.20462, 2), TRUE ~ Total))

        avg_total <- mean(totals$Total)

        valueBox(
            value = round(avg_total, 2),
            subtitle = "Average Total for 1st-Place Winners"
        )

    })

    output$liftBoxplot <- renderPlotly({

        boxplot <- usapl_meets |>
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
                `Meet Name` == input$meet) |>
            rowwise() |>
            mutate(Squat = max(0, `Squat 1`, `Squat 2`, `Squat 3`, na.rm = TRUE),
                   `Bench Press` = max(0, `Bench Press 1`, `Bench Press 2`, `Bench Press 3`, na.rm = TRUE),
                   Deadlift = max(0, `Deadlift 1`, `Deadlift 2`, `Deadlift 3`, na.rm = TRUE)) |>
            pivot_longer(names_to = "Lift", values_to = "Values", cols = c("Squat", "Bench Press", "Deadlift")) |>
            select(Name, Lift, Values) |>
            mutate(
                Values = case_when(input$lift_units == "lbs" ~ round(Values*2.20462, 2), TRUE ~ Values))

        p <- ggplot(boxplot, aes( x = Lift, y = Values, fill = Lift)) +
            geom_boxplot() +
            labs(x = "Lift",
                 y = "Weight") +
            theme_classic() +
            scale_fill_manual(values = c("Squat" = "#9A3B3B", "Bench Press" = "#C08261", "Deadlift" = "#D8C4B6"))

        ggplotly(p)

    })



}
