navbarPage(tags$b("USA Powerlifting"), id = "nav",

           tabPanel("Interactive Map",
                    div(class = "outer",

                        tags$head(
                          includeCSS("styles.css")
                        ),

                        leafletOutput("map", width="100%", height="100%"),

                        # Panel for map selection
                        absolutePanel(id = "controls", fixed = TRUE, draggable = FALSE,
                                      top = 60, left = 190, right = "auto", bottom = "auto",
                                      width = 450, height = "auto",

                                      h3(tags$b("USAPL Competitions in the US")),

                                      fluidRow(
                                        column(6,
                                               selectInput("maptype",
                                                           "Choose a map",
                                                           choices = c("Member Count", "Meet Count",
                                                                       "Change in membership count", "Change in meet count"),
                                                           selected = "Count", multiple = FALSE)
                                        )
                                      )
                                      ),

                        # Panel for state info selection
                        absolutePanel(id = "controls", fixed = TRUE, draggable = FALSE,
                                      top = 60, left = "auto", right = 10, bottom = "auto",
                                      width = 750, height = "auto",

                                      h3(tags$b("Meets Throughout Time")),

                                      h4(tags$b("Find meets in your area:")),
                                      fluidRow(
                                        column(6,
                                               selectInput("state",
                                                           "Choose a state",
                                                           choices = state_choices,
                                                           selected = "", multiple = FALSE)
                                        )
                                      ),
                                      card(
                                        height = 280,
                                        full_screen = FALSE,
                                        card_body(
                                          layout_column_wrap(
                                            width = 1/2,
                                            div(
                                              style = "height: 280px; overflow-y: auto;",
                                              tableOutput("uniquemeetTable")
                                            ),
                                            # tableOutput("uniquemeetTable"),
                                            plotlyOutput("meetcount")
                                          )
                                        )

                                      ),

                                      br(),


                                      card(
                                        height = 330,
                                        full_screen = TRUE,
                                        card_body(
                                          layout_column_wrap(
                                            width = 1/2,
                                            plotlyOutput("membercount"),
                                            highchartOutput("allmeets_type")

                                          )
                                        )
                                      )
                        )
                    )
           ),

           tabPanel("Meet Results",

                    fluidRow(
                      column(3,
                             selectInput("states",
                                         "Select a state",
                                         choices = state_choices,
                                         multiple = FALSE)
                      ),
                      column(3,
                             selectInput("year",
                                         "Select a year",
                                         choices = c("All", year_choices),
                                         selected = "All",
                                         multiple = FALSE)
                      ),
                      column(3,
                             radioButtons("type",
                                         "Meet Type",
                                         choices = c("All", type_choices),
                                         inline = TRUE,
                                         selected = "All")
                      ),
                      column(3,
                             selectizeInput("meet",
                                         "Select a meet",
                                         choices = "",
                                         selected = "")
                      )
                    ),
                    absolutePanel(fixed = TRUE, draggable = FALSE,
                                  top = 160, left = "auto", right = 40, bottom = "auto",
                                  width = 100, height = "auto",

                                  radioButtons("lift_units",
                                               "Select units",
                                               choices = c("kg", "lbs"),
                                               selected = "kg",
                                               inline = TRUE)
                    ),
                    navset_pill(
                      nav_panel("Summary",
                                fluidRow(
                                  br(),
                                  column(6,
                                         h4(tags$b("First place competitors"))
                                  ),
                                  column(6,
                                         h4(tags$b("Meet Summary"))
                                  )
                                ),
                                br(),
                                # h4(tags$b("First place competitors")),
                                fluidRow(
                                  column(6,
                                         plotlyOutput("firstplaceGraph")),
                                  column(2,
                                    card(
                                      br(),
                                      valueBoxOutput("competitor_count", width = 200),
                                      valueBoxOutput("averageSquat", width = 200),
                                      valueBoxOutput("averageBenchpress", width = 200),
                                      valueBoxOutput("averageDeadlift", width = 200),
                                      valueBoxOutput("averageTotal", width = 200)
                                    )
                                  ),
                                  column(4,
                                         plotlyOutput("liftBoxplot", width = "500px")
                                  )
                                )
                                ),


                      nav_panel("Graph",

                                h4(tags$b("All competitors")),
                                plotlyOutput("usaplmeetsGraph")
                                ),

                      nav_panel("Table", tableOutput("usaplmeetsTable"))


           )
           )

)


