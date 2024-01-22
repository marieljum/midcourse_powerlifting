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
                                      width = 500, height = "auto",
                                      
                                      h3(tags$b("USAPL Competitions in the US")),
                                      
                                      fluidRow(
                                        column(6, 
                                               selectInput("maptype",
                                                           "Choose a map",
                                                           choices = c("Count", "Change in count"),
                                                           selected = "Count", multiple = FALSE)
                                        )
                                      )
                                      ),
                        
                        # Panel for state info selection 
                        absolutePanel(id = "controls", fixed = TRUE, draggable = FALSE,
                                      top = 60, left = "auto", right = 10, bottom = "auto",
                                      width = 700, height = "auto",
                                      
                                      h3(tags$b("Find meets in your area:")),
                                      
                                      fluidRow(
                                        column(6, 
                                               selectInput("state",
                                                           "Choose a state",
                                                           choices = state_choices,
                                                           selected = "", multiple = FALSE)
                                        )
                                      ),
                                      tableOutput("uniquemeetTable"), 
                                      
                                      h4(tags$b("Meets Throughout Time")),
                                      
                                      plotOutput("allmeets_time")
                        )
                    )
           ), 
           
           tabPanel("Meet Results",
                    fluidRow(
                      column(4,
                             selectInput("states", 
                                         "States", 
                                         choices = state_choices, 
                                         multiple = FALSE)
                      ),
                      column(4, 
                             selectInput("type",
                                         "Meet Type",
                                         choices = type_choices,
                                         multiple = TRUE)
                      ),
                      column(4, 
                             selectInput("meet",
                                         "Select a meet",
                                         choices = "", 
                                         selected = "")
                      )
                    )
           )
)


