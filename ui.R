#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage("Navbar!", 
       tags$head(includeCSS("styles.css")),
       tabPanel("Carte",
                div(class="mainContainer",
                    
                    leafletOutput("mymap", width="100%", height="100%"),
                    absolutePanel(id = "controls", class = "panel panel-default",
                                  top = 80, left = 80, width = 300, fixed=TRUE,
                                  draggable = TRUE, height = "auto",
                                  sliderTextInput("plot_event",
                                                  label = h5("Choix du tournoi"),
                                                  choices = majorName[7:length(majorName)],
                                                  selected = majorName[7],
                                                  grid = FALSE,
                                                  animate=animationOptions(interval = 3000, loop = FALSE))
                    )
                )
                
       ),
       tabPanel("Histogramme",
                div(class="mainContainer",
                    plotlyOutput("firstHistoPlot", width="100%", height="50%"),
                    plotlyOutput("secondHistoPlot", width="100%", height="50%")
                )
       ),
       tabPanel("Kill-Difference",
                div(class="mainContainer",
                    plotlyOutput("kd_plot", width="100%", height="100%")   
                )
       ),
       tabPanel("Joueur",
                div(class="mainContainer",
                    div(class="recherche",
                        tags$head(includeCSS("styles.css")),
                        textInput("player",NULL,placeholder = "Player"),
                        textInput("team",NULL,placeholder ="Team"),
                        selectInput(
                          "event",
                          NULL,
                          majorName[7:length(majorName)]),
                        actionButton("submitButton", "Valider")
                    ),
                    verbatimTextOutput("ruban"),
                    div(id="stats",
                        div(id="kills",
                            class="rank",
                            textOutput("kills_value"),
                            textOutput("kills_rank"),
                        ),
                        div(id="deaths",
                            class="rank",
                            textOutput("deaths_value"),
                            textOutput("deaths_rank"),
                        ),
                        div(id="kd",
                            class="rank",
                            textOutput("kd_value"),
                            textOutput("kd_rank"),
                        ),
                        div(id="ratio",
                            class="rank",
                            textOutput("ratio_value"),
                            textOutput("ratio_rank"),
                        ),
                        div(id="hs",
                            class="rank",
                            textOutput("hs_value"),
                            textOutput("hs_rank"),
                        ),
                        div(id="assists",
                            class="rank",
                            textOutput("assists_value"),
                            textOutput("assists_rank"),
                        ),
                        div(id="fassists",
                            class="rank",
                            textOutput("fassists_value"),
                            textOutput("fassists_rank"),
                        ),
                        div(id="adr",
                            class="rank",
                            textOutput("adr_value"),
                            textOutput("adr_rank"),
                        ),
                        div(id="rating",
                            class="rank",
                            textOutput("rating_value"),
                            textOutput("rating_rank"),
                        ),
                        div(id="kast",
                            class="rank",
                            textOutput("kast_value"),
                            textOutput("kast_rank"),
                        )
                    ),
                    div(class="boxplots_container",
                        plotlyOutput("team_plot", width="50%", height="100%"),
                        plotlyOutput("rating_plot_player", width="50%", height="100%")
                    ),
                )
       )
)

