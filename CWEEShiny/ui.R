library(shinydashboard)
library(shiny)
library(rCharts)

# Define UI for application that draws a histogram
dashboardPage(skin = "blue",
              dashboardHeader(
                title = "California Water Challenge",
                dropdownMenuOutput("messageMenu"),
                titleWidth = 320
              ),
              dashboardSidebar(
                width = 320,
                sidebarMenu(
                  menuItem(
                    "Undergroud Water", tabName="Underwater"
                  ),
                  menuItem(
                    "Crop", tabName="crop"
                  ),
                  menuItem(
                    "Info", tabName="info"
                  )
                )
              ),
              dashboardBody(
                {tabItem(
                  tabName = "Underwater",
                  fluidRow(
                    box(
                      title = "Input", 
                      status = "primary",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      width=6,
                      selectInput("WaterSelect", label = h4("Prediction Year"), 
                                  choices = list("2011" = 2011, "2012" = 2012,
                                                 "2013" = 2013,"2014" = 2014,
                                                 "2015" = 2015,"2016" = 2016,
                                                 "2017" = 2017), selected = 2011),
                      radioButtons("WaterRadio", label = h4("Prediction Season"),
                                 choices = list("Spring" = 1, "Fall" = 2),selected = 1),
                      actionButton("WaterButton","See Output!")
                    ),
                    box(
                      title = "Model Summary", 
                      status = "primary",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      width=6,
                      helpText(h4("We use Spatio-Temporal Bayesian Model on the WESL data of San 
                               Joaquin valley. We try to predict the WESL at any location and any 
                               year.Now, you can choose the year from 2011 to 2017 and season from 
                               fall and spring."))
                    )
                  ),
                  fluidRow(
                    box(
                      title = "Prediction on Map", 
                      status = "primary",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      width=12,
                      showOutput("WaterPredictPlot", "nvd3")
                  )
                  )
                )}
              )
)