
library(shiny)
library(tidyverse)
library(shiny)
library(fec16)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(ggforce)
library(readr)


######################################################################################
######################################################################################
# 
# 1. Shiny Apps have two basic parts to them
# 
#   - The user interface (UI) defines how the app should look.
#
#     -- For example, the text on the page, the placement of the elements, etc.
#
#   - The server defines how the app should behave.
#
#     -- This is how live elements are updated - like selecting a state from a list.
#
#   - Those two pieces are combined by running shinyApp(ui, server) to create the app.
# 
#      -- You can also click the green "Run App" button on the top right or 
#         run runApp() in the console

# reading in data sets
nbainfo
playercontracts
playercontracts_modified

ui <- navbarPage(
    "Buddy Ball: Understanding NBA Finances Amidst COVID-19",
    
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("This project will attempt to analyze the potential 
             ramifications of the ongoing COVID-19 pandemic for the finances of 
             NBA teams and the market for NBA players. NBA Commissioner Adam 
             Silver revealed in May 2020 that ticket sales for in-person fan 
             attendances comprises an estimated 40% of the NBA’s revenue. The 
             NBA’s revenue is split roughly 50-50 between players and the 
             owners of these teams. If this holds true, both the players’ and 
             the owners’ share of Basketball Related Income would decrease from 
             $4 billion to $2.4 billion. If this is the case, the league might 
             have to withhold an increased percentage of players’ salaries in 
             escrow for the upcoming season to finance the league’s operations, 
             potentially leading to heavily contested debates at the bargaining
             table for an updated Collective Bargaining Agreement that has led 
             to a lockout in the past. The two main topics covered will be the 
             league’s potentially inaccurate forward guidance provided to teams 
             about salary cap levels for the upcoming season, as well as 
             potential inequalities emerging from teams’ varying levels of cash 
             flows and spending power that could potentially harm competitive 
             balance in a zero-sum league."),
             h3("About Me"),
             p("My name is Buddy Scott and I study Economics with a secondary 
              in Government at Harvard College. 
             You can reach me at jamesscott@college.harvard.edu."), 
             a("Connect with me on LinkedIn", 
               href = "https://www.linkedin.com/in/buddyscott13/"),
             br(),
             a("Please see my GitHub repo here", 
               href = "https://github.com/buddyscott/finalproject"),
             mainPanel(img(src = "nbalogo.png", height = 140, width = 400))
             ),
    
    tabPanel("Model",
             mainPanel(
                 p("This section allows you to plot two variables with each 
                   other, either as a scatterplot, bar graph, or jittered plot."),
                 br(),
                 p("1. Try plotting 'team' as the x variable, 'valuation' as 
                   the y variable, and 'column' as the geom. Notice the wide 
                   range of team values (meaning how much the team) is worth. 
                   The y axis is in billions of dollars."),
             fluidPage(
                 selectInput("x", "X variable", choices = names(nbainfo)),
                 selectInput("y", "Y variable", choices = names(nbainfo)),
                 selectInput("label", "label", c("team", "owner")),
                 selectInput("geom", "geom", c("point", "column", "jitter")),
                 plotOutput("plot")),
                 
             )
             
             ),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("This will be a discussion about what the model shows as well as 
             potential policy suggestions to mitigate this damage"))
)
    
    # Define server logic
    server <- function(input, output, session) {
        plot_geom <- reactive({
            switch(input$geom,
                   point = geom_point(),
                   column = geom_col(),
                   jitter = geom_jitter()
            )
        })

        output$plot <- renderPlot({
            ggplot(nbainfo, aes(.data[[input$x]], .data[[input$y]])) +
                plot_geom()
                
        }, res = 96)
        
        
    }
    
    shinyApp(ui, server)
    
    
    
    

    
    