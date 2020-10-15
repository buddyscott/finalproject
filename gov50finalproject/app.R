
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

nbainfo <- read_csv("nbainfo2.csv", 
                    col_type = cols(team = col_character(), 
                                    "1920winpct" = col_number(), 
                                    winpctrank = col_number(), 
                                    "1920ortg" = col_number(),
                                    ortgrank = col_number(), 
                                    "1920drtg" = col_number(), 
                                    drtgrank = col_number(), 
                                    "1920nrtg" = col_number(), 
                                    nrtgrank = col_number(), 
                                    "1920pace" = col_number(), 
                                    pacerank = col_number(), 
                                    gtcontracts = col_number(), 
                                    po_ngs = col_number(), 
                                    avgage = col_number(), 
                                    medage = col_number(), 
                                    avgexp = col_number(), 
                                    medexp = col_number(), 
                                    "2021projsalary" = col_number(), 
                                    "2021projcapspace" = col_number(), 
                                    "2021projexceptions" = col_character(), 
                                    tenyrwin = col_number(), 
                                    tenyrloss = col_number(), 
                                    tenyrwinpct = col_number(), 
                                    winpctrank_10 = col_number(), 
                                    playoffpct = col_number(), 
                                    principal_owner = col_character(), 
                                    nw = col_number(), 
                                    owned_since = col_number(), 
                                    business = col_character(), 
                                    other_owners = col_character(), 
                                    valuation = col_number(), 
                                    percent_change = col_number(), 
                                    debt_to_value = col_number(), 
                                    revenue = col_number(), 
                                    operating_income = col_number())) %>%
    rename_with(~ str_replace(.x, "1920", "current")) %>% 
    rename_with(~ str_replace(.x, "2021", "future")) %>% 
    slice(1:30) %>%
    subset(select = -futureprojcapspace)

playercontracts <- read_csv("bbrefcontractdata2.csv", col_type = cols(
    playername = col_character(),
    playerid = col_character(),
    team = col_character(),
    salary1920 = col_double(),
    salary2021 = col_double(),
    salary2122 = col_double(),
    salary2223 = col_double(),
    salary2324 = col_double(),
    salary2425 = col_double(),
    signedusing = col_character(),
    guaranteed = col_double()
))

playercontracts_modified <- playercontracts %>%
    filter(salary2021 >= 20000000) %>%
    arrange(desc(salary2021))

ui <- navbarPage(
    "Buddy Ball: Understanding NBA Finances Amidst COVID-19",
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             a("Please see my GitHub repo here", 
             href = "https://github.com/buddyscott/finalproject"),
             h3("About Me"),
             p("My name is Buddy Scott and I study Economics with a secondary 
              in Government at Harvard College. 
             You can reach me at jamesscott@college.harvard.edu."), 
             a("Connect with me on LinkedIn", 
               href = "https://www.linkedin.com/in/buddyscott13/"), 
    ),
    tabPanel("Model",
             fluidPage(
                 selectInput("x", "X variable", choices = names(nbainfo)),
                 selectInput("y", "Y variable", choices = names(nbainfo)),
                 selectInput("geom", "geom", c("point", "column", "jitter")),
                 plotOutput("plot")
             )),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them"),)
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
    
    
    
    

    
    