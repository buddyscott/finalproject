# loading in libraries

library(shiny)
library(tidyverse)
library(fec16)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(ggforce)
library(readr)

# reading in data sets

nbainfo <- read_csv("raw_data/nbainfo2.csv", 
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
    
    # This makes the variables names easier to work with if they do not start
    # with numbers.
    
    rename_with(~ str_replace(.x, "1920", "current")) %>% 
    rename_with(~ str_replace(.x, "2021", "future")) %>%
    slice(1:30) %>%
    
    # This column did not carry over well and I do not anticipate using it 
    # so I am just excluding it.
    
    subset(select = -futureprojcapspace)

playercontracts <- read_csv("raw_data/bbrefcontractdata2.csv", col_type = cols(
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
    guaranteed = col_double()))


ui <- navbarPage(
    "Buddy Ball: Understanding NBA Finances Amidst COVID-19",
    
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("This project will analyze the potential ramifications of the 
             ongoing COVID-19 pandemic on the finances of National Basketball
             Association (NBA) teams and the market for NBA players. NBA 
             Commissioner Adam Silver revealed in May 2020 that ticket sales for 
             in-person fan attendance comprises an estimated 40% of the NBA’s 
             revenue. In a doomdsay scenario that would have no fans in the 
             arenas for next season, both the players’ and the owners’ share of 
             Basketball Related Income would decrease from $4 billion to $2.4 
             billion according to Silver's projection. If this is the case, 
             the league might have to withhold an 
             increased percentage of players’ salaries in escrow for the 
             upcoming season to finance the league’s operations, potentially 
             leading to heavily contested debates at the bargaining
             table for an updated Collective Bargaining Agreement that has led 
             to lockouts in the past. The two main topics covered will be the 
             league’s potentially inaccurate forward guidance provided to teams 
             about salary cap levels for the upcoming season, as well as 
             potential inequalities emerging from teams’ varying levels of cash 
             flows and spending power that could potentially harm competitive 
             balance in a zero-sum league."),
             h3("Data Sources"),
             p("Most of the data used is from a massive spreadsheet that I
               update that has NBA rosters, player contracts, and info
               about each team. The two external data sources I pulled were
               salary data from Basketball Reference to complement the salary 
               data that I maintain, as well as information from Forbes 
               about the wealth of the majority owners of these 30 NBA teams."),
             h3("About Me"),
             p("My name is Buddy Scott and I study Economics with a secondary 
              in Government at Harvard College. 
             You can reach me at jamesscott@college.harvard.edu."), 
             a("Connect with me on LinkedIn", 
               href = "https://www.linkedin.com/in/buddyscott13/"),
             br(),
             a("Please see my GitHub repo here", 
               href = "https://github.com/buddyscott/finalproject"),
             br(), 
             a("Please see my NBA spreadsheet work here", 
               href = "https://tinyurl.com/buddyscottnba")
             ),
    
    tabPanel("NBA Team Info",
             mainPanel(
                 p("This section allows you to plot two variables with each 
                   other, either as a scatterplot, bar graph, 
                   or jittered plot."),
                 br(),
                 p("1. Try plotting 'team' as the x variable, 'valuation' as 
                   the y variable (you have to scroll down a bit), and 'column'
                   as the geom. Notice the wide range of team values (meaning 
                   how much the team) is worth. The y axis is in billions of 
                   dollars."),
                 p("2. Now try plotting 'tenyrwinpct' as the x variable, 
                   'revenue' as the y variable (again, you have to scroll down
                   to the bottom), and 'point' as the geom. This shows the
                   revenue of the team (as of February 2020) versus the
                   team's winning percentage over the past ten years. There
                   seems to be a relatively positive correlation between the 
                   two, and the correlation would probably be even more positive 
                   if not for the two richest teams - the Knicks and the 
                   Lakers - having two of the worst winning percentages over 
                   the past ten years."),
                 
             # This allows for the user to select their own variables of choice
             # as opposed to pre-determined ggplots that they have to look at.
             # I decided to use geom_point(), geom_col(), and geom_jitter(),
             # but I might change these selections moving forward.
             
             fluidPage(
                 selectInput("x", "X variable", choices = names(nbainfo)),
                 selectInput("y", "Y variable", choices = names(nbainfo)),
                 selectInput("geom", "geom", c("point", "column", "jitter")),
                 plotOutput("plot"))
             )
             ),
    
    tabPanel("Player Salary Info",
            h2("NBA Salary Data"), 
            p("Type your favorite player's name into the search bar to see 
              their current contract."),
            
            # This gives the data set playercontracts (read in earlier) as a
            # somewhat interactive table - interactive in the sense that 
            # a user can search a specific player and see their salary. This 
            # will eventually be updated to be a more detailed analysis that 
            # will give a lot more information than just the player and his
            # contract. 
            
            DT::dataTableOutput("playercontracts")
            ),
    
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("This will be a discussion about what the model shows as well as 
             potential policy suggestions to mitigate this potential damage."))
)

    
    # Define server logic
    server <- function(input, output, session) {
        
        # This corresponds to the fluidPage() code above that allows the user 
        # to make the plot that they so choose. The syntax here was pretty
        # straight forward.
        
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
        
        output$playercontracts = DT::renderDataTable({
            playercontracts
        })
        
    }
    
    shinyApp(ui, server)
    
    
    

    
    