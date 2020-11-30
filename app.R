# loading in libraries

library(plyr)
library(tidyverse)
library(ggplot2)
library(ggforce)
library(ggthemes)
library(readr)
library(readxl)
library(tibble)
library(rvest)
library(XML)
library(RCurl)
library(rlist)
library(ggrepel)
library(shiny)
library(plotly)

# reading in data sets


nbainfo <- read_csv("data-files/nbainfo.csv", 
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
    rename_with(~ str_replace(.x, "1920", "lastseason")) %>% 
    rename_with(~ str_replace(.x, "2021", "nextseason")) %>% 
    slice(1:30) %>%
    subset(select = -nextseasonprojcapspace) %>%
    select(team, lastseasonwinpct, lastseasonortg, lastseasondrtg, 
           lastseasonnrtg, lastseasonpace, gtcontracts, avgage, medage, avgexp,
           medexp, nextseasonprojsalary, nextseasonprojexceptions, 
           tenyrwinpct, playoffpct, principal_owner, nw, valuation, 
           percent_change, debt_to_value, revenue, operating_income)

playercontracts <- read_csv("data-files/bbrefcontractdata.csv", col_type = cols(
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
    guaranteed = col_double())) %>%
    filter(!is.na(salary2021)) %>%
    subset(select = -c(salary1920, guaranteed)) %>%
    mutate(pctsalary2021 = salary2021 / 109140000)

forbes1 <- read_csv("data-files/forbes2020.csv", 
                    col_type = cols(.default = col_character())) %>%
    mutate(rank = str_sub(rank, start = 2)) %>%
    mutate(valuation = substr(valuation, 2, nchar(valuation)-1)) %>%
    mutate(value_change = gsub('.{1}$', '', value_change)) %>%
    mutate(debt_to_value = gsub('.{1}$', '', debt_to_value)) %>%
    mutate(revenue = substr(revenue, 2, nchar(revenue)-1)) %>%
    mutate(operating_income = substr(operating_income, 2, 
                                     nchar(operating_income)-1)) %>%
    mutate(rank = as.numeric(rank)) %>%
    mutate(valuation = as.numeric(valuation)) %>%
    mutate(value_change = as.numeric(value_change)) %>%
    mutate(debt_to_value = as.numeric(debt_to_value)) %>%
    mutate(revenue = as.numeric(revenue)) %>%
    mutate(operating_income = as.numeric(operating_income)) %>%
    subset(select = -rank)

forbes2 <- read_csv("data-files/moreforbes.csv", 
                    col_type = cols(.default = col_double(), 
                                    team = col_character()))

forbes_joined <- inner_join(forbes1, forbes2, by = "team")

full_dataset <- inner_join(forbes_joined, nbainfo, by = "team") %>%
    subset(select = -c(valuation.y, debt_to_value.y, revenue.y, 
                       operating_income.y, percent_change)) %>%
    mutate(valuation = valuation.x) %>%
    mutate(debt_to_value = debt_to_value.x) %>%
    mutate(revenue = revenue.x) %>%
    mutate(operating_income = operating_income.x) %>%
    subset(select = -c(valuation.x, debt_to_value.x, revenue.x, 
                       operating_income.x)) %>%
    mutate(valuation = valuation*1000) %>%
    mutate(sport_pct = sport/valuation) %>%
    mutate(market_pct = market/valuation) %>%
    mutate(stadium_pct = stadium/valuation) %>%
    mutate(brand_pct = brand/valuation) %>%
    select(team:lastseasonpace, nw:brand_pct)

pivoted_dataset <- full_dataset %>%
    select(team, valuation, sport, market, stadium, brand) %>%
    pivot_longer(sport:brand, names_to = "aspect", values_to = "values") %>%
    arrange(desc(valuation))

ui <- navbarPage(
    "Buddy Ball: Understanding NBA Finances Amidst COVID-19",
    
    tabPanel("Introduction", 
             h3("Project Background and Motivations"),
             p("This project will analyze the potential ramifications of the 
             ongoing COVID-19 pandemic on the finances of National Basketball
             Association (NBA) teams. NBA Commissioner Adam Silver revealed in 
             May 2020 that ticket sales for in-person fan attendance comprises 
             an estimated 40% of the NBA’s revenue. In a doomdsay scenario that 
             would have no fans in the arenas for the 2020-2021, both the 
             players’ and the owners’ share of Basketball Related Income would 
             decrease from roughly $4 billion to $2.4 billion according to 
             Silver's projection. If this is the case, the league will have to 
             rethink its entire business model. The commisioner has already 
             discussed the possibility of the league turning to sports betting, 
             serving hard alcohol at games, and casinos (long thought of by the 
             league as sinful endeavours) as a way to get cash into the hands 
             of teams. This project will take a deep dive of the most recent 
             update on each team's finances to better understand the situations 
             and constraints each individual team faces in the COVID-19 world."),
             br(), 
             p("This is a video of one of the greatest buzzer-beater shots in 
               NBA history. This analysis is being done to better understand
               the new normal we are in that involves limited to no fans in 
               arenas, but hopefully we will get back to jubilant moments like 
               this one soon."),
             HTML('<iframe width="1000" height="500" 
                  src="https://www.youtube.com/embed/a-M3x-eZpV8" 
                  frameborder="0" allow="accelerometer; autoplay; 
                  clipboard-write; encrypted-media; gyroscope; 
                  picture-in-picture" allowfullscreen></iframe>'),
             ),
    
    tabPanel("Methodology",
             h3("Methodology"),
             p("Write about methodology."),
             br(),
             br(),
             h3("Explanation of Variables in Dataset"),
             p("Explanation..."),
             ),
    
    tabPanel("Interactive Plot",
             mainPanel(
                 p("This section allows you to plot two variables with each 
                   other, either as a scatterplot or bar graph."),
                 br(),
             
             fluidPage(
                 selectInput("x", "X variable", choices = names(full_dataset)),
                 selectInput("y", "Y variable", choices = names(full_dataset)),
                 selectInput("geom", "geom", c("point", "column")),
                 plotOutput("plot1")),
             
             )),
    
    tabPanel("Plots",
    
             p("This is a plot of team valuations."),
             plotOutput("plot2"), 
             
             p("This is a plot of the breakdown of the four components 
               of team valuations."),
             plotOutput("plot3"),
             
             p("This is a scatterplot of the metro area population of a 
               franchise versus the team's valuation by Forbes in Feb 2020."),
             plotOutput("plot4"), 
             
             p("This is a scatterplot of the year a team was purchased 
               compared to the price paid by the buyer of the team."),
             plotOutput("plot5"),
             
             p("This is a scatterplot of each team's winning percentage in 
               the most recent NBA season compared to a team's valuation by 
               Forbes in Feb 2020."),
             plotOutput("plot6")
             
             ),
    
    tabPanel("Player Salary Info",
            h2("NBA Player Salary Data"), 
            p("Type your favorite player's name into the search bar to see 
              their current contract."),
            
            # This gives the data set playercontracts (read in earlier) as a
            # somewhat interactive table - interactive in the sense that 
            # a user can search a specific player and see their salary. This 
            # will eventually be updated to be a more detailed analysis that 
            # will give a lot more information than just the player and his
            # contract. 
            
            DT::dataTableOutput("playercontracts"),
            ),
    
    tabPanel("Model",
             p("This will be a regression model"),
            ),
    
    tabPanel("Discussion",
             p("This will be a discussion about what the model shows as well as 
             potential policy suggestions to mitigate this potential damage."),
             ),
    
    tabPanel("About",
             h3("Data Sources"),
             p("Most of this data comes from the Forbes February 2020 NBA Team
               Valuations publication. Other data comes from spreadsheets that I
               maintain in my free time (the link to those spreadsheets is at 
               the bottom of the page."),
             h3("About Me"),
             p("My name is Buddy Scott and I concentrate in Economics with a 
               secondary in Government at Harvard College. You can reach me at 
               jamesscott@college.harvard.edu."), 
             a("Connect with me on LinkedIn", 
               href = "https://www.linkedin.com/in/buddyscott13/"),
             br(),
             a("Please see my GitHub repo here", 
               href = "https://github.com/buddyscott/nba-team-business-models"),
             br(), 
             a("Please see my NBA spreadsheet work here", 
               href = "https://hu-my.sharepoint.com/:x:/g/personal/jamesscott_college_harvard_edu/Ees1sxrxTG1AobHhy3Z_SEEBxzYTcnAFO1zm5XM22L-JGQ?e=gfcWXM")
    ))


    
    # Define server logic
    server <- function(input, output, session) {
        
        # This corresponds to the fluidPage() code above that allows the user 
        # to make the plot that they so choose. The syntax here was pretty
        # straight forward.
        
        plot_geom <- reactive({
            switch(input$geom,
                   point = geom_point(),
                   column = geom_col()
            )
        })

        output$plot1 <- renderPlot({
            ggplot(full_dataset, aes(.data[[input$x]], .data[[input$y]])) +
                plot_geom() + theme_bw() + geom_smooth(formula = y ~ x)
                
        }, res = 96)
        
        output$playercontracts = DT::renderDataTable({
            playercontracts
        })
        
        output$plot3 <- 
            renderPlot({
                pivoted_dataset %>%
                    ggplot(aes(x = aspect, y = values)) + 
                    geom_col() + 
                    facet_wrap(~ team, ncol = 10) + 
                    theme(strip.text = element_text(size = 6), 
                          axis.text = element_text(size = 4), 
                          panel.grid = element_blank(), 
                          panel.spacing.x = unit(3, "mm")) + 
                    labs(title = "Team Valuation Breakdown", 
                         x = "Valuation Breakdown", 
                         y = "Dollars") +
                    scale_x_discrete(breaks = c("brand", "market", 
                                                "sport", "stadium"), 
                                     labels = c("Brand", "Market", 
                                                "Sport", "Stadium")) + 
                    scale_y_continuous(breaks = c(500, 1000, 1500, 2000), 
                                       labels = c("$500M", "$1B", "$1.5B", 
                                                  "$2B")) + 
                    theme_classic()
            })
        
        output$plot2 <- 
            renderPlot({
                full_dataset %>%
                    ggplot(aes(x = fct_reorder(team, valuation), y = valuation)) + 
                    geom_col() + 
                    scale_y_continuous(breaks = c(1000, 2000, 3000, 4000, 5000), 
                                       labels = c("$1B", "$2B", "$3B", "$4B", 
                                                  "$5B")) + 
                    theme(axis.text = element_text(size = 8)) +
                    labs(title = "Team Valuations", 
                         x = "Team", y = "Valuation") + 
                    coord_flip() + 
                    theme_classic()
            })
        
        output$plot4 <- 
            renderPlot({
                full_dataset %>%
                    ggplot(aes(x = metro_area_pop, y = valuation)) + 
                    geom_point() + 
                    geom_text_repel(aes(label = team)) + 
                    geom_smooth(formula = y ~ x) + 
                    labs(title = "Metro Area Population vs. Team's Valuation", 
                         subtitle = "Correlation = 0.72", 
                         x = "Metro Area Population", y = "Valuation") + 
                    theme_classic() + 
                    scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25), 
                                       label = c("0M", "5M", "10M", "15M", 
                                                 "20M", "25M")) + 
                    scale_y_continuous(breaks = c(1000, 2000, 3000, 4000, 5000), 
                                       label = c("$1B", "$2B", "$3B", "$4B", 
                                                 "$5B"))
            })
        
        output$plot5 <- 
            renderPlot({
                full_dataset %>%
                    ggplot(aes(x = year_purchased, y = price_paid)) + 
                    geom_point() + 
                    geom_text_repel(aes(label = team)) + 
                    geom_smooth(formula = y ~ x) + 
                    labs(title = "Franchise's Year Purchased vs. Price Paid", 
                         subtitle = "Correlation = 0.72", x = "Year Purchased", 
                         y = "Price Paid") + 
                    theme_classic() + 
                    scale_y_continuous(breaks = c(0, 1000, 2000, 3000), 
                                       label = c("$0", "$1B", "$2B", "$3B"))
            })
        
        output$plot6 <- 
            renderPlot({
                full_dataset %>%
                    ggplot(aes(x = lastseasonwinpct, y = valuation)) + 
                    geom_point() + 
                    geom_text_repel(aes(label = team)) + 
                    geom_smooth(formula = y ~ x) + 
                    labs(title = "2019-2020 Winning Percentage vs. Team's Valuation", 
                         subtitle = "Correlation = 0.01", x = "19-20 Winning Percentage", 
                         y = "Valuation") + 
                    scale_y_continuous(breaks = c(1000, 2000, 3000, 4000, 5000), 
                                       label = c("$1B", "$2B", "$3B", "$4B", "$5B")) + 
                    theme_classic()
            })
        
    }
    
    shinyApp(ui, server)
    
    
    

    
    