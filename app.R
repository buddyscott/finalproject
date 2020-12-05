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
library(gt)

# reading in data from different .R file to reduce clutter

source("forbes.R",local = TRUE)

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
             h3("Data Sources"),
             p("Most of this data comes from the Forbes February 2020 NBA Team
               Valuations publication. Other data comes from spreadsheets that I
               maintain in my free time (the link to those spreadsheets is at 
               the bottom of the page."),
             br(),
             h3("Explanation of Variables in Dataset"),
             p("team: Abbreviation of team name"),
             p("value_change: Percentage change in team's valuation between
               February 2019 and February 2020"),
             p("year_purchased: Year current ownership group purchased team"),
             p("price_paid: Price (in millions) that current ownership group
               paid when they bought the team"),
             p("player_expenses: Money spent on players, including benefits
               and bonuses"),
             p("gate_receipts: Amount (in millions) that team brings in 
               through fan attendance"),
             p("wins_to_player: Compares the number of wins per player payroll
               relative to the rest of the NBA"),
             p("revenue_per_fan: Local revenues divded by metro population
               (New York and Los Angeles markets divided in half)"),
             p("metro_area_pop: Population (in millions) of metropolitan area"),
             p("sport: Portion of franchise's value attributable to revenue 
               shared among all teams"),
             p("market: Portion of franchise's value attributable to its city 
               and market size"),
             p("stadium: Portion of franchise's value attributable to its 
               arena"),
             p("brand: Portion of franchise's value attributable to its brand"),
             p("avg_ticket: Average ticket price"),
             p("build_cost: Amount (in millions) that arena cost to build"),
             p("nw: Net worth (in billions) of majority owner"),
             p("valuation: Team's valuation (in millions)"),
             p("debt_to_value: Debt divided by value, including arena debt"),
             p("revenue: Amount of revenue (in millions)"),
             p("operating_income: Earnings before interest, taxes, etc."),
             p("sport_pct: Percentage of franchise's value attributable to 
               revenue shared among all teams"),
             p("market_pct: Percentage of franchise's value attributable to
               its city and market size"),
             p("stadium_pct: Percentage of franchise's value attributable to 
               its arena"),
             p("brand_pct: Percentage of franchise's value attributable to its 
               brand"),
             p("growth_rate: Team's valuation minus price paid, divded by 
               difference between 2020 and year purchased"),
             p("income_no_fans: The operating income for a franchise, minus 
               the gate receipts that they project to lose with zero fan attendance"),
             p("revenue_no_fans: The revenue for a franchise, minus 
               the gate receipts that they project to lose with zero fan attendance"),
             
             ),
    
    tabPanel("Data",
             p("Please reference the methodology tab for explanations of what
               the variables represent."),
             DT::dataTableOutput("full_dataset"),
             ),
    
    tabPanel("Plots",
             p("This section allows you to plot two variables with each 
               other, either as a scatterplot or bar graph. Please reference
               the methodology tab for explanations of what the variables
               represent."),
             br(),
             
             fluidPage(
                 selectInput("x", "X variable", choices = names(full_dataset)),
                 selectInput("y", "Y variable", choices = names(full_dataset)),
                 selectInput("geom", "geom", c("point", "column")),
                 plotOutput("plot1")),
             br(),
    
             p("This is a plot of raw team valuations"),
             plotOutput("plot2"), 
             br(),
             
             p("This is a plot of the growth rate of team valuations, 
               calculated as (valuation - price_paid)/(2020-year_purchased)"),
             plotOutput("plot3"),
             br(),
             
             p("This is a plot of the raw values of the four components of
               a team's valuation"),
             plotOutput("plot4"),
             br(),
             
             p("This is a plot of the percentage breakdowns of the four
               components of a team's valuation"),
             plotOutput("plot5"), 
             
             ),
    
    tabPanel("Big Market",
             h3("Golden State Warriors"),
             imageOutput("myImage2"),
             br(), 
             br(),
             gt_output(outputId = "table1")
             ),
    
    
    tabPanel("Middle Market",
             h3("Portland Trail Blazers"),
             imageOutput("myImage3"),
             br(), 
             br(),
             gt_output(outputId = "table2")
    ),
    
    tabPanel("Small Market",
             h3("Memphis Grizzlies"),
             imageOutput("myImage4"),
             br(),
             br(),
             gt_output(outputId = "table3")
    ),
    
    tabPanel("Model",
             h3("Standard Generalized Linear Model Output"),
             br(),
             gt_output(outputId = "table4")
            ),
    
    tabPanel("Discussion",
             p("This will be a discussion about what the model shows as well as 
             potential policy suggestions to mitigate this potential damage."),
             ),
    
    tabPanel("About",
             h3("About Me"),
             p("My name is Buddy Scott and I concentrate in Economics with a 
               secondary in Government at Harvard College. I am a setter on the 
               Men's Volleyball team, the Editor in Chief for the Harvard 
               Sports Analysis Collective, and a Spring 2021 Intern at the 
               National Basketball Players Association (NBPA). You can reach me at 
               jamesscott@college.harvard.edu."), 
             imageOutput("myImage1"),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
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
        
        output$full_dataset = DT::renderDataTable({
            full_dataset
        })
        
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
        
        output$plot3 <- 
            renderPlot({
                full_dataset %>%
                    ggplot(aes(x = fct_reorder(team, growth_rate), 
                               y = growth_rate)) + 
                    geom_col() + 
                    coord_flip() + 
                    theme_classic() + 
                    labs(title = "Valuation Growth Rate by Team", x = "Team", 
                         y = "Growth Rate (Dollars per Year)") + 
                    scale_y_continuous(breaks = c(-800, -400, 0, 400), 
                                       labels = c("-$800M", "-$400M", "$0", 
                                                  "$400M"))
            })
        
        
        output$plot4 <- 
            renderPlot({
                pivoted_raw_dataset %>%
                    ggplot(aes(x = aspect, y = values)) + 
                    geom_col() + 
                    facet_wrap(~ team, ncol = 6) + 
                    theme(strip.text = element_text(size = 6), 
                          axis.text = element_text(size = 4), 
                          panel.grid = element_blank(), 
                          panel.spacing.x = unit(3, "mm")) + 
                    labs(title = "Team Valuation Breakdown", 
                         x = "Raw Valuation Breakdown", 
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
        
        
        output$plot5 <- 
            renderPlot({
                pivoted_pct_dataset %>%
                    ggplot(aes(x = aspect, y = values)) + 
                    geom_col() + 
                    facet_wrap(~ team, ncol = 6) + 
                    theme(strip.text = element_text(size = 6), 
                          axis.text = element_text(size = 4), 
                          panel.grid = element_blank(), 
                          panel.spacing.x = unit(3, "mm")) + 
                    labs(title = "Team Valuation Breakdown", 
                         x = "Percent Valuation Breakdown", 
                         y = "Percentage") +
                    scale_x_discrete(breaks = c("brand_pct", "market_pct", 
                                                "sport_pct", "stadium_pct"), 
                                     labels = c("Brand", "Market", 
                                                "Sport", "Stadium")) + 
                    scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6), 
                                       labels = c("0%", "20%", "40%", "60%")) + 
                    theme_classic()
            })
        
        output$myImage1 <- 
            renderImage({
            list(src = "buddyscott.png",
                 width = 300,
                 height = 500,
                 alt = "This is alternate text")
        }, deleteFile = FALSE)
        
        output$myImage2 <- 
            renderImage({
                list(src = "gsw.png",
                     width = 300,
                     height = 500,
                     alt = "This is alternate text")
            }, deleteFile = FALSE)
        
        output$myImage3 <- 
            renderImage({
                list(src = "por.png",
                     width = 300,
                     height = 500,
                     alt = "This is alternate text")
            }, deleteFile = FALSE)
        
        output$myImage4 <- 
            renderImage({
                list(src = "mem.png",
                     width = 300,
                     height = 500,
                     alt = "This is alternate text")
            }, deleteFile = FALSE)
        
        output$table1 <- 
            render_gt(
                
                tibble(subject = c("Valuation", "Metro Area Population", 
                                   "Stadium", "Stadium Percentage", "Debt to Value",
                                   "Gate Receipts", "Operating Income", "Income w/ No Fans", 
                                   "Revenue", "Revenue w / No Fans"), 
                       beta = c("$4.3B", "7.0M", "$1.1B", "25%", "18%", "$178M", 
                                "$109M", "-$69M", "$440M", "$262M"), 
                       `95% CI` = c("3rd", "7th", "2nd", "2nd", "26th", "1st", 
                                    "4th", "29th", "2nd", "3rd")) %>%
                    gt() %>%
                    cols_label(subject = "Variable", beta = "Value", 
                               `95% CI` = "League Rank") %>%
                    tab_style(cell_borders(sides = "right"), 
                              location = cells_body(columns = vars(subject))) %>%
                    tab_style(cell_text(weight = "bold"),
                              location = cells_body(columns = vars(subject))) %>%
                    cols_align(align = "center", columns = TRUE) %>%
                    fmt_markdown(columns = TRUE) %>%
                    tab_header(title = "Golden State Warriors Financials")
            )
        
        output$table2 <- 
            render_gt(
                
                tibble(subject = c("Valuation", "Metro Area Population", 
                                   "Stadium", "Stadium Percentage", "Debt to Value",
                                   "Gate Receipts", "Operating Income", "Income w/ No Fans", 
                                   "Revenue", "Revenue w / No Fans"), 
                       beta = c("$1.85B", "2.4M", "$364M", "20%", "7%", "$67M", 
                                "$51M", "-$16M", "$287M", "$220M"), 
                       `95% CI` = c("13th", "22nd", "11th", "9th", "13th", "11th", 
                                    "22nd", "25th", "13th", "13th")) %>%
                    gt() %>%
                    cols_label(subject = "Variable", beta = "Value", 
                               `95% CI` = "League Rank") %>%
                    tab_style(cell_borders(sides = "right"), 
                              location = cells_body(columns = vars(subject))) %>%
                    tab_style(cell_text(weight = "bold"),
                              location = cells_body(columns = vars(subject))) %>%
                    cols_align(align = "center", columns = TRUE) %>%
                    fmt_markdown(columns = TRUE) %>%
                    tab_header(title = "Portland Trail Blazers Financials")
            )
        
        output$table3 <- 
            render_gt(
                
                tibble(subject = c("Valuation", "Metro Area Population", 
                                   "Stadium", "Stadium Percentage", "Debt to Value",
                                   "Gate Receipts", "Operating Income", "Income w/ No Fans", 
                                   "Revenue", "Revenue w / No Fans"), 
                       beta = c("$1.3B", "1.4M", "$116M", "9%", "20%", "$20M", 
                                "$24M", "$4M", "$224M", "$204M"), 
                       `95% CI` = c("30th", "26th", "30th", "30th", "28th", "30th", 
                                    "29th", "20th", "30th", "25th")) %>%
                    gt() %>%
                    cols_label(subject = "Variable", beta = "Value", 
                               `95% CI` = "League Rank") %>%
                    tab_style(cell_borders(sides = "right"), 
                              location = cells_body(columns = vars(subject))) %>%
                    tab_style(cell_text(weight = "bold"),
                              location = cells_body(columns = vars(subject))) %>%
                    cols_align(align = "center", columns = TRUE) %>%
                    fmt_markdown(columns = TRUE) %>%
                    tab_header(title = "Memphis Grizzlies Financials")
            )
        
        output$table4 <- 
            render_gt(
        
        tibble(subject = c("Intercept", "Gate Receipts", "Operating Income", "Metro Area Population"), 
               beta = c("168.18", "1.21", "0.45", "2.52"), 
               `95% CI` = c("154.59, 181.34", "0.987, 1.417", "0.256, 0.653", "1.174, 3.890")) %>%
            gt() %>%
            cols_label(subject = "Variable", beta = "Beta", 
                       `95% CI` = "95% CI") %>%
            tab_style(cell_borders(sides = "right"), 
                      location = cells_body(columns = vars(subject))) %>%
            tab_style(cell_text(weight = "bold"),
                      location = cells_body(columns = vars(subject))) %>%
            cols_align(align = "center", columns = TRUE) %>%
            fmt_markdown(columns = TRUE) %>%
            tab_header(title = "Regression of Revenue on Variables of Interest", 
                       subtitle = "Focusing on coefficient of gate_receipts") %>%
            tab_footnote(footnote = "CI = Confidence Interval", 
                         locations = cells_column_labels(columns = vars(`95% CI`)))
            )
        
    }
    
    shinyApp(ui, server)
    
    
    

    
    