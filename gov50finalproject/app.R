
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
             p("The NBA salary cap is the mechanism that effectively equilibrates supply and demand for players. The salary cap level is 
set based on Basketball Related Income (BRI). NBA Commissioner Adam Silver 
revealed in May 2020 that ticket sales for in-person fan attendances comprises 
an estimated 40% of the NBA's revenue. The NBA's revenue is split roughly 50-50 
between players and the owners of these teams. Figure 1 shows how the players' 
and owners' split of BRI would each decrease according to this projection
from $4 billion to $2.4 billion. If this is the case, the league might have to 
withhold an increased percentage of players' salaries in escrow for the 
upcoming season to finance the league's operations, potentially leading to 
heavily contested debates at the bargaining table for an updated 
Collective Bargaining Agreement. The two pressing economic problems this paper 
will address are the league's potentially inaccurate forward guidance provided 
to teams about salary cap levels for the upcoming season, as well as potential 
inequalities emerging from teams' varying levels of cash flows and spending 
power that could potentially harm competitive balance in a zero-sum league.

The potential and perhaps inevitable inaccuracies to the forward guidance 
provided by the league in regard to future salary cap levels is quite similar 
to the concept of expected inflation; accurate levels of expectations are in a 
lot of ways more important than the actual level. Teams plan their finances 
as it pertains to player salaries years in advance using the latest projections
given to them by the league on future salary cap levels. The salary cap was 
$109.14 million in the 2019-2020 season, and the last pre-pandemic projection 
for the following season's level was $115 million. Now, most salary cap experts
project that the salary cap staying at the $109.14 million level (the same as 
the previous season) is the best-case scenario. 

Stemming off of this concept of inaccurate forward guidance, the looming
problem is potential decreases in the competitive balance of the league, 
specifically a rise in correlation between payroll / team wealth and winning 
percentage. Figure 2 shows team valuation (as measured by Forbes in February 
2020) compared to team winning percentage in the 2019-2020 season. We can see a
near-zero correlation of 0.012, though this number might be affected by two of 
the three richest teams (the New York Knicks and Golden State Warriors) having 
two of the six worst records in the league this past season. Figure 3 shows 
the comparison of the same two variables, this time over the last ten years, 
and we see a more positive, yet still quite small, correlation of 0.125. 
Unlike the capitalistic nature of the American economy, the economy within the 
NBA and most other major sports leagues is quite socialist in nature. Although 
the 30 NBA teams are by no means equal in financial status - as evidenced by 
team wealth in Figure 4 and wealth of the team's majority owner in Figure 5 - 
the league is set up to promote competitive balance. This is done through 
policies like the teams with the worst records getting higher picks in the 
amateur player draft, teams generally being allowed to pay their players more 
than opposing teams can to retain their services, and revenue sharing to
smaller-market teams that rely on this revenue to stay financially afloat. 
The fear among those plugged into the NBA community is that the dire financial 
conditions that some teams, presumably smaller-market and poorer, will face 
might force teams to prioritize saving money over improving their roster, which 
is the worst thing that could happen for the competitive balance of the league.

The five figures presented in this summary are merely a snapshot of the analysis
that this paper will eventually conduct. Data will be collected on the 
businesses that these teams' majority owners are invested in and how their 
businesses have been impacted by COVID-19. For example, the owner of the 
Brooklyn Nets is the co-founder of Alibaba (the equivalent of Amazon in China)
whereas the owner of the Miami Heat has all of his money in cruise ships and 
the owner of the Indiana Pacers invests in shopping malls. Analyzing not only 
each NBA team's financial condition but also the financial condition of their 
majority owner will prove crucial to better understanding the financial 
situation of the NBA."),
             h3("About Me"),
             p("My name is Buddy Scott and I study Economics with a secondary 
              in Government at Harvard College. 
             You can reach me at jamesscott@college.harvard.edu."), 
             a("Connect with me on LinkedIn", 
               href = "https://www.linkedin.com/in/buddyscott13/"),
             a("Please see my GitHub repo here", 
               href = "https://github.com/buddyscott/finalproject"),
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
    
    
    
    

    
    