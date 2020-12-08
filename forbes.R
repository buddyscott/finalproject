library(tidyverse)
library(readr)

nbainfo <- read_csv("data-files/nbainfo.csv", 
                    
                    # Getting code in the right format so I can perform 
                    # my analysis.
                    
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
  
  # Renaming variables so they do not start with numbers, which makes it easier
  # to type them without having to put them in quotes every time.
  
  rename_with(~ str_replace(.x, "1920", "lastseason")) %>% 
  rename_with(~ str_replace(.x, "2021", "nextseason")) %>% 
  slice(1:30) %>%
  subset(select = -nextseasonprojcapspace) %>%
  select(team, lastseasonwinpct, lastseasonortg, lastseasondrtg, 
         lastseasonnrtg, lastseasonpace, gtcontracts, avgage, medage, avgexp,
         medexp, nextseasonprojsalary, nextseasonprojexceptions, 
         tenyrwinpct, playoffpct, principal_owner, nw, valuation, 
         percent_change, debt_to_value, revenue, operating_income)


forbes1 <- read_csv("data-files/forbes2020.csv", 
                    col_type = cols(.default = col_character())) %>%
  
  # Making a lot of the varaibles numeric.
  
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

# Joining the forbes 1 and forbes 2 data to make the full dataset that is the 
# data set I will be using for the plots.

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
  
  # Using a bunch of mutate functions to create new variables like percentages
  # and standardizing valuation by making multiplying by 100.
  
  mutate(valuation = valuation*1000) %>%
  mutate(sport_pct = sport/valuation) %>%
  mutate(market_pct = market/valuation) %>%
  mutate(stadium_pct = stadium/valuation) %>%
  mutate(brand_pct = brand/valuation) %>%
  mutate(growth_rate = (valuation-price_paid)/(2020-year_purchased)) %>%
  mutate(income_no_fans = operating_income - gate_receipts) %>%
  mutate(revenue_no_fans = revenue - gate_receipts)

full_dataset_condensed <- full_dataset %>%
  
  # I wanted less variables for the interactive plot in my ShinyApp so I 
  # decreased the number of variables in the full data set from roughly 40 to 20.
  
  select(team, valuation, metro_area_pop, operating_income, revenue, 
         gate_receipts, avg_ticket, brand, brand_pct, market, market_pct, 
         stadium, stadium_pct, sport, sport_pct, income_no_fans, 
         revenue_no_fans, value_change, year_purchased, price_paid, build_cost, 
         growth_rate, value_change)

# I needed to create two "new" pivoted datasets to produce two plots in my "plots"
# section that have all 30 team values for four different values, so the dataset
# became 120 rows instead of 30 (four per team).

pivoted_raw_dataset <- full_dataset_condensed %>%
  select(team, valuation, sport, market, stadium, brand) %>%
  pivot_longer(sport:brand, names_to = "aspect", values_to = "values") %>%
  arrange(desc(valuation))

pivoted_pct_dataset <- full_dataset_condensed %>%
  select(team, valuation, sport_pct, market_pct, stadium_pct, brand_pct) %>%
  pivot_longer(sport_pct:brand_pct, 
               names_to = "aspect", values_to = "values") %>%
  arrange(desc(valuation))

# Doing the same pivoted technique here, except just want to get the data 
# for three specific teams.

pivoted_pct_dataset_gsw <- full_dataset_condensed %>%
  filter(team == "GSW") %>%
  select(team, valuation, sport_pct, market_pct, stadium_pct, brand_pct) %>%
  pivot_longer(sport_pct:brand_pct, 
               names_to = "aspect", values_to = "values") %>%
  arrange(desc(valuation))

pivoted_pct_dataset_por <- full_dataset_condensed %>%
  filter(team == "POR") %>%
  select(team, valuation, sport_pct, market_pct, stadium_pct, brand_pct) %>%
  pivot_longer(sport_pct:brand_pct, 
               names_to = "aspect", values_to = "values") %>%
  arrange(desc(valuation))

pivoted_pct_dataset_mem <- full_dataset_condensed %>%
  filter(team == "MEM") %>%
  select(team, valuation, sport_pct, market_pct, stadium_pct, brand_pct) %>%
  pivot_longer(sport_pct:brand_pct, 
               names_to = "aspect", values_to = "values") %>%
  arrange(desc(valuation))

# This is my code for the regression trees that I display in my model section.

revenue_tree <- rpart(revenue ~ operating_income + gate_receipts + 
                        metro_area_pop, data = full_dataset_condensed, 
                      cp = 0.01)

valuation_tree <- rpart(valuation ~ market + stadium + sport + brand, 
                        data = full_dataset_condensed, cp = 0.01)


