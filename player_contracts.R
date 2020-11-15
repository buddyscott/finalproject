library(tidyverse)
library(readr)

playercontracts <- read_csv("data-files/bbrefcontractdata2.csv", col_type = cols(
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