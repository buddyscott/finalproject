library(tidyverse)
library(readxl)


nbacapsheets <- "data-files/nbacapsheets.xlsx"
excel_sheets(path = nbacapsheets)
tab_names <- excel_sheets(path = nbacapsheets)
list_all <- lapply(tab_names, function(x) read_excel(path = nbacapsheets, 
                                                     sheet = x))
agg_capsheets <- rbind.fill(list_all) %>%
  filter(!is.na(num)) %>%
  filter(!is.na(currentcontract)) %>%
  select(name, position, age, experience:contractdetails)