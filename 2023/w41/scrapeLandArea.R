# To clean data
library(tidyverse)
library(lubridate)
library(janitor)
# To scrape data
library(rvest)
library(httr)
library(polite)

url = "https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States"

url_bow <- polite::bow(url)
url_bow


ind_html <-
  polite::scrape(url_bow) %>%  # scrape web page
  rvest::html_nodes("table.wikitable") %>% # pull out specific table
  rvest::html_table(fill = TRUE) 



s = ind_html[[1]] %>%
  row_to_names(1) %>%
  clean_names() %>%
  select(.,c(1,2, 8)) %>%
  setNames(c("state", "state_abbrev", "LandArea"))


s1 = ind_html[[2]] %>%
  row_to_names(1) %>%
  clean_names() %>%
  select(.,c(1,2, 6)) %>%
  setNames(c("state", "state_abbrev", "LandArea"))


s2 = bind_rows(s,s1)
