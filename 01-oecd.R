library(OECD)

# Search for data ---------------------------------------------------------

gds <- get_data_structure("HOUSE_PRICES")

library(tidyverse)
library(lubridate)

countries_id <- tibble(gds$COU$label) %>% 
  t() %>% 
  as_tibble() %>% 
  set_names(gds$COU$id)

countries <-  countries_id %>% 
  as.character()

# ind <- gds$IND$id
# names(ind) <- gds$IND$label

oecd <- get_dataset("HOUSE_PRICES")

gds %>% 
  with(IND)


# Real house price indices ------------------------------------------------

price <- oecd %>% 
  filter(IND == "RHP") %>% 
  select(COU, obsTime, obsValue) %>% 
  spread(COU, obsValue) %>% 
  filter_all(any_vars(str_detect(., "[0-9][0-9][0-9][0-9]-Q[1-4]"))) %>% 
  mutate(Date = zoo::as.Date(zoo::as.yearqtr(obsTime, format = "%Y-Q%q")),
         obsTime = NULL) %>% 
  select(Date, everything()) %>% 
  filter(Date >= as.Date("1975-01-01")) 

price <-  price %>% 
  setNames(c("Date", countries_id %>% 
               select(colnames(price)[-1]) %>%
               as.character()))

readr::write_csv(price, path = 'csv/price.csv')

# Price to rent -----------------------------------------------------------

rent <- oecd %>% 
  filter(IND == "HPI_RPI") %>% 
  select(COU, obsTime, obsValue) %>% 
  spread(COU, obsValue) %>% 
  filter_all(any_vars(str_detect(., "[0-9][0-9][0-9][0-9]-Q[1-4]"))) %>% 
  mutate(Date = zoo::as.Date(zoo::as.yearqtr(obsTime, format = "%Y-Q%q")),
         obsTime = NULL) %>% 
  select(Date, everything()) %>% 
  filter(Date >= as.Date("1975-01-01"))

rent <- rent %>% 
  setNames(c("Date", countries_id %>% 
               select(colnames(rent)[-1]) %>%
               as.character()))



# Price to income ---------------------------------------------------------

income <- oecd %>% 
  filter(IND == "HPI_YDH") %>% 
  select(COU, obsTime, obsValue) %>% 
  spread(COU, obsValue) %>% 
  filter_all(any_vars(str_detect(., "[0-9][0-9][0-9][0-9]-Q[1-4]"))) %>% 
  mutate(Date = zoo::as.Date(zoo::as.yearqtr(obsTime, format = "%Y-Q%q")),
         obsTime = NULL) %>% 
  select(Date, everything()) %>% 
  filter(Date >= as.Date("1975-01-01"))

income <- income %>% 
  setNames(c("Date", countries_id %>% 
               select(colnames(income)[-1]) %>%
               as.character()))

