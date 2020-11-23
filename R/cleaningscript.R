library(tidyverse)

# Read and Clean ----------------------------------------------------------
files <- list.files(path = here::here("raw_data"), pattern = ".csv")

cleaning <- function(df){
  df <- pivot_longer(df, cols = -country, names_to = "year")
}

data <- files %>% 
  map(function(x) read_csv(paste0("raw_data/", x))) %>% 
  setNames(gsub("\\.csv$", "", files)) %>% 
  map(cleaning) %>% 
  bind_rows(.id = "id") %>% 
  pivot_wider(names_from = id)

# Filtering ---------------------------------------------------------------

countries <- readRDS(here::here("data", "countries.RDS"))[15:51]


data <- data %>%
  filter(year > 2000, year < 2019, country %in% countries) %>% 
  group_by(country) %>%
  mutate_at(vars(-country),list(~ifelse(is.na(.), median(., na.rm = TRUE), .))) %>% 
# Iceland spent 0 so I needed to manually recode that
  mutate(military_spending_pct_of_gdp = replace_na(military_spending_pct_of_gdp, 0)) %>% 
  mutate(murder_per_mil_people = replace(murder_per_mil_people, country == "Mexico", 29.07),
         murder_per_mil_people = replace(murder_per_mil_people, country == "Chile", 4.4),
         murder_per_mil_people = replace(murder_per_mil_people, country == "Colombia", 25.34)) %>% 
         # https://en.wikipedia.org/wiki/List_of_countries_by_intentional_homicide_rate
         # these murder rates were not included in the gapminder but i didnt want to 
         # lose 3 important countries in LA
  relocate(polrights_fh) %>% mutate(polrights_fh = (8 - polrights_fh),
                                    military_spending_pct_of_gdp = military_spending_pct_of_gdp * 100)%>% 
  mutate(corruption_perception_index_cpi = (corruption_perception_index_cpi - 100) * -1)

# writing -----------------------------------------------------------------

write_csv(data, here::here("data", "df.csv"))
