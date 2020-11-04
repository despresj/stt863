library(tidyverse)

# Read and Clean ----------------------------------------------------------
files <- list.files(path = here::here("raw_data"), pattern = ".csv")
files

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
data %>% filter(year > 2000) %>% 
write_csv(here::here("data", "df.csv"))

data <- read_csv(here::here("data", "df.csv")) 

