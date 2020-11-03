library(tidyverse)

# Read and Clean ----------------------------------------------------------
files <- list.files(path = here::here("data"), pattern = ".csv")
files

cleaning <- function(df){
  df <- pivot_longer(df, cols = -country, names_to = "year")
}

files %>% 
  map(function(x) read_csv(paste0("data/", x))) %>% 
  setNames(gsub("\\.csv$", "", files)) %>% 
  map(cleaning) %>% 
  bind_rows(.id = "id") %>% 
  pivot_wider(names_from = id)# %>% 
write_csv(here::here("merged", "df.csv"))

# Filtering ---------------------------------------------------------------
data <- read_csv(here::here("merged", "df.csv"))
data %>% View()



