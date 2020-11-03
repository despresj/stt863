library(tidyverse)
#hey we are editing
# Read and Clean ----------------------------------------------------------
files <- list.files(path = here::here("sttproj", "data"), pattern = ".csv")

files

cleaning <- function(df){
  df <- pivot_longer(df, cols = -country, names_to = "year")
}

files %>% 
  map(function(x) read_csv(paste0("sttproj/data/", x))) %>% 
  setNames(gsub("\\.csv$", "", files)) %>% 
  map(cleaning) %>% 
  bind_rows(.id = "id") %>% 
  pivot_wider(names_from = id) # %>% 
write_csv(here::here("sttproj", "merged", "df.csv"))

# Filtering ---------------------------------------------------------------

data <- read_csv(here::here("sttproj", "merged", "df.csv"))
vars <- paste0(colnames(data), sep = ", ", collapse = "" )
data %>% select(-ends_with("_fh")) %>% names() %>% paste0(., sep = ", ", collapse = "" )




