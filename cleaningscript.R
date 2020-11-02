library(tidyverse)

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

# Exploration -------------------------------------------------------------

data %>% 
  select(-year) %>% 
  select_if(is.numeric) %>% 
  cor(.,  use = "complete.obs") %>% 
  View()

data %>% 
  filter(year > 2015) %>% 
  ggplot(mapping = aes(x = freedix_fh)) + 
  geom_histogram()

data %>% 
  filter(year > 2015) %>% 
  ggplot(mapping = aes(x = polrights_fh)) + 
  geom_histogram()

data %>% 
  filter(year > 2015) %>% 
  ggplot(mapping = aes(x = cliberities_fh)) + 
  geom_histogram()

data %>% 
  filter(year > 2015) %>% 
  mutate(trans_civlib = exp(cliberities_fh)) %>% 
  ggplot(mapping = aes(x = trans_civlib)) + 
  geom_histogram()

data %>% 
  filter(year > 2015) %>% 
#  mutate(trans_civlib = gini) %>% 
  ggplot(mapping = aes(x = gini)) + 
  geom_histogram()

data %>% 
  select(country, gini) %>% 
  group_by(country) %>%
  summarise(mean = mean(gini, na.rm = TRUE)) %>% 
  View()

data %>% 
  select(country, journakilled) %>% 
  group_by(country) %>%
  summarise(mean = mean(journakilled, na.rm = TRUE)) %>% 
  View()

# Best Subsets ------------------------------------------------------------

names(data)
vars <- data %>% 
  select(-country, -year, -ends_with("_fh")) %>%
  names()

outcome <- "polrights_fh"

models <- list()
for (i in 1:length(vars)) {
  vc <- combn(vars,i)
  for (j in 1:ncol(vc)) {
    model <- as.formula(paste0(outcome, " ~", paste0(vc[,j], collapse = " + ")))
    models <- c(models, model)
  }
}

subsets <- map(models, function(x) lm(x, data)) %>% 
  map(broom::glance) %>% 
  setNames(models) %>% 
  bind_rows(.id = "id") %>% 
  distinct() %>% 
  rename(model = id) 

subsets %>% View()


