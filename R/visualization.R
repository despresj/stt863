library(tidyverse)

# Visuals -------------------------------------------------------------

data <- read_csv(here::here("data", "df.csv"))

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