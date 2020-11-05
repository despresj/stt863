library(tidyverse)

# best_subset_selection ---------------------------------------------------

data <- read_csv(here::here("data", "df.csv"))

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

subsets %>% 
  write_csv(here::here("data", "best_subsets.csv"))





# this cannot be right ---------------------------------------------------


data$garbage <- data$polrights_fh + rnorm(length(data$polrights_fh),3.68,1.94)
outcome <- "garbage"
