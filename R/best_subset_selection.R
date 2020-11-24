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
    for(k in 1:ncol(vc)){
    if(k == j){
    model <- as.formula(paste0(outcome, " ~", paste0(vc[,j], collapse = " + ")))
    }else{
    model <- as.formula(paste0(outcome, " ~", paste0(vc[,j],"*", vc[,k], collapse = " + ")))
    }
    models <- c(models, model)
    }
  }
}

subsets <- map(models, function(x) lm(x, data)) %>% 
  map(broom::glance) %>% 
  setNames(models) %>% 
  bind_rows(.id = "id") %>% 
  distinct() %>% 
  rename(model = id) %>% 
  mutate(
    Model = str_replace_all(model, "_", " "),
    Model = str_replace(Model, "~", "="), 
    Model = str_to_title(Model))
View(subsets)

beepr::beep()
# writing -----------------------------------------------------------------

#write_csv(subsets, here::here("data", "subsets.csv"))

subsets %>% select(model) %>% tail(1) %>% as.character()
