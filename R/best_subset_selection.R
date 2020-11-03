
# best_subset_selection ---------------------------------------------------


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

