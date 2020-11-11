library(tidyverse)

data <- readr::read_csv(here::here("data", "df.csv"))
subsets <- readr::read_csv(here::here("data", "subsets.csv"))

# First model -------------------------------------------------------------

model_df5_formula <- subsets %>% filter(df == 5) %>% 
  arrange(desc(adj.r.squared)) %>% 
  select(model) %>% head(1) %>% as.character()

model_df5 <- lm(polrights_fh ~ corruption_perception_index_cpi + 
            edu_exp_gdp_per_person + 
            life_expectancy_years + 
            military_spending_pct_of_gdp + 
            murder_per_mil_people, data)

subsets %>% filter(df == 9) %>% 
  arrange(desc(adj.r.squared)) %>% 
  select(model) %>% head(1) %>% as.character()


# Second model ------------------------------------------------------------

subsets %>% filter(df == 4) %>% 
  arrange(desc(adj.r.squared)) 

model_df4_formula <- subsets %>% filter(df == 4) %>% 
  arrange(desc(adj.r.squared)) %>% 
  select(model) %>% head(1) %>% as.character() 

fit_df4 <- lm(model_df4_formula, data)

summary(fit_df4)

# third -------------------------------------------------------------------

subsets %>% filter(df == 6) %>% 
  arrange(desc(adj.r.squared)) 

model_df6_formula <- subsets %>% filter(df == 6) %>% 
  arrange(desc(adj.r.squared)) %>% 
  select(model) %>% head(1) %>% as.character() 

fit_df6 <- lm(model_df6_formula, data)

summary(fit_df6)
