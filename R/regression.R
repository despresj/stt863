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

summary(model_df5)

hist(resid(model_df5), breaks = 100)
qqnorm(resid(model_df5))
boxplot(resid(model_df5))

plot(data$edu_exp_gdp_per_person, resid(model_df5))
plot(data$corruption_perception_index_cpi, resid(model_df5))
plot(data$polrights_fh, resid(model_df5))
plot(data$electricity_use_per_person, resid(model_df5))
plot(data$gini, resid(model_df5))
plot(data$internet_users, resid(model_df5))
plot(data$labour_force_participation_rate_percent, resid(model_df5))
plot(data$life_expectancy_years, resid(model_df5))
plot(data$military_spending_pct_of_gdp, resid(model_df5))
plot(data$murder_per_mil_people, resid(model_df5))


sort(resid(model_df5))
data$resid <- resid(model_df5)

# Second model ------------------------------------------------------------

subsets %>% filter(df == 4) %>% 
  arrange(desc(adj.r.squared)) %>% 
  View()

model_df4_formula <- subsets %>% filter(df == 4) %>% 
  arrange(desc(adj.r.squared)) %>% 
  select(model) %>% head(1) %>% as.character() 

fit_df4 <- lm(model_df4_formula, data)

summary(fit_df4)

# third -------------------------------------------------------------------

subsets %>% filter(df == 6) %>% 
  arrange(desc(adj.r.squared)) %>% 
  View()

model_df6_formula <- subsets %>% filter(df == 6) %>% 
  arrange(desc(adj.r.squared)) %>% 
  select(model) %>% head(1) %>% as.character() 

fit_df6 <- lm(model_df6_formula, data)

summary(fit_df6)

