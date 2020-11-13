library(tidyverse)
library(kableExtra)
library(patchwork)

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



# model stats -------------------------------------------------------------


# remedial measures -------------------------------------------------------------------
outliars <- model_df5 %>% 
  broom::augment() %>% 
  filter(.resid < -4*sd(.resid))

hist <- model_df5 %>% 
  broom::augment() %>% 
  ggplot(aes(.resid)) + 
  geom_histogram(bins = 100, fill = "black") +
  scale_x_continuous(limits = c(-4,4)) 
hist
qq <- model_df5 %>% 
  broom::augment() %>% 
  ggplot(aes(sample = .resid)) + 
  stat_qq() +
  stat_qq_line() + 
scale_x_continuous(limits = c(-4,4)) + 
scale_y_continuous(limits = c(-4,4)) 



labeled_outliars <- hist +
  geom_point(outliars, mapping = aes(x = .resid, y = 1), color = "#D7261E", shape = 13, size = 10) + 
  geom_label(outliars, mapping = aes(x = .resid, y = 10, label = "OUTLIARS"), color = "#D7261E", size = 3)

labeled_qq <- qq + geom_point(outliars, mapping = aes(x = -3, y = -3.9), color = "#D7261E", shape = 0, size = 20) 

labeled_qq 
