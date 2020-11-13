library(tidyverse)
library(kableExtra)
library(patchwork)

data <- readr::read_csv(here::here("data", "df.csv"))
subsets <- readr::read_csv(here::here("data", "subsets.csv"))

# First model -------------------------------------------------------------

model_df5_formula <- subsets %>% filter(df == 5) %>% 
  arrange(desc(adj.r.squared)) %>% 
  select(model) %>% head(1) %>% as.character()

fit_df5 <- lm(model_df5_formula, data)

# Second model ------------------------------------------------------------

model_df4_formula <- subsets %>% filter(df == 4) %>% 
  arrange(desc(adj.r.squared)) %>% 
  select(model) %>% head(1) %>% as.character() 

fit_df4 <- lm(model_df4_formula, data)

model_df3_formula <- subsets %>% filter(df == 3) %>% 
  arrange(desc(adj.r.squared)) %>% 
  select(model) %>% head(1) %>% as.character() 

fit_df3 <- lm(model_df3_formula, data)

# third -------------------------------------------------------------------

model_df6_formula <- subsets %>% filter(df == 6) %>% 
  arrange(desc(adj.r.squared)) %>% 
  select(model) %>% head(1) %>% as.character() 

fit_df6 <- lm(model_df6_formula, data)

# remedial measures -------------------------------------------------------------------

outliars <- fit_df5 %>% 
  broom::augment() %>% 
  filter(.resid < -4*sd(.resid))

hist <- fit_df5 %>% 
  broom::augment() %>% 
  ggplot(aes(.resid)) + 
  geom_histogram(bins = 100, fill = "black") +
  scale_x_continuous(limits = c(-4,4)) 

hist

qq <- fit_df5 %>% 
  broom::augment() %>% 
  ggplot(aes(sample = .resid)) + 
  stat_qq() +
  stat_qq_line() + 
scale_x_continuous(limits = c(-4,4)) + 
scale_y_continuous(limits = c(-4,4)) 

labeled_outliars <- hist +
  geom_point(outliars, mapping = aes(x = .resid, y = 1), color = "#D7261E", shape = 13, size = 10) + 
  geom_label(outliars, mapping = aes(x = -3.3, y = 5, label = "OUTLIARS"), color = "#D7261E", size = 3)

labeled_outliars

labeled_qq <- qq + geom_point(outliars, mapping = aes(x = -3, y = -3.9), color = "#D7261E", shape = 0, size = 15) +
                   geom_point(outliars, mapping = aes(x = -2.6, y = -2.7), color = "#D7261E", shape = 0, size = 15) +
                   geom_label(outliars, mapping = aes(x = -3, y = -3.5, label = "OUTLIARS"), color = "#D7261E", size = 3)


labeled_qq 

