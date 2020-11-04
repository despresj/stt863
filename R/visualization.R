library(tidyverse)
# Visuals -------------------------------------------------------------
theme_set(theme_light())

data <- read_csv(here::here("data", "df.csv"))
subsets <- read_csv(here::here("data", "subsets.csv"))

corr_matrix <- data %>% 
  select(-year) %>% 
  select_if(is.numeric) %>% 
  cor(.,  use = "complete.obs")



data %>% 
  ggplot(mapping = aes(x = freedix_fh)) + 
  geom_histogram(bins = 10)

data %>% 
  ggplot(mapping = aes(x = polrights_fh)) + 
  geom_histogram()

data %>% 
  ggplot(mapping = aes(x = cliberities_fh)) + 
  geom_histogram()

data %>% 
  mutate(trans_civlib = exp(cliberities_fh)) %>% 
  ggplot(mapping = aes(x = trans_civlib)) + 
  geom_histogram()

data %>% 
  filter(year > 2015) %>% 
  ggplot(mapping = aes(x = gini)) + 
  geom_histogram()

data %>% 
  ggplot(aes(x = gini, y = polrights_fh)) +
  geom_hex(binwidth = c(3, .6)) + 
  scale_fill_gradient(low = "#7FFFD4", high = "#8B8378") + 
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  geom_point(data = data %>% 
               filter(country == "United States", 
                         year == 2018), 
      aes(x = gini, y = polrights_fh), size = 5, color = "red") +
  labs(x = "GINI Coeffeceint", y = "Political Rights")

data %>% 
  ggplot(aes(x = gini, y = polrights_fh)) + 
  geom_smooth(se = FALSE)

r <- subsets %>% group_by(df) %>% summarise(adj = max(adj.r.squared, na.rm = T))

subsets %>% 
  ggplot(aes(x = df, y = adj.r.squared)) + 
  geom_point() + 
  geom_smooth(data = r, aes(df, adj), se = FALSE, span = 0.5, color = "Green") + 
  labs(title = "Best Subsets", x = "Parameters", y = "Adjusted R Squared")

AIC <- subsets %>% group_by(df) %>% summarise(AIC = max(AIC, na.rm = T))

subsets %>% 
  ggplot(aes(x = df, y = AIC)) + 
  geom_point() + 
  geom_smooth(data = AIC, aes(df, AIC), se = FALSE, span = .5, color = "Green") + 
  labs(title = "Best Subsets", x = "Parameters", y = "AIC")

model_df5 <- subsets %>% filter(df == 5) %>% 
  arrange(desc(adj.r.squared)) %>% 
  select(model) %>% head(1) %>% as.character()

model_df5
fit <- lm(model_df5, data = data)
fit %>% broom::tidy()

model_df4 <- subsets %>% filter(df == 4) %>% 
  arrange(desc(adj.r.squared)) %>% 
  select(model) %>% head(1) %>% as.character()

fit <- lm(model_df4, data = data)
fit %>% broom::tidy()

fit <- lm(model_df3, data = data)
fit %>% broom::tidy()

model_df3 <- subsets %>% filter(df == 3) %>% 
  arrange(desc(adj.r.squared)) %>% 
  select(model) %>% head(1) %>% as.character()

model_df2 <- subsets %>% filter(df == 2) %>% 
  arrange(desc(adj.r.squared)) %>% 
  select(model) %>% head(1) %>% as.character()

fit <- lm(model_df2, data = data)
fit %>% broom::tidy()

