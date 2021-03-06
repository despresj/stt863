library(tidyverse)
# Visuals -------------------------------------------------------------
theme_set(theme_light())

data <- read_csv(here::here("data", "df.csv"))
subsets <- read_csv(here::here("data", "subsets.csv"))
names(data)

corr_matrix <- data %>% 
  select(-year, -country) %>% 
  cor(.,  use = "complete.obs")

View(corr_matrix)

names(data)

data %>% 
  ggplot(mapping = aes(x = polrights_fh)) + 
  geom_histogram(bins = 10)

data %>% 
  ggplot(mapping = aes(x = polrights_fh)) + 
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
summary(fit)

model_df4 <- subsets %>% filter(df == 4) %>% 
  arrange(desc(adj.r.squared)) %>% 
  select(model) %>% head(1) %>% as.character()

fit <- lm(model_df4, data = data)
fit %>% broom::tidy()

summary(fit)

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

summary(model_df5)

hist(resid(model_df5), breaks = 100)
qqnorm(resid(model_df5))
boxplot(resid(model_df5))

plot(data$edu_exp_gdp_per_person, resid(fit))

plot(data$corruption_perception_index_cpi, resid(fit))

plot(data$polrights_fh, resid(fit))

plot(data$electricity_use_per_person, resid(fit))
plot(data$gini, resid(fit))
plot(data$internet_users, resid(fit))
plot(data$labour_force_participation_rate_percent, resid(fit))
plot(data$life_expectancy_years, resid(fit))
plot(data$military_spending_pct_of_gdp, resid(fit))
plot(data$murder_per_mil_people, resid(fit))



hsit <- model_df5 %>% 
  broom::augment() %>% 
  ggplot() + 
  geom_histogram(aes(.resid), bins = 200) +
  scale_x_continuous(limits = c(-4,4))

model_df5 %>% 
  broom::augment() %>% 
  ggplot() + 
  geom_qq(aes(.resid))