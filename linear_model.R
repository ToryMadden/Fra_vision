





# Load packages
library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(magrittr)

# Import data

df <- read_rds('./data/fra-vision.rds')

df %<>% mutate(log_distance = log10(distance))

  df %>% filter(pain == "no") %>%
    filter(stimulus == "tactile") %>%
    filter(site == "forearm") %>%
    ggplot() +
    aes(x = log_distance,
        y = hitrate,
        colour = vision) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    geom_hline(yintercept = 6*0.75)
  
  df %<>% mutate(good_perf = ifelse(hitrate > 4,
                               yes = "yes",
                               no = ifelse(hitrate < 5,
                                           yes = "no",
                                           no = NA)))
#Logitic regressin with binary variable
  
library(ordinal)  
library(lme4)

log_regression <- glmer(data = df, as.factor(good_perf) ~ distance + vision + (1 | id), family = binomial)
summary(log_regression)

log_regression2 <- glmer(data = df, as.factor(good_perf) ~ distance + vision + (distance | id), family = binomial)
summary(log_regression)

anova(log_regression, log_regression2)
# log_regression 2 is a better fit (with random slope and intercept)

log_regression3 <- glmer(data = df, as.factor(good_perf) ~ distance * vision + (distance | id), family = binomial)
summary(log_regression)

anova(log_regression2, log_regression3)

# Adding an interaction between distance and vision does not improve the model.

model_null <- glmer(data = df, as.factor(good_perf) ~ 1 + (distance | id), family = binomial)
summary(log_regression)

anova(model_null, log_regression2)

# Model is informative (better than the null, which has no predictors).