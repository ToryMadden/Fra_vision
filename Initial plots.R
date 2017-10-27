############################################################
#                                                          #
#                 Basic plots of Day 1 data                #
#                                                          #
############################################################

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

# Use forearm site, tactile stimulus, pain-free participants.

# Create plots

pain_free <- df %>% filter(pain == "no") %>%
  filter(stimulus == "tactile") %>%
  filter(site == "forearm")

  unique(pain_free$id)

df %>% filter(pain == "no") %>%
  filter(stimulus == "tactile") %>%
  filter(site == "forearm") %>%
  ggplot() +
  aes(x = as.factor(distance),
      y = hitrate,
      colour = as.factor(distance),
      fill = as.factor(distance)) +
  geom_violin()

df %>% filter(hitrate > 0) %>%
  filter(stimulus == "tactile") %>%
  filter(site == "back") %>%
  ggplot() +
  aes(x = as.factor(distance),
      y = hitrate,
      colour = as.factor(pain),
      fill = as.factor(pain)) +
  geom_violin()

df_medians <- df %>%
  filter(pain == "no") %>%
  filter(stimulus == "tactile") %>%
  filter(site == "forearm") %>%
  group_by(distance) %>%
  summarise(median_hitrate = median(hitrate, na.rm = TRUE))

df_medians %>%
  ggplot() +
  aes(x = distance,
      y = median_hitrate) +
  geom_point() +
  geom_smooth()

# Cumulative approach

df_summed_FT <- df %>% 
  filter(pain == "no") %>%
  filter(stimulus == "tactile") %>%
  filter(site == "forearm") %>%
  group_by(distance) %>%
  summarise(cumulative_hitrate = sum(hitrate, na.rm = TRUE))

df_summed_FT %>%
  ggplot() +
  aes(x = distance,
      y = cumulative_hitrate) +
  geom_point() +
  geom_smooth()
  

# Use forearm site, tactile stimulus, all participants.

# Create plots

df %>%
  filter(stimulus == "tactile") %>%
  filter(site == "forearm") %>%
  ggplot() +
  aes(x = distance,
      y = hitrate,
      colour = as.factor(id)) +
  geom_point()

df_medians <- df %>%
  filter(stimulus == "tactile") %>%
  filter(site == "forearm") %>%
  group_by(distance) %>%
  summarise(median_hitrate = median(hitrate, na.rm = TRUE))

df_medians %>%
  ggplot() +
  aes(x = distance,
      y = median_hitrate) +
  geom_point() +
  geom_smooth()

# Cumulative approach, all participants

df_summed_FT <- df %>%
  filter(stimulus == "tactile") %>%
  filter(site == "forearm") %>%
  group_by(distance) %>%
  summarise(cumulative_hitrate = sum(hitrate, na.rm = TRUE))

df_summed_FT %>%
  ggplot() +
  aes(x = distance,
      y = cumulative_hitrate) +
  geom_point() +
  geom_smooth()
