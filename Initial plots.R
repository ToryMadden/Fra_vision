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

# Create plots

# Use forearm site, tactile stimulus, pain-free participants, both sessions.

df %>% filter(pain == "no") %>%
  filter(hitrate > 2) %>%
  filter(stimulus == "tactile") %>%
  filter(site == "forearm") %>%
  ggplot() +
  aes(x = as.factor(distance),
      y = hitrate,
      colour = vision,
      fill = vision) +
  geom_violin()

# Compare sessions, tactile, forearm, pain-free

df %>% filter(pain == "no") %>%
  filter(stimulus == "tactile") %>%
  filter(site == "forearm") %>%
  ggplot() +
  aes(x = as.factor(distance),
      y = hitrate,
      colour = as.factor(session),
      fill = as.factor(session)) +
  geom_violin()

# Compare vision at session 2, tactile, forearm, pain-free

df %>% filter(pain == "no") %>%
  filter(stimulus == "tactile") %>%
  filter(session == 2) %>%
  filter(site == "forearm") %>%
  ggplot() +
  aes(x = as.factor(distance),
      y = hitrate,
      colour = as.factor(vision),
      fill = as.factor(vision)) +
  geom_violin()

# Use forearm site, nociceptive stimulus, pain-free participants, both sessions.

df %>% filter(pain == "no") %>%
  filter(stimulus == "nociceptive") %>%
  filter(site == "forearm") %>%
  ggplot() +
  aes(x = as.factor(distance),
      y = hitrate,
      colour = as.factor(distance),
      fill = as.factor(distance)) +
  geom_violin()

# Compare sessions, nociceptive, forearm, pain-free

df %>% filter(pain == "no") %>%
  filter(stimulus == "nociceptive") %>%
  filter(site == "forearm") %>%
  ggplot() +
  aes(x = as.factor(distance),
      y = hitrate,
      colour = as.factor(session),
      fill = as.factor(session)) +
  geom_violin()

# Compare vision at session 2, nociceptive, forearm, pain-free

df %>% filter(pain == "no") %>%
  filter(stimulus == "nociceptive") %>%
  filter(session == 2) %>%
  filter(site == "forearm") %>%
  ggplot() +
  aes(x = as.factor(distance),
      y = hitrate,
      colour = as.factor(vision),
      fill = as.factor(vision)) +
  geom_violin()

# Compare tactile to nociceptive

df %>% filter(pain == "no") %>%
  filter(site == "forearm") %>%
  ggplot() +
  aes(x = as.factor(distance),
      y = hitrate,
      colour = as.factor(stimulus),
      fill = as.factor(stimulus)) +
  geom_violin()

# Compare vision at session 2, both stimulus types, forearm, pain-free

df %>% filter(pain == "no") %>%
  filter(session == 2) %>%
  filter(site == "forearm") %>%
  ggplot() +
  aes(x = as.factor(distance),
      y = hitrate,
      colour = as.factor(vision),
      fill = as.factor(vision)) +
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
