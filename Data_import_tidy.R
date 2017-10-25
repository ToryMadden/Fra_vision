############################################################
#                                                          #
#                   Import and tidy data                   #
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

df <- read_xlsx('/Users/tory/Google Drive/Canned studies/Back-Fra Vision Study/Experiment 1 data with all 20.xlsx', 
                col_names = TRUE)

# Mutate column to indicate pain classification (CLBP or pain-free), then assign new IDs

df %<>% mutate(pain = ifelse(str_detect(ID, pattern = '^CLBP'), 
                                  yes = "yes",
                                  no = "no"))

df[ ,1] <- 1:54

# Delete unwanted columns

df %<>% select(ID, AGE, SEX, pain, GROUP,
               starts_with("FN"),
               starts_with("FT"),
               starts_with("BN"),
               starts_with("BT"),
               -contains("AV"))

# Sum hits for each outcome
nam <- names(df)
df_hits <- df %>% mutate(FN4_hitrate = rowSums(.[grep(pattern = '^FN4.$', x = nam)])) %>%
  mutate(FN8_hitrate = rowSums(.[grep(pattern = '^FN8.$', x = nam)])) %>%
  mutate(FN12_hitrate = rowSums(.[grep(pattern = '^FN12.$', x = nam)])) %>%
  mutate(FN20_hitrate = rowSums(.[grep(pattern = '^FN20.$', x = nam)])) %>%
  mutate(FN44_hitrate = rowSums(.[grep(pattern = '^FN44.$', x = nam)])) %>%
  mutate(FT4_hitrate = rowSums(.[grep(pattern = '^FT4.$', x = nam)])) %>%
  mutate(FT8_hitrate = rowSums(.[grep(pattern = '^FT8.$', x = nam)])) %>%
  mutate(FT12_hitrate = rowSums(.[grep(pattern = '^FT12.$', x = nam)])) %>%
  mutate(FT20_hitrate = rowSums(.[grep(pattern = '^FT20.$', x = nam)])) %>%
  mutate(FT44_hitrate = rowSums(.[grep(pattern = '^FT44.$', x = nam)])) %>%
  mutate(BT4_hitrate = rowSums(.[grep(pattern = '^BT4.$', x = nam)])) %>%
  mutate(BT8_hitrate = rowSums(.[grep(pattern = '^BT8.$', x = nam)])) %>%
  mutate(BT12_hitrate = rowSums(.[grep(pattern = '^BT12.$', x = nam)])) %>%
  mutate(BT20_hitrate = rowSums(.[grep(pattern = '^BT20.$', x = nam)])) %>%
  mutate(BT44_hitrate = rowSums(.[grep(pattern = '^BT44.$', x = nam)])) %>%
  mutate(BN4_hitrate = rowSums(.[grep(pattern = '^BN4.$', x = nam)])) %>%
  mutate(BN8_hitrate = rowSums(.[grep(pattern = '^BN8.$', x = nam)])) %>%
  mutate(BN12_hitrate = rowSums(.[grep(pattern = '^BN12.$', x = nam)])) %>%
  mutate(BN20_hitrate = rowSums(.[grep(pattern = '^BN20.$', x = nam)])) %>%
  mutate(BN44_hitrate = rowSums(.[grep(pattern = '^BN44.$', x = nam)])) %>%
  select(ID, AGE, SEX, pain, GROUP, contains("hitrate")) %>%
  rename(id = ID,
         age = AGE,
         sex = SEX,
         group = GROUP)

