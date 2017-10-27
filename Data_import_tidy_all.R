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

df1 <- read_xlsx('/Users/tory/Google Drive/Canned studies/Back-Fra Vision Study/Experiment 1 data with all 20.xlsx', col_names = TRUE)

df2 <- read_xlsx('/Users/tory/Google Drive/Canned studies/Back-Fra Vision Study/Experiment 2 data.xlsx', 
                col_names = TRUE)

# Bind together by ID

df_all <- df1 %>% left_join(df2, by = "ID")

# Mutate column to indicate pain classification (CLBP or pain-free), then assign new IDs

df_all %<>% mutate(pain = ifelse(str_detect(ID, pattern = '^CLBP'), 
                                  yes = "yes",
                                  no = "no"))

df_all[ ,1] <- 1:54

# Delete unwanted columns

df_all %<>% select(ID, AGE.y, SEX.y, pain, GROUP,
               starts_with("FN"),
               starts_with("FT"),
               starts_with("BN"),
               starts_with("BT"),
               starts_with("VT"),
               starts_with("NVT"),
               starts_with("VN"),
               starts_with("NVN"),
               -contains("AV"))

# Sum hits for each outcome
nam <- names(df_all)
df_hits <- df_all %>% 
  mutate(FT4_hitrate1 = rowSums(.[grep(pattern = '^FT4.$', x = nam)])) %>%
  mutate(FT8_hitrate1 = rowSums(.[grep(pattern = '^FT8.$', x = nam)])) %>%
  mutate(FT12_hitrate1 = rowSums(.[grep(pattern = '^FT12.$', x = nam)])) %>%
  mutate(FT20_hitrate1 = rowSums(.[grep(pattern = '^FT20.$', x = nam)])) %>%
  mutate(FT44_hitrate1 = rowSums(.[grep(pattern = '^FT44.$', x = nam)])) %>%
  mutate(FN4_hitrate1 = rowSums(.[grep(pattern = '^FN4.$', x = nam)])) %>%
  mutate(FN8_hitrate1 = rowSums(.[grep(pattern = '^FN8.$', x = nam)])) %>%
  mutate(FN12_hitrate1 = rowSums(.[grep(pattern = '^FN12.$', x = nam)])) %>%
  mutate(FN20_hitrate1 = rowSums(.[grep(pattern = '^FN20.$', x = nam)])) %>%
  mutate(FN44_hitrate1 = rowSums(.[grep(pattern = '^FN44.$', x = nam)])) %>%
  mutate(BT4_hitrate1 = rowSums(.[grep(pattern = '^BT4.$', x = nam)])) %>%
  mutate(BT8_hitrate1 = rowSums(.[grep(pattern = '^BT8.$', x = nam)])) %>%
  mutate(BT12_hitrate1 = rowSums(.[grep(pattern = '^BT12.$', x = nam)])) %>%
  mutate(BT20_hitrate1 = rowSums(.[grep(pattern = '^BT20.$', x = nam)])) %>%
  mutate(BT44_hitrate1 = rowSums(.[grep(pattern = '^BT44.$', x = nam)])) %>%
  mutate(BN4_hitrate1 = rowSums(.[grep(pattern = '^BN4.$', x = nam)])) %>%
  mutate(BN8_hitrate1 = rowSums(.[grep(pattern = '^BN8.$', x = nam)])) %>%
  mutate(BN12_hitrate1 = rowSums(.[grep(pattern = '^BN12.$', x = nam)])) %>%
  mutate(BN20_hitrate1 = rowSums(.[grep(pattern = '^BN20.$', x = nam)])) %>%
  mutate(BN44_hitrate1 = rowSums(.[grep(pattern = '^BN44.$', x = nam)])) %>%
  mutate(FT4_hitrate2 = rowSums(.[grep(pattern = '^NVT4.$', x = nam)])) %>%
  mutate(FT8_hitrate2 = rowSums(.[grep(pattern = '^NVT8.$', x = nam)])) %>%
  mutate(FT12_hitrate2 = rowSums(.[grep(pattern = '^NVT12.$', x = nam)])) %>%
  mutate(FT20_hitrate2 = rowSums(.[grep(pattern = '^NVT20.$', x = nam)])) %>%
  mutate(FT44_hitrate2 = rowSums(.[grep(pattern = '^NVT44.$', x = nam)])) %>%
  mutate(FN4_hitrate2 = rowSums(.[grep(pattern = '^NVN4.$', x = nam)])) %>%
  mutate(FN8_hitrate2 = rowSums(.[grep(pattern = '^NVN8.$', x = nam)])) %>%
  mutate(FN12_hitrate2 = rowSums(.[grep(pattern = '^NVN12.$', x = nam)])) %>%
  mutate(FN20_hitrate2 = rowSums(.[grep(pattern = '^NVN20.$', x = nam)])) %>%
  mutate(FN44_hitrate2 = rowSums(.[grep(pattern = '^NVN44.$', x = nam)])) %>%
  mutate(FT4_hitrate2V = rowSums(.[grep(pattern = '^VT4.$', x = nam)])) %>%
  mutate(FT8_hitrate2V = rowSums(.[grep(pattern = '^VT8.$', x = nam)])) %>%
  mutate(FT12_hitrate2V = rowSums(.[grep(pattern = '^VT12.$', x = nam)])) %>%
  mutate(FT20_hitrate2V = rowSums(.[grep(pattern = '^VT20.$', x = nam)])) %>%
  mutate(FT44_hitrate2V = rowSums(.[grep(pattern = '^VT44.$', x = nam)])) %>%
  mutate(FN4_hitrate2V = rowSums(.[grep(pattern = '^VN4.$', x = nam)])) %>%
  mutate(FN8_hitrate2V = rowSums(.[grep(pattern = '^VN8.$', x = nam)])) %>%
  mutate(FN12_hitrate2V = rowSums(.[grep(pattern = '^VN12.$', x = nam)])) %>%
  mutate(FN20_hitrate2V = rowSums(.[grep(pattern = '^VN20.$', x = nam)])) %>%
  mutate(FN44_hitrate2V = rowSums(.[grep(pattern = '^VN44.$', x = nam)])) %>%
  select(ID, AGE.y, SEX.y, pain, GROUP, contains("hitrate")) %>%
  rename(id = ID,
         age = AGE.y,
         sex = SEX.y,
         group = GROUP)



# Gather from wide to long format

gathered <- gather(data = df_hits, 
                   key = measure_type, 
                   value = hitrate, 
                   contains('hitrate'))

gathered %<>% mutate(site = ifelse(str_detect(measure_type, pattern = '^F'), 
                                  yes = "forearm",
                                  no = "back"))

gathered %<>% mutate(stimulus = ifelse(str_detect(measure_type, pattern = '^.T'), 
                                        yes = "tactile",
                                        no = "nociceptive"))

gathered %<>% mutate(vision = ifelse(str_detect(measure_type, pattern = 'V$'), 
                                       yes = "yes",
                                       no = "no"))

gathered %<>% separate(col = measure_type, 
                       into = c("measure", "x"))

data <- gathered %>% mutate(
  distance = ifelse(
    grepl(pattern = '44$', x = measure),
    yes = 44,
    no = ifelse(grepl(pattern = '20$', x = measure),
                yes = 20,
                no = ifelse(grepl(pattern = '12$', x = measure),
                                  yes = 12,
                                  no = ifelse(grepl(pattern = '8$', x = measure),
                                              yes = 8,
                                              no = 4)))))

data %<>% mutate(stimulus = ifelse(str_detect(measure, pattern = '^.T'), 
                                       yes = "tactile",
                                       no = "nociceptive"))

data %<>% mutate(session = ifelse(x == "hitrate1",
                                  yes = 1,
                                  no = ifelse(x == "hitrate2",
                                  yes = 2,
                                  no = ifelse(x == "hitrate2V",
                                              yes = 2,
                                              no = NA))))

data %<>% select(-x, -measure)

# Save outputs

write_rds(x = data, 
          path = './data/fra-vision.rds')
write_csv(x = data,
          path = './data/fra-vision.csv')
