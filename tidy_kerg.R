setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rm(list =ls())

library(tidyverse)
library(zoo)

# Wahldaten 
# file source: https://www.bundeswahlleiter.de/dam/jcr/ce2d2b6a-f211-4355-8eea-355c98cd4e47/btw_kerg.zip

# example for 2017 should work with any file
raw <- 'btw2017_kerg.csv'

# Prepare column names
cols <- read_delim(raw, delim = ";", na = "", skip = 5, col_names = F) %>%
  slice(1:3) %>% t() %>% as_tibble() %>% zoo::na.locf(na.rm = F) %>%
  transmute(col = paste(V1,V2,V3, sep = "_")) %>%
  mutate(
    col = as.character(col),
    col = case_when(col == "Nr_NA_NA" ~ "id",
                         col == "Gebiet_NA_NA" ~ "region",
                         col == "gehört zu_NA_NA" ~ "land",
                         T ~ col),
    col = gsub("[[:blank:]]", "", col),
    col = gsub("ä", "ae", col),
    col = gsub("ü", "ue", col),
    col = gsub("ö", "oe", col),
    col = gsub(",", ".", col,),
    col = gsub("-", ".", col,),
    col = tolower(col)) %>% 
  remove_rownames() %>%
  as_vector()

# add column names
d <- read_delim(raw, delim = ";", na = "", skip = 8, col_names = F)
colnames(d) <- cols # the dplyr with replace_with did not work for me. I am happy to hear suggestions 


tidy_kerg17 <- d %>%
  pivot_longer(cols = -c(id, region, land), names_to = "var", values_to = "votes") %>%
  separate(var, sep = "_", into = c("group", "vote_type", "count_type")) %>%
  mutate(land_str = recode(land, `1` = 'Schleswig-Holstein',
                       `2` = 'Hamburg',
                       `3` = 'Niedersachsen',
                       `4` = 'Bremen',
                       `5` = 'Nordrhein-Westfalen',
                       `6` = 'Hessen',
                       `7` = 'Rheinland-Pfalz',
                       `8` = 'Baden-Württemberg',
                       `9` = 'Bayern',
                       `10` = 'Saarland',
                       `11` = 'Berlin',
                       `12` = 'Brandenburg',
                       `13` = 'Mecklenburg-Vorpommern',
                       `14` = 'Sachsen',
                       `15` = 'Sachsen-Anhalt',
                       `16` = 'Thüringen')) %>%
  na.omit()


saveRDS(tidy_kerg17, file = "tidy_kerg17.rds")
write_csv(tidy_kerg17, file = "tidy_kerg17.csv")