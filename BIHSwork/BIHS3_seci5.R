#### this is a code for section I

library(tidyverse)
library(haven)

## reading original data file

BIHS3_hhsecI5 <- read_dta("../BIHSRound3/Male/037_bihs_r3_male_mod_i5.dta")
names(BIHS3_hhsecI5)
str(BIHS3_hhsecI5)


BIHS3_hhsecI5 <- 
  BIHS3_hhsecI5 %>% mutate(hhid = a01) %>%
  mutate(tech_list = sjlabelled::as_label(i5_01)) %>% #how to specify which ones
  mutate(tech_list_oth = sjlabelled::as_label(i5_01oth)) %>%
  mutate(tech_use = sjlabelled::as_label(i5_02)) %>%
  mutate(tech_used_rent_or_own = sjlabelled::as_label(i5_18)) %>%
  mutate(tech_used_past = sjlabelled::as_label(i5_03))
  
  




BIHS3_hhsecI5 %>% 
  count(tech_list) %>% 
  print(n = Inf)

BIHS3_hhsecI5 %>% 
  count(tech_list_oth) %>% 
  print(n = Inf)

BIHS3_hhsecI5 %>% 
  count(BIHS3_hhsecI5$tech_list, as.numeric(BIHS3_hhsecI5$tech_list)) %>% 
  print(n = Inf)

#### type of techology, and past use dummy for types

BIHS3_hhsecI5_pastuse_type <- 
  BIHS3_hhsecI5 %>% 
  mutate(tech_type = case_when(as.numeric(tech_list)  == 1 ~ "GutiUrea",
                               as.numeric(tech_list) %in% c(2:21) ~  "BRRIdhan",
                               as.numeric(tech_list) %in% c(22:25) ~  "Bina", 
                               as.numeric(tech_list) %in% c(26:27) ~  "Tractor",
                               as.numeric(tech_list) %in% c(28:31) ~  "Pump",
                               as.numeric(tech_list) %in% c(33,34,43,44) ~  "Thresher",
                               as.numeric(tech_list) %in% c(35,36,42) ~  "Planting",
                               as.numeric(tech_list) %in% c(37:38) ~  "Sprayer",
                               TRUE ~ NA_character_)) %>% 
  mutate(tech_use_past_dum = 2-as.numeric(tech_used_past)) %>% 
  select(hhid, tech_type, tech_use_past_dum )  
  

BIHS3_hhsecI5_pastuse_type <- 
  BIHS3_hhsecI5_pastuse_type %>% 
  group_by(hhid, tech_type) %>% 
  summarise(tech_use_past_dum = sum(tech_use_past_dum)) %>% 
  mutate(tech_use_past_dum = case_when(tech_use_past_dum >=1 ~ 1,
                                      tech_use_past_dum == 0 ~ 0))

### long
BIHS3_hhsecI5_pastuse_type

### wide
BIHS3_hhsecI5_pastuse_type <- 
BIHS3_hhsecI5_pastuse_type %>% 
  pivot_wider(id_cols = hhid,
              names_from = tech_type,
              values_from = tech_use_past_dum,
              values_fill = 0)

BIHS3_hhsecI5_pastuse_type

#### final data is save by dta format.

write_dta(BIHS3_hhsecI5_pastuse_type, "BIHS3_hhsecI5_pastuse_type.dta")        

