#### this is a code for section H

library(tidyverse)
library(haven)

## reading original data file
BIHS3_hhsecwe4_male <- read_dta("../BIHSRound3/Male/084_bihs_r3_male_weai_ind_mod_we4.dta")
str(BIHS3_hhsecwe4_male)


BIHS3_hhsecwe4_male <- 
  BIHS3_hhsecwe4_male %>% mutate(hhid = a01) %>%
  #  mutate(agrcultrl_fish_lstock_group =m sjlabelled::as_label(we4_07a)) %>%
  mutate(male_agrcultrl_fish_lstock_groupmem = sjlabelled::as_label(we4_08a)) %>%
  #  mutate(water_group = sjlabelled::as_label(we4_07b)) %>%
  mutate(male_water_groupmem = sjlabelled::as_label(we4_08b)) %>%
  #  mutate(credit_group = sjlabelled::as_label(we4_07d)) %>%
  mutate(male_credit_groupmem = sjlabelled::as_label(we4_08d)) %>%
  #  mutate(insurance_group = sjlabelled::as_label(we4_07e)) %>%
  mutate(male_insurance_groupmem = sjlabelled::as_label(we4_08e)) %>%
  #  mutate(trade_group = sjlabelled::as_label(we4_07f)) %>%
  mutate(male_trade_groupmem = sjlabelled::as_label(we4_08f)) %>% 
  select(hhid, contains("groupmem"))

BIHS3_hhsecwe4_male


BIHS3_hhsecwe4_male %>% 
  count(male_agrcultrl_fish_lstock_groupmem)

BIHS3_hhsecwe4_male %>% 
  count(male_water_groupmem)

BIHS3_hhsecwe4_male %>% 
  count(male_credit_groupmem)



BIHS3_hhsecwe4_fem <- read_dta("../BIHSRound3/Female/132_bihs_r3_female_weai_ind_mod_we4.dta")
names(BIHS3_hhsecwe4_fem)
str(BIHS3_hhsecwe4_fem)


BIHS3_hhsecwe4_fem <- 
  BIHS3_hhsecwe4_fem %>% mutate(hhid = a01) %>%
  #  mutate(agrcultrl_fish_lstock_group = sjlabelled::as_label(we4_07a)) %>%
  mutate(fem_agrcultrl_fish_lstock_groupmem = sjlabelled::as_label(we4_08a)) %>%
  #  mutate(water_group = sjlabelled::as_label(we4_07b)) %>%
  mutate(fem_water_groupmem = sjlabelled::as_label(we4_08b)) %>%
  #  mutate(credit_group = sjlabelled::as_label(we4_07d)) %>%
  mutate(fem_credit_groupmem = sjlabelled::as_label(we4_08d)) %>%
  #  mutate(insurance_group = sjlabelled::as_label(we4_07e)) %>%
  mutate(fem_insurance_groupmem = sjlabelled::as_label(we4_08e)) %>%
  #  mutate(trade_group = sjlabelled::as_label(we4_07f)) %>%
  mutate(fem_trade_groupmem = sjlabelled::as_label(we4_08f)) %>% 
  select(hhid, contains("groupmem"))


BIHS3_hhsecwe4_fem %>% 
  count(fem_credit_groupmem)

BIHS3_hhsecwe4_fem %>% 
  count(fem_agrcultrl_fish_lstock_groupmem)


BIHS3_hhsecwe4 <-
  full_join(BIHS3_hhsecwe4_male,
            BIHS3_hhsecwe4_fem,
            by = "hhid")

BIHS3_hhsecwe4  <-
  BIHS3_hhsecwe4 %>% 
  mutate(agrcultrl_fish_lstock_groupmem = case_when(male_agrcultrl_fish_lstock_groupmem == "Yes"|
                                                      fem_agrcultrl_fish_lstock_groupmem == "Yes" ~ 1,
                                                    TRUE ~ 0))
BIHS3_hhsecwe4  <-
  BIHS3_hhsecwe4 %>% 
  mutate(water_groupmem = case_when(male_water_groupmem == "Yes"|
                                                      fem_water_groupmem == "Yes" ~ 1,
                                                    TRUE ~ 0))
BIHS3_hhsecwe4  <-
  BIHS3_hhsecwe4 %>% 
  mutate(credit_groupmem = case_when(male_credit_groupmem == "Yes"|
                                                      fem_credit_groupmem == "Yes" ~ 1,
                                                    TRUE ~ 0))
BIHS3_hhsecwe4  <-
  BIHS3_hhsecwe4 %>% 
  mutate(insurance_groupmem = case_when(male_insurance_groupmem == "Yes"|
                                                      fem_insurance_groupmem == "Yes" ~ 1,
                                                    TRUE ~ 0))
BIHS3_hhsecwe4  <-
  BIHS3_hhsecwe4 %>% 
  mutate(trade_groupmem = case_when(male_trade_groupmem == "Yes"|
                                                      fem_trade_groupmem == "Yes" ~ 1,
                                                    TRUE ~ 0))

BIHS3_hhsecwe4 %>% 
  count(male_agrcultrl_fish_lstock_groupmem,
        fem_agrcultrl_fish_lstock_groupmem,
        agrcultrl_fish_lstock_groupmem) %>% 
  spread(agrcultrl_fish_lstock_groupmem, n) %>% 
  arrange(male_agrcultrl_fish_lstock_groupmem, fem_agrcultrl_fish_lstock_groupmem)

BIHS3_hhsecwe4 %>% 
  count(male_water_groupmem,
        fem_water_groupmem,
        water_groupmem) %>% 
  spread(water_groupmem, n) %>% 
  arrange(male_water_groupmem, fem_water_groupmem)

BIHS3_hhsecwe4 %>% 
  count(male_credit_groupmem,
        fem_credit_groupmem,
        credit_groupmem) %>% 
  spread(credit_groupmem, n) %>% 
  arrange(male_credit_groupmem, fem_credit_groupmem)

BIHS3_hhsecwe4 %>% 
  count(male_insurance_groupmem,
        fem_insurance_groupmem,
        insurance_groupmem) %>% 
  spread(insurance_groupmem, n) %>% 
  arrange(male_insurance_groupmem, fem_insurance_groupmem)

BIHS3_hhsecwe4 %>% 
  count(male_trade_groupmem,
        fem_trade_groupmem,
        trade_groupmem) %>% 
  spread(trade_groupmem, n) %>% 
  arrange(male_trade_groupmem, fem_trade_groupmem)
  

names(BIHS3_hhsecwe4)

BIHS3_hhsecwe4 <-
  BIHS3_hhsecwe4 %>% 
  select(!contains("male_")) %>% 
  select(!contains("fem_"))

names(BIHS3_hhsecwe4)

write_dta(BIHS3_hhsecwe4, "BIHS3_hhsecwe4.dta")
