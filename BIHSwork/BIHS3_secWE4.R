#### this is a code for section H

library(tidyverse)
library(haven)

## reading original data file

BIHS3_hhsecwe4 <- read_dta("../BIHSRound3/Female/132_bihs_r3_female_weai_ind_mod_we4.dta")
names(BIHS3_hhsecwe4)
str(BIHS3_hhsecwe4)


BIHS3_hhsecwe4 <- 
  BIHS3_hhsecwe4 %>% mutate(hhid = a01) %>%
  mutate(agrcultrl_fish_lstock_group = sjlabelled::as_label(we4_07a)) %>%
  mutate(agrcultrl_fish_lstock_groupmem = sjlabelled::as_label(we4_08a)) %>%
  mutate(water_group = sjlabelled::as_label(we4_07b)) %>%
  mutate(water_groupmem = sjlabelled::as_label(we4_08b)) %>%
  mutate(credit_group = sjlabelled::as_label(we4_07d)) %>%
  mutate(credit_groupmem = sjlabelled::as_label(we4_08d)) %>%
  mutate(insurance_group = sjlabelled::as_label(we4_07e)) %>%
  mutate(insurance_groupmem = sjlabelled::as_label(we4_08e)) %>%
  mutate(trade_group = sjlabelled::as_label(we4_07f)) %>%
  mutate(trade_groupmem = sjlabelled::as_label(we4_08f)) %>%

  