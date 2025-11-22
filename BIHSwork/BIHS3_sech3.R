#### this is a code for section H

library(tidyverse)
library(haven)

## reading original data file

BIHS3_hhsecH3 <- read_dta("../BIHSRound3/Male/023_bihs_r3_male_mod_h3.dta")
names(BIHS3_hhsecH3)
str(BIHS3_hhsecH3)


BIHS3_hhsecH3 <- 
  BIHS3_hhsecH3 %>% mutate(hhid = a01,
                           hh_plotno =h3_sl) %>%
  mutate(crop_1 = sjlabelled::as_label(crop_a_h3)) %>%
  mutate(crop_1oth = sjlabelled::as_label(crop_aoth_h3)) %>%
  mutate(crop_2 = sjlabelled::as_label(crop_b_h3)) %>%
  mutate(crop_2oth = sjlabelled::as_label(crop_both_h3)) 
