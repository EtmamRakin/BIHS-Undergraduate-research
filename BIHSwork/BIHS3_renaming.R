#### this is a code for household basic information

library(tidyverse)
library(haven)

## reading original data file

BIHS3_hhsecA <- read_dta("../BIHSRound3/Male/009_bihs_r3_male_mod_a.dta")
names(BIHS3_hhsecA)
str(BIHS3_hhsecA)

BIHS3_hhsecA <- 
  BIHS3_hhsecA %>% mutate(hhid = a01) %>%
  mutate(hh_religion = sjlabelled::as_label(a13)) %>%
  mutate(mem_count = a23) %>%
  mutate(ethnicity = a15) %>%
  mutate(ethnicity = sjlabelled::as_label(a15)) %>%
  mutate(hh_dec_maker = a26) %>%
  mutate(hh_dec_maker = sjlabelled::as_label(a26)) %>%
  mutate(prim_lang = a14) %>%
  mutate(prim_lang = sjlabelled::as_label(a14))




