#### this is a code for section H

library(tidyverse)
library(haven)

## reading original data file

BIHS3_hhsecH4 <- read_dta("../BIHSRound3/Male/024_bihs_r3_male_mod_h4.dta")
names(BIHS3_hhsecH4)
str(BIHS3_hhsecH4)


BIHS3_hhsecH4 <- 
  BIHS3_hhsecH4 %>% mutate(hhid = a01,
                           hh_plotno =h4_sl) %>%
  mutate(crop_1 = sjlabelled::as_label(crop_a_h4)) %>%
  mutate(crop_1oth = sjlabelled::as_label(crop_aoth_h4)) %>%
  mutate(crop_2 = sjlabelled::as_label(crop_b_h4)) %>%
  mutate(crop_2oth = sjlabelled::as_label(crop_both_h4)) %>%
  mutate(cost_landprep_animal = sjlabelled::as_label(h4_01)) %>%
  mutate(tls_lndprep_yn = sjlabelled::as_label(h4_03n)) %>%
  mutate(tls_lndprep_tillr = sjlabelled::as_label(h4_03_1)) %>%
  mutate(tls_lndprep_tillr_rentcost = sjlabelled::as_label(h4_04_1)) %>%
  mutate(tls_lndprep_tillr_fuelcost = sjlabelled::as_label(h4_05_1)) %>%
  mutate(tls_lndprep_trctr = sjlabelled::as_label(h4_03_2)) %>%
  mutate(tls_lndprep_trctr_rentcost = sjlabelled::as_label(h4_04_2)) %>%
  mutate(tls_lndprep_trctr_fuelcost = sjlabelled::as_label(h4_05_2)) %>%
  mutate(tls_lndprep_plgh = sjlabelled::as_label(h4_03_3)) %>%
  mutate(tls_lndprep_plgh_rentcost = sjlabelled::as_label(h4_04_3)) %>%
  mutate(tls_planting_yn = sjlabelled::as_label(h4_06n)) %>%
  mutate(tls_planting_cost = sjlabelled::as_label(h4_06)) %>%
  mutate(tls_fertlzer_yn = sjlabelled::as_label(h4_07n)) %>%
  mutate(tls_fertlzer_cost = sjlabelled::as_label(h4_07)) %>%
  mutate(tls_pesticide_yn = sjlabelled::as_label(h4_08n)) %>%
  mutate(tls_pesticide_cost = sjlabelled::as_label(h4_08)) %>%
  mutate(tls_weeding_yn = sjlabelled::as_label(h4_09n)) %>%
  mutate(tls_weeding_cost = sjlabelled::as_label(h4_09)) %>%
  mutate(tls_harvesting_yn = sjlabelled::as_label(h4_10n)) %>%
  mutate(tls_harvesting_cost = sjlabelled::as_label(h4_10))

