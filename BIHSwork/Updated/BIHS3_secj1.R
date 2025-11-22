#### this is a code for section H

library(tidyverse)
library(haven)

## reading original data file

BIHS3_hhsecJ1 <- read_dta("../BIHSRound3/Male/038_bihs_r3_male_mod_j1.dta")
names(BIHS3_hhsecJ1)
str(BIHS3_hhsecJ1)

str(BIHS3_hhsecJ1$j1_03a)



BIHS3_hhsecJ1 <- 
  BIHS3_hhsecJ1 %>% mutate(hhid = a01,
                           hh_plotno =j1) %>%
  mutate(aes_vst = sjlabelled::as_label(j1_01)) %>%
  mutate(aes_vst_countgov = sjlabelled::as_label(j1_02a)) %>%
  mutate(aes_vst_countngo = sjlabelled::as_label(j1_02b1)) %>%
  mutate(aes_vst_countoth = sjlabelled::as_label(j1_02c)) %>%
  mutate(aes_advce_fert = sjlabelled::as_label(j1_03a)) %>%
  mutate(org_advce_fert = sjlabelled::as_label(j1_06a)) %>%
  mutate(scl_advce_fert = sjlabelled::as_label(j1_09a)) %>%
  mutate(aes_advce_seed = sjlabelled::as_label(j1_03c)) %>%
  mutate(org_advce_seed = sjlabelled::as_label(j1_06c)) %>%
  mutate(scl_advce_seed = sjlabelled::as_label(j1_09c)) %>%
  mutate(aes_advce_irri = sjlabelled::as_label(j1_03e)) %>%
  mutate(org_advce_irri = sjlabelled::as_label(j1_06e)) %>%
  mutate(scl_advce_irri = sjlabelled::as_label(j1_09e)) %>%
  mutate(aes_advce_pestcde = sjlabelled::as_label(j1_03g)) %>%
  mutate(org_advce_pestcde = sjlabelled::as_label(j1_06g)) %>%
  mutate(scl_advce_pestcde = sjlabelled::as_label(j1_09g)) %>%
  mutate(aes_advce_pestdisease = sjlabelled::as_label(j1_03i)) %>%
  mutate(org_advce_pestdisease = sjlabelled::as_label(j1_06i)) %>%
  mutate(scl_advce_pestdisease = sjlabelled::as_label(j1_09i)) %>%
  mutate(aes_advce_crpngprac = sjlabelled::as_label(j1_03k)) %>%
  mutate(org_advce_crpngprac = sjlabelled::as_label(j1_06k)) %>%
  mutate(scl_advce_crpngprac = sjlabelled::as_label(j1_09k)) %>%
  mutate(aes_advce_soiltyp = sjlabelled::as_label(j1_03m)) %>% 
  mutate(org_advce_soiltyp = sjlabelled::as_label(j1_06m)) %>% 
  mutate(scl_advce_soiltyp = sjlabelled::as_label(j1_09m))

BIHS3_hhsecJ1  <- 
  BIHS3_hhsecJ1 %>% 
  mutate(advice_fert = case_when(aes_advce_fert == "Yes" |
                                  org_advce_fert == "Yes"|
                                  scl_advce_fert == "Yes" ~ 1,
                                TRUE ~ 0))


BIHS3_hhsecJ1 %>% 
  count(aes_advce_fert, org_advce_fert, scl_advce_fert, advice_fert) %>% 
  print(n = Inf)

BIHS3_hhsecJ1  <- 
  BIHS3_hhsecJ1 %>% 
  mutate(advice_seed = case_when(aes_advce_seed == "Yes" |
                                  org_advce_seed == "Yes"|
                                  scl_advce_seed == "Yes" ~ 1,
                                TRUE ~ 0))


BIHS3_hhsecJ1 %>% 
  count(aes_advce_seed, org_advce_seed, scl_advce_seed, advice_seed) %>% 
  print(n = Inf)


BIHS3_hhsecJ1  <- 
  BIHS3_hhsecJ1 %>% 
  mutate(advice_irri = case_when(aes_advce_irri == "Yes" |
                                  org_advce_irri == "Yes"|
                                  scl_advce_irri == "Yes" ~ 1,
                                TRUE ~ 0))


BIHS3_hhsecJ1 %>% 
  count(aes_advce_irri, org_advce_irri, scl_advce_irri, advice_irri) %>% 
  print(n = Inf)

BIHS3_hhsecJ1  <- 
  BIHS3_hhsecJ1 %>% 
  mutate(advice_pestcde = case_when(aes_advce_pestcde == "Yes" |
                                         org_advce_pestcde == "Yes"|
                                         scl_advce_pestcde == "Yes" ~ 1,
                                       TRUE ~ 0))


BIHS3_hhsecJ1 %>% 
  count(aes_advce_pestcde, org_advce_pestcde, scl_advce_pestcde, advice_pestcde) %>% 
  print(n = Inf)


###
BIHS3_hhsecJ1  <- 
  BIHS3_hhsecJ1 %>% 
  mutate(advice_pestdisease = case_when(aes_advce_pestdisease == "Yes" |
                                  org_advce_pestdisease == "Yes"|
                                  scl_advce_pestdisease == "Yes" ~ 1,
                                TRUE ~ 0))


BIHS3_hhsecJ1 %>% 
  count(aes_advce_pestdisease, org_advce_pestdisease, scl_advce_pestdisease, advice_pestdisease) %>% 
  print(n = Inf)

###
BIHS3_hhsecJ1  <- 
  BIHS3_hhsecJ1 %>% 
  mutate(advice_crpngprac = case_when(aes_advce_crpngprac == "Yes" |
                                  org_advce_crpngprac == "Yes"|
                                  scl_advce_crpngprac == "Yes" ~ 1,
                                TRUE ~ 0))


BIHS3_hhsecJ1 %>% 
  count(aes_advce_crpngprac, org_advce_crpngprac, scl_advce_crpngprac, advice_crpngprac) %>% 
  print(n = Inf)

###
BIHS3_hhsecJ1  <- 
  BIHS3_hhsecJ1 %>% 
  mutate(advice_soiltyp = case_when(aes_advce_soiltyp == "Yes" |
                                  org_advce_soiltyp == "Yes"|
                                  scl_advce_soiltyp == "Yes" ~ 1,
                                TRUE ~ 0))


BIHS3_hhsecJ1 %>% 
  count(aes_advce_soiltyp, org_advce_soiltyp, scl_advce_soiltyp, advice_soiltyp) %>% 
  print(n = Inf)

BIHS3_hhsecJ1 <-
  BIHS3_hhsecJ1 %>% 
  select(hhid,contains("advice"))

write_dta(BIHS3_hhsecJ1, "BIHS3_hhsecJ1.dta")
       
