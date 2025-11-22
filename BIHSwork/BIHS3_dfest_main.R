library(haven)
library(tidyverse)
library(sandwich)
library(estimatr)
library(lmtest)
library(censReg)

dir(pattern = "dta")

BIHS3_hhsecA <-
  read_dta("BIHS3_hhsecA.dta")
BIHS3_hhsize <-
  read_dta("BIHS3_hhsize.dta")
BIHS3_hheduc <-
  read_dta("BIHS3_hheduc.dta")
BIHS3_hhpaddytypesarea <-
  read_dta("BIHS3_paddytypearea_season_wide.dta")
BIHS3_hhsecd1 <-
  read_dta("BIHS_hhsec_d1.dta")
BIHS3_hhsecd2 <-
  read_dta("BIHS3_hhsecd2")
BIHS3_hhseci5 <-
  read_dta("BIHS3_hhsecI5_pastuse_type.dta")
BIHS3_hhsecj1 <-
  read_dta("BIHS3_hhsecJ1.dta")
BIHS3_hhsecwe4 <-
  read_dta("BIHS3_hhsecwe4.dta")
BIHS3_hhsecG <-
  read_dta("BIHS3_secG.dta")
BIHS3_hhirr <-
  read_dta("BIHS3_hhirr.dta")
BIHS3_sampling <-
  read_dta("../158_BIHS sampling weights_r3.dta")
BIHS3_sampling <-
  BIHS3_sampling %>% rename(hhid = a01)

BIHS3_dfest <-
  left_join(BIHS3_hhsecA, BIHS3_hhsize, by = "hhid")

BIHS3_dfest <-
  left_join(BIHS3_dfest, BIHS3_hheduc, by = "hhid")

BIHS3_dfest <-
  left_join(BIHS3_dfest, BIHS3_hhsecd1, by = "hhid")

BIHS3_dfest <-
  left_join(BIHS3_dfest, BIHS3_hhsecd2, by = "hhid")

BIHS3_dfest <-
  left_join(BIHS3_dfest, BIHS3_hhpaddytypesarea, by = "hhid")

BIHS3_dfest <-
  left_join(BIHS3_dfest, BIHS3_hhseci5, by = "hhid")

BIHS3_dfest <-
  left_join(BIHS3_dfest, BIHS3_hhsecj1, by = "hhid")

BIHS3_dfest <-
  left_join(BIHS3_dfest, BIHS3_hhsecwe4, by = "hhid")

BIHS3_dfest <-
  left_join(BIHS3_dfest, BIHS3_hhsecG, by = "hhid")

BIHS3_dfest <-
  left_join(BIHS3_dfest, BIHS3_hhirr, by = "hhid")

BIHS3_dfest <-
  left_join(BIHS3_dfest, BIHS3_sampling, by = "hhid")

BIHS3_dfest <-
  BIHS3_dfest %>% 
  mutate(aman_Local_dummy = case_when(area_Aman_Local > 0 ~ 1, 
                                      area_Aman_Local == 0 ~ 0,
                                      TRUE ~ NA_real_)) %>% 
  mutate(area_Aman_total = area_Aman_Hybrid + area_Aman_HYV + area_Aman_Local,
         share_Aman_HYV = area_Aman_HYV/area_Aman_total,
         share_Aman_Hybrid = area_Aman_Hybrid/area_Aman_total,
         share_Aman_Local = area_Aman_Local/area_Aman_total,
         area_Boro_total = area_Boro_Hybrid + area_Boro_HYV,
         share_Boro_Hybrid = area_Boro_Hybrid/area_Boro_total,
         share_Boro_HYV = area_Boro_HYV/area_Boro_total) %>% 
  mutate(hh_educ0 = case_when(hh_educ == 1 ~ 1,
                              TRUE ~ 0),
         hh_educp = case_when(hh_educ == 2 ~ 1,
                              TRUE ~ 0),
         hh_educh = case_when(hh_educ > 2 ~ 1,
                              TRUE ~ 0))

names(BIHS3_dfest)
str(BIHS3_dfest)


BIHS3_dfest <-
  BIHS3_dfest %>% mutate(clid = as.numeric(div)*100000000000000 + 
                           district*1000000000000 +
                           upazila*1000000000 +
                           union*1000000 +
                           mouza*1000 +
                           village*1) %>% 
  mutate(hh_educ = as_factor(hh_educ), 
         Female_1564 = as.numeric(Female_1564),
         Male_1564 = as.numeric(Male_1564))


BIHS3_dfest <-
  BIHS3_dfest %>% mutate(Aman_HYV = case_when(area_Aman_HYV > 0 ~ 1, 
                                              area_Aman_HYV == 0 ~ 0,
                                              TRUE ~ NA_real_), 
                         Aman_Hybrid = case_when(area_Aman_Hybrid > 0 ~ 1,
                                                 area_Aman_Hybrid == 0 ~ 0,
                                                 TRUE ~ NA_real_),
                         Aman_Local = case_when(area_Aman_Local > 0 ~ 1,
                                                area_Aman_Local == 0 ~ 0,
                                                TRUE ~ NA_real_))

BIHS3_dfest <-
  BIHS3_dfest %>% mutate(Boro_HYV = case_when(area_Boro_HYV > 0 ~ 1, 
                                              area_Boro_HYV == 0 ~ 0,
                                              TRUE ~ NA_real_), 
                         Boro_Hybrid = case_when(area_Boro_Hybrid > 0 ~ 1,
                                                 area_Boro_Hybrid == 0 ~ 0,
                                                 TRUE ~ NA_real_))

write_dta(BIHS3_dfest, "BIHS3_dfest.dta")

rm(list = ls())
