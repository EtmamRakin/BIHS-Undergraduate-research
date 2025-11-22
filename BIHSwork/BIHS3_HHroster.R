#### this is a code for household basic information

library(tidyverse)
library(haven)

## reading original data file

BIHS3_hhsecA <- read_dta("../BIHSRound3/Male/009_bihs_r3_male_mod_a.dta")
names(BIHS3_hhsecA)
str(BIHS3_hhsecA)

BIHS3_hhsecA <- 
  BIHS3_hhsecA %>% mutate(hhid = a01) %>%
  mutate(hh_religion = sjlabelled::as_label(a13))




### check the observations by divisions

BIHS3_hhsecA %>%
  count(div_name, district) %>% data.frame()

### check the observations by district

BIHS3_hhsecA %>%
  count(district) %>% data.frame()

### check the observations by hh religions

BIHS3_hhsecA %>%
  count(a13, hh_religion) %>% data.frame()

names(BIHS3_hhsecA)
BIHS3_hhsecA <-
  BIHS3_hhsecA %>% select(hhid, dvcode, div, div_name, district, 
                          upazila, union, mouza, village, community_id, hh_religion )
write_dta(BIHS3_hhsecA, "BIHS3_hhsecA.dta")

