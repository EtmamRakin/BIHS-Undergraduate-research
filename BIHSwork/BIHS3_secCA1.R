#### this is a code for section community module CA part 1

library(tidyverse)
library(haven)

## reading original data file

BIHS3_hhsecCA_1 <- read_dta("../BIHSRound3/Community/140_r3_com_mod_ca_1.dta")
names(BIHS3_hhsecCA_1)
str(BIHS3_hhsecCA_1)

BIHS3_hhsecCA_1 <- 
  BIHS3_hhsecCA_1 %>% mutate(hhid = community_id) %>%
  mutate(division = sjlabelled::as_label(div_name)) %>%
  mutate(district = sjlabelled::as_label(district)) %>%
  mutate(upazila = sjlabelled::as_label(upazila)) %>%
  mutate(union = sjlabelled::as_label(union)) %>%
  mutate(village = sjlabelled::as_label(village)) %>%
  mutate(vill_ppltion = sjlabelled::as_label(ca02)) %>%
  mutate(hh_num_vill = sjlabelled::as_label(ca03)) 

