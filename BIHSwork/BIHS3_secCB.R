#### this is a code for section community module CB

library(tidyverse)
library(haven)

## reading original data file

BIHS3_hhsecCB <- read_dta("../BIHSRound3/Community/142_r3_com_mod_cb_1.dta")
names(BIHS3_hhsecCB)
str(BIHS3_hhsecCB)

BIHS3_hhsecCB <- 
  BIHS3_hhsecCB %>% mutate(hhid = community_id) %>%
  mutate(facilities = sjlabelled::as_label(cb01)) %>% #how to choose only certain ones
  mutate(facilities_avail = sjlabelled::as_label(cb03)) %>% #how to merge the two
  mutate(facilities_avail_num = sjlabelled::as_label(cb04))

