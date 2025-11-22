#### age class structure

library(tidyverse)
library(haven)

## reading original data file

BIHS3_hhsecB1 <- read_dta("../BIHSRound3/Male/010_bihs_r3_male_mod_b1.dta")
names(BIHS3_hhsecB1)
str(BIHS3_hhsecB1)

### b1_01: gender
### b1_02: age

### what is flagb1?

BIHS3_hhsecB1 %>% 
  count(flag_b1)
# -> Current member only


BIHS3_hhsecB1_ageclass <-
BIHS3_hhsecB1 %>% 
  mutate(hhid = a01,
         gender = sjlabelled::as_label(b1_01),
         age = b1_02) %>%
  filter(flag_b1 == 1) %>% 
  select(hhid, age, gender)

BIHS3_hhsecB1_ageclass

BIHS3_familystructure <-
  BIHS3_hhsecB1_ageclass %>% 
  mutate(ageclass = case_when(age < 6 ~ "0005",
                              age %in% c(6:14) ~ "0614",
                              age %in% c(15:64) ~ "1564",
                              age >= 65 ~ "65ov")) %>% 
  group_by(hhid, gender, ageclass) %>% 
  count() %>% arrange(ageclass, gender) %>% 
  pivot_wider(id_cols = hhid,
              names_from = c(gender, ageclass),
              values_from = n,
              values_fill = 0) %>% arrange(hhid) 

BIHS3_hhsize <-
  BIHS3_hhsecB1_ageclass %>% 
  count(hhid) %>% 
  rename(hhsize = n)

BIHS3_hhsize <-
  left_join(BIHS3_hhsize, BIHS3_familystructure, by="hhid")

write_dta(BIHS3_hhsize, "BIHS3_hhsize.dta")

