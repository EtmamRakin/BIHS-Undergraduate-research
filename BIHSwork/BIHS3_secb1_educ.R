#### this is a code for household basic information

library(tidyverse)
library(haven)
library(dplyr)
## reading original data file

BIHS3_hhsecB1 <- read_dta("../BIHSRound3/Male/010_bihs_r3_male_mod_b1.dta")
names(BIHS3_hhsecB1)
str(BIHS3_hhsecB1)


BIHS3_hhsecB1 <- 
  BIHS3_hhsecB1 %>% mutate(hhid = a01,
                           hh_plotno =mid) %>%
  mutate(mem_gen = sjlabelled::as_label(b1_01)) %>%
  mutate(mem_age = sjlabelled::as_label(b1_02)) %>%
  mutate(mem_rltion = sjlabelled::as_label(b1_03)) %>%
  mutate(mem_mrgsttus = sjlabelled::as_label(b1_04)) %>%
  mutate(mem_litrcy = sjlabelled::as_label(b1_07)) %>%
  mutate(mem_edu = sjlabelled::as_label(b1_08)) %>%
  mutate(mem_curr_ocup = sjlabelled::as_label(b1_10)) %>%
  mutate(mem_lction = sjlabelled::as_label(b1_11)) %>%
  mutate(mem_earn_srce1 = sjlabelled::as_label(b1_13a)) %>%
  mutate(mem_earn_srce2 = sjlabelled::as_label(b1_13b)) 

BIHS3_hhsecB1 %>% filter(as.numeric(mem_rltion) == 1) %>% count(mem_litrcy, mem_edu) %>% 
  spread(mem_litrcy, n) %>% 
  print(n=Inf)
### b1_01: gender
### b1_02: age

### what is flagb1?

BIHS3_hhsecB1 %>% 
  count(flag_b1)
# -> Current member only

### detain in mem_stat
BIHS3_hhsecB1 %>% 
  count(flag_b1, mem_stat)


### relation
BIHS3_hhsecB1 %>% 
  count(b1_03, b1_01) %>% 
  spread(b1_01, n)


### b1_07: literate
BIHS3_hhsecB1 %>% 
  count(b1_07)


BIHS3_hhsecB1 <- BIHS3_hhsecB1 %>%
  mutate(literacy_status = case_when(
    b1_07 %in% c(1, 2) ~ "Illiterate",
    b1_07 %in% c(3, 4) ~ "Literate",
    TRUE ~ NA_character_
  ))

# To verify the changes
BIHS3_hhsecB1 %>%
  count(literacy_status, b1_07)

# Extract unique hhid and their corresponding literacy_status
unique_literacy_status <- BIHS3_hhsecB1 %>%
  filter(as.numeric(mem_rltion) == 1) %>% 
  ungroup()


unique_literacy_status %>% 
  count(literacy_status)
### b1_08: education
BIHS3_hhsecB1 %>% 
  count(b1_08) %>% print(n = Inf)  ## Many levels

BIHS3_hhsecB1 <- 
BIHS3_hhsecB1 %>% 
  mutate(educ_level = sjlabelled::as_label(b1_08)) 

#Education catagory
library(readxl)
educ_level <- read_xlsx("new_educ_level.xlsx")
educ_level <- 
  educ_level %>% 
  mutate(new_educ_level=factor(new_educ_level, levels = c("No education" , "primary"  , "highschool" , "higher sec school", "higher education" )))      
BIHS3_hhsecB1 <- 
  left_join(BIHS3_hhsecB1, 
            educ_level,
            by="educ_level")

names(BIHS3_hhsecB1)
BIHS3_hhsecB1 <- 
  BIHS3_hhsecB1 %>% select(hhid, mem_gen, mem_age, new_educ_level, mem_rltion)
str(BIHS3_hhsecB1)

BIHS3_hhead_educ <-
  BIHS3_hhsecB1 %>% 
  filter(as.numeric(mem_rltion)==1) %>% 
  rename(hh_educ = new_educ_level, 
         hh_gen = mem_gen,
         hh_age = mem_age) %>% 
  mutate(fhh=as.numeric(hh_gen)-1) %>% 
  select(hhid, hh_age, hh_gen, fhh, hh_educ)

BIHS3_hhead_educ %>% 
  count(hh_educ, fhh)

BIHS3_male_educ <- 
  BIHS3_hhsecB1 %>% 
  group_by(hhid, mem_gen) %>% 
  mutate(max_educ = max(as.numeric(new_educ_level))) %>% 
  filter(as.numeric(new_educ_level)==max_educ) %>% 
  select(hhid, mem_gen, new_educ_level) %>% 
  unique() %>% filter(mem_gen=="Male") %>% ungroup() %>% 
  select(hhid, new_educ_level) %>% rename(Male_mx_edlvl=new_educ_level)
         
BIHS3_female_educ <- 
  BIHS3_hhsecB1 %>% 
  group_by(hhid, mem_gen) %>% 
  mutate(max_educ = max(as.numeric(new_educ_level))) %>% 
  filter(as.numeric(new_educ_level)==max_educ) %>% 
  select(hhid, mem_gen, new_educ_level) %>% 
  unique() %>% filter(mem_gen=="Female") %>% ungroup() %>% 
  select(hhid, new_educ_level) %>% rename(Female_mx_edlvl=new_educ_level)

BIHS3_hheduc <-
  left_join(BIHS3_hhead_educ, BIHS3_male_educ, by="hhid")
BIHS3_hheduc <-
  left_join(BIHS3_hheduc, BIHS3_female_educ, by="hhid")
BIHS3_hheduc <- 
  left_join(BIHS3_hheduc, unique_literacy_status, by = "hhid")

# Verify the final structure and contents of the dataframe
str(BIHS3_hheduc)
BIHS3_hheduc %>%
  count(literacy_status)

BIHS3_hheduc %>%
  count(literacy_status, hh_educ)

write_dta(BIHS3_hheduc, "BIHS3_hheduc.dta")

