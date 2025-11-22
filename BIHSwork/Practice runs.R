##Beginning
library(tidyverse)
library(haven)

#reading dta file

BIHS3_hhsecA <- read_dta("../BIHSRound3/Male/009_bihs_r3_male_mod_a.dta")
names(BIHS3_hhsecA)
str(BIHS3_hhsecA)


read.csv("../BIHSwork/educ_level_orignal.csv")


PracticBM <- read.csv("../BIHSwork/educ_level_orignal.csv")
names(PracticBM)
str(PracticBM)
view(PracticBM)

PracticBM <- 
  PracticBM %>% 
  mutate(education_of_BD = educ_level) %>% 
  mutate(education_of_BD = )

