library(haven)
library(tidyverse)
library(sandwich)
library(estimatr)
library(lmtest)
library(censReg)
library(survey)
library(oglmx)
library(gt)
library(gtsummary)
library(writexl)

#### reading main data
source("BIHS3_dfest_main.R")

BIHS3_dfest <-
  read_dta("BIHS3_dfest.dta")

### check
names(BIHS3_dfest)


#### rice farmers only
BIHS3_dfest_rice <- 
  BIHS3_dfest %>% 
  filter(!is.na(Adopt_Hybrid)) %>% 
  filter(!is.na(Adopt_HYV)) %>% 
  filter(!is.na(Adopt_Local)) 

### Adoption rate
BIHS3_dfest_rice %>%
  select(contains("Adopt")) %>% 
  tbl_summary()


### Adoption Farmers
BIHS3_dfest_rice %>%
  select(contains("Adopt") , "div_name") %>% 
  tbl_summary(by=div_name)


BIHS3_dfest_rice %>%
  select(contains("Aman")) %>% 
  select(contains("Boro")) %>% 
  select(!contains("_Aman")) %>% 
  select(!contains("dummy")) %>%
  select(!contains("_Boro")) %>% 
  tbl_summary()

BIHS3_dfest_rice %>%
  select(contains("Boro")) %>% 
  select(!contains("_Boro")) %>% 
  tbl_summary()


BIHS3_dfest_rice %>%
  select(Aman_HYV, Aman_Local, Aman_Hybrid,  div_name) %>% 
  tbl_summary(by = "div_name")




BIHS3_dfest_rice %>% 
  select(-hhid) %>% 
  tbl_summary(
    by=div_name,
    label = list(fhh = "Female headed houshold",
                 hh_age = "Household head age",
                 hh_gen = "Household head gen",
                 totalasset = "Total household asset",
                 hhsize = "Household size",
                 div = "Division",
                 BRRIdhan = "Experienced cultivating BRRIdhan (HYV)",
                 Bina = "Experienced cultivating Bina (Hybrid)",
                 hh_educ = "Household education",
                 Male_mx_edlvl = "Male member highest education level",
                 Female_mx_edlvl = "Female member highest education level",
                 Machine_assetvalue = "Machine asset value",
                 area_Aman_Hybrid = "Aman season hybrid area",
                 area_Aman_HYV = "Aman season HYV area",
                 area_Aman_Local = "Aman season local area",
                 area_Boro_Hybrid = "Boro season hybrid area",
                 area_Boro_HYV = "Boro season HYV area",
                 area_Boro_Local = "Boro season local area",
                 advice_fert = "Advice received on Fertilizer",
                 advice_seed = "Advice received on Seed",
                 advice_irri = "Advice received on Irrigation",
                 advice_pestcde = "Advice received on Pesticide",
                 advice_pestdisease = "Advice received on Pest&Diseases",
                 advice_crpngprac = "Advice received on Cropping practice",
                 advice_soiltyp = "Advice received on Soiltype",
                 agrcultrl_fish_lstock_groupmem = "Agricultural/Fish/Livestock groupmember",
                 water_groupmem = "Water groupmember",
                 credit_groupmem = "Credit groupmember",
                 insurance_groupmem = "Insurance groupmember",
                 trade_groupmem = "Trade groupmember",
                 irr_ratio_Boro = "Irrigation ratio in Boro season",
                 irr_ratio_Aman = "Irrigation ratio in Aman season",
                 irr_ratio_Aus = "Irrgation ratio in Aus season"), #past experience include
    statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  as_gt() %>% 
  tab_footnote(footnote = "BIHS3")


names(BIHS3_dfest_rice)
install.packages("writexl")

library(writexl)


