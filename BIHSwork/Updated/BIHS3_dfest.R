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
         share_Boro_HYV = area_Boro_HYV/area_Boro_total)

names(BIHS3_dfest)
str(BIHS3_dfest)
library(gtsummary)
library(gt)


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

BIHS3_dfest_temp <-
  BIHS3_dfest %>% select(hhid, fhh, hh_age, hhsize, div_name,
                         totalasset, hh_educ, BRRIdhan, Bina,
                         Female_1564, Male_1564, Female_65ov, Male_65ov,
                         contains("advice"), contains("groupmem"),
                         aman_Local_dummy, contains("area_Aman"), contains("area_Boro")) %>%
  
  filter(!is.na(area_Aman_Hybrid))
BIHS3_dfest_temp %>% 
  select(-hhid) %>% 
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})"))

BIHS3_dfest_temp %>% 
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
                 trade_groupmem = "Trade groupmember"), #past experience include
    statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  as_gt() %>% 
  tab_footnote(footnote = "BIHS3")



BIHS3_dfest_Aman <-
  BIHS3_dfest %>% filter(!is.na(area_Aman_total))


#estimation for Aman
#HYV
est_area_Aman_HYV <-
  lm(share_Aman_HYV ~ log(area_Aman_total/hhsize) + hh_educ + hh_age + log(hhsize) + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + div_name + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
          # BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman)
est_areashare_Aman_HYV <-
  lm(share_Aman_HYV ~ div_name + 
       ownland_ha + 
       fhh + hh_educ + hh_age + 
       hhsize + Female_1564 + Male_1564 + Female_65ov + Male_65ov  + 
       totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman)
est_area_Aman_HYV <-
  lm(area_Aman_HYV ~ div_name + 
       ownland_ha + 
       fhh + hh_educ + hh_age + 
       hhsize + Female_1564 + Male_1564 + Female_65ov + Male_65ov  + 
       totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       irr_ratio_Aman +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman)
summary(est_area_Aman_HYV)
coeftest(est_area_Aman_HYV, vcov. = vcovHC(est_area_Aman_HYV))
coeftest(est_area_Aman_HYV, vcov. = vcovCL(est_area_Aman_HYV, cluster =~ clid )) ###final report
censest_area_Aman_HYV <-
  censReg(share_Aman_HYV ~ log(area_Aman_total) + hh_educ + hh_age + hhsize + div_name + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       # BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman)
summary(censest_area_Aman_HYV)


#Local
est_area_Aman_Local <-
  lm(share_Aman_Local ~ log(area_Aman_total) + hh_educ + hh_age + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + hhsize + div_name + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       # BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman)
summary(est_area_Aman_Local)
coeftest(est_area_Aman_Local, vcov. = vcovHC(est_area_Aman_Local))
coeftest(est_area_Aman_Local, vcov. = vcovCL(est_area_Aman_Local, cluster =~ clid ))
est_area_Aman_Local <-
  lm(area_Aman_Local ~ div_name + 
       ownland_ha + 
       fhh + hh_educ + hh_age + 
       hhsize + Female_1564 + Male_1564 + Female_65ov + Male_65ov  + 
       totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       irr_ratio_Aman +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman)
summary(est_area_Aman_Local)
coeftest(est_area_Aman_Local, vcov. = vcovHC(est_area_Aman_Local))
coeftest(est_area_Aman_Local, vcov. = vcovCL(est_area_Aman_Local, cluster =~ clid )) ###final report
censest_area_Aman_Local <-
  censReg(share_Aman_Local ~ log(area_Aman_total) + hh_educ + hh_age + hhsize + div_name + 
            fhh + totalasset + 
            advice_irri + advice_seed + 
            # BRRIdhan + Bina +
            agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman)
summary(censest_area_Aman_Local)



#Hybrid
est_area_Aman_Hybrid <-
  lm(share_Aman_Hybrid ~ log(area_Aman_total) + hh_educ + hh_age + hhsize + div_name + 
       fhh + totalasset + advice_irri + advice_seed + 
       # BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman)
summary(est_area_Aman_Hybrid)
coeftest(est_area_Aman_Hybrid, vcov. = vcovHC(est_area_Aman_Hybrid))
coeftest(est_area_Aman_Hybrid, vcov. = vcovCL(est_area_Aman_Hybrid, cluster =~ clid ))
est_area_Aman_Hybrid <-
  lm(area_Aman_Hybrid ~ div_name + 
       ownland_ha + 
       fhh + hh_educ + hh_age + 
       hhsize + Female_1564 + Male_1564 + Female_65ov + Male_65ov  + 
       totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       irr_ratio_Aman +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman)
summary(est_area_Aman_Hybrid)
coeftest(est_area_Aman_Hybrid, vcov. = vcovHC(est_area_Aman_Hybrid))
coeftest(est_area_Aman_Hybrid, vcov. = vcovCL(est_area_Aman_Hybrid, cluster =~ clid )) ###final report
cenest_area_Aman_Hybrid <-
  censReg(share_Aman_Hybrid ~ log(area_Aman_total) + hh_educ + hh_age + hhsize + div_name + 
       fhh + totalasset + advice_irri + advice_seed + 
       # BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman)
summary(cenest_area_Aman_Hybrid)



BIHS3_dfest_Boro <-
  BIHS3_dfest %>% filter(!is.na(area_Boro_total))
#estimation of Boro
#HYV
est_area_Boro_HYV <-
  lm(share_Boro_HYV ~ log(area_Boro_total) + hh_educ + hh_age + hhsize + div_name + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       # BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Boro)
summary(est_area_Boro_HYV)
coeftest(est_area_Boro_HYV, vcov. = vcovHC(est_area_Boro_HYV))
coeftest(est_area_Boro_HYV, vcov. = vcovCL(est_area_Boro_HYV, cluster =~ clid ))
est_area_Boro_HYV <-
  lm(area_Boro_HYV ~ div_name + 
       ownland_ha + 
       fhh + hh_educ + hh_age + 
       hhsize + Female_1564 + Male_1564 + Female_65ov + Male_65ov  + 
       totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       irr_ratio_Boro +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman)
summary(est_area_Boro_HYV)
coeftest(est_area_Boro_HYV, vcov. = vcovHC(est_area_Boro_HYV))
coeftest(est_area_Boro_HYV, vcov. = vcovCL(est_area_Boro_HYV, cluster =~ clid ))
censest_area_Boro_HYV <-
  censReg(share_Boro_HYV ~ log(area_Boro_total) + hh_educ + hh_age + hhsize + div_name + 
            fhh + totalasset + 
            advice_irri + advice_seed + 
            # BRRIdhan + Bina +
            agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Boro)
summary(censest_area_Boro_HYV)


#Hybrid
est_area_Boro_Hybrid <-
  lm(share_Boro_Hybrid ~ log(area_Boro_total) + hh_educ + hh_age + hhsize + div_name + 
       fhh + totalasset + advice_irri + advice_seed + 
       # BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Boro)
summary(est_area_Boro_Hybrid)
est_area_Boro_Hybrid <-
  lm(area_Boro_Hybrid ~ div_name + 
       ownland_ha + 
       fhh + hh_educ + hh_age + 
       hhsize + Female_1564 + Male_1564 + Female_65ov + Male_65ov  + 
       totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       irr_ratio_Boro +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman)
summary(est_area_Boro_Hybrid)
coeftest(est_area_Boro_Hybrid, vcov. = vcovHC(est_area_Boro_Hybrid))
coeftest(est_area_Boro_Hybrid, vcov. = vcovCL(est_area_Boro_Hybrid, cluster =~ clid ))
cenest_area_Boro_Hybrid <-
  censReg(share_Boro_Hybrid ~ log(area_Boro_total) + hh_educ + hh_age + hhsize + div_name + 
            fhh + totalasset + advice_irri + advice_seed + 
            # BRRIdhan + Bina +
            agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Boro)
summary(cenest_area_Boro_Hybrid)



##### Aman Division HYV
est_area_Aman_HYV_Barisal <-
  lm(share_Aman_HYV ~ log(area_Aman_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman %>% filter(div_name == "Barisal")) 
summary(est_area_Aman_HYV_Barisal)
coeftest(est_area_Aman_HYV_Barisal, vcov. = vcovHC(est_area_Aman_HYV_Barisal))
coeftest(est_area_Aman_HYV_Barisal, vcov. = vcovCL(est_area_Aman_HYV_Barisal, cluster =~ clid ))

est_area_Aman_HYV_Dhaka <-
  lm(share_Aman_HYV ~ log(area_Aman_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman %>% filter(div_name == "Dhaka")) 
summary(est_area_Aman_HYV_Dhaka)
coeftest(est_area_Aman_HYV_Dhaka, vcov. = vcovHC(est_area_Aman_HYV_Dhaka))
coeftest(est_area_Aman_HYV_Dhaka, vcov. = vcovCL(est_area_Aman_HYV_Dhaka, cluster =~ clid )) ###final report


est_area_Aman_HYV_Chittagong <-
  lm(share_Aman_HYV ~ log(area_Aman_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman %>% filter(div_name == "Chittagong")) 
summary(est_area_Aman_HYV_Chittagong)
coeftest(est_area_Aman_HYV_Chittagong, vcov. = vcovHC(est_area_Aman_HYV_Chittagong))
coeftest(est_area_Aman_HYV_Chittagong, vcov. = vcovCL(est_area_Aman_HYV_Chittagong, cluster =~ clid ))

est_area_Aman_HYV_Khulna <-
  lm(share_Aman_HYV ~ log(area_Aman_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman %>% filter(div_name == "Khulna")) 
summary(est_area_Aman_HYV_Khulna)
coeftest(est_area_Aman_HYV_Khulna, vcov. = vcovHC(est_area_Aman_HYV_Khulna))
coeftest(est_area_Aman_HYV_Khulna, vcov. = vcovCL(est_area_Aman_HYV_Khulna, cluster =~ clid ))

est_area_Aman_HYV_Rajshahi <-
  lm(share_Aman_HYV ~ log(area_Aman_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman %>% filter(div_name == "Rajshahi")) 
summary(est_area_Aman_HYV_Rajshahi)
coeftest(est_area_Aman_HYV_Rajshahi, vcov. = vcovHC(est_area_Aman_HYV_Rajshahi))
coeftest(est_area_Aman_HYV_Rajshahi, vcov. = vcovCL(est_area_Aman_HYV_Rajshahi, cluster =~ clid ))

est_area_Aman_HYV_Rangpur <-
  lm(share_Aman_HYV ~ log(area_Aman_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman %>% filter(div_name == "Rangpur")) 
summary(est_area_Aman_HYV_Rangpur)
coeftest(est_area_Aman_HYV_Rangpur, vcov. = vcovHC(est_area_Aman_HYV_Rangpur))
coeftest(est_area_Aman_HYV_Rangpur, vcov. = vcovCL(est_area_Aman_HYV_Rangpur, cluster =~ clid ))

est_area_Aman_HYV_Sylhet <-
  lm(share_Aman_HYV ~ log(area_Aman_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman %>% filter(div_name == "Sylhet")) 
summary(est_area_Aman_HYV_Sylhet)
coeftest(est_area_Aman_HYV_Sylhet, vcov. = vcovHC(est_area_Aman_HYV_Sylhet))
coeftest(est_area_Aman_HYV_Sylhet, vcov. = vcovCL(est_area_Aman_HYV_Sylhet, cluster =~ clid ))


################################??????????

#####Aman Division Hybrid
est_area_Aman_Hybrid_Barisal <-
  lm(share_Aman_Hybrid ~ log(area_Aman_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman %>% filter(div_name == "Barisal")) 
summary(est_area_Aman_Hybrid_Barisal)
coeftest(est_area_Aman_Hybrid_Barisal, vcov. = vcovHC(est_area_Aman_Hybrid_Barisal))
coeftest(est_area_Aman_Hybrid_Barisal, vcov. = vcovCL(est_area_Aman_Hybrid_Barisal, cluster =~ clid ))

est_area_Aman_Hybrid_Dhaka <-
  lm(share_Aman_Hybrid ~ log(area_Aman_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman %>% filter(div_name == "Dhaka")) 
summary(est_area_Aman_Hybrid_Dhaka)
coeftest(est_area_Aman_Hybrid_Dhaka, vcov. = vcovHC(est_area_Aman_Hybrid_Dhaka))
coeftest(est_area_Aman_Hybrid_Dhaka, vcov. = vcovCL(est_area_Aman_Hybrid_Dhaka, cluster =~ clid )) ###final report


est_area_Aman_Hybrid_Chittagong <-
  lm(share_Aman_Hybrid ~ log(area_Aman_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman %>% filter(div_name == "Chittagong")) 
summary(est_area_Aman_Hybrid_Chittagong)
coeftest(est_area_Aman_Hybrid_Chittagong, vcov. = vcovHC(est_area_Aman_Hybrid_Chittagong))
coeftest(est_area_Aman_Hybrid_Chittagong, vcov. = vcovCL(est_area_Aman_Hybrid_Chittagong, cluster =~ clid ))

est_area_Aman_Hybrid_Khulna <-
  lm(share_Aman_Hybrid ~ log(area_Aman_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman %>% filter(div_name == "Khulna")) 
summary(est_area_Aman_Hybrid_Khulna)
coeftest(est_area_Aman_Hybrid_Khulna, vcov. = vcovHC(est_area_Aman_Hybrid_Khulna))
coeftest(est_area_Aman_Hybrid_Khulna, vcov. = vcovCL(est_area_Aman_Hybrid_Khulna, cluster =~ clid ))

est_area_Aman_Hybrid_Rajshahi <-
  lm(share_Aman_Hybrid ~ log(area_Aman_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman %>% filter(div_name == "Rajshahi")) 
summary(est_area_Aman_Hybrid_Rajshahi)
coeftest(est_area_Aman_Hybrid_Rajshahi, vcov. = vcovHC(est_area_Aman_Hybrid_Rajshahi))
coeftest(est_area_Aman_Hybrid_Rajshahi, vcov. = vcovCL(est_area_Aman_Hybrid_Rajshahi, cluster =~ clid ))

est_area_Aman_Hybrid_Rangpur <-
  lm(share_Aman_Hybrid ~ log(area_Aman_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman %>% filter(div_name == "Rangpur")) 
summary(est_area_Aman_Hybrid_Rangpur)
coeftest(est_area_Aman_Hybrid_Rangpur, vcov. = vcovHC(est_area_Aman_Hybrid_Rangpur))
coeftest(est_area_Aman_Hybrid_Rangpur, vcov. = vcovCL(est_area_Aman_Hybrid_Rangpur, cluster =~ clid ))

est_area_Aman_Hybrid_Sylhet <-
  lm(share_Aman_Hybrid ~ log(area_Aman_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman %>% filter(div_name == "Sylhet")) 
summary(est_area_Aman_Hybrid_Sylhet)
coeftest(est_area_Aman_Hybrid_Sylhet, vcov. = vcovHC(est_area_Aman_Hybrid_Sylhet))
coeftest(est_area_Aman_Hybrid_Sylhet, vcov. = vcovCL(est_area_Aman_Hybrid_Sylhet, cluster =~ clid ))

####Aman Division Local
est_area_Aman_Local_Barisal <-
  lm(share_Aman_Local ~ log(area_Aman_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman %>% filter(div_name == "Barisal")) 
summary(est_area_Aman_Local_Barisal)
coeftest(est_area_Aman_Local_Barisal, vcov. = vcovHC(est_area_Aman_Local_Barisal))
coeftest(est_area_Aman_Local_Barisal, vcov. = vcovCL(est_area_Aman_Local_Barisal, cluster =~ clid ))

est_area_Aman_Local_Dhaka <-
  lm(share_Aman_Local ~ log(area_Aman_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman %>% filter(div_name == "Dhaka")) 
summary(est_area_Aman_Local_Dhaka)
coeftest(est_area_Aman_Local_Dhaka, vcov. = vcovHC(est_area_Aman_Local_Dhaka))
coeftest(est_area_Aman_Local_Dhaka, vcov. = vcovCL(est_area_Aman_Local_Dhaka, cluster =~ clid )) ###final report


est_area_Aman_Local_Chittagong <-
  lm(share_Aman_Local ~ log(area_Aman_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman %>% filter(div_name == "Chittagong")) 
summary(est_area_Aman_Local_Chittagong)
coeftest(est_area_Aman_Local_Chittagong, vcov. = vcovHC(est_area_Aman_Local_Chittagong))
coeftest(est_area_Aman_Local_Chittagong, vcov. = vcovCL(est_area_Aman_Local_Chittagong, cluster =~ clid ))

est_area_Aman_Local_Khulna <-
  lm(share_Aman_Local ~ log(area_Aman_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman %>% filter(div_name == "Khulna")) 
summary(est_area_Aman_Local_Khulna)
coeftest(est_area_Aman_Local_Khulna, vcov. = vcovHC(est_area_Aman_Local_Khulna))
coeftest(est_area_Aman_Local_Khulna, vcov. = vcovCL(est_area_Aman_Local_Khulna, cluster =~ clid ))

est_area_Aman_Local_Rajshahi <-
  lm(share_Aman_Local ~ log(area_Aman_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman %>% filter(div_name == "Rajshahi")) 
summary(est_area_Aman_Local_Rajshahi)
coeftest(est_area_Aman_Local_Rajshahi, vcov. = vcovHC(est_area_Aman_Local_Rajshahi))
coeftest(est_area_Aman_Local_Rajshahi, vcov. = vcovCL(est_area_Aman_Local_Rajshahi, cluster =~ clid ))

est_area_Aman_Local_Rangpur <-
  lm(share_Aman_Local ~ log(area_Aman_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman %>% filter(div_name == "Rangpur")) 
summary(est_area_Aman_Local_Rangpur)
coeftest(est_area_Aman_Local_Rangpur, vcov. = vcovHC(est_area_Aman_Local_Rangpur))
coeftest(est_area_Aman_Local_Rangpur, vcov. = vcovCL(est_area_Aman_Local_Rangpur, cluster =~ clid ))

est_area_Aman_Local_Sylhet <-
  lm(share_Aman_Local ~ log(area_Aman_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Aman %>% filter(div_name == "Sylhet")) 
summary(est_area_Aman_Local_Sylhet)
coeftest(est_area_Aman_Local_Sylhet, vcov. = vcovHC(est_area_Aman_Local_Sylhet))
coeftest(est_area_Aman_Local_Sylhet, vcov. = vcovCL(est_area_Aman_Local_Sylhet, cluster =~ clid ))

#### Boro DIvision HYV
est_area_Boro_HYV_Barisal <-
  lm(share_Boro_HYV ~ log(area_Boro_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Boro %>% filter(div_name == "Barisal")) 
summary(est_area_Boro_HYV_Barisal)
coeftest(est_area_Boro_HYV_Barisal, vcov. = vcovHC(est_area_Boro_HYV_Barisal))
coeftest(est_area_Boro_HYV_Barisal, vcov. = vcovCL(est_area_Boro_HYV_Barisal, cluster =~ clid ))

est_area_Boro_HYV_Dhaka <-
  lm(share_Boro_HYV ~ log(area_Boro_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Boro %>% filter(div_name == "Dhaka")) 
summary(est_area_Boro_HYV_Dhaka)
coeftest(est_area_Boro_HYV_Dhaka, vcov. = vcovHC(est_area_Boro_HYV_Dhaka))
coeftest(est_area_Boro_HYV_Dhaka, vcov. = vcovCL(est_area_Boro_HYV_Dhaka, cluster =~ clid )) ###final report


est_area_Boro_HYV_Chittagong <-
  lm(share_Boro_HYV ~ log(area_Boro_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Boro %>% filter(div_name == "Chittagong")) 
summary(est_area_Boro_HYV_Chittagong)
coeftest(est_area_Boro_HYV_Chittagong, vcov. = vcovHC(est_area_Boro_HYV_Chittagong))
coeftest(est_area_Boro_HYV_Chittagong, vcov. = vcovCL(est_area_Boro_HYV_Chittagong, cluster =~ clid ))

est_area_Boro_HYV_Khulna <-
  lm(share_Boro_HYV ~ log(area_Boro_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Boro %>% filter(div_name == "Khulna")) 
summary(est_area_Boro_HYV_Khulna)
coeftest(est_area_Boro_HYV_Khulna, vcov. = vcovHC(est_area_Boro_HYV_Khulna))
coeftest(est_area_Boro_HYV_Khulna, vcov. = vcovCL(est_area_Boro_HYV_Khulna, cluster =~ clid ))

est_area_Boro_HYV_Rajshahi <-
  lm(share_Boro_HYV ~ log(area_Boro_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Boro %>% filter(div_name == "Rajshahi")) 
summary(est_area_Boro_HYV_Rajshahi)
coeftest(est_area_Boro_HYV_Rajshahi, vcov. = vcovHC(est_area_Boro_HYV_Rajshahi))
coeftest(est_area_Boro_HYV_Rajshahi, vcov. = vcovCL(est_area_Boro_HYV_Rajshahi, cluster =~ clid ))

est_area_Boro_HYV_Rangpur <-
  lm(share_Boro_HYV ~ log(area_Boro_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Boro %>% filter(div_name == "Rangpur")) 
summary(est_area_Boro_HYV_Rangpur)
coeftest(est_area_Boro_HYV_Rangpur, vcov. = vcovHC(est_area_Boro_HYV_Rangpur))
coeftest(est_area_Boro_HYV_Rangpur, vcov. = vcovCL(est_area_Boro_HYV_Rangpur, cluster =~ clid ))

est_area_Boro_HYV_Sylhet <-
  lm(share_Boro_HYV ~ log(area_Boro_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Boro %>% filter(div_name == "Sylhet")) 
summary(est_area_Boro_HYV_Sylhet)
coeftest(est_area_Boro_HYV_Sylhet, vcov. = vcovHC(est_area_Boro_HYV_Sylhet))
coeftest(est_area_Boro_HYV_Sylhet, vcov. = vcovCL(est_area_Boro_HYV_Sylhet, cluster =~ clid ))

###Boro Division hybrid
est_area_Boro_Hybrid_Barisal <-
  lm(share_Boro_Hybrid ~ log(area_Boro_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Boro %>% filter(div_name == "Barisal")) 
summary(est_area_Boro_Hybrid_Barisal)
coeftest(est_area_Boro_Hybrid_Barisal, vcov. = vcovHC(est_area_Boro_Hybrid_Barisal))
coeftest(est_area_Boro_Hybrid_Barisal, vcov. = vcovCL(est_area_Boro_Hybrid_Barisal, cluster =~ clid ))

est_area_Boro_Hybrid_Dhaka <-
  lm(share_Boro_Hybrid ~ log(area_Boro_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Boro %>% filter(div_name == "Dhaka")) 
summary(est_area_Boro_Hybrid_Dhaka)
coeftest(est_area_Boro_Hybrid_Dhaka, vcov. = vcovHC(est_area_Boro_Hybrid_Dhaka))
coeftest(est_area_Boro_Hybrid_Dhaka, vcov. = vcovCL(est_area_Boro_Hybrid_Dhaka, cluster =~ clid )) ###final report


est_area_Boro_Hybrid_Chittagong <-
  lm(share_Boro_Hybrid ~ log(area_Boro_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Boro %>% filter(div_name == "Chittagong")) 
summary(est_area_Boro_Hybrid_Chittagong)
coeftest(est_area_Boro_Hybrid_Chittagong, vcov. = vcovHC(est_area_Boro_Hybrid_Chittagong))
coeftest(est_area_Boro_Hybrid_Chittagong, vcov. = vcovCL(est_area_Boro_Hybrid_Chittagong, cluster =~ clid ))

est_area_Boro_Hybrid_Khulna <-
  lm(share_Boro_Hybrid ~ log(area_Boro_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Boro %>% filter(div_name == "Khulna")) 
summary(est_area_Boro_Hybrid_Khulna)
coeftest(est_area_Boro_Hybrid_Khulna, vcov. = vcovHC(est_area_Boro_Hybrid_Khulna))
coeftest(est_area_Boro_Hybrid_Khulna, vcov. = vcovCL(est_area_Boro_Hybrid_Khulna, cluster =~ clid ))

est_area_Boro_Hybrid_Rajshahi <-
  lm(share_Boro_Hybrid ~ log(area_Boro_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Boro %>% filter(div_name == "Rajshahi")) 
summary(est_area_Boro_Hybrid_Rajshahi)
coeftest(est_area_Boro_Hybrid_Rajshahi, vcov. = vcovHC(est_area_Boro_Hybrid_Rajshahi))
coeftest(est_area_Boro_Hybrid_Rajshahi, vcov. = vcovCL(est_area_Boro_Hybrid_Rajshahi, cluster =~ clid ))

est_area_Boro_Hybrid_Rangpur <-
  lm(share_Boro_Hybrid ~ log(area_Boro_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Boro %>% filter(div_name == "Rangpur")) 
summary(est_area_Boro_Hybrid_Rangpur)
coeftest(est_area_Boro_Hybrid_Rangpur, vcov. = vcovHC(est_area_Boro_Hybrid_Rangpur))
coeftest(est_area_Boro_Hybrid_Rangpur, vcov. = vcovCL(est_area_Boro_Hybrid_Rangpur, cluster =~ clid ))

est_area_Boro_Hybrid_Sylhet <-
  lm(share_Boro_Hybrid ~ log(area_Boro_total) + hh_educ + hh_age + hhsize + I(Female_1564/hhsize) + I(Male_1564/hhsize) + I(Female_65ov/hhsize) + I(Male_65ov/hhsize) + 
       fhh + totalasset + 
       advice_irri + advice_seed + 
       BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Boro %>% filter(div_name == "Sylhet")) 
summary(est_area_Boro_Hybrid_Sylhet)
coeftest(est_area_Boro_Hybrid_Sylhet, vcov. = vcovHC(est_area_Boro_Hybrid_Sylhet))
coeftest(est_area_Boro_Hybrid_Sylhet, vcov. = vcovCL(est_area_Boro_Hybrid_Sylhet, cluster =~ clid ))
"""
est_totalasset <-
  lm( hh_educ + hh_age + hhsize + div_name + 
       fhh + advice_irri + advice_seed + 
       # BRRIdhan + Bina +
       agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_totalasset)
summary(est_totalasset)
cenest_totalasset <-
  censReg(hh_educ + hh_age + hhsize + div_name + 
            fhh + advice_irri + advice_seed + 
            # BRRIdhan + Bina +
            agrcultrl_fish_lstock_groupmem, data = BIHS3_dfest_Boro)
summary(cenest_totalasset)
"""
###############################


#construct estimation formula in several way --- availavility or usage of bina/bridhan --- 
#change input with different variables like in line 125
#what varibls are consistent with previous studies  - the two - check for sign and significant
#negative means decreasing of HYV as area total increases


#Do by other division
#How to manage different region + discussion on which factor to include + different factors + education part error
#monday

