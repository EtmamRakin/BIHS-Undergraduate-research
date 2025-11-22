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

BIHS3_dfest <-
  read_dta("BIHS3_dfest.dta")
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
BIHS3_dfest %>% count(Aman_HYV)
BIHS3_dfest %>% count(Aman_Hybrid)
BIHS3_dfest %>% count(Aman_Local)
BIHS3_dfest %>% count(Aman_HYV, Aman_Hybrid)
BIHS3_dfest %>% count(Aman_HYV, Aman_Hybrid, Aman_Local)

BIHS3_dfest %>% count(Boro_HYV)
BIHS3_dfest %>% count(Boro_Hybrid)
BIHS3_dfest %>% count(Boro_HYV, Boro_Hybrid)


names(BIHS3_dfest)
BIHS3_dfest_Aman <-
  BIHS3_dfest %>% filter(area_Aman_total >= 0)
BIHS3_dfest_Aman %>% count(Aman_HYV)
BIHS3_dfest_Aman %>% count(Aman_Hybrid)
BIHS3_dfest_Aman %>% count(Aman_Local)
BIHS3_dfest_Aman %>% count(Aman_HYV, Aman_Hybrid, Aman_Local)

BIHS3_dfest_Boro <-
  BIHS3_dfest %>% filter(area_Boro_total >= 0)



BIHS3_dfest_Aman %>% 
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

eq_area_Aman_HYV <-
  area_Aman_HYV ~ div_name + 
  ownland_ha + 
  fhh + hh_educp + hh_educh + hh_age + 
  hhsize + Female_1564 + Male_1564 + Female_65ov + Male_65ov  + 
  totalasset + 
  advice_irri + advice_seed + 
  BRRIdhan + Bina +
  irr_ratio_Aman +
  agrcultrl_fish_lstock_groupmem

est_area_Aman_HYV <-lm(eq_area_Aman_HYV, data = BIHS3_dfest_Aman)
summary(est_area_Aman_HYV)
coeftest(est_area_Aman_HYV, vcov. = vcovHC(est_area_Aman_HYV))
coeftest(est_area_Aman_HYV, vcov. = vcovCL(est_area_Aman_HYV, cluster =~ clid ))



eq_area_Aman_Hybrid <-
  area_Aman_Hybrid ~ div_name + 
  ownland_ha + 
  fhh + hh_educp + hh_educh + hh_age + 
  hhsize + Female_1564 + Male_1564 + Female_65ov + Male_65ov  + 
  totalasset + 
  advice_irri + advice_seed + 
  BRRIdhan + Bina +
  irr_ratio_Aman +
  agrcultrl_fish_lstock_groupmem

est_area_Aman_Hybrid <-lm(eq_area_Aman_Hybrid, data = BIHS3_dfest_Aman)
summary(est_area_Aman_Hybrid)
coeftest(est_area_Aman_Hybrid, vcov. = vcovHC(est_area_Aman_Hybrid))
coeftest(est_area_Aman_Hybrid, vcov. = vcovCL(est_area_Aman_Hybrid, cluster =~ clid ))


eq_area_Aman_Local <-
  area_Aman_Local ~ div_name + 
  ownland_ha + 
  fhh + hh_educp + hh_educh + hh_age + 
  hhsize + Female_1564 + Male_1564 + Female_65ov + Male_65ov  + 
  totalasset + 
  advice_irri + advice_seed + 
  BRRIdhan + Bina +
  irr_ratio_Aman +
  agrcultrl_fish_lstock_groupmem

est_area_Aman_Local <-lm(eq_area_Aman_Local, data = BIHS3_dfest_Aman)
summary(est_area_Aman_Local)
coeftest(est_area_Aman_Local, vcov. = vcovHC(est_area_Aman_Local))
coeftest(est_area_Aman_Local, vcov. = vcovCL(est_area_Aman_Local, cluster =~ clid ))


eq_area_Boro_HYV <-
  area_Boro_HYV ~ div_name + 
  ownland_ha + 
  fhh + hh_educp + hh_educh + hh_age + 
  hhsize + Female_1564 + Male_1564 + Female_65ov + Male_65ov  + 
  totalasset + 
  advice_irri + advice_seed + 
  BRRIdhan + Bina +
  irr_ratio_Boro +
  agrcultrl_fish_lstock_groupmem

est_area_Boro_HYV <-lm(eq_area_Boro_HYV, data = BIHS3_dfest_Boro)
summary(est_area_Boro_HYV)
coeftest(est_area_Boro_HYV, vcov. = vcovHC(est_area_Boro_HYV))
coeftest(est_area_Boro_HYV, vcov. = vcovCL(est_area_Boro_HYV, cluster =~ clid ))


eq_area_Boro_Hybrid <-
  area_Boro_Hybrid ~ div_name + 
  ownland_ha + 
  fhh + hh_educp + hh_educh + hh_age + 
  hhsize + Female_1564 + Male_1564 + Female_65ov + Male_65ov  + 
  totalasset + 
  advice_irri + advice_seed + 
  BRRIdhan + Bina +
  irr_ratio_Boro +
  agrcultrl_fish_lstock_groupmem

est_area_Boro_Hybrid <-lm(eq_area_Boro_Hybrid, data = BIHS3_dfest_Boro)
summary(est_area_Boro_Hybrid)
coeftest(est_area_Boro_Hybrid, vcov. = vcovHC(est_area_Boro_Hybrid))
coeftest(est_area_Boro_Hybrid, vcov. = vcovCL(est_area_Boro_Hybrid, cluster =~ clid ))


BIHS3_dfest %>% 
  group_by() %>% 
  summarize(mean(hh_educp, na.rm = TRUE), weighted.mean(w = hhweight, x= hh_educp, na.rm = TRUE))

####probit estimation



eq_Aman_HYV <-
  Aman_HYV ~ div_name + 
  ownland_ha + 
  fhh + hh_educp + hh_educh + hh_age + 
  hhsize + Female_1564 + Male_1564 + Female_65ov + Male_65ov  + 
  totalasset + 
  advice_irri + advice_seed + 
  BRRIdhan + Bina +
  irr_ratio_Aman +
  agrcultrl_fish_lstock_groupmem

est_Aman_HYV <-glm(eq_Aman_HYV, data = BIHS3_dfest_Aman, family = binomial(probit))
summary(est_Aman_HYV)
coeftest(est_Aman_HYV, vcov. = vcovHC(est_Aman_HYV))
coeftest(est_Aman_HYV, vcov. = vcovCL(est_Aman_HYV, cluster =~ clid ))


eq_Aman_Hybrid <-
  Aman_Hybrid ~ div_name + 
  ownland_ha + 
  fhh + hh_educp + hh_educh + hh_age + 
  hhsize + Female_1564 + Male_1564 + Female_65ov + Male_65ov  + 
  totalasset + 
  advice_irri + advice_seed + 
  BRRIdhan + Bina +
  irr_ratio_Aman +
  agrcultrl_fish_lstock_groupmem

est_Aman_Hybrid <-glm(eq_Aman_Hybrid, data = BIHS3_dfest_Aman, family = binomial(probit))
summary(est_Aman_Hybrid)
coeftest(est_Aman_Hybrid, vcov. = vcovHC(est_Aman_Hybrid))
coeftest(est_Aman_Hybrid, vcov. = vcovCL(est_Aman_Hybrid, cluster =~ clid ))

eq_Aman_Local <-
  Aman_Local ~ div_name + 
  ownland_ha + 
  fhh + hh_educp + hh_educh + hh_age + 
  hhsize + Female_1564 + Male_1564 + Female_65ov + Male_65ov  + 
  totalasset + 
  advice_irri + advice_seed + 
  BRRIdhan + Bina +
  irr_ratio_Aman +
  agrcultrl_fish_lstock_groupmem

est_Aman_Local <-glm(eq_Aman_Local, data = BIHS3_dfest_Aman, family = binomial(probit))
summary(est_Aman_Local)
coeftest(est_Aman_Local, vcov. = vcovHC(est_Aman_Local))
coeftest(est_Aman_Local, vcov. = vcovCL(est_Aman_Local, cluster =~ clid ))


eq_Boro_Hybrid <-
  Boro_Hybrid ~ div_name + 
  ownland_ha + 
  fhh + hh_educp + hh_educh + hh_age + 
  hhsize + Female_1564 + Male_1564 + Female_65ov + Male_65ov  + 
  totalasset + 
  advice_irri + advice_seed + 
  BRRIdhan + Bina +
  irr_ratio_Boro +
  agrcultrl_fish_lstock_groupmem

est_Boro_Hybrid <-glm(eq_Boro_Hybrid, data = BIHS3_dfest_Boro, family = binomial(probit))
summary(est_Boro_Hybrid)
coeftest(est_Boro_Hybrid, vcov. = vcovHC(est_Boro_Hybrid))
coeftest(est_Boro_Hybrid, vcov. = vcovCL(est_Boro_Hybrid, cluster =~ clid ))

eq_Boro_HYV <-
  Boro_HYV ~ div_name + 
  ownland_ha + 
  fhh + hh_educp + hh_educh + literacy_status + hh_age + 
  hhsize + Female_1564 + Male_1564 + Female_65ov + Male_65ov  + 
  totalasset + 
  advice_irri + advice_seed + 
  BRRIdhan + Bina +
  irr_ratio_Boro +
  agrcultrl_fish_lstock_groupmem

est_Boro_HYV <-glm(eq_Boro_HYV, data = BIHS3_dfest_Boro, family = binomial(probit))
summary(est_Boro_HYV)
coeftest(est_Boro_HYV, vcov. = vcovHC(est_Boro_HYV))
coeftest(est_Boro_HYV, vcov. = vcovCL(est_Boro_HYV, cluster =~ clid ))

BIHS3_dfest_Boro %>% 
  count(div_name, Boro_HYV)

est_Boro_HYV_Rajshahi<-
  Boro_HYV ~ 
  ownland_ha + 
  fhh + hh_educp + hh_educh + literacy_status + hh_age + 
  hhsize + Female_1564 + Male_1564 + Female_65ov + Male_65ov  + 
  totalasset + 
  advice_irri + advice_seed + 
  BRRIdhan + Bina +
  irr_ratio_Boro +
  agrcultrl_fish_lstock_groupmem
BIHS3_dfest_Boro_Rajshahi <- BIHS3_dfest_Boro %>% filter(div_name == "Rajshahi")
est_Boro_HYV_Rajshahi <-glm(eq_Boro_HYV, data = BIHS3_dfest_Boro %>% filter(div_name == "Rajshahi"),  family = binomial(probit))
est_Boro_HYV_Rajshahi <-probit.reg(eq_Boro_HYV, data = BIHS3_dfest_Boro_Rajshahi)

summary(est_Boro_HYV)
coeftest(est_Boro_HYV, vcov. = vcovHC(est_Boro_HYV))
coeftest(est_Boro_HYV, vcov. = vcovCL(est_Boro_HYV, cluster =~ clid ))

###do the b1 - literacy 

##apply this
##what variables to include

