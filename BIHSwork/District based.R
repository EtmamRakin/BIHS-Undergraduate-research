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

BIHS3_dfest_Boro %>% 
  ggplot() + aes(x = hh_educ, y = area_Boro_total, group = div_name, color = div_name) +
  geom_point() + facet_wrap(~ hhsize)
BIHS3_dfest_Boro %>% filter(div_name == "Chittagong") %>% 
  ggplot() + aes(x = hh_educ, y = area_Boro_Hybrid, group = district, color = district) +
  geom_point() + facet_wrap(~ district)


#Aman_HYV
eq_area_Aman_HYV_div <-
  area_Aman_HYV ~ as.factor(district)  +
  ownland_ha + 
  fhh + hh_educp + hh_educh + literacy_status + hh_age + 
  hhsize + Female_1564 + Male_1564 + Female_65ov + Male_65ov  + 
  totalasset + 
  advice_irri + advice_seed + 
  BRRIdhan + Bina +
  irr_ratio_Aman +
  agrcultrl_fish_lstock_groupmem

est_area_Aman_HYV_Dhaka <-lm(eq_area_Aman_HYV_div, data = BIHS3_dfest_Aman %>% filter(div_name == "Dhaka"))
summary(est_area_Aman_HYV_Dhaka)
coeftest(est_area_Aman_HYV_Dhaka, vcov. = vcovHC(est_area_Aman_HYV_Dhaka))
coeftest(est_area_Aman_HYV_Dhaka, vcov. = vcovCL(est_area_Aman_HYV_Dhaka, cluster =~ clid ))

est_area_Aman_HYV_Chittagong <-lm(eq_area_Aman_HYV_div, data = BIHS3_dfest_Aman %>% filter(div_name == "Chittagong"))
summary(est_area_Aman_HYV_Chittagong)
coeftest(est_area_Aman_HYV_Chittagong, vcov. = vcovHC(est_area_Aman_HYV_Chittagong))
coeftest(est_area_Aman_HYV_Chittagong, vcov. = vcovCL(est_area_Aman_HYV_Chittagong, cluster =~ clid ))

est_area_Aman_HYV_Khulna <-lm(eq_area_Aman_HYV_div, data = BIHS3_dfest_Aman %>% filter(div_name == "Khulna"))
summary(est_area_Aman_HYV_Khulna)
coeftest(est_area_Aman_HYV_Khulna, vcov. = vcovHC(est_area_Aman_HYV_Khulna))
coeftest(est_area_Aman_HYV_Khulna, vcov. = vcovCL(est_area_Aman_HYV_Khulna, cluster =~ clid ))

est_area_Aman_HYV_Barisal <-lm(eq_area_Aman_HYV_div, data = BIHS3_dfest_Aman %>% filter(div_name == "Barisal"))
summary(est_area_Aman_HYV_Barisal)
coeftest(est_area_Aman_HYV_Barisal, vcov. = vcovHC(est_area_Aman_HYV_Barisal))
coeftest(est_area_Aman_HYV_Barisal, vcov. = vcovCL(est_area_Aman_HYV_Barisal, cluster =~ clid ))

est_area_Aman_HYV_Rajshahi <-lm(eq_area_Aman_HYV_div, data = BIHS3_dfest_Aman %>% filter(div_name == "Rajshahi"))
summary(est_area_Aman_HYV_Rajshahi)
coeftest(est_area_Aman_HYV_Rajshahi, vcov. = vcovHC(est_area_Aman_HYV_Rajshahi))
coeftest(est_area_Aman_HYV_Rajshahi, vcov. = vcovCL(est_area_Aman_HYV_Rajshahi, cluster =~ clid ))

est_area_Aman_HYV_Rangpur <-lm(eq_area_Aman_HYV_div, data = BIHS3_dfest_Aman %>% filter(div_name == "Rangpur"))
summary(est_area_Aman_HYV_Rangpur)
coeftest(est_area_Aman_HYV_Rangpur, vcov. = vcovHC(est_area_Aman_HYV_Rangpur))
coeftest(est_area_Aman_HYV_Rangpur, vcov. = vcovCL(est_area_Aman_HYV_Rangpur, cluster =~ clid ))

est_area_Aman_HYV_Sylhet <-lm(eq_area_Aman_HYV_div, data = BIHS3_dfest_Aman %>% filter(div_name == "Sylhet"))
summary(est_area_Aman_HYV_Sylhet)
coeftest(est_area_Aman_HYV_Sylhet, vcov. = vcovHC(est_area_Aman_HYV_Sylhet))
coeftest(est_area_Aman_HYV_Sylhet, vcov. = vcovCL(est_area_Aman_HYV_Sylhet, cluster =~ clid ))

#Aman_Hybrid
eq_area_Aman_Hybrid_div <-
  area_Aman_Hybrid ~ as.factor(district)  +
  ownland_ha + 
  fhh + hh_educp + hh_educh + literacy_status + hh_age + 
  hhsize + Female_1564 + Male_1564 + Female_65ov + Male_65ov  + 
  totalasset + 
  advice_irri + advice_seed + 
  BRRIdhan + Bina +
  irr_ratio_Aman +
  agrcultrl_fish_lstock_groupmem


est_area_Aman_Hybrid_Dhaka <-lm(eq_area_Aman_Hybrid_div, data = BIHS3_dfest_Aman %>% filter(div_name == "Dhaka"))
summary(est_area_Aman_Hybrid_Dhaka)
coeftest(est_area_Aman_Hybrid_Dhaka, vcov. = vcovHC(est_area_Aman_Hybrid_Dhaka))
coeftest(est_area_Aman_Hybrid_Dhaka, vcov. = vcovCL(est_area_Aman_Hybrid_Dhaka, cluster =~ clid ))

est_area_Aman_Hybrid_Chittagong <-lm(eq_area_Aman_Hybrid_div, data = BIHS3_dfest_Aman %>% filter(div_name == "Chittagong"))
summary(est_area_Aman_Hybrid_Chittagong)
coeftest(est_area_Aman_Hybrid_Chittagong, vcov. = vcovHC(est_area_Aman_Hybrid_Chittagong))
coeftest(est_area_Aman_Hybrid_Chittagong, vcov. = vcovCL(est_area_Aman_Hybrid_Chittagong, cluster =~ clid ))

est_area_Aman_Hybrid_Khulna <-lm(eq_area_Aman_Hybrid_div, data = BIHS3_dfest_Aman %>% filter(div_name == "Khulna"))
summary(est_area_Aman_Hybrid_Khulna)
coeftest(est_area_Aman_Hybrid_Khulna, vcov. = vcovHC(est_area_Aman_Hybrid_Khulna))
coeftest(est_area_Aman_Hybrid_Khulna, vcov. = vcovCL(est_area_Aman_Hybrid_Khulna, cluster =~ clid ))

est_area_Aman_Hybrid_Barisal <-lm(eq_area_Aman_Hybrid_div, data = BIHS3_dfest_Aman %>% filter(div_name == "Barisal"))
summary(est_area_Aman_Hybrid_Barisal)
coeftest(est_area_Aman_Hybrid_Barisal, vcov. = vcovHC(est_area_Aman_Hybrid_Barisal))
coeftest(est_area_Aman_Hybrid_Barisal, vcov. = vcovCL(est_area_Aman_Hybrid_Barisal, cluster =~ clid ))

est_area_Aman_Hybrid_Rajshahi <-lm(eq_area_Aman_Hybrid_div, data = BIHS3_dfest_Aman %>% filter(div_name == "Rajshahi"))
summary(est_area_Aman_Hybrid_Rajshahi)
coeftest(est_area_Aman_Hybrid_Rajshahi, vcov. = vcovHC(est_area_Aman_Hybrid_Rajshahi))
coeftest(est_area_Aman_Hybrid_Rajshahi, vcov. = vcovCL(est_area_Aman_Hybrid_Rajshahi, cluster =~ clid ))

est_area_Aman_Hybrid_Rangpur <-lm(eq_area_Aman_Hybrid_div, data = BIHS3_dfest_Aman %>% filter(div_name == "Rangpur"))
summary(est_area_Aman_Hybrid_Rangpur)
coeftest(est_area_Aman_Hybrid_Rangpur, vcov. = vcovHC(est_area_Aman_Hybrid_Rangpur))
coeftest(est_area_Aman_Hybrid_Rangpur, vcov. = vcovCL(est_area_Aman_Hybrid_Rangpur, cluster =~ clid ))

est_area_Aman_Hybrid_Sylhet <-lm(eq_area_Aman_Hybrid_div, data = BIHS3_dfest_Aman %>% filter(div_name == "Sylhet"))
summary(est_area_Aman_Hybrid_Sylhet)
coeftest(est_area_Aman_Hybrid_Sylhet, vcov. = vcovHC(est_area_Aman_Hybrid_Sylhet))
coeftest(est_area_Aman_Hybrid_Sylhet, vcov. = vcovCL(est_area_Aman_Hybrid_Sylhet, cluster =~ clid ))

#Aman_Local
eq_area_Aman_Local_div <-
  area_Aman_Local ~ as.factor(district)  +
  ownland_ha + 
  fhh + hh_educp + hh_educh + literacy_status + hh_age + 
  hhsize + Female_1564 + Male_1564 + Female_65ov + Male_65ov  + 
  totalasset + 
  advice_irri + advice_seed + 
  BRRIdhan + Bina +
  irr_ratio_Aman +
  agrcultrl_fish_lstock_groupmem


est_area_Aman_Local_Dhaka <-lm(eq_area_Aman_Local_div, data = BIHS3_dfest_Aman %>% filter(div_name == "Dhaka"))
summary(est_area_Aman_Local_Dhaka)
coeftest(est_area_Aman_Local_Dhaka, vcov. = vcovHC(est_area_Aman_Local_Dhaka))
coeftest(est_area_Aman_Local_Dhaka, vcov. = vcovCL(est_area_Aman_Local_Dhaka, cluster =~ clid ))

est_area_Aman_Local_Chittagong <-lm(eq_area_Aman_Local_div, data = BIHS3_dfest_Aman %>% filter(div_name == "Chittagong"))
summary(est_area_Aman_Local_Chittagong)
coeftest(est_area_Aman_Local_Chittagong, vcov. = vcovHC(est_area_Aman_Local_Chittagong))
coeftest(est_area_Aman_Local_Chittagong, vcov. = vcovCL(est_area_Aman_Local_Chittagong, cluster =~ clid ))

est_area_Aman_Local_Khulna <-lm(eq_area_Aman_Local_div, data = BIHS3_dfest_Aman %>% filter(div_name == "Khulna"))
summary(est_area_Aman_Local_Khulna)
coeftest(est_area_Aman_Local_Khulna, vcov. = vcovHC(est_area_Aman_Local_Khulna))
coeftest(est_area_Aman_Local_Khulna, vcov. = vcovCL(est_area_Aman_Local_Khulna, cluster =~ clid ))

est_area_Aman_Local_Barisal <-lm(eq_area_Aman_Local_div, data = BIHS3_dfest_Aman %>% filter(div_name == "Barisal"))
summary(est_area_Aman_Local_Barisal)
coeftest(est_area_Aman_Local_Barisal, vcov. = vcovHC(est_area_Aman_Local_Barisal))
coeftest(est_area_Aman_Local_Barisal, vcov. = vcovCL(est_area_Aman_Local_Barisal, cluster =~ clid ))

est_area_Aman_Local_Rajshahi <-lm(eq_area_Aman_Local_div, data = BIHS3_dfest_Aman %>% filter(div_name == "Rajshahi"))
summary(est_area_Aman_Local_Rajshahi)
coeftest(est_area_Aman_Local_Rajshahi, vcov. = vcovHC(est_area_Aman_Local_Rajshahi))
coeftest(est_area_Aman_Local_Rajshahi, vcov. = vcovCL(est_area_Aman_Local_Rajshahi, cluster =~ clid ))

est_area_Aman_Local_Rangpur <-lm(eq_area_Aman_Local_div, data = BIHS3_dfest_Aman %>% filter(div_name == "Rangpur"))
summary(est_area_Aman_Local_Rangpur)
coeftest(est_area_Aman_Local_Rangpur, vcov. = vcovHC(est_area_Aman_Local_Rangpur))
coeftest(est_area_Aman_Local_Rangpur, vcov. = vcovCL(est_area_Aman_Local_Rangpur, cluster =~ clid ))

est_area_Aman_Local_Sylhet <-lm(eq_area_Aman_Local_div, data = BIHS3_dfest_Aman %>% filter(div_name == "Sylhet"))
summary(est_area_Aman_Local_Sylhet)
coeftest(est_area_Aman_Local_Sylhet, vcov. = vcovHC(est_area_Aman_Local_Sylhet))
coeftest(est_area_Aman_Local_Sylhet, vcov. = vcovCL(est_area_Aman_Local_Sylhet, cluster =~ clid ))


#Boro_HYV
eq_area_Boro_HYV_div <-
  area_Boro_HYV ~ as.factor(district)  +
  ownland_ha + 
  fhh + hh_educp + hh_educh + literacy_status + hh_age + 
  hhsize + Female_1564 + Male_1564 + Female_65ov + Male_65ov  + 
  totalasset + 
  advice_irri + advice_seed + 
  BRRIdhan + Bina +
  irr_ratio_Boro +
  agrcultrl_fish_lstock_groupmem

est_area_Boro_HYV_Dhaka <-lm(eq_area_Boro_HYV_div, data = BIHS3_dfest_Boro %>% filter(div_name == "Dhaka"))
summary(est_area_Boro_HYV_Dhaka)
coeftest(est_area_Boro_HYV_Dhaka, vcov. = vcovHC(est_area_Boro_HYV_Dhaka))
coeftest(est_area_Boro_HYV_Dhaka, vcov. = vcovCL(est_area_Boro_HYV_Dhaka, cluster =~ clid ))

est_area_Boro_HYV_Chittagong <-lm(eq_area_Boro_HYV_div, data = BIHS3_dfest_Boro %>% filter(div_name == "Chittagong"))
summary(est_area_Boro_HYV_Chittagong)
coeftest(est_area_Boro_HYV_Chittagong, vcov. = vcovHC(est_area_Boro_HYV_Chittagong))
coeftest(est_area_Boro_HYV_Chittagong, vcov. = vcovCL(est_area_Boro_HYV_Chittagong, cluster =~ clid ))

est_area_Boro_HYV_Khulna <-lm(eq_area_Boro_HYV_div, data = BIHS3_dfest_Boro %>% filter(div_name == "Khulna"))
summary(est_area_Boro_HYV_Khulna)
coeftest(est_area_Boro_HYV_Khulna, vcov. = vcovHC(est_area_Boro_HYV_Khulna))
coeftest(est_area_Boro_HYV_Khulna, vcov. = vcovCL(est_area_Boro_HYV_Khulna, cluster =~ clid ))

est_area_Boro_HYV_Barisal <-lm(eq_area_Boro_HYV_div, data = BIHS3_dfest_Boro %>% filter(div_name == "Barisal"))
summary(est_area_Boro_HYV_Barisal)
coeftest(est_area_Boro_HYV_Barisal, vcov. = vcovHC(est_area_Boro_HYV_Barisal))
coeftest(est_area_Boro_HYV_Barisal, vcov. = vcovCL(est_area_Boro_HYV_Barisal, cluster =~ clid ))

est_area_Boro_HYV_Rajshahi <-lm(eq_area_Boro_HYV_div, data = BIHS3_dfest_Boro %>% filter(div_name == "Rajshahi"))
summary(est_area_Boro_HYV_Rajshahi)
coeftest(est_area_Boro_HYV_Rajshahi, vcov. = vcovHC(est_area_Boro_HYV_Rajshahi))
coeftest(est_area_Boro_HYV_Rajshahi, vcov. = vcovCL(est_area_Boro_HYV_Rajshahi, cluster =~ clid ))

est_area_Boro_HYV_Rangpur <-lm(eq_area_Boro_HYV_div, data = BIHS3_dfest_Boro %>% filter(div_name == "Rangpur"))
summary(est_area_Boro_HYV_Rangpur)
coeftest(est_area_Boro_HYV_Rangpur, vcov. = vcovHC(est_area_Boro_HYV_Rangpur))
coeftest(est_area_Boro_HYV_Rangpur, vcov. = vcovCL(est_area_Boro_HYV_Rangpur, cluster =~ clid ))

est_area_Boro_HYV_Sylhet <-lm(eq_area_Boro_HYV_div, data = BIHS3_dfest_Boro %>% filter(div_name == "Sylhet"))
summary(est_area_Boro_HYV_Sylhet)
coeftest(est_area_Boro_HYV_Sylhet, vcov. = vcovHC(est_area_Boro_HYV_Sylhet))
coeftest(est_area_Boro_HYV_Sylhet, vcov. = vcovCL(est_area_Boro_HYV_Sylhet, cluster =~ clid ))

#Boro_Hybrid
eq_area_Boro_Hybrid_div <-
  area_Boro_Hybrid ~ as.factor(district)  +
  ownland_ha + 
  fhh + hh_educp + hh_educh + literacy_status + hh_age + 
  hhsize + Female_1564 + Male_1564 + Female_65ov + Male_65ov  + 
  totalasset + 
  advice_irri + advice_seed + 
  BRRIdhan + Bina +
  irr_ratio_Boro +
  agrcultrl_fish_lstock_groupmem


est_area_Boro_Hybrid_Dhaka <-lm(eq_area_Boro_Hybrid_div, data = BIHS3_dfest_Boro %>% filter(div_name == "Dhaka"))
summary(est_area_Boro_Hybrid_Dhaka)
coeftest(est_area_Boro_Hybrid_Dhaka, vcov. = vcovHC(est_area_Boro_Hybrid_Dhaka))
coeftest(est_area_Boro_Hybrid_Dhaka, vcov. = vcovCL(est_area_Boro_Hybrid_Dhaka, cluster =~ clid ))

est_area_Boro_Hybrid_Chittagong <-lm(eq_area_Boro_Hybrid_div, data = BIHS3_dfest_Boro %>% filter(div_name == "Chittagong"))
summary(est_area_Boro_Hybrid_Chittagong)
coeftest(est_area_Boro_Hybrid_Chittagong, vcov. = vcovHC(est_area_Boro_Hybrid_Chittagong))
coeftest(est_area_Boro_Hybrid_Chittagong, vcov. = vcovCL(est_area_Boro_Hybrid_Chittagong, cluster =~ clid ))

est_area_Boro_Hybrid_Khulna <-lm(eq_area_Boro_Hybrid_div, data = BIHS3_dfest_Boro %>% filter(div_name == "Khulna"))
summary(est_area_Boro_Hybrid_Khulna)
coeftest(est_area_Boro_Hybrid_Khulna, vcov. = vcovHC(est_area_Boro_Hybrid_Khulna))
coeftest(est_area_Boro_Hybrid_Khulna, vcov. = vcovCL(est_area_Boro_Hybrid_Khulna, cluster =~ clid ))

est_area_Boro_Hybrid_Barisal <-lm(eq_area_Boro_Hybrid_div, data = BIHS3_dfest_Boro %>% filter(div_name == "Barisal"))
summary(est_area_Boro_Hybrid_Barisal)
coeftest(est_area_Boro_Hybrid_Barisal, vcov. = vcovHC(est_area_Boro_Hybrid_Barisal))
coeftest(est_area_Boro_Hybrid_Barisal, vcov. = vcovCL(est_area_Boro_Hybrid_Barisal, cluster =~ clid ))

est_area_Boro_Hybrid_Rajshahi <-lm(eq_area_Boro_Hybrid_div, data = BIHS3_dfest_Boro %>% filter(div_name == "Rajshahi"))
summary(est_area_Boro_Hybrid_Rajshahi)
coeftest(est_area_Boro_Hybrid_Rajshahi, vcov. = vcovHC(est_area_Boro_Hybrid_Rajshahi))
coeftest(est_area_Boro_Hybrid_Rajshahi, vcov. = vcovCL(est_area_Boro_Hybrid_Rajshahi, cluster =~ clid ))

est_area_Boro_Hybrid_Rangpur <-lm(eq_area_Boro_Hybrid_div, data = BIHS3_dfest_Boro %>% filter(div_name == "Rangpur"))
summary(est_area_Boro_Hybrid_Rangpur)
coeftest(est_area_Boro_Hybrid_Rangpur, vcov. = vcovHC(est_area_Boro_Hybrid_Rangpur))
coeftest(est_area_Boro_Hybrid_Rangpur, vcov. = vcovCL(est_area_Boro_Hybrid_Rangpur, cluster =~ clid ))

est_area_Boro_Hybrid_Sylhet <-lm(eq_area_Boro_Hybrid_div, data = BIHS3_dfest_Boro %>% filter(div_name == "Sylhet"))
summary(est_area_Boro_Hybrid_Sylhet)
coeftest(est_area_Boro_Hybrid_Sylhet, vcov. = vcovHC(est_area_Boro_Hybrid_Sylhet))
coeftest(est_area_Boro_Hybrid_Sylhet, vcov. = vcovCL(est_area_Boro_Hybrid_Sylhet, cluster =~ clid ))

###run for each division  -ctg brs etc
### data construction check again -- let sensei know early
###literate dummy - b1



###community ca cb