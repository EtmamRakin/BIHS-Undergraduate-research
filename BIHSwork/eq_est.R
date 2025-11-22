library(haven)
library(tidyverse)
library(sandwich)
library(estimatr)
library(lmtest)
library(censReg)



BIHS3_dfest %>% 
  count(Adopt_HYV)
BIHS3_dfest %>% 
  count(Adopt_Hybrid)
BIHS3_dfest %>% 
  count(Adopt_Local)

BIHS3_dfest %>% 
  count(Adopt_HYV, Adopt_Hybrid) %>% 
  spread(Adopt_Hybrid, n)


BIHS3_dfest %>% 
  count(Adopt_HYV, Adopt_Hybrid, Adopt_Local) %>% 
  spread(Adopt_Hybrid, n)
eq_HYV <-
  Adopt_HYV ~ div_name + 
  ownland_ha + 
  fhh + hh_educp + hh_educh + hh_age + 
  hhsize + Female_1564 + Male_1564 + Female_65ov + Male_65ov  + 
  Machine_assetvalue +
  advice_irri + advice_seed + 
  BRRIdhan + Bina +
  irr_ratio_Boro +
  agrcultrl_fish_lstock_groupmem

est_HYV <-probit.reg(eq_HYV, data = BIHS3_dfest)
summary(est_HYV)

eq_Hybrid <-
  Adopt_Hybrid ~ div_name + 
  ownland_ha + 
  fhh + literacy_status + hh_age + 
  hhsize + Female_1564 + Male_1564 + Female_65ov + Male_65ov  + 
  Machine_assetvalue + 
  advice_irri + advice_seed + 
  BRRIdhan + Bina +
  irr_ratio_Boro +
  agrcultrl_fish_lstock_groupmem

est_Hybrid <-probit.reg(eq_Hybrid, data = BIHS3_dfest)
summary(est_Hybrid)

eq_Local <-
  Adopt_Local ~ div_name + 
  ownland_ha + 
  fhh + literacy_status + hh_age + 
  hhsize + Female_1564 + Male_1564 + Female_65ov + Male_65ov  + 
  Machine_assetvalue + 
  advice_irri + advice_seed + 
  BRRIdhan + Bina +
  irr_ratio_Boro +
  agrcultrl_fish_lstock_groupmem

est_Local <-probit.reg(eq_Local, data = BIHS3_dfest)
summary(est_Local)
