library(mvProbit)
library(mvord)
BIHS3_dfest_mvP <-
  BIHS3_dfest %>% filter(!is.na(Adopt_HYV)) %>% 
  filter(!is.na(Adopt_Hybrid)) %>% 
  filter(!is.na(Adopt_Local))
eq_mvProbit <- 
  mvord(formula = MMO2(Adopt_Hybrid, Adopt_HYV) ~ 0 +
             div_name + 
             ownland_ha + 
             fhh + hh_educp + hh_educh + hh_age + 
             hhsize + Female_1564 + Male_1564 + Female_65ov + Male_65ov  + 
             ##totalasset + 
             ##advice_irri + advice_seed + 
             ##BRRIdhan + Bina +
             ##irr_ratio_Aman +
             agrcultrl_fish_lstock_groupmem, 
           data = BIHS3_dfest_mvP) 
summary(eq_mvProbit)  


###

