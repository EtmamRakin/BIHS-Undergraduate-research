BIHS3_dfest_Boro %>% 
  ggplot() + aes(x = hh_educ, y = area_Boro_total, group = div_name, color = div_name) +
  geom_point() + facet_wrap(~ hhsize)
BIHS3_dfest_Boro %>% filter(div_name == "Chittagong") %>% 
  ggplot() + aes(x = hh_educ, y = area_Boro_Hybrid, group = district, color = district) +
  geom_point() + facet_wrap(~ district)

eq_area_Aman_HYV_div <-
  area_Aman_HYV ~ as.factor(district)  +
  ownland_ha + 
  fhh + hh_educp + hh_educh + hh_age + 
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

eq_area_Aman_Hybrid_div <-
  area_Aman_Hybrid ~ as.factor(district)  +
  ownland_ha + 
  fhh + hh_educp + hh_educh + hh_age + 
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

eq_area_Aman_Hybrid_div <-
  area_Aman_Hybrid ~ as.factor(district)  +
  ownland_ha + 
  fhh + hh_educp + hh_educh + hh_age + 
  hhsize + Female_1564 + Male_1564 + Female_65ov + Male_65ov  + 
  totalasset + 
  advice_irri + advice_seed + 
  BRRIdhan + Bina +
  irr_ratio_Aman +
  agrcultrl_fish_lstock_groupmem

est_area_Aman_Hybrid_Khulna <-lm(eq_area_Aman_Hybrid_div, data = BIHS3_dfest_Aman %>% filter(div_name == "Khulna"))
summary(est_area_Aman_Hybrid_Khulna)
coeftest(est_area_Aman_Hybrid_Khulna, vcov. = vcovHC(est_area_Aman_Hybrid_Khulna))
coeftest(est_area_Aman_Hybrid_Khulna, vcov. = vcovCL(est_area_Aman_Hybrid_Khulna, cluster =~ clid ))

###run for each division  -ctg brs etc
### data construction check again -- let sensei know early
###literate dummy - b1



###community ca cb