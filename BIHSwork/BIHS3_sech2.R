#### this is a code for section H

library(tidyverse)
library(haven)

## reading original data file

BIHS3_hhsecH2 <- read_dta("../BIHSRound3/Male/022_bihs_r3_male_mod_h2.dta")
names(BIHS3_hhsecH2)
str(BIHS3_hhsecH2)


BIHS3_hhsecH2 <- 
  BIHS3_hhsecH2 %>% mutate(hhid = a01,
                           plotno =h2_sl) %>%
  mutate(crop_1 = sjlabelled::as_label(crop_a_h2)) %>%
  mutate(crop_1oth = sjlabelled::as_label(crop_aoth_h2)) %>%
  mutate(crop_2 = sjlabelled::as_label(crop_b_h2)) %>%
  mutate(crop_2oth = sjlabelled::as_label(crop_both_h2)) %>%
  mutate(water_source = sjlabelled::as_label(h2_01)) %>%
  mutate(irr_method = sjlabelled::as_label(h2_02)) %>%
  mutate(irr_num = sjlabelled::as_label(h2_04_1)) %>%
  mutate(cash_cost = sjlabelled::as_label(h2_05)) %>%
  mutate(water_lack_month = sjlabelled::as_label(h2_07)) %>%
  mutate(harvest_qntty = sjlabelled::as_label(h2_10)) 

BIHS3_hhsecH2 <-
  BIHS3_hhsecH2 %>% 
  select(hhid, plotno, water_source, irr_method)
BIHS3_hhsecH1 <- read_dta("../BIHSRound3/Male/021_bihs_r3_male_mod_h1.dta")
names(BIHS3_hhsecH1)
str(BIHS3_hhsecH1)


BIHS3_hhsecH1 <- 
  BIHS3_hhsecH1 %>% mutate(hhid = a01,
                           plotno =h1_sl) %>%
  mutate(paddy_vrty = sjlabelled::as_label(h1_01)) %>%
  mutate(crop_sea = sjlabelled::as_label(h1_season)) %>%
  mutate(crop_1 = sjlabelled::as_label(crop_a_h1)) %>%
  mutate(crop_1oth = sjlabelled::as_label(crop_aoth_h1)) %>%
  mutate(crop_2 = sjlabelled::as_label(crop_b_h1)) %>%
  mutate(crop_2oth = sjlabelled::as_label(crop_both_h1)) %>%
  mutate(vrty_typ = sjlabelled::as_label(h1_02)) %>%
  mutate(plntd_area = sjlabelled::as_label(h1_03)) %>%
  mutate(seed_source_cr1 = sjlabelled::as_label(h1_05a)) %>%
  mutate(seed_source_cr2 = sjlabelled::as_label(h1_05b)) %>%
  mutate(line_sowing = sjlabelled::as_label(h1_05c)) %>%
  mutate(seed_cost_tk = sjlabelled::as_label(h1_07)) %>%
  mutate(seedlng_cost_tk = sjlabelled::as_label(h1_08)) %>%
  mutate(last_sd_lng_prchs_dte = sjlabelled::as_label(h1_09))

BIHS3_hhsecH1 <-
  BIHS3_hhsecH1 %>% 
  filter(crop_a_h1%in%c(10:20)|crop_b_h1%in%c(10:20)) %>%
  select(hhid, plotno, crop_sea, plntd_area) 

BIHS3_hhirr  <-
  left_join(BIHS3_hhsecH1, BIHS3_hhsecH2, by = c("hhid", "plotno"))
BIHS3_hhirr <-
  BIHS3_hhirr %>% 
  filter(!is.na(plntd_area))
names(BIHS3_hhirr)
BIHS3_hhirr %>% 
  count(water_source, irr_method) %>% 
  print(n=Inf)
BIHS3_hhirr <-
  BIHS3_hhirr %>% 
  mutate(irr = case_when(irr_method == "Rainfed"~irr_method, 
                         TRUE ~ "Irrigated"))
BIHS3_hhirr %>% 
  count(irr)
BIHS3_hhirr <-
  BIHS3_hhirr %>% 
  mutate(crop_sea = case_when(str_detect(crop_sea, pattern = "Boro") ~ "Boro",
                              str_detect(crop_sea, pattern = "Aus") ~ "Aus",
                              str_detect(crop_sea, pattern = "Aman") ~ "Aman")) %>% 
  group_by(hhid, crop_sea, irr) %>% 
  summarise(plntd_area = sum(plntd_area, na.rm = TRUE)) %>% 
  pivot_wider(id_cols = hhid, 
              names_from = c("crop_sea", "irr"), 
              values_from = plntd_area, 
              values_fill = 0) %>% 
  mutate(irr_ratio_Boro = Boro_Irrigated/(Boro_Irrigated+Boro_Rainfed),
         irr_ratio_Aman = Aman_Irrigated/(Aman_Irrigated+Aman_Rainfed),
         irr_ratio_Aus = Aus_Irrigated/(Aus_Irrigated+Aus_Rainfed)) %>% 
  select(hhid, contains("irr_ratio"))

write_dta(BIHS3_hhirr, "BIHS3_hhirr.dta")

