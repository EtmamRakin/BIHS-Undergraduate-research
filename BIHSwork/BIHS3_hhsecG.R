#### this is a code for section G  -> owning land

library(tidyverse)
library(haven)

## reading original data file

BIHS3_hhsecG <- read_dta("../BIHSRound3/Male/020_bihs_r3_male_mod_g.dta")
names(BIHS3_hhsecG)
str(BIHS3_hhsecG)

BIHS3_hhsecG <- 
  BIHS3_hhsecG %>% mutate(hhid = a01,
                           hh_plotno =plotid) %>%
  mutate(plot_type = sjlabelled::as_label(g01)) %>% 
  mutate(plot_size = sjlabelled::as_label(g02)) %>% 
  mutate(plot_status = sjlabelled::as_label(g20)) %>% 
  mutate(dis_from_h = sjlabelled::as_label(g03)) %>% 
  mutate(flood_depth  = sjlabelled::as_label(g04)) %>% 
  mutate(soil_type = sjlabelled::as_label(g05)) %>% 
  mutate(current_stat = sjlabelled::as_label(g06)) %>% 
  mutate(owner_shrcrpd = sjlabelled::as_label(g21)) %>% 
  mutate(owner_shrcrpd_rsdnc = sjlabelled::as_label(g22))  %>% 
  mutate(owner_shrcrpd_socio_eco = sjlabelled::as_label(g23)) %>% 
  mutate(land_price = sjlabelled::as_label(g10)) %>% 
  mutate(plot_uti_type = sjlabelled::as_label(g14)) %>% 
  mutate(labor_id = sjlabelled::as_label(g15a)) %>% 
  mutate(dec_maker_crop_a = sjlabelled::as_label(g16a)) %>% 
  mutate(dec_maker_crop_b = sjlabelled::as_label(g16b)) %>%
  mutate(dec_maker_crop_c = sjlabelled::as_label(g16c)) %>%
  mutate(dec_maker_inputs_a = sjlabelled::as_label(g17a)) %>% 
  mutate(dec_maker_inputs_b = sjlabelled::as_label(g17b)) %>% 
  mutate(dec_maker_inputs_c = sjlabelled::as_label(g17c)) 
  

BIHS3_hhsecG %>% 
#  filter(!as.numeric(current_stat) %in% c(3, 4, 5, 9, 11, 13, 15)) %>% 
  count(current_stat)

BIHS3_hhsecG %>% 
  filter(is.na(current_stat)) %>% 
  filter(plot_type == "Cultivable/arable land") %>% 
  count(plot_status)



BIHS3_hhsecG %>% 
  count(plot_type, plot_status) %>% 
  spread(plot_type, n)
  


#### conputing the land owning (for crop)
## plot_type: (2) arable land and (5) Waste/non-arable land 
## current status : (1) (2) , out (6)-(8), (10), (14) (16)

BIHS3_hhsecG %>% 
  filter(is.na(current_stat)) %>% 
  count(plot_size)

BIHS3_hhsecG %>% 
  group_by(current_stat) %>% 
  summarise(mean_size = mean(plot_size, na.rm = TRUE))
### -> current status = "NA" has 0 plot size
### we can delete current status = NA
BIHS3_hhsecG <-
  BIHS3_hhsecG %>% 
  filter(as.numeric(current_stat) %in% c(1, 2, 6, 7, 8, 10, 14, 16)) %>% 
  filter(as.numeric(plot_type) %in% c(2,5)) %>% 
  group_by(hhid) %>% 
  summarise(ownland_ha = sum(plot_size, na.rm = TRUE)*40.47/10000)

BIHS3_hhsecG <-
  BIHS3_hhsecG %>% 
  select(!contains("g0")) %>% 
  select(!contains("g1")) %>% 
  select(!contains("g2"))
names(BIHS3_hhsecG)



write_dta(BIHS3_hhsecG, "BIHS3_secG.dta")

