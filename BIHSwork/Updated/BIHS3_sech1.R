#### this is a code for section H

library(tidyverse)
library(haven)
library(ggthemes)

## reading original data file

BIHS3_hhsecH1 <- read_dta("../BIHSRound3/Male/021_bihs_r3_male_mod_h1.dta")
names(BIHS3_hhsecH1)
str(BIHS3_hhsecH1)


BIHS3_hhsecH1 <- 
  BIHS3_hhsecH1 %>% mutate(hhid = a01,
                           hh_plotno =h1_sl) %>%
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

#### observations by varieties 

df_paddyvar <- BIHS3_hhsecH1 %>%
  count(paddy_vrty, vrty_typ) 

df_paddyvar %>% data.frame()


library(writexl)

write_xlsx(df_paddyvar, "df_paddyvarieties.xlsx")

#### planted area by varieties

BIHS3_hhsecH1 <- 
  BIHS3_hhsecH1 %>%
  mutate(plntd_area_ha = plntd_area * 40.47/10000) ### decimal to hectare


BIHS3_hhsecH1_paddytype_season <- 
  BIHS3_hhsecH1 %>%
  filter(crop_a_h1%in%c(10:20)|crop_b_h1%in%c(10:20)) %>% 
  group_by(hhid,crop_sea, vrty_typ) %>%
  summarize(plnt_area_paddytype_ha = sum(plntd_area_ha, na.rm = TRUE))

BIHS3_hhsecH1_paddytype_season 

BIHS3_hhsecH1_paddytype_season_wide <-BIHS3_hhsecH1_paddytype_season %>% 
  pivot_wider(id_cols = c(hhid, crop_sea),
              names_from = vrty_typ,
              values_from = plnt_area_paddytype_ha,
              values_fill = 0)

BIHS3_hhsecH1_paddytype_boro_wide <-BIHS3_hhsecH1_paddytype_season_wide %>% 
  filter(crop_sea == "Boro (robi)")
BIHS3_hhsecH1_paddytype_aman_wide <-BIHS3_hhsecH1_paddytype_season_wide %>% 
  filter(crop_sea == "Aman/ (kharif 2)")

#### introduce hh basic information
source("BIHS3_HHroster.R")

BIHS3_hhsecH1_paddytype_season  <-
  left_join(BIHS3_hhsecH1_paddytype_season ,
            BIHS3_hhsecA %>% select(hhid, div, div_name, district),
            by = "hhid", relationship = "many-to-many")

BIHS3_hhsecH1_paddytype_season  <- 
  BIHS3_hhsecH1_paddytype_season %>% 
  group_by(hhid, crop_sea) %>% 
  mutate(plnt_area_paddy_ha = sum(plnt_area_paddytype_ha, na.rm = TRUE))

BIHS3_hhsecH1_paddytype_season %>% 
  filter(crop_sea == "Boro (robi)") %>%
  ggplot() +
  geom_histogram(aes(x= plnt_area_paddy_ha)) +
  facet_wrap( ~ div_name)




### by division and season
BIHS3_hhsecH1_paddytype_season %>%
  filter(plnt_area_paddytype_ha>0) %>% 
  group_by(div_name, crop_sea, vrty_typ) %>%
  summarize(area = sum(plnt_area_paddytype_ha, na.rm = TRUE)) %>% 
  ggplot() + aes(x=div_name, y = area, fill = vrty_typ) +
  geom_bar(stat = "identity") +
  facet_wrap(~ crop_sea) 

BIHS3_hhsecH1_paddytype_season %>%
  filter(plnt_area_paddytype_ha>0) %>% 
  group_by(div_name, crop_sea, vrty_typ) %>%
  summarize(area = sum(plnt_area_paddytype_ha, na.rm = TRUE)) %>% 
  ggplot() + aes(x=div_name, y = area, fill = vrty_typ) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ crop_sea) 

### by plant size scale for Boro
 

















#Boro rice only
BIHS3_hhsecH1_paddytype_boro_wide <-
  left_join(BIHS3_hhsecH1_paddytype_boro_wide,
            BIHS3_hhsecA,
            by= "hhid")

BIHS3_hhsecH1_paddytype_boro_wide <-
  BIHS3_hhsecH1_paddytype_boro_wide %>% ungroup() %>% 
  mutate(paddytype=case_when(`Local Variety`>0 & HYV==0 & Hybrid == 0~"L",
                        `Local Variety`==0 & HYV>0 & Hybrid == 0~"V",
                        `Local Variety`==0 & HYV==0 & Hybrid > 0~"B",
                        `Local Variety`>0 & HYV>0 & Hybrid == 0~"LV",
                        `Local Variety`>0 & HYV==0 & Hybrid >0~"LB",
                        `Local Variety`==0 & HYV>0 & Hybrid>0~"VB",
                        `Local Variety`>0 & HYV>0 & Hybrid>0~"LVB",
                        TRUE~NA_character_)) %>% 
  mutate(paddytype=factor(paddytype,levels=c("L","LV","LB","VB","LVB","B","V")))
BIHS3_hhsecH1_paddytype_boro_wide %>% 
  count(paddytype,div_name) %>% 
  spread(paddytype,n)

BIHS3_hhsecH1_paddytype_boro_wide %>% 
  count(paddytype,div_name) %>% 
  ggplot()+
  aes(x=div_name,y=n,fill=paddytype)+
  geom_bar(stat = "identity")+
  scale_fill_tableau()

BIHS3_hhsecH1_paddytype_boro_wide %>% 
  count(paddytype,div_name) %>% 
  ggplot()+
  aes(x=div_name,y=n,fill=paddytype)+
  geom_bar(stat = "identity",position = "fill")+
  labs(x="Division name",y="Share of Households",fill="Type of variety choices",
       title = "Region based household share of rice variety choices",
       subtitle="Boro season",
       caption = "BIHS,2018-2019
       L=local only, V = HYV, B = hybrid, 
       LV = local + HYV , LB = local + hybrid, VB = HYV + hybrid,
       LVB = local + HYV + hybrid")

ggsave("Region based household share of rice variety choices (BORO).png",width = 18, height = 12, units = "cm")

#area share by variety
BIHS3_hhsecH1_paddytype_boro_wide <-
  BIHS3_hhsecH1_paddytype_boro_wide %>% 
  mutate(total_area = `Local Variety`+ HYV + Hybrid) %>% 
  mutate(area_share_local = `Local Variety`/total_area, 
         area_share_HYV = HYV/total_area,
         area_share_hybrid = Hybrid/total_area)

quantile(BIHS3_hhsecH1_paddytype_boro_wide$total_area)
BIHS3_hhsecH1_paddytype_boro_wide <-
BIHS3_hhsecH1_paddytype_boro_wide %>% 
mutate(plntsize_class = case_when(total_area <= 0.3 ~ "small",
                                  total_area > 0.3 & total_area <= 0.6 ~ "medium",
                                  total_area > 0.6 ~ "large"),
       plntsize_class = factor(plntsize_class, levels = c("small", "medium", "large")))
BIHS3_hhsecH1_paddytype_boro_wide %>% count(plntsize_class,div_name,paddytype) %>% 
  ggplot()+
  aes(x=plntsize_class,y=n,fill=paddytype)+
  geom_bar(stat = "identity",position = "fill")+
  facet_wrap(~div_name)

BIHS3_hhsecH1_paddytype_boro_wide %>% 
  group_by(div_name) %>% 
  summarize(local=mean(`Local Variety`),
            HYV=mean(HYV),
            Hybrid=mean(Hybrid)) %>% 
  pivot_longer(cols = -div_name,
               names_to = "type",
               values_to = "average_area") %>% 
  ggplot()+
  aes(x=div_name, y=average_area, fill=type)+
  geom_bar(stat = "identity") + 
  labs(title = "Region based household farm area average by variety types", 
       subtitle="Boro season",
       caption = "Data source = BIHS18-19", 
       x="division name",
       y="area by variety types (ha/household)")

ggsave("Region based household farm area average by variety types (BORO).png",width = 18, height = 12, units = "cm")


#aman rice only
BIHS3_hhsecH1_paddytype_aman_wide <-
  left_join(BIHS3_hhsecH1_paddytype_aman_wide,
            BIHS3_hhsecA,
            by= "hhid")

BIHS3_hhsecH1_paddytype_aman_wide <-
  BIHS3_hhsecH1_paddytype_aman_wide %>% ungroup() %>% 
  mutate(paddytype=case_when(`Local Variety`>0 & HYV==0 & Hybrid == 0~"L",
                             `Local Variety`==0 & HYV>0 & Hybrid == 0~"V",
                             `Local Variety`==0 & HYV==0 & Hybrid > 0~"B",
                             `Local Variety`>0 & HYV>0 & Hybrid == 0~"LV",
                             `Local Variety`>0 & HYV==0 & Hybrid >0~"LB",
                             `Local Variety`==0 & HYV>0 & Hybrid>0~"VB",
                             `Local Variety`>0 & HYV>0 & Hybrid>0~"LVB",
                             TRUE~NA_character_)) %>% 
  mutate(paddytype=factor(paddytype,levels=c("L","LV","LB","VB","LVB","B","V")))
BIHS3_hhsecH1_paddytype_aman_wide %>% 
  count(paddytype,div_name) %>% 
  spread(paddytype,n)

BIHS3_hhsecH1_paddytype_aman_wide %>% 
  count(paddytype,div_name) %>% 
  ggplot()+
  aes(x=div_name,y=n,fill=paddytype)+
  geom_bar(stat = "identity")+
  scale_fill_tableau()

BIHS3_hhsecH1_paddytype_aman_wide %>% 
  count(paddytype,div_name) %>% 
  ggplot()+
  aes(x=div_name,y=n,fill=paddytype)+
  geom_bar(stat = "identity",position = "fill")+
  labs(x="Division name",y="Share of Households",fill="Type of variety choices",
       title = "Region based household share of rice variety choices",
       subtitle="Aman season",
       caption = "BIHS,2018-2019
       L=local only, V = HYV, B = hybrid, 
       LV = local + HYV , LB = local + hybrid, VB = HYV + hybrid,
       LVB = local + HYV + hybrid")

ggsave("Region based household share of rice variety choices (Aman).png",width = 18, height = 12, units = "cm")

#area share by variety
BIHS3_hhsecH1_paddytype_aman_wide <-
  BIHS3_hhsecH1_paddytype_aman_wide %>% 
  mutate(total_area = `Local Variety`+ HYV + Hybrid) %>% 
  mutate(area_share_local = `Local Variety`/total_area, 
         area_share_HYV = HYV/total_area,
         area_share_hybrid = Hybrid/total_area)
BIHS3_hhsecH1_paddytype_aman_wide %>% count(paddytype)
quantile(BIHS3_hhsecH1_paddytype_aman_wide$total_area)
BIHS3_hhsecH1_paddytype_aman_wide <-
  BIHS3_hhsecH1_paddytype_aman_wide %>% 
  mutate(plntsize_class = case_when(total_area <= 0.3 ~ "small",
                                    total_area > 0.3 & total_area <= 0.6 ~ "medium",
                                    total_area > 0.6 ~ "large"),
         plntsize_class = factor(plntsize_class, levels = c("small", "medium", "large")))
BIHS3_hhsecH1_paddytype_aman_wide %>% count(plntsize_class,div_name,paddytype) %>% 
  ggplot()+
  aes(x=plntsize_class,y=n,fill=paddytype)+
  geom_bar(stat = "identity",position = "fill")+
  facet_wrap(~div_name)

BIHS3_hhsecH1_paddytype_aman_wide %>% 
  group_by(div_name) %>% 
  summarize(local=mean(`Local Variety`),
            HYV=mean(HYV),
            Hybrid=mean(Hybrid)) %>% 
  pivot_longer(cols = -div_name,
               names_to = "type",
               values_to = "average_area") %>% 
  ggplot()+
  aes(x=div_name, y=average_area, fill=type)+
  geom_bar(stat = "identity") + 
  labs(title = "Region based household farm area average by variety types", 
       subtitle="aman season",
       caption = "Data source = BIHS18-19", 
       x="division name",
       y="area by variety types (ha/household)")

ggsave("Region based household farm area average by variety types (Aman).png",width = 18, height = 12, units = "cm")


### final data in this section
names(BIHS3_hhsecH1_paddytype_season_wide)

BIHS3_hhsecH1_paddytype_season_wide %>% ungroup() %>% 
  count(crop_sea)

BIHS3_hhsecH1_paddytype_season_wide<- 
 BIHS3_hhsecH1_paddytype_season_wide %>% 
  rename(Local = "Local Variety") %>% 
  mutate(crop_sea = case_when(str_detect(crop_sea, pattern = "Boro") ~ "Boro",
                              str_detect(crop_sea, pattern = "Aus") ~ "Aus",
                              str_detect(crop_sea, pattern = "Aman") ~ "Aman")) 


BIHS3_paddytypearea_season_wide <- 
BIHS3_hhsecH1_paddytype_season_wide %>% 
  pivot_longer(cols = -c(hhid, crop_sea),
               names_to = "variety",
               values_to = "ha") %>% 
  pivot_wider(id_cols = hhid,
              names_from = c("crop_sea", "variety"),
              values_from = "ha",
              names_glue = "area_{crop_sea}_{variety}",
              values_fill = 0)

#### final data is save by dta format.

write_dta(BIHS3_paddytypearea_season_wide, "BIHS3_paddytypearea_season_wide.dta")        
