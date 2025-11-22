#### section D1 : assets

BIHS3_hhsec_d1 <- read_dta("../BIHSRound3/Male/015_bihs_r3_male_mod_d1.dta") 
str(BIHS3_hhsec_d1)

BIHS3_hhsec_d1  <- 
  BIHS3_hhsec_d1 %>% 
  mutate(hhid = a01,
         asset_type = sjlabelled::as_label(d1_02),
         asset_value = d1_10,
         asset_purpose = sjlabelled::as_label(d1_11)) %>% 
  select(hhid, asset_type, asset_value, asset_purpose) %>% 
  group_by(hhid, asset_purpose) %>% 
  summarize(asset_value = sum(asset_value, na.rm = TRUE)) %>% 
  pivot_wider(id_cols = hhid,
              names_from = asset_purpose,
              values_from = asset_value,
              values_fill = 0,
              names_glue = "assetvalue_{asset_purpose}") 

BIHS3_hhsec_d1 <-   
  BIHS3_hhsec_d1 %>% 
  rowwise() %>% 
  mutate(totalasset = sum(c_across(contains("assetvalue"))))

BIHS3_hhsec_d1

BIHS3_hhsec_d1 <-
  BIHS3_hhsec_d1 %>% select(hhid, totalasset)
write_dta(BIHS3_hhsec_d1, "BIHS_hhsec_d1.dta")
