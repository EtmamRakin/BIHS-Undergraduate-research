library(tidyverse)
library(haven)

## reading original data file

BIHS3_hhsecd2 <- read_dta("../BIHSRound3/Male/016_bihs_r3_male_mod_d2.dta")
names(BIHS3_hhsecd2)
str(BIHS3_hhsecd2)

BIHS3_hhsecd2  <- 
  BIHS3_hhsecd2 %>% 
  mutate(hhid = a01,
         asset_type = sjlabelled::as_label(d2_02),
         asset_type_code = as.numeric(d2_02),
         asset_value = d2_10) %>% 
  select(hhid, asset_type, asset_type_code, asset_value) %>% 
  mutate(type = case_when(asset_type_code %in% c(12:29, 36, 37, 38) ~ "Machine",
                          TRUE ~ "OtherthanMachine"))
  
  BIHS3_hhsecd2 <-  
    BIHS3_hhsecd2 %>% 
    group_by(hhid, type) %>% 
  summarize(asset_value = sum(asset_value, na.rm = TRUE)) %>% 
    pivot_wider(id_cols = hhid,
                names_from = type,
                values_from = asset_value,
                names_glue = "{type}_assetvalue",
                values_fill = 0)
write_dta(BIHS3_hhsecd2, "BIHS3_hhsecd2.dta")
  