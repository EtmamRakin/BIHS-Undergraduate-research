#### Estimation part

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

#### reading main data
source("BIHS3_dfest_main.R")

BIHS3_dfest <-
  read_dta("BIHS3_dfest.dta")

### check
names(BIHS3_dfest)
names(BIHS3_dfest_rice)
str(BIHS3_dfest_rice)

#### rice farmers only
BIHS3_dfest_rice <- 
  BIHS3_dfest %>% 
  filter(!is.na(Adopt_Hybrid)) %>% 
  filter(!is.na(Adopt_HYV)) %>% 
  filter(!is.na(Adopt_Local)) 


#####
names(BIHS3_dfest_rice)

## regressors set
## landsize
regressors1 <- 
  c("ownland_ha", 
    "hhsize", 
    "Male_1564", "Female_1564", "Male_65ov", "Female_65ov",
    "fhh", "hh_age",
    "hh_educp", "hh_educh")

## education
regressors2 <-
  c("ownland_ha", 
    "hhsize", 
    "Male_1564", "Female_1564", "Male_65ov", "Female_65ov", 
    "fhh", "hh_age",                       
    "Male_mx_edlvl","Female_mx_edlvl", 
     "literacy_status" )

#asset
regressors3 <-
  c("ownland_ha", 
    "hhsize", 
    "Male_1564", "Female_1564", "Male_65ov", "Female_65ov",
    "fhh", "hh_age", "totalasset", "OtherthanMachine_assetvalue", "Machine_assetvalue"  )

#Membership/community
regressors4 <-
  c("ownland_ha", 
    "fhh", "hh_age",
    "hhsize", 
    "Male_1564", "Female_1564", "Male_65ov", "Female_65ov",
    "advice_fert", "advice_seed", "irr_ratio_Boro",                  
   "agrcultrl_fish_lstock_groupmem", "Machine_assetvalue")

#Past experience
regressors5 <-
  c("ownland_ha", 
    "hhsize", 
    "Male_1564", "Female_1564", "Male_65ov", "Female_65ov",
    "fhh", "hh_age",
    "BRRIdhan", "Bina", "GutiUrea" ,                     
    "Planting" ,  "Pump", "Sprayer" ,                      
    "Thresher", "Tractor", "Machine_assetvalue")

#technology
regressors6 <-
  c("ownland_ha", 
    "hhsize", 
    "Male_1564", "Female_1564", "Male_65ov", "Female_65ov",
    "fhh", "hh_age",
    "BRRIdhan", "Bina", "GutiUrea" ,                     
    "Planting" ,  "Pump", "Sprayer" ,                      
    "Thresher", "Tractor", "Machine_assetvalue", "irr_ratio_Boro")




#r1
regressors1_country <-
  c("div_name", regressors1)

regressors1_country

regressors1_div <-
  c("as.factor(district)", regressors1)

#r2
regressors2_country <- 
  c("div_name", regressors2)

regressors2_country

regressors2_div <-
  c("as.factor(district)", regressors2)

#r3
regressors3_country <- 
  c("div_name", regressors3)

regressors3_country

regressors3_div <-
  c("as.factor(district)", regressors3)


#r4
regressors4_country <- 
  c("div_name", regressors4)

regressors4_country

regressors4_div <-
  c("as.factor(district)", regressors4)



#r5
regressors5_country <-
  c("div_name", regressors5)

regressors5_country

regressors5_div <-
  c("as.factor(district)", regressors5)


#r6
regressors6_country <- 
  c("div_name", regressors6)

regressors6_country

regressors6_div <-
  c("as.factor(district)", regressors6)

#estimation default
eq_area_Aman_HYV <- 
  as.formula(paste0("area_Aman_HYV ~ ", ### dependent(LHS) variables
                    paste0(regressors1_country, collapse = " + ")))


eq_area_Aman_Hybrid <- 
  as.formula(paste0("area_Aman_Hybrid ~ ", ### dependent(LHS) variables
                    paste0(regressors1_country, collapse = " + ")))

eq_area_Aman_Local <- 
  as.formula(paste0("area_Aman_Local ~ ", ### dependent(LHS) variables
                    paste0(regressors1_country, collapse = " + ")))

eq_area_Boro_HYV <- 
  as.formula(paste0("area_Boro_HYV ~ ", ### dependent(LHS) variables
                    paste0(regressors1_country, collapse = " + ")))

eq_area_Boro_Hybrid <- 
  as.formula(paste0("area_Boro_Hybrid ~ ", ### dependent(LHS) variables
                    paste0(regressors1_country, collapse = " + ")))

###########--------------
#estimation check
eq_area_Aman_HYV <- 
  as.formula(paste0("area_Aman_HYV ~ ", ### dependent(LHS) variables
                    paste0(regressors4_country, collapse = " + ")))


eq_area_Aman_Hybrid <- 
  as.formula(paste0("area_Aman_Hybrid ~ ", ### dependent(LHS) variables
                    paste0(regressors4_country, collapse = " + ")))

eq_area_Aman_Local <- 
  as.formula(paste0("area_Aman_Local ~ ", ### dependent(LHS) variables
                    paste0(regressors4_country, collapse = " + ")))

eq_area_Boro_HYV <- 
  as.formula(paste0("area_Boro_HYV ~ ", ### dependent(LHS) variables
                    paste0(regressors4_country, collapse = " + ")))

eq_area_Boro_Hybrid <- 
  as.formula(paste0("area_Boro_Hybrid ~ ", ### dependent(LHS) variables
                    paste0(regressors4_country, collapse = " + ")))


##########-------------

############################
# check Aman 
###########################
eq_area_Aman_HYV

ols_area_Aman_HYV <- 
  lm(eq_area_Aman_HYV, data = BIHS3_dfest_rice)
summary(ols_area_Aman_HYV)             
coeftest(ols_area_Aman_HYV, vcov. = vcovCL(ols_area_Aman_HYV, cluster =  ~ clid))

eq_area_Aman_Hybrid

ols_area_Aman_Hybrid <- 
  lm(eq_area_Aman_Hybrid, data = BIHS3_dfest_rice)
summary(ols_area_Aman_Hybrid)             
coeftest(ols_area_Aman_Hybrid, vcov. = vcovCL(ols_area_Aman_Hybrid, cluster =  ~ clid))

eq_area_Aman_Local


ols_area_Aman_Local <- 
  lm(eq_area_Aman_Local, data = BIHS3_dfest_rice)
summary(ols_area_Aman_Local)             
coeftest(ols_area_Aman_Local, vcov. = vcovCL(ols_area_Aman_Local, cluster =  ~ clid))

###########################
#check Boro - error showing same result
###########################
eq_area_Boro_HYV

ols_area_Boro_HYV <- 
  lm(eq_area_Boro_HYV, data = BIHS3_dfest_rice)
summary(ols_area_Boro_HYV)             
coeftest(ols_area_Boro_HYV, vcov. = vcovCL(ols_area_Boro_HYV, cluster =  ~ clid))

eq_area_Boro_Hybrid

ols_area_Boro_Hybrid <- 
  lm(eq_area_Boro_Hybrid, data = BIHS3_dfest_rice)
summary(ols_area_Boro_Hybrid)             
coeftest(ols_area_Boro_Hybrid, vcov. = vcovCL(ols_area_Boro_Hybrid, cluster =  ~ clid))






### for estimation of division Aman HYV
eq_area_Aman_HYV_div <- 
  as.formula(paste0("area_Aman_HYV ~ ", ### dependent(LHS) variables
                    paste0(regressors1_div, collapse = " + ")))


ols_area_Aman_HYV_Dhaka <- 
  lm(eq_area_Aman_HYV_div, data = BIHS3_dfest_rice %>% filter(div_name == "Dhaka"))
summary(ols_area_Aman_HYV_Dhaka)             
coeftest(ols_area_Aman_HYV_Dhaka, vcov. = vcovCL(ols_area_Aman_HYV_Dhaka, cluster =  ~ clid))

ols_area_Aman_HYV_Barisal <- 
  lm(eq_area_Aman_HYV_div, data = BIHS3_dfest_rice %>% filter(div_name == "Barisal"))
summary(ols_area_Aman_HYV_Barisal)             
coeftest(ols_area_Aman_HYV_Barisal, vcov. = vcovCL(ols_area_Aman_HYV_Barisal, cluster =  ~ clid))

ols_area_Aman_HYV_Chittagong <- 
  lm(eq_area_Aman_HYV_div, data = BIHS3_dfest_rice %>% filter(div_name == "Chittagong"))
summary(ols_area_Aman_HYV_Chittagong)             
coeftest(ols_area_Aman_HYV_Chittagong, vcov. = vcovCL(ols_area_Aman_HYV_Chittagong, cluster =  ~ clid))

ols_area_Aman_HYV_Khulna <- 
  lm(eq_area_Aman_HYV_div, data = BIHS3_dfest_rice %>% filter(div_name == "Khulna"))
summary(ols_area_Aman_HYV_Khulna)             
coeftest(ols_area_Aman_HYV_Khulna, vcov. = vcovCL(ols_area_Aman_HYV_Khulna, cluster =  ~ clid))

ols_area_Aman_HYV_Rajshahi <- 
  lm(eq_area_Aman_HYV_div, data = BIHS3_dfest_rice %>% filter(div_name == "Rajshahi"))
summary(ols_area_Aman_HYV_Rajshahi)             
coeftest(ols_area_Aman_HYV_Rajshahi, vcov. = vcovCL(ols_area_Aman_HYV_Rajshahi, cluster =  ~ clid))

ols_area_Aman_HYV_Rangpur <- 
  lm(eq_area_Aman_HYV_div, data = BIHS3_dfest_rice %>% filter(div_name == "Rangpur"))
summary(ols_area_Aman_HYV_Rangpur)             
coeftest(ols_area_Aman_HYV_Rangpur, vcov. = vcovCL(ols_area_Aman_HYV_Rangpur, cluster =  ~ clid))

ols_area_Aman_HYV_Sylhet <- 
  lm(eq_area_Aman_HYV_div, data = BIHS3_dfest_rice %>% filter(div_name == "Sylhet"))
summary(ols_area_Aman_HYV_Sylhet)             
coeftest(ols_area_Aman_HYV_Sylhet, vcov. = vcovCL(ols_area_Aman_HYV_Sylhet, cluster =  ~ clid))


### for estimation of division Aman Hybrid
eq_area_Aman_Hybrid_div <- 
  as.formula(paste0("area_Aman_Hybrid ~ ", ### dependent(LHS) variables
                    paste0(regressors1_div, collapse = " + ")))


ols_area_Aman_Hybrid_Dhaka <- 
  lm(eq_area_Aman_Hybrid_div, data = BIHS3_dfest_rice %>% filter(div_name == "Dhaka"))
summary(ols_area_Aman_Hybrid_Dhaka)             
coeftest(ols_area_Aman_Hybrid_Dhaka, vcov. = vcovCL(ols_area_Aman_Hybrid_Dhaka, cluster =  ~ clid))

ols_area_Aman_Hybrid_Barisal <- 
  lm(eq_area_Aman_Hybrid_div, data = BIHS3_dfest_rice %>% filter(div_name == "Barisal"))
summary(ols_area_Aman_Hybrid_Barisal)             
coeftest(ols_area_Aman_Hybrid_Barisal, vcov. = vcovCL(ols_area_Aman_Hybrid_Barisal, cluster =  ~ clid))

ols_area_Aman_Hybrid_Chittagong <- 
  lm(eq_area_Aman_Hybrid_div, data = BIHS3_dfest_rice %>% filter(div_name == "Chittagong"))
summary(ols_area_Aman_Hybrid_Chittagong)             
coeftest(ols_area_Aman_Hybrid_Chittagong, vcov. = vcovCL(ols_area_Aman_Hybrid_Chittagong, cluster =  ~ clid))

ols_area_Aman_Hybrid_Khulna <- 
  lm(eq_area_Aman_Hybrid_div, data = BIHS3_dfest_rice %>% filter(div_name == "Khulna"))
summary(ols_area_Aman_Hybrid_Khulna)             
coeftest(ols_area_Aman_Hybrid_Khulna, vcov. = vcovCL(ols_area_Aman_Hybrid_Khulna, cluster =  ~ clid))

ols_area_Aman_Hybrid_Rajshahi <- 
  lm(eq_area_Aman_Hybrid_div, data = BIHS3_dfest_rice %>% filter(div_name == "Rajshahi"))
summary(ols_area_Aman_Hybrid_Rajshahi)             
coeftest(ols_area_Aman_Hybrid_Rajshahi, vcov. = vcovCL(ols_area_Aman_Hybrid_Rajshahi, cluster =  ~ clid))

ols_area_Aman_Hybrid_Rangpur <- 
  lm(eq_area_Aman_Hybrid_div, data = BIHS3_dfest_rice %>% filter(div_name == "Rangpur"))
summary(ols_area_Aman_Hybrid_Rangpur)             
coeftest(ols_area_Aman_Hybrid_Rangpur, vcov. = vcovCL(ols_area_Aman_Hybrid_Rangpur, cluster =  ~ clid))

ols_area_Aman_Hybrid_Sylhet <- 
  lm(eq_area_Aman_Hybrid_div, data = BIHS3_dfest_rice %>% filter(div_name == "Sylhet"))
summary(ols_area_Aman_Hybrid_Sylhet)             
coeftest(ols_area_Aman_Hybrid_Sylhet, vcov. = vcovCL(ols_area_Aman_Hybrid_Sylhet, cluster =  ~ clid))


#### for estimation of division Aman local
eq_area_Aman_Local_div <- 
  as.formula(paste0("area_Aman_Local ~ ", ### dependent(LHS) variables
                    paste0(regressors1_div, collapse = " + ")))


ols_area_Aman_Local_Dhaka <- 
  lm(eq_area_Aman_Local_div, data = BIHS3_dfest_rice %>% filter(div_name == "Dhaka"))
summary(ols_area_Aman_Local_Dhaka)             
coeftest(ols_area_Aman_Local_Dhaka, vcov. = vcovCL(ols_area_Aman_Local_Dhaka, cluster =  ~ clid))

ols_area_Aman_Local_Barisal <- 
  lm(eq_area_Aman_Local_div, data = BIHS3_dfest_rice %>% filter(div_name == "Barisal"))
summary(ols_area_Aman_Local_Barisal)             
coeftest(ols_area_Aman_Local_Barisal, vcov. = vcovCL(ols_area_Aman_Local_Barisal, cluster =  ~ clid))

ols_area_Aman_Local_Chittagong <- 
  lm(eq_area_Aman_Local_div, data = BIHS3_dfest_rice %>% filter(div_name == "Chittagong"))
summary(ols_area_Aman_Local_Chittagong)             
coeftest(ols_area_Aman_Local_Chittagong, vcov. = vcovCL(ols_area_Aman_Local_Chittagong, cluster =  ~ clid))

ols_area_Aman_Local_Khulna <- 
  lm(eq_area_Aman_Local_div, data = BIHS3_dfest_rice %>% filter(div_name == "Khulna"))
summary(ols_area_Aman_Local_Khulna)             
coeftest(ols_area_Aman_Local_Khulna, vcov. = vcovCL(ols_area_Aman_Local_Khulna, cluster =  ~ clid))

ols_area_Aman_Local_Rajshahi <- 
  lm(eq_area_Aman_Local_div, data = BIHS3_dfest_rice %>% filter(div_name == "Rajshahi"))
summary(ols_area_Aman_Local_Rajshahi)             
coeftest(ols_area_Aman_Local_Rajshahi, vcov. = vcovCL(ols_area_Aman_Local_Rajshahi, cluster =  ~ clid))

ols_area_Aman_Local_Rangpur <- 
  lm(eq_area_Aman_Local_div, data = BIHS3_dfest_rice %>% filter(div_name == "Rangpur"))
summary(ols_area_Aman_Local_Rangpur)             
coeftest(ols_area_Aman_Local_Rangpur, vcov. = vcovCL(ols_area_Aman_Local_Rangpur, cluster =  ~ clid))

ols_area_Aman_Local_Sylhet <- 
  lm(eq_area_Aman_Local_div, data = BIHS3_dfest_rice %>% filter(div_name == "Sylhet"))
summary(ols_area_Aman_Local_Sylhet)             
coeftest(ols_area_Aman_Local_Sylhet, vcov. = vcovCL(ols_area_Aman_Local_Sylhet, cluster =  ~ clid))












### for estimation of division Boro HYV
eq_area_Boro_HYV_div <- 
  as.formula(paste0("area_Boro_HYV ~ ", ### dependent(LHS) variables
                    paste0(regressors1_div, collapse = " + ")))


ols_area_Boro_HYV_Dhaka <- 
  lm(eq_area_Boro_HYV_div, data = BIHS3_dfest_rice %>% filter(div_name == "Dhaka"))
summary(ols_area_Boro_HYV_Dhaka)             
coeftest(ols_area_Boro_HYV_Dhaka, vcov. = vcovCL(ols_area_Boro_HYV_Dhaka, cluster =  ~ clid))

ols_area_Boro_HYV_Barisal <- 
  lm(eq_area_Boro_HYV_div, data = BIHS3_dfest_rice %>% filter(div_name == "Barisal"))
summary(ols_area_Boro_HYV_Barisal)             
coeftest(ols_area_Boro_HYV_Barisal, vcov. = vcovCL(ols_area_Boro_HYV_Barisal, cluster =  ~ clid))

ols_area_Boro_HYV_Chittagong <- 
  lm(eq_area_Boro_HYV_div, data = BIHS3_dfest_rice %>% filter(div_name == "Chittagong"))
summary(ols_area_Boro_HYV_Chittagong)             
coeftest(ols_area_Boro_HYV_Chittagong, vcov. = vcovCL(ols_area_Boro_HYV_Chittagong, cluster =  ~ clid))

ols_area_Boro_HYV_Khulna <- 
  lm(eq_area_Boro_HYV_div, data = BIHS3_dfest_rice %>% filter(div_name == "Khulna"))
summary(ols_area_Boro_HYV_Khulna)             
coeftest(ols_area_Boro_HYV_Khulna, vcov. = vcovCL(ols_area_Boro_HYV_Khulna, cluster =  ~ clid))

ols_area_Boro_HYV_Rajshahi <- 
  lm(eq_area_Boro_HYV_div, data = BIHS3_dfest_rice %>% filter(div_name == "Rajshahi"))
summary(ols_area_Boro_HYV_Rajshahi)             
coeftest(ols_area_Boro_HYV_Rajshahi, vcov. = vcovCL(ols_area_Boro_HYV_Rajshahi, cluster =  ~ clid))

ols_area_Boro_HYV_Rangpur <- 
  lm(eq_area_Boro_HYV_div, data = BIHS3_dfest_rice %>% filter(div_name == "Rangpur"))
summary(ols_area_Boro_HYV_Rangpur)             
coeftest(ols_area_Boro_HYV_Rangpur, vcov. = vcovCL(ols_area_Boro_HYV_Rangpur, cluster =  ~ clid))

ols_area_Boro_HYV_Sylhet <- 
  lm(eq_area_Boro_HYV_div, data = BIHS3_dfest_rice %>% filter(div_name == "Sylhet"))
summary(ols_area_Boro_HYV_Sylhet)             
coeftest(ols_area_Boro_HYV_Sylhet, vcov. = vcovCL(ols_area_Boro_HYV_Sylhet, cluster =  ~ clid))


### for estimation of division Boro Hybrid
eq_area_Boro_Hybrid_div <- 
  as.formula(paste0("area_Boro_Hybrid ~ ", ### dependent(LHS) variables
                    paste0(regressors1_div, collapse = " + ")))


ols_area_Boro_Hybrid_Dhaka <- 
  lm(eq_area_Boro_Hybrid_div, data = BIHS3_dfest_rice %>% filter(div_name == "Dhaka"))
summary(ols_area_Boro_Hybrid_Dhaka)             
coeftest(ols_area_Boro_Hybrid_Dhaka, vcov. = vcovCL(ols_area_Boro_Hybrid_Dhaka, cluster =  ~ clid))

ols_area_Boro_Hybrid_Barisal <- 
  lm(eq_area_Boro_Hybrid_div, data = BIHS3_dfest_rice %>% filter(div_name == "Barisal"))
summary(ols_area_Boro_Hybrid_Barisal)             
coeftest(ols_area_Boro_Hybrid_Barisal, vcov. = vcovCL(ols_area_Boro_Hybrid_Barisal, cluster =  ~ clid))

ols_area_Boro_Hybrid_Chittagong <- 
  lm(eq_area_Boro_Hybrid_div, data = BIHS3_dfest_rice %>% filter(div_name == "Chittagong"))
summary(ols_area_Boro_Hybrid_Chittagong)             
coeftest(ols_area_Boro_Hybrid_Chittagong, vcov. = vcovCL(ols_area_Boro_Hybrid_Chittagong, cluster =  ~ clid))

ols_area_Boro_Hybrid_Khulna <- 
  lm(eq_area_Boro_Hybrid_div, data = BIHS3_dfest_rice %>% filter(div_name == "Khulna"))
summary(ols_area_Boro_Hybrid_Khulna)             
coeftest(ols_area_Boro_Hybrid_Khulna, vcov. = vcovCL(ols_area_Boro_Hybrid_Khulna, cluster =  ~ clid))

ols_area_Boro_Hybrid_Rajshahi <- 
  lm(eq_area_Boro_Hybrid_div, data = BIHS3_dfest_rice %>% filter(div_name == "Rajshahi"))
summary(ols_area_Boro_Hybrid_Rajshahi)             
coeftest(ols_area_Boro_Hybrid_Rajshahi, vcov. = vcovCL(ols_area_Boro_Hybrid_Rajshahi, cluster =  ~ clid))

ols_area_Boro_Hybrid_Rangpur <- 
  lm(eq_area_Boro_Hybrid_div, data = BIHS3_dfest_rice %>% filter(div_name == "Rangpur"))
summary(ols_area_Boro_Hybrid_Rangpur)             
coeftest(ols_area_Boro_Hybrid_Rangpur, vcov. = vcovCL(ols_area_Boro_Hybrid_Rangpur, cluster =  ~ clid))

ols_area_Boro_Hybrid_Sylhet <- 
  lm(eq_area_Boro_Hybrid_div, data = BIHS3_dfest_rice %>% filter(div_name == "Sylhet"))
summary(ols_area_Boro_Hybrid_Sylhet)             
coeftest(ols_area_Boro_Hybrid_Sylhet, vcov. = vcovCL(ols_area_Boro_Hybrid_Sylhet, cluster =  ~ clid))










#### default
eq_adopt_HYV <-
  as.formula(paste0("Adopt_HYV ~ ", ### dependent(LHS) variables
                    paste0(regressors1_country, collapse = " + ")))

probit_HYV <- probit.reg(eq_adopt_HYV, data = BIHS3_dfest_rice)
summary(probit_HYV)

eq_adopt_Hybrid <-
  as.formula(paste0("Adopt_Hybrid ~ ", ### dependent(LHS) variables
                    paste0(regressors1_country, collapse = " + ")))

probit_Hybrid <- probit.reg(eq_adopt_Hybrid, data = BIHS3_dfest_rice)
summary(probit_Hybrid)

eq_adopt_Local <-
  as.formula(paste0("Adopt_Local ~ ", ### dependent(LHS) variables
                    paste0(regressors1_country, collapse = " + ")))

probit_Local <- probit.reg(eq_adopt_Local, data = BIHS3_dfest_rice)
summary(probit_Local)

####test

eq_adopt_HYV <-
  as.formula(paste0("Adopt_HYV ~ ", ### dependent(LHS) variables
                    paste0(regressors3_country, collapse = " + ")))

probit_HYV <- probit.reg(eq_adopt_HYV, data = BIHS3_dfest_rice)
summary(probit_HYV)

eq_adopt_Hybrid <-
  as.formula(paste0("Adopt_Hybrid ~ ", ### dependent(LHS) variables
                    paste0(regressors3_country, collapse = " + ")))

probit_Hybrid <- probit.reg(eq_adopt_Hybrid, data = BIHS3_dfest_rice)
summary(probit_Hybrid)

eq_adopt_Local <-
  as.formula(paste0("Adopt_Local ~ ", ### dependent(LHS) variables
                    paste0(regressors3_country, collapse = " + ")))

probit_Local <- probit.reg(eq_adopt_Local, data = BIHS3_dfest_rice)
summary(probit_Local)
###hybrid or local probit - replace above
###if by season,
BIHS3_dfest_rice<- 
  BIHS3_dfest_rice %>% 
  mutate(Boro = case_when(Boro_HYV + Boro_Hybrid > 0 ~1,
                          Boro_HYV + Boro_Hybrid == 0 ~ 0),
         Aman = case_when(Aman_HYV + Aman_Hybrid + Aman_Local > 0 ~1,
                          Aman_HYV + Aman_Hybrid + Aman_Local  == 0 ~ 0),
  )
##### trivariate probit HYV/Hybrid/Local
BIHS3_dfest_Adopt_mvp <-
  BIHS3_dfest_rice %>% 
  select(contains("Adopt"), all_of(regressors1_country), "hhweight") %>% 
  na.omit()

library(mvord)
est_mvord_HHL <- mvord(as.formula(paste0("MMO2(Adopt_HYV, Adopt_Hybrid, Adopt_Local) ~ 0 +", paste0(regressors1_country, collapse = " +"))),
                   data = BIHS3_dfest_Adopt_mvp,
                   control = mvord.control(solver = "nlm"))

summary(est_mvord_HHL)

BIHS3_dfest_Adopt_mvp <-
  BIHS3_dfest_rice %>% 
  select(contains("Adopt"), all_of(regressors2_country), "hhweight") %>% 
  na.omit()

library(mvord)
est_mvord_HHL <- mvord(as.formula(paste0("MMO2(Adopt_HYV, Adopt_Hybrid, Adopt_Local) ~ 0 +", paste0(regressors2_country, collapse = " +"))),
                       data = BIHS3_dfest_Adopt_mvp,
                       control = mvord.control(solver = "nlm"))

summary(est_mvord_HHL)


### Choice in Aman
BIHS3_dfest_Aman_mvp <-
BIHS3_dfest_rice %>% 
  select(Aman_HYV, Aman_Hybrid, Aman_Local, all_of(regressors4_country), "hhweight") %>% 
  na.omit()

est_mvord_Aman <- mvord(as.formula(paste0("MMO2(Aman_HYV, Aman_Hybrid, Aman_Local) ~ 0 +", paste0(regressors4_country, collapse = " +"))),
                       data = BIHS3_dfest_Aman_mvp,
                       control = mvord.control(solver = "spg"))

summary(est_mvord_Aman)


### Choice in Boro
BIHS3_dfest_Boro_mvp <-
  BIHS3_dfest_rice %>% 
  select(Boro_HYV, Boro_Hybrid, all_of(regressors4_country), "hhweight") %>% 
  na.omit()

est_mvord_Boro <- mvord(as.formula(paste0("MMO2(Boro_HYV, Boro_Hybrid) ~ 0 +", paste0(regressors4_country, collapse = " +"))),
                        data = BIHS3_dfest_Boro_mvp,
                        control = mvord.control(solver = "nlm"))

summary(est_mvord_Boro)

BIHS3_dfest_rice<- 
  BIHS3_dfest_rice %>% 
  mutate(Boro = case_when(Boro_HYV + Boro_Hybrid > 0 ~1,
                          Boro_HYV + Boro_Hybrid == 0 ~ 0),
         Aman = case_when(Aman_HYV + Aman_Hybrid + Aman_Local > 0 ~1,
                          Aman_HYV + Aman_Hybrid + Aman_Local  == 0 ~ 0),
  )
###run the sets - regressor - 
###multivariat boro aman
###make summary statistics of choice variables - regressors - run the probit for boro aman - multivariat as well