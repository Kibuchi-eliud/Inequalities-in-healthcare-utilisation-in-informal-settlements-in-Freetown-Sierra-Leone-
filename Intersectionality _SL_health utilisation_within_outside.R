#=====================
#ARISE Sierra Leone:
#Determinants of Healthcare Utilization within and outside"
#Eliud Kibuchi 
#09/02/2024
#=============================================
rm(list=ls())
#============================================
#.libPaths()
#remove.packages("Matrix",lib ="C:/R-4.0.2/library")
#==============================
#Load packages 
require(tidyverse)
library(ROCR)
library(lme4)
library(glmmTMB)
require(glm2)
library(logbin)
library(datawizard)
library(brms)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
require(rcompanion)
#===========================================
#Set working directory 
setwd("C:/____Kibuchi_local/MyOneDrive/OneDrive - University of Glasgow/kibuchi/Arise_C/Sierra Leone/Paper 2/Data")
list.files()

#Import data
data_cleaned <- read.csv("HU_data_cleaned_within_outside.csv")

head(data_cleaned)

#===============================================================================
#Global model explanatory variables: 1) head of household gender, 2) marital status, 
#3) involved in income activity, 4), presence of disability in household,
#5) food security in household; and 6) the community where people live

#===============================
#Create Strata

#In the past one month, were you or any household member not able to eat the kinds of food you preferred due to a lack of resources?
data_cleaned$food_security <- as.factor(data_cleaned$food_security)
summary(data_cleaned$food_security)

levels(data_cleaned$food_security) <- list("food_secure" = "No", "food_secure" = "Rarely_once_twice",
                                           "food_insecure" = "Sometimes_3_10_times)",
                                                 "food_insecure" = "Often_10_more")
summary(as.factor(data_cleaned$food_security))

summary(as.factor(data_cleaned$income_activity))
summary(as.factor(data_cleaned$income_household))
summary(as.factor(data_cleaned$healthcare_barrier))
#===============================================================================
#Model 1
data_cleaned$health_utilization_in_out <- as.factor(data_cleaned$health_utilization_in_out)
summary(data_cleaned$health_utilization_in_out)
summary(data_cleaned$household_number)
#unclass outcome variable 

data_cleaned$health_utilization_in_out<- unclass(data_cleaned$health_utilization_in_out)
summary(data_cleaned$health_utilization_in_out)
data_cleaned$health_utilization_in_out<- data_cleaned$health_utilization_in_out- 1
data_cleaned$health_utilization_in_out<- as.factor(data_cleaned$health_utilization_in_out)
summary(data_cleaned$health_utilization_in_out)

#===============================================================================
#Variable selection 
#Length of stay

data_cleaned$length_of_stay <- as.factor(data_cleaned$length_of_stay)
levels(data_cleaned$length_of_stay)
data_cleaned <- within(data_cleaned, length_of_stay <- relevel(length_of_stay, ref = "one_yr"))

crosstable::crosstable(data_cleaned, length_of_stay, by = health_utilization_in_out, total ="all")


model_length_of_stay <- glm(health_utilization_in_out~ 1 + length_of_stay + offset(household_number), 
                          family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_length_of_stay,), digits = 2)
exp(cbind(coef(model_length_of_stay), confint(model_length_of_stay)))  

#hh_income_business

crosstable::crosstable(data_cleaned, hh_income_business, by = health_utilization_in_out, total ="all")


model_hh_income_business <- glm(health_utilization_in_out~ 1 + hh_income_business + offset(household_number), 
                            family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_hh_income_business,), digits = 2)
exp(cbind(coef(model_hh_income_business), confint(model_hh_income_business)))  

#hh_income_fishing

crosstable::crosstable(data_cleaned, hh_income_fishing, by = health_utilization_in_out, total ="all")

model_hh_income_fishing <- glm(health_utilization_in_out~ 1 + hh_income_fishing + offset(household_number), 
                                family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_hh_income_fishing,), digits = 2)
exp(cbind(coef(model_hh_income_fishing), confint(model_hh_income_fishing)))

#hh_income_govt_salaried"
crosstable::crosstable(data_cleaned, hh_income_govt_salaried, by = health_utilization_in_out, total ="all")

model_hh_income_govt_salaried <- glm(health_utilization_in_out~ 1 + hh_income_govt_salaried + offset(household_number), 
                               family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_hh_income_govt_salaried,), digits = 2)
exp(cbind(coef(model_hh_income_govt_salaried), confint(model_hh_income_govt_salaried)))


#"hh_income_private_salaried"

crosstable::crosstable(data_cleaned, hh_income_private_salaried, by = health_utilization_in_out, total ="all")

model_hh_income_private_salaried <- glm(health_utilization_in_out~ 1 + hh_income_private_salaried + offset(household_number), 
                                     family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_hh_income_private_salaried,), digits = 2)
exp(cbind(coef(model_hh_income_private_salaried), confint(model_hh_income_private_salaried)))

##"hh_income_informal_salaried"
crosstable::crosstable(data_cleaned, hh_income_informal_salaried, by = health_utilization_in_out, total ="all")

model_hh_income_informal_salaried <- glm(health_utilization_in_out~ 1 + hh_income_informal_salaried + offset(household_number), 
                                        family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_hh_income_informal_salaried,), digits = 2)
exp(cbind(coef(model_hh_income_informal_salaried), confint(model_hh_income_informal_salaried)))

#"hh_income_daily_wage"
crosstable::crosstable(data_cleaned, hh_income_daily_wage, by = health_utilization_in_out, total ="all")

model_hh_income_daily_wage <- glm(health_utilization_in_out~ 1 + hh_income_daily_wage + offset(household_number), 
                                         family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_hh_income_daily_wage,), digits = 2)
exp(cbind(coef(model_hh_income_daily_wage), confint(model_hh_income_daily_wage)))

#"hh_income_bikeride"
crosstable::crosstable(data_cleaned, hh_income_bikeride, by = health_utilization_in_out, total ="all")

model_hh_income_bikeride <- glm(health_utilization_in_out~ 1 + hh_income_bikeride + offset(household_number), 
                                  family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_hh_income_bikeride,), digits = 2)
exp(cbind(coef(model_hh_income_bikeride), confint(model_hh_income_bikeride)))

#hh_income_stone_mine
crosstable::crosstable(data_cleaned, hh_income_stone_mine, by = health_utilization_in_out, total ="all")

model_hh_income_stone_mine <- glm(health_utilization_in_out~ 1 + hh_income_stone_mine + offset(household_number), 
                                family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_hh_income_stone_mine,), digits = 2)
exp(cbind(coef(model_hh_income_stone_mine), confint(model_hh_income_stone_mine)))

#hh_income_unemployed
crosstable::crosstable(data_cleaned, hh_income_unemployed, by = health_utilization_in_out, total ="all")

model_hh_income_unemployed <- glm(health_utilization_in_out~ 1 + hh_income_unemployed + offset(household_number), 
                                  family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_hh_income_unemployed,), digits = 2)
exp(cbind(coef(model_hh_income_unemployed), confint(model_hh_income_unemployed)))

#hh_income_others
crosstable::crosstable(data_cleaned, hh_income_others, by = health_utilization_in_out, total ="all")

model_hh_income_others <- glm(health_utilization_in_out~ 1 + hh_income_others + offset(household_number), 
                                  family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_hh_income_others,), digits = 2)
exp(cbind(coef(model_hh_income_others), confint(model_hh_income_others)))

#"hh_tenure"
crosstable::crosstable(data_cleaned, hh_tenure, by = health_utilization_in_out, total ="all")

data_cleaned$hh_tenure <- as.factor(data_cleaned$hh_tenure)
levels(data_cleaned$hh_tenure)
data_cleaned <- within(data_cleaned, hh_tenure <- relevel(hh_tenure, ref = "tenant"))

model_hh_tenure <- glm(health_utilization_in_out~ 1 + hh_tenure + offset(household_number), 
                              family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_hh_tenure,), digits = 2)
exp(cbind(coef(model_hh_tenure), confint(model_hh_tenure)))

#water_cook_piped_dwelling",
crosstable::crosstable(data_cleaned, water_cook_piped_dwelling, by = health_utilization_in_out, total ="all")

model_water_cook_piped_dwelling <- glm(health_utilization_in_out~ 1 + water_cook_piped_dwelling + offset(household_number), 
                       family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_water_cook_piped_dwelling,), digits = 2)
exp(cbind(coef(model_water_cook_piped_dwelling), confint(model_water_cook_piped_dwelling)))

#"water_cook_piped_compound"
crosstable::crosstable(data_cleaned, water_cook_piped_compound, by = health_utilization_in_out, total ="all")

model_water_cook_piped_compound <- glm(health_utilization_in_out~ 1 + water_cook_piped_compound + offset(household_number), 
                                       family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_water_cook_piped_compound,), digits = 2)
exp(cbind(coef(model_water_cook_piped_compound), confint(model_water_cook_piped_compound)))

#"water_cook_piped_neighbor
crosstable::crosstable(data_cleaned, water_cook_piped_neighbor, by = health_utilization_in_out, total ="all")

model_water_cook_piped_neighbor <- glm(health_utilization_in_out~ 1 + water_cook_piped_neighbor + offset(household_number), 
                                       family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_water_cook_piped_neighbor,), digits = 2)
exp(cbind(coef(model_water_cook_piped_neighbor), confint(model_water_cook_piped_neighbor)))

#"water_cook_public tap_standpipe"
crosstable::crosstable(data_cleaned, water_cook_publictap_standpipe, by = health_utilization_in_out, total ="all")

model_water_cook_publictap_standpipe <- glm(health_utilization_in_out~ 1 + water_cook_publictap_standpipe + offset(household_number), 
                                       family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_water_cook_publictap_standpipe,), digits = 2)
exp(cbind(coef(model_water_cook_publictap_standpipe), confint(model_water_cook_publictap_standpipe)))

#"water_cook_compound_well"
crosstable::crosstable(data_cleaned, water_cook_compound_well, by = health_utilization_in_out, total ="all")

model_water_cook_compound_well <- glm(health_utilization_in_out~ 1 + water_cook_compound_well + offset(household_number), 
                                            family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_water_cook_compound_well,), digits = 2)
exp(cbind(coef(model_water_cook_compound_well), confint(model_water_cook_compound_well)))

#"water_cook_spring"
crosstable::crosstable(data_cleaned, water_cook_spring, by = health_utilization_in_out, total ="all")

model_water_cook_spring <- glm(health_utilization_in_out~ 1 + water_cook_spring + offset(household_number), 
                                      family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_water_cook_spring,), digits = 2)
exp(cbind(coef(model_water_cook_spring), confint(model_water_cook_spring)))

#"water_cook_rain"
crosstable::crosstable(data_cleaned,water_cook_rain, by = health_utilization_in_out, total ="all")

model_water_cook_rain <- glm(health_utilization_in_out~ 1 + water_cook_rain + offset(household_number), 
                               family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_water_cook_rain,), digits = 2)
exp(cbind(coef(model_water_cook_rain), confint(model_water_cook_rain)))

#"water_cook_bowser"
crosstable::crosstable(data_cleaned,water_cook_bowser, by = health_utilization_in_out, total ="all")

model_water_cook_bowser <- glm(health_utilization_in_out~ 1 + water_cook_bowser + offset(household_number), 
                             family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_water_cook_bowser,), digits = 2)
exp(cbind(coef(model_water_cook_bowser), confint(model_water_cook_bowser)))

#"water_cook_kiosk"

crosstable::crosstable(data_cleaned,water_cook_kiosk, by = health_utilization_in_out, total ="all")


model_water_cook_kiosk <- glm(health_utilization_in_out~ 1 + water_cook_kiosk + offset(household_number), 
                               family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_water_cook_kiosk,), digits = 2)
exp(cbind(coef(model_water_cook_kiosk), confint(model_water_cook_kiosk)))

#"water_cook_bottled"
crosstable::crosstable(data_cleaned,water_cook_bottled, by = health_utilization_in_out, total ="all")

model_water_cook_bottled <- glm(health_utilization_in_out~ 1 + water_cook_bottled + offset(household_number), 
                              family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_water_cook_bottled,), digits = 2)
exp(cbind(coef(model_water_cook_bottled), confint(model_water_cook_bottled)))
    
#"water_sachet", 
crosstable::crosstable(data_cleaned,water_sachet, by = health_utilization_in_out, total ="all")

model_water_sachet <- glm(health_utilization_in_out~ 1 + water_sachet + offset(household_number), 
                                family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_water_sachet,), digits = 2)
exp(cbind(coef(model_water_sachet), confint(model_water_sachet)))
    
#"water_cook_surface"
crosstable::crosstable(data_cleaned,water_cook_surface, by = health_utilization_in_out, total ="all")

model_water_cook_surface <- glm(health_utilization_in_out~ 1 + water_cook_surface + offset(household_number), 
                          family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_water_cook_surface,), digits = 2)
exp(cbind(coef(model_water_cook_surface), confint(model_water_cook_surface)))

#"water_cook_neighor_well"
crosstable::crosstable(data_cleaned,water_cook_neighor_well, by = health_utilization_in_out, total ="all")

model_water_cook_neighor_well <- glm(health_utilization_in_out~ 1 + water_cook_neighor_well + offset(household_number), 
                                family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_water_cook_neighor_well,), digits = 2)
exp(cbind(coef(model_water_cook_neighor_well), confint(model_water_cook_neighor_well)))

#"water_cook_other"
crosstable::crosstable(data_cleaned,water_cook_other, by = health_utilization_in_out, total ="all")

model_water_cook_other <- glm(health_utilization_in_out~ 1 + water_cook_other + offset(household_number), 
                                     family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_water_cook_other,), digits = 2)
exp(cbind(coef(model_water_cook_other), confint(model_water_cook_other)))

#"water_cook_distance"
data_cleaned$water_cook_distance <- as.factor(data_cleaned$water_cook_distance)
levels(data_cleaned$water_cook_distance)
data_cleaned <- within(data_cleaned, water_cook_distance <- relevel(water_cook_distance, ref = "less_30_min"))

crosstable::crosstable(data_cleaned,water_cook_distance, by = health_utilization_in_out, total ="all")


model_water_cook_distance <- glm(health_utilization_in_out~ 1 + water_cook_distance + offset(household_number), 
                              family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_water_cook_distance,), digits = 2)
exp(cbind(coef(model_water_cook_distance), confint(model_water_cook_distance)))


#"water_shortage"
data_cleaned$water_shortage <- as.factor(data_cleaned$water_shortage)
levels(data_cleaned$water_shortage)
data_cleaned <- within(data_cleaned, water_shortage <- relevel(water_shortage, ref = "Yes"))

crosstable::crosstable(data_cleaned,water_shortage, by = health_utilization_in_out, total ="all")


model_water_shortage <- glm(health_utilization_in_out~ 1 + water_shortage + offset(household_number), 
                                 family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_water_shortage,), digits = 2)
exp(cbind(coef(model_water_shortage), confint(model_water_shortage)))

#"toilet_flush"
crosstable::crosstable(data_cleaned,toilet_flush, by = health_utilization_in_out, total ="all")

model_toilet_flush <- glm(health_utilization_in_out~ 1 + toilet_flush + offset(household_number), 
                            family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_toilet_flush,), digits = 2)
exp(cbind(coef(model_toilet_flush), confint(model_toilet_flush)))

#"toilet_latrine"
crosstable::crosstable(data_cleaned,toilet_latrine, by = health_utilization_in_out, total ="all")

model_toilet_latrine <- glm(health_utilization_in_out~ 1 + toilet_latrine + offset(household_number), 
                          family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_toilet_latrine,), digits = 2)
exp(cbind(coef(model_toilet_latrine), confint(model_toilet_latrine)))

#"toilet_bucket"
crosstable::crosstable(data_cleaned,toilet_bucket, by = health_utilization_in_out, total ="all")

model_toilet_bucket <- glm(health_utilization_in_out~ 1 + toilet_bucket + offset(household_number), 
                            family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_toilet_bucket,), digits = 2)
exp(cbind(coef(model_toilet_bucket), confint(model_toilet_bucket)))

#"toilet_hanging"
crosstable::crosstable(data_cleaned,toilet_hanging, by = health_utilization_in_out, total ="all")

model_toilet_hanging <- glm(health_utilization_in_out~ 1 + toilet_hanging + offset(household_number), 
                           family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_toilet_hanging,), digits = 2)
exp(cbind(coef(model_toilet_hanging), confint(model_toilet_hanging)))
    
#"toilet_flying"   
crosstable::crosstable(data_cleaned,toilet_flying, by = health_utilization_in_out, total ="all")

model_toilet_flying <- glm(health_utilization_in_out~ 1 + toilet_flying + offset(household_number), 
                            family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_toilet_flying,), digits = 2)
exp(cbind(coef(model_toilet_flying), confint(model_toilet_flying))) 

#"toilet_open_defecation"

crosstable::crosstable(data_cleaned,toilet_open_defecation, by = health_utilization_in_out, total ="all")

model_toilet_open_defecation <- glm(health_utilization_in_out~ 1 + toilet_open_defecation + offset(household_number), 
                           family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_toilet_open_defecation,), digits = 2)
exp(cbind(coef(model_toilet_open_defecation), confint(model_toilet_open_defecation))) 

#"toilet_other"
crosstable::crosstable(data_cleaned,toilet_other, by = health_utilization_in_out, total ="all")


model_toilet_other <- glm(health_utilization_in_out~ 1 + toilet_other + offset(household_number), 
                                    family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_toilet_other,), digits = 2)
exp(cbind(coef(model_toilet_other), confint(model_toilet_other))) 

#"toilet_shared"
crosstable::crosstable(data_cleaned,toilet_shared, by = health_utilization_in_out, total ="all")

data_cleaned$toilet_shared <- as.factor(data_cleaned$toilet_shared)
levels(data_cleaned$toilet_shared)
data_cleaned <- within(data_cleaned, toilet_shared <- relevel(toilet_shared, ref = "Yes"))


model_toilet_shared <- glm(health_utilization_in_out~ 1 + toilet_shared + offset(household_number), 
                          family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_toilet_shared,), digits = 2)
exp(cbind(coef(model_toilet_shared), confint(model_toilet_shared)))

#"toilet_access"
crosstable::crosstable(data_cleaned,toilet_access, by = health_utilization_in_out, total ="all")

data_cleaned$toilet_access <- as.factor(data_cleaned$toilet_access)
levels(data_cleaned$toilet_access)
data_cleaned <- within(data_cleaned, toilet_access <- relevel(toilet_access, ref = "Yes"))

model_toilet_access <- glm(health_utilization_in_out~ 1 + toilet_access + offset(household_number), 
                           family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_toilet_access,), digits = 2)
exp(cbind(coef(model_toilet_access), confint(model_toilet_access)))
          
#"waste_disposal_community",

crosstable::crosstable(data_cleaned, waste_disposal_community, by = health_utilization_in_out, total ="all")
data_cleaned$waste_disposal_community <- as.factor(data_cleaned$waste_disposal_community)
levels(data_cleaned$waste_disposal_community)
data_cleaned <- within(data_cleaned, waste_disposal_community <- relevel(waste_disposal_community, ref = "Yes"))

crosstable::crosstable(data_cleaned,waste_disposal_community, by = health_utilization_in_out, total ="all")

model_waste_disposal_community <- glm(health_utilization_in_out~ 1 + waste_disposal_community + offset(household_number), 
                           family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_waste_disposal_community,), digits = 2)
exp(cbind(coef(model_waste_disposal_community), confint(model_waste_disposal_community)))
          
#"waste_around_house"

crosstable::crosstable(data_cleaned,waste_around_house, by = health_utilization_in_out, total ="all")

model_waste_around_house <- glm(health_utilization_in_out~ 1 + waste_around_house + offset(household_number), 
                                      family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_waste_around_house,), digits = 2)
exp(cbind(coef(model_waste_around_house), confint(model_waste_around_house)))
                                                        
#"waste_dumping_site"
crosstable::crosstable(data_cleaned,waste_dumping_site, by = health_utilization_in_out, total ="all")

model_waste_dumping_site <- glm(health_utilization_in_out~ 1 + waste_dumping_site + offset(household_number), 
                                family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_waste_dumping_site,), digits = 2)
exp(cbind(coef(model_waste_dumping_site), confint(model_waste_dumping_site)))

#"waste_drainage",
crosstable::crosstable(data_cleaned,waste_drainage, by = health_utilization_in_out, total ="all")

model_waste_drainage <- glm(health_utilization_in_out~ 1 + waste_drainage + offset(household_number), 
                                family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_waste_drainage,), digits = 2)
exp(cbind(coef(model_waste_drainage), confint(model_waste_drainage)))

#"waste_solid_collectors"
crosstable::crosstable(data_cleaned, waste_solid_collectors, by = health_utilization_in_out, total ="all")

model_waste_solid_collectors <- glm(health_utilization_in_out~ 1 + waste_solid_collectors + offset(household_number), 
                            family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_waste_solid_collectors,), digits = 2)
exp(cbind(coef(model_waste_solid_collectors), confint(model_waste_solid_collectors)))

#"waste_sea"
crosstable::crosstable(data_cleaned, waste_sea, by = health_utilization_in_out, total ="all")

model_waste_sea <- glm(health_utilization_in_out~ 1 + waste_sea + offset(household_number), 
                                    family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_waste_sea,), digits = 2)
exp(cbind(coef(model_waste_sea), confint(model_waste_sea)))

##"waste_others"
crosstable::crosstable(data_cleaned, waste_others, by = health_utilization_in_out, total ="all")

model_waste_others <- glm(health_utilization_in_out~ 1 + waste_others + offset(household_number), 
                       family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_waste_others,), digits = 2)
exp(cbind(coef(model_waste_others), confint(model_waste_others)))

#"waste_payment"
crosstable::crosstable(data_cleaned, waste_payment, by = health_utilization_in_out, total ="all")

model_waste_payment <- glm(health_utilization_in_out~ 1 + waste_payment + offset(household_number), 
                          family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_waste_payment,), digits = 2)
exp(cbind(coef(model_waste_payment), confint(model_waste_payment)))


##"environ_disaster",
crosstable::crosstable(data_cleaned, environ_disaster , by = health_utilization_in_out, total ="all")

model_environ_disaster <- glm(health_utilization_in_out~ 1 + environ_disaster + offset(household_number), 
                                                      family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_environ_disaster,), digits = 2)
exp(cbind(coef(model_environ_disaster), confint(model_environ_disaster)))

data_cleaned$barrier_qualitycare

#===============================
#Intersection variables 

#hh_disabled
crosstable::crosstable(data_cleaned, hh_disabled , by = health_utilization_in_out, total ="all")

model_hh_disabled <- glm(health_utilization_in_out~ 1 + hh_disabled + offset(household_number), 
                              family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_hh_disabled,), digits = 2)
exp(cbind(coef(model_hh_disabled), confint(model_hh_disabled)))

#food_security 
crosstable::crosstable(data_cleaned, food_security , by = health_utilization_in_out, total ="all")

model_food_security <- glm(health_utilization_in_out~ 1 + food_security + offset(household_number), 
                         family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_food_security,), digits = 2)
exp(cbind(coef(model_food_security), confint(model_food_security)))

crosstable::crosstable(data_cleaned, food_security, by = health_utilization_in_out, total ="all")

# Income generating activity in household
data_cleaned$income_household <- as.factor(data_cleaned$income_household)
summary(data_cleaned$income_household)
#income_activity, income_activity_household

crosstable::crosstable(data_cleaned, income_activity, by = health_utilization_in_out, total ="all")



model_income_household <- glm(health_utilization_in_out~ 1 + income_activity + offset(household_number), 
                           family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_income_household,), digits = 2)
exp(cbind(coef(model_income_household), confint(model_income_household)))

#Community 
crosstable::crosstable(data_cleaned, community_type, by = health_utilization_in_out, total ="all")

model_community_type <- glm(health_utilization_in_out~ 1 + community_type + offset(household_number), 
                              family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_community_type,), digits = 2)
exp(cbind(coef(model_community_type), confint(model_community_type)))

#family type  
data_cleaned$marital_status <- as.factor(data_cleaned$marital_status)
levels(data_cleaned$marital_status)
data_cleaned <- within(data_cleaned, marital_status <- relevel(marital_status, ref = "Single"))

crosstable::crosstable(data_cleaned, marital_status, by = health_utilization_in_out, total ="all")


model_marital_status <- glm(health_utilization_in_out~ 1 + marital_status + offset(household_number), 
                            family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_marital_status,), digits = 2)
exp(cbind(coef(model_marital_status), confint(model_community_type)))

#Head of household gender 
model_head_gender <- glm(health_utilization_in_out~ 1 + head_gender + offset(household_number), 
                            family=binomial(link='logit'), data = data_cleaned) 

print(summary(model_head_gender,), digits = 2)
exp(cbind(coef(model_head_gender), confint(model_head_gender)))

#===============================================================================
#is.na(data_cleaned$income_household)
summary(as.factor(data_cleaned$income_household))

data_cleaned$strata <- ifelse(
  is.na(data_cleaned$head_gender) | is.na(data_cleaned$marital_status) | is.na(data_cleaned$income_activity) |
    is.na(data_cleaned$hh_disabled) |is.na(data_cleaned$food_security)|is.na(data_cleaned$community_type) ,
  NA_character_,
  paste0(data_cleaned$head_gender, ", ", data_cleaned$marital_status, ", ", data_cleaned$income_activity, ", ", 
         data_cleaned$hh_disabled, ", ", data_cleaned$food_security, ", ", data_cleaned$community_type)
)

data_cleaned$strata <- as.factor(data_cleaned$strata)
levels(data_cleaned$strata)
data_tabulate(data_cleaned$strata)
#Exclude NA
data_cleaned$strata <- factor(data_cleaned$strata , exclude = "<NA>")

#Convert categorical into numerical 
data_cleaned$strata_id <- unclass(data_cleaned$strata)
data_tabulate(data_cleaned$strata_id)

#===============================================================================

HU_within_outside_model_1<- brm(health_utilization_in_out ~ 1  + (1|strata_id) + offset(household_number), 
                            data = data_cleaned,family = bernoulli, 
                            warmup = 2000, iter = 20000, control = list(adapt_delta = 0.90),
                  cores = 2, chains = 2, seed = 123)

print(summary(HU_within_outside_model_1), digits = 3)

HU_within_outside_model_1_exp <- exp(fixef(HU_within_outside_model_1)); HU_within_outside_model_1_exp
write.csv(HU_within_outside_model_1_exp, "results/HU_within_outside_model_1_exp.csv")

#==================================

#Compute AUC for predicting Class with the model
Prob <- predict(HU_within_outside_model_1, type="response")
Prob <- Prob[,1]
Pred <- prediction(Prob, as.vector(pull(data_cleaned, health_utilization_in_out)))
AUC <- performance(Pred, measure = "auc")
AUC <- AUC@y.values[[1]]
AUC

#With an AUC score of  0.62, the model discrimination is moderate 
#=====================================================================================================
#VPC Indicates the share of the total individual variance  in the propensity for having cough that is accounted for at the intersectional strata level.
variance_group <-   2.000^2; variance_group 
VPC_model1 <-   2.000^2 / (2.000^2 +3.29)*100; VPC_model1
#model1_vpc = 54.87% - indicates high discriminatory accuracy of intersection strata 
#VPC by 100 and interpreted it as the percentage share of the individual variance which lies between strata
#===================================================================================================
#Plot
#extract posterior distributions of all the random effect terms
data_RandomEffect <- ranef(HU_within_outside_model_1)

#extract posterior distributions of `sd(Intercept)`
r_Intercept <- data_RandomEffect$strata_id[, , 1] %>%
  as_tibble() %>%
  rownames_to_column(var = "StrataID") %>%
  mutate(Variable = "sd(Intercept)")


#arrange in ascending order 
names(r_Intercept)
r_Intercept2 <-r_Intercept[order(r_Intercept$Estimate),]
r_Intercept2$Strata_ID <- seq.int(nrow(r_Intercept2))
head(r_Intercept2)

#plot
plot1 <- r_Intercept2 %>%
  mutate(Contain_Zero = if_else(Q2.5*Q97.5 > 0, "no", "yes")) %>%
  ggplot(aes( x = Strata_ID, y = Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5)) +
  scale_x_continuous(breaks = round(seq(min(r_Intercept2$Strata_ID), max(r_Intercept2$Strata_ID), by = 5),1)) +
  scale_y_continuous(breaks = round(seq(-14,14, by = 2),1))+
  coord_cartesian(ylim = c(-14, 14))+
  geom_hline(aes(yintercept = 0),linetype="dotted")+
  xlab("Stratum rank") +
  labs(title = "A")+
  ylab("Intersectional inequalities") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.line = element_line(colour = "black"),
        legend.position="none")
plot1

r_Intercept2$strata_id<- r_Intercept2$StrataID

data_cleaned_model1 <- data_cleaned

HU_within_outside_model_11 <- merge(data_cleaned_model1, r_Intercept2, by = "strata_id") 

#Export data
write.csv(HU_within_outside_model_11, file = "results/HU_within_outside_model_11.csv")

#==============================================================================
#model 2: controlling for strata covariates

#Relevel some variables:
#data_cleaned$head_gender <- as.factor(data_cleaned$head_gender)
#levels(data_cleaned$head_gender)
#data_cleaned <- within(data_cleaned, head_gender <- relevel(head_gender, ref = "Female"))

HU_within_outside_model_1_2 <- brm(health_utilization_in_out~ 1  + head_gender + marital_status +
                              income_activity + hh_disabled + food_security + community_type + (1|strata_id) + 
                               offset(household_number), data = data_cleaned,family = bernoulli, 
                             warmup = 2000, iter = 20000, control = list(adapt_delta = 0.90),
                             cores = 2, chains = 2, seed = 123)

print(summary(HU_within_outside_model_1_2), digits = 3)
HU_within_outside_model_1_2_exp <- exp(fixef(HU_within_outside_model_1_2)); HU_within_outside_model_1_2_exp 
write.csv(HU_within_outside_model_1_2_exp, "results/HU_within_outside_model_1_2_exp.csv")

#===============
#Classification rate

model2_Prob <- predict(HU_within_outside_model_1_2, type="response")
model2_Prob <- model2_Prob[,1]
modell2_Pred <- prediction(model2_Prob, as.vector(pull(data_cleaned, health_utilization_in_out)))
AUC_2 <- performance(modell2_Pred, measure = "auc")
AUC_2v <- AUC_2@y.values[[1]]
AUC_2v
#0.61
#===================================================================================
#Partially adjusted intersection model (PCV)
#PCV quantifies the degree to which the different dimensions used to construct the intersectional strata contributed to the 
#between-stratum variance observed in previous model 1
#PCV = (model1_variance - model2_variance)/ model1_variance 
#VPC

model2_variance <- 1.148^2; model2_variance 
model2_vpc <-   1.148^2 / (1.148^2 +3.29)*100; model2_vpc
#28.60%

#PCV
model1_variance <-  2.000^2
model2_variance <-  1.148^2
model2_PCV <- (2.000^2 - 1.148^2)/ 2.000^2 ; model2_PCV
model2_PCV <- model2_PCV*100; model2_PCV
#PCV=67.05%
#This indicates 32.95% of variance was not explained by adding fixed effects 
#=================================================================================================
#Plot
data2_RandomEffect <- ranef(HU_within_outside_model_1_2)

#extract posterior distributions of `sd(Intercept)`
r2_Intercept <- data2_RandomEffect$strata_id[, , 1] %>%
  as_tibble() %>%
  rownames_to_column(var = "StrataID") %>%
  mutate(Variable = "sd(Intercept)")

#arrange in ascending order 
names(r2_Intercept)
r2_Intercept2 <-r2_Intercept[order(r2_Intercept$Estimate),]
r2_Intercept2$Strata_ID <- seq.int(nrow(r2_Intercept2))
head(r2_Intercept2)

#plot
plot2 <- r2_Intercept2 %>%
  mutate(Contain_Zero = if_else(Q2.5*Q97.5 > 0, "no", "yes")) %>%
  ggplot(aes( x = Strata_ID, y = Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5)) +
  scale_x_continuous(breaks = round(seq(min(r2_Intercept2$Strata_ID), max(r2_Intercept2$Strata_ID), by = 5),1)) +
  scale_y_continuous(breaks = round(seq(-14,14, by = 2),1))+
  coord_cartesian(ylim = c(-14, 14))+
  geom_hline(aes(yintercept = 0),linetype="dotted")+
  xlab("Stratum rank") +
  labs(title = "B")+
  ylab("Intersectional inequalities") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.line = element_line(colour = "black"),
        legend.position="none")

plot2

r_Intercept2$strata_id2 <- r_Intercept2$StrataID

data_cleaned_model2 <- data_cleaned

HU_within_outside_model_12 <- merge(data_cleaned_model2, r_Intercept2, by = "strata_id") 

#Export data
write.csv(HU_within_outside_model_12, file = "results/HU_within_outside_model_2.csv")

#===============================================================================
#Model 3 controlling for other covariates  

data_cleaned$water_cook_publictap_standpipe

HU_within_outside_model_1_3 <- brm(health_utilization_in_out~ 1+ head_gender + marital_status +
                        income_activity + hh_disabled + food_security + community_type + length_of_stay +
                          hh_income_business + hh_income_govt_salaried + hh_income_private_salaried + hh_income_daily_wage +
                         hh_income_stone_mine + hh_income_bikeride + hh_income_others + hh_tenure +  water_cook_piped_neighbor + 
                          water_cook_publictap_standpipe + water_cook_compound_well + water_cook_spring + water_cook_rain + 
                          water_cook_kiosk + water_cook_bowser +  water_cook_bottled + water_sachet + water_cook_surface + 
                          water_cook_neighor_well + water_cook_distance +  toilet_flush + toilet_bucket + toilet_hanging +
                          toilet_flying + toilet_latrine + toilet_open_defecation + toilet_other + toilet_shared +
                          waste_around_house + waste_solid_collectors + waste_dumping_site + waste_drainage + waste_sea +
                          waste_others + waste_payment + environ_disaster + (1|strata_id) + offset(household_number), 
                        data = data_cleaned,family = bernoulli,  warmup = 2000, iter = 20000, 
                        control = list(adapt_delta = 0.90), cores = 2, chains = 2, seed = 123)

print(summary(HU_within_outside_model_1_3), digits = 3)
HU_within_outside_model_1_3_exp <- exp(fixef(HU_within_outside_model_1_3)); HU_within_outside_model_1_3 
write.csv(HU_within_outside_model_1_3_exp, "results/HU_within_outside_model_3_exp.csv")
#===============
#Classification rate

model3_Prob <- predict(HU_within_outside_model_1_3, type="response")
model3_Prob <- model3_Prob[,1]
modell3_Pred <- prediction(model3_Prob, as.vector(pull(data_cleaned, health_utilization_in_out)))
AUC_3 <- performance(modell3_Pred, measure = "auc")
AUC_3v <- AUC_3@y.values[[1]]
AUC_3v
#0.64
#===================================================================================
#Partially adjusted intersection model (PCV)
#PCV quantifies the degree to which the different dimensions used to construct the intersectional strata contributed to the 
#between-stratum variance observed in previous model 1
#PCV = (model1_variance - model2_variance)/ model1_variance 
#VPC

model3_variance <- 0.867^2; model3_variance 
model3_vpc <-   0.867^2 / (0.867^2 +3.29)*100; model3_vpc

# 18.59%

#PCV
model1_variance <- 2.000^2
model3_variance <-  0.867^2
model3_PCV <- (2.000^2 - 0.867^2)/ 2.000^2 ; model3_PCV
model3_PCV <- model3_PCV*100; model3_PCV
#PCV=81.21%
#This indicates 18.79% of variance was not explained by adding strata and covariates variables
#=================================================================================================
#Plot
data3_RandomEffect <- ranef(HU_within_outside_model_1_3)

#extract posterior distributions of `sd(Intercept)`
r3_Intercept <- data3_RandomEffect$strata_id[, , 1] %>%
  as_tibble() %>%
  rownames_to_column(var = "StrataID") %>%
  mutate(Variable = "sd(Intercept)")

#arrange in ascending order 
names(r3_Intercept)
r3_Intercept2 <-r3_Intercept[order(r3_Intercept$Estimate),]
r3_Intercept2$Strata_ID <- seq.int(nrow(r3_Intercept2))
head(r3_Intercept2)

#plot
plot3 <- r3_Intercept2 %>%
  mutate(Contain_Zero = if_else(Q2.5*Q97.5 > 0, "no", "yes")) %>%
  ggplot(aes( x = Strata_ID, y = Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5)) +
  scale_x_continuous(breaks = round(seq(min(r2_Intercept2$Strata_ID), max(r2_Intercept2$Strata_ID), by = 5),1)) +
  scale_y_continuous(breaks = round(seq(-14,14, by = 2),1))+
  coord_cartesian(ylim = c(-14, 14))+
  geom_hline(aes(yintercept = 0),linetype="dotted")+
  xlab("Stratum rank") +
  labs(title = "C")+
  ylab("Intersection inequalities") +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.line = element_line(colour = "black"),
        legend.position="none")

plot3

r3_Intercept2$strata_id <- r3_Intercept2$StrataID

data_cleaned_model3 <- data_cleaned

HU_within_outside_model_1_model3 <- merge(data_cleaned_model3, r3_Intercept2, by = "strata_id") 

#Export data
write.csv(HU_within_outside_model_1_model3, file = "results/HU_within_outside_model_13.csv")

#===============================================================================
#Plots 
grid.arrange(plot1, plot2,plot3, nrow=3, ncol=1)

HU_within_outside_model_1_plot <- ggpubr::ggarrange(plot1, plot2,plot3, ncol = 1, nrow = 3)

ggsave('results/HU_within_outside_model_1_plot.pdf',HU_within_outside_model_1_plot, width =18,height=15,dpi=1200)
#ggsave('results/HU_within_outside_model_1_plot.tiff',HU_within_outside_model_1_plot, width =18,height=15,dpi=1200)
ggsave('results/HU_within_outside_model_1_plot.eps',HU_within_outside_model_1_plot, width =18,height=15,dpi=1200)
#===============================================================================












