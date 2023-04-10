#-------------------------------------------------------------------------------- #
# Modeling 2| Comparing temporal Trend Specifications           #
# Previously we selected models based on number of internal knots on the spline that describe the baseline hazards
# We identified that models in the odds scale performed better based on AIC and that the number of knots needed was somewhat stable for each subpopulation. 
# the cohort analysis showed that for the female population 3 knots was the best fit for the spline, and for the male population 7 knots was the best fit for the spline
# the period analysis showed that for the female population 3 knots was the best fit for the spline, and for the male population 6 knots was the best fit for the spline

# Here we take those models and extend them to incorporate non-linear time trends. The covariates are Period (years) or Cohort( decennial birth cohorts), 
# and to specify polynomial temporal trends without multi-collinearity we use orthogonal polynomials of different degrees(1-3). 
# Author: Mauricio, Lopez Mendez
# Date: 11/07/2020
#-------------------------------------------------------------------------------- #
# 0. Libraries ----
library(survival)
library(splines)
library(flexsurv)
library(mgcv)

library(eha)
library(epitools)
#library(interval)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)

library(reshape2)
library(readxl)
library(openxlsx)
library(dampack)
#-------------------------------------------------------------------------------- #
# 1. LOAD INPUTS  ----
#Cohort
load("model_inputs_cohort.RData")
#Period
load("model_inputs_period.RData")

#-------------------------------------------------------------------------------- #
# 2. Comparing Temporal Trends Specifications and Proportionality of Hazards-----
#-------------------------------------------------------------------------------- #
## 2.1 Female, cohort ----
# proportional hazards, different temporal trends
mod7_f_c_1 <-flexsurvspline(model_inputs_cohort$surv_objects$so_female_c         ~ poly(Cohort.dec,2, raw = F),
                           k = 3, scale = "odds",           
                           data = model_inputs_cohort$data_dfs$df_frmale_c) 

mod8_f_c_1 <-flexsurvspline(model_inputs_cohort$surv_objects$so_female_c         ~ poly(Cohort.dec,3, raw = F),
                           k = 3, scale = "odds",           
                           data = model_inputs_cohort$data_dfs$df_frmale_c)           

mod9_f_c_1 <-flexsurvspline(model_inputs_cohort$surv_objects$so_female_c         ~ poly(Cohort.dec,1, raw = F),
                           k = 3, scale = "odds",           
                           data = model_inputs_cohort$data_dfs$df_frmale_c)         
#-------------------------------------------------------------------------------- #
# non-proportional hazards, different temporal trends
mod7_f_c_2    <- flexsurvspline(model_inputs_cohort$surv_objects$so_female_c     ~ poly(Cohort.dec,2) + gamma1(Cohort.dec) + gamma2(Cohort.dec),
                  k = 3, scale = "hazard",
                  data = model_inputs_cohort$data_dfs$df_frmale_c)

mod8_f_c_2    <- flexsurvspline(model_inputs_cohort$surv_objects$so_female_c     ~ poly(Cohort.dec,3) + gamma1(Cohort.dec) + gamma2(Cohort.dec),
                                k = 3, scale = "hazard",
                                data = model_inputs_cohort$data_dfs$df_frmale_c)

mod9_f_c_2    <- flexsurvspline(model_inputs_cohort$surv_objects$so_female_c     ~ poly(Cohort.dec,1) + gamma1(Cohort.dec) + gamma2(Cohort.dec),
                             k = 3, scale = "hazard",
                             data = model_inputs_cohort$data_dfs$df_frmale_c)
#-------------------------------------------------------------------------------- #
## 2.2 Male, cohort ---- 
# proportional hazards, different temporal trends


mod7_m_c_1    <- flexsurvspline(model_inputs_cohort$surv_objects$so_male_c       ~ poly(Cohort.dec,2, raw = F),
                   k = 7, scale = "odds",
                   data = model_inputs_cohort$data_dfs$dF_male_c)
    
mod8_m_c_1    <- flexsurvspline(model_inputs_cohort$surv_objects$so_male_c       ~ poly(Cohort.dec,3, raw = F),
                                k = 7, scale = "odds",
                                data = model_inputs_cohort$data_dfs$dF_male_c)
    
mod9_m_c_1    <- flexsurvspline(model_inputs_cohort$surv_objects$so_male_c       ~ poly(Cohort.dec,1, raw = F),
                            k = 7, scale = "odds",
                            data = model_inputs_cohort$data_dfs$dF_male_c)
#-------------------------------------------------------------------------------- #
#non-proportional hazards, different temporal trends
mod7_m_c_2<- flexsurvspline(model_inputs_cohort$surv_objects$so_male_c           ~ poly(Cohort.dec,2) + gamma1(Cohort.dec) + gamma2(Cohort.dec),
                            k = 7, scale = "odds",
                            data = model_inputs_cohort$data_dfs$dF_male_c)

mod8_m_c_2<- flexsurvspline(model_inputs_cohort$surv_objects$so_male_c           ~ poly(Cohort.dec,3) + gamma1(Cohort.dec) + gamma2(Cohort.dec),
                            k = 7, scale = "odds",
                            data = model_inputs_cohort$data_dfs$dF_male_c)

mod9_m_c_2<- flexsurvspline(model_inputs_cohort$surv_objects$so_male_c           ~ poly(Cohort.dec,1) + gamma1(Cohort.dec) + gamma2(Cohort.dec),
                            k = 7, scale = "odds",
                            data = model_inputs_cohort$data_dfs$dF_male_c)
#-------------------------------------------------------------------------------- #
## 2.3 Female, period ----
# Proportional hazards, different temporal trends

mod7_f_p_1 <- flexsurvspline(model_inputs_period$surv_objects$so_female_p        ~ poly(Year,2, raw = F),
                            k = 3, scale = "odds",           
                            data = model_inputs_period$data_dfs$df_frmale_p)

mod8_f_p_1 <- flexsurvspline(model_inputs_period$surv_objects$so_female_p        ~ poly(Year,3, raw = F),
                            k = 3, scale = "odds",           
                            data = model_inputs_period$data_dfs$df_frmale_p)

mod9_f_p_1 <- flexsurvspline(model_inputs_period$surv_objects$so_female_p        ~ poly(Year,1, raw = F),
                            k = 3, scale = "odds",           
                            data = model_inputs_period$data_dfs$df_frmale_p)
#-------------------------------------------------------------------------------- #
# Non-proportional hazards, different temporal trends
mod7_f_p_2<- flexsurvspline(model_inputs_period$surv_objects$so_female_p         ~ poly(Year,2)+ gamma1(Year) + gamma2(Year),
                 k = 3, scale = "odds",
                 data = model_inputs_period$data_dfs$df_frmale_p)

mod8_f_p_2<- flexsurvspline(model_inputs_period$surv_objects$so_female_p         ~ poly(Year,3)+ gamma1(Year) + gamma2(Year),
                 k = 3, scale = "odds",
                 data = model_inputs_period$data_dfs$df_frmale_p)

mod9_f_p_2<- flexsurvspline(model_inputs_period$surv_objects$so_female_p         ~ poly(Year,1)+ gamma1(Year) + gamma2(Year),
                            k = 3, scale = "odds",
                            data = model_inputs_period$data_dfs$df_frmale_p)
#-------------------------------------------------------------------------------- #
## 2.4 Male, period ----
# Proportional hazards, different temporal trends
mod7_m_p_1 <- flexsurvspline(model_inputs_period$surv_objects$so_male_p          ~ poly(Year,2, raw = F),
                             k=6, scale="odds", data= model_inputs_period$data_dfs$dF_male_p)

mod8_m_p_1 <- flexsurvspline(model_inputs_period$surv_objects$so_male_p          ~ poly(Year,3, raw = F),
                             k=6, scale="odds", data= model_inputs_period$data_dfs$dF_male_p)

mod9_m_p_1 <- flexsurvspline(model_inputs_period$surv_objects$so_male_p          ~ poly(Year,1, raw = F),
                             k=6, scale="odds", data= model_inputs_period$data_dfs$dF_male_p)

#-------------------------------------------------------------------------------- #
# Non-proportional hazards, different temporal trends
mod7_m_p_2 <- flexsurvspline(model_inputs_period$surv_objects$so_male_p          ~ poly(Year,2)+ gamma1(Year) + gamma2(Year),
                             k=6, scale="odds", data= model_inputs_period$data_dfs$dF_male_p)

mod8_m_p_2 <- flexsurvspline(model_inputs_period$surv_objects$so_male_p          ~ poly(Year,3)+ gamma1(Year) + gamma2(Year),
                             k=6, scale="odds", data= model_inputs_period$data_dfs$dF_male_p)


mod9_m_p_2 <- flexsurvspline(model_inputs_period$surv_objects$so_male_p          ~ poly(Year,1)+ gamma1(Year) + gamma2(Year),
                             k=6, scale="odds", data= model_inputs_period$data_dfs$dF_male_p)
#-------------------------------------------------------------------------------- #
# 3. Comparing trends by AIC ----
mod7_f_c_1$AIC
mod8_f_c_1$AIC #best
mod9_f_c_1$AIC

mod7_m_c_1$AIC
mod8_m_c_1$AIC #best
mod9_m_c_1$AIC

mod7_f_p_1$AIC #best
mod8_f_p_1$AIC 
mod9_f_p_1$AIC

mod7_m_p_1$AIC
mod8_m_p_1$AIC #best
mod9_m_p_1$AIC
#-------------------------------------------------------------------------------- #
# 4. Saving Models  ----

model_output_f_c<- list(female_c_prop = list(
# mod1_f_c_1=mod1_f_c_1,
# mod2_f_c_1=mod2_f_c_1,
# mod3_f_c_1=mod3_f_c_1,
# mod4_f_c_1=mod4_f_c_1,
# mod5_f_c_1=mod5_f_c_1,
# mod6_f_c_1=mod6_f_c_1,
mod7_f_c_1=mod7_f_c_1,
mod8_f_c_1=mod8_f_c_1,
mod9_f_c_1=mod9_f_c_1),
female_c_nonprop = list(
#mod1_f_c_2=mod1_f_c_2,
# mod2_f_c_2=mod2_f_c_2,
# mod3_f_c_2=mod3_f_c_2,
# mod4_f_c_2=mod4_f_c_2,
# mod5_f_c_2=mod5_f_c_2,
# mod6_f_c_2=mod6_f_c_2,
mod7_f_c_2=mod7_f_c_2,
mod8_f_c_2=mod8_f_c_2,
mod9_f_c_2=mod9_f_c_2))
save(model_output_f_c, file = "D_Output/model_output_f_c.RData")

model_output_m_c<- list(male_c_prop = list(
# mod1_m_c_1=mod1_m_c_1,
# mod2_m_c_1=mod2_m_c_1,
# mod3_m_c_1=mod3_m_c_1,
# mod4_m_c_1=mod4_m_c_1,
# mod5_m_c_1=mod5_m_c_1,
# mod6_m_c_1=mod6_m_c_1,
mod7_m_c_1=mod7_m_c_1,
mod8_m_c_1=mod8_m_c_1,
mod9_m_c_1=mod9_m_c_1),
male_c_nonprop = list(
# mod1_m_c_2=mod1_m_c_2,
# mod2_m_c_2=mod2_m_c_2,
# mod3_m_c_2=mod3_m_c_2,
# mod4_m_c_2=mod4_m_c_2,
# mod5_m_c_2=mod5_m_c_2,
# mod6_m_c_2=mod6_m_c_2,
mod7_m_c_2=mod7_m_c_2,
mod8_m_c_2=mod8_m_c_2,
mod9_m_c_2=mod9_m_c_2))
save(model_output_m_c, file = "D_Output/model_output_m_c.RData")


model_output_f_p<- list(female_p_prop = list(
 # mod1_f_p_1=mod1_f_p_1,
#  mod2_f_p_1=mod2_f_p_1,
#  mod3_f_p_1=mod3_f_p_1,
#  mod4_f_p_1=mod4_f_p_1,
#  mod5_f_p_1=mod5_f_p_1,
#  mod6_f_p_1=mod6_f_p_1,
  mod7_f_p_1=mod7_f_p_1,
  mod8_f_p_1=mod8_f_p_1,
  mod9_f_p_1=mod9_f_p_1),
  female_p_nonprop = list(
#    mod1_f_p_2=mod1_f_p_2,
#    mod2_f_p_2=mod2_f_p_2,
#    mod3_f_p_2=mod3_f_p_2,
#    mod4_f_p_2=mod4_f_p_2,
#    mod5_f_p_2=mod5_f_p_2,
#    mod6_f_p_2=mod6_f_p_2,
    mod7_f_p_2=mod7_f_p_2,
    mod8_f_p_2=mod8_f_p_2,
    mod9_f_p_2=mod9_f_p_2))
save(model_output_f_p, file = "D_Output/model_output_f_p.RData")

model_output_m_p<- list(male_p_prop = list(
#  mod1_m_p_1=mod1_f_p_1,
#  mod2_m_p_1=mod2_f_p_1,
#  mod3_m_p_1=mod3_f_p_1,
#  mod4_m_p_1=mod4_f_p_1,
#  mod5_m_p_1=mod5_f_p_1,
#  mod6_m_p_1=mod6_f_p_1,
  mod7_m_p_1=mod7_m_p_1,
  mod8_m_p_1=mod8_m_p_1,
  mod9_m_p_1=mod9_m_p_1),
  male_p_nonprop = list(
#   mod1_m_p_2=mod1_f_p_2,
#    mod2_m_p_2=mod2_f_p_2,
#    mod3_m_p_2=mod3_f_p_2,
#    mod4_m_p_2=mod4_f_p_2,
#    mod5_m_p_2=mod5_f_p_2,
#    mod6_m_p_2=mod6_f_p_2,
    mod7_m_p_2=mod7_m_p_2,
    mod8_m_p_2=mod8_m_p_2,
    mod9_m_p_2=mod9_m_p_2))
save(model_output_m_p, file = "D_Output/model_output_m_p.RData")


female_c_prop  <- list(mod7_f_c_1=mod7_f_c_1, mod8_f_c_1=mod8_f_c_1, mod9_f_c_1=mod9_f_c_1)
male_c_prop    <- list(mod7_m_c_1=mod7_m_c_1, mod8_m_c_1=mod8_m_c_1, mod9_m_c_1=mod9_m_c_1)

female_p_prop  <- list(mod7_f_p_1=mod7_f_p_1, mod8_f_p_1=mod8_f_p_1, mod9_f_p_1=mod9_f_p_1)
male_p_prop    <- list(mod7_m_p_1=mod7_m_p_1, mod8_m_p_1=mod8_m_p_1, mod9_m_p_1=mod9_m_p_1)

final_models <- list(female_c_prop=female_c_prop,
                     male_c_prop  =male_c_prop  ,
                     female_p_prop=female_p_prop,
                     male_p_prop  =male_p_prop  )

save(final_models, file="D_Output/final_models.RData")

best_fit_prop_models_final <- list(mod8_f_c_1 = mod8_f_c_1, mod8_m_c_1 = mod8_m_c_1,
                                   mod9_f_p_1 = mod9_f_p_1, mod8_m_p_1 = mod8_m_p_1)
save(best_fit_prop_models_final, file="D_Output/best_fit_prop_models_final.RData")


