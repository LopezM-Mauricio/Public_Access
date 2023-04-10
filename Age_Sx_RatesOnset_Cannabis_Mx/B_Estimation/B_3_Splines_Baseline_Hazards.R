#-------------------------------------------------------------------------------- #
# Modeling 1| Comparing spline specifications (flexible-parametric models)           #
# Notes: I test the spline specification, varying number of Knots, saturated vs non-saturated linear predictors, and odds vs hazard scale
# Comparison is done using AIC.
# I don't test polynomial trends here, I focus on the evidence for selecting the appropriate specification of the baseline hazard (spline)
# See file "Splines_TemporalTrends_Orthogonal_Polynomials" for the second part of the modeling analysis. 
# Author: Mauricio, Lopez Mendez
# Date: 11/07/2020
#-------------------------------------------------------------------------------- #
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
# ----------------------------------------------------------------------------- #
# 2. by Cohort ----
## 2.1 Strata 1 | Female -----
# Note: ns() specifies a natural spline with no internal knots(linear function of the predictor), 
# to test linear trend with only one slope coefficient, aka non-saturated specification
# factor() specifies a linear function of the predictor to test linear trends with one slope coefficient for each level of the predictor, aka saturated specification.
#----------------------------------#
sp.fit.mod1.ic.c.female.h_year_k1 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_female_c ~ ns(Cohort.dec),
                                                                      k = 1, scale = "hazard",
                                                                      data = model_inputs_cohort$data_dfs$df_frmale_c)
sp.fit.mod1.ic.c.female.h_year_k1$AIC
#---#
sp.fit.mod1.ic.c.female.h_year_k2 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_female_c ~ ns(Cohort.dec),
                                                                      k = 2, scale = "hazard",
                                                                      data = model_inputs_cohort$data_dfs$df_frmale_c)
sp.fit.mod1.ic.c.female.h_year_k2$AIC
#---#
sp.fit.mod1.ic.c.female.h_year_k3 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_female_c ~ ns(Cohort.dec),
                                                                      k = 3, scale = "hazard",
                                                                      data = model_inputs_cohort$data_dfs$df_frmale_c)
sp.fit.mod1.ic.c.female.h_year_k3$AIC
#---#
sp.fit.mod1.ic.c.female.h_year_k5 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_female_c ~ ns(Cohort.dec),
                                                                      k = 5, scale = "hazard",
                                                                      data = model_inputs_cohort$data_dfs$df_frmale_c)
sp.fit.mod1.ic.c.female.h_year_k5$AIC
#---#
sp.fit.mod1.ic.c.female.h_year_k7 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_female_c ~ ns(Cohort.dec),
                                                                      k = 7, scale = "hazard",
                                                                      data = model_inputs_cohort$data_dfs$df_frmale_c)
sp.fit.mod1.ic.c.female.h_year_k7$AIC
#----------------------------------#
sp.fit.mod1.ic.c.female.h_sat_k1 <-                    flexsurvspline(model_inputs_cohort$surv_objects$so_female_c ~ factor(Cohort.dec),
                                                                      k = 1, scale = "hazard",
                                                                      data = model_inputs_cohort$data_dfs$df_frmale_c)
sp.fit.mod1.ic.c.female.h_sat_k1$AIC
#---#
sp.fit.mod1.ic.c.female.h_sat_k2 <-                    flexsurvspline(model_inputs_cohort$surv_objects$so_female_c ~ factor(Cohort.dec),
                                                                      k = 2, scale = "hazard",
                                                                      data = model_inputs_cohort$data_dfs$df_frmale_c)
sp.fit.mod1.ic.c.female.h_sat_k2$AIC
#---#
sp.fit.mod1.ic.c.female.h_sat_k3 <-                    flexsurvspline(model_inputs_cohort$surv_objects$so_female_c ~ factor(Cohort.dec),
                                                                      k = 3, scale = "hazard",
                                                                      data = model_inputs_cohort$data_dfs$df_frmale_c)
sp.fit.mod1.ic.c.female.h_sat_k3$AIC
#---#
sp.fit.mod1.ic.c.female.h_sat_k5 <-                    flexsurvspline(model_inputs_cohort$surv_objects$so_female_c ~ factor(Cohort.dec),
                                                                      k = 5, scale = "hazard",
                                                                      data = model_inputs_cohort$data_dfs$df_frmale_c)
sp.fit.mod1.ic.c.female.h_sat_k5$AIC
#---#
sp.fit.mod1.ic.c.female.h_sat_k7 <-                    flexsurvspline(model_inputs_cohort$surv_objects$so_female_c ~ factor(Cohort.dec),
                                                                      k = 7, scale = "hazard",
                                                                      data = model_inputs_cohort$data_dfs$df_frmale_c)
sp.fit.mod1.ic.c.female.h_sat_k7$AIC
#-----------ODDS SCALE-----------------------#
sp.fit.mod1.ic.o.c.female.h_year_k1 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_female_c ~ ns(Cohort.dec),
                                                                      k = 1, scale = "odds",
                                                                      data = model_inputs_cohort$data_dfs$df_frmale_c)
sp.fit.mod1.ic.o.c.female.h_year_k1$AIC
#---#
sp.fit.mod1.ic.o.c.female.h_year_k2 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_female_c ~ ns(Cohort.dec),
                                                                      k = 2, scale = "odds",
                                                                      data = model_inputs_cohort$data_dfs$df_frmale_c)
sp.fit.mod1.ic.o.c.female.h_year_k2$AIC
#---#
sp.fit.mod1.ic.o.c.female.h_year_k3 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_female_c ~ ns(Cohort.dec),
                                                                      k = 3, scale = "odds",
                                                                      data = model_inputs_cohort$data_dfs$df_frmale_c)
sp.fit.mod1.ic.o.c.female.h_year_k3$AIC
#---#
sp.fit.mod1.ic.o.c.female.h_year_k5 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_female_c ~ ns(Cohort.dec),
                                                                      k = 5, scale = "odds",
                                                                      data = model_inputs_cohort$data_dfs$df_frmale_c)
sp.fit.mod1.ic.o.c.female.h_year_k5$AIC
#---#
sp.fit.mod1.ic.o.c.female.h_year_k7 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_female_c ~ ns(Cohort.dec),
                                                                        k = 7, scale = "odds",
                                                                        data = model_inputs_cohort$data_dfs$df_frmale_c)
sp.fit.mod1.ic.o.c.female.h_year_k7$AIC
#-------------------------------------------------------------------------------- #
## 2.2 Strata 2 | Male  -----
sp.fit.mod1.ic.c.male.h_year_k1 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ ns(Cohort.dec),
                                                                    k = 1, scale = "hazard",
                                                                    data = model_inputs_cohort$data_dfs$dF_male_c)
sp.fit.mod1.ic.c.male.h_year_k1$AIC
#---#
sp.fit.mod1.ic.c.male.h_year_k2 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ ns(Cohort.dec),
                                                                    k = 2, scale = "hazard",
                                                                    data = model_inputs_cohort$data_dfs$dF_male_c)
sp.fit.mod1.ic.c.male.h_year_k2$AIC
#---#
sp.fit.mod1.ic.c.male.h_year_k3 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ ns(Cohort.dec),
                                                                    k = 3, scale = "hazard",
                                                                    data = model_inputs_cohort$data_dfs$dF_male_c)
sp.fit.mod1.ic.c.male.h_year_k3$AIC
#---#
sp.fit.mod1.ic.c.male.h_year_k5 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ ns(Cohort.dec),
                                                                    k = 5, scale = "hazard",
                                                                    data = model_inputs_cohort$data_dfs$dF_male_c)
sp.fit.mod1.ic.c.male.h_year_k5$AIC
#---#
sp.fit.mod1.ic.c.male.h_year_k6 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ ns(Cohort.dec),
                                                                    k = 6, scale = "hazard",
                                                                    data = model_inputs_cohort$data_dfs$dF_male_c)
sp.fit.mod1.ic.c.male.h_year_k6$AIC
#---#
sp.fit.mod1.ic.c.male.h_year_k7 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ ns(Cohort.dec),
                                                                    k = 7, scale = "hazard",
                                                                    data = model_inputs_cohort$data_dfs$dF_male_c)
sp.fit.mod1.ic.c.male.h_year_k7$AIC
#---#
sp.fit.mod1.ic.c.male.h_year_k8 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ ns(Cohort.dec),
                                                                    k = 8, scale = "hazard",
                                                                    data = model_inputs_cohort$data_dfs$dF_male_c)
sp.fit.mod1.ic.c.male.h_year_k8$AIC
#---#
sp.fit.mod1.ic.c.male.h_sat_k1 <-                    flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ factor(Cohort.dec),
                                                                    k = 1, scale = "hazard",
                                                                    data = model_inputs_cohort$data_dfs$dF_male_c)
# -> does not converge
#---#
sp.fit.mod1.ic.c.male.h_sat_k2 <-                    flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ factor(Cohort.dec),
                                                                    k = 2, scale = "hazard",
                                                                    data = model_inputs_cohort$data_dfs$dF_male_c)
sp.fit.mod1.ic.c.male.h_sat_k2$AIC
#---#
sp.fit.mod1.ic.c.male.h_sat_k3 <-                    flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ factor(Cohort.dec),
                                                                    k = 3, scale = "hazard",
                                                                    data = model_inputs_cohort$data_dfs$dF_male_c)
sp.fit.mod1.ic.c.male.h_sat_k3$AIC
#---#
sp.fit.mod1.ic.c.male.h_sat_k5 <-                    flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ factor(Cohort.dec),
                                                                    k = 5, scale = "hazard",
                                                                    data = model_inputs_cohort$data_dfs$dF_male_c)
sp.fit.mod1.ic.c.male.h_sat_k5$AIC
#---#
sp.fit.mod1.ic.c.male.h_sat_k6 <-                    flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ factor(Cohort.dec),
                                                                    k = 6, scale = "hazard",
                                                                    data = model_inputs_cohort$data_dfs$dF_male_c)
sp.fit.mod1.ic.c.male.h_sat_k6$AIC
#---#
sp.fit.mod1.ic.c.male.h_sat_k7 <-                    flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ factor(Cohort.dec),
                                                                    k = 7, scale = "hazard",
                                                                    data = model_inputs_cohort$data_dfs$dF_male_c)
sp.fit.mod1.ic.c.male.h_sat_k7$AIC
#---#
sp.fit.mod1.ic.c.male.h_sat_k8 <-                    flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ factor(Cohort.dec),
                                                                    k = 8, scale = "hazard",
                                                                    data = model_inputs_cohort$data_dfs$dF_male_c)
sp.fit.mod1.ic.c.male.h_sat_k8$AIC
# -----------ODDS SCALE-----------------------#
sp.fit.mod1.ic.o.c.male.h_sat_k7<-         flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ factor(Cohort.dec),
                                                  k = 7, scale = "odds",
                                                  data = model_inputs_cohort$data_dfs$dF_male_c)
sp.fit.mod1.ic.o.c.male.h_sat_k7$AIC
#---#
sp.fit.mod1.ic.o.c.male.h_year_k7<-              flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ ns(Cohort.dec),
                                                   k = 7, scale = "odds",
                                                   data = model_inputs_cohort$data_dfs$dF_male_c)
sp.fit.mod1.ic.o.c.male.h_year_k7$AIC
#--------------------------------------------#
sp.fit.mod1.ic.o.c.male.h_year_k1 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ ns(Cohort.dec),
                                                                    k = 1, scale = "odds",
                                                                    data = model_inputs_cohort$data_dfs$dF_male_c)
sp.fit.mod1.ic.o.c.male.h_year_k1$AIC
#---#
sp.fit.mod1.ic.o.c.male.h_year_k2 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ ns(Cohort.dec),
                                                                    k = 2, scale = "odds",
                                                                    data = model_inputs_cohort$data_dfs$dF_male_c)
sp.fit.mod1.ic.o.c.male.h_year_k2$AIC
#---#
sp.fit.mod1.ic.o.c.male.h_year_k3 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ ns(Cohort.dec),
                                                                    k = 3, scale = "odds",
                                                                    data = model_inputs_cohort$data_dfs$dF_male_c)
sp.fit.mod1.ic.o.c.male.h_year_k3$AIC
#---#
sp.fit.mod1.ic.o.c.male.h_year_k5 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ ns(Cohort.dec),
                                                                    k = 5, scale = "odds",
                                                                    data = model_inputs_cohort$data_dfs$dF_male_c)
sp.fit.mod1.ic.o.c.male.h_year_k5$AIC
#---#
sp.fit.mod1.ic.o.c.male.h_year_k7 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ ns(Cohort.dec),
                                                                      k = 7, scale = "odds",
                                                                      data = model_inputs_cohort$data_dfs$dF_male_c)
sp.fit.mod1.ic.o.c.male.h_year_k7$AIC
#-------------------------------------------------------------------------------- #
#-------------------------------------------------------------------------------- #
# 3. by Period ----
## 3.1 Strata 2 | Male --------
# sp.fit.mod1.ic.p.male.h_year_k1 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ ns(Year),
#                                                                     k = 1, scale = "hazard",
#                                                                     data = model_inputs_period$data_dfs$dF_male_p)
#sp.fit.mod1.ic.p.male.h_year_k1$AIC
#---#
sp.fit.mod1.ic.p.male.h_year_k2 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ ns(Year),
                                                                    k = 2, scale = "hazard",
                                                                    data = model_inputs_period$data_dfs$dF_male_p)
sp.fit.mod1.ic.p.male.h_year_k2$AIC
#---#
sp.fit.mod1.ic.p.male.h_year_k3 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ ns(Year),
                                                                    k = 3, scale = "hazard",
                                                                    data = model_inputs_period$data_dfs$dF_male_p)
sp.fit.mod1.ic.p.male.h_year_k3$AIC
#---#
sp.fit.mod1.ic.p.male.h_year_k5 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ ns(Year),
                                                                    k = 5, scale = "hazard",
                                                                    data = model_inputs_period$data_dfs$dF_male_p)
sp.fit.mod1.ic.p.male.h_year_k5$AIC
#----------------------------------#
sp.fit.mod1.ic.p.male.h_sat_k1 <-                    flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ factor(Year),
                                                                    k = 1, scale = "hazard",
                                                                    data = model_inputs_period$data_dfs$dF_male_p)
# -> does not converge
sp.fit.mod1.ic.p.male.h_sat_k2 <-                    flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ factor(Year),
                                                                    k = 2, scale = "hazard",
                                                                    data = model_inputs_period$data_dfs$dF_male_p)
# -> does not converge
#---#
sp.fit.mod1.ic.p.male.h_sat_k3 <-                    flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ factor(Year),
                                                                    k = 3, scale = "hazard",
                                                                    data = model_inputs_period$data_dfs$dF_male_p)
# -> DOES NOT CONVERGE
#---#
sp.fit.mod1.ic.p.male.h_sat_k5 <-                    flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ factor(Year),
                                                                    k = 5, scale = "hazard",
                                                                    data = model_inputs_period$data_dfs$dF_male_p)
#-> DOES NOT CONVERGE
#-----------ODDS SCALE-----------------------#
sp.fit.mod1.ic.o.male.h_year_k1 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ ns(Year),
                                                                    k = 1, scale = "odds",
                                                                    data = model_inputs_period$data_dfs$dF_male_p)
#does not converge
#--#
sp.fit.mod1.ic.o.male.h_year_k2 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ ns(Year),
                                                                    k = 2, scale = "odds",
                                                                    data = model_inputs_period$data_dfs$dF_male_p)
sp.fit.mod1.ic.o.male.h_year_k2$AIC
#---#
sp.fit.mod1.ic.o.male.h_year_k3 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ ns(Year),
                                                                    k = 3, scale = "odds",
                                                                    data = model_inputs_period$data_dfs$dF_male_p)
sp.fit.mod1.ic.o.male.h_year_k3$AIC
#---#
sp.fit.mod1.ic.o.male.h_year_k5 <-                   flexsurvspline(model_inputs_cohort$surv_objects$so_male_c ~ ns(Year),
                                                                    k = 5, scale = "odds",
                                                                    data = model_inputs_period$data_dfs$dF_male_p)
sp.fit.mod1.ic.o.male.h_year_k5$AIC
#-------------------------------------------------------------------------------- #
## 3.2 Strata 1 | Female -------
sp.fit.mod1.ic.p.female.h_year_k1 <-                   flexsurvspline(model_inputs_period$surv_objects$so_female_p ~ ns(Year),
                                                                      k = 1, scale = "hazard",
                                                                      data = model_inputs_period$data_dfs$df_frmale_p)
sp.fit.mod1.ic.p.female.h_year_k1$AIC
#---#
sp.fit.mod1.ic.p.female.h_year_k2 <-                   flexsurvspline(model_inputs_period$surv_objects$so_female_p ~ ns(Year),
                                                                      k = 2, scale = "hazard",
                                                                      data = model_inputs_period$data_dfs$df_frmale_p)
sp.fit.mod1.ic.p.female.h_year_k2$AIC
#---#
sp.fit.mod1.ic.p.female.h_year_k3 <-                   flexsurvspline(model_inputs_period$surv_objects$so_female_p ~ ns(Year),
                                                                      k = 3, scale = "hazard",
                                                                      data = model_inputs_period$data_dfs$df_frmale_p)
sp.fit.mod1.ic.p.female.h_year_k3$AIC
#---#
sp.fit.mod1.ic.p.female.h_year_k5 <-                   flexsurvspline(model_inputs_period$surv_objects$so_female_p ~ ns(Year),
                                                                      k = 5, scale = "hazard",
                                                                      data = model_inputs_period$data_dfs$df_frmale_p)
sp.fit.mod1.ic.p.female.h_year_k5$AIC
#----------------------------------#
sp.fit.mod1.ic.p.female.h_sat_k1 <-                    flexsurvspline(model_inputs_period$surv_objects$so_female_p ~ factor(Year),
                                                                      k = 1, scale = "hazard",
                                                                      data = model_inputs_period$data_dfs$df_frmale_p)
# -> does not converge
#sp.fit.mod1.ic.p.female.h_sat_k1$AIC

#---#
sp.fit.mod1.ic.p.female.h_sat_k2 <-                    flexsurvspline(model_inputs_period$surv_objects$so_female_p ~ factor(Year),
                                                                      k = 2, scale = "hazard",
                                                                      data = model_inputs_period$data_dfs$df_frmale_p)
# -> does not converge
#sp.fit.mod1.ic.p.female.h_sat_k2$AIC
#---#
sp.fit.mod1.ic.p.female.h_sat_k3 <-                    flexsurvspline(model_inputs_period$surv_objects$so_female_p ~ factor(Year),
                                                                      k = 3, scale = "hazard",
                                                                      data = model_inputs_period$data_dfs$df_frmale_p)
# -> DOES NOT CONVERGE
#sp.fit.mod1.ic.p.female.h_sat_k3$AIC
#---#
sp.fit.mod1.ic.p.female.h_sat_k5 <-                    flexsurvspline(model_inputs_period$surv_objects$so_female_p ~ factor(Year),
                                                                      k = 5, scale = "hazard",
                                                                      data = model_inputs_period$data_dfs$df_frmale_p)
#-> DOES NOT CONVERGE
#sp.fit.mod1.ic.p.female.h_sat_k5$AIC
#-----------ODDS SCALE-----------------------#
sp.fit.mod1.ic.o.female.h_year_k1 <-                   flexsurvspline(model_inputs_period$surv_objects$so_female_p ~ ns(Year),
                                                                      k = 1, scale = "odds",
                                                                      data = model_inputs_period$data_dfs$df_frmale_p)
sp.fit.mod1.ic.o.female.h_year_k1$AIC
#---#
sp.fit.mod1.ic.o.female.h_year_k2 <-                   flexsurvspline(model_inputs_period$surv_objects$so_female_p ~ ns(Year),
                                                                      k = 2, scale = "odds",
                                                                      data = model_inputs_period$data_dfs$df_frmale_p)
sp.fit.mod1.ic.o.female.h_year_k2$AIC
#---#
sp.fit.mod1.ic.o.female.h_year_k3 <-                   flexsurvspline(model_inputs_period$surv_objects$so_female_p ~ ns(Year),
                                                                      k = 3, scale = "odds",
                                                                      data = model_inputs_period$data_dfs$df_frmale_p)
sp.fit.mod1.ic.o.female.h_year_k3$AIC
#---#
sp.fit.mod1.ic.o.female.h_year_k5 <-                   flexsurvspline(model_inputs_period$surv_objects$so_female_p ~ ns(Year),
                                                                      k = 5, scale = "odds",
                                                                      data = model_inputs_period$data_dfs$df_frmale_p)
sp.fit.mod1.ic.o.female.h_year_k5$AIC
