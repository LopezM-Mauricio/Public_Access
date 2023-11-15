# Micro-simulation, ADP performance assessment
# Author: MLM
# Description : Micro-simulation to compare performance of ADPs that use serial hs-cTn measurements
# Composed of three modules 
# Module  1: hs-cTn trajectories (from mixture model)
# Modeule 2: ADP performance (compare accuracy, ppv, npv.)
# Modelue 3: length of stay in ED (EDLoS)
# ----------------------------------------------------------------------------- #
# 0.1 LIBRARIES              ----
library(tidyr)
library(ggplot2)
library(dplyr)
library(stats)
library(tmvtnorm)
library(scam)
library(purrr)
library(patchwork)
library(viridis)
library(fitdistrplus)
# ----------------------------------------------------------------------------- #
# 0.2 FUNCTIONS ----
source("Funs/Calibration/Stochastic_Calibration_funs.R") 
source("Funs/Microsimulation/Microsimulation_funs.R")
# ----------------------------------------------------------------------------- #
# 0.3 INPUTS ----
## hs-cTn trajectories: calibrated model parameters ----
# Mean, Covariance, Parameters Joint Distribution of params 
load(file = "Outputs/cTn_T_sample_params.RData")
load(file = "Outputs/cTn_I_sample_params.RData")
# MI specific model params
load("Outputs/optim_params_l.RData")
# hs-cTn trajectories: mixture weights
load(file =  "Outputs/optimal_mixture_configs.RData")
## Waiting times: calibrated model parameters ----
# time to presentation
load("Outputs/time2presentation_mixture_params_fit_weib.RData")
# EDLoS ESC0/1
load("Outputs/EDLOS_0_1_params.RData")
# EDLoS ESC0/2
load("Outputs/EDLOS_0_2_params.RData")
# EDLoS ESC0/1/3
load("Outputs/EDLOS_0_1_3_params.RData")
## ADP Thresholds ----
### thresholds for ESC 0/1 algo 
thresholds_ESC_0_1<- list(
  esc_0_1_thresh_ro_1 = 5,
  esc_0_1_thresh_ro_2 = 12,
  esc_0_1_thresh_ro_3 = 3,
  
  esc_0_1_thresh_ri_1 = 52,
  esc_0_1_thresh_ri_2 = 5)
### thresholds for ESC 0/2 algo 
thresholds_ESC_0_2<- list(
  esc_0_2_thresh_ro_1 = 5,
  esc_0_2_thresh_ro_2 = 14,
  esc_0_2_thresh_ro_3 = 4,
  
  esc_0_2_thresh_ri_1 = 52,
  esc_0_2_thresh_ri_2 = 10)

### thresholds for ESC 0/1/3 algo 
thresholds_ESC_0_1_3<- list(
  esc_0_1_thresh_ro_1     = 5,
  esc_0_1_thresh_ro_2     = 12,
  esc_0_1_thresh_ro_3     = 3,
      
  esc_0_1_thresh_ri_1     = 52,
  esc_0_1_thresh_ri_2     = 5,

  # auxiliary thresholds
  esc_0_3_thresh_ro_1     = 7,
  esc_0_3_thresh_ri_1     = 7
)
## Samples Sizes -----
N_sim          <- 5*10^3
N_sim_plot     <- 5*10^3
## Common seed ----
common_seed<- 2
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# MODULE 1. hs-cTn trajectories ----
l_optim_params <- optim_params_l
set.seed(common_seed)
v_mixture_w<- optimal_mixture_configs$optim_config2[1:5]
trajectories <- gen_mixture_trajectories_df(
                                     N_sim = N_sim  ,                              # total simulation  size (No. unique individuals)
                                     v_time = seq(0,60,0.5), 
                                     l_optim_params =l_optim_params ,              # list input parameters for each subpopulation
                                     v_mixture_w   = v_mixture_w                   # optimal mixture weights)
)
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# MODULE 2. ADP Simulation----
## 2.1 ESC 0/1 algorithm ----
esc_0_1_performance <- sim_esc_0_1(
  trajectories  = trajectories,
  time2present  = time2presentation_mixture_params_fit_weib,
  thresholds    = thresholds_ESC_0_1,
  n             = N_sim
)
round(t(esc_0_1_performance$df_ADP_performance),3)
# ----------------------------------------------------------------------------- #
## 2.2 ESC 0/2 algorithm ----
esc_0_2_performance <- sim_esc_0_2(
  trajectories  = trajectories,
  time2present  = time2presentation_mixture_params_fit_weib,
  thresholds    = thresholds_ESC_0_2,
  n             = N_sim
)
round(t(esc_0_2_performance$df_ADP_performance),3)

# ----------------------------------------------------------------------------- #
## 2.3 ESC 0/1/3 algorithm ----
esc_0_1_3_performance <- sim_esc_0_1_3(
  trajectories  = trajectories,
  time2present  = time2presentation_mixture_params_fit_weib,
  thresholds    = thresholds_ESC_0_1_3,
  n             = N_sim
)
round(t(esc_0_1_3_performance$df_ADP_performance),3)
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# MODULE 3 ED-LoS  ----
# create individual functions
# quantiles 10%,25%,50%,75%,90%
## 3.1 ESC 0/1 algorithm ----
edlos_esc_0_1_summary <- edlos_esc_0_1(
  performance_data    = esc_0_1_performance,
  wating_times_params = EDLOS_0_1_params,
  n                   = N_sim
)
# ----------------------------------------------------------------------------- #
## 3.2 ESC 0/2 algorithm ---- 
edlos_esc_0_2_summary <- edlos_esc_0_2(
  performance_data    = esc_0_2_performance,
  wating_times_params = EDLOS_0_2_params,
  n                   = N_sim
)

# ----------------------------------------------------------------------------- #
## 4.3 ESC 0/1/3 algorithm ----
edlos_esc_0_1_3_summary <-edlos_esc_0_1_3(
  performance_data    = esc_0_1_3_performance,
  wating_times_params = EDLOS_0_1_3_params,
  n                   = N_sim
)
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# Summary Tables ----

# ESC 0/1
short_summary(
  performance = esc_0_1_performance$df_ADP_performance,
  ED_LoS = edlos_esc_0_1_summary,
  ADP = "ESC 0/1"
)
# ESC 0/2
short_summary(
  performance = esc_0_2_performance$df_ADP_performance,
  ED_LoS = edlos_esc_0_2_summary,
  ADP = "ESC 0/2"
)
# ESC 0/1/3
short_summary(
  performance = esc_0_1_3_performance$df_ADP_performance,
  ED_LoS = edlos_esc_0_1_3_summary,
  ADP = "ESC 0/1/3"
)

# ALL ADPs

View(rbind(short_summary(
  performance = esc_0_1_performance$df_ADP_performance,
  ED_LoS = edlos_esc_0_1_summary,
  ADP = "ESC 0/1"
),
short_summary(
  performance = esc_0_2_performance$df_ADP_performance,
  ED_LoS = edlos_esc_0_2_summary,
  ADP = "ESC 0/2"
),
short_summary(
  performance = esc_0_1_3_performance$df_ADP_performance,
  ED_LoS = edlos_esc_0_1_3_summary,
  ADP = "ESC 0/1/3"
)))

