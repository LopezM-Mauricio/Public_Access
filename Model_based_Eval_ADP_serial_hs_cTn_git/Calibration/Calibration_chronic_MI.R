# Calibration to MI-specific subpopulation, subpopulations based on five underlying potential diagnosis
# Subpopulation: Chronic MI
# Author: MLM
# Description : In this file I calibrate the kinetic model to represent presentation and peak troponin concentration levels for each chronic MI 
# Wereski reports presentation and peak troponin levels using  hs-cTn I assay
# ----------------------------------------------------------------------------- #
# 0.1 Libraries              ----
library(tidyr)
library(ggplot2)
library(dplyr)
library(stats)
library(tmvtnorm)
library(truncnorm)
library(scam)
library(purrr)
library(patchwork)
library(viridis)

# 0.2 Aux Funs               ----
source("Funs/Calibration/Stochastic_Calibration_funs.R")
source("Funs/Microsimulation/Microsimulation_funs.R") 
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# 1. Global Inputs                  ----
## Mean, Covariance, Parameters Joint Distribution ----
load(file = "Outputs/cTn_I_sample_params.RData")
## Targets (Wereski 2021) ----
l_targets_chronic_MI <- list(peak= list(median = 55.2,  quant_25pct = 34.1, quant_75pct = 144.7 ), 
                             presentation = list(median = 51.1, quant_25pct =  30.5, quant_75pct = 130.2),
                             delta_12hrs  = list(median=6 ,quant_25pct =  2, quant_75pct = 22),              # not clear if necessary or useful, paper is not clear how they measured it
                             time_presentation=list(median = 5, quant_25pct = 3, quant_75pct = 13))   
# ----------------------------------------------------------------------------- #
## Sample sizes ----
N_sim <- 5000
N_sim_plot <- 5000
seed <- 597
set.seed(seed)
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# 2. Chronic MI Calibration            ----
# Initial values
# a. Peak parameter, "c"
c      <- l_targets_chronic_MI$peak$median    # + 30                                   # peak param
# b. Mean scaling constants,  for s1, and u1
k_s1   <- 1.1                                                                   # shape param
k_u1   <- 0.0006                                                                   # inflection point param
k      <- c(k_s1,k_u1)
# c. Variance scaling constants, we are only scaling variance for s1 and u1
l1     <- 0.5 #1
l2     <- 1 #1
l3     <- 0.87
l4     <- 1
l      <- c(l1,l2,l3,l4)

v_cparams<- c(
  c                                                                           #peak concentration
  , l                                                                           #scale constant for covariance matrix
  , k                                                                           #scale constant for mean of s1 and u1 
)
v_cparams                                                                       # length 5
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# Optimization
start_time <- Sys.time()
tau_k_optim_chronic_MI <-optim(
  v_cparams, 
  stochastic_calibration_fun, # objective function, stochastic optimization
  N_sim       = N_sim,
  w           = 1, 
  v_mean      = unlist(cTn_I_sample_params$mu_cTn_I), 
  m_sigma     = cTn_I_sample_params$Sigma_cTn_I,
  l_targets   = l_targets_chronic_MI,                           
  v_time      = seq(0,60,0.5),                                                         # time horizon
  
  method      = "L-BFGS-B",
  lower       = c(l_targets_chronic_MI$peak$quant_25pct,
                  0.6,0.9,0.7,0.95,
                  0.9,0.0001), 
  upper       = c(l_targets_chronic_MI$peak$quant_75pct,
                  2,2,2,2,
                  1.5,1)
)
tau_k_optim_chronic_MI
end_time <- Sys.time()
start_time-end_time
# ----------------------------------------------------------------------------- #
# Saving optimal configuration 
optim_args_chronic_MI <- list(   #time = 0:60, 
                                 s1     =   cTn_I_sample_params$mu_cTn_I[1],
                                 s2     =   cTn_I_sample_params$mu_cTn_I[2],
                                 u1     =   cTn_I_sample_params$mu_cTn_I[3],
                                 u2     =   cTn_I_sample_params$mu_cTn_I[4],
                                 # scaling constants
                                 c      =   tau_k_optim_chronic_MI$par[1],
                                 l      =   tau_k_optim_chronic_MI$par[2:5],
                                 k      =   tau_k_optim_chronic_MI$par[6:7],
                                 target =   l_targets_chronic_MI)


# Save optim params
save(optim_args_chronic_MI, file = "Outputs/optim_args_chronic_MI.RData")
# ----------------------------------------------------------------------------- #
# trajectories with optim params
chronic_df <- gen_MI_specific_trajectories_df(
  v_cparams = c(optim_args_chronic_MI$c, optim_args_chronic_MI$l, optim_args_chronic_MI$k),
  N_sim = N_sim_plot,
  v_mean = c(optim_args_chronic_MI$s1,optim_args_chronic_MI$s2, optim_args_chronic_MI$u1,optim_args_chronic_MI$u2), 
  m_sigma = cTn_I_sample_params$Sigma_cTn_I,
)
save(chronic_df, file = "Outputs/chronic_df.RData")
# ----------------------------------------------------------------------------- #
# 3. Quick Diagnostics ----
chronic_df$MI <- "E. Chronic"
summary(chronic_df[chronic_df$time==5,]$y)
summary(abs(chronic_df[chronic_df$time==5+12,]$y -chronic_df[chronic_df$time==5,]$y))
summary(chronic_df%>% group_by(ID)%>%summarise(max = max(y)))

temp <- chronic_df %>% group_by(ID)%>%summarise(max_cTn = max(y))
summary(temp$max_cTn)
chronic_IQR_y <-chronic_df%>% group_by(time)%>%summarize(
  min_y    = min(y),
  pct_05_y = quantile(y, c(.05)),
  pct_10_y = quantile(y, c(.10)),
  pct_25_y = quantile(y, c(.25)), 
  median_y = median(y), 
  pct_75_y = quantile(y, c(.75)),
  pct_90_y = quantile(y, c(.90)),
  pct_95_y = quantile(y, c(.95)),
  max_y    = max(y)
)
chronic_IQR_y$MI <- "Chronic"

ggplot(chronic_IQR_y,aes(x=time, y = median_y))+
  geom_line()+
  geom_ribbon(aes(ymin = pct_10_y, ymax = pct_90_y), alpha =0.2)+
  coord_cartesian(xlim=c(0,60), ylim =c(0,600))   #+
#geom_vline(xintercept = c(4,12))#+
#geom_hline(yintercept = 458.8)
summary(chronic_IQR_y$median_y)
