# Calibration to MI-specific subpopulation, subpopulations based on five underlying potential diagnosis
# Subpopulation: Type I MI
# Author: MLM
# Description : In this file I calibrate the kinetic model to represent presentation and peak troponin concentration levels for each type I MI 
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
cTn_I_sample_params$mu_cTn_I
## Targets (Wereski 2021) ----
l_targets_typeI_MI   <- list(peak= list(median = 538.4, quant_25pct = 85.0, quant_75pct = 3584.9), 
                             presentation = list(median = 91.0, quant_25pct =  30.2, quant_75pct = 492.5),
                             delta_12hrs  = list(median=177, quant_25pct =  21, quant_75pct = 1929),         # not clear if necessary or useful, paper is not clear how they measured it
                             time_presentation=list(median = 4, quant_25pct = 3, quant_75pct = 7)) 
# ----------------------------------------------------------------------------- #
## Sample sizes ----
N_sim <- 5000
N_sim_plot <- 5000
seed <- 597
set.seed(seed)
# ----------------------------------------------------------------------------- #
# 2. Type I MI Calibration            ----
# Initial values
# a. Peak parameter, "c"
c      <- l_targets_typeI_MI$peak$median     + 40                               # peak param
# b. Mean scaling constants,  for s1, and u1
k_s1   <- 1.2                                                                   # shape param
k_u1   <- 0.09                                                                  # inflection point param
k      <- c(k_s1,k_u1)
# c. Variance scaling constants, we are only scaling variance for s1 and u1
l1     <- 0.5  #Ks1
l2     <- 0.5  #Ku1
l3     <- 0.99 #ks2
l4     <- 1.5  #ku2
l      <- c(l1,l2,l3,l4)
v_cparams<- c(
  c                                                                             # scale constant peak concentration
  , l                                                                           # scale constant for covariance matrix
  , k                                                                           # scale constant for  s1 and u1 
)
v_cparams                                                                       
# ----------------------------------------------------------------------------- #
# Optimization routine
# Optim with lower/upper values for peak parameter and scaling factors
start_time <- Sys.time()
tau_k_optim_typeI_MI <-optim(
  v_cparams, 
  stochastic_calibration_fun, # objective function, stochastic optimization
  N_sim       = N_sim,
  w           = 1, 
  v_mean      = unlist(cTn_I_sample_params$mu_cTn_I), 
  m_sigma     = cTn_I_sample_params$Sigma_cTn_I,
  l_targets   = l_targets_typeI_MI,       
  v_time      = seq(0,60,0.5),                                                  # time horizon
  
  
  method      = "L-BFGS-B",
  lower       = c(l_targets_typeI_MI$peak$quant_25pct,
                  0.8,0.85,0.8,0.85,
                  0.7,0.01), 
  upper       = c(l_targets_typeI_MI$peak$quant_75pct,
                  2,2,2,2,
                  1.5,1.2)
)
tau_k_optim_typeI_MI
end_time <- Sys.time()
start_time-end_time
# ----------------------------------------------------------------------------- #
# Saving optimal configuration 
optim_args_typeI_MI <- list(   #time = 0:60, 
  s1     =   cTn_I_sample_params$mu_cTn_I[1],
  s2     =   cTn_I_sample_params$mu_cTn_I[2],
  u1     =   cTn_I_sample_params$mu_cTn_I[3],
  u2     =   cTn_I_sample_params$mu_cTn_I[4],
  # scaling constants
  c      =   tau_k_optim_typeI_MI$par[1],
  l      =   tau_k_optim_typeI_MI$par[2:5],
  k      =   tau_k_optim_typeI_MI$par[6:7],
  target =   l_targets_typeI_MI)

# Save optim params
save(optim_args_typeI_MI, file = "Outputs/optim_args_typeI_MI.RData")
# ----------------------------------------------------------------------------- #
# Simulate trajectories with optim params 
typeI_df <- gen_MI_specific_trajectories_df(
  v_cparams = c(optim_args_typeI_MI$c, optim_args_typeI_MI$l, optim_args_typeI_MI$k),
  N_sim = N_sim_plot,
  v_mean = c(optim_args_typeI_MI$s1,optim_args_typeI_MI$s2, optim_args_typeI_MI$u1,optim_args_typeI_MI$u2), 
  m_sigma = cTn_I_sample_params$Sigma_cTn_I)
save(typeI_df, file = "Outputs/typeI_df.RData")
# ----------------------------------------------------------------------------- #
# 3. Quick Diagnostics ----
typeI_df$MI <- "B. Type I"
summary(typeI_df[typeI_df$time== 4,]$y) # at presentation 
summary(abs(typeI_df[typeI_df$time==4+12,]$y -typeI_df[typeI_df$time==4,]$y))
summary(abs(typeI_df[typeI_df$time==4+1,]$y -typeI_df[typeI_df$time==4,]$y))

summary(typeI_df%>% group_by(ID)%>%summarise(max = max(y)))

temp <- typeI_df %>% group_by(ID)%>%summarise(max_cTn = max(y))
summary(temp$max_cTn)
typeI_IQR_y <-typeI_df%>% group_by(time)%>%summarize(
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
typeI_IQR_y$MI <- "Type I"

ggplot(typeI_IQR_y,aes(x=time, y = median_y))+
  geom_line()+
  geom_ribbon(aes(ymin = pct_10_y, ymax = pct_90_y), alpha =0.2)+
  coord_cartesian(xlim=c(0,60), ylim =c(0,650)) 

