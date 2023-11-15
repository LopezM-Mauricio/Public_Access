# Calibration to MI-specific subpopulation, subpopulations based on five underlying potential diagnosis
# Subpopulation: Type II MI
# Author: MLM
# Description : In this file I calibrate the kinetic model to represent presentation and peak troponin concentration levels for each type II MI 
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
l_targets_typeII_MI  <- list(peak= list(median = 123.8, quant_25pct = 48.1, quant_75pct = 599.7 ), 
                             presentation = list(median = 49.5, quant_25pct =  22.0, quant_75pct = 147.2), 
                             delta_12hrs  = list(median=46, quant_25pct =  10, quant_75pct = 365),           # not clear if necessary or useful, paper is not clear how they measured it
                             time_presentation=list(median = 4, quant_25pct = 3, quant_75pct = 7))  
# ----------------------------------------------------------------------------- #
## Sample sizes ----
N_sim <- 5000
N_sim_plot <- 5000
seed <- 597
set.seed(seed)
# ----------------------------------------------------------------------------- #
# 2. Type II MI Calibration           ----
# ----------------------------------------------------------------------------- #

# Initial values
# a. Peak parameter, "c"
c      <- l_targets_typeII_MI$peak$median     #+ 25                                   # peak param
# b. Mean scaling constants,  for s1, and u1
k_s1   <- 1.31                                                                   # shape param
k_u1   <- 0.0075                                                                   # inflection point param
k      <- c(k_s1,k_u1)
# c. Variance scaling constants, we are only scaling variance for s1 and u1
l1     <- 0.85#1
l2     <- 1 #1
l3     <- 0.85
l4     <- 1.5
l      <- c(l1,l2,l3,l4)

v_cparams<- c(
  c                                                                             #scale constant peak concentration
  , l                                                                           #scale constant for covariance matrix
  , k                                                                           #scale constant for mean of s1 and u1 
)
v_cparams                                                                       # length 5
# ----------------------------------------------------------------------------- #
start_time <- Sys.time()
tau_k_optim_typeII_MI <-optim(
  v_cparams, 
  stochastic_calibration_fun, # objective function, stochastic optimization
  N_sim       = N_sim,
  w           = 1, 
  v_mean      = unlist(cTn_I_sample_params$mu_cTn_I), 
  m_sigma     = cTn_I_sample_params$Sigma_cTn_I,
  l_targets   = l_targets_typeII_MI,                           
  v_time      = seq(0,60,0.5),                                                         # time horizon
  
  method      = "L-BFGS-B",
  lower       = c(l_targets_typeII_MI$peak$quant_25pct,
                  0.8,0.9,0.8,0.9,
                  0.7,0.0065), 
  upper       = c(l_targets_typeII_MI$peak$quant_75pct,
                  2,2,2,2,
                  1.5,1)
)
tau_k_optim_typeII_MI
end_time <- Sys.time()
start_time-end_time
# ----------------------------------------------------------------------------- #
# Saving optimal configuration 
optim_args_typeII_MI <- list(  # time = 0:60, 
  s1     =   cTn_I_sample_params$mu_cTn_I[1],
  s2     =   cTn_I_sample_params$mu_cTn_I[2],
  u1     =   cTn_I_sample_params$mu_cTn_I[3],
  u2     =   cTn_I_sample_params$mu_cTn_I[4],
  # scaling constants
  c      =   tau_k_optim_typeII_MI$par[1],
  l      =   tau_k_optim_typeII_MI$par[2:5],
  k      =   tau_k_optim_typeII_MI$par[6:7],
  target =   l_targets_typeII_MI)

# Save optim params
save(optim_args_typeII_MI, file = "Outputs/optim_args_typeII_MI.RData")
# ----------------------------------------------------------------------------- #
# trajectories with optim params 
typeII_df <-gen_MI_specific_trajectories_df(
  v_cparams = c(optim_args_typeII_MI$c, optim_args_typeII_MI$l, optim_args_typeII_MI$k),
  N_sim = N_sim_plot,
  v_mean = c(optim_args_typeII_MI$s1,optim_args_typeII_MI$s2, optim_args_typeII_MI$u1,optim_args_typeII_MI$u2), 
  m_sigma = cTn_I_sample_params$Sigma_cTn_I,
)
save(typeII_df, file = "Outputs/typeII_df.RData")
# ----------------------------------------------------------------------------- #
# 3. Quick diagnostics ----
typeII_df$MI <- "C. Type II"
summary(typeII_df[typeII_df$time==4,]$y)
summary(abs(typeII_df[typeII_df$time==4+12,]$y -typeII_df[typeII_df$time==4,]$y))
summary(typeII_df%>% group_by(ID)%>%summarise(max = max(y)))

temp <- typeII_df %>% group_by(ID)%>%summarise(max_cTn = max(y))
summary(temp$max_cTn)
typeII_IQR_y <-typeII_df%>% group_by(time)%>%summarize(
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
typeII_IQR_y$MI <- "Type II"

ggplot(typeII_IQR_y,aes(x=time, y = median_y))+
  geom_line()+
  geom_ribbon(aes(ymin = pct_10_y, ymax = pct_90_y), alpha =0.2)+
  coord_cartesian(xlim=c(0,60), ylim =c(0,600))   #+
