# Calibration to MI-specific subpopulation, subpopulations based on five underlying potential diagnosis
# Subpopulation: No MI
# Author: MLM
# Description : In this file I calibrate the kinetic model to represent presentation and peak troponin concentration levels for each no MI 
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
l_targets_No_MI      <- list(peak= list(median = 2.6,   quant_25pct = 1 , quant_75pct = 7   ),     
                             presentation = list(median = 2.2,  quant_25pct =  1 , quant_75pct = 6  ),  
                             delta_12hrs  = list(median=1, quant_25pct =  0.1, quant_75pct = 1.5),           # not clear if necessary or useful, paper is not clear how they measured it
                             time_presentation=list(median = 5, quant_25pct = 2, quant_75pct = 13))   



# ----------------------------------------------------------------------------- #
## Sample sizes -----
N_sim <- 5000
N_sim_plot <- 5000
seed <- 597
set.seed(seed)
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# 2. No MI  Calibration          ----
# Initial values
# a. Peak parameter, "c"
c      <- l_targets_No_MI$peak$median    +1.1                                   # peak param
# b. Mean scaling constants,  for s1, and u1
k_s1   <- 1.5                                                                   # shape param
k_u1   <- 1                                                                     # inflection point param
k      <- c(k_s1,k_u1)
# c. Variance scaling constants, we are only scaling variance for s1 and u1
l1     <- 0.65 #1
l2     <- 1 #1
l3     <- 1
l4     <- 0.9
l      <- c(l1,l2,l3,l4)
v_cparams<- c(
  c                                                                             #scale constantpeak concentration
  , l                                                                           #scale constant for covariance matrix
  , k                                                                           #scale constant for mean of s1 and u1 
)
v_cparams                                                                       # length 5
# ----------------------------------------------------------------------------- #
# # Optim with lower values for peak parameter (tau) and covariance scaling factor (k)
# # Modify value of shape 2 to accommodate flat curve
v_mean      <- cTn_I_sample_params$mu_cTn_I
v_mean[1]   <- 10
v_mean[2]   <- 1
v_mean[3]   <- 2.65
v_mean[4]   <- 10
#v_mean$n2  <-cTn_I_sample_params$mu_cTn_I[2]*.5

start_time <- Sys.time()
tau_k_optim_No_MI <-optim(
  v_cparams, 
  stochastic_calibration_fun, # objective function, stochastic optimization
  N_sim       = N_sim,
  w           = 1, 
  v_mean      = unlist(v_mean), 
  m_sigma     = cTn_I_sample_params$Sigma_cTn_I,
  l_targets   = l_targets_No_MI,                           
  v_time      = seq(0,60,0.2),                                                  # time horizon
  
  method      = "L-BFGS-B",
  lower       = c(0.01,#l_targets_No_MI$peak$quant_25pct,
                  0.3,0.9,0.9,0.85,
                  0.85,0.007), 
  upper       = c(l_targets_No_MI$peak$quant_75pct,
                  2,2,2,2,
                  1.5,1)
)
tau_k_optim_No_MI
end_time <- Sys.time()
start_time-end_time
# ----------------------------------------------------------------------------- #
# Saving optimal configuration 
optim_args_No_MI <- list(   #time = 0:60, 
                            s1     =   v_mean[1],
                            s2     =   v_mean[2],
                            u1     =   v_mean[3],
                            u2     =   v_mean[4],
                            # scaling constants
                            c      =   tau_k_optim_No_MI$par[1],
                            l      =   tau_k_optim_No_MI$par[2:5],
                            k      =   tau_k_optim_No_MI$par[6:7],
                            target =   l_targets_No_MI)
# Save optim params
save(optim_args_No_MI, file = "Outputs/optim_args_No_MI.RData")
# ----------------------------------------------------------------------------- #
# trajectories with optim params
No_MI_df <- gen_MI_specific_trajectories_df(
  v_cparams = c(optim_args_No_MI$c, optim_args_No_MI$l, optim_args_No_MI$k),
  N_sim = N_sim_plot,
  v_mean = c(optim_args_No_MI$s1,optim_args_No_MI$s2, optim_args_No_MI$u1,optim_args_No_MI$u2), 
  m_sigma = cTn_I_sample_params$Sigma_cTn_I,
)
save(No_MI_df, file = "Outputs/No_MI_df.RData")
# ----------------------------------------------------------------------------- #
# 3. Quick Diagnositcs ----
No_MI_df$MI <- "A. No MI"
summary(No_MI_df[No_MI_df$time==5,]$y)
summary(abs(No_MI_df[No_MI_df$time==5+1,]$y -No_MI_df[No_MI_df$time==5,]$y))
summary(No_MI_df%>% group_by(ID)%>%summarise(max = max(y)))

temp <- No_MI_df %>% group_by(ID)%>%summarise(max_cTn = max(y))
summary(temp$max_cTn)
No_MI_IQR_y <-No_MI_df%>% group_by(time)%>%summarize(
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
No_MI_IQR_y$MI <- "No MI"

ggplot(No_MI_IQR_y,aes(x=time, y = median_y))+
  geom_line()+
  geom_ribbon(aes(ymin = pct_10_y, ymax = pct_90_y), alpha =0.2)+
  coord_cartesian(xlim=c(0,60), ylim =c(0,15))   +
  geom_vline(xintercept = c(5,6))#+
#geom_hline(yintercept = 458.8)

