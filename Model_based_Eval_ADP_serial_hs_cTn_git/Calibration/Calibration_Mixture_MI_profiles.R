# Mixture weights
# Author: MLM
# Description : In this file I calibrate a mixture of 5 archetypical HS-cTn trajectories to 
# the targets of a population from the study Twerenbold et al 2019,"Outcome of Applying the ESC 0/1-hour Algorithm in Patients With Suspected Myocardial Infarction", 
#  https://pubmed.ncbi.nlm.nih.gov/31345421/
# ----------------------------------------------------------------------------- #
# 0.1 Libraries              ----
library(tidyr)
library(ggplot2)
library(dplyr)
library(stats)
library(tmvtnorm)
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
load(file = "Outputs/cTn_T_sample_params.RData")
load(file = "Outputs/cTn_I_sample_params.RData")
N_sim <- 5000
N_sim_plot <- 5000
seed <- 597
set.seed(seed)
##  Optim params from Calibration Step 1 ----
# load optim params
load("Outputs/optim_args_typeI_MI.RData")
load("Outputs/optim_args_typeII_MI.RData")
load("Outputs/optim_args_acute_MI.RData")
load("Outputs/optim_args_chronic_MI.RData")
load("Outputs/optim_args_No_MI.RData")
# Consolidate optim param sets
optim_params_l <- list(optim_args_typeI_MI   = optim_args_typeI_MI,
                       optim_args_typeII_MI  = optim_args_typeII_MI,
                       optim_args_acute_MI   = optim_args_acute_MI,
                       optim_args_chronic_MI = optim_args_chronic_MI,
                       optim_args_No_MI      = optim_args_No_MI)
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# 2. Calibration of mixture weights ----
## Inputs ----
# Initial values
v_init_values <- c(0.28,0.027,0.035,0.01,0.64) 
sum(v_init_values)
# targets, twerenbold
l_targets <- list(
  cTn_presentation = list(median = 8, pct25 = 4, pct75 = 15),
  cTn_1hr          = list(median = 9, pct25 = 5, pct75 = 19),
  cTn_delta_1hr    = list(median = 1, pct25 = 0, pct75 = 2 )
)
# Grid-box params
bandwidth     <- 0.15
bins          <- 5
epsilon       <- (bandwidth/(bins*5))
epsilon
# 3. optimal mixture ----
start_time <- Sys.time()
optim_output <- mixture_calibration_fun(    # mixture calibration function 
  v_init_values,                            # vector of initial values for weights
  l_targets,                                # list of targets for calibration
  bandwidth = bandwidth,
  bins      = bins,
  epsilon   = epsilon ,
  N_sim     = N_sim  ,                      # total sample size
  v_time    = seq(0,60,0.5),
  optim_params_l                            # list input parameters for each subpopulation
)
end_time <- Sys.time()
end_time - start_time
save(optim_output, file =  "Outputs/optim_output.RData" )
# ----------------------------------------------------------------------------- #
## Optimal Param. Configuration ----
index1 <- which.min(optim_output$comb_loss1)
index2 <- which.min(optim_output$comb_loss2)

optim_config1 <- optim_output[index1,]
optim_config1

optim_config2 <- optim_output[index2,]
optim_config2
optimal_mixture_configs <- list(optim_config1=optim_config1, optim_config2=optim_config2)
save(optimal_mixture_configs, file =  "Outputs/optimal_mixture_configs.RData")
# ----------------------------------------------------------------------------- #
# 4. Quick Diagnostic Plots ----
l_optim_params<- optim_params_l
l_targets
ylim <- 50
v_mixture_w<- optim_config2[1:5]
# Generating mixture trajectories
temp <- gen_mixture_trajectories_df(
                         N_sim = N_sim_plot  ,                # total sample size
                         v_time = seq(0,60,0.5),              # time horizon (grid)
                         l_optim_params =l_optim_params ,     # list input parameters for each subpopulation
                         v_mixture_w   = v_mixture_w          # optimal mixture weights)
)
# DFs for plots
cTn_presentation_df<- temp[temp$time_presentation==temp$time,]
cTn_1hr_df<- temp[temp$time_presentation+1==temp$time,]
cTn_delta_1hr_df<- cbind(cTn_presentation_df, delta_1hr = abs(cTn_1hr_df$cTn_concentration -cTn_presentation_df$cTn_concentration))
## Presentation ----
cTn_presentation<-ggplot(cTn_presentation_df, aes( x= cTn_concentration))+
  stat_boxplot( aes(x= cTn_concentration), 
                geom='errorbar', linetype=1, width=0.5)+
  geom_boxplot(fill='gray',outlier.shape = NA, alpha = 0.5)+
  coord_cartesian(xlim = c(0,   ylim) )+
  geom_vline(xintercept = l_targets$cTn_presentation$pct25, color = "red")+
  geom_vline(xintercept = l_targets$cTn_presentation$median, color = "red")+
  geom_vline(xintercept = l_targets$cTn_presentation$pct75, color = "red")+
  theme_light()+
  labs(title = "Presentation hs-cTn Concentration (ng/L)", 
       subtitle = "", 
       x = "hs-cTn Concentration (ng/L) "#, 
  ) + theme(axis.text.y=element_blank()) 
cTn_presentation
## 1 hr ----
cTn_1hr<-ggplot(cTn_1hr_df, aes( x= cTn_concentration))+
  stat_boxplot( aes(x= cTn_concentration), 
                geom='errorbar', linetype=1, width=0.5)+
  geom_boxplot(fill='gray',outlier.shape = NA, alpha = 0.5)+
  coord_cartesian(xlim = c(0,   ylim) )+
  geom_vline(xintercept = l_targets$cTn_1hr$pct25, color = "red")+
  geom_vline(xintercept = l_targets$cTn_1hr$median, color = "red")+
  geom_vline(xintercept = l_targets$cTn_1hr$pct75, color = "red")+
  theme_light()+
  labs(title = "hs-cTn Concentration (ng/L) 1hr after Presentation", 
       subtitle = "", 
       x = "hs-cTn Concentration (ng/L)"#, 
  ) + theme(axis.text.y=element_blank())

cTn_1hr
## delta 1hr ----
cTn_delta_1hr<-ggplot(cTn_delta_1hr_df, aes( x= delta_1hr))+
  stat_boxplot( aes(x= cTn_concentration), 
                geom='errorbar', linetype=1, width=0.5)+
  geom_boxplot(fill='gray',outlier.shape = NA, alpha = 0.5)+
  coord_cartesian(xlim = c(0,   ylim))+
  geom_vline(xintercept = l_targets$cTn_delta_1hr$pct25, color = "red")+
  geom_vline(xintercept = l_targets$cTn_delta_1hr$median, color = "red")+
  geom_vline(xintercept = l_targets$cTn_delta_1hr$pct75, color = "red")+
  theme_light()+
  labs(title = "Change in hs-cTn Concentration (ng/L) after 1hr", 
       subtitle = "", 
       x = "1hr Delta hs-cTn Concentration (ng/L) "#, 
  ) + theme(axis.text.y=element_blank())

cTn_delta_1hr
require(patchwork)

final_plot_mixture <- (cTn_presentation/ cTn_1hr +cTn_delta_1hr) + 
  plot_annotation(
    title = 'Calibration of mixture hs-cTn trajectories',
    subtitle = 'Predicted (black) vs Observed (red), n= 5,000',
    theme = theme(plot.title = element_text(size = 22))
  )
final_plot_mixture
ggsave(final_plot_mixture, file= "Figures/final_plot_mixture.png", width = 16, height = 13.2)

# ----------------------------------------------------------------------------- #

