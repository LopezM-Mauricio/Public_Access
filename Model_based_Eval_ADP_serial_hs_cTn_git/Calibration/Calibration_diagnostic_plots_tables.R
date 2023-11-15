# Diagnositc Plots and Tables
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
# 1.  Inputs ----
## 1.1  Mean, Covariance, Parameters Joint Distribution ----
load(file = "Outputs/cTn_I_sample_params.RData")
cTn_I_sample_params$mu_cTn_I
## 1.2 Targets (Wereski 2021) ----
l_targets_typeI_MI   <- list(peak= list(median = 538.4, quant_25pct = 85.0, quant_75pct = 3584.9), 
                             presentation = list(median = 91.0, quant_25pct =  30.2, quant_75pct = 492.5),
                             delta_12hrs  = list(median=177, quant_25pct =  21, quant_75pct = 1929),         # not clear if necessary or useful, paper is not clear how they measured it
                             time_presentation=list(median = 4, quant_25pct = 3, quant_75pct = 7)) 
l_targets_typeII_MI  <- list(peak= list(median = 123.8, quant_25pct = 48.1, quant_75pct = 599.7 ), 
                             presentation = list(median = 49.5, quant_25pct =  22.0, quant_75pct = 147.2), 
                             delta_12hrs  = list(median=46, quant_25pct =  10, quant_75pct = 365),           # not clear if necessary or useful, paper is not clear how they measured it
                             time_presentation=list(median = 4, quant_25pct = 3, quant_75pct = 7))  
l_targets_acute_MI   <- list(peak= list(median = 74.0,  quant_25pct = 37.1, quant_75pct = 307.1 ), 
                             presentation = list(median = 50.0, quant_25pct =  25.5, quant_75pct = 134.2), 
                             delta_12hrs  = list(median=57, quant_25pct =  17, quant_75pct = 384),           # not clear if necessary or useful, paper is not clear how they measured it
                             time_presentation=list(median = 4, quant_25pct = 3, quant_75pct = 7)) 
l_targets_chronic_MI <- list(peak= list(median = 55.2,  quant_25pct = 34.1, quant_75pct = 144.7 ), 
                             presentation = list(median = 51.1, quant_25pct =  30.5, quant_75pct = 130.2),
                             delta_12hrs  = list(median=6 ,quant_25pct =  2, quant_75pct = 22),              # not clear if necessary or useful, paper is not clear how they measured it
                             time_presentation=list(median = 5, quant_25pct = 3, quant_75pct = 13))   
l_targets_No_MI      <- list(peak= list(median = 2.6,   quant_25pct = 1 , quant_75pct = 7   ),     
                             presentation = list(median = 2.2,  quant_25pct =  1 , quant_75pct = 6  ),  
                             delta_12hrs  = list(median=1, quant_25pct =  0.1, quant_75pct = 1.5),           # not clear if necessary or useful, paper is not clear how they measured it
                             time_presentation=list(median = 5, quant_25pct = 2, quant_75pct = 13))   
## 1.3 Sample sizes
N_sim <- 5000
N_sim_plot <- 5000
# seed <- 597
# set.seed(seed)
# ----------------------------------------------------------------------------- #
# 1.4 load optim params
load("Outputs/optim_args_typeI_MI.RData")
load("Outputs/optim_args_typeII_MI.RData")
load("Outputs/optim_args_acute_MI.RData")
load("Outputs/optim_args_chronic_MI.RData")
load("Outputs/optim_args_No_MI.RData")

# 1.5 load trajectories with optim params
load("Outputs/typeI_df.RData")
load("Outputs/typeII_df.RData")
load("Outputs/acute_df.RData")
load("Outputs/chronic_df.RData")
load("Outputs/No_MI_df.RData")
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# Consolidate optim param sets
optim_params_l <- list(optim_args_typeI_MI   = optim_args_typeI_MI,
                       optim_args_typeII_MI  = optim_args_typeII_MI,
                       optim_args_acute_MI   = optim_args_acute_MI,
                       optim_args_chronic_MI = optim_args_chronic_MI,
                       optim_args_No_MI      = optim_args_No_MI)
save(optim_params_l, file = "Outputs/optim_params_l.RData")

# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# Diagnoistic Plots -----
# Obtain distribution of peak for all MI subpops
typeI_df   $MI <-"B. Type I"
typeII_df  $MI <-"C. Type II"
acute_df   $MI <-"D. Acute"
chronic_df $MI <-"E. Chronic"
No_MI_df   $MI <-"A. No MI"
data_all<- rbind( typeI_df   ,
                  typeII_df  ,
                  acute_df   ,
                  chronic_df ,
                  No_MI_df   )

#unique(data_all$MI)
peak_dist<- data_all%>% group_by(ID, MI) %>% summarize(peak = max(y))
peak_dist_TypeI   <- peak_dist[peak_dist$MI == "B. Type I",] 
peak_dist_TypeII  <- peak_dist[peak_dist$MI == "C. Type II",] 
peak_dist_Acute   <- peak_dist[peak_dist$MI == "D. Acute",] 
peak_dist_chronic <- peak_dist[peak_dist$MI == "E. Chronic",]  
peak_dist_noMI    <- peak_dist[peak_dist$MI == "A. No MI",] 

# IQR predicted
summary(peak_dist_TypeI$peak)
summary(peak_dist_TypeII$peak)
summary(peak_dist_Acute$peak)
summary(peak_dist_chronic$peak)
summary(peak_dist_noMI$peak)

# Obtain distribution of presentation
unique(data_all$MI)
# times presentation: 4,4,4,5,5
presentation_dist<- data_all%>% group_by(ID, MI)
presentation_dist_TypeI   <- presentation_dist[presentation_dist$MI == "B. Type I" & presentation_dist$time == l_targets_typeI_MI$time_presentation$median,] 
presentation_dist_TypeII  <- presentation_dist[presentation_dist$MI == "C. Type II"& presentation_dist$time == l_targets_typeII_MI$time_presentation$median,]  
presentation_dist_Acute   <- presentation_dist[presentation_dist$MI == "D. Acute"  & presentation_dist$time == l_targets_acute_MI$time_presentation$median,]  
presentation_dist_chronic <- presentation_dist[presentation_dist$MI == "E. Chronic"& presentation_dist$time == l_targets_chronic_MI$time_presentation$median,]   
presentation_dist_noMI    <- presentation_dist[presentation_dist$MI == "A. No MI"  & presentation_dist$time == l_targets_No_MI$time_presentation$median,]  
colnames(presentation_dist_TypeI)

# IQR predicted
summary(presentation_dist_TypeI$y)
summary(presentation_dist_TypeII$y)
summary(presentation_dist_Acute$y)
summary(presentation_dist_chronic$y)
summary(presentation_dist_noMI$y)

### 3.2.2 Boxplots HS-cTn Peak ----
require(scales)
#color palette from trajectories plot
hex <- hue_pal()(5)
# Type I
#set.seed(597)
#index<-sample(1:nrow(peak_dist_TypeI),1000)
bp_peak_typeI<-ggplot(peak_dist_TypeI, aes( x= peak))+
  #bp_peak_typeI<-ggplot(peak_dist_TypeI[index,], aes( x= peak))+
  #ggplot(peak_dist_TypeI, aes( x= peak))+
  stat_boxplot( aes(x= peak), 
                geom='errorbar', linetype=1, width=0.5)+
  geom_boxplot(fill=hex[4],outlier.shape = NA, alpha = 0.5)+
  coord_cartesian(xlim = c(0,   max(quantile(peak_dist_TypeI$peak, .75)*1.2,l_targets_typeI_MI$peak$quant_75pct*1.2)  ))+
  #geom_vline(xintercept = median(peak_dist_TypeI$peak), color = "blue")+
  geom_vline(xintercept = l_targets_typeI_MI$peak$quant_25pct, color = "red")+
  geom_vline(xintercept = l_targets_typeI_MI$peak$median, color = "red")+
  geom_vline(xintercept = l_targets_typeI_MI$peak$quant_75pct, color = "red")+
  theme_light()+
  labs(title = "Type I", 
       subtitle = "", 
       x = "Peak hs-cTn Concentration (ng/L)"#, 
  ) +
  theme(legend.title=element_text(size=22)
  )
bp_peak_typeI
#index<-sample(1:nrow(peak_dist_TypeII),N_sim)
bp_peak_typeII<-ggplot(peak_dist_TypeII, aes( x= peak))+
  stat_boxplot( aes(x= peak), 
                geom='errorbar', linetype=1, width=0.5)+
  geom_boxplot(fill=hex[5],outlier.shape = NA, alpha = 0.5)+
  coord_cartesian(xlim = c(0,   max(quantile(peak_dist_TypeII$peak, .75)*1.2,l_targets_typeII_MI$peak$quant_75pct*1.2)  ))+
  #geom_vline(xintercept = median(peak_dist_TypeI$peak), color = "blue")+
  geom_vline(xintercept = l_targets_typeII_MI$peak$quant_25pct, color = "red")+
  geom_vline(xintercept = l_targets_typeII_MI$peak$median, color = "red")+
  geom_vline(xintercept = l_targets_typeII_MI$peak$quant_75pct, color = "red")+
  theme_light()+
  labs(title = "Type II", 
       subtitle = "", 
       x = "Peak hs-cTn Concentration (ng/L)", 
  ) +
  theme(legend.title=element_text(size=22)
  )
bp_peak_typeII
#index<-sample(1:nrow(peak_dist_Acute),1000)
bp_peak_acute<-ggplot(peak_dist_Acute, aes( x= peak))+
  #bp_peak_acute<-ggplot(peak_dist_Acute[index,], aes( x= peak))+
  #ggplot(peak_dist_TypeI, aes( x= peak))+
  stat_boxplot( aes(x= peak), 
                geom='errorbar', linetype=1, width=0.5)+
  geom_boxplot(fill=hex[1],outlier.shape = NA, alpha = 0.5)+
  coord_cartesian(xlim = c(0,   max(quantile(peak_dist_Acute$peak, .75)*1.2,l_targets_acute_MI$peak$quant_75pct*1.2)  ))+
  #geom_vline(xintercept = median(peak_dist_TypeI$peak), color = "blue")+
  geom_vline(xintercept = l_targets_acute_MI$peak$quant_25pct, color = "red")+
  geom_vline(xintercept = l_targets_acute_MI$peak$median, color = "red")+
  geom_vline(xintercept = l_targets_acute_MI$peak$quant_75pct, color = "red")+
  theme_light()+
  labs(title = "Acute", 
       subtitle = "", 
       x = "Peak hs-cTn Concentration (ng/L)", 
  ) +
  theme(legend.title=element_text(size=22)
  )
bp_peak_acute
#index<-sample(1:nrow(peak_dist_chronic),1000)
bp_peak_chronic<- ggplot(peak_dist_chronic, aes( x= peak))+
  #bp_peak_chronic<- ggplot(peak_dist_chronic[index,], aes( x= peak))+
  #ggplot(peak_dist_TypeI, aes( x= peak))+
  stat_boxplot( aes(x= peak), 
                geom='errorbar', linetype=1, width=0.5)+
  geom_boxplot(fill=hex[2],outlier.shape = NA, alpha = 0.5)+
  coord_cartesian(xlim = c(0,   max(quantile(peak_dist_chronic$peak, .75)*1.2,l_targets_chronic_MI$peak$quant_75pct*1.2)  ))+
  #geom_vline(xintercept = median(peak_dist_TypeI$peak), color = "blue")+
  geom_vline(xintercept = l_targets_chronic_MI$peak$quant_25pct, color = "red")+
  geom_vline(xintercept = l_targets_chronic_MI$peak$median, color = "red")+
  geom_vline(xintercept = l_targets_chronic_MI$peak$quant_75pct, color = "red")+
  theme_light()+
  labs(title = "Chronic", 
       subtitle = "", 
       x = "Peak hs-cTn Concentration (ng/L)", 
  ) +
  theme(legend.title=element_text(size=22)
  )
bp_peak_chronic
#index<-sample(1:nrow(peak_dist_noMI),1000)
bp_peak_NoMI<-ggplot(peak_dist_noMI, aes( x= peak))+
  stat_boxplot( aes(x= peak), 
                geom='errorbar', linetype=1, width=0.5)+
  geom_boxplot(fill=hex[3],outlier.shape = NA, alpha = 0.5)+
  coord_cartesian(xlim = c(0,   max(quantile(peak_dist_noMI$peak, .75)*1.2,l_targets_No_MI$peak$quant_75pct*1.2)  ))+
  #geom_vline(xintercept = median(peak_dist_TypeI$peak), color = "blue")+
  geom_vline(xintercept = l_targets_No_MI$peak$quant_25pct, color = "red")+
  geom_vline(xintercept = l_targets_No_MI$peak$median, color = "red")+
  geom_vline(xintercept = l_targets_No_MI$peak$quant_75pct, color = "red")+
  theme_light()+
  labs(title = "No MI", 
       subtitle = "", 
       x = "Peak hs-cTn Concentration (ng/L)", 
  ) +
  theme(legend.title=element_text(size=22)
  )
bp_peak_NoMI
require(patchwork)

bxplot_peak_all <- (bp_peak_acute+bp_peak_chronic+bp_peak_NoMI)/ (bp_peak_typeI + bp_peak_typeII) + plot_annotation(
  title = 'Interquartile Range of Peak hs-cTn Concentration (ng/L)',
  subtitle = 'Predicted (black) vs Observed (red), n= 5,000',
  theme = theme(plot.title = element_text(size = 22))
  #caption = 'Disclaimer: None of these plots are insightful'
)
bxplot_peak_all
ggsave(bxplot_peak_all, file= "Figures/bxplot_peak_all.png", width = 16, height = 13.2)

# ------------------- #
# Boxplot all subpops peak
ggplot(peak_dist, aes( x= peak,color = MI))+
  #ggplot(peak_dist_TypeI, aes( x= peak))+
  # stat_boxplot( aes( x= peak,color = MI), 
  #               geom='errorbar', linetype=1, width=0.5)+
  geom_boxplot(outlier.shape = NA)+ 
  coord_cartesian(xlim = c(min(peak_dist$peak),quantile(peak_dist$peak, 0.99)))+
  #geom_vline(xintercept = median(peak_dist_TypeI$peak), color = "blue")+
  # geom_vline(xintercept = l_targets_typeI_MI$peak$quant_25pct, color = "red")+
  # geom_vline(xintercept = l_targets_typeI_MI$peak$median, color = "red")+
  # geom_vline(xintercept = l_targets_typeI_MI$peak$quant_75pct, color = "red")+
  #
  theme_light()+
  labs(title = "Interquartile Range of Predicted Peak hs-cTn Concentration (ng/L)", 
       subtitle = "by MI type", 
       x = "Peak hs-cTn Concentration (ng/L)", 
       y = "")+
  theme(axis.text.y=element_blank(), legend.position = "bottom")

### 3.2.2 Boxplots HS-cTn Presentation ----

# --------#
#set.seed(1)
#index<-sample(1:nrow(presentation_dist_TypeI),10000)
bp_prest_typeI<-ggplot(presentation_dist_TypeI, aes( x= y))+
  #bp_prest_typeI<-ggplot(presentation_dist_TypeI[index,], aes( x= y))+
  #ggplot(presentation_dist_TypeI, aes( x= peak))+
  stat_boxplot( aes(x= y), 
                geom='errorbar', linetype=1, width=0.5)+
  geom_boxplot(fill=hex[4],outlier.shape = NA, alpha = 0.5)+
  #coord_cartesian(xlim = c(min(presentation_dist_TypeI$y),quantile(presentation_dist_TypeI$y, .99)))+
  coord_cartesian(xlim = c(0,max(l_targets_typeI_MI$presentation$quant_75pct,quantile(presentation_dist_TypeI$y, .75))*1.2))+
  #geom_vline(xintercept = median(presentation_dist_TypeI$peak), color = "blue")+
  geom_vline(xintercept = l_targets_typeI_MI$presentation$quant_25pct, color = "red")+
  geom_vline(xintercept = l_targets_typeI_MI$presentation$median, color = "red")+
  geom_vline(xintercept = l_targets_typeI_MI$presentation$quant_75pct, color = "red")+
  theme_light()+
  labs(title = "Type I MI", 
       subtitle = "", 
       x = "Presentation hs-cTn Concentration (ng/L)", 
  ) +
  theme(legend.title=element_text(size=22)
  )
bp_prest_typeI
#index<-sample(1:nrow(presentation_dist_TypeII),10000)
bp_prest_typeII<-ggplot(presentation_dist_TypeII, aes( x= y))+
  #bp_prest_typeII<-ggplot(presentation_dist_TypeII[index,], aes( x= y))+
  #ggplot(presentation_dist_TypeI, aes( x= peak))+
  stat_boxplot( aes(x= y), 
                geom='errorbar', linetype=1, width=0.5)+
  geom_boxplot(fill=hex[5],outlier.shape = NA, alpha = 0.5)+
  #coord_cartesian(xlim = c(min(presentation_dist_TypeII$y),quantile(presentation_dist_TypeII$y, .997)))+
  coord_cartesian(xlim = c(0,max(l_targets_typeII_MI$presentation$quant_75pct,quantile(presentation_dist_TypeII$y, .75))*1.2))+
  #geom_vline(xintercept = median(presentation_dist_TypeI$peak), color = "blue")+
  geom_vline(xintercept = l_targets_typeII_MI$presentation$quant_25pct, color = "red")+
  geom_vline(xintercept = l_targets_typeII_MI$presentation$median, color = "red")+
  geom_vline(xintercept = l_targets_typeII_MI$presentation$quant_75pct, color = "red")+
  theme_light()+
  labs(title = "Type II", 
       subtitle = "", 
       x = "Presentation hs-cTn Concentration (ng/L)", 
  ) +
  theme(legend.title=element_text(size=22)
  )
bp_prest_typeII
#index<-sample(1:nrow(presentation_dist_Acute),1000)
bp_prest_acute<-ggplot(presentation_dist_Acute, aes( x= y))+
  #bp_prest_acute<-ggplot(presentation_dist_Acute[index,], aes( x= y))+
  #ggplot(presentation_dist_TypeI, aes( x= peak))+
  stat_boxplot( aes(x= y), 
                geom='errorbar', linetype=1, width=0.5)+
  geom_boxplot(fill=hex[1],outlier.shape = NA, alpha = 0.5)+
  #coord_cartesian(xlim = c(min(presentation_dist_Acute$y),quantile(presentation_dist_Acute$y, .994)))+
  coord_cartesian(xlim = c(0,max(l_targets_acute_MI$presentation$quant_75pct,quantile(presentation_dist_Acute$y, .75))*1.2))+
  #geom_vline(xintercept = median(presentation_dist_TypeI$peak), color = "blue")+
  geom_vline(xintercept = l_targets_acute_MI$presentation$quant_25pct, color = "red")+
  geom_vline(xintercept = l_targets_acute_MI$presentation$median, color = "red")+
  geom_vline(xintercept = l_targets_acute_MI$presentation$quant_75pct, color = "red")+
  theme_light()+
  labs(title = "Acute", 
       subtitle = "", 
       x = "Presentation hs-cTn Concentration (ng/L)", 
  ) +
  theme(legend.title=element_text(size=22)
  )
bp_prest_acute
#index<-sample(1:nrow(presentation_dist_chronic),10000)
bp_prest_chronic<- ggplot(presentation_dist_chronic, aes( x= y))+
  #bp_prest_chronic<- ggplot(presentation_dist_chronic[index,], aes( x= y))+
  
  #ggplot(presentation_dist_TypeI, aes( x= peak))+
  stat_boxplot( aes(x= y), 
                geom='errorbar', linetype=1, width=0.5)+
  geom_boxplot(fill=hex[2],outlier.shape = NA, alpha = 0.5)+
  #coord_cartesian(xlim = c(min(presentation_dist_chronic$y),quantile(presentation_dist_chronic$y, .994)))+
  coord_cartesian(xlim = c(0,max(l_targets_chronic_MI$presentation$quant_75pct,quantile(presentation_dist_chronic$y, .75))*1.2))+
  #geom_vline(xintercept = median(presentation_dist_TypeI$peak), color = "blue")+
  geom_vline(xintercept = l_targets_chronic_MI$presentation$quant_25pct, color = "red")+
  geom_vline(xintercept = l_targets_chronic_MI$presentation$median, color = "red")+
  geom_vline(xintercept = l_targets_chronic_MI$presentation$quant_75pct, color = "red")+
  theme_light()+
  labs(title = "Chronic", 
       subtitle = "", 
       x = "Presentation hs-cTn Concentration (ng/L)", 
  ) +
  theme(legend.title=element_text(size=22)
  )
bp_prest_chronic
#index<-sample(1:nrow(presentation_dist_noMI),10000)
bp_prest_NoMI<-ggplot(presentation_dist_noMI, aes( x= y))+
  #bp_prest_NoMI<-ggplot(presentation_dist_noMI[index,], aes( x= y))+
  #ggplot(presentation_dist_TypeI, aes( x= peak))+
  stat_boxplot( aes(x= y), 
                geom='errorbar', linetype=1, width=0.5)+
  geom_boxplot(fill=hex[3],outlier.shape = NA, alpha = 0.5)+
  #coord_cartesian(xlim = c(min(presentation_dist_noMI$y),quantile(presentation_dist_noMI$y, .995)))+
  coord_cartesian(xlim = c(0,max(l_targets_No_MI$presentation$quant_75pct,quantile(presentation_dist_noMI$y, .75))*1.2))+
  #geom_vline(xintercept = median(presentation_dist_TypeI$peak), color = "blue")+
  geom_vline(xintercept = l_targets_No_MI$presentation$quant_25pct, color = "red")+
  geom_vline(xintercept = l_targets_No_MI$presentation$median, color = "red")+
  geom_vline(xintercept = l_targets_No_MI$presentation$quant_75pct, color = "red")+
  theme_light()+
  labs(title = "No MI", 
       subtitle = "", 
       x = "Presentation hs-cTn Concentration (ng/L)", 
  ) +
  theme(legend.title=element_text(size=22)
  )
bp_prest_NoMI
require(patchwork)

bxplot_prest_all <- (bp_prest_acute + bp_prest_chronic +bp_prest_NoMI)/ (bp_prest_typeI +bp_prest_typeII) + plot_annotation(
  title = 'Interquartile Range of hs-cTn Concentration (ng/L) at Presentation to ED',
  subtitle = 'Predicted (black) vs Observed (red), n= 5,000',
  theme = theme(plot.title = element_text(size = 22))
  #caption = 'Disclaimer: None of these plots are insightful'
)
bxplot_prest_all
ggsave(bxplot_prest_all, file= "Figures/bxplot_prest_all.png", width = 16, height = 13.2)

