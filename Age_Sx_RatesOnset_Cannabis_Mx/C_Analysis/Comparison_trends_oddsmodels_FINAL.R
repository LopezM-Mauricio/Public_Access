# Splines, Interactions, GAMs
#Load libraries

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
library(patchwork)

library(reshape2)
library(readxl)
library(openxlsx)
library(dampack)
library(viridis)
#-------------------------------------------------------
## ---------------------------------------------------------------------------------------------------------- #
load("model_inputs_cohort.RData")
load("model_inputs_period.RData")
# ---------------------------------------------------------------------------------------------------------- #
model_inputs_cohort$data_dfs$df_frmale_c$Cohort.dec <- ifelse(model_inputs_cohort$data_dfs$df_frmale_c$Cohort.dec== 1, 1930,
                                                              ifelse(model_inputs_cohort$data_dfs$df_frmale_c$Cohort.dec==2, 1940,
                                                                     ifelse(model_inputs_cohort$data_dfs$df_frmale_c$Cohort.dec==3,1950,
                                                                            ifelse(model_inputs_cohort$data_dfs$df_frmale_c$Cohort.dec==4,1960,
                                                                                   ifelse(model_inputs_cohort$data_dfs$df_frmale_c$Cohort.dec==5,1970,
                                                                                          ifelse(model_inputs_cohort$data_dfs$df_frmale_c$Cohort.dec==6,1980,1990))))))

model_inputs_cohort$data_dfs$dF_male_c$Cohort.dec <- ifelse(model_inputs_cohort$data_dfs$dF_male_c$Cohort.dec== 1, 1930,
                                                            ifelse(model_inputs_cohort$data_dfs$dF_male_c$Cohort.dec==2,  1940,
                                                                   ifelse(model_inputs_cohort$data_dfs$dF_male_c$Cohort.dec==3,  1950,
                                                                          ifelse(model_inputs_cohort$data_dfs$dF_male_c$Cohort.dec==4,  1960,
                                                                                 ifelse(model_inputs_cohort$data_dfs$dF_male_c$Cohort.dec==5,  1970,
                                                                                        ifelse(model_inputs_cohort$data_dfs$dF_male_c$Cohort.dec==6,  1980,
                                                                                               1990))))))

# d1_f_c_1 <-flexsurvspline(model_inputs_cohort$surv_objects$so_female_c ~            poly(Cohort.dec,1,raw=F),
#                           k = 3, scale = "odds",           
#                           data = model_inputs_cohort$data_dfs$df_frmale_c) 
# round(d1_f_c_1$res,3)
# d2_f_c_1 <-flexsurvspline(model_inputs_cohort$surv_objects$so_female_c ~            poly(Cohort.dec,2,raw=F),
#                           k = 3, scale = "odds",           
#                           data = model_inputs_cohort$data_dfs$df_frmale_c) 
# 
# round(d2_f_c_1$res,3)
# d3_f_c_1 <-flexsurvspline(model_inputs_cohort$surv_objects$so_female_c ~            poly(Cohort.dec,3,raw=F),
#                           k = 3, scale = "odds",           
#                           data = model_inputs_cohort$data_dfs$df_frmale_c) 
# round(d3_f_c_1$res,3)
# 
# f_c_o_comparisons<-list(f_c_o_p1=  d1_f_c_1,
#                         f_c_o_p2=d2_f_c_1,
#                         f_c_o_p3= d3_f_c_1)
# save(f_c_o_comparisons, file="f_c_o_comparisons.RData")


load(file="final_comparison_all_models.RData")
load(file="f_c_o_comparisons.RData")

final_comparison_all_models$linear$d1_f_c_1     <- f_c_o_comparisons$f_c_o_p1
final_comparison_all_models$quadratic$d2_f_c_1  <- f_c_o_comparisons$f_c_o_p2
final_comparison_all_models$cubic$d3_f_c_1      <- f_c_o_comparisons$f_c_o_p3

#------------------------------------------------------- Comparing relative risk 

#load(file="final_comparison_all_hazmodels.RData")

# linear trends 

# extracting est for linear models
round(final_comparison_all_models$linear$d1_f_c_1$res,3)
round(final_comparison_all_models$linear$d1_m_c_1$res,3)
round(final_comparison_all_models$linear$d1_f_p_1$res,3)
round(final_comparison_all_models$linear$d1_m_p_1$res,3)

coeffs_f_c_d1<- 411.234 
coeffs_m_c_d1<- 128.668
coeffs_f_p_d1<- 143.344
coeffs_m_p_d1<- 91.469 

cohorts<- c(1930,1940,1950,1960,1970,1980,1990)
periods<- c(1998,2002,2008,2011,2016)

poly_trend <- function(x, poly_d, coeffs){
  trend <- list()
  x_num <- unique(as.numeric(x))
  m <- as.matrix(poly(x_num,poly_d))
  trend[[1]] <- (m%*%coeffs)%*%c(rep(1,poly_d))
  return(trend)
}

female_c_trend_d1<- as.data.frame(poly_trend(x=cohorts, poly_d  = 1, coeffs = coeffs_f_c_d1)  )#%>%mutate(Cohort=unique(plot_df_female_c$Cohort))
female_p_trend_d1<- as.data.frame(poly_trend(x=periods, poly_d  = 1, coeffs = coeffs_f_p_d1)  )#%>%mutate(Period=unique(plot_df_female_p$Period))
male_c_trend_d1  <- as.data.frame(poly_trend(x=cohorts, poly_d  = 1, coeffs = coeffs_m_c_d1)  )  #%>%mutate(Cohort=unique(plot_df_female_c$Cohort))
male_p_trend_d1  <- as.data.frame(poly_trend(x=periods, poly_d  = 1, coeffs = coeffs_m_p_d1) )   #%>%mutate(Period=unique(plot_df_female_p$Period))

names(female_c_trend_d1)<- "est"
names(female_p_trend_d1)<- "est"
names(male_c_trend_d1)<- "est"
names(male_p_trend_d1)<- "est"
female_c_trend_d1<-female_c_trend_d1 %>%mutate(Cohort=cohorts, Sex = "Female")
female_p_trend_d1<-female_p_trend_d1 %>%mutate(Period=periods, Sex ="Female")
male_c_trend_d1<-male_c_trend_d1 %>%mutate(Cohort=cohorts, Sex ="Male")
male_p_trend_d1<-male_p_trend_d1 %>%mutate(Period=periods, Sex ="Male")

cohort_trends_d1 <- rbind(female_c_trend_d1,male_c_trend_d1)

library(viridis)
linear_trend_cohort_comparison<- ggplot(cohort_trends_d1, aes(x=Cohort, y=est, color=Sex, shape=Sex))+
  labs(#title = "Change in the Log Cum Rate/Odds of Onset as a Function of Cohort, by Sex",
  subtitle = "Linear temporal trend"
  #subtitle = ""
  )+    
  xlab("Cohort") +
  ylab(bquote("Effect on Log-Cumulative Odds of Onset")) +
  theme_bw()+
  theme(legend.position = "right",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14))+
  scale_x_continuous(breaks = seq(1930,1990,10)) +
  #scale_y_continuous(breaks = number_ticks(14)) +
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = 1))+
  
  geom_line(size=1)+
  geom_point(size=4.5, alpha=1)



linear_trend_cohort_comparison


period_trends <- rbind(female_p_trend_d1,male_p_trend_d1)

linear_trend_period_comparison<-  ggplot(period_trends, aes(x=Period, y=est, color=Sex, shape=Sex))+
  labs(#title = "Change in the Log Cum Rate/Odds of Onset as a Function of Period, by Sex",
  subtitle = "Linear temporal trend"
  #  subtitle = ""
  )+    
  xlab("Period") +
  ylab(bquote("Effect on Log-Cumulative Odds of Onset")) +
  theme_bw()+
  theme(legend.position = "right",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14))+
  scale_x_discrete(limits = seq(1998,2016,1),  #c(1998,2002,2008,2011,2016)) +
                   labels = c("1998","","","","2002","","","","","","2008","","","2011","","","","","2016")) +
  #guides(colour = guide_legend(nrow = 1))+
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = 1))+
  
  geom_line(size=1)+
  geom_point(size=4.5, alpha=1)

linear_trend_period_comparison

# --- coeffs cohort linear, comparison by Sex -----
# Additional graph linear
coeffs2_f_c_d1<- data.frame(est =c(411.234  ),poly_term=  c("Cohort" ),  Sex =  "Female", trend ="Cohort")
coeffs2_m_c_d1<- data.frame(est =c(128.668 ), poly_term=  c("Cohort"), Sex = "Male", trend ="Cohort")
coeffs2_f_p_d1<- data.frame(est =c(143.344 ), poly_term=  c("Period")  ,Sex = "Female", trend ="Period")
coeffs2_m_p_d1<- data.frame(est =c(91.469 ),poly_term=  c("Period" ), Sex = "Male", trend ="Period")


cohort_comparison_d1<- rbind(coeffs2_f_c_d1,coeffs2_m_c_d1)
period_comparison_d1<- rbind(coeffs2_f_p_d1,coeffs2_m_p_d1)

linear_trend_coeffs_comparison_cohort <-ggplot(cohort_comparison_d1, aes(x= est, y= poly_term, color=Sex, shape = Sex))+
  labs(#title = "Comparison of Temporal Trend  Estimates by Sex",
    subtitle = "Linear temporal trend"
  )+    
  xlab("Coefficient Estimate") +
  #ylab(bquote("Temporal Trend Term")) +
  ylab(bquote("")) +
  
  geom_point(size=4, alpha=0.7)+
  geom_vline(xintercept = 0, lty= 3, size =1.5)+
  xlim(c(-450,450) )+
  theme_bw()+
  theme(legend.position = "right",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 16))+
  #scale_x_continuous(breaks = number_ticks(50)) +
  #scale_y_continuous(breaks = number_ticks(14)) +
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = 1))
linear_trend_coeffs_comparison_cohort

linear_trend_coeffs_comparison_period<- ggplot(period_comparison_d1, aes(x= est, y= poly_term, color=Sex, shape = Sex))+
  labs(#title = "Comparison of Temporal Trend Estimates by Sex",
    subtitle = "Linear temporal trend"
  )+    
  xlab("Coefficient Estimate") +
  #ylab(bquote("Temporal Trend Term")) +
  ylab(bquote("")) +
  
  geom_point(size=4, alpha=0.7)+
  geom_vline(xintercept = 0, lty= 3, size =1.5)+
  xlim(c(-450,450) )+
  theme_bw()+
  theme(legend.position = "right",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 16))+
  #scale_x_continuous(breaks = number_ticks(50)) +
  #scale_y_continuous(breaks = number_ticks(14)) +
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = 1))
linear_trend_coeffs_comparison_period

# ------------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------- Quadratic ------------------------------------------------------------- 
round(final_comparison_all_models$quadratic$d2_f_c_1$res,3)
round(final_comparison_all_models$quadratic$d2_m_c_1$res,3)
round(final_comparison_all_models$quadratic$d2_f_p_1$res,3)
round(final_comparison_all_models$quadratic$d2_m_p_1$res,3)


coeffs_f_c_d2<- diag(c(199.467 ,   70.541 ))
coeffs_m_c_d2<- diag(c(124.669 ,  -4.643  ))
coeffs_f_p_d2<- diag(c(145.918 ,   16.815 ))
coeffs_m_p_d2<- diag(c(103.757 ,  -14.560 ))

female_c_trend_d2<- as.data.frame(poly_trend(x=cohorts, poly_d  = 2, coeffs = coeffs_f_c_d2)  )#%>%mutate(Cohort=unique(plot_df_female_c$Cohort))
female_p_trend_d2<- as.data.frame(poly_trend(x=periods, poly_d  = 2, coeffs = coeffs_f_p_d2)  )#%>%mutate(Period=unique(plot_df_female_p$Period))
male_c_trend_d2<- as.data.frame(poly_trend(x=cohorts, poly_d  = 2, coeffs = coeffs_m_c_d2)  )  #%>%mutate(Cohort=unique(plot_df_female_c$Cohort))
male_p_trend_d2<- as.data.frame(poly_trend(x=periods, poly_d  = 2, coeffs = coeffs_m_p_d2) )   #%>%mutate(Period=unique(plot_df_female_p$Period))

names(female_c_trend_d2)<- "est"
names(female_p_trend_d2)<- "est"
names(male_c_trend_d2)<- "est"
names(male_p_trend_d2)<- "est"
female_c_trend_d2<-female_c_trend_d2 %>%mutate(Cohort=cohorts, Sex = "Female")
female_p_trend_d2<-female_p_trend_d2 %>%mutate(Period=periods, Sex ="Female")
male_c_trend_d2  <-male_c_trend_d2   %>%mutate(Cohort=cohorts, Sex ="Male")
male_p_trend_d2  <-male_p_trend_d2   %>%mutate(Period=periods, Sex ="Male")

cohort_trends_d2 <- rbind(female_c_trend_d2,male_c_trend_d2)

quadratic_trend_cohort_comparison<-ggplot(cohort_trends_d2, aes(x=Cohort, y=est, color=Sex, shape=Sex))+
  labs(#title = "Change in the Log Cum Rate/Odds of Onset as a Function of Cohort, by Sex",
  subtitle = "Quadratic temporal trend"
  #  subtitle = ""
  )+    
  xlab("Cohort") +
  ylab(bquote("Effect on Log-Cumulative Odds of Onset")) +
  theme_bw()+
  theme(legend.position = "right",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14))+
  scale_x_continuous(breaks = seq(1930,1990,10)) +
  #scale_y_continuous(breaks = number_ticks(14)) +
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = 1))+
  
  geom_line(size=1)+
  geom_point(size=4.5, alpha=1)
quadratic_trend_cohort_comparison

period_trends_d2 <- rbind(female_p_trend_d2,male_p_trend_d2)

quadratic_trend_period_comparison<-ggplot(period_trends_d2, aes(x=Period, y=est, color=Sex, shape=Sex))+
  labs(#title = "Change in the Log Cum Rate/Odds of Onset as a Function of Cohort, by Sex",
  subtitle = "Quadratic temporal trend"
  #subtitle = ""
  )+    
  xlab("Period") +
  ylab(bquote("Effect on Log-Cumulative Odds of Onset")) +
  theme_bw()+
  theme(legend.position = "right",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14))+
  scale_x_discrete(limits = seq(1998,2016,1),  #c(1998,2002,2008,2011,2016)) +
                   labels = c("1998","","","","2002","","","","","","2008","","","2011","","","","","2016")) +
  #guides(colour = guide_legend(nrow = 1))+
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = 1))+
  
  geom_line(size=1)+
  geom_point(size=4.5, alpha=1)
quadratic_trend_period_comparison


# --- coeffs cohort quadratic, comparison by Sex ----

# Additional graph quadratic
coeffs2_f_c_d2<- data.frame(est =c(199.467 ,   70.541 ),poly_term=  c("Cohort","Cohort^2"),  Sex =  "Female", trend ="Cohort")
coeffs2_m_c_d2<- data.frame(est =c(124.669 ,  -4.643  ), poly_term=  c("Cohort","Cohort^2"), Sex =  "Male", trend =  "Cohort")
coeffs2_f_p_d2<- data.frame(est =c(145.918 ,   16.815 ), poly_term=  c("Period","Period^2")  ,Sex = "Female", trend ="Period")
coeffs2_m_p_d2<- data.frame(est =c(103.757 ,  -14.560 ),poly_term=  c("Period","Period^2"), Sex =   "Male", trend =  "Period")

cohort_comparison_d2<- rbind(coeffs2_f_c_d2,coeffs2_m_c_d2)
period_comparison_d2<- rbind(coeffs2_f_p_d2,coeffs2_m_p_d2)

quadratic_trend_coeffs_comparison_cohort <- ggplot(cohort_comparison_d2, aes(x= est, y= poly_term, color=Sex, shape = Sex))+
  labs(#title = "Comparison of Temporal Trend  Estimates by Sex",
    subtitle = "Quadratic temporal trend"
  )+    
  xlab("Coefficient Estimate") +
  #ylab(bquote("Temporal Trend Term")) +
  ylab(bquote("")) +
  
  geom_point(size=4, alpha=0.7)+
  geom_vline(xintercept = 0, lty= 3, size =1.5)+
  xlim(c(-450,450) )+
  theme_bw()+
  theme(legend.position = "right",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 16))+
  #scale_x_continuous(breaks = number_ticks(50)) +
  #scale_y_continuous(breaks = number_ticks(14)) +
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = 1))
quadratic_trend_coeffs_comparison_cohort

quadratic_trend_coeffs_comparison_period <- ggplot(period_comparison_d2, aes(x= est, y= poly_term, color=Sex, shape = Sex))+
  labs(#title = "Comparison of Temporal Trend Estimates by Sex",
       subtitle = "Quadratic temporal trend"
  )+    
  xlab("Coefficient Estimate") +
  #ylab(bquote("Temporal Trend Term")) +
  ylab(bquote("")) +
  
  geom_point(size=4, alpha=0.7)+
  geom_vline(xintercept = 0, lty= 3, size =1.5)+
  xlim(c(-450,450) )+
  theme_bw()+
  theme(legend.position = "right",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 16))+
  #scale_x_continuous(breaks = number_ticks(50)) +
  #scale_y_continuous(breaks = number_ticks(14)) +
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = 1))
quadratic_trend_coeffs_comparison_period

# ---------------------------------------------------------------------------------------------------------- #
# --------------------------------------------- -Cubic ------------------------------------------------------------- 
round(final_comparison_all_models$cubic$d3_f_c_1$res,3)
round(final_comparison_all_models$cubic$d3_m_c_1$res,3)
round(final_comparison_all_models$cubic$d3_f_p_1$res,3)
round(final_comparison_all_models$cubic$d3_m_p_1$res,3)

coeffs_f_c_d3<- diag(c(328.031,93.023,34.183  ))
coeffs_m_c_d3<- diag(c(123.420 ,11.235,19.113 ))
coeffs_f_p_d3<- diag(c(131.602,7.178, 34.095  ))
coeffs_m_p_d3<- diag(c(72.471, 3.920, 27.916  ))

female_c_trend_d3<- as.data.frame(poly_trend(x=cohorts, poly_d  = 3, coeffs = coeffs_f_c_d3)  )#%>%mutate(Cohort=unique(plot_df_female_c$Cohort))
female_p_trend_d3<- as.data.frame(poly_trend(x=periods, poly_d  = 3, coeffs = coeffs_f_p_d3)  )#%>%mutate(Period=unique(plot_df_female_p$Period))
male_c_trend_d3  <- as.data.frame(poly_trend(x=cohorts, poly_d  = 3, coeffs = coeffs_m_c_d3)  )  #%>%mutate(Cohort=unique(plot_df_female_c$Cohort))
male_p_trend_d3  <- as.data.frame(poly_trend(x=periods, poly_d  = 3, coeffs = coeffs_m_p_d3) )   #%>%mutate(Period=unique(plot_df_female_p$Period))

names(female_c_trend_d3)<- "est"
names(female_p_trend_d3)<- "est"
names(male_c_trend_d3)<- "est"
names(male_p_trend_d3)<- "est"
female_c_trend_d3<-female_c_trend_d3 %>%mutate(Cohort=cohorts, Sex ="Female")
female_p_trend_d3<-female_p_trend_d3 %>%mutate(Period=periods, Sex ="Female")
male_c_trend_d3  <-male_c_trend_d3   %>%mutate(Cohort=cohorts, Sex ="Male")
male_p_trend_d3  <-male_p_trend_d3   %>%mutate(Period=periods, Sex ="Male")

cohort_trends_d3 <- rbind(female_c_trend_d3,male_c_trend_d3)

cubic_trend_cohort_comparison <- ggplot(cohort_trends_d3, aes(x=Cohort, y=est, color=Sex, shape=Sex))+
  labs(#title = "Trajectories of the baseline log-cumulative rates of onset/odds of onset across Cohorts , by Sex",
  subtitle = "Cubic Temporal Trend"
  #subtitle = ""
  )+    
  xlab("Cohort") +
  ylab(bquote("Effect on Log-Cumulative Odds of Onset")) +
  theme_bw()+
  theme(legend.position = "right",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14))+
  scale_x_continuous(breaks = seq(1930,1990,10)) +
  #scale_y_continuous(breaks = number_ticks(14)) +
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = 1))+
  
  geom_line(size=1)+
  geom_point(size=4.5, alpha=1)
cubic_trend_cohort_comparison

period_trends_d3 <- rbind(female_p_trend_d3,male_p_trend_d3)

cubic_trend_period_comparison<- ggplot(period_trends_d3, aes(x=Period, y=est, color=Sex, shape=Sex))+
  labs(#title = "Change in the Log Cum Rate/Odds of Onset as a Function of Period, by Sex",
  subtitle = "Cubic temporal trend"
  #  subtitle = ""
  )+    
  xlab("Period") +
  ylab(bquote("Effect on Log-Cumulative Odds of Onset")) +
  theme_bw()+
  theme(legend.position = "right",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14))+
  scale_x_discrete(limits = seq(1998,2016,1),  #c(1998,2002,2008,2011,2016)) +
                   labels = c("1998","","","","2002","","","","","","2008","","","2011","","","","","2016")) +
  #guides(colour = guide_legend(nrow = 1))+
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = 1))+
  
  geom_line(size=1)+
  geom_point(size=4.5, alpha=1)
cubic_trend_period_comparison

# --- coeffs cohort quadratic, comparison by Sex ----

# Additional graph cubic
coeffs2_f_c_d3<- data.frame(est =c(328.031,93.023,34.183 ),poly_term=    c("Cohort","Cohort^2", "Cohort^3"), Sex = "Female", trend ="Cohort")
coeffs2_m_c_d3<- data.frame(est =c(123.420 ,11.235,19.113 ), poly_term=  c("Cohort","Cohort^2", "Cohort^3"), Sex = "Male",   trend ="Cohort")
coeffs2_f_p_d3<- data.frame(est =c(131.602,7.178, 34.095 ), poly_term=   c("Period","Period^2","Period^3"),  Sex = "Female", trend ="Period")
coeffs2_m_p_d3<- data.frame(est =c(72.471, 3.920, 27.916 ),poly_term=    c("Period","Period^2","Period^3"),  Sex = "Male",   trend ="Period")


cohort_comparison_d3<- rbind(coeffs2_f_c_d3,coeffs2_m_c_d3)
period_comparison_d3<- rbind(coeffs2_f_p_d3,coeffs2_m_p_d3)

cubic_trend_coeffs_comparison_cohort <- ggplot(cohort_comparison_d3, aes(x= est, y= poly_term, color=Sex, shape = Sex))+
  labs(#title = "Comparison of Temporal Trend  Estimates by Sex",
    subtitle = "Cubic temporal trend"
  )+    
  xlab("Coefficient Estimate") +
  #ylab(bquote("Temporal Trend Term")) +
  ylab(bquote("")) +
  
  geom_point(size=4, alpha=0.7)+
  geom_vline(xintercept = 0, lty= 3, size =1.5)+
  xlim(c(-450,450) )+
  theme_bw()+
  theme(legend.position = "right",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 16))+
  #scale_x_continuous(breaks = number_ticks(50)) +
  #scale_y_continuous(breaks = number_ticks(14)) +
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = 1))
cubic_trend_coeffs_comparison_cohort

cubic_trend_coeffs_comparison_period<- ggplot(period_comparison_d3, aes(x= est, y= poly_term, color=Sex, shape = Sex))+
  labs(#title = "Comparison of Temporal Trend Estimates by Sex",
    subtitle = "Cubic temporal trend"
  )+    
  xlab("Coefficient Estimate") +
  #ylab(bquote("Temporal Trend Term")) +
  ylab(bquote("")) +
  
  geom_point(size=4, alpha=0.7)+
  geom_vline(xintercept = 0, lty= 3, size =1.5)+
  xlim(c(-450,450) )+
  theme_bw()+
  theme(legend.position = "right",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 16))+
  #scale_x_continuous(breaks = number_ticks(50)) +
  #scale_y_continuous(breaks = number_ticks(14)) +
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = 1))
cubic_trend_coeffs_comparison_period
# --- Appendix Figure 1, Coeffs ----

period_all_coeffs<-  (linear_trend_coeffs_comparison_period / quadratic_trend_coeffs_comparison_period / cubic_trend_coeffs_comparison_period) +plot_layout(guides = 'collect')
period_all_coeffs
cohort_all_coeffs<-  (linear_trend_coeffs_comparison_cohort / quadratic_trend_coeffs_comparison_cohort / cubic_trend_coeffs_comparison_cohort) +plot_layout(guides = 'collect')
cohort_all_coeffs

all_coeffs <- (period_all_coeffs | cohort_all_coeffs)  #+ plot_layout(guides = 'collect') 
  # plot_annotation(tag_levels = list(c("(A) by Period ","(B)  by Cohort"))) & 
  # theme(plot.tag = element_text(size = 20, hjust = 0, vjust = 0, face = "bold"),
  #      plot.tag.position = c(0,.99)) 

all_coeffs

Figure_all_coefs <- all_coeffs

ggsave(
  "Figure_all_coefs.png",
  plot = Figure_all_coefs,
  #device = "png",
  #path = "~/HR_plots",
  scale = 1,
  width = 15,
  height = 10,
  units = "in",
  dpi = 300,
  limitsize = F
)

ggsave(
  "Figure_all_coefs.pdf",
  plot = Figure_all_coefs,
  #device = "png",
  #path = "~/HR_plots",
  scale = 1,
  width = 15,
  height = 10,
  units = "in",
  dpi = 300,
  limitsize = F
)

ggsave(
  "Figure_all_coefs.tiff",
  plot = Figure_all_coefs,
  #device = "png",
  #path = "~/HR_plots",
  scale = 1,
  width = 15,
  height = 10,
  units = "in",
  dpi = 300,
  limitsize = F
)
unlink(Figure_4)
dev.off()
# --- Appendix Figure 2, Trends ----

period_all_trends<-  (linear_trend_period_comparison / quadratic_trend_period_comparison / cubic_trend_period_comparison) + plot_layout(guides = 'collect')
period_all_trends
cohort_all_trends<-  (linear_trend_cohort_comparison / quadratic_trend_cohort_comparison / cubic_trend_cohort_comparison) + plot_layout(guides = 'collect')
cohort_all_trends

all_trends <- (period_all_trends | cohort_all_trends)  + plot_layout(guides = 'collect') #+
  #plot_layout(tag_level = 'new') +
  #plot_annotation(tag_levels = list(c('(A)  by Period','','','(B)  by Cohort'))) & 
  #theme(plot.tag = element_text(size = 20, hjust = 1, vjust = 0, face = "bold"),
  #      plot.tag.position = c(0,1)) 
all_trends

Figure_all_trends <- all_trends

ggsave(
  "Figure_all_trends.png",
  plot = Figure_all_trends,
  #device = "png",
  #path = "~/HR_plots",
  scale = 1,
  width = 15,
  height = 13,
  units = "in",
  dpi = 300,
  limitsize = F
)

ggsave(
  "Figure_all_trends.pdf",
  plot = Figure_all_trends,
  #device = "png",
  #path = "~/HR_plots",
  scale = 1,
  width = 15,
  height = 12,
  units = "in",
  dpi = 300,
  limitsize = F
)

ggsave(
  "Figure_all_trends.tiff",
  plot = Figure_all_trends,
  #device = "png",
  #path = "~/HR_plots",
  scale = 1,
  width = 15,
  height = 12,
  units = "in",
  dpi = 300,
  limitsize = F
)
unlink(Figure_all_trends)
dev.off()






#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# ---  Best fit model curves of the change in the log cum odds ------- 


best_fit_trends_cohort <- cohort_trends_d3
best_fit_trends_cohort$Trend <- "Cubic"
best_fit_trends_cohort$Sex <- c(rep("Female",7),rep("Male",7))
plot_cohort_trend <- ggplot(best_fit_trends_cohort, aes(x=Cohort, y=est, color=Sex, shape=Sex, linetype = Trend))+
  labs(#title = "Change in the Log Cum Rate/Odds of Onset as a function of Cohort, by Sex",
    subtitle = ""
  )+    
  xlab("Cohort") +
  ylab(bquote("Effect on Log-Cumulative Odds of Onset")) +
  theme_bw()+
  theme(legend.position = "",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 16))+
  scale_x_continuous(breaks = seq(1930,1990,10)) +
  #scale_y_continuous(breaks = number_ticks(14)) +
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = 1))+
  
  geom_line(size=1)+
  geom_point(size=4.5, alpha=1)
plot_cohort_trend

female_p_trend_d1$Trend <- "Linear"
female_p_trend_d1$Sex <- "Female"
male_p_trend_d3$Trend <- "Cubic"
male_p_trend_d3$Sex <- "Male"
best_fit_trends_period<- rbind(female_p_trend_d1,male_p_trend_d3)

plot_period_trend <-ggplot(best_fit_trends_period, aes(x=(as.integer(Period)), y=est, color=Sex, shape=Sex, linetype = Trend))+
  labs(#title = "Change in the Log Cum Rate/Odds of Onset as a function of Period, by Sex",
    subtitle = ""
  )+    
  xlab("Period") +
  ylab(bquote("Effect on Log-Cumulative Odds of Onset")) +
  theme_bw()+
  theme(legend.position = "right",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 16))+
  scale_x_discrete(limits = seq(1998,2016,1),  #c(1998,2002,2008,2011,2016)) +
                     labels = c("1998","","","","2002","","","","","","2008","","","2011","","","","","2016")) +
  #guides(colour = guide_legend(nrow = 1))+
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = 1))+
  
  geom_line(size=1)+
  geom_point(size=4.5, alpha=1)
plot_period_trend

# --- Final Figure 4 -----

patch_3 <- plot_period_trend / plot_cohort_trend
Figure_4 <- patch_3 + plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = list(c("(A) Linear Trend (Females) vs Cubic Trend (Males), by Period ","(B) Cubic Trend (Females) vs Cubic Trend (Males), by Cohort"))) & 
  theme(plot.tag = element_text(size = 22, hjust = 0, vjust = 0, face = "bold"),
        plot.tag.position = c(0,.98)) 
Figure_4

ggsave(
  "Figure_4.png",
  plot = Figure_4,
  #device = "png",
  #path = "~/HR_plots",
  scale = 1,
  width = 12,
  height = 11,
  units = "in",
  dpi = 300,
  limitsize = F
)

ggsave(
  "Figure_4.pdf",
  plot = Figure_4,
  #device = "png",
  #path = "~/HR_plots",
  scale = 1,
  width = 12,
  height = 11,
  units = "in",
  dpi = 300,
  limitsize = F
)

ggsave(
  "Figure_4.tiff",
  plot = Figure_4,
  #device = "png",
  #path = "~/HR_plots",
  scale = 1,
  width = 12,
  height = 11,
  units = "in",
  dpi = 300,
  limitsize = F
)
unlink(Figure_4)
dev.off()


