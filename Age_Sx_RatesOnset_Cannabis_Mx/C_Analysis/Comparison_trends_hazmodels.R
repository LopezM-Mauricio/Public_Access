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

library(reshape2)
library(readxl)
library(openxlsx)
library(dampack)
library(viridis)


# ---------------------------------------------------------------------------------------------------------- #
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


#d_poly =2
#model_inputs_cohort$data_dfs$df_frmale_c$Cohort
d1_f_c_1 <-flexsurvspline(model_inputs_cohort$surv_objects$so_female_c ~            poly(Cohort.dec,1,raw=F),
                              k = 3, scale = "hazard",           
                              data = model_inputs_cohort$data_dfs$df_frmale_c) 
round(d1_f_c_1$res,3)

#model_inputs_cohort$data_dfs$dF_male_c$Cohort
d1_m_c_1<- flexsurvspline(model_inputs_cohort$surv_objects$so_male_c~               poly(Cohort.dec,1,raw=F),
                              k = 7, scale = "hazard",
                              data = model_inputs_cohort$data_dfs$dF_male_c)

#model_inputs_period$data_dfs$df_frmale_p$Year
d1_f_p_1 <- flexsurvspline(model_inputs_period$surv_objects$so_female_p ~           poly(Year,1,raw=F),
                               k = 3, scale = "hazard",           
                               data = model_inputs_period$data_dfs$df_frmale_p)

#model_inputs_period$data_dfs$dF_male_p$Year
d1_m_p_1 <- flexsurvspline(model_inputs_period$surv_objects$so_male_p ~             poly(Year,1,raw=F),
                               k=6, scale="hazard", 
                               data= model_inputs_period$data_dfs$dF_male_p)


#----- 

#model_inputs_cohort$data_dfs$df_frmale_c$Cohort
d2_f_c_1 <-flexsurvspline(model_inputs_cohort$surv_objects$so_female_c ~            poly(Cohort.dec,2,raw=F),
                            k = 3, scale = "hazard",           
                            data = model_inputs_cohort$data_dfs$df_frmale_c) 

round(d2_f_c_1$res,3)

#model_inputs_cohort$data_dfs$dF_male_c$Cohort
d2_m_c_1<- flexsurvspline(model_inputs_cohort$surv_objects$so_male_c~               poly(Cohort.dec,2,raw=F),
                            k = 7, scale = "hazard",
                            data = model_inputs_cohort$data_dfs$dF_male_c)

#model_inputs_period$data_dfs$df_frmale_p$Year
d2_f_p_1 <- flexsurvspline(model_inputs_period$surv_objects$so_female_p ~           poly(Year,2,raw=F),
                             k = 3, scale = "hazard",           
                             data = model_inputs_period$data_dfs$df_frmale_p)

#model_inputs_period$data_dfs$dF_male_p$Year
d2_m_p_1 <- flexsurvspline(model_inputs_period$surv_objects$so_male_p ~             poly(Year,2,raw=F),
                             k=6, scale="hazard", 
                             data= model_inputs_period$data_dfs$dF_male_p)


#----- 

#model_inputs_cohort$data_dfs$df_frmale_c$Cohort
d3_f_c_1 <-flexsurvspline(model_inputs_cohort$surv_objects$so_female_c ~            poly(Cohort.dec,3,raw=F),
                              k = 3, scale = "hazard",           
                              data = model_inputs_cohort$data_dfs$df_frmale_c) 
round(d3_f_c_1$res,3)

#model_inputs_cohort$data_dfs$dF_male_c$Cohort
d3_m_c_1<- flexsurvspline(model_inputs_cohort$surv_objects$so_male_c~               poly(Cohort.dec,3,raw=F),
                              k = 7, scale = "hazard",
                              data = model_inputs_cohort$data_dfs$dF_male_c)

#model_inputs_period$data_dfs$df_frmale_p$Year
d3_f_p_1 <- flexsurvspline(model_inputs_period$surv_objects$so_female_p ~           poly(Year,3,raw=F),
                               k = 3, scale = "hazard",           
                               data = model_inputs_period$data_dfs$df_frmale_p)

#model_inputs_period$data_dfs$dF_male_p$Year
d3_m_p_1 <- flexsurvspline(model_inputs_period$surv_objects$so_male_p ~             poly(Year,3,raw=F),
                               k=6, scale="hazard", 
                               data= model_inputs_period$data_dfs$dF_male_p)


final_comparison_all_hazmodels <- list( linear = list(d1_f_c_1=d1_f_c_1,
                                                   d1_m_c_1=d1_m_c_1,
                                                   d1_f_p_1=d1_f_p_1,
                                                   d1_m_p_1=d1_m_p_1), 
                                     quadratic = list(d2_f_c_1=d2_f_c_1,
                                                      d2_m_c_1=d2_m_c_1,
                                                      d2_f_p_1=d2_f_p_1,
                                                      d2_m_p_1=d2_m_p_1), 
                                     cubic = list(d3_f_c_1=d3_f_c_1,
                                                  d3_m_c_1=d3_m_c_1,
                                                  d3_f_p_1=d3_f_p_1,
                                                  d3_m_p_1=d3_m_p_1))
save(final_comparison_all_hazmodels, file="final_comparison_all_hazmodels.RData")
load(file="final_comparison_all_hazmodels.RData")
 # f_c_o_comparisons<-list(f_c_o_p1=  d1_f_c_1,
 # f_c_o_p2=d2_f_c_1,
 # f_c_o_p3= d3_f_c_1)
 # save(f_c_o_comparisons, file="f_c_o_comparisons.RData")

# extracting est for linear models
round(final_comparison_all_hazmodels$linear$d1_f_c_1$res,3)
round(final_comparison_all_hazmodels$linear$d1_m_c_1$res,3)
round(final_comparison_all_hazmodels$linear$d1_f_p_1$res,3)
round(final_comparison_all_hazmodels$linear$d1_m_p_1$res,3)

#extracting est for quadr models
round(final_comparison_all_hazmodels$quadratic$d2_f_c_1$res,3)
round(final_comparison_all_hazmodels$quadratic$d2_m_c_1$res,3)
round(final_comparison_all_hazmodels$quadratic$d2_f_p_1$res,3)
round(final_comparison_all_hazmodels$quadratic$d2_m_p_1$res,3)

# extracting est for cubic models
round(final_comparison_all_hazmodels$cubic$d3_f_c_1$res,3)
round(final_comparison_all_hazmodels$cubic$d3_m_c_1$res,3)
round(final_comparison_all_hazmodels$cubic$d3_f_p_1$res,3)
round(final_comparison_all_hazmodels$cubic$d3_m_p_1$res,3)

#-------
# AIC
# extracting est for linear models
round(final_comparison_all_hazmodels$linear$d1_f_c_1$AIC,3)
round(final_comparison_all_hazmodels$linear$d1_m_c_1$AIC,3)
round(final_comparison_all_hazmodels$linear$d1_f_p_1$AIC,3)
round(final_comparison_all_hazmodels$linear$d1_m_p_1$AIC,3)

#extracting est for quadr models
round(final_comparison_all_hazmodels$quadratic$d2_f_c_1$AIC,3)
round(final_comparison_all_hazmodels$quadratic$d2_m_c_1$AIC,3)
round(final_comparison_all_hazmodels$quadratic$d2_f_p_1$AIC,3)
round(final_comparison_all_hazmodels$quadratic$d2_m_p_1$AIC,3)

# extracting est for cubic models
round(final_comparison_all_hazmodels$cubic$d3_f_c_1$AIC,3)
round(final_comparison_all_hazmodels$cubic$d3_m_c_1$AIC,3)
round(final_comparison_all_hazmodels$cubic$d3_f_p_1$AIC,3)
round(final_comparison_all_hazmodels$cubic$d3_m_p_1$AIC,3)


# Best fit models 
# cohort
final_comparison_all_hazmodels$cubic$d3_f_c_1
final_comparison_all_hazmodels$cubic$d3_m_c_1
# period
final_comparison_all_hazmodels$linear$d1_f_p_1
final_comparison_all_hazmodels$cubic$d3_m_p_1


final_hazmodels<- list(female_cohort=final_comparison_all_hazmodels$cubic$d3_f_c_1,
                       male_cohort=final_comparison_all_hazmodels$cubic$d3_m_c_1,
                       female_period = final_comparison_all_hazmodels$linear$d1_f_p_1,
                       male_period = final_comparison_all_hazmodels$cubic$d3_m_p_1)
save(final_hazmodels, file="final_hazmodels.RData")
#-------------------------------------------
#
#------------------------------------------------------- Comparing relative risk 

#load(file="final_comparison_all_hazmodels.RData")

# linear trends 

# extracting est for linear models
round(final_comparison_all_hazmodels$linear$d1_f_c_1$res,3)
round(final_comparison_all_hazmodels$linear$d1_m_c_1$res,3)
round(final_comparison_all_hazmodels$linear$d1_f_p_1$res,3)
round(final_comparison_all_hazmodels$linear$d1_m_p_1$res,3)


coeffs_f_c_d1<- 411.234 
coeffs_m_c_d1<- 120.754 
coeffs_f_p_d1<- 98.024
coeffs_m_p_d1<- 70.220 

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
female_c_trend_d1<-female_c_trend_d1 %>%mutate(Cohort=cohorts, Sex = "female")
female_p_trend_d1<-female_p_trend_d1 %>%mutate(Period=periods, Sex ="female")
male_c_trend_d1<-male_c_trend_d1 %>%mutate(Cohort=cohorts, Sex ="male")
male_p_trend_d1<-male_p_trend_d1 %>%mutate(Period=periods, Sex ="male")

cohort_trends_d1 <- rbind(female_c_trend_d1,male_c_trend_d1)

library(viridis)
linear_trend_cohort_comparison<- ggplot(cohort_trends_d1, aes(x=Cohort, y=est, color=Sex, shape=Sex))+
  labs(#title = "Change in the Log Cum Rate/Odds of Onset as a Function of Cohort, by Sex",
       subtitle = "Linear temporal trend, Cohort"
  )+    
  xlab("Cohort") +
  ylab(bquote("Additive Effect on the Log-Cumulative Rates of Onset")) +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 18))+
  scale_x_continuous(breaks = number_ticks(10)) +
  #scale_y_continuous(breaks = number_ticks(14)) +
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = 1))+
  
  geom_line()+
  geom_point(size=1.5, alpha=.9,color=1)



linear_trend_cohort_comparison


period_trends <- rbind(female_p_trend_d1,male_p_trend_d1)

linear_trend_period_comparison<-  ggplot(period_trends, aes(x=Period, y=est, color=Sex, shape=Sex))+
  labs(#title = "Change in the Log Cum Rate/Odds of Onset as a Function of Period, by Sex",
       subtitle = "Linear temporal trend, Period"
  )+    
  xlab("Period") +
  ylab(bquote("Additive Effect on the Log-Cumulative Rates of Onset")) +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 18))+
  scale_x_continuous(breaks = number_ticks(14)) +
  #scale_y_continuous(breaks = number_ticks(14)) +
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = 1))+
  
  geom_line()+
  geom_point(size=1.5, alpha=.9,color=1)

linear_trend_period_comparison


# Additional graph linear
coeffs2_f_c_d1<- data.frame(est =c(411.234 ),poly_term=  c("cohort" ),  sex =  "female", trend ="cohort")
coeffs2_m_c_d1<- data.frame(est =c(120.754 ), poly_term=  c("cohort"), sex = "male", trend ="cohort")
coeffs2_f_p_d1<- data.frame(est =c(98.024), poly_term=  c("period")  ,sex = "female", trend ="period")
coeffs2_m_p_d1<- data.frame(est =c(70.220 ),poly_term=  c("period" ), sex = "male", trend ="period")


cohort_comparison_d1<- rbind(coeffs2_f_c_d1,coeffs2_m_c_d1)
period_comparison_d1<- rbind(coeffs2_f_p_d1,coeffs2_m_p_d1)

ggplot(cohort_comparison_d1, aes(x= est, y= poly_term, color=sex))+
  labs(#title = "Comparison of Temporal Trend  Estimates by Sex",
       subtitle = "Linear temporal trend, Cohort"
  )+    
  xlab("Coefficient Estimate") +
  ylab(bquote("Temporal Trend Term")) +
  
  geom_point(size=4, alpha=0.7)+
  geom_vline(xintercept = 0, lty= 3, size =1.5)+
  xlim(c(-450,450))+
  theme_bw()+
  theme(legend.position = "right",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 18))+
  #scale_x_continuous(breaks = number_ticks(10)) +
  #scale_y_continuous(breaks = number_ticks(14)) +
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = 1))


ggplot(period_comparison_d1, aes(x= est, y= poly_term, color=sex))+
  labs(#title = "Comparison of Temporal Trend Estimates by Sex",
       subtitle = "Linear temporal trend, Period"
  )+    
  xlab("Coefficient Estimate") +
  ylab(bquote("Temporal Trend Term")) +
  geom_point(size=4, alpha=0.7)+
  geom_vline(xintercept = 0, lty= 3, size =1.5)+
  xlim(c(-150,150))+
  theme_bw()+
  theme(legend.position = "right",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 18))+
  #scale_x_continuous(breaks = number_ticks(10)) +
  #scale_y_continuous(breaks = number_ticks(14)) +
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = 1))


# ------------------------------------------------------------------------------------------------------------------- #
# ---------------------------------------------Quadratic------------------------------------------------------------- #
round(final_comparison_all_hazmodels$quadratic$d2_f_c_1$res,3)
round(final_comparison_all_hazmodels$quadratic$d2_m_c_1$res,3)
round(final_comparison_all_hazmodels$quadratic$d2_f_p_1$res,3)
round(final_comparison_all_hazmodels$quadratic$d2_m_p_1$res,3)


coeffs_f_c_d2<- diag(c(199.467,  70.541))
coeffs_m_c_d2<- diag(c(121.299, -1.066 ))
coeffs_f_p_d2<- diag(c(145.494,   4.643))
coeffs_m_p_d2<- diag(c(69.706 ,   2.253))

female_c_trend_d2<- as.data.frame(poly_trend(x=cohorts, poly_d  = 2, coeffs = coeffs_f_c_d2)  )#%>%mutate(Cohort=unique(plot_df_female_c$Cohort))
female_p_trend_d2<- as.data.frame(poly_trend(x=periods, poly_d  = 2, coeffs = coeffs_f_p_d2)  )#%>%mutate(Period=unique(plot_df_female_p$Period))
male_c_trend_d2<- as.data.frame(poly_trend(x=cohorts, poly_d  = 2, coeffs = coeffs_m_c_d2)  )  #%>%mutate(Cohort=unique(plot_df_female_c$Cohort))
male_p_trend_d2<- as.data.frame(poly_trend(x=periods, poly_d  = 2, coeffs = coeffs_m_p_d2) )   #%>%mutate(Period=unique(plot_df_female_p$Period))

names(female_c_trend_d2)<- "est"
names(female_p_trend_d2)<- "est"
names(male_c_trend_d2)<- "est"
names(male_p_trend_d2)<- "est"
female_c_trend_d2<-female_c_trend_d2 %>%mutate(Cohort=cohorts, Sex = "female")
female_p_trend_d2<-female_p_trend_d2 %>%mutate(Period=periods, Sex ="female")
male_c_trend_d2  <-male_c_trend_d2   %>%mutate(Cohort=cohorts, Sex ="male")
male_p_trend_d2  <-male_p_trend_d2   %>%mutate(Period=periods, Sex ="male")

cohort_trends_d2 <- rbind(female_c_trend_d2,male_c_trend_d2)

quadratic_trend_cohort_comparison<-ggplot(cohort_trends_d2, aes(x=Cohort, y=est, color=Sex, shape=Sex))+
  labs(#title = "Change in the Log Cum Rate/Odds of Onset as a Function of Cohort, by Sex",
       subtitle = "Quadratic temporal trend, Cohort"
  )+    
  xlab("Cohort") +
  ylab(bquote("Additive Effect on the Log-Cumulative Rates of Onset")) +
  theme_bw()+
  scale_x_continuous(breaks = number_ticks(10)) +
  theme(legend.position = "right",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 18))+
  #scale_x_continuous(breaks = number_ticks(10)) +
  #scale_y_continuous(breaks = number_ticks(14)) +
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = 1))+
  geom_line()+
  geom_point(size=1.5, alpha=.9,color=1)
quadratic_trend_cohort_comparison

period_trends_d2 <- rbind(female_p_trend_d2,male_p_trend_d2)

quadratic_trend_period_comparison<-ggplot(period_trends_d2, aes(x=Period, y=est, color=Sex, shape=Sex))+
  labs(#title = "Change in the Log Cum Rate/Odds of Onset as a Function of Cohort, by Sex",
       subtitle = "Quadratic temporal trend, Period"
  )+    
  xlab("Period") +
  ylab(bquote("Additive Effect on the Log-Cumulative Rates of Onset")) +
  theme_bw()+
  scale_x_continuous(breaks = number_ticks(14)) +
  #scale_y_continuous(breaks = number_ticks(14)) +
  theme(legend.position = "right",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 18))+
  #scale_x_continuous(breaks = number_ticks(10)) +
  #scale_y_continuous(breaks = number_ticks(14)) +
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = 1))+
  geom_line()+
  geom_point(size=1.5, alpha=.9,color=1)
quadratic_trend_period_comparison

# Additional graph quadratic
coeffs2_f_c_d2<- data.frame(est =c(199.467,  70.541),poly_term=  c("cohort","cohort^2"),  sex =  "female", trend ="cohort")
coeffs2_m_c_d2<- data.frame(est =c(121.299, -1.066 ), poly_term=  c("cohort","cohort^2"), sex = "male", trend ="cohort")
coeffs2_f_p_d2<- data.frame(est =c(145.494,   4.643), poly_term=  c("period","period^2")  ,sex = "female", trend ="period")
coeffs2_m_p_d2<- data.frame(est =c(69.706 ,   2.253),poly_term=  c("period","period^2"), sex = "male", trend ="period")

cohort_comparison_d2<- rbind(coeffs2_f_c_d2,coeffs2_m_c_d2)
period_comparison_d2<- rbind(coeffs2_f_p_d2,coeffs2_m_p_d2)

ggplot(cohort_comparison_d2, aes(x= est, y= poly_term, color=sex))+
  labs(#title = "Comparison of Temporal Trend  Estimates by Sex",
       subtitle = "Quadratic temporal trend, Cohort"
  )+    
  xlab("Coefficient Estimate") +
  ylab(bquote("Temporal Trend Term")) +
  
  geom_point(size=4, alpha=0.7)+
  geom_vline(xintercept = 0, lty= 3, size =1.5)+
  xlim(c(-350,350))+
  theme_bw()+
  theme(legend.position = "right",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 18))+
  #scale_x_continuous(breaks = number_ticks(10)) +
  #scale_y_continuous(breaks = number_ticks(14)) +
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = 1))


ggplot(period_comparison_d2, aes(x= est, y= poly_term, color=sex))+
  labs(title = "Comparison of Temporal Trend Estimates by Sex",
       subtitle = "Quadratic temporal trend, Period"
  )+    
  xlab("Coefficient Estimate") +
  ylab(bquote("Temporal Trend Term")) +
  
  geom_point(size=4, alpha=0.7)+
  geom_vline(xintercept = 0, lty= 3, size =1.5)+
  xlim(c(-150,150))+
  theme_bw()+
  theme(legend.position = "right",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 18))+
  #scale_x_continuous(breaks = number_ticks(10)) +
  #scale_y_continuous(breaks = number_ticks(14)) +
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = 1))

  
# ---------------------------------------------------------------------------------------------------------- #
# ---------------------------------------------Cubic------------------------------------------------------------- #
round(final_comparison_all_hazmodels$cubic$d3_f_c_1$res,3)
round(final_comparison_all_hazmodels$cubic$d3_m_c_1$res,3)
round(final_comparison_all_hazmodels$cubic$d3_f_p_1$res,3)
round(final_comparison_all_hazmodels$cubic$d3_m_p_1$res,3)

coeffs_f_c_d3<- diag(c(328.031,93.023,34.183 ))
coeffs_m_c_d3<- diag(c(90.706 ,15.569,31.580  ))
coeffs_f_p_d3<- diag(c(126.364,-3.440, 44.823 ))
coeffs_m_p_d3<- diag(c(69.313,-0.090, 31.575  ))

female_c_trend_d3<- as.data.frame(poly_trend(x=cohorts, poly_d  = 3, coeffs = coeffs_f_c_d3)  )#%>%mutate(Cohort=unique(plot_df_female_c$Cohort))
female_p_trend_d3<- as.data.frame(poly_trend(x=periods, poly_d  = 3, coeffs = coeffs_f_p_d3)  )#%>%mutate(Period=unique(plot_df_female_p$Period))
male_c_trend_d3  <- as.data.frame(poly_trend(x=cohorts, poly_d  = 3, coeffs = coeffs_m_c_d3)  )  #%>%mutate(Cohort=unique(plot_df_female_c$Cohort))
male_p_trend_d3  <- as.data.frame(poly_trend(x=periods, poly_d  = 3, coeffs = coeffs_m_p_d3) )   #%>%mutate(Period=unique(plot_df_female_p$Period))

names(female_c_trend_d3)<- "est"
names(female_p_trend_d3)<- "est"
names(male_c_trend_d3)<- "est"
names(male_p_trend_d3)<- "est"
female_c_trend_d3<-female_c_trend_d3 %>%mutate(Cohort=cohorts, Sex = "female")
female_p_trend_d3<-female_p_trend_d3 %>%mutate(Period=periods, Sex ="female")
male_c_trend_d3  <-male_c_trend_d3   %>%mutate(Cohort=cohorts, Sex ="male")
male_p_trend_d3  <-male_p_trend_d3   %>%mutate(Period=periods, Sex ="male")

cohort_trends_d3 <- rbind(female_c_trend_d3,male_c_trend_d3)

ggplot(cohort_trends_d3, aes(x=Cohort, y=est, color=Sex, shape=Sex))+
  labs(#title = "Trajectories of the baseline log-cumulative rates of onset/odds of onset across Cohorts , by Sex",
       subtitle = "Cubic Temporal Trend, Cohort"
  )+    
  xlab("Cohort") +
  ylab(bquote("Additive Effect in the Log-Cumulative Rate/Odds of Onset")) +
  theme_bw()+
  scale_x_continuous(breaks = number_ticks(10)) +
  #scale_y_continuous(breaks = number_ticks(14)) +
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = -1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = -1))+
  
  geom_line()+
  geom_point(size=2.5, alpha=.9,color=1)+
  theme(legend.position = "bottom",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 18))
  

period_trends_d3 <- rbind(female_p_trend_d3,male_p_trend_d3)
ggplot(period_trends_d3, aes(x=Period, y=est, color=Sex, shape=Sex))+
  labs(#title = "Change in the Log Cum Rate/Odds of Onset as a Function of Period, by Sex",
       subtitle = "Cubic temporal trend, Period"
  )+    
  xlab("Period") +
  ylab(bquote("Additive Effect in the Log-Cumulative Rate/Odds of Onset")) +
  theme_bw()+
  scale_x_continuous(breaks = number_ticks(14)) +
  #scale_y_continuous(breaks = number_ticks(14)) +
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = -1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = -1))+
  theme(legend.position = "bottom",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 18))+
  geom_line()+
  geom_point(size=2, alpha=.9,color=1)

# Additional graph cubic
coeffs2_f_c_d3<- data.frame(est =c(319.529,116.417,37.067 ),poly_term=  c("cohort","cohort^2", "cohort^3"),  sex =  "female", trend ="cohort")
coeffs2_m_c_d3<- data.frame(est =c(90.706 ,15.569,31.580  ), poly_term=  c("cohort","cohort^2", "cohort^3"), sex = "male", trend ="cohort")
coeffs2_f_p_d3<- data.frame(est =c(126.364,-3.440, 44.823 ), poly_term=  c("period","period^2","period^3")  ,sex = "female", trend ="period")
coeffs2_m_p_d3<- data.frame(est =c(69.313,-0.090, 31.575  ),poly_term=  c("period","period^2","period^3"), sex = "male", trend ="period")


cohort_comparison_d3<- rbind(coeffs2_f_c_d3,coeffs2_m_c_d3)
period_comparison_d3<- rbind(coeffs2_f_p_d3,coeffs2_m_p_d3)

ggplot(cohort_comparison_d3, aes(x= est, y= poly_term, color=sex))+
  labs(#title = "Comparison of Temporal Trend  Estimates by Sex",
       subtitle = "Cubic temporal trend, Cohort"
  )+    
  xlab("Coefficient Estimate") +
  ylab(bquote("Temporal Trend Term")) +
  
  geom_point(size=3, alpha=0.7)+
  geom_vline(xintercept = 0, lty= 3, size =1.5)+
  xlim(c(-350,350))+
  theme_bw()+
  theme(legend.position = "right",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 18))+
  #scale_x_continuous(breaks = number_ticks(50)) +
  #scale_y_continuous(breaks = number_ticks(14)) +
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = 1))


ggplot(period_comparison_d3, aes(x= est, y= poly_term, color=sex))+
  labs(#title = "Comparison of Temporal Trend Estimates by Sex",
       subtitle = "Cubic temporal trend, Period"
  )+    
  xlab("Coefficient Estimate") +
  ylab(bquote("Temporal Trend Term")) +
  
  geom_point(size=3, alpha=0.7)+
  geom_vline(xintercept = 0, lty= 3, size =1.5)+
  xlim(c(-150,150))+
  theme_bw()+
  theme(legend.position = "right",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 18))+
  #scale_x_continuous(breaks = number_ticks(25)) +
  #scale_y_continuous(breaks = number_ticks(14)) +
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = 1))
#------

# Best fit model curves of the change in the log cum rates



ggplot(cohort_trends_d3, aes(x=Cohort, y=est, color=Sex, shape=Sex))+
  labs(#title = "Change in the Log Cum Rate/Odds of Onset as a function of Cohort, by Sex",
       subtitle = "Cubic Trend (females) vs Cubic Trend (males)"
  )+    
  xlab("Cohort") +
  ylab(bquote("Additive Effect on the Log-Cumulative Rates of Onset")) +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 18))+
  scale_x_continuous(breaks = number_ticks(10)) +
  #scale_y_continuous(breaks = number_ticks(14)) +
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = 1))+
  
  geom_line()+
  geom_point(size=2.5, alpha=.9,color=1)

best_fit_trends_period<- rbind(female_p_trend_d1,male_p_trend_d3)

ggplot(best_fit_trends_period, aes(x=Period, y=est, color=Sex, shape=Sex))+
  labs(#title = "Change in the Log Cum Rate/Odds of Onset as a function of Period, by Sex",
       subtitle = "Linear Trend (females) vs Cubic Trend (males)"
  )+    
  xlab("Period") +
  ylab(bquote("Additive Effect on the Log-Cumulative Rates of Onset")) +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 18))+
  scale_x_continuous(breaks = number_ticks(14)) +
  #scale_y_continuous(breaks = number_ticks(14)) +
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = 1, direction = 1))+
  
  geom_line()+
  geom_point(size=2.5, alpha=.9,color=1)


# ---- ORTHOGONAL POLYNOMIALS

# y <- exp(c(1,-2,1,3,2,4,-2,3))
# x<-  c(1,2,3,4,5,6,7,8)
# fit_2 <- lm(y~poly(x,2))
# fit_3 <- lm(y~poly(x,3, raw = T))
# plot(x,y)
# plot(lm(y~poly(x,3)))
# 
# plot(x,predict.lm(fit_3,newdata=data.frame(x=x)), type="l")
# points(x,y,col="red")
# ? plot
# 
# fit_3$coefficients[2:4]
# 
# c3 <- diag(c(20.40769,   -11.52838,   -12.52449))
# c3_2<- diag(c(-15.311250          ,   6.048027         ,   -0.513886 ))
# poly_trend <- function(x, poly_d, coeffs){
#   trend <- list()
#   x_num <- unique(as.numeric(x))
#   m <- as.matrix(poly(x_num,poly_d, raw = T))
#   trend[[1]] <- (m%*%coeffs)%*%c(rep(1,poly_d))
#   return(trend)
# }
# 
# try3<- as.data.frame(poly_trend(x=x, poly_d  = 3, coeffs = c3_2)  )#%>%mutate(Cohort=unique(plot_df_female_c$Cohort))
# names(try3)<- "est"
# plot(x,(try3$est+mean(y)-2.074522 ), type = "p", col = "green")
# points(x,y,col="red")
# lines(x,predict.lm(fit_3,newdata=data.frame(x=x)))
# #-(try3$est+mean(y)))
# 
# m <- as.matrix(poly(x,3))
# trend3<- m%*%c3
# poly(x,3)
# 
# 
# fit_3$coefficients[2:4]
