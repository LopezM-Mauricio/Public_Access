#-------------------------------------------------------------------------------- #
# Results | Result Plots and Tables#
# Author: Mauricio, Lopez Mendez
# Date: 08/10/2020
#-------------------------------------------------------------------------------- #
# 0. Libraries ----
library(survival)
library(splines)
library(flexsurv)
library(eha)
library(epitools)
library(stats)

library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(viridis)
library(patchwork)

library(reshape2)
library(readxl)
library(openxlsx)
library(dampack)

library(magrittr) 
library(purrr)
#------------------------------------------------------------------------------- #
# 1. LOAD INPUTS  ----
load(file="D_Output/best_fit_prop_models_final.RData")
mod_odds_males_period     <- best_fit_prop_models_final$mod8_m_p_1
mod_odds_females_period   <- best_fit_prop_models_final$mod9_f_p_1
mod_odds_males_cohort     <- best_fit_prop_models_final$mod8_m_c_1
mod_odds_females_cohort   <- best_fit_prop_models_final$mod8_f_c_1
#------------------------------------------------------------------------------- #
load("D_Output/model_inputs_cohort.RData")
load("D_Output/model_inputs_period.RData")
years_male   <- model_inputs_period$data_dfs$dF_male_p$Year
years_female <- model_inputs_period$data_dfs$df_frmale_p$Year
#------------------------------------------------------------------------------- #
#------------------------------------------------------------------------------- #
# 2. Recoding Cohort Dec ----
model_inputs_cohort$data_dfs$dF_male_c$Cohort.dec <- ifelse(model_inputs_cohort$data_dfs$dF_male_c$Cohort.dec== 1, 1930,
                                                            ifelse(model_inputs_cohort$data_dfs$dF_male_c$Cohort.dec==2,  1940,
                                                                   ifelse(model_inputs_cohort$data_dfs$dF_male_c$Cohort.dec==3,  1950,
                                                                          ifelse(model_inputs_cohort$data_dfs$dF_male_c$Cohort.dec==4,  1960,
                                                                                 ifelse(model_inputs_cohort$data_dfs$dF_male_c$Cohort.dec==5,  1970,
                                                                                        ifelse(model_inputs_cohort$data_dfs$dF_male_c$Cohort.dec==6,  1980,
                                                                                               1990))))))
cohorts_male <- model_inputs_cohort$data_dfs$dF_male_c$Cohort.dec 

model_inputs_cohort$data_dfs$df_frmale_c$Cohort.dec <- ifelse(model_inputs_cohort$data_dfs$df_frmale_c$Cohort.dec== 1, 1930,
                                                              ifelse(model_inputs_cohort$data_dfs$df_frmale_c$Cohort.dec==2, 1940,
                                                                     ifelse(model_inputs_cohort$data_dfs$df_frmale_c$Cohort.dec==3,1950,
                                                                            ifelse(model_inputs_cohort$data_dfs$df_frmale_c$Cohort.dec==4,1960,
                                                                                   ifelse(model_inputs_cohort$data_dfs$df_frmale_c$Cohort.dec==5,1970,
                                                                                          ifelse(model_inputs_cohort$data_dfs$df_frmale_c$Cohort.dec==6,1980,1990))))))

cohorts_female <- model_inputs_cohort$data_dfs$df_frmale_c$Cohort.dec
#------------------------------------------------------------------------------- #
#------------------------------------------------------------------------------- #
# 3. Predictions Using Best Fit Models for Period ----
## 3.1 Strata 2 | Males ---- 
#Use : mod_odds_males_period 
#mod_odds_males_period$coefficients

pred_1998_m <-as.data.frame(summary(mod_odds_males_period, 
                                    newdata=data.frame(Year= 1998, F=0),
                                    type = "hazard", ci = TRUE,
                                    tidy = TRUE))%>%mutate(Year = "1998")
pred_2002_m <-as.data.frame(summary(mod_odds_males_period, 
                                    newdata = data.frame(Year= 2002, F=0) ,
                                    type = "hazard", ci = TRUE,
                                    tidy = TRUE))%>%mutate(Year = "2002")
pred_2008_m <-as.data.frame(summary(mod_odds_males_period, 
                                    newdata = data.frame(Year= 2008, F=0) ,
                                    type = "hazard", ci = TRUE,
                                    tidy = TRUE))%>%mutate(Year = "2008")
pred_2011_m <-as.data.frame(summary(mod_odds_males_period, 
                                    newdata = data.frame(Year= 2011, F=0) ,
                                    type = "hazard", ci = TRUE,
                                    tidy = TRUE))%>%mutate(Year = "2011")
pred_2016_m <-as.data.frame(summary(mod_odds_males_period, 
                                    newdata = data.frame(Year= 2016, F=0) ,
                                    type = "hazard", ci = TRUE,
                                    tidy = TRUE))%>%mutate(Year = "2016")
plot_df_male_p<-as.data.frame(rbind(pred_1998_m[,c(1,2,6)],pred_2002_m[,c(1,2,6)],pred_2008_m[,c(1,2,6)],pred_2011_m[,c(1,2,6)],pred_2016_m[,c(1,2,6)]))
names(plot_df_male_p) <- c("Init.Age", "h_t","Period")
plot_df_male_p        <- plot_df_male_p%>%mutate(lcl=NA, ucl=NA)
#------------------------------------------------------------------------------- #
#------------------------------------------------------------------------------- #
## 3.1 Strata 1 | Females ---- 
# use model: mod_odds_females_period
pred_1998_f      <-as.data.frame(summary(mod_odds_females_period, 
                                       newdata = data.frame(Year= 1998, F=0) ,
                                       type = "hazard", ci = TRUE,
                                       tidy = TRUE))%>%mutate(Year = "1998")
pred_2002_f      <-as.data.frame(summary(mod_odds_females_period, 
                                       newdata = data.frame(Year= 2002, F=0) ,
                                       type = "hazard", ci = TRUE,
                                       tidy = TRUE))%>%mutate(Year = "2002")
pred_2008_f      <-as.data.frame(summary(mod_odds_females_period, 
                                       newdata = data.frame(Year= 2002, F=0) ,
                                       type = "hazard", ci = TRUE,
                                       tidy = TRUE))%>%mutate(Year = "2008")
pred_2011_f      <-as.data.frame(summary(mod_odds_females_period, 
                                       newdata = data.frame(Year= 2011, F=0) ,
                                       type = "hazard", ci = TRUE,
                                       tidy = TRUE))%>%mutate(Year = "2011")
pred_2016_f      <-as.data.frame(summary(mod_odds_females_period, 
                                       newdata = data.frame(Year= 2016, F=0) ,
                                       type = "hazard", ci = TRUE,
                                       tidy = TRUE))%>%mutate(Year = "2016")

plot_df_female_p <-as.data.frame(rbind(pred_1998_f[,c(1,2,3,4,6)],pred_2002_f[,c(1,2,3,4,6)],
                                      pred_2008_f[,c(1,2,3,4,6)],pred_2011_f[,c(1,2,3,4,6)],
                                      pred_2016_f[,c(1,2,3,4,6)]))
names(plot_df_female_p)<- c("Init.Age", "h_t","lcl","ucl","Period")
colnames(plot_df_female_p)
colnames(plot_df_male_p)
plot_df_female_p$Sex  <- "female"
plot_df_male_p$Sex    <- "male"

#plot_df_female_p$Period <- as.factor(plot_df_female_p$Period)
#plot_df_female_p$Period <-factor(plot_df_female_p$Period , levels = c("2016","2011","2008","2002","1998"))
# plot_df_female_p[plot_df_female_p$h_t <0]<-0
# plot_df_female_p[plot_df_female_p$lcl <0]<-0
# plot_df_female_p[plot_df_female_p$ucl <0]<-0

#------------------------------------------------------------------------------- #
#------------------------------------------------------------------------------- #
# 3. Predictions Using Best Fit Models for Period ----
## 3.1 Strata 2 | Males ---- 
#Use : mod_odds_males_cohort
pred_1930_m<- as.data.frame(summary(mod_odds_males_cohort, 
                                    newdata = data.frame(Cohort.dec= 1930, F=0) ,
                                    type = "hazard", ci = TRUE,
                                    tidy = TRUE))%>%mutate(Cohort = "1930")
pred_1940_m<- as.data.frame(summary(mod_odds_males_cohort, 
                                    newdata = data.frame(Cohort.dec= 1940, F=0) ,
                                    type = "hazard", ci = TRUE,
                                    tidy = TRUE))%>%mutate(Cohort = "1940")
pred_1950_m<- as.data.frame(summary(mod_odds_males_cohort, 
                                    newdata = data.frame(Cohort.dec= 1950, F=0) ,
                                    type = "hazard", ci = TRUE,
                                    tidy = TRUE))%>%mutate(Cohort = "1950")
pred_1960_m<- as.data.frame(summary(mod_odds_males_cohort, 
                                    newdata = data.frame(Cohort.dec= 1960, F=0) ,
                                    type = "hazard", ci = TRUE,
                                    tidy = TRUE))%>%mutate(Cohort = "1960")
pred_1970_m<- as.data.frame(summary(mod_odds_males_cohort, 
                                    newdata = data.frame(Cohort.dec= 1970, F=0) ,
                                    type = "hazard", ci = TRUE,
                                    tidy = TRUE))%>%mutate(Cohort = "1970")
pred_1980_m<- as.data.frame(summary(mod_odds_males_cohort, 
                                    newdata = data.frame(Cohort.dec= 1980, F=0) ,
                                    type = "hazard", ci = TRUE,
                                    tidy = TRUE))%>%mutate(Cohort = "1980")
pred_1990_m<- as.data.frame(summary(mod_odds_males_cohort, 
                                    newdata = data.frame(Cohort.dec= 1990, F=0) ,
                                    type = "hazard", ci = TRUE,
                                    tidy = TRUE))%>%mutate(Cohort = "1990")
plot_df_male_c<-as.data.frame(
  rbind(pred_1930_m[,c(1,2,6)],pred_1940_m[,c(1,2,6)],
        pred_1950_m[,c(1,2,6)],pred_1960_m[,c(1,2,6)],
        pred_1970_m[,c(1,2,6)],pred_1980_m[,c(1,2,6)],
        pred_1990_m[,c(1,2,6)]))

names(plot_df_male_c)<- c("Init.Age", "h_t","Cohort")
colnames(plot_df_male_c)
plot_df_male_c<-plot_df_male_c%>%mutate("lcl"=NA,"ucl"=NA)
names(plot_df_male_c)<- c("Init.Age","h_t", "Cohort","lcl","ucl")
#------------------------------------------------------------------------------- #
#------------------------------------------------------------------------------- #
## 3.1 Strata 1 | Females ---- 
pred_1930_f<- as.data.frame(summary(mod_odds_females_cohort, 
                                    newdata = data.frame(Cohort.dec= 1930, F=0) ,
                                    type = "hazard", ci = TRUE,
                                    tidy = TRUE))%>%mutate(Cohort = "1930")
pred_1940_f<- as.data.frame(summary(mod_odds_females_cohort, 
                                    newdata = data.frame(Cohort.dec= 1940, F=0) ,
                                    type = "hazard", ci = TRUE,
                                    tidy = TRUE))%>%mutate(Cohort = "1940")
pred_1950_f<- as.data.frame(summary(mod_odds_females_cohort, 
                                    newdata = data.frame(Cohort.dec= 1950, F=0) ,
                                    type = "hazard", ci = TRUE,
                                    tidy = TRUE))%>%mutate(Cohort = "1950")
pred_1960_f<- as.data.frame(summary(mod_odds_females_cohort, 
                                    newdata = data.frame(Cohort.dec= 1960, F=0) ,
                                    type = "hazard", ci = TRUE,
                                    tidy = TRUE))%>%mutate(Cohort = "1960")
pred_1970_f<- as.data.frame(summary(mod_odds_females_cohort, 
                                    newdata = data.frame(Cohort.dec= 1970, F=0) ,
                                    type = "hazard", ci = TRUE,
                                    tidy = TRUE))%>%mutate(Cohort = "1970")
pred_1980_f<- as.data.frame(summary(mod_odds_females_cohort, 
                                    newdata = data.frame(Cohort.dec= 1980, F=0) ,
                                    type = "hazard", ci = TRUE,
                                    tidy = TRUE))%>%mutate(Cohort = "1980")
pred_1990_f<- as.data.frame(summary(mod_odds_females_cohort, 
                                    newdata = data.frame(Cohort.dec= 1990, F=0) ,
                                    type = "hazard", ci = TRUE,
                                    tidy = TRUE))%>%mutate(Cohort = "1990")
plot_df_female_c<-as.data.frame(
  rbind(pred_1930_f[,c(1,2,3,4,6)],pred_1940_f[,c(1,2,3,4,6)],
        pred_1950_f[,c(1,2,3,4,6)],pred_1960_f[,c(1,2,3,4,6)],
        pred_1970_f[,c(1,2,3,4,6)],pred_1980_f[,c(1,2,3,4,6)],
        pred_1990_f[,c(1,2,3,4,6)]))

names(plot_df_female_c)    <- c("Init.Age", "h_t","lcl","ucl","Cohort")
colnames(plot_df_female_c)
plot_df_male_c$Sex         <- "male"
plot_df_female_c$Sex       <- "female"
plot_df_all_c              <- rbind(plot_df_female_c, plot_df_male_c)

#------------------------------------------------------------------------------- #
# Consolidating Dataframes
plot_df_all_p <- rbind(plot_df_female_p,plot_df_male_p)
plot_df_all_c  <- rbind(plot_df_female_c, plot_df_male_c)
#------------------------------------------------------------------------------- #
#------------------------------------------------------------------------------- #
# 4. Rates of Onset Plots ----
## 4.1  Facet wrap Plot, period ----

plot_all_period <- ggplot(data = plot_df_all_p, aes(x = Init.Age, y = h_t*(1000), color= Period, fill = Period)) + 
  geom_line( size=0.4)+ 
  #geom_ribbon(aes(ymin = lcl, ymax = ucl, alpha = 0.35), linetype = 0, show.legend = F) +
  #geom_point(data = df_surv.p.female, shape= df_surv.p.female$Period, size = 0.7) +
  labs(title = ""
       #,subtitle = "Flexible Parametric Model: internal knots = 3, constant and age-homogeneous period trend, AIC = 22166.17"
  )+    
  xlab("Age (years)") +
  ylab(bquote("Rate of Onset per 1,000")) +
  #ylim(c(0, 0.035*(1000)))+
  #xlim(c(0,65))+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.key.size = unit(.75, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        #legend.key.width =   unit(.5, "cm"),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=16),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 16),
        strip.background = element_blank(),
        #strip.background = element_rect(fill = "white", color = "black"),
        strip.text = element_text(hjust = 0, face = "bold", size = 16),
  )+
  guides(colour = guide_legend(nrow = 1))+
  
  
  scale_x_continuous(breaks = seq(0,65,5))+
  scale_y_continuous(breaks = seq(0,30,5)) +
  scale_colour_manual(values = inferno(5, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(5, alpha = 1, begin = 0, end = 1, direction = 1))+
  facet_wrap(~Sex)

plot_all_period
#------------------------------------------------------------------------------- #
## 4.2  Facet wrap Plot , cohort ----
plot_all_cohort <- ggplot(data = plot_df_all_c, aes(x = Init.Age, y = h_t*(1000), color= Cohort, fill = Cohort)) + 
  geom_line(linetype="solid", size=0.4)+ 
  #geom_ribbon(aes(ymin = lcl, ymax = ucl, alpha = 0.15), linetype = 0, show.legend = F) +
  #geom_point( aes(shape= Cohort, size = 0.2)) +
  labs(title = ""
       #,subtitle = "Flexible Parametric Model: internal knots = 3, constant and age-homogeneous period trend, AIC = 22166.17"
  )+    
  xlab("Age (years)") +
  ylab(bquote("Rates of Onset per 1,000")) +
  #ylim(c(0, 0.055*(1000)))+
  #xlim(c(0,65))+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.key.size = unit(.75, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        #legend.key.width =   unit(.5, "cm"),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=16),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 16),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold", size = 16),
        #strip.wrap =  unit(.15, "cm")
  )+
  guides(colour = guide_legend(nrow = 1))+
  
  
  scale_x_continuous(breaks = seq(0,65,5)) +
  scale_y_continuous(breaks = seq(0,55,5)) +
  scale_colour_manual(values = magma(7, alpha = 1, begin = 0.1, end = .7, direction = 1))+
  scale_fill_manual(values = magma(7, alpha = 1, begin = 0.1, end = .7, direction = 1))+
  facet_wrap(~Sex)

plot_all_cohort

#------------------------------------------------------------------------------- #
## 4.3 Figure 3 ----
patch_1 <- plot_all_period /plot_all_cohort
Figure_3 <- patch_1 + plot_annotation(tag_levels = list(c("(A) by Period","(B) by Decennial Birth Cohort"))) & 
  theme(plot.tag = element_text(size = 20, hjust = 0, vjust = 0, face = "bold"),
        plot.tag.position = c(0,.98)) 
Figure_3
#------------------------------------------------------------------------------- #
#------------------------------------------------------------------------------- #
# 5. Conditional Mean Plots ----
female_cmean_cohort <-data.frame(Trend ="Cohort", 
                                 Temp  =c(1930,1940,1950,1960,1970,1980,1990),
                                 df    = c(1,9,53,115,205,344,754),
                                 est   = c(17,21.6,24.3,21.5,19.7,18.3,15.7),  
                                 se    = c(NA, 2.0912, 1.6886, 0.7040, 0.4816, 0.2513, 0.0913),
                                 Sex  = "female") %>% mutate(
                                   lcl1  = est - qt(0.05, df)*se,
                                   ucl1  = est + qt(0.05, df)*se,
                                   ucl2  = est - (qnorm(0.05))*se,
                                   lcl2  = est + (qnorm(0.05))*se)
                                   
male_cmean_cohort   <- data.frame(Trend ="Cohort",
                                  Temp = c(1930,1940,1950,1960,1970,1980,1990), 
                                  df   = c(10,92,553,1044,1445,1378,1540),
                                  est = c(24.6,23.6,20.2,18.3,17.8,17.1,15.5), 
                                  se  = c(3.4846, 1.0692, 0.3345, 0.1806, 0.1244, 0.1040, 0.0651),
                                  Sex="male") %>% mutate(
                                    lcl1 = est - qt(0.05, df)*se,
                                    ucl1 = est + qt(0.05, df)*se,
                                    ucl2  = est - (qnorm(0.05))*se,
                                    lcl2  = est + (qnorm(0.05))*se)
                                    
female_cmean_period <-data.frame(Trend  ="Period",
                                 Temp = c(1998,2002,2008,2011,2016), 
                                 df   = c(28,55,353,119,929),
                                 est = c(20.6,14.4,17.5,18.3,17.7), 
                                 se  = c(0.940, 0.849, 0.228, 0.606, 0.185),
                                 Sex = "female") %>% mutate(
                                   lcl1 = NA,
                                   ucl1 = NA,
                                   ucl2  = est - (qnorm(0.05))*se,
                                   lcl2  = est + (qnorm(0.05))*se)
                                   
                                 

male_cmean_period   <- data.frame(Trend ="Period",
                                  Temp = c(1998,2002,2008,2011,2016), 
                                  df   = c(295,404,1584,731,3051),
                                  est = c(19.3,13.4,17.9,17.9,17.4), 
                                  se  = c(0.3166, 0.2588, 0.1244, 0.1738, 0.0901),
                                  Sex="male") %>%mutate(
                                    lcl1 = NA,
                                    ucl1 = NA,
                                    ucl2  = est - (qnorm(0.05))*se,
                                    lcl2  = est + (qnorm(0.05))*se)
                                    
                                  


all.cmean_cohort <- rbind(female_cmean_cohort,male_cmean_cohort
)

all.cmean_period <- rbind(female_cmean_period,
                          male_cmean_period  )

cm_plot1<- ggplot(all.cmean_cohort, aes(y= est, x= as.factor(Temp), ymin = as.numeric(lcl2), ymax = as.numeric(ucl2), shape=Sex ,color = Sex, linetype = Sex))+
  geom_pointrange(size=2, alpha=0.8, position = position_dodge(width=0.5))+  
  labs(title = "")+    
  xlab("Cohort") +
  ylab(bquote("Mean Age of Onset")) +
  #geom_hline(aes(yintercept=13, color="white"))+
  theme_bw()+
  ylim(c(10,35))+
  theme(legend.position = "right",
        legend.key.size = unit(.85, "cm"),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16),
        plot.title = element_text(size=16),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))+
  #scale_x_continuous(breaks = seq(1930,1990,10)) +
  #scale_y_continuous(breaks = seq(10,35,5)) +
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))
cm_plot1
# ylim(c(13,25))


cm_plot2<- ggplot(all.cmean_period, aes(y= est, x= as.factor(Temp), ymin = as.numeric(lcl2), ymax = as.numeric(ucl2), shape=Sex ,color = Sex, linetype = Sex))+
  geom_pointrange(size=2, alpha=0.8, position = position_dodge(width=0.5))+  
  labs(title = "")+    
  xlab("Period") +
  ylab(bquote("Mean Age of Onset")) +
  theme_bw()+
  ylim(c(10,35))+
  theme(legend.position = "",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16),
        plot.title = element_text(size=16),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))+
  #scale_x_continuous(breaks = c(1998,2002, 2008, 2011,2016)) +
  #scale_y_continuous(breaks = seq(10,35,5)) +
  scale_colour_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(2, alpha = 1, begin = 0, end = .8, direction = 1))
cm_plot2

## 5.1 Figure 2 ----
patch2   <- cm_plot2 + cm_plot1
Figure_2 <- patch2 + plot_annotation(tag_levels = list(c("(A) by Period","(B) by Decennial Birth Cohort"))) & 
  theme(plot.tag = element_text(size = 20, hjust = 0, vjust = 0, face = "bold"),
        plot.tag.position = c(0,.98)) 
Figure_2
#------------------------------------------------------------------------------- #
#-------------------------------------------------------------------------------- #
# 6 Trends (Log Cum Odds) ----
#-------------------------------------------------------------------------------- #
# Function to compute the contribution of covariates to the log cumulative odds from best fitted model coefficients
# takes as input a vector of sorted values of the temporal covariate (period,cohort), the degree of the orth. polynomial model, 
# and the coefficients of the orth. poynomial model
# returns a vector in the log cumulative odds scale that represent the change in the log cumulative odds per unitary change in the covariate

poly_trend <- function(x, poly_d, coeffs){
  trend <- list()
  x_num <- unique(as.numeric(x))
  m <- as.matrix(poly(x_num,poly_d))
  trend[[1]] <- (m%*%coeffs)%*%c(rep(1,poly_d))
  return(trend)
}
#-------------------------------------------------------------------------------- #
# 6.1 Extracting trends ----
# extracting est for linear model
coeffs_f_p_d1<- mod_odds_females_period$res[6,1]

cohorts<- c(1930,1940,1950,1960,1970,1980,1990)
periods<- c(1998,2002,2008,2011,2016)
female_p_trend_d1<- as.data.frame(poly_trend(x=periods, poly_d  = 1, coeffs = coeffs_f_p_d1)  )
names(female_p_trend_d1)<- "est"
female_p_trend_d1<-female_p_trend_d1 %>%mutate(Period=periods, Sex ="Female")
#-------------------------------------------------------------------------------- #
# extracting est for cubic models

coeffs_f_c_d3<- diag(mod_odds_females_cohort$res[6:8,1])
coeffs_m_c_d3<- diag(mod_odds_males_cohort$res[10:12,1])
coeffs_m_p_d3<- diag(mod_odds_males_period$res[9:11,1])

female_c_trend_d3<- as.data.frame(poly_trend(x=cohorts, poly_d  = 3, coeffs = coeffs_f_c_d3)  )
male_c_trend_d3  <- as.data.frame(poly_trend(x=cohorts, poly_d  = 3, coeffs = coeffs_m_c_d3)  )
male_p_trend_d3  <- as.data.frame(poly_trend(x=periods, poly_d  = 3, coeffs = coeffs_m_p_d3) ) 

names(female_c_trend_d3)<- "est"
names(male_c_trend_d3)<- "est"
names(male_p_trend_d3)<- "est"
female_c_trend_d3<-female_c_trend_d3 %>%mutate(Cohort=cohorts, Sex ="Female")
male_c_trend_d3  <-male_c_trend_d3   %>%mutate(Cohort=cohorts, Sex ="Male")
male_p_trend_d3  <-male_p_trend_d3   %>%mutate(Period=periods, Sex ="Male")

cohort_trends_d3 <- rbind(female_c_trend_d3,male_c_trend_d3)
#-------------------------------------------------------------------------------- #
## 6.2 Trend Plots ----
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
#-------------------------------------------------------------------------------- #
## 6.3 Figure 4 ----
patch_3 <- plot_period_trend / plot_cohort_trend
Figure_4 <- patch_3 + plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = list(c("(A) Linear Trend (Females) vs Cubic Trend (Males), by Period ","(B) Cubic Trend (Females) vs Cubic Trend (Males), by Cohort"))) & 
  theme(plot.tag = element_text(size = 20, hjust = 0, vjust = 0, face = "bold"),
        plot.tag.position = c(0,.98)) 
Figure_4

