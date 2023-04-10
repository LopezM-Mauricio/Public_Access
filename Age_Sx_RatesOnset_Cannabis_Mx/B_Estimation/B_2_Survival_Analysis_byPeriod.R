# SPLINE FITTING : SURVIVAL AND HAZARD FUNCTIONS (Period) --------------------- #
# Author: Mauricio, Lopez Mendez
# Date: NOV, 26/2019
# ----------------------------------------------------------------------------- #
# Notes: 
# DATA: interval censored and right censored (1998-2016)
# ANALYSIS: include  predictors (Cohort) 
#   KM:  Non-parametric survival and hazard estimates through kaplan-meier estimator
#   SPLINES: different specifications on the splines (# of knots on log cum hazard/odds, #of knots on Cohort, 
#   also in time-varying or linear predictors (Cohort)
#   OUTPUT: lists, containing fitted objects from spline models + AIC and Coefficients for each specification
#   PLOTS: for KM estimates, Splines, comparison of hazards KM vs Splines
# Stratification: Using subsets of the data split into binary sex categories: Male, Female
# The reason is that survival times seem to distribute quite differently for each population, we did a log-rank test to confirm this too
# We get better fit using this analysis,but consider we are using less data
# ----------------------------------------------------------------------------- #
# 0. LIBRARIES ----

library(survival)
library(splines)
library(flexsurv)
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
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# 1. LOAD DATA ---- 
# Interval and Right-Censored Data (1998-2016)
DMarihuana_1998_2016 <- read_excel("C:/Users/mlopezme/Dropbox (Brown)/Troubleshooting-Parametric Models and Splines/DS_SURV_MARIHUANA_1998_2016.xlsx")
DMarihuana_1998_2016$Sex  <- factor(DMarihuana_1998_2016$Sex, labels = c("Male", "Female"))
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# 2. RECODING 4 ANALYSIS ----
## 2.2 Strata
# Female population
DMarihuana_1998_2016.p.female <- DMarihuana_1998_2016%>%
  filter(Sex == "Female")
nrow(DMarihuana_1998_2016.p.female) #79684
# Male population
DMarihuana_1998_2016.p.male <- DMarihuana_1998_2016%>%
  filter(Sex == "Male")
nrow(DMarihuana_1998_2016.p.male) #61658
str(DMarihuana_1998_2016.p.male)
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# 3  LOG-RANK TEST ------
DATA_02_16 <- filter(DMarihuana_1998_2016, Year!="1998")
SURV_ALL <- Surv(time  = DATA_02_16 $AgeR_Marihuana, 
     event = DATA_02_16 $Event, 
     type  = "right")

logrank_period <- survdiff(SURV_ALL ~   factor(Sex) + factor(Year), data = DATA_02_16)
logrank_period
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# 4. SURVIVAL OBJECTS ----
## 4.1 STRATA 1|Female ----
my.surv.ic.p.female <- Surv(time  = DMarihuana_1998_2016.p.female$AgeL_Marihuana, 
                            time2 = DMarihuana_1998_2016.p.female$AgeR_Marihuana, 
                            event = DMarihuana_1998_2016.p.female$Event, 
                            type  = "interval")
#restricted mean survival
np.fit.ic.p.female <- survfit(my.surv.ic.p.female ~ Year, data = DMarihuana_1998_2016.p.female)
print(np.fit.ic.p.female, rm=18) #restricted mean survival
print(np.fit.ic.p.female, rm=25) #restricted mean survival
print(np.fit.ic.p.female, rm=65) #restricted mean survival
# KM PLOT
plot(np.fit.ic.p.female, main="Kaplan-Meier estimate with 95% confidence bounds",
     xlab="time", ylab="survival function")
# ----------------------------------------------------------------------------- #
## 4.2 STRATA 2| Male ----
my.surv.ic.p.male <- Surv(time  = DMarihuana_1998_2016.p.male$AgeL_Marihuana, 
                          time2 = DMarihuana_1998_2016.p.male$AgeR_Marihuana, 
                          event = DMarihuana_1998_2016.p.male$Event, 
                          type  = "interval")
#restricted mean survival
np.fit.ic.p.male <- survfit(my.surv.ic.p.male ~ Year, data = DMarihuana_1998_2016.p.male)
print(np.fit.ic.p.male, rm=18) #restricted mean survival
print(np.fit.ic.p.male, rm=25) #restricted mean survival
print(np.fit.ic.p.male, rm=65) #restricted mean survival

# KM PLOT
plot(np.fit.ic.p.male, main="Kaplan-Meier estimate with 95% confidence bounds",
     xlab="time", ylab="survival function")
# ----------------------------------------------------------------------------- #
# Saving Surv objects and Data, by Period
model_inputs_period <- list(surv_objects = list(so_female_p=my.surv.ic.p.female, so_male_p=my.surv.ic.p.male), 
                            data_dfs     = list(df_frmale_p=DMarihuana_1998_2016.p.female, dF_male_p =DMarihuana_1998_2016.p.male))

save(model_inputs_period, file = "Data_/model_inputs_period.RData")

# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# 5. ANDERSON PLOTS ----
# NOTES: A VISUAL TEST OF PROPORTIONALITY OF HAZARDS ACROSS PERIODS, non-parallel
# trends indicate possible violation of proportional hazards
## 5.1 STRATA 1 | Female  ----
# Subsets by period
DMarihuana_1998ONLY.p.female <- filter(DMarihuana_1998_2016.p.female, Year=="1998")
DMarihuana_2002ONLY.p.female <- filter(DMarihuana_1998_2016.p.female, Year=="2002")
DMarihuana_2008ONLY.p.female <- filter(DMarihuana_1998_2016.p.female, Year=="2008")
DMarihuana_2011ONLY.p.female <- filter(DMarihuana_1998_2016.p.female, Year=="2011")
DMarihuana_2016ONLY.p.female <- filter(DMarihuana_1998_2016.p.female, Year=="2016")
# Surv objects by period
surv_female_1998 <-  Surv(time  = DMarihuana_1998ONLY.p.female$AgeL_Marihuana, 
                          time2 = DMarihuana_1998ONLY.p.female$AgeR_Marihuana, 
                          event = DMarihuana_1998ONLY.p.female$Event, 
                          type  = "interval")

surv_female_2002 <-  Surv(time  = DMarihuana_2002ONLY.p.female$AgeL_Marihuana, 
                          time2 = DMarihuana_2002ONLY.p.female$AgeR_Marihuana, 
                          event = DMarihuana_2002ONLY.p.female$Event, 
                          type  = "interval")

surv_female_2008 <-  Surv(time  = DMarihuana_2008ONLY.p.female$AgeL_Marihuana, 
                          time2 = DMarihuana_2008ONLY.p.female$AgeR_Marihuana, 
                          event = DMarihuana_2008ONLY.p.female$Event, 
                          type  = "interval")

surv_female_2011 <-  Surv(time  = DMarihuana_2011ONLY.p.female$AgeL_Marihuana, 
                          time2 = DMarihuana_2011ONLY.p.female$AgeR_Marihuana, 
                          event = DMarihuana_2011ONLY.p.female$Event, 
                          type  = "interval")

surv_female_2016 <-  Surv(time  = DMarihuana_2016ONLY.p.female$AgeL_Marihuana, 
                          time2 = DMarihuana_2016ONLY.p.female$AgeR_Marihuana, 
                          event = DMarihuana_2016ONLY.p.female$Event, 
                          type  = "interval")
# Cum hazards, from naelson aelen
np.fit.ic.p.female_1998ONLY <- survfit(surv_female_1998 ~ 1, data = DMarihuana_1998ONLY.p.female, type='fh')
np.fit.ic.p.female_2002ONLY <- survfit(surv_female_2002 ~ 1, data = DMarihuana_2002ONLY.p.female, type='fh')
np.fit.ic.p.female_2008ONLY <- survfit(surv_female_2008 ~ 1, data = DMarihuana_2008ONLY.p.female, type='fh')
np.fit.ic.p.female_2011ONLY <- survfit(surv_female_2011 ~ 1, data = DMarihuana_2011ONLY.p.female, type='fh')
np.fit.ic.p.female_2016ONLY <- survfit(surv_female_2016 ~ 1, data = DMarihuana_2016ONLY.p.female, type='fh')
# extracting log cum haz
# -log(S) = H
Ha_female_1998ONLY <- -log(np.fit.ic.p.female_1998ONLY$surv)
Ha_female_1998ONLY_df <- data.frame(CumHaz =Ha_female_1998ONLY)
Ha_female_1998ONLY_df$Period   <- "1998"
Ha_female_1998ONLY_df$Sex      <- "Female"
Ha_female_1998ONLY_df$Init.Age <- np.fit.ic.p.female_1998ONLY$time

Ha_female_2002ONLY <- -log(np.fit.ic.p.female_2002ONLY$surv)
Ha_female_2002ONLY_df <- data.frame(CumHaz =Ha_female_2002ONLY)
Ha_female_2002ONLY_df$Period   <- "2002"
Ha_female_2002ONLY_df$Sex      <- "Female"
Ha_female_2002ONLY_df$Init.Age <- np.fit.ic.p.female_2002ONLY$time

Ha_female_2008ONLY <- -log(np.fit.ic.p.female_2008ONLY$surv)
Ha_female_2008ONLY_df <- data.frame(CumHaz =Ha_female_2008ONLY)
Ha_female_2008ONLY_df$Period         <-"2008"
Ha_female_2008ONLY_df$Sex      <-"Female"
Ha_female_2008ONLY_df$Init.Age    <- np.fit.ic.p.female_2008ONLY$time

Ha_female_2011ONLY <- -log(np.fit.ic.p.female_2011ONLY$surv)
Ha_female_2011ONLY_df <- data.frame(CumHaz =Ha_female_2011ONLY)
Ha_female_2011ONLY_df$Period         <-"2011"
Ha_female_2011ONLY_df$Sex      <-"Female"
Ha_female_2011ONLY_df$Init.Age    <- np.fit.ic.p.female_2011ONLY$time

Ha_female_2016ONLY <- -log(np.fit.ic.p.female_2016ONLY$surv)
Ha_female_2016ONLY_df <- data.frame(CumHaz =Ha_female_2016ONLY)
Ha_female_2016ONLY_df$Period        <-"2016"
Ha_female_2016ONLY_df$Sex      <-"Female"
Ha_female_2016ONLY_df$Init.Age    <- np.fit.ic.p.female_2016ONLY$time

Cum_haz_female_period <- rbind(Ha_female_1998ONLY_df,
                               Ha_female_2002ONLY_df,
                               Ha_female_2008ONLY_df,
                               Ha_female_2011ONLY_df,
                               Ha_female_2016ONLY_df)

# Anderson plot
plot( log(np.fit.ic.p.female_1998ONLY$time), log(Ha_female_1998ONLY), type = "s", ylim =c(-10.5,0),  xlim =c(0,5),xlab = "log(Age)", ylab = "Log H(t)", lty = 5,
      main = "Anderson Plot-Proportionality Test of Period, for Females"
      , sub= "Log Cumulative Hazard vs Log Age by Sex and Period")
lines(log(np.fit.ic.p.female_2002ONLY$time),log(Ha_female_2002ONLY), type = "s",lty = 5, col = "blue")
lines(log(np.fit.ic.p.female_2008ONLY$time),log(Ha_female_2008ONLY), type = "s",lty = 5, col = "red")
lines(log(np.fit.ic.p.female_2011ONLY$time),log(Ha_female_2011ONLY), type = "s",lty = 5, col = "green")
lines(log(np.fit.ic.p.female_2016ONLY$time),log(Ha_female_2016ONLY), type = "s",lty = 5, col = "yellow")
abline(v=log(16), col = "gray", lty = 3)
abline(v=log(20), col = "gray", lty = 3)
legend("topleft", legend=c( "2016","2011","2008","2002", "1998"),
       col=c("yellow","green","red","blue", "black"), lty=5, cex=0.8)
# ----------------------------------------------------------------------------- #
## 5.2 STRATA 2 | Male  ----
# Subsets by period, males
DMarihuana_1998ONLY.p.male <- filter(DMarihuana_1998_2016.p.male, Year=="1998")
DMarihuana_2002ONLY.p.male <- filter(DMarihuana_1998_2016.p.male, Year=="2002")
DMarihuana_2008ONLY.p.male <- filter(DMarihuana_1998_2016.p.male, Year=="2008")
DMarihuana_2011ONLY.p.male <- filter(DMarihuana_1998_2016.p.male, Year=="2011")
DMarihuana_2016ONLY.p.male <- filter(DMarihuana_1998_2016.p.male, Year=="2016")
# Surv objects by period
surv_male_1998 <-  Surv(time  = DMarihuana_1998ONLY.p.male$AgeL_Marihuana, 
                                              time2 = DMarihuana_1998ONLY.p.male$AgeR_Marihuana, 
                                              event = DMarihuana_1998ONLY.p.male$Event, 
                                              type  = "interval")

surv_male_2002 <-  Surv(time  = DMarihuana_2002ONLY.p.male$AgeL_Marihuana, 
                        time2 = DMarihuana_2002ONLY.p.male$AgeR_Marihuana, 
                        event = DMarihuana_2002ONLY.p.male$Event, 
                        type  = "interval")

surv_male_2008 <-  Surv(time  = DMarihuana_2008ONLY.p.male$AgeL_Marihuana, 
                        time2 = DMarihuana_2008ONLY.p.male$AgeR_Marihuana, 
                        event = DMarihuana_2008ONLY.p.male$Event, 
                        type  = "interval")

surv_male_2011 <-  Surv(time  = DMarihuana_2011ONLY.p.male$AgeL_Marihuana, 
                        time2 = DMarihuana_2011ONLY.p.male$AgeR_Marihuana, 
                        event = DMarihuana_2011ONLY.p.male$Event, 
                        type  = "interval")

surv_male_2016 <-  Surv(time  = DMarihuana_2016ONLY.p.male$AgeL_Marihuana, 
                        time2 = DMarihuana_2016ONLY.p.male$AgeR_Marihuana, 
                        event = DMarihuana_2016ONLY.p.male$Event, 
                        type  = "interval")
# Cum hazards
np.fit.ic.p.male_1998ONLY <- survfit(surv_male_1998 ~ 1, data = DMarihuana_1998ONLY.p.male, type='fh')
np.fit.ic.p.male_2002ONLY <- survfit(surv_male_2002 ~ 1, data = DMarihuana_2002ONLY.p.male, type='fh')
np.fit.ic.p.male_2008ONLY <- survfit(surv_male_2008 ~ 1, data = DMarihuana_2008ONLY.p.male, type='fh')
np.fit.ic.p.male_2011ONLY <- survfit(surv_male_2011 ~ 1, data = DMarihuana_2011ONLY.p.male, type='fh')
np.fit.ic.p.male_2016ONLY <- survfit(surv_male_2016 ~ 1, data = DMarihuana_2016ONLY.p.male, type='fh')
# extracting log cum haz
# -log(S) = H
Ha_male_1998ONLY <- -log(np.fit.ic.p.male_1998ONLY$surv)
Ha_male_1998ONLY_df <- data.frame(CumHaz =Ha_male_1998ONLY)
Ha_male_1998ONLY_df$Period <- "1998"
Ha_male_1998ONLY_df$Sex    <- "Male"
Ha_male_1998ONLY_df$Init.Age <- np.fit.ic.p.male_1998ONLY$time

Ha_male_2002ONLY <- -log(np.fit.ic.p.male_2002ONLY$surv)
Ha_male_2002ONLY_df <- data.frame(CumHaz =Ha_male_2002ONLY)
Ha_male_2002ONLY_df$Period <- "2002"
Ha_male_2002ONLY_df$Sex  <- "Male"
Ha_male_2002ONLY_df$Init.Age <- np.fit.ic.p.male_2002ONLY$time

Ha_male_2008ONLY <- -log(np.fit.ic.p.male_2008ONLY$surv)
Ha_male_2008ONLY_df <- data.frame(CumHaz =Ha_male_2008ONLY)
Ha_male_2008ONLY_df$Period <- "2008"
Ha_male_2008ONLY_df$Sex <- "Male"
Ha_male_2008ONLY_df$Init.Age <- np.fit.ic.p.male_2008ONLY$time

Ha_male_2011ONLY <- -log(np.fit.ic.p.male_2011ONLY$surv)
Ha_male_2011ONLY_df <- data.frame(CumHaz =Ha_male_2011ONLY)
Ha_male_2011ONLY_df$Period <- "2011"
Ha_male_2011ONLY_df$Sex    <- "Male"
Ha_male_2011ONLY_df$Init.Age <- np.fit.ic.p.male_2011ONLY$time

Ha_male_2016ONLY <- -log(np.fit.ic.p.male_2016ONLY$surv)
Ha_male_2016ONLY_df <- data.frame(CumHaz =Ha_male_2016ONLY)
Ha_male_2016ONLY_df$Period <- "2016" 
Ha_male_2016ONLY_df$Sex    <- "Male"
Ha_male_2016ONLY_df$Init.Age <- np.fit.ic.p.male_2016ONLY$time

Cum_haz_male_period <- rbind(Ha_male_1998ONLY_df,
      Ha_male_2002ONLY_df,
      Ha_male_2008ONLY_df,
      Ha_male_2011ONLY_df,
      Ha_male_2016ONLY_df)
# Anderson Plot
plot( log(np.fit.ic.p.male_1998ONLY$time), log(Ha_male_1998ONLY), type = "s", ylim =c(-10.5,0),  xlim =c(0,5),xlab = "Log(Age)", ylab = "Log H(t)", lty = 5,
      main = "Anderson Plot-Proportionality Test of Period, for Males"
      , sub= "Log Cumulative Hazard vs Log Age by Sex and Period")
lines(log(np.fit.ic.p.male_2002ONLY$time),log(Ha_male_2002ONLY), type = "s",lty = 5, col = "blue")
lines(log(np.fit.ic.p.male_2008ONLY$time),log(Ha_male_2008ONLY), type = "s",lty = 5, col = "red")
lines(log(np.fit.ic.p.male_2011ONLY$time),log(Ha_male_2011ONLY), type = "s",lty = 5, col = "green")
lines(log(np.fit.ic.p.male_2016ONLY$time),log(Ha_male_2016ONLY), type = "s",lty = 5, col = "yellow")
abline(v=log(16), col = "gray", lty = 3)
abline(v=log(20), col = "gray", lty = 3)
legend("topleft", legend=c( "2016","2011","2008","2002", "1998"),
       col=c("yellow","green","red","blue", "black"), lty=5, cex=0.8)
# ----------------------------------------------------------------------------- #
## 5.3 STRATA 1 AND 2 ----
Cum_haz_all_period <- rbind(Cum_haz_female_period ,Cum_haz_male_period )%>% 
  group_by(Period, Sex)%>%mutate( Haz =  c(0,diff(CumHaz)))

plot_all_cumhaz_period <- ggplot(data = Cum_haz_all_period, aes(x = (Init.Age), y = log(CumHaz), color= Period, fill = Period)) + 
  geom_step(linetype="solid", size=0.4)+
  geom_point(alpha =0.5)+
  #geom_vline(xintercept = c(16,30), size =0.2, lty =2)+
  labs(title = "Anderson Plot: Log Cumulative Rates of Onset (Kaplan-Meier) vs Age by Sex and Period "
  )+    
  xlab("Age (years)") +
  ylab(bquote("Log Cumulative Rates of Onset")) +
  ylim(c(0, 0.035*(1000)))+
  #xlim(c(0,65))+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.key.size = unit(.7, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 18))+
  scale_x_continuous(breaks = number_ticks(13)) +
  scale_y_continuous(breaks = number_ticks(13)) +
  scale_colour_manual(values = inferno(6, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(6, alpha = 1, begin = 0, end = 1, direction = 1))+
  facet_wrap(~Sex)

plot_all_cumhaz_period
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# 6. SURVIVAL ANALYSIS (NON-PARAMETRIC) ----
# Notes: Kaplan-Meier based estimates of Hazards            
## 6.2 STRATA 1 | Male    ----

df_surv.p.male <- data.frame(Period = c(rep("1998",     np.fit.ic.p.male$strata[1]) ,
                                            rep("2002",     np.fit.ic.p.male$strata[2]) ,
                                            rep("2008",     np.fit.ic.p.male$strata[3]) ,
                                            rep("2011",     np.fit.ic.p.male$strata[4]) ,
                                            rep("2016",     np.fit.ic.p.male$strata[5])),
                           Sex = c(         rep("Male",   np.fit.ic.p.male$strata[1]) , 
                                            rep("Male",   np.fit.ic.p.male$strata[2]) , 
                                            rep("Male",   np.fit.ic.p.male$strata[3]) , 
                                            rep("Male",   np.fit.ic.p.male$strata[4]) ,
                                            rep("Male",   np.fit.ic.p.male$strata[5])),
                           Init.Age  =   np.fit.ic.p.male$time, 
                           `At Risk` = as.integer(np.fit.ic.p.male$n.risk), 
                           `Events`  = as.integer(np.fit.ic.p.male$n.event), 
                           S_t = np.fit.ic.p.male$surv,
                           LB  = np.fit.ic.p.male$lower,
                           UB  = np.fit.ic.p.male$upper,
                           F_t = 1-np.fit.ic.p.male$surv, 
                           H_t = NA, # ignoring for now
                           check.names = FALSE)
# Computing h(t), f(t) 
# h(t): hazard, H(t), cumulative hazard which can be obtained as the -log transformation of the survival function
df_surv.p.male <- df_surv.p.male %>%
  group_by(Period) %>%
  mutate(f_t = c(F_t[1], diff(F_t)),
         h_t = f_t/S_t,
         H_t = -log(S_t))%>%
  dplyr::select(Period, Init.Age, `At Risk`, Events, S_t, LB, UB, F_t, H_t, h_t, f_t)
# ----------------------------------------------------------------------------- #
# Quick Check on Age of peak Hazard 
head(df_surv.p.male)
df_surv.p.male<- filter(df_surv.p.male, h_t < .249)
age.peak.male.np <- df_surv.p.male%>%
  dplyr::group_by(Period) %>%
  dplyr::summarize(rate.max = max(h_t))
age.peak.male.np<- age.peak.male.np%>%
  mutate(ageofmaxrate =df_surv.p.male$Init.Age[which(df_surv.p.male$h_t %in% age.peak.male.np$rate.max)] )
age.peak.male.np
# ----------------------------------------------------------------------------- #
# Survival Curve Plot 
require(dampack) # we need it for the #ticks
plot1.km.surv.male <-ggplot(df_surv.p.male, aes(x = Init.Age, y = S_t, color = Period))+
  labs(title="Panel-A: Kaplan-Meier Survival Estimates by Period for Males",
       subtitle = "95% Confidence Intervals") +
  labs(y="Survival Probabilities", x = "Age")+
  geom_linerange(aes(ymin = LB, ymax = UB), size = .5,alpha =0.8)+
  geom_step(aes( y = S_t), size=.8)+
  ylim(c(0.75,1))+
  scale_x_continuous(breaks = number_ticks(13)) +
  theme_bw()
plot1.km.surv.male
# Hazard Curve Plot
plot2.km.haz.male<-ggplot(df_surv.p.male, aes(x = Init.Age, y = h_t, color = Period)) +
  labs(title="Kaplan-Meier Initiation Rates Estimates by Period for Males") +
  labs(y=" Rate of Cannabis Initiation, h(a)", x = "Age")+
  geom_point(alpha=0.7)+
  #geom_line( size =0.5)+
  ylim(c(0,0.037))+
  scale_x_continuous(breaks = number_ticks(13)) +
  theme_bw()
plot2.km.haz.male
# ----------------------------------------------------------------------------- #
## 6.1 STRATA 1 | Female  ----
df_surv.p.female <- data.frame(Period = c(rep("1998",     np.fit.ic.p.female$strata[1]) ,
                                          rep("2002",     np.fit.ic.p.female$strata[2]) ,
                                          rep("2008",     np.fit.ic.p.female$strata[3]) ,
                                          rep("2011",     np.fit.ic.p.female$strata[4]) ,
                                          rep("2016",     np.fit.ic.p.female$strata[5])),
                             Sex = c(         rep("Female",   np.fit.ic.p.female$strata[1]) , 
                                              rep("Female",   np.fit.ic.p.female$strata[2]) , 
                                              rep("Female",   np.fit.ic.p.female$strata[3]) , 
                                              rep("Female",   np.fit.ic.p.female$strata[4]) ,
                                              rep("Female",   np.fit.ic.p.female$strata[5])),
                             Init.Age  =   np.fit.ic.p.female$time, 
                             `At Risk` = as.integer(np.fit.ic.p.female$n.risk), 
                             `Events`  = as.integer(np.fit.ic.p.female$n.event), 
                             S_t = np.fit.ic.p.female$surv,
                             LB  = np.fit.ic.p.female$lower,
                             UB  = np.fit.ic.p.female$upper,
                             F_t = 1-np.fit.ic.p.female$surv, 
                             H_t = NA, # ignoring for now
                             check.names = FALSE)
# Computing h(t), f(t) 
# h(t): hazard, H(t), cumulative hazard which can be obtained as the -log transformation of the survival function
df_surv.p.female <- df_surv.p.female %>%
  group_by(Period) %>%
  mutate(f_t = c(F_t[1], diff(F_t)),
         h_t = f_t/S_t,
         H_t = -log(S_t))%>%
  dplyr::select(Period, Init.Age, `At Risk`, Events, S_t, LB, UB, F_t, H_t, h_t, f_t)
# ----------------------------------------------------------------------------- #
# Quick Check on Age of peak Hazard, 
age.peak.female.np <- df_surv.p.female%>%
  group_by(Period) %>%
  dplyr::summarize(rate.max = max(h_t))

age.peak.female.np<-age.peak.female.np%>%
  mutate(ageofmaxrate =df_surv.p.female$Init.Age[which(df_surv.p.female$h_t %in% age.peak.female.np$rate.max)] )
age.peak.female.np
# ----------------------------------------------------------------------------- #
# Survival Curve Plot 
plot1.km.surv.female <-ggplot(df_surv.p.female, aes(x = Init.Age, color = Period ))+
  labs(title="Panel-B: Kaplan-Meier Survival Estimates by Period for Females",
       subtitle = "95% Confidence Intervals") +
  labs(y="Survival Probabilities", x = "Age")+
  geom_linerange(aes(ymin = LB, ymax = UB), size = .5,alpha =0.8)+
  geom_step(aes( y = S_t), size=.8)+
  ylim(c(0.75,1))+
  #scale_x_continuous(breaks = number_ticks(13)) +
  theme_bw()
plot1.km.surv.female
# Hazard Curve Plot
plot2.km.haz.female<-ggplot(df_surv.p.female, aes(x = Init.Age, y = h_t, color = Period)) +
  labs(title="Kaplan-Meier Initiation Rates Estimates by Period for Females") +
  labs(y=" Rate of Cannabis Initiation, h(a)", x = "Age")+
  geom_point( alpha=0.3)+
  geom_line( size =0.5)+
  ylim(c(0,0.037))+
  #scale_x_continuous(breaks = number_ticks(13)) +
  theme_bw()
plot2.km.haz.female
#Saving km plots
save(plot1.km.surv.female, plot2.km.haz.female, file="km.plots.period.female.RData")
## 6.3 Facet plot all Strata ----
df_surv.p.female$Sex <- "Female"
df_surv.p.male$Sex <- "Male"
df_surv_all_p <- rbind(df_surv.p.female, df_surv.p.male)
# --- Plots for Manuscript ---- 
plot_surv_all_cohort <-ggplot(df_surv_all_c, aes(x = Init.Age, color = Cohort ))+
  labs(title=""
  ) +
  labs(y="Probability of not Using Cannabis", x = "Age")+
  geom_linerange(aes(ymin = LB, ymax = UB), size = .5,alpha =0.8)+
  geom_step(aes( y = S_t), size=.8)+
  ylim(c(0.5,1))+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.key.size = unit(.75, "cm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        #legend.key.width =   unit(.5, "cm"),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=16),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold", size = 16),
        #strip.wrap =  unit(.15, "cm")
  )+
  guides(colour = guide_legend(nrow = 1))+
  scale_x_continuous(breaks = seq(0,65,5)) +
  #scale_y_continuous(breaks = seq(0,55,5)) +
  scale_colour_manual(values = magma(7, alpha = 1, begin = 0.1, end = .7, direction = 1))+
  scale_fill_manual(values = magma(7, alpha = 1, begin = 0.1, end = .7, direction = 1))+
  facet_wrap(~Sex)

plot_surv_all_cohort
# Combined Plot
KM_plot_all <- (plot_surv_all_period /plot_surv_all_cohort) + plot_annotation(tag_levels = list(c("(A) by Period","(B) by Decennial Birth Cohort"))) & 
  theme(plot.tag = element_text(size = 22, hjust = 0, vjust = 0, face = "bold"),
        plot.tag.position = c(0,.98))

KM_plot_all

ggsave(
  "KM_plot_all.png",
  plot = KM_plot_all,
  #device = "png",
  #path = "~/HR_plots",
  scale = 1,
  width = 10,
  height = 10,
  units = "in",
  dpi = 300,
  limitsize = F
)