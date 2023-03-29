#Here I replicate the analysis but using period as the linear predictor on the splines and the km estimation

# Using subsets of the data split into the two sex categories: Male, Female
# The reason is that survivial times seem to distribute quite differently for each population
# We get better fits using this analysis,but consider we are using less data


# ---------------------------------------------------------------------------------------------------------- #
# ======================== SPLINE FITTING : SURVIVAL AND HAZARD FUNCTIONS (Cohort) ========================= #
# DATA: interval censored and right censored (1998-2016)
# ANALYSIS: include  predictors (Period) 
#   KM:  Non-parametric survival and hazard estimates through kaplan-meier estimator
#   SPLINES: different specifications on the splines 
#   OUTPUT: lists, containing fit objects from spline models + AIC and Coefficients for each specification
#   PLOTS: for KM estimates, Splines, comparison of hazards KM vs Splines
# AUTHOR: MLM- NOV, 26/2019
# ---------------------------------------------------------------------------------------------------------- #

#Load libraries

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
# ---------------------------------------------------------------------------------------------------------- #

# LOADING DATA  ============================= #
# =========================================== #

# Interval and Right-Censored Data (1998-2016)
DMarihuana_1998_2016 <- read_excel("C:/Users/mlopezme/Dropbox (Brown)/Troubleshooting-Parametric Models and Splines/DS_SURV_MARIHUANA_1998_2016.xlsx")
DMarihuana_1998_2016$Sex  <- factor(DMarihuana_1998_2016$Sex, labels = c("Male", "Female"))
head(DMarihuana_1998_2016)
nrow(DMarihuana_1998_2016)#, total observations
sum(1- as.numeric(is.na(DMarihuana_1998_2016$Event)))#, censored observations
# Male population
DMarihuana_1998_2016.p.male <- DMarihuana_1998_2016%>%
  filter(Sex == "Male")
nrow(DMarihuana_1998_2016.p.male) #61658
str(DMarihuana_1998_2016.p.male)
# Female population
DMarihuana_1998_2016.p.female <- DMarihuana_1998_2016%>%
  filter(Sex == "Female")
nrow(DMarihuana_1998_2016.p.female) #79684

# LOG-RANK TEST ----------------------------------#
DATA_02_16 <- filter(DMarihuana_1998_2016, Year!="1998")
SURV_ALL <- Surv(time  = DATA_02_16 $AgeR_Marihuana, 
     event = DATA_02_16 $Event, 
     type  = "right")

plot(survfit(SURV_ALL ~ factor(Sex) + factor(Year), data = DATA_02_16))

logrank_period <- survdiff(SURV_ALL ~   factor(Sex) + factor(Year), data = DATA_02_16)
logrank_period

#write.xlsx(logrank_period, file ="/Users/mauriciolopezm/Dropbox (Brown)/Troubleshooting-Parametric Models and Splines/Tables/tables_logrank_period.xlsx")


# str(logrank)
# logrank$strata
# DATA_02_16$Sex*DATA_02_16$Year
# fit_not98<-survfit(SURV_ALL ~ Sex + factor(Year), data = DATA_02_16)
# summary(fit_not98)$cumhaz
# plot(survfit( Surv(time  = DMarihuana_1998_2016$AgeL_Marihuana, 
#                    time2 = DMarihuana_1998_2016$AgeR_Marihuana, 
#                    event = DMarihuana_1998_2016$Event, 
#                    type  = "interval") ~ Sex + factor(Year), data = DMarihuana_1998_2016))
# install.packages("FHtest")
# library("FHtest")
# FHtesticp(Surv(left, right, type = "interval2") ~ zgen, data = duser,
#             + Lin = TRUE, Rin = TRUE, icontrol = icfitControl(maxit = 50000))
# 
# 
# survdiff(Surv(time  = DMarihuana_1998_2016$AgeL_Marihuana, 
#               time2 = DMarihuana_1998_2016$AgeR_Marihuana, 
#               event = DMarihuana_1998_2016$Event, 
#               type  = "interval") ~ Sex + factor(Year), data = DMarihuana_1998_2016)


# SURVIVAL OBJECTS  ============================= #
# =========================================== #
#all
my.surv.ic.all <- Surv(time  = DMarihuana_1998_2016$AgeL_Marihuana,
                          time2 = DMarihuana_1998_2016$AgeR_Marihuana,
                          event = DMarihuana_1998_2016$Event,
                          type  = "interval")
head(my.surv.ic.all)
np.fit.ic.all <- survfit(my.surv.ic.all ~ Sex, data = DMarihuana_1998_2016)
plot(np.fit.ic.all)
table(DMarihuana_1998_2016$Sex)
df_surv.all <- data.frame(Sex = c(rep("Male",     np.fit.ic.all$strata[1]) ,
                                        rep("Female",     np.fit.ic.all$strata[2])) ,
                             Init.Age  =   np.fit.ic.all$time, 
                             `At Risk` = as.integer(np.fit.ic.all$n.risk), 
                             `Events`  = as.integer(np.fit.ic.all$n.event), 
                             S_t = np.fit.ic.all$surv,
                             LB  = np.fit.ic.all$lower,
                             UB  = np.fit.ic.all$upper,
                             F_t = 1-np.fit.ic.all$surv, 
                             H_t = NA, # ignoring for now
                             check.names = FALSE)
summary(df_surv.all)
head(df_surv.all)

# ADDING h(t), f(t) ##

df_surv.all <- df_surv.all %>%
  group_by(Sex) %>%
  mutate(f_t = c(F_t[1], diff(F_t)),
         h_t = f_t/S_t,
         H_t = -log(S_t))%>%
  dplyr::select(Sex, Init.Age, `At Risk`, Events, S_t, LB, UB, F_t, H_t, h_t, f_t)

head(df_surv.all)

plot1.km.surv.all <-ggplot(df_surv.all, aes(x = Init.Age, y = S_t, color = Sex))+
  labs(title="Panel-A: Kaplan-Meier Crude Survival Estimates by Sex",
       subtitle = "95% Confidence Intervals") +
  labs(y="Survival Probabilities", x = "Age")+
  geom_linerange(aes(ymin = LB, ymax = UB), size = .5,alpha =0.8)+
  geom_step(aes( y = S_t), size=.8)+
  ylim(c(0.75,1))+
  #scale_x_continuous(breaks = number_ticks(13)) +
  theme_bw()
plot1.km.surv.all
#hazard function

plot2.km.haz.all<-ggplot(df_surv.all, aes(x = Init.Age, y = h_t, color = Sex)) +
  labs(title=" Crude Rates of Onset Estimates by Sex") +
  labs(y=" Rate of Cannabis Initiation, h(a)", x = "Age")+
  geom_line(alpha=0.7)+
  geom_point(alpha=0.7)+
  #geom_line( size =0.5)+
  ylim(c(0,0.04))+
  #scale_x_continuous(breaks = number_ticks(13)) +
  theme_bw()
plot2.km.haz.all

# cum haz function
plot3.km.haz.all<-ggplot(df_surv.all, aes(x = Init.Age, y = H_t, color = Sex)) +
  labs(title=" Cumulative Crude Rates of Onset Estimates by Sex") +
  labs(y=" Rate of Cannabis Initiation, h(a)", x = "Age")+
  geom_line(alpha=0.7)+
  geom_point(alpha=0.7)+
  #geom_line( size =0.5)+
  ylim(c(0,1))+
  #scale_x_continuous(breaks = number_ticks(13)) +
  theme_bw()
plot3.km.haz.all




# ----------------------------------------------- #
# male
my.surv.ic.p.male <- Surv(time  = DMarihuana_1998_2016.p.male$AgeL_Marihuana, 
                          time2 = DMarihuana_1998_2016.p.male$AgeR_Marihuana, 
                          event = DMarihuana_1998_2016.p.male$Event, 
                          type  = "interval")
head(my.surv.ic.p.male)

np.fit.ic.p.male <- survfit(my.surv.ic.p.male ~ Year, data = DMarihuana_1998_2016.p.male)

plot(np.fit.ic.p.male, main="Kaplan-Meier estimate with 95% confidence bounds",
     xlab="time", ylab="survival function")

# female
my.surv.ic.p.female <- Surv(time  = DMarihuana_1998_2016.p.female$AgeL_Marihuana, 
                            time2 = DMarihuana_1998_2016.p.female$AgeR_Marihuana, 
                            event = DMarihuana_1998_2016.p.female$Event, 
                            type  = "interval")
head(my.surv.ic.p.female)

np.fit.ic.p.female <- survfit(my.surv.ic.p.female ~ Year, data = DMarihuana_1998_2016.p.female)

plot(np.fit.ic.p.female, main="Kaplan-Meier estimate with 95% confidence bounds",
     xlab="time", ylab="survival function")

# Saving Surv objects and Data, by Cohort

model_inputs_period <- list(surv_objects = list(so_female_p=my.surv.ic.p.female, so_male_p=my.surv.ic.p.male), 
                            data_dfs     = list(df_frmale_p=DMarihuana_1998_2016.p.female, dF_male_p =DMarihuana_1998_2016.p.male))

#save(model_inputs_period, file = "/Users/mauriciolopezm/Dropbox (Brown)/Troubleshooting-Parametric Models and Splines/Code/Data_Objects/model_inputs_period.RData")

# ======================================================================== #
# ======================================================================== #
# ======================================================================== #

# TESTING PROPORTINALITY OF HAZARDS ACROSS PERIODS           ============= #
# ======================================================================== #
#MALES, SUBSETTING DATA BY PERIOD

colnames(DMarihuana_1998_2016.p.male)
unique(DMarihuana_1998_2016.p.male$Year)
DMarihuana_1998ONLY.p.male <- filter(DMarihuana_1998_2016.p.male, Year=="1998")
DMarihuana_2002ONLY.p.male <- filter(DMarihuana_1998_2016.p.male, Year=="2002")
DMarihuana_2008ONLY.p.male <- filter(DMarihuana_1998_2016.p.male, Year=="2008")
DMarihuana_2011ONLY.p.male <- filter(DMarihuana_1998_2016.p.male, Year=="2011")
DMarihuana_2016ONLY.p.male <- filter(DMarihuana_1998_2016.p.male, Year=="2016")

# ---------------------------------------Sample Sizes, Mean Age, Prop Females #
DMarihuana_1998 <- filter(DMarihuana_1998_2016, Year=="1998")
DMarihuana_2002 <- filter(DMarihuana_1998_2016, Year=="2002")
DMarihuana_2008 <- filter(DMarihuana_1998_2016, Year=="2008")
DMarihuana_2011 <- filter(DMarihuana_1998_2016, Year=="2011")
DMarihuana_2016 <- filter(DMarihuana_1998_2016, Year=="2016")

nrow(DMarihuana_1998_2016)
sum(is.na(DMarihuana_2016$Response))
table_survey_samples <- data.frame( N = rep(NA, 5), Prop_Female = rep(NA, 5), Mean_Age = rep(NA,5), SD_Age = rep(NA,5))
prop.table(table(DMarihuana_1998$Sex, useNA = "always"))
prop.table(table(DMarihuana_2002$Sex, useNA = "always"))
prop.table(table(DMarihuana_2008$Sex, useNA = "always"))
prop.table(table(DMarihuana_2011$Sex, useNA = "always"))
prop.table(table(DMarihuana_2016$Sex, useNA = "always"))

sum(is.na(DMarihuana_1998$Response))
sum(is.na(DMarihuana_2002$Response))
sum(is.na(DMarihuana_2008$Response))
sum(is.na(DMarihuana_2011$Response))

colnames(DMarihuana_1998)
table(DMarihuana_1998$Response, useNA = "always")
table(DMarihuana_2002$Response, useNA = "always")
table(DMarihuana_2008$Response, useNA = "always")
table(DMarihuana_2011$Response, useNA = "always")
table(DMarihuana_2016$Response, useNA = "always")

table(DMarihuana_1998$Age_Response, useNA = "always")


nrow(DMarihuana_1998_2016)
mean(DMarihuana_1998_2016$Age_Response)
sd(DMarihuana_1998_2016$Age_Response)
prop.table(table(DMarihuana_1998_2016$Sex, useNA = "always"))
table(DMarihuana_1998_2016$Sex, useNA = "always")

DMarihuana_1998_2016%>%
  group_by( Year) %>%
  summarise( N            = n(),
             Mean_Age    = mean(Age_Response),
             Sd_Age      = sd(Age_Response))
# --------------------------------------------------------------------------- #
# Surv objects
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
# Cum hazards, from naelson aelen

np.fit.ic.p.male_1998ONLY <- survfit(surv_male_1998 ~ 1, data = DMarihuana_1998ONLY.p.male, type='fh')
np.fit.ic.p.male_2002ONLY <- survfit(surv_male_2002 ~ 1, data = DMarihuana_2002ONLY.p.male, type='fh')
np.fit.ic.p.male_2008ONLY <- survfit(surv_male_2008 ~ 1, data = DMarihuana_2008ONLY.p.male, type='fh')
np.fit.ic.p.male_2011ONLY <- survfit(surv_male_2011 ~ 1, data = DMarihuana_2011ONLY.p.male, type='fh')
np.fit.ic.p.male_2016ONLY <- survfit(surv_male_2016 ~ 1, data = DMarihuana_2016ONLY.p.male, type='fh')

# extracting log cum haz
str(summary(np.fit.ic.p.male_1998ONLY))
summary(np.fit.ic.p.male_1998ONLY)
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


# Plots
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

#We observe proportionality for years 2008, 2011, 2016, after ages 10
#Porportionality is not clear for years 1998, 2002


# # Shoenfeld Residuals, Score test
# data_male_2002_2016<- filter(DMarihuana_1998_2016.p.male, Year !=1998)
# my.surv_data_male_2002_2016 <- Surv(time  = data_male_2002_2016$AgeR_Marihuana, 
#         #time2 = data_male_2002_2016$AgeR_Marihuana, 
#         event = data_male_2002_2016$Event, 
#         type  = "right")
# 
# mod <-coxph(my.surv_data_male_2002_2016  ~ Year, data =data_male_2002_2016 , method="breslow")
# std.cum.res <-sqrt(diag(mod$var))*(cumsum(resid(mod, type="schoenfeld")))
# plot(sort(data_male_2002_2016$AgeR_Marihuana[data_male_2002_2016$Event==1]), std.cum.res,type= "l", 
#      ylim =c(-10,2),
#      xlab = "Time in Weeks (for Events)",
#      ylab = "Standardized Score Resid")
# abline(h =c(-1.3581, 1.3581), col = "darkgray" )


# the plot shows that we reject the proportionality of hazards at .95 confidence, even excluding 1998

# --------------------------------------
#FEMALES, SUBSETTING DATA BY PERIOD

colnames(DMarihuana_1998_2016.p.female)
unique(DMarihuana_1998_2016.p.female$Year)
DMarihuana_1998ONLY.p.female <- filter(DMarihuana_1998_2016.p.female, Year=="1998")
DMarihuana_2002ONLY.p.female <- filter(DMarihuana_1998_2016.p.female, Year=="2002")
DMarihuana_2008ONLY.p.female <- filter(DMarihuana_1998_2016.p.female, Year=="2008")
DMarihuana_2011ONLY.p.female <- filter(DMarihuana_1998_2016.p.female, Year=="2011")
DMarihuana_2016ONLY.p.female <- filter(DMarihuana_1998_2016.p.female, Year=="2016")

# Surv objects
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
str(summary(np.fit.ic.p.female_1998ONLY))
summary(np.fit.ic.p.female_1998ONLY)
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

Cum_haz_all_period <- rbind(Cum_haz_female_period ,Cum_haz_male_period )%>% 
  group_by(Period, Sex)%>%mutate( Haz =  c(0,diff(CumHaz)))
ggplot(data=Cum_haz_all_period, aes(x=Init.Age, y=Haz, color = Period, shape = Period))+geom_point()+geom_line()+ geom_vline(xintercept = c(12,15))+ facet_wrap(~Sex, scales = "free_y")

# ANDERSON PLOTS

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



# Plots
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

#We observe proportionality for years 2002, 2008, 2011, 2016, after ages 16
#Porportionality is not clear for year 1998



# ======================================================================== #
# ======================================================================== #
# ======================================================================== #
# ======================================================================== #

# Kaplan-Meier estimates (Non-parametric)                    ============= #
# ======================================================================== #

# Male

# KM SURV and HAZARD ESTIMATES

# Interval and Right-Censored Data (1998-2016)
#str(summary(np.fit.ic.p.male))    # note the # of strata: 5 x 2 = 10


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
summary(df_surv.p.male)
head(df_surv.p.male)

# ADDING h(t), f(t) ##

df_surv.p.male <- df_surv.p.male %>%
  group_by(Period) %>%
  mutate(f_t = c(F_t[1], diff(F_t)),
         h_t = f_t/S_t,
         H_t = -log(S_t))%>%
  dplyr::select(Period, Init.Age, `At Risk`, Events, S_t, LB, UB, F_t, H_t, h_t, f_t)

head(df_surv.p.male)
# ---------------------------------------------------------#

# Kaplan-Meier Plots (Non-parametric) MALES ============= #
# ===================================================== #

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
#hazard function
plot2.km.haz.male<-ggplot(df_surv.p.male, aes(x = Init.Age, y = h_t, color = Period)) +
  labs(title="Kaplan-Meier Initiation Rates Estimates by Period for Males") +
  labs(y=" Rate of Cannabis Initiation, h(a)", x = "Age")+
  geom_point(alpha=0.7)+
  #geom_line( size =0.5)+
  ylim(c(0,0.037))+
  scale_x_continuous(breaks = number_ticks(13)) +
  theme_bw()
plot2.km.haz.male

# --------------------------------- #
# Age of Peak Cannabis Initiation

head(df_surv.p.male)
df_surv.p.male<- filter(df_surv.p.male, h_t < .249)
age.peak.male.np <- df_surv.p.male%>%
  dplyr::group_by(Period) %>%
  dplyr::summarize(rate.max = max(h_t))
age.peak.male.np<- age.peak.male.np%>%
  mutate(ageofmaxrate =df_surv.p.male$Init.Age[which(df_surv.p.male$h_t %in% age.peak.male.np$rate.max)] )
age.peak.male.np
# --------------------------------------------------------------------#
#female

#str(summary(np.fit.ic.p.female))    # note the # of strata: 5 x 2 = 10


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
summary(df_surv.p.female)
head(df_surv.p.female)

# ADDING h(t), f(t) ##

df_surv.p.female <- df_surv.p.female %>%
  group_by(Period) %>%
  mutate(f_t = c(F_t[1], diff(F_t)),
         h_t = f_t/S_t,
         H_t = -log(S_t))%>%
  dplyr::select(Period, Init.Age, `At Risk`, Events, S_t, LB, UB, F_t, H_t, h_t, f_t)

head(df_surv.p.female)

# ---------------------------------------------------------------------------------------------------------- #
# Age of Peak Cannabis Initiation

age.peak.female.np <- df_surv.p.female%>%
  group_by(Period) %>%
  dplyr::summarize(rate.max = max(h_t))

age.peak.female.np<-age.peak.female.np%>%
  mutate(ageofmaxrate =df_surv.p.female$Init.Age[which(df_surv.p.female$h_t %in% age.peak.female.np$rate.max)] )
age.peak.female.np

# ---------------------------------------------------------------------------------------------------------- #

# ---------------------------------------------------------#

# Kaplan-Meier Plots (Non-parametric) FEMALES ======== #
# ===================================================== #

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
# hazard function

# facet plot, period
colnames(df_surv.p.female)
colnames(df_surv.p.male)
df_surv.p.female$Sex <- "Female"
df_surv.p.male$Sex <- "Male"
df_surv_all_p <- rbind(df_surv.p.female, df_surv.p.male)

plot_surv_all_period <-ggplot(df_surv_all_p, aes(x = Init.Age, color = Period ))+
  labs(title=""
       #subtitle = "95% Confidence Intervals"
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
  scale_colour_manual(values = magma(5, alpha = 1, begin = 0.1, end = .8, direction = 1))+
  scale_fill_manual(values = magma(5, alpha = 1, begin = 0.1, end = .8, direction = 1))+
  facet_wrap(~Sex)

plot_surv_all_period


# plot2.km.haz.female<-ggplot(df_surv.p.female, aes(x = Init.Age, y = h_t, color = Period)) +
#   labs(title="Kaplan-Meier Initiation Rates Estimates by Period for Females") +
#   labs(y=" Rate of Cannabis Initiation, h(a)", x = "Age")+
#   geom_point( alpha=0.3)+
#   geom_line( size =0.5)+
#   ylim(c(0,0.037))+
#   #scale_x_continuous(breaks = number_ticks(13)) +
#   theme_bw()
# plot2.km.haz.female
# #Saving km plots
# 
# #save(plot1.km.surv.female, plot2.km.haz.female, file="km.plots.period.female.RData")


# --- Plots for Manuscript ---- 
load( file = "df_surv_all_c.RData") # saving to plot together

plot_surv_all_cohort <-ggplot(df_surv_all_c, aes(x = Init.Age, color = Cohort ))+
  labs(title=""
       #subtitle = "95% Confidence Intervals"
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
unlink(KM_plot_all)
dev.off()











# ---------------------------------------------------------------------------------------------------------- #
# Spline Models (flexible -parametric) ============= #
# ===================================================== #
# MALES
# BEST FIT MODELS, BY KNOTS, BY SATURATED VS NOT SATURATED, BY QUADRATIC TERM, BY AGE-VARYING EFFECTS

#----------------------------------#
# not saturated, by year
sp.fit.mod1.ic.p.male.h_year_k1 <-                   flexsurvspline(my.surv.ic.p.male ~ ns(Year),
                                                        k = 1, scale = "hazard",
                                                        data = DMarihuana_1998_2016.p.male)
# does not converge
#sp.fit.mod1.ic.p.male.h_year_k1
#sp.fit.mod1.ic.p.male.h_year_k1$coefficients[6]
#sp.fit.mod1.ic.p.male.h_year_k1$AIC
 
#---#
sp.fit.mod1.ic.p.male.h_year_k2 <-                   flexsurvspline(my.surv.ic.p.male ~ ns(Year),
                                                        k = 2, scale = "hazard",
                                                        data = DMarihuana_1998_2016.p.male)
# 72097.18
sp.fit.mod1.ic.p.male.h_year_k2
sp.fit.mod1.ic.p.male.h_year_k2$coefficients[6]
sp.fit.mod1.ic.p.male.h_year_k2$AIC

#---#
sp.fit.mod1.ic.p.male.h_year_k3 <-                   flexsurvspline(my.surv.ic.p.male ~ ns(Year),
                                                        k = 3, scale = "hazard",
                                                        data = DMarihuana_1998_2016.p.male)
# 70187.48, third best fit, hazard scale
sp.fit.mod1.ic.p.male.h_year_k3
sp.fit.mod1.ic.p.male.h_year_k3$coefficients[6]
sp.fit.mod1.ic.p.male.h_year_k3$AIC
plot(sp.fit.mod1.ic.p.male.h_year_k3)
#---#
sp.fit.mod1.ic.p.male.h_year_k5 <-                   flexsurvspline(my.surv.ic.p.male ~ ns(Year),
                                                        k = 5, scale = "hazard",
                                                        data = DMarihuana_1998_2016.p.male)
# 70254.18
sp.fit.mod1.ic.p.male.h_year_k5
sp.fit.mod1.ic.p.male.h_year_k5$coefficients[6]
sp.fit.mod1.ic.p.male.h_year_k5$AIC

#----------------------------------#
# Proportional hazard, saturated
sp.fit.mod1.ic.p.male.h_sat_k1 <-                    flexsurvspline(my.surv.ic.p.male ~ factor(Year),
                                                        k = 1, scale = "hazard",
                                                        data = DMarihuana_1998_2016.p.male)
# -> does not converge

sp.fit.mod1.ic.p.male.h_sat_k2 <-                    flexsurvspline(my.surv.ic.p.male ~ factor(Year),
                                                        k = 2, scale = "hazard",
                                                        data = DMarihuana_1998_2016.p.male)
# -> does not converge

#---#
sp.fit.mod1.ic.p.male.h_sat_k3 <-                    flexsurvspline(my.surv.ic.p.male ~ factor(Year),
                                                        k = 3, scale = "hazard",
                                                        data = DMarihuana_1998_2016.p.male)
# -> DOES NOT CONVERGE

#---#
sp.fit.mod1.ic.p.male.h_sat_k5 <-                    flexsurvspline(my.surv.ic.p.male ~ factor(Year),
                                                        k = 5, scale = "hazard",
                                                        data = DMarihuana_1998_2016.p.male)
#-> DOES NOT CONVERGE

#----------------------------------#
# Proportional hazard, NOT SATURATED, ADDING AGE-VARYING EFFECTS, BY KNOTS

sp.fit.mod1.ic.p.male.h_AGEVARY_k1 <-                flexsurvspline(my.surv.ic.p.male ~ ns(Year) + gamma1(Year),
                                                                 k = 1, scale = "hazard",
                                                                 data = DMarihuana_1998_2016.p.male)
#-> does not converge


#---#
sp.fit.mod1.ic.p.male.h_AGEVARY_k2 <-                flexsurvspline(my.surv.ic.p.male ~ ns(Year) + gamma1(Year)+gamma2(Year),
                                                                    k = 2, scale = "hazard",
                                                                    data = DMarihuana_1998_2016.p.male)

#-> AIC = 74217.6

sp.fit.mod1.ic.p.male.h_AGEVARY_k2
sp.fit.mod1.ic.p.male.h_AGEVARY_k2$coefficients[6]
sp.fit.mod1.ic.p.male.h_AGEVARY_k2$AIC


#---#
sp.fit.mod1.ic.p.male.h_AGEVARY_k3 <-                flexsurvspline(my.surv.ic.p.male ~ ns(Year) + gamma1(Year) + gamma2(Year)  ,
                                                                 k = 3, scale = "hazard",
                                                                 data = DMarihuana_1998_2016.p.male)
#AIC = 70206.14
sp.fit.mod1.ic.p.male.h_AGEVARY_k3
sp.fit.mod1.ic.p.male.h_AGEVARY_k3$coefficients[6]
sp.fit.mod1.ic.p.male.h_AGEVARY_k3$AIC

#---#
sp.fit.mod1.ic.p.male.h_AGEVARY_k5 <-                flexsurvspline(my.surv.ic.p.male ~ ns(Year)+ gamma1(Year) + gamma2(Year) ,
                                                                 k = 5, scale = "hazard",
                                                                 data = DMarihuana_1998_2016.p.male)
# AIC = 70184.99, second  best fit , hazard scale
sp.fit.mod1.ic.p.male.h_AGEVARY_k5
sp.fit.mod1.ic.p.male.h_AGEVARY_k5$coefficients[6]
sp.fit.mod1.ic.p.male.h_AGEVARY_k5$AIC

plot(sp.fit.mod1.ic.p.male.h_AGEVARY_k5)
#---#
sp.fit.mod1.ic.p.male.h_AGEVARY_k6 <-                flexsurvspline(my.surv.ic.p.male ~ ns(Year)+ gamma1(Year) + gamma2(Year) ,
                                                                    k = 6, scale = "hazard",
                                                                    data = DMarihuana_1998_2016.p.male)
# AIC = 70623.54
sp.fit.mod1.ic.p.male.h_AGEVARY_k6
sp.fit.mod1.ic.p.male.h_AGEVARY_k6$coefficients[6]
sp.fit.mod1.ic.p.male.h_AGEVARY_k6$AIC

#----------------------------------#

# Proportional hazard, NOT SATURATED, ADDING QUADRATIC TERM, BY KNOTS

sp.fit.mod1.ic.p.male.h_quadratic_k1 <-              flexsurvspline(my.surv.ic.p.male ~ ns(Year)+ ns(Year*Year),
                                                              k = 1, scale = "hazard",
                                                              data = DMarihuana_1998_2016.p.male)
# -> does not converge
#---#
sp.fit.mod1.ic.p.male.h_quadratic_k2 <-              flexsurvspline(my.surv.ic.p.male ~ ns(Year)+ ns(Year*Year),
                                                                    k = 2, scale = "hazard",
                                                                    data = DMarihuana_1998_2016.p.male)
# -> does not converge
#---#
sp.fit.mod1.ic.p.male.h_quadratic_k3 <-              flexsurvspline(my.surv.ic.p.male ~ ns(Year)+ ns(Year*Year),
                                                              k = 3, scale = "hazard",
                                                              data = DMarihuana_1998_2016.p.male)
# does not converge
#---#
sp.fit.mod1.ic.p.male.h_quadratic_k5 <-              flexsurvspline(my.surv.ic.p.male ~ ns(Year)+ ns(Year*Year),
                                                              k = 5, scale = "hazard",
                                                              data = DMarihuana_1998_2016.p.male)
# does not converge
#----------------------------------#

# Proportional hazard, NOT SATURATED, ADDING AGE-VARYING EFFECTS, ADDING QUADRATIC TERM, BY KNOTS
sp.fit.mod1.ic.p.male.h_AGEVARY_quadratic_k1 <-      flexsurvspline(my.surv.ic.p.male ~ ns(Year) + gamma1(Year) + ns(Year*Year),
                                                          k = 1, scale = "hazard",
                                                          data = DMarihuana_1998_2016.p.male)
#-> does not converge

#---#
sp.fit.mod1.ic.p.male.h_AGEVARY_quadratic_k2 <-      flexsurvspline(my.surv.ic.p.male ~ ns(Year) + gamma1(Year) + gamma2(Year) + ns(Year*Year),
                                                          k = 2, scale = "hazard",
                                                          data = DMarihuana_1998_2016.p.male)

#-> does not converge

#---#
sp.fit.mod1.ic.p.male.h_AGEVARY_quadratic_k3 <-      flexsurvspline(my.surv.ic.p.male ~ ns(Year) + gamma1(Year) + gamma2(Year) + ns(Year*Year) ,
                                                          k = 3, scale = "hazard",
                                                          data = DMarihuana_1998_2016.p.male)
# does not converge
#---#
sp.fit.mod1.ic.p.male.h_AGEVARY_quadratic_k5 <-      flexsurvspline(my.surv.ic.p.male ~ ns(Year)+ gamma1(Year) + gamma2(Year) + ns(Year*Year) ,
                                                          k = 5, scale = "hazard",
                                                          data = DMarihuana_1998_2016.p.male)
# does not converge
#----------------------------------#
#-----------ODDS SCALE-----------------------#
sp.fit.mod1.ic.o.male.h_year_k1 <-                   flexsurvspline(my.surv.ic.p.male ~ ns(Year),
                                                                    k = 1, scale = "odds",
                                                                    data = DMarihuana_1998_2016.p.male)
#does not converge

#---#
sp.fit.mod1.ic.o.male.h_year_k2 <-                   flexsurvspline(my.surv.ic.p.male ~ ns(Year),
                                                                    k = 2, scale = "odds",
                                                                    data = DMarihuana_1998_2016.p.male)
#AIC = 73327.27
sp.fit.mod1.ic.o.male.h_year_k2
sp.fit.mod1.ic.o.male.h_year_k2$coefficients[6]
sp.fit.mod1.ic.o.male.h_year_k2$AIC


#---#
sp.fit.mod1.ic.o.male.h_year_k3 <-                   flexsurvspline(my.surv.ic.p.male ~ ns(Year),
                                                            k = 3, scale = "odds",
                                                             data = DMarihuana_1998_2016.p.male)
#AIC = 70552.26
sp.fit.mod1.ic.o.male.h_year_k3
sp.fit.mod1.ic.o.male.h_year_k3$coefficients[6]
sp.fit.mod1.ic.o.male.h_year_k3$AIC

#---#
sp.fit.mod1.ic.o.male.h_year_k5 <-                   flexsurvspline(my.surv.ic.p.male ~ ns(Year),
                                                                    k = 5, scale = "odds",
                                                                    data = DMarihuana_1998_2016.p.male)
#AIC = 70957
sp.fit.mod1.ic.o.male.h_year_k5
sp.fit.mod1.ic.o.male.h_year_k5$coefficients[6]
sp.fit.mod1.ic.o.male.h_year_k5$AIC

#----------------------------------#
sp.fit.mod1.ic.o.male.h_AGEVARY_k3 <-                flexsurvspline(my.surv.ic.p.male ~ ns(Year)+ gamma1(Year) + gamma2(Year) ,
                                                                    k = 3, scale = "odds",
                                                                    data = DMarihuana_1998_2016.p.male)
# AIC =  113,270.2
sp.fit.mod1.ic.o.male.h_AGEVARY_k3
sp.fit.mod1.ic.o.male.h_AGEVARY_k3$coefficients[6]
sp.fit.mod1.ic.o.male.h_AGEVARY_k3$AIC

#---#
sp.fit.mod1.ic.o.male.h_AGEVARY_k5 <-                flexsurvspline(my.surv.ic.p.male ~ ns(Year)+ gamma1(Year) + gamma2(Year) ,
                                                                    k = 5, scale = "odds",
                                                                    data = DMarihuana_1998_2016.p.male)
# AIC = 71,219.24
sp.fit.mod1.ic.o.male.h_AGEVARY_k5
sp.fit.mod1.ic.o.male.h_AGEVARY_k5$coefficients[6]
sp.fit.mod1.ic.o.male.h_AGEVARY_k5$AIC

#---#

sp.fit.mod1.ic.o.male.h_AGEVARY_k6 <-                flexsurvspline(my.surv.ic.p.male ~ ns(Year)+ gamma1(Year) + gamma2(Year) ,
                                                                    k = 6, scale = "odds",
                                                                    data = DMarihuana_1998_2016.p.male)
# AIC = 70129.37, seems to be the best fit
sp.fit.mod1.ic.o.male.h_AGEVARY_k6
sp.fit.mod1.ic.o.male.h_AGEVARY_k6$coefficients[6]
sp.fit.mod1.ic.o.male.h_AGEVARY_k6$AIC

plot(sp.fit.mod1.ic.o.male.h_AGEVARY_k6)

# SUMMARY, BEST FIR MODELS FOR MALES
# Top three models
# No1 : sp.fit.mod1.ic.o.male.h_AGEVARY_k6, AIC = 70129.37
plot(sp.fit.mod1.ic.o.male.h_AGEVARY_k6)
# Assumptions: not saturated, odds-scale, age varying effect (gamma1, gamma2), k=6

# No2 : sp.fit.mod1.ic.p.male.h_AGEVARY_k5, AIC = 70184.99
plot(sp.fit.mod1.ic.p.male.h_AGEVARY_k5)
# Assumptions: not saturated,  hazard scale, age varying effect (gamma1, gamma2), k=5

#No3  : sp.fit.mod1.ic.p.male.h_year_k3, AIC = 70187.48
plot(sp.fit.mod1.ic.p.male.h_year_k3)
# Assumptions: not saturated, hazard scale, not age-varying effect, k=3

#------- SAVING RESULTS-TOP THREE MODELS Males-Period---------#
topthree_males_period_splines <- list(fit.object = list( first = sp.fit.mod1.ic.o.male.h_AGEVARY_k6, second = sp.fit.mod1.ic.p.male.h_AGEVARY_k5, third = sp.fit.mod1.ic.p.male.h_year_k3),
                                        AIC = list( first = sp.fit.mod1.ic.o.male.h_AGEVARY_k6$AIC, second = sp.fit.mod1.ic.p.male.h_AGEVARY_k5$AIC, third = sp.fit.mod1.ic.p.male.h_year_k3$AIC),
                                        Coeffs = list( first = sp.fit.mod1.ic.o.male.h_AGEVARY_k6$coefficients, second = sp.fit.mod1.ic.p.male.h_AGEVARY_k5$coefficients, third = sp.fit.mod1.ic.p.male.h_year_k3$coefficients))
save(topthree_males_period_splines, file = "/Users/mauriciolopezm/Dropbox (Brown)/Troubleshooting-Parametric Models and Splines/topthree_males_period_splines.RData")

# Saving all

all_males_period_splines<- list( 
      "sp.fit.mod1.ic.p.male.h_year_k1-not converge", 
      "sp.fit.mod1.ic.p.male.h_year_k2-not converge",
      sp.fit.mod1.ic.p.male.h_year_k3,
      sp.fit.mod1.ic.p.male.h_year_k5,
      
      "sp.fit.mod1.ic.p.male.h_sat_k1-not converge", 
      "sp.fit.mod1.ic.p.male.h_sat_k2-not converge",
      "sp.fit.mod1.ic.p.male.h_sat_k3-not converge",
      "sp.fit.mod1.ic.p.male.h_sat_k5-not converge",
     
      "sp.fit.mod1.ic.p.male.h_AGEVARY_k1-notconverge",
      sp.fit.mod1.ic.p.male.h_AGEVARY_k2,
      sp.fit.mod1.ic.p.male.h_AGEVARY_k3,
      sp.fit.mod1.ic.p.male.h_AGEVARY_k5,
      sp.fit.mod1.ic.p.male.h_AGEVARY_k6,
      
      "sp.fit.mod1.ic.p.male.h_quadratic_k1-not converge",
      "sp.fit.mod1.ic.p.male.h_quadratic_k2-notconverge",
      "sp.fit.mod1.ic.p.male.h_quadratic_k3-notconverge",
      "sp.fit.mod1.ic.p.male.h_quadratic_k4-notconverge",
      "sp.fit.mod1.ic.p.male.h_quadratic_k5-notconverge",
      
      "sp.fit.mod1.ic.p.male.h_AGEVARY_quadratic_k1-notconverge",
      "sp.fit.mod1.ic.p.male.h_AGEVARY_quadratic_k2-notconverge",
      "sp.fit.mod1.ic.p.male.h_AGEVARY_quadratic_k3-notconverge",
      "sp.fit.mod1.ic.p.male.h_AGEVARY_quadratic_k4-notconverge",
      "sp.fit.mod1.ic.p.male.h_AGEVARY_quadratic_k5-notconverge",
      
      "sp.fit.mod1.ic.o.male.h_year_k1-notconverge",
      sp.fit.mod1.ic.o.male.h_year_k2,
      sp.fit.mod1.ic.o.male.h_year_k3,
      sp.fit.mod1.ic.o.male.h_year_k5,
      
      sp.fit.mod1.ic.o.male.h_AGEVARY_k3,
      sp.fit.mod1.ic.o.male.h_AGEVARY_k5,
      sp.fit.mod1.ic.o.male.h_AGEVARY_k6)
save(all_males_period_splines, file = "/Users/mauriciolopezm/Dropbox (Brown)/Troubleshooting-Parametric Models and Splines/all_males_period_splines.RData")

#----------------------------------#
#-----------FEMALES-----------------------#

# BEST FIT MODELS, BY KNOTS, BY SATURATED VS NOT SATURATED, BY QUADRATIC TERM, BY AGE-VARYING EFFECTS

#----------------------------------#
# not saturated, by year
sp.fit.mod1.ic.p.female.h_year_k1 <-                   flexsurvspline(my.surv.ic.p.female ~ ns(Year),
                                                                    k = 1, scale = "hazard",
                                                                    data = DMarihuana_1998_2016.p.female)
# 22811.41
sp.fit.mod1.ic.p.female.h_year_k1
sp.fit.mod1.ic.p.female.h_year_k1$coefficients[6]
sp.fit.mod1.ic.p.female.h_year_k1$AIC

#---#
sp.fit.mod1.ic.p.female.h_year_k2 <-                   flexsurvspline(my.surv.ic.p.female ~ ns(Year),
                                                                    k = 2, scale = "hazard",
                                                                    data = DMarihuana_1998_2016.p.female)
# 22376.67
sp.fit.mod1.ic.p.female.h_year_k2
sp.fit.mod1.ic.p.female.h_year_k2$coefficients[6]
sp.fit.mod1.ic.p.female.h_year_k2$AIC

#---#
sp.fit.mod1.ic.p.female.h_year_k3 <-                   flexsurvspline(my.surv.ic.p.female ~ ns(Year),
                                                                    k = 3, scale = "hazard",
                                                                    data = DMarihuana_1998_2016.p.female)
# 22170.64, third best model
sp.fit.mod1.ic.p.female.h_year_k3
sp.fit.mod1.ic.p.female.h_year_k3$coefficients[6]
sp.fit.mod1.ic.p.female.h_year_k3$AIC
plot(sp.fit.mod1.ic.p.female.h_year_k3)
#---#
sp.fit.mod1.ic.p.female.h_year_k5 <-                   flexsurvspline(my.surv.ic.p.female ~ ns(Year),
                                                                    k = 5, scale = "hazard",
                                                                    data = DMarihuana_1998_2016.p.female)
#  22170.33, second best model
sp.fit.mod1.ic.p.female.h_year_k5
sp.fit.mod1.ic.p.female.h_year_k5$coefficients[6]
sp.fit.mod1.ic.p.female.h_year_k5$AIC

#----------------------------------#
# Proportional hazard, saturated
sp.fit.mod1.ic.p.female.h_sat_k1 <-                    flexsurvspline(my.surv.ic.p.female ~ factor(Year),
                                                                    k = 1, scale = "hazard",
                                                                    data = DMarihuana_1998_2016.p.female)
# -> does not converge
#sp.fit.mod1.ic.p.female.h_sat_k1$AIC

#---#
sp.fit.mod1.ic.p.female.h_sat_k2 <-                    flexsurvspline(my.surv.ic.p.female ~ factor(Year),
                                                                    k = 2, scale = "hazard",
                                                                    data = DMarihuana_1998_2016.p.female)
# -> does not converge
#sp.fit.mod1.ic.p.female.h_sat_k2$AIC
#---#
sp.fit.mod1.ic.p.female.h_sat_k3 <-                    flexsurvspline(my.surv.ic.p.female ~ factor(Year),
                                                                    k = 3, scale = "hazard",
                                                                    data = DMarihuana_1998_2016.p.female)
# -> DOES NOT CONVERGE
#sp.fit.mod1.ic.p.female.h_sat_k3$AIC
#---#
sp.fit.mod1.ic.p.female.h_sat_k5 <-                    flexsurvspline(my.surv.ic.p.female ~ factor(Year),
                                                                    k = 5, scale = "hazard",
                                                                    data = DMarihuana_1998_2016.p.female)
#-> DOES NOT CONVERGE
#sp.fit.mod1.ic.p.female.h_sat_k5$AIC
#----------------------------------#
# Proportional hazard, NOT SATURATED, ADDING AGE-VARYING EFFECTS, BY KNOTS

sp.fit.mod1.ic.p.female.h_AGEVARY_k1 <-                flexsurvspline(my.surv.ic.p.female ~ ns(Year) + gamma1(Year),
                                                                    k = 1, scale = "hazard",
                                                                    data = DMarihuana_1998_2016.p.female)
#-> does not converge
sp.fit.mod1.ic.p.female.h_AGEVARY_k1$AIC

#---#
sp.fit.mod1.ic.p.female.h_AGEVARY_k2 <-                flexsurvspline(my.surv.ic.p.female ~ ns(Year) + gamma1(Year)+gamma2(Year),
                                                                    k = 2, scale = "hazard",
                                                                    data = DMarihuana_1998_2016.p.female)
#-> AIC = 22633.3

sp.fit.mod1.ic.p.female.h_AGEVARY_k2
sp.fit.mod1.ic.p.female.h_AGEVARY_k2$coefficients[6]
sp.fit.mod1.ic.p.female.h_AGEVARY_k2$AIC


#---#
sp.fit.mod1.ic.p.female.h_AGEVARY_k3 <-                flexsurvspline(my.surv.ic.p.female ~ ns(Year) + gamma1(Year) + gamma2(Year)  ,
                                                                    k = 3, scale = "hazard",
                                                                    data = DMarihuana_1998_2016.p.female)
#AIC = 22265.49
sp.fit.mod1.ic.p.female.h_AGEVARY_k3
sp.fit.mod1.ic.p.female.h_AGEVARY_k3$coefficients[6]
sp.fit.mod1.ic.p.female.h_AGEVARY_k3$AIC

#---#
sp.fit.mod1.ic.p.female.h_AGEVARY_k5 <-                flexsurvspline(my.surv.ic.p.female ~ ns(Year)+ gamma1(Year) + gamma2(Year) ,
                                                                    k = 5, scale = "hazard",
                                                                    data = DMarihuana_1998_2016.p.female)
# does not converge
#plot(sp.fit.mod1.ic.p.female.h_AGEVARY_k5)
#---#
sp.fit.mod1.ic.p.female.h_AGEVARY_k6 <-                flexsurvspline(my.surv.ic.p.female ~ ns(Year)+ gamma1(Year) + gamma2(Year) ,
                                                                    k = 6, scale = "hazard",
                                                                    data = DMarihuana_1998_2016.p.female)
# AIC = 22521.31
sp.fit.mod1.ic.p.female.h_AGEVARY_k6
sp.fit.mod1.ic.p.female.h_AGEVARY_k6$coefficients[6]
sp.fit.mod1.ic.p.female.h_AGEVARY_k6$AIC

#----------------------------------#

# Proportional hazard, NOT SATURATED, ADDING QUADRATIC TERM, BY KNOTS

sp.fit.mod1.ic.p.female.h_quadratic_k1 <-              flexsurvspline(my.surv.ic.p.female ~ ns(Year)+ ns(Year*Year),
                                                                    k = 1, scale = "hazard",
                                                                    data = DMarihuana_1998_2016.p.female)
# -> does not converge
#sp.fit.mod1.ic.p.female.h_quadratic_k1$AIC
#---#
sp.fit.mod1.ic.p.female.h_quadratic_k2 <-              flexsurvspline(my.surv.ic.p.female ~ ns(Year)+ ns(Year*Year),
                                                                    k = 2, scale = "hazard",
                                                                    data = DMarihuana_1998_2016.p.female)
# -> does not converge
#sp.fit.mod1.ic.p.female.h_quadratic_k2$AIC
#---#
sp.fit.mod1.ic.p.female.h_quadratic_k3 <-              flexsurvspline(my.surv.ic.p.female ~ ns(Year)+ ns(Year*Year),
                                                                    k = 3, scale = "hazard",
                                                                    data = DMarihuana_1998_2016.p.female)
# does not converge
#sp.fit.mod1.ic.p.female.h_quadratic_k3$AIC
#---#
sp.fit.mod1.ic.p.female.h_quadratic_k5 <-              flexsurvspline(my.surv.ic.p.female ~ ns(Year)+ ns(Year*Year),
                                                                    k = 5, scale = "hazard",
                                                                    data = DMarihuana_1998_2016.p.female)
# does not converge
#sp.fit.mod1.ic.p.female.h_quadratic_k5$AIC
#----------------------------------#

# Proportional hazard, NOT SATURATED, ADDING AGE-VARYING EFFECTS, ADDING QUADRATIC TERM, BY KNOTS
sp.fit.mod1.ic.p.female.h_AGEVARY_quadratic_k1 <-      flexsurvspline(my.surv.ic.p.female ~ ns(Year) + gamma1(Year) + ns(Year*Year),
                                                                    k = 1, scale = "hazard",
                                                                    data = DMarihuana_1998_2016.p.female)
#-> does not converge
#sp.fit.mod1.ic.p.female.h_AGEVARY_quadratic_k1$AIC

#---#
sp.fit.mod1.ic.p.female.h_AGEVARY_quadratic_k2 <-      flexsurvspline(my.surv.ic.p.female ~ ns(Year) + gamma1(Year) + gamma2(Year) + ns(Year*Year),
                                                                    k = 2, scale = "hazard",
                                                                    data = DMarihuana_1998_2016.p.female)

#-> does not converge
#sp.fit.mod1.ic.p.female.h_AGEVARY_quadratic_k2$AIC

#---#
sp.fit.mod1.ic.p.female.h_AGEVARY_quadratic_k3 <-      flexsurvspline(my.surv.ic.p.female ~ poly(Year, 2) + gamma1(Year) + gamma2(Year),
                                                                    k = 3, scale = "hazard",
                                                                    data = DMarihuana_1998_2016.p.female)
# does not converge
#sp.fit.mod1.ic.p.female.h_AGEVARY_quadratic_k3$AIC
#---#
sp.fit.mod1.ic.p.female.h_AGEVARY_quadratic_k5 <-      flexsurvspline(my.surv.ic.p.female ~ ns(Year)+ gamma1(Year) + gamma2(Year) + (Year*Year) ,
                                                                    k = 5, scale = "hazard",
                                                                    data = DMarihuana_1998_2016.p.female)
# does not converge
sp.fit.mod1.ic.p.female.h_AGEVARY_quadratic_k5$AIC
#----------------------------------#
#-----------ODDS SCALE-----------------------#
sp.fit.mod1.ic.o.female.h_year_k1 <-                   flexsurvspline(my.surv.ic.p.female ~ ns(Year),
                                                                    k = 1, scale = "odds",
                                                                    data = DMarihuana_1998_2016.p.female)
#AIC = 22803.04
sp.fit.mod1.ic.o.female.h_year_k1
sp.fit.mod1.ic.o.female.h_year_k1$coefficients[6]
sp.fit.mod1.ic.o.female.h_year_k1$AIC

#---#
sp.fit.mod1.ic.o.female.h_year_k2 <-                   flexsurvspline(my.surv.ic.p.female ~ ns(Year),
                                                                    k = 2, scale = "odds",
                                                                    data = DMarihuana_1998_2016.p.female)
#AIC = 22377.85
sp.fit.mod1.ic.o.female.h_year_k2
sp.fit.mod1.ic.o.female.h_year_k2$coefficients[6]
sp.fit.mod1.ic.o.female.h_year_k2$AIC


#---#
sp.fit.mod1.ic.o.female.h_year_k3 <-                   flexsurvspline(my.surv.ic.p.female ~ ns(Year),
                                                                    k = 3, scale = "odds",
                                                                    data = DMarihuana_1998_2016.p.female)
#AIC = 22166.17, best fit model
sp.fit.mod1.ic.o.female.h_year_k3
sp.fit.mod1.ic.o.female.h_year_k3$coefficients[6]
sp.fit.mod1.ic.o.female.h_year_k3$AIC

#---#
sp.fit.mod1.ic.o.female.h_year_k5 <-                   flexsurvspline(my.surv.ic.p.female ~ ns(Year),
                                                                    k = 5, scale = "odds",
                                                                    data = DMarihuana_1998_2016.p.female)
#AIC = 22202.84
sp.fit.mod1.ic.o.female.h_year_k5
sp.fit.mod1.ic.o.female.h_year_k5$coefficients[6]
sp.fit.mod1.ic.o.female.h_year_k5$AIC

#----------------------------------#
sp.fit.mod1.ic.o.female.h_AGEVARY_k3 <-                flexsurvspline(my.surv.ic.p.female ~ ns(Year)+ gamma1(Year) + gamma2(Year) ,
                                                                    k = 3, scale = "odds",
                                                                    data = DMarihuana_1998_2016.p.female)
# AIC = 22251.97
sp.fit.mod1.ic.o.female.h_AGEVARY_k3
sp.fit.mod1.ic.o.female.h_AGEVARY_k3$coefficients[6]
sp.fit.mod1.ic.o.female.h_AGEVARY_k3$AIC
plot()
#---#
sp.fit.mod1.ic.o.female.h_AGEVARY_k5 <-                flexsurvspline(my.surv.ic.p.female ~ ns(Year)+ gamma1(Year) + gamma2(Year) ,
                                                                    k = 5, scale = "odds",
                                                                    data = DMarihuana_1998_2016.p.female)
# AIC = 22337.92
sp.fit.mod1.ic.o.female.h_AGEVARY_k5
sp.fit.mod1.ic.o.female.h_AGEVARY_k5$coefficients[6]
sp.fit.mod1.ic.o.female.h_AGEVARY_k5$AIC
#---#

sp.fit.mod1.ic.o.female.h_AGEVARY_k6 <-                flexsurvspline(my.surv.ic.p.female ~ ns(Year)+ gamma1(Year) + gamma2(Year) ,
                                                                    k = 6, scale = "odds",
                                                                    data = DMarihuana_1998_2016.p.female)
# AIC = 22334.41
sp.fit.mod1.ic.o.female.h_AGEVARY_k6
sp.fit.mod1.ic.o.female.h_AGEVARY_k6$coefficients[6]
sp.fit.mod1.ic.o.female.h_AGEVARY_k6$AIC

plot(sp.fit.mod1.ic.o.female.h_AGEVARY_k6)


# ------- Summary of Models ----------- #
# Top three models
# No1 : sp.fit.mod1.ic.o.female.h_year_k3, AIC = 22166.17
# Assumptions: odds scale, not saturated, not age-varying effect, k =3
plot(sp.fit.mod1.ic.o.female.h_year_k3)
# No2 : sp.fit.mod1.ic.p.female.h_year_k5, AIC = 22170.33
# Assupmtions: hazard scale, not saturated, not age-varying, k=5
plot(sp.fit.mod1.ic.p.female.h_year_k5)
# No3 : sp.fit.mod1.ic.p.female.h_year_k3, AIC = 22170.64
# Assumptions: hazard scale, not aturated, not age-varying, k=3
plot(sp.fit.mod1.ic.p.female.h_year_k3)

# ---------SAVING BEST FIT MODELS, FEMALES PERIOD ------------#

topthree_females_period_splines <- list(fit.object = list( first = sp.fit.mod1.ic.o.female.h_year_k3, second = sp.fit.mod1.ic.p.female.h_year_k5, third = sp.fit.mod1.ic.p.female.h_year_k3),
                                        AIC = list( first = sp.fit.mod1.ic.o.female.h_year_k3$AIC, second = sp.fit.mod1.ic.p.female.h_year_k5$AIC, third = sp.fit.mod1.ic.p.female.h_year_k3$AIC),
                                        Coeffs = list( first = sp.fit.mod1.ic.o.female.h_year_k3$coefficients, second = sp.fit.mod1.ic.p.female.h_year_k5$coefficients, third = sp.fit.mod1.ic.p.female.h_year_k3$coefficients))
save(topthree_females_period_splines, file = "/Users/mauriciolopezm/Dropbox (Brown)/Troubleshooting-Parametric Models and Splines/topthree_females_period_splines.RData")

all_females_period_splines <- list(
  sp.fit.mod1.ic.p.female.h_year_k1,
  sp.fit.mod1.ic.p.female.h_year_k2,
  sp.fit.mod1.ic.p.female.h_year_k3,
  sp.fit.mod1.ic.p.female.h_year_k5, 
  
  "sp.fit.mod1.ic.p.female.h_sat_k1-notconverge",
  "sp.fit.mod1.ic.p.female.h_sat_k2-notconverge",
  "sp.fit.mod1.ic.p.female.h_sat_k3-notconverge",
  "sp.fit.mod1.ic.p.female.h_sat_k5-notconverge",
  
  "sp.fit.mod1.ic.p.female.h_AGEVARY_k1-notconverge",
  sp.fit.mod1.ic.p.female.h_AGEVARY_k2,
  sp.fit.mod1.ic.p.female.h_AGEVARY_k3,
  "sp.fit.mod1.ic.p.female.h_AGEVARY_k5-notconverge",
  sp.fit.mod1.ic.p.female.h_AGEVARY_k6,
  
  "sp.fit.mod1.ic.p.female.h_quadratic_k1-notconverge",
  "sp.fit.mod1.ic.p.female.h_quadratic_k2-notconverge",
  "sp.fit.mod1.ic.p.female.h_quadratic_k3-notconverge",
  "sp.fit.mod1.ic.p.female.h_quadratic_k5-notconverge",
  
  "sp.fit.mod1.ic.p.female.h_AGEVARY_quadratic_k1-notconverge",
  "sp.fit.mod1.ic.p.female.h_AGEVARY_quadratic_k2-notconverge",
  "sp.fit.mod1.ic.p.female.h_AGEVARY_quadratic_k3-notconverge",
  "sp.fit.mod1.ic.p.female.h_AGEVARY_quadratic_k5-notconverge",

  sp.fit.mod1.ic.o.female.h_year_k1,
  sp.fit.mod1.ic.o.female.h_year_k2,
  sp.fit.mod1.ic.o.female.h_year_k3,
  sp.fit.mod1.ic.o.female.h_year_k5, 
  
  sp.fit.mod1.ic.o.female.h_AGEVARY_k3,
  sp.fit.mod1.ic.o.female.h_AGEVARY_k5,
  sp.fit.mod1.ic.o.female.h_AGEVARY_k6
  
)
save(all_females_period_splines, file ="/Users/mauriciolopezm/Dropbox (Brown)/Troubleshooting-Parametric Models and Splines/all_females_period_splines.RData")

# ---------------------------------------------------------------------------------------------------------- #
# Spline Models | Estimates within-sample by Period ============================= #
# ================================================================================================ #
# using model 1 on hazard scale

#------------ MALES
v.years.ext.ic <- c(1998,2002,2008, 2011,2016)
df.haz.ext.ic.p.male <- expand.grid(Year = v.years.ext.ic, 
                             stringsAsFactors = T)
df.haz.ext.ic.p.male  

haz.sp.fit.mod1.ic.p.male <- as.data.frame(summary(sp.fit.mod1.ic.p.male.h, 
                                                   newdata = df.haz.ext.ic.p.male,
                                                   type = "hazard", ci = TRUE,
                                                   tidy = TRUE))
#colnames(haz.sp.fit.mod1.ic.p.male)
val.periods <- unique(haz.sp.fit.mod1.ic.p.male$`ns(Year)`)

haz.sp.fit.mod1.ic.p.male<- haz.sp.fit.mod1.ic.p.male%>%
   dplyr::rename(Period = `ns(Year)`)%>%
   mutate(Period = as.numeric(Period),
          Period = ifelse(Period == val.periods[1],'1998',
                   ifelse(Period==  val.periods[2],'2002',
                   ifelse(Period==  val.periods[3], '2008',
                   ifelse(Period==  val.periods[4], '2011','2016'))))) %>%
   dplyr::rename("h_t" = est,
          "Init.Age" = time)
colnames(haz.sp.fit.mod1.ic.p.male)

head(haz.sp.fit.mod1.ic.p.male)

unique(haz.sp.fit.mod1.ic.p.male$Period)

plot_df_male  <- haz.sp.fit.mod1.ic.p.male
plot_df_male$Period <- as.factor(plot_df_male$Period)
plot_df_male$Period <-factor(plot_df_male$Period , levels = c("2016","2011","2008","2002","1998"))
levels(plot_df_male$Period)

# ---------------------- GENERATING BASELINE CUM HAZARDS 

v.years.ext.ic <- c(1998,2002,2008, 2011,2016)
df.survival.ext.ic.p.male <- expand.grid(Year = v.years.ext.ic, 
                                    stringsAsFactors = T)
df.survival.ext.ic.p.male  

survival.sp.fit.mod1.ic.p.male <- as.data.frame(summary(sp.fit.mod1.ic.p.male.h, 
                                                   newdata = df.survival.ext.ic.p.male,
                                                   type = "survival", ci = TRUE,
                                                   tidy = TRUE))
#colnames(haz.sp.fit.mod1.ic.p.male)
val.periods <- unique(survival.sp.fit.mod1.ic.p.male$`ns(Year)`)

survival.sp.fit.mod1.ic.p.male<- survival.sp.fit.mod1.ic.p.male%>%
  dplyr::rename(Period = `ns(Year)`)%>%
  mutate(Period = as.numeric(Period),
         Period = ifelse(Period == val.periods[1],'1998',
                         ifelse(Period==  val.periods[2],'2002',
                                ifelse(Period==  val.periods[3], '2008',
                                       ifelse(Period==  val.periods[4], '2011','2016'))))) %>%
  dplyr::rename("S_t" = est,
         "Init.Age" = time)%>%
  mutate("logminuslogS_T" =  log(-log(S_t)))
colnames(survival.sp.fit.mod1.ic.p.male)

head(survival.sp.fit.mod1.ic.p.male)

unique(survival.sp.fit.mod1.ic.p.male$Period)

plot_df_male_logcumhaz  <- survival.sp.fit.mod1.ic.p.male
plot_df_male_logcumhaz$Period <- as.factor(plot_df_male_logcumhaz$Period)
plot_df_male_logcumhaz$Period <-factor(plot_df_male_logcumhaz$Period , levels = c("2016","2011","2008","2002","1998"))
levels(plot_df_male_logcumhaz$Period)
range(plot_df_male_logcumhaz$logminuslogS_T)

colnames(plot_df_male_logcumhaz)
plot3.mod1.ic.p.male_logcumhaz <- ggplot(data = plot_df_male_logcumhaz, aes(x = log(Init.Age), y = logminuslogS_T, color = Period)) + 
  geom_line(linetype="solid", size=0.5)+  
  #geom_point(data = df_surv.p.male, shape= df_surv.p.male$Period, size = 0.7) +
  labs(title = "Log Cumulative Hazard by Period, for Males",
       subtitle = "Flexible Parametric Model - Natural Cubic Spline (knots = 3)")+    
  xlab("log[Age (years)]") +
  ylab(bquote("Log Cumulative Hazard, log[H(a)]")) +
  ylim(c(-14, 0))+
  #xlim(c(0,65))+
  theme_bw()+
  #theme(legend.position = c(.9, .68),
  #      legend.box.background = element_rect(colour = "white", size=2))+
  scale_x_continuous(breaks = number_ticks(13)) 
#scale_y_continuous(breaks = number_ticks(10)) 

plot3.mod1.ic.p.male_logcumhaz


#-------------------#
# v.years.ext.ic2<- c(1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010, 2011,2012,2013,2014,2015,2016)
# length(v.years.ext.ic2)
# df.haz.ext.ic.p.male2 <- expand.grid(Year = v.years.ext.ic2, 
#                                      stringsAsFactors = T)
# df.haz.ext.ic.p.male2  
# 
# haz.sp.fit.mod1.ic.p.male2 <- as.data.frame(summary(sp.fit.mod1.ic.p.male.h, 
#                                                    newdata = df.haz.ext.ic.p.male2,
#                                                    type = "hazard", ci = TRUE,
#                                                    tidy = TRUE))
# colnames(haz.sp.fit.mod1.ic.p.male2)
# val.periods <- unique(haz.sp.fit.mod1.ic.p.male2$`ns(Year)`)
# 
# haz.sp.fit.mod1.ic.p.male2<- haz.sp.fit.mod1.ic.p.male2%>%
#   rename(Period = `ns(Year)`)%>%
#   mutate(Period = as.numeric(Period),
#          Period = ifelse(Period == val.periods[1],'1998',
#                          ifelse(Period == val.periods[2],'1999',
#                          ifelse(Period==  val.periods[3],'2000',
#                          ifelse(Period==  val.periods[4],'2001',
#                          ifelse(Period==  val.periods[5],'2002',
#                          ifelse(Period==  val.periods[6],'2003',
#                          ifelse(Period==  val.periods[7],'2004',
#                          ifelse(Period==  val.periods[8],'2005',
#                          ifelse(Period==  val.periods[9],'2006',
#                          ifelse(Period==  val.periods[10],'2007',
#                          ifelse(Period==  val.periods[11],'2008',
#                          ifelse(Period==  val.periods[12],'2009',
#                          ifelse(Period==  val.periods[13],'2010',
#                          ifelse(Period==  val.periods[14],'2011',
#                          ifelse(Period==  val.periods[15],'2012',
#                          ifelse(Period==  val.periods[16],'2013',
#                          ifelse(Period==  val.periods[17],'2014',
#                          ifelse(Period==  val.periods[18], '2015','2016'))))))))))))))))))) %>%
#   rename("h_t" = est,
#          "Init_Age" = time) %>%
#   mutate(in_survey = as.factor(ifelse(Period %in% c("2016","2011","2008","2002","1998"),"in survey","not in survey")))
# colnames(haz.sp.fit.mod1.ic.p.male2)
# 
# head(haz.sp.fit.mod1.ic.p.male2)
# 
# unique(haz.sp.fit.mod1.ic.p.male2$Period)
# 
# 
# 
# plot_df_male2  <- haz.sp.fit.mod1.ic.p.male2
# plot_df_male2$Period <- as.factor(plot_df_male2$Period)
# plot_df_male2$Period <-factor(plot_df_male2$Period , levels = c('1998',  '1999',
#                                                                 '2000',
#                                                                 '2001',
#                                                                 '2002',
#                                                                 '2003',
#                                                                 '2004',
#                                                                 '2005',
#                                                                 '2006',
#                                                                 '2007',
#                                                                 '2008',
#                                                                 '2009',
#                                                                 '2010',
#                                                                 '2011',
#                                                                 '2012',
#                                                                 '2013',
#                                                                 '2014','2015','2016'))
# levels(plot_df_male2$Period)
# levels(plot_df_male2$in_survey)
# plot_df_male2 <- plot_df_male2 %>%
#   mutate(h_t_rateperyr = h_t*sp.fit.mod1.ic.p.male.h$coefficients[6])
# head(plot_df_male2)
# 
# age_peak.male <- plot_df_male2%>%
#   group_by(Period) %>%
#   summarize(rate.max = max(h_t))
# age_peak.male<- age_peak.male%>%
#   mutate(ageofmaxrate =plot_df_male2$Init_Age[which(plot_df_male2$h_t %in% age_peak.male$rate.max)] )
# age_peak.male
# 
# plot_df_male2 <- plot_df_male2 %>%
#   mutate(Ind_peak = ifelse(plot_df_male2$h_t %in% age_peak.male$rate.max,1,0))
# 
# plot_rateofchange_male <- plot_df_male2 %>% filter(Ind_peak==1)

# ---------------------------------------------------------------------------------------------------------- #
# using model 1 on hazard scale 

#------------ FEMALES
v.years.ext.ic <- c(1998,2002,2008, 2011,2016)


df.haz.ext.ic.p.female <- expand.grid(Year = v.years.ext.ic, 
                                    stringsAsFactors = T)
df.haz.ext.ic.p.female 


haz.sp.fit.mod1.ic.p.female <- as.data.frame(summary(sp.fit.mod1.ic.p.female.h, 
                                                   newdata = df.haz.ext.ic.p.female,
                                                   type = "hazard", ci = TRUE,
                                                   tidy = TRUE))
colnames(haz.sp.fit.mod1.ic.p.female)
val.periods <- unique(haz.sp.fit.mod1.ic.p.female$`ns(Year)`)

haz.sp.fit.mod1.ic.p.female<- haz.sp.fit.mod1.ic.p.female%>%
  rename(Period = `ns(Year)`)%>%
  mutate(Period = as.numeric(Period),Period = ifelse(Period == val.periods[1],'1998',
                    ifelse(Period==  val.periods[2],'2002',
                           ifelse(Period==  val.periods[3], '2008',
                                  ifelse(Period==  val.periods[4], '2011','2016'))))) %>%
  rename("h_t" = est,
         "Init.Age" = time)
colnames(haz.sp.fit.mod1.ic.p.female)

head(haz.sp.fit.mod1.ic.p.female)

unique(haz.sp.fit.mod1.ic.p.female$Period)
plot_df_female  <- haz.sp.fit.mod1.ic.p.female
plot_df_female$Period <- as.factor(plot_df_female$Period)
plot_df_female$Period <-factor(plot_df_female$Period , levels = c("2016","2011","2008","2002","1998"))
levels(plot_df_female$Period)

#------------#

# v.years.ext.ic2<- c(1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010, 2011,2012,2013,2014,2015,2016)
# df.haz.ext.ic.p.female2 <- expand.grid(Year = v.years.ext.ic2, 
#                                       stringsAsFactors = T)
# df.haz.ext.ic.p.female2 
# 
# 
# haz.sp.fit.mod1.ic.p.female2 <- as.data.frame(summary(sp.fit.mod1.ic.p.female.h, 
#                                                      newdata = df.haz.ext.ic.p.female2,
#                                                      type = "hazard", ci = TRUE,
#                                                      tidy = TRUE))
# colnames(haz.sp.fit.mod1.ic.p.female2)
# val.periods <- unique(haz.sp.fit.mod1.ic.p.female2$`ns(Year)`)
# 
# haz.sp.fit.mod1.ic.p.female2<- haz.sp.fit.mod1.ic.p.female2%>%
#   mutate(Period = `ns(Year)`,
#          Period = ifelse(Period == val.periods[1],'1998',
#                   ifelse(Period == val.periods[2],'1999',
#                   ifelse(Period==  val.periods[3],'2000',
#                   ifelse(Period==  val.periods[4],'2001',
#                   ifelse(Period==  val.periods[5],'2002',
#                   ifelse(Period==  val.periods[6],'2003',
#                   ifelse(Period==  val.periods[7],'2004',
#                   ifelse(Period==  val.periods[8],'2005',
#                   ifelse(Period==  val.periods[9],'2006',
#                   ifelse(Period==  val.periods[10],'2007',
#                   ifelse(Period==  val.periods[11],'2008',
#                   ifelse(Period==  val.periods[12],'2009',
#                   ifelse(Period==  val.periods[13],'2010',
#                   ifelse(Period==  val.periods[14],'2011',
#                   ifelse(Period==  val.periods[15],'2012',
#                   ifelse(Period==  val.periods[16],'2013',
#                   ifelse(Period==  val.periods[17],'2014',
#                   ifelse(Period==  val.periods[18], '2015','2016'))))))))))))))))))) %>%
#   rename("h_t" = est,
#          "Init_Age" = time) %>%
#   mutate(in_survey = as.factor(ifelse(Period %in% c("2016","2011","2008","2002","1998"),"in survey","not in survey")))%>%
#   select(-`ns(Year)`)
# colnames(haz.sp.fit.mod1.ic.p.female2)
# 
# head(haz.sp.fit.mod1.ic.p.female2)
# 
# unique(haz.sp.fit.mod1.ic.p.female2$Period)
# 
# plot_df_female2  <- haz.sp.fit.mod1.ic.p.female2
# plot_df_female2$Period <- as.factor(plot_df_female2$Period)
# plot_df_female2$Period <-factor(plot_df_female2$Period , levels = c('1998',  '1999',
#                                                                              '2000',
#                                                                              '2001',
#                                                                              '2002',
#                                                                              '2003',
#                                                                              '2004',
#                                                                              '2005',
#                                                                              '2006',
#                                                                              '2007',
#                                                                              '2008',
#                                                                              '2009',
#                                                                              '2010',
#                                                                              '2011',
#                                                                              '2012',
#                                                                              '2013',
#                                                                              '2014','2015','2016'))
# levels(plot_df_female2$Period)
# levels(plot_df_male2$in_survey)
# 
# plot_df_female2 <- plot_df_female2 %>%
#   mutate(h_t_rateperyr = h_t*sp.fit.mod1.ic.p.female.h$coefficients[6])
# head(plot_df_female2)
# 
# age_peak.female <- plot_df_female2%>%
#   group_by(Period) %>%
#   summarize(rate.max = max(h_t))
# age_peak.female<- age_peak.female%>%
#   mutate(ageofmaxrate =plot_df_female2$Init_Age[which(plot_df_female2$h_t %in% age_peak.female$rate.max)] )
# age_peak.female
# 
# plot_df_female2 <- plot_df_female2 %>%
#   mutate(Ind_peak = ifelse(plot_df_female2$h_t %in% age_peak.female$rate.max,1,0))
# 
# plot_rateofchange_female <- plot_df_female2 %>% filter(Ind_peak==1)



# ---------------------------------------------------------------------------------------------------------- #
# Plots from KM versus Splines, by PERIOD ============================================= #
# ================================================================================================ #
# Within Sample, Interval and Right-censored data (1998-2016)
# Male

head(df_surv.p.male)
head(haz.sp.fit.mod1.ic.p.male)

#Hazard rates plot by Sex and Cohort
# plot1.mod1.ic.p.male <- ggplot(data = haz.sp.fit.mod1.ic.p.male, aes(x = Init.Age, y = h_t, color= Period)) + 
#   geom_line(linetype="solid", size=0.5)+  
#   geom_step(data = df_surv.p.male, linetype="solid", size = 0.2, color="blue") +
#   facet_grid(~Period)+
#   labs(title = "Age-specific Cannabis Initiation Rates by Period for Males:",
#        subtitle = "Non-parametric vs flexible parametric estimates")+
#   ylab("h(a)") +
#   ylim(c(0, 0.045))+
#   theme_bw()
# plot1.mod1.ic.p.male
# 
# plot2.mod1.ic.p.male <- ggplot(data = haz.sp.fit.mod1.ic.p.male, aes(x = Init.Age, y = h_t, color= Period)) + 
#   geom_line(linetype="solid", size=0.5)+  
#   geom_point(data = df_surv.p.male, shape= df_surv.p.male$Period, size = 1) +
#   facet_grid(~Period)+
#   labs(title = "Age-specific Cannabis Initiation Rates by Period for Males:",
#        subtitle = "Non-parametric vs flexible parametric estimates")+
#   theme(legend.position = "bottom") +
#   xlab("Age (years)") +
#   ylab("h(a)") +
#   #scale_x_continuous(breaks = number_ticks(13))+
#   ylim(c(0, 0.037))+
#   theme_bw()
# plot2.mod1.ic.p.male
colnames(plot_df_male)
plot3.mod1.ic.p.male <- ggplot(data = plot_df_male, aes(x = Init.Age, y = h_t, color = Period)) + 
  geom_line(linetype="solid", size=0.5)+  
  #geom_point(data = df_surv.p.male, shape= df_surv.p.male$Period, size = 0.7) +
  labs(title = "PANEL-A: Age-Specific Rates of Onset of Cannabis Use by Period for Males",
       subtitle = "Flexible Parametric Model - Natural Cubic Spline (knots = 3)")+    
  xlab("Age (years)") +
  ylab(bquote("Rates of Onset, h(a)")) +
  ylim(c(0, 0.035))+
  #xlim(c(0,65))+
  theme_bw()+
  #theme(legend.position = c(.9, .68),
  #      legend.box.background = element_rect(colour = "white", size=2))+
  scale_x_continuous(breaks = number_ticks(13)) 
#scale_y_continuous(breaks = number_ticks(10)) 

plot3.mod1.ic.p.male

# plot all years

plot3.mod1.ic.p.male_allyrs <- ggplot(data = plot_df_male2, aes(x = Init_Age, y = h_t, color = Period, linetype= in_survey)) + 
  geom_line( size=0.5)+  
  #geom_point(data = df_surv.p.male, shape= df_surv.p.male$Period, size = 0.7) +
  labs(title = "PANEL-A: Age-Specific Rates of Onset of Cannabis Use by Period for Males",
       subtitle = "Flexible Parametric Model - Natural Cubic Spline (knots = 3)")+    
  xlab("Age (years)") +
  ylab(bquote("Rates of Onset, h(a)")) +
  ylim(c(0, 0.035))+
  #xlim(c(0,65))+
  theme_bw()+
  #theme(legend.position = c(.9, .68),
  #      legend.box.background = element_rect(colour = "white", size=2))+
  scale_x_continuous(breaks = number_ticks(13)) 
#scale_y_continuous(breaks = number_ticks(10)) 

plot3.mod1.ic.p.male_allyrs

# rate of change of hazard rate at peak rate of onset 
#  
# plot_rateofchange_male

#save(plot2.mod1.ic.p.male, plot3.mod1.ic.p.male, file="spline.plots.period.male.RData")
plot4.mod1.ic.p.male_allyrs_rates <- ggplot(data = plot_rateofchange_male, aes(x = as.numeric(Period), y = h_t_rateperyr)) + 
  geom_point( size=0.5)+  
  #geom_point(data = df_surv.p.male, shape= df_surv.p.male$Period, size = 0.7) +
  labs(title = "PANEL-A: Annual Rate of Change of Peak Rates of Onset of Cannabis,  Males")+
#       subtitle = "Flexible Parametric Model - Natural Cubic Spline (knots = 3)")+    
  xlab("Period (calendar years)") +
  ylab(bquote("Rates of Change of Peak Hazard Rate, dh(a)/dperiod")) +
  ylim(c(0, 0.035))+
  #xlim(c(0,65))+
  theme_bw()+
  #theme(legend.position = c(.9, .68),
  #      legend.box.background = element_rect(colour = "white", size=2))+
  scale_x_continuous(breaks = number_ticks(19)) 
#scale_y_continuous(breaks = number_ticks(10)) 

plot4.mod1.ic.p.male_allyrs_rates


# Female

# head(df_surv.p.female)
# head(haz.sp.fit.mod1.ic.p.female)
# 
# #Hazard rates plot by Sex and Cohort
# plot1.mod1.ic.p.female <- ggplot(data = haz.sp.fit.mod1.ic.p.female, aes(x = Init.Age, y = h_t, color= Period)) + 
#   geom_line(linetype="solid", size=0.5)+  
#   geom_step(data = df_surv.p.female, linetype="solid", size = 0.2, color="blue") +
#   facet_grid(~Period)+
#   labs(title = "Age-specific Cannabis Initiation Rates by Period for Females:",
#        subtitle = "Non-parametric vs flexible parametric estimates")+
#   xlab("Age (years)") +
#   ylab("h(a)") +
#   ylim(c(0, 0.015))+
#   theme_bw()
# plot1.mod1.ic.p.female
# 
# plot2.mod1.ic.p.female <- ggplot(data = haz.sp.fit.mod1.ic.p.female, aes(x = Init.Age, y = h_t, color= Period)) + 
#   geom_line(linetype="solid", size=0.5)+  
#   geom_point(data = df_surv.p.female, shape= df_surv.p.female$Period, size = 1) +
#   facet_grid(~Period)+
#   theme(legend.position = "bottom") +
#   labs(title = "Age-specific Cannabis Initiation Rates by Period for Females:",
#        subtitle = "Non-parametric vs flexible parametric estimates")+
#   xlab("Age (years)") +
#   ylab("h(a)") +
#   #scale_x_continuous(breaks = number_ticks(13)) +
#   ylim(c(0, 0.037))+
#   theme_bw()
# plot2.mod1.ic.p.female


plot3.mod1.ic.p.female <- ggplot(data = plot_df_female, aes(x = Init.Age, y = h_t, color= Period)) + 
  geom_line(linetype="solid", size=0.5)+  
  #geom_point(data = df_surv.p.female, shape= df_surv.p.female$Period, size = 0.7) +
  labs(title = "PANEL-B: Age-Specific Rates of Onset of Cannabis Use by Period for Females",
       subtitle = "Flexible Parametric Model - Natural Cubic Spline (knots = 3)")+    
  xlab("Age (years)") +
  ylab(bquote("Rates of Onset, h(a)")) +
  ylim(c(0, 0.035))+
  theme_bw()+
  #theme(legend.position = c(.9, .68),
  #      legend.box.background = element_rect(colour = "white", size=2))+
  scale_x_continuous(breaks = number_ticks(13)) 
#scale_y_continuous(breaks = number_ticks(10)) 


plot3.mod1.ic.p.female

#save(plot2.mod1.ic.p.female, plot3.mod1.ic.p.female, file="spline.plots.period.female.RData")

# plot all years

plot3.mod1.ic.p.female_allyrs <- ggplot(data = plot_df_female2, aes(x = Init_Age, y = h_t, color = Period, linetype= in_survey)) + 
  geom_line( size=0.5)+  
  #geom_point(data = df_surv.p.male, shape= df_surv.p.male$Period, size = 0.7) +
  labs(title = "PANEL-A: Age-Specific Rates of Onset of Cannabis Use by Period for Females",
       subtitle = "Flexible Parametric Model - Natural Cubic Spline (knots = 3)")+    
  xlab("Age (years)") +
  ylab(bquote("Rates of Onset, h(a)")) +
  ylim(c(0, 0.035))+
  #xlim(c(0,65))+
  theme_bw()+
  #theme(legend.position = c(.9, .68),
  #      legend.box.background = element_rect(colour = "white", size=2))+
  scale_x_continuous(breaks = number_ticks(13)) 
#scale_y_continuous(breaks = number_ticks(10)) 

plot3.mod1.ic.p.female_allyrs

# rate of change of hazard rate at peak rate of onset 
#  
# plot_rateofchange_female

#save(plot2.mod1.ic.p.male, plot3.mod1.ic.p.male, file="spline.plots.period.male.RData")
plot4.mod1.ic.p.female_allyrs_rates <- ggplot(data = plot_rateofchange_female, aes(x = as.numeric(Period), y = h_t_rateperyr)) + 
  geom_point( size=0.5)+  
  #geom_point(data = df_surv.p.male, shape= df_surv.p.male$Period, size = 0.7) +
  labs(title = "PANEL-A: Annual Rate of Change of Peak Rates of Onset of Cannabis,  Females")+
  #       subtitle = "Flexible Parametric Model - Natural Cubic Spline (knots = 3)")+    
  xlab("Period (calendar years)") +
  ylab(bquote("Rates of Change of Peak Hazard Rate, dh(a)/dperiod")) +
  ylim(c(0, 0.035))+
  #xlim(c(0,65))+
  theme_bw()+
  #theme(legend.position = c(.9, .68),
  #      legend.box.background = element_rect(colour = "white", size=2))+
  scale_x_continuous(breaks = number_ticks(19)) 
#scale_y_continuous(breaks = number_ticks(10)) 

plot4.mod1.ic.p.female_allyrs_rates

plot_rateofchange_female<- plot_rateofchange_female%>% mutate (sex = 'female', delta_h_t = as.double(c(h_t[1],diff(h_t) )))
plot_rateofchange_female$h_t[2]-plot_rateofchange_female$h_t[1]

plot_rateofchange_female$delta_h_t-plot_rateofchange_female$h_t_rateperyr
plot_rateofchange_male<- plot_rateofchange_male%>% mutate (sex = 'male')

plot_rateofchange_joint <- rbind(plot_rateofchange_female,plot_rateofchange_male)

plot5.mod1.ic.p.female_allyrs_rates <- ggplot(data = plot_rateofchange_joint, aes(x = as.numeric(Period), y = h_t_rateperyr, color = sex)) + 
  geom_point( size=0.5)+  
  #geom_point(data = df_surv.p.male, shape= df_surv.p.male$Period, size = 0.7) +
  labs(title = "PANEL-A: Annual Rate of Change of Peak Rates of Onset of Cannabis,  Females")+
  #       subtitle = "Flexible Parametric Model - Natural Cubic Spline (knots = 3)")+    
  xlab("Period (calendar years)") +
  ylab(bquote("Rates of Change of Peak Hazard Rate, dh(a)/dperiod")) +
  ylim(c(0, 0.035))+
  #xlim(c(0,65))+
  theme_bw()+
  #theme(legend.position = c(.9, .68),
  #      legend.box.background = element_rect(colour = "white", size=2))+
  scale_x_continuous(breaks = number_ticks(19)) 

plot5.mod1.ic.p.female_allyrs_rates
