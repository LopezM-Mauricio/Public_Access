# SPLINE FITTING : SURVIVAL AND HAZARD FUNCTIONS (Cohort) --------------------- #
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
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(viridis)
library(reshape2)
library(readxl)
library(openxlsx)
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# 1. LOAD DATA ---- 
# Interval and Right-Censored Data (1998-2016)
DMarihuana_1998_2016      <- read_excel("C:/Users/mlopezme/Dropbox (Brown)/Troubleshooting-Parametric Models and Splines/DS_SURV_MARIHUANA_1998_2016.xlsx")
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# 2. RECODING 4 ANALYSIS ----
## 2.1 Decennial Birth-cohorts -----
# cohorts range from year 1933 to year 2004, we roughy categorize them into decennial cohorts, the first category only
# includes 7 years and the last category includes 14. 

v_names_cohort_groups <- c('1930', #1933-1939
                           '1940', 
                           '1950', 
                           '1960', 
                           '1970',
                           '1980', 
                           '1990') #1990-2004

DMarihuana_1998_2016.c <- DMarihuana_1998_2016%>%
  mutate(Cohort.dec = cut(Cohort, c(1932,1939, 1949, 1959, 1969, 1979, 1989, 2004)))
levels(DMarihuana_1998_2016.c$Cohort.dec) = v_names_cohort_groups 
DMarihuana_1998_2016.c <- DMarihuana_1998_2016.c%>%
  mutate(Cohort.dec = as.numeric(Cohort.dec))
head(DMarihuana_1998_2016.c)
#Checking NAs in cohort variable
sum(as.numeric(is.na(DMarihuana_1998_2016.c$Cohort.dec)))
# ----------------------------------------------------------------------------- #
## 2.2  Strata   ----
DMarihuana_1998_2016$Sex  <- factor(DMarihuana_1998_2016$Sex, labels = c("Male", "Female"))
#Checking NAs in the response
#sum(1-as.numeric(is.na(DMarihuana_1998_2016$Age_Response)))
DMarihuana_1998_2016.c.male <- DMarihuana_1998_2016.c%>%
  filter(Sex == "Male")
#61658/141342
summary(DMarihuana_1998_2016.c.male)

DMarihuana_1998_2016.c.female <- DMarihuana_1998_2016.c%>%
  filter(Sex == "Female")
#79684/141342
summary(DMarihuana_1998_2016.c.female)
head(DMarihuana_1998_2016.c.female)
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# 3  LOG-RANK TEST ------
# For LR test we need to exclude 1998, the routine cannot handle interval censored data
DATA_02_16 <- filter(DMarihuana_1998_2016.c, Year!="1998")
SURV_ALL <- Surv(time  = DATA_02_16 $AgeR_Marihuana, 
                 event = DATA_02_16 $Event, 
                 type  = "right")
# Log-rank Test
logrank_cohort <- survdiff(SURV_ALL ~   factor(Sex) + factor(Cohort.dec), data = DATA_02_16)
logrank_cohort
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# 4. SURVIVAL OBJECTS ----

## 4.1 STRATA 1|Female ----
my.surv.ic.c.female <- Surv(time  = DMarihuana_1998_2016.c.female$AgeL_Marihuana, 
                            time2 = DMarihuana_1998_2016.c.female$AgeR_Marihuana, 
                            event = DMarihuana_1998_2016.c.female$Event, 
                            type  = "interval")
#restricted mean survival
np.fit.ic.c.female <- survfit(my.surv.ic.c.female ~ Cohort.dec, data = DMarihuana_1998_2016.c.female)
print(np.fit.ic.c.female, rm=18) #restricted mean survival
print(np.fit.ic.c.female, rm=25) #restricted mean survival
print(np.fit.ic.c.female, rm=65) #restricted mean survival
# KM PLOT
plot(np.fit.ic.c.female, main="Kaplan-Meier estimate with 95% confidence bounds",
     xlab="time", ylab="survival function")
# ----------------------------------------------------------------------------- #
## 4.2 STRATA 2| Male ----
my.surv.ic.c.male <- Surv(time  = DMarihuana_1998_2016.c.male$AgeL_Marihuana, 
                   time2 = DMarihuana_1998_2016.c.male$AgeR_Marihuana, 
                   event = DMarihuana_1998_2016.c.male$Event, 
                   type  = "interval")
#restricted mean survival
np.fit.ic.c.male <- survfit(my.surv.ic.c.male ~ Cohort.dec, data = DMarihuana_1998_2016.c.male)
print(np.fit.ic.c.male, rm=18) #restricted mean survival
print(np.fit.ic.c.male, rm=25) #restricted mean survival
print(np.fit.ic.c.male, rm=65) #restricted mean survival
# KM PLOT
plot(np.fit.ic.c.male, main="Kaplan-Meier estimate with 95% confidence bounds",
     xlab="time", ylab="survival function")

# Saving Surv objects and Data, by Cohort
model_inputs_cohort <- list(surv_objects = list(so_female_c=my.surv.ic.c.female, so_male_c=my.surv.ic.c.male), 
     data_dfs     = list(df_frmale_c=DMarihuana_1998_2016.c.female, dF_male_c =DMarihuana_1998_2016.c.male))

save(model_inputs_cohort, file = "Data_/model_inputs_cohort.RData")
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# 5. ANDERSON PLOTS ----
# NOTES: A VISUAL TEST OF PROPORTIONALITY OF HAZARDS ACROSS PERIODS, non-parallel
# trends indicate possible violation of proportional hazards
## 5.1 STRATA 1 | Female  ----
DMarihuana_1930ONLY.c.female <- filter(DMarihuana_1998_2016.c.female, Cohort.dec== 1)
DMarihuana_1940ONLY.c.female <- filter(DMarihuana_1998_2016.c.female, Cohort.dec== 2)
DMarihuana_1950ONLY.c.female <- filter(DMarihuana_1998_2016.c.female, Cohort.dec== 3)
DMarihuana_1960ONLY.c.female <- filter(DMarihuana_1998_2016.c.female, Cohort.dec== 4)
DMarihuana_1970ONLY.c.female <- filter(DMarihuana_1998_2016.c.female, Cohort.dec== 5)
DMarihuana_1980ONLY.c.female <- filter(DMarihuana_1998_2016.c.female, Cohort.dec== 6)
DMarihuana_1990ONLY.c.female <- filter(DMarihuana_1998_2016.c.female, Cohort.dec== 7)
# Surv objects
surv_female_1930 <-  Surv(time  = DMarihuana_1930ONLY.c.female$AgeL_Marihuana, 
                          time2 = DMarihuana_1930ONLY.c.female$AgeR_Marihuana, 
                          event = DMarihuana_1930ONLY.c.female$Event, 
                          type  = "interval")

surv_female_1940 <-  Surv(time  = DMarihuana_1940ONLY.c.female$AgeL_Marihuana, 
                          time2 = DMarihuana_1940ONLY.c.female$AgeR_Marihuana, 
                          event = DMarihuana_1940ONLY.c.female$Event, 
                          type  = "interval")

surv_female_1950 <-  Surv(time  = DMarihuana_1950ONLY.c.female$AgeL_Marihuana, 
                          time2 = DMarihuana_1950ONLY.c.female$AgeR_Marihuana, 
                          event = DMarihuana_1950ONLY.c.female$Event, 
                          type  = "interval")

surv_female_1960 <-  Surv(time  = DMarihuana_1960ONLY.c.female$AgeL_Marihuana, 
                          time2 = DMarihuana_1960ONLY.c.female$AgeR_Marihuana, 
                          event = DMarihuana_1960ONLY.c.female$Event, 
                          type  = "interval")

surv_female_1970 <-  Surv(time  = DMarihuana_1970ONLY.c.female$AgeL_Marihuana, 
                          time2 = DMarihuana_1970ONLY.c.female$AgeR_Marihuana, 
                          event = DMarihuana_1970ONLY.c.female$Event, 
                          type  = "interval")

surv_female_1980 <-  Surv(time  = DMarihuana_1980ONLY.c.female$AgeL_Marihuana, 
                          time2 = DMarihuana_1980ONLY.c.female$AgeR_Marihuana, 
                          event = DMarihuana_1980ONLY.c.female$Event, 
                          type  = "interval")

surv_female_1990 <-  Surv(time  = DMarihuana_1990ONLY.c.female$AgeL_Marihuana, 
                          time2 = DMarihuana_1990ONLY.c.female$AgeR_Marihuana, 
                          event = DMarihuana_1990ONLY.c.female$Event, 
                          type  = "interval")
# Cum hazards
np.fit.ic.c.female_1930ONLY <- survfit(surv_female_1930 ~ 1, data = DMarihuana_1930ONLY.c.female, type='fh')
np.fit.ic.c.female_1940ONLY <- survfit(surv_female_1940 ~ 1, data = DMarihuana_1940ONLY.c.female, type='fh')
np.fit.ic.c.female_1950ONLY <- survfit(surv_female_1950 ~ 1, data = DMarihuana_1950ONLY.c.female, type='fh')
np.fit.ic.c.female_1960ONLY <- survfit(surv_female_1960 ~ 1, data = DMarihuana_1960ONLY.c.female, type='fh')
np.fit.ic.c.female_1970ONLY <- survfit(surv_female_1970 ~ 1, data = DMarihuana_1970ONLY.c.female, type='fh')
np.fit.ic.c.female_1980ONLY <- survfit(surv_female_1980 ~ 1, data = DMarihuana_1980ONLY.c.female, type='fh')
np.fit.ic.c.female_1990ONLY <- survfit(surv_female_1990 ~ 1, data = DMarihuana_1990ONLY.c.female, type='fh')
# extracting log cum haz
# -log(S) = H
Ha_female_1930ONLY             <- -log(np.fit.ic.c.female_1930ONLY$surv)
Ha_female_1930ONLY_df          <- data.frame(CumHaz =Ha_female_1930ONLY)
Ha_female_1930ONLY_df$Cohort   <- "1930"
Ha_female_1930ONLY_df$Sex      <- "Female"
Ha_female_1930ONLY_df$Init.Age <- np.fit.ic.c.female_1930ONLY$time

Ha_female_1940ONLY             <- -log(np.fit.ic.c.female_1940ONLY$surv)
Ha_female_1940ONLY_df          <- data.frame(CumHaz =Ha_female_1940ONLY)
Ha_female_1940ONLY_df$Cohort   <- "1940"
Ha_female_1940ONLY_df$Sex      <- "Female"
Ha_female_1940ONLY_df$Init.Age <- np.fit.ic.c.female_1940ONLY$time

Ha_female_1950ONLY             <- -log(np.fit.ic.c.female_1950ONLY$surv)
Ha_female_1950ONLY_df          <- data.frame(CumHaz =Ha_female_1950ONLY)
Ha_female_1950ONLY_df$Cohort   <- "1950"
Ha_female_1950ONLY_df$Sex      <- "Female"
Ha_female_1950ONLY_df$Init.Age <- np.fit.ic.c.female_1950ONLY$time

Ha_female_1960ONLY             <- -log(np.fit.ic.c.female_1960ONLY$surv)
Ha_female_1960ONLY_df          <- data.frame(CumHaz =Ha_female_1960ONLY)
Ha_female_1960ONLY_df$Cohort   <- "1960"
Ha_female_1960ONLY_df$Sex      <- "Female"
Ha_female_1960ONLY_df$Init.Age <- np.fit.ic.c.female_1960ONLY$time

Ha_female_1970ONLY             <- -log(np.fit.ic.c.female_1970ONLY$surv)
Ha_female_1970ONLY_df          <- data.frame(CumHaz =Ha_female_1970ONLY)
Ha_female_1970ONLY_df$Cohort   <- "1970"
Ha_female_1970ONLY_df$Sex      <- "Female"
Ha_female_1970ONLY_df$Init.Age <- np.fit.ic.c.female_1970ONLY$time

Ha_female_1980ONLY             <- -log(np.fit.ic.c.female_1980ONLY$surv)
Ha_female_1980ONLY_df          <- data.frame(CumHaz =Ha_female_1980ONLY)
Ha_female_1980ONLY_df$Cohort   <- "1980"
Ha_female_1980ONLY_df$Sex      <- "Female"
Ha_female_1980ONLY_df$Init.Age <- np.fit.ic.c.female_1980ONLY$time

Ha_female_1990ONLY             <- -log(np.fit.ic.c.female_1990ONLY$surv)
Ha_female_1990ONLY_df          <- data.frame(CumHaz =Ha_female_1990ONLY)
Ha_female_1990ONLY_df$Cohort   <- "1990"
Ha_female_1990ONLY_df$Sex      <- "Female"
Ha_female_1990ONLY_df$Init.Age <- np.fit.ic.c.female_1990ONLY$time

Cum_haz_female_cohort          <- rbind(Ha_female_1930ONLY_df,
                               Ha_female_1940ONLY_df,
                               Ha_female_1950ONLY_df,
                               Ha_female_1960ONLY_df,
                               Ha_female_1970ONLY_df,
                               Ha_female_1980ONLY_df,
                               Ha_female_1990ONLY_df )
# Anderson Plot
plot( log(np.fit.ic.c.female_1930ONLY$time),log(Ha_female_1930ONLY), type = "s",lty = 5, col = "black",
      ylim =c(-10.5,-.5),  xlim =c(-0,5),xlab = "Log(Age)", ylab = "Log H(t)",
      main = "Anderson Plot-Proportionality Test of Cohort, for Females"
      , sub= "Log Cumulative Hazard vs Log Age by Decennial Birth -Cohort")
lines(log(np.fit.ic.c.female_1940ONLY$time),log(Ha_female_1940ONLY), type = "s",lty = 5, col = "blue")
lines(log(np.fit.ic.c.female_1950ONLY$time),log(Ha_female_1950ONLY), type = "s",lty = 5, col = "red")
lines(log(np.fit.ic.c.female_1960ONLY$time),log(Ha_female_1960ONLY), type = "s",lty = 5, col = "green")
lines(log(np.fit.ic.c.female_1970ONLY$time),log(Ha_female_1970ONLY), type = "s",lty = 5, col = "yellow")
lines(log(np.fit.ic.c.female_1980ONLY$time),log(Ha_female_1980ONLY), type = "s",lty = 5, col = "orange")
lines(log(np.fit.ic.c.female_1990ONLY$time),log(Ha_female_1990ONLY), type = "s",lty = 5, col = "purple")
abline(v=log(13), col = "gray", lty = 3)
abline(v=log(16), col = "gray", lty = 3)
legend("topleft", legend=c( "1990","1980","1970","1960","1950", "1940", "1930"),
       col=c("purple","orange", "yellow","green","red","blue","black"), lty=5, cex=0.8)

# ----------------------------------------------------------------------------- #
## 5.2 STRATA 2 | Male  ----
# Subsets by Cohort
DMarihuana_1930ONLY.c.male <- filter(DMarihuana_1998_2016.c.male, Cohort.dec== 1)
DMarihuana_1940ONLY.c.male <- filter(DMarihuana_1998_2016.c.male, Cohort.dec== 2)
DMarihuana_1950ONLY.c.male <- filter(DMarihuana_1998_2016.c.male, Cohort.dec== 3)
DMarihuana_1960ONLY.c.male <- filter(DMarihuana_1998_2016.c.male, Cohort.dec== 4)
DMarihuana_1970ONLY.c.male <- filter(DMarihuana_1998_2016.c.male, Cohort.dec== 5)
DMarihuana_1980ONLY.c.male <- filter(DMarihuana_1998_2016.c.male, Cohort.dec== 6)
DMarihuana_1990ONLY.c.male <- filter(DMarihuana_1998_2016.c.male, Cohort.dec== 7)
# Surv objects by Cohort
surv_male_1930 <-  Surv(time  = DMarihuana_1930ONLY.c.male$AgeL_Marihuana, 
                        time2 = DMarihuana_1930ONLY.c.male$AgeR_Marihuana, 
                        event = DMarihuana_1930ONLY.c.male$Event, 
                        type  = "interval")

surv_male_1940 <-  Surv(time  = DMarihuana_1940ONLY.c.male$AgeL_Marihuana, 
                        time2 = DMarihuana_1940ONLY.c.male$AgeR_Marihuana, 
                        event = DMarihuana_1940ONLY.c.male$Event, 
                        type  = "interval")

surv_male_1950 <-  Surv(time  = DMarihuana_1950ONLY.c.male$AgeL_Marihuana, 
                        time2 = DMarihuana_1950ONLY.c.male$AgeR_Marihuana, 
                        event = DMarihuana_1950ONLY.c.male$Event, 
                        type  = "interval")

surv_male_1960 <-  Surv(time  = DMarihuana_1960ONLY.c.male$AgeL_Marihuana, 
                        time2 = DMarihuana_1960ONLY.c.male$AgeR_Marihuana, 
                        event = DMarihuana_1960ONLY.c.male$Event, 
                        type  = "interval")

surv_male_1970 <-  Surv(time  = DMarihuana_1970ONLY.c.male$AgeL_Marihuana, 
                        time2 = DMarihuana_1970ONLY.c.male$AgeR_Marihuana, 
                        event = DMarihuana_1970ONLY.c.male$Event, 
                        type  = "interval")

surv_male_1980 <-  Surv(time  = DMarihuana_1980ONLY.c.male$AgeL_Marihuana, 
                        time2 = DMarihuana_1980ONLY.c.male$AgeR_Marihuana, 
                        event = DMarihuana_1980ONLY.c.male$Event, 
                        type  = "interval")

surv_male_1990 <-  Surv(time  = DMarihuana_1990ONLY.c.male$AgeL_Marihuana, 
                        time2 = DMarihuana_1990ONLY.c.male$AgeR_Marihuana, 
                        event = DMarihuana_1990ONLY.c.male$Event, 
                        type  = "interval")

# Cum hazards
np.fit.ic.c.male_1930ONLY <- survfit(surv_male_1930 ~ 1, data = DMarihuana_1930ONLY.c.male, type='fh')
np.fit.ic.c.male_1940ONLY <- survfit(surv_male_1940 ~ 1, data = DMarihuana_1940ONLY.c.male, type='fh')
np.fit.ic.c.male_1950ONLY <- survfit(surv_male_1950 ~ 1, data = DMarihuana_1950ONLY.c.male, type='fh')
np.fit.ic.c.male_1960ONLY <- survfit(surv_male_1960 ~ 1, data = DMarihuana_1960ONLY.c.male, type='fh')
np.fit.ic.c.male_1970ONLY <- survfit(surv_male_1970 ~ 1, data = DMarihuana_1970ONLY.c.male, type='fh')
np.fit.ic.c.male_1980ONLY <- survfit(surv_male_1980 ~ 1, data = DMarihuana_1980ONLY.c.male, type='fh')
np.fit.ic.c.male_1990ONLY <- survfit(surv_male_1990 ~ 1, data = DMarihuana_1990ONLY.c.male, type='fh')

# extracting log cum haz
# -log(S) = H
Ha_male_1930ONLY             <- -log(np.fit.ic.c.male_1930ONLY$surv)
Ha_male_1930ONLY_df          <- data.frame(CumHaz =Ha_male_1930ONLY)
Ha_male_1930ONLY_df$Cohort   <- "1930"
Ha_male_1930ONLY_df$Sex      <- "Male"
Ha_male_1930ONLY_df$Init.Age <- np.fit.ic.c.male_1930ONLY$time

Ha_male_1940ONLY             <- -log(np.fit.ic.c.male_1940ONLY$surv)
Ha_male_1940ONLY_df          <- data.frame(CumHaz =Ha_male_1940ONLY)
Ha_male_1940ONLY_df$Cohort   <- "1940"
Ha_male_1940ONLY_df$Sex      <- "Male"
Ha_male_1940ONLY_df$Init.Age <- np.fit.ic.c.male_1940ONLY$time

Ha_male_1950ONLY             <- -log(np.fit.ic.c.male_1950ONLY$surv)
Ha_male_1950ONLY_df          <- data.frame(CumHaz =Ha_male_1950ONLY)
Ha_male_1950ONLY_df$Cohort   <- "1950"
Ha_male_1950ONLY_df$Sex      <- "Male"
Ha_male_1950ONLY_df$Init.Age <- np.fit.ic.c.male_1950ONLY$time

Ha_male_1960ONLY             <- -log(np.fit.ic.c.male_1960ONLY$surv)
Ha_male_1960ONLY_df          <- data.frame(CumHaz =Ha_male_1960ONLY)
Ha_male_1960ONLY_df$Cohort   <- "1960"
Ha_male_1960ONLY_df$Sex      <- "Male"
Ha_male_1960ONLY_df$Init.Age <- np.fit.ic.c.male_1960ONLY$time

Ha_male_1970ONLY             <- -log(np.fit.ic.c.male_1970ONLY$surv)
Ha_male_1970ONLY_df          <- data.frame(CumHaz =Ha_male_1970ONLY)
Ha_male_1970ONLY_df$Cohort   <- "1970"
Ha_male_1970ONLY_df$Sex      <- "Male"
Ha_male_1970ONLY_df$Init.Age <- np.fit.ic.c.male_1970ONLY$time

Ha_male_1980ONLY             <- -log(np.fit.ic.c.male_1980ONLY$surv)
Ha_male_1980ONLY_df          <- data.frame(CumHaz =Ha_male_1980ONLY)
Ha_male_1980ONLY_df$Cohort   <- "1980"
Ha_male_1980ONLY_df$Sex      <- "Male"
Ha_male_1980ONLY_df$Init.Age <- np.fit.ic.c.male_1980ONLY$time

Ha_male_1990ONLY             <- -log(np.fit.ic.c.male_1990ONLY$surv)
Ha_male_1990ONLY_df          <- data.frame(CumHaz =Ha_male_1990ONLY)
Ha_male_1990ONLY_df$Cohort   <- "1990"
Ha_male_1990ONLY_df$Sex      <- "Male"
Ha_male_1990ONLY_df$Init.Age <- np.fit.ic.c.male_1990ONLY$time

Cum_haz_male_cohort <- rbind(Ha_male_1930ONLY_df,
                             Ha_male_1940ONLY_df,
                             Ha_male_1950ONLY_df,
                             Ha_male_1960ONLY_df,
                             Ha_male_1970ONLY_df ,
                             Ha_male_1980ONLY_df,
                             Ha_male_1990ONLY_df )
# Anderson Plot
plot( log(np.fit.ic.c.male_1930ONLY$time),log(Ha_male_1930ONLY), type = "s",lty = 5, col = "black",
      ylim =c(-10.5,-.5),  xlim =c(-0,5),xlab = "Log(Age)", ylab = "Log H(t)",
      main = "Anderson Plot-Proportionality Test of Cohort, for Males"
      , sub= "Log Cumulative Hazard vs Log Age by  Decennial Birth -Cohort")
lines(log(np.fit.ic.c.male_1940ONLY$time),log(Ha_male_1940ONLY), type = "s",lty = 5, col = "blue")
lines(log(np.fit.ic.c.male_1950ONLY$time),log(Ha_male_1950ONLY), type = "s",lty = 5, col = "red")
lines(log(np.fit.ic.c.male_1960ONLY$time),log(Ha_male_1960ONLY), type = "s",lty = 5, col = "green")
lines(log(np.fit.ic.c.male_1970ONLY$time),log(Ha_male_1970ONLY), type = "s",lty = 5, col = "yellow")
lines(log(np.fit.ic.c.male_1980ONLY$time),log(Ha_male_1980ONLY), type = "s",lty = 5, col = "orange")
lines(log(np.fit.ic.c.male_1990ONLY$time),log(Ha_male_1990ONLY), type = "s",lty = 5, col = "purple")
abline(v=log(13), col = "gray", lty = 3)
abline(v=log(16), col = "gray", lty = 3)
legend("topleft", legend=c( "1990","1980","1970","1960","1950", "1940", "1930"),
       col=c("purple","orange", "yellow","green","red","blue", "black"), lty=5, cex=0.8)


## 5.3 STRATA 1 AND 2 ----

Cum_haz_all_cohort <- rbind(Cum_haz_female_cohort ,Cum_haz_male_cohort )

# Anderson Plot
plot_all_cumhaz_cohort <- ggplot(data = Cum_haz_all_cohort, aes(x = (Init.Age), y = log(CumHaz), color= Cohort, fill = Cohort)) + 
  geom_step(linetype="solid", size=0.4)+
  geom_point(alpha =0.5)+
  #geom_vline(xintercept = c(16,30), size =0.2, lty =2)+
  labs(title = "Anderson Plot: Log Cumulative Rates of Onset (Kaplan-Meier) vs Age by Sex and Decennial Cohort "
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
  scale_colour_manual(values = inferno(7, alpha = 1, begin = 0, end = .8, direction = 1))+
  scale_fill_manual(values = inferno(7, alpha = 1, begin = 0, end = 1, direction = 1))+
  facet_wrap(~Sex)

plot_all_cumhaz_cohort
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# 6. SURVIVAL ANALYSIS (NON-PARAMETRIC) ----
# Notes: Kaplan-Meier based estimates of Hazards            

## 6.1 STRATA 1 | Female  ----

df_surv.c.female <- data.frame(Cohort = c( rep('1930',   np.fit.ic.c.female$strata[1]) , 
                                           rep('1940',   np.fit.ic.c.female$strata[2]) ,
                                           rep('1950',   np.fit.ic.c.female$strata[3]) ,
                                           rep('1960',   np.fit.ic.c.female$strata[4]) ,
                                           rep('1970',   np.fit.ic.c.female$strata[5]) ,
                                           rep('1980',   np.fit.ic.c.female$strata[6]) ,
                                           rep('1990',   np.fit.ic.c.female$strata[7])) ,
                                 Sex = c(rep("Female",   np.fit.ic.c.female$strata[1]) , 
                                         rep("Female",   np.fit.ic.c.female$strata[2])    , 
                                         rep("Female",   np.fit.ic.c.female$strata[3]) , 
                                         rep("Female",   np.fit.ic.c.female$strata[4])    ,
                                         rep("Female",   np.fit.ic.c.female$strata[5]) , 
                                         rep("Female",   np.fit.ic.c.female$strata[6])    ,
                                         rep("Female",   np.fit.ic.c.female$strata[7])),
                                Init.Age      =          np.fit.ic.c.female$time, 
                               `At Risk`      =    as.integer(np.fit.ic.c.female$n.risk), 
                               `Events`       =    as.integer(np.fit.ic.c.female$n.event), 
                               S_t            =    np.fit.ic.c.female$surv,
                               LB             =    np.fit.ic.c.female$lower,
                               UB             =    np.fit.ic.c.female$upper,
                               F_t            =    1-np.fit.ic.c.female$surv, 
                               H_t            =    NA,
                               check.names    = FALSE)

# Computing h(t), f(t) 
# h(t): hazard, H(t), cumulative hazard which can be obtained as the -log transformation of the survival function
head(df_surv.c.female)
df_surv.c.female <- df_surv.c.female %>%
  group_by(Cohort) %>%
  mutate(f_t = c(F_t[1], diff(F_t)),
         h_t = f_t/S_t,
         H_t = -log(S_t))%>% 
  dplyr::select(Cohort, Init.Age, `At Risk`, Events, S_t, LB, UB, F_t, H_t, h_t, f_t)
# ----------------------------------------------------------------------------- #
# Quick Check on Age of peak Hazard, 
age.peak.c.female.np <-df_surv.c.female%>%
  mutate( age.maxr = ifelse(h_t == max(h_t), Init.Age, NA))%>%
  group_by(Cohort, age.maxr)%>%
  summarize( rate.max = max(h_t))
age.peak.c.female.np
# ----------------------------------------------------------------------------- #
# Survival Curve Plot 
require(dampack) #for #ticks
plot1.km.surv.female <-  ggplot(df_surv.c.female, aes(x = Init.Age, y = S_t))+
  labs(title = "Panel-D: Kaplan-Meier Survival Estimates by Birth-Cohorts of Females",
       subtitle = "95% Confidence Intervals") +
  labs(y="Survival Probabilities", x = "Age")+
  geom_linerange(aes(ymin = LB, ymax = UB, color = Cohort),size=0.5, alpha =0.5)+
  geom_step(aes(color = Cohort))+
  ylim(c(0.75, 1))+
  #scale_x_continuous(breaks = number_ticks(13)) +
  theme_bw()
plot1.km.surv.female 
# Hazard Curve Plot
plot2.km.haz.female <-  ggplot(df_surv.c.female, aes(x = Init.Age, y = h_t, color = Cohort)) +
     labs(title = "Kaplan-Meier Initiation Rates Estimates by Birth-Cohorts of Females") +
     labs(y=" Rate of Cannabis Initiation, h(a)", x = "Age")+
     geom_point(aes(color = Cohort), alpha=0.3)+
     #geom_line( size =0.5)+
     ylim(c(0,.037))+
     scale_x_continuous(breaks = number_ticks(13)) +
     theme_bw()
plot2.km.haz.female
#Saving Km plots
save(plot1.km.surv.female, plot2.km.haz.female, file="E_Tables_Figures/km.plots.cohort.female.RData")
# ----------------------------------------------------------------------------- #
## 6.2 STRATA 1 | Male    ----

df_surv.c.male <- data.frame(Cohort = c( rep('1930',   np.fit.ic.c.male$strata[1]) , 
                                         rep('1940',   np.fit.ic.c.male$strata[2]) ,
                                         rep('1950',   np.fit.ic.c.male$strata[3]) ,
                                         rep('1960',   np.fit.ic.c.male$strata[4]) ,
                                         rep('1970',   np.fit.ic.c.male$strata[5]) ,
                                         rep('1980',   np.fit.ic.c.male$strata[6]) ,
                                         rep('1990',   np.fit.ic.c.male$strata[7])) ,
                                 Sex = c(rep("Male" ,  np.fit.ic.c.male$strata[1]) , 
                                         rep("Male",   np.fit.ic.c.male$strata[2])    , 
                                         rep("Male",   np.fit.ic.c.male$strata[3]) , 
                                         rep("Male",   np.fit.ic.c.male$strata[4])    ,
                                         rep("Male",   np.fit.ic.c.male$strata[5]) , 
                                         rep("Male",  np.fit.ic.c.male$strata[6])    ,
                                         rep("Male",  np.fit.ic.c.male$strata[7])),
                        Init.Age  = np.fit.ic.c.male$time, 
                        `At Risk` = as.integer(np.fit.ic.c.male$n.risk), 
                        `Events`  = as.integer(np.fit.ic.c.male$n.event), 
                        S_t       =    np.fit.ic.c.male$surv,
                        LB        =    np.fit.ic.c.male$lower,
                        UB        =    np.fit.ic.c.male$upper,
                        F_t       =  1 - np.fit.ic.c.male$surv, 
                        H_t       =    NA,
                        check.names = FALSE)

# Computing h(t), f(t) 
# h(t): hazard, H(t), cumulative hazard which can be obtained as the -log transformation of the survival function
df_surv.c.male <- df_surv.c.male %>%
  group_by(Cohort) %>%
  mutate(f_t = c(F_t[1], diff(F_t)),
         h_t = f_t/S_t, 
         H_t = -log(S_t))%>%
  dplyr::select(Cohort, Init.Age, `At Risk`, Events, S_t, LB, UB, F_t, H_t, h_t, f_t)

head(df_surv.c.male)

# ----------------------------------------------------------------------------- #
# Quick Check on Age of peak Hazard 
age.peak.c.male.np <-df_surv.c.male%>%
  mutate( age.maxr = ifelse(h_t == max(h_t), Init.Age, NA))%>%
  group_by(Cohort, age.maxr)%>%
  summarize( rate.max = max(h_t))
age.peak.c.male.np
# ----------------------------------------------------------------------------- #
# Survival Curve Plot 
require(dampack) # we need it for the #ticks
plot1.km.surv.male <-ggplot(df_surv.c.male, aes(x = Init.Age, y = S_t))+
  labs(title="Panel-C: Kaplan-Meier Survival Estimates by Birth-Cohort of Males",
       subtitle = "95% Confidence Intervals") +
  labs(y="Survival Probabilities", x = "Age")+
  geom_linerange(aes(ymin = LB, ymax = UB, color = Cohort), size = 0.5,alpha =0.7)+
  geom_step(aes(color = Cohort))+
  ylim(c(0.75,1))+
  scale_x_continuous(breaks = number_ticks(13)) +
  theme_bw()
plot1.km.surv.male
# Hazard Curve Plot
plot2.km.haz.male<-ggplot(df_surv.c.male, aes(x = Init.Age, y = h_t, color = Cohort)) +
  labs(title="Kaplan-Meier Initiation Rates Estimates by Birth-Cohort for Males") +
  labs(y=" Rate of Cannabis Initiation, h(a)", x = "Age")+
  geom_point(aes(color = Cohort), alpha=0.3)+
  #geom_line( size =0.5)+
  ylim(c(0,0.037))+
  scale_x_continuous(breaks = number_ticks(13)) +
  theme_bw()
plot2.km.haz.male
#Saving 
save(plot1.km.surv.male, plot2.km.haz.male, file="E_Tables_Figures/km.plots.cohort.male.RData")
# ----------------------------------------------------------------------------- #
## 6.3 Facet plot all Strata ----
colnames(df_surv.c.female)
colnames(df_surv.c.male)
df_surv.c.female$Sex <- "Female"
df_surv.c.male$Sex <- "Male"

df_surv_all_c <- rbind(df_surv.c.female,df_surv.c.male) 

save(df_surv_all_c, file = "df_surv_all_c.RData") # saving to plot together

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

ggsave(
  "plot_surv_all_cohort.png",
  plot = plot_surv_all_cohort,
  device = "png",
  path = "E_Tables_Figure/Surv_all_plot",
  scale = 1,
  width = 10,
  height = 6,
  units = "in",
  dpi = 300,
  limitsize = F
)
