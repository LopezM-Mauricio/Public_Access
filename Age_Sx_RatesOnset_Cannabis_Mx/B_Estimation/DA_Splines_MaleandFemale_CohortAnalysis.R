# Using subsets of the data split into the two sex categories: Male, FeMale
# The reason is that survivial times seem to distribute quite differently for ach population
# We get better fits using this analysis,but consider we are using less data


# ---------------------------------------------------------------------------------------------------------- #
# ======================== SPLINE FITTING : SURVIVAL AND HAZARD FUNCTIONS (Cohort) ========================= #
# DATA: interval censored and right censored (1998-2016)
# ANALYSIS: include  predictors (Cohort) 
#   KM:  Non-parametric survival and hazard estimates through kaplan-meier estimator
#   SPLINES: different specifications on the splines (# of knots on log cum hazard/odds, #of knots on Cohort, 
#   also in time-varying or linear predictors (Cohort)
#   OUTPUT: lists, containing fit objects from spline models + AIC and Coefficients for each specification
#   PLOTS: for KM estiates, Splines, comparison of hazards KM vs Splines
# AUTHOR: MLM- NOV, 26/2019
# ---------------------------------------------------------------------------------------------------------- #

#Load libraries

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

# ---------------------------------------------------------------------------------------------------------- #

# LOADING DATA  ============================= #
# =========================================== #

# # Right-Censored Data (2002-2016)
# DMarihuana_2002_2016 <- read_excel("DS_SURV_MARIHUANA_2002_2016.xlsx")
# DMarihuana_2002_2016$Sex  <- factor(DMarihuana_2002_2016$Sex, labels = c("Male", "Female"))
#head(DMarihuana_2002_2016)

# Interval and Right-Censored Data (1998-2016)
DMarihuana_1998_2016 <- read_excel("C:/Users/mlopezme/Dropbox (Brown)/Troubleshooting-Parametric Models and Splines/DS_SURV_MARIHUANA_1998_2016.xlsx")

DMarihuana_1998_2016$Sex  <- factor(DMarihuana_1998_2016$Sex, labels = c("Male", "Female"))
#head(DMarihuana_1998_2016)
#summary(DMarihuana_1998_2016)
#sum(1-as.numeric(is.na(DMarihuana_1998_2016$Age_Response)))
# ---------------------------------------------------------------------------------------------------------- #
# Creating decennial (10yrs) birth-cohorts ============================= #
# ====================================================================== #


# 2002-2016
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

sum(as.numeric(is.na(DMarihuana_1998_2016.c$Cohort.dec)))

#str(DMarihuana_1998_2016.c )
#head(DMarihuana_1998_2016.c )
#table(DMarihuana_1998_2016.c$Cohort, DMarihuana_1998_2016.c$Cohort.dec)

# LOG-RANK TEST ----------------------------------#
DATA_02_16 <- filter(DMarihuana_1998_2016.c, Year!="1998")
SURV_ALL <- Surv(time  = DATA_02_16 $AgeR_Marihuana, 
                 event = DATA_02_16 $Event, 
                 type  = "right")

plot(survfit(SURV_ALL ~ factor(Sex) + factor(Cohort.dec), data = DATA_02_16))

logrank_cohort <- survdiff(SURV_ALL ~   factor(Sex) + factor(Cohort.dec), data = DATA_02_16)
logrank_cohort

#write.xlsx(logrank_cohort, file ="/C:/Users/mlopezme/Dropbox (Brown)/Troubleshooting-Parametric Models and Splines/Tables/tables_logrank_cohort.xlsx")


# =========== DATA USED FOR ESTIMATION ========= #
#DMarihuana_1998_2016.c.male and DMarihuana_1998_2016.c.female
#
# Non parametric KM,  Male population
DMarihuana_1998_2016.c.male <- DMarihuana_1998_2016.c%>%
  filter(Sex == "Male")
#61658/141342
summary(DMarihuana_1998_2016.c.male)

DMarihuana_1998_2016.c.female <- DMarihuana_1998_2016.c%>%
  filter(Sex == "Female")
#79684/141342
summary(DMarihuana_1998_2016.c.female)
head(DMarihuana_1998_2016.c.female)

# SURVIVAL OBJETS
#male
my.surv.ic.c.male <- Surv(time  = DMarihuana_1998_2016.c.male$AgeL_Marihuana, 
                   time2 = DMarihuana_1998_2016.c.male$AgeR_Marihuana, 
                   event = DMarihuana_1998_2016.c.male$Event, 
                   type  = "interval")
#head(my.surv.ic.c.male)

np.fit.ic.c.male <- survfit(my.surv.ic.c.male ~ Cohort.dec, data = DMarihuana_1998_2016.c.male)
print(np.fit.ic.c.male, rm=18) #restricted mean survival
print(np.fit.ic.c.male, rm=25) #restricted mean survival
print(np.fit.ic.c.male, rm=65) #restricted mean survival

plot(np.fit.ic.c.male, main="Kaplan-Meier estimate with 95% confidence bounds",
     xlab="time", ylab="survival function")

#log-rank test between birth cohorts
#tbd


# female
my.surv.ic.c.female <- Surv(time  = DMarihuana_1998_2016.c.female$AgeL_Marihuana, 
                          time2 = DMarihuana_1998_2016.c.female$AgeR_Marihuana, 
                          event = DMarihuana_1998_2016.c.female$Event, 
                          type  = "interval")
#head(my.surv.ic.c.female)

np.fit.ic.c.female <- survfit(my.surv.ic.c.female ~ Cohort.dec, data = DMarihuana_1998_2016.c.female)
print(np.fit.ic.c.female, rm=18) #restricted mean survival
print(np.fit.ic.c.female, rm=25) #restricted mean survival
print(np.fit.ic.c.female, rm=65) #restricted mean survival

plot(np.fit.ic.c.female, main="Kaplan-Meier estimate with 95% confidence bounds",
     xlab="time", ylab="survival function")

# Saving Surv objects and Data, by Cohort

model_inputs_cohort <- list(surv_objects = list(so_female_c=my.surv.ic.c.female, so_male_c=my.surv.ic.c.male), 
     data_dfs     = list(df_frmale_c=DMarihuana_1998_2016.c.female, dF_male_c =DMarihuana_1998_2016.c.male))

#save(model_inputs_cohort, file = "/Users/mauriciolopezm/Dropbox (Brown)/Troubleshooting-Parametric Models and Splines/Code/Data_Objects/model_inputs_cohort.RData")

# TESTING PROPORTINALITY OF HAZARDS ACROSS PERIODS           ============= #
# ======================================================================== #
#MALES, SUBSETTING DATA BY PERIOD

colnames(DMarihuana_1998_2016.c.male)
unique(DMarihuana_1998_2016.c.male$Cohort)
unique(DMarihuana_1998_2016.c.male$Cohort.dec)
DMarihuana_1930ONLY.c.male <- filter(DMarihuana_1998_2016.c.male, Cohort.dec== 1)
DMarihuana_1940ONLY.c.male <- filter(DMarihuana_1998_2016.c.male, Cohort.dec== 2)
DMarihuana_1950ONLY.c.male <- filter(DMarihuana_1998_2016.c.male, Cohort.dec== 3)
DMarihuana_1960ONLY.c.male <- filter(DMarihuana_1998_2016.c.male, Cohort.dec== 4)
DMarihuana_1970ONLY.c.male <- filter(DMarihuana_1998_2016.c.male, Cohort.dec== 5)
DMarihuana_1980ONLY.c.male <- filter(DMarihuana_1998_2016.c.male, Cohort.dec== 6)
DMarihuana_1990ONLY.c.male <- filter(DMarihuana_1998_2016.c.male, Cohort.dec== 7)

# Surv objects
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
#str(summary(np.fit.ic.c.male_1998ONLY))
#summary(np.fit.ic.c.male_1998ONLY)
# -log(S) = H
Ha_male_1930ONLY <- -log(np.fit.ic.c.male_1930ONLY$surv)
Ha_male_1930ONLY_df <- data.frame(CumHaz =Ha_male_1930ONLY)
Ha_male_1930ONLY_df$Cohort <- "1930"
Ha_male_1930ONLY_df$Sex    <- "Male"
Ha_male_1930ONLY_df$Init.Age <- np.fit.ic.c.male_1930ONLY$time

Ha_male_1940ONLY <- -log(np.fit.ic.c.male_1940ONLY$surv)
Ha_male_1940ONLY_df <- data.frame(CumHaz =Ha_male_1940ONLY)
Ha_male_1940ONLY_df$Cohort <- "1940"
Ha_male_1940ONLY_df$Sex    <- "Male"
Ha_male_1940ONLY_df$Init.Age <- np.fit.ic.c.male_1940ONLY$time

Ha_male_1950ONLY <- -log(np.fit.ic.c.male_1950ONLY$surv)
Ha_male_1950ONLY_df <- data.frame(CumHaz =Ha_male_1950ONLY)
Ha_male_1950ONLY_df$Cohort <- "1950"
Ha_male_1950ONLY_df$Sex    <- "Male"
Ha_male_1950ONLY_df$Init.Age <- np.fit.ic.c.male_1950ONLY$time

Ha_male_1960ONLY <- -log(np.fit.ic.c.male_1960ONLY$surv)
Ha_male_1960ONLY_df <- data.frame(CumHaz =Ha_male_1960ONLY)
Ha_male_1960ONLY_df$Cohort <- "1960"
Ha_male_1960ONLY_df$Sex    <- "Male"
Ha_male_1960ONLY_df$Init.Age <- np.fit.ic.c.male_1960ONLY$time

Ha_male_1970ONLY <- -log(np.fit.ic.c.male_1970ONLY$surv)
Ha_male_1970ONLY_df <- data.frame(CumHaz =Ha_male_1970ONLY)
Ha_male_1970ONLY_df$Cohort <- "1970"
Ha_male_1970ONLY_df$Sex    <- "Male"
Ha_male_1970ONLY_df$Init.Age <- np.fit.ic.c.male_1970ONLY$time

Ha_male_1980ONLY <- -log(np.fit.ic.c.male_1980ONLY$surv)
Ha_male_1980ONLY_df <- data.frame(CumHaz =Ha_male_1980ONLY)
Ha_male_1980ONLY_df$Cohort <- "1980"
Ha_male_1980ONLY_df$Sex    <- "Male"
Ha_male_1980ONLY_df$Init.Age <- np.fit.ic.c.male_1980ONLY$time

Ha_male_1990ONLY <- -log(np.fit.ic.c.male_1990ONLY$surv)
Ha_male_1990ONLY_df <- data.frame(CumHaz =Ha_male_1990ONLY)
Ha_male_1990ONLY_df$Cohort <- "1990"
Ha_male_1990ONLY_df$Sex    <- "Male"
Ha_male_1990ONLY_df$Init.Age <- np.fit.ic.c.male_1990ONLY$time

Cum_haz_male_cohort <- rbind(Ha_male_1930ONLY_df,
                             Ha_male_1940ONLY_df,
                             Ha_male_1950ONLY_df,
                             Ha_male_1960ONLY_df,
                             Ha_male_1970ONLY_df ,
                             Ha_male_1980ONLY_df,
                             Ha_male_1990ONLY_df )
# Plots
 #plot(log(np.fit.ic.c.male_1930ONLY$time),log(Ha_male_1930ONLY), type = "s", ylim =c(-10.5,0),  xlim =c(0,4.5),xlab = "log(Time in Years)", ylab = "Log H(t)", lty = 5)
plot( log(np.fit.ic.c.male_1930ONLY$time),log(Ha_male_1930ONLY), type = "s",lty = 5, col = "black",
      ylim =c(-10.5,-.5),  xlim =c(-0,5),xlab = "Log(Age)", ylab = "Log H(t)",
      main = "Anderson Plot-Proportionality Test of Cohort, for Males"
      , sub= "Log Cumulative Hazard vs Log Age by Sex and Decennial Birth -Cohort")
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

# We observe that proportionality seems to hold at least for all, the first years are a bit problematic
 #----------------------------------
#FEMALES, SUBSETTING DATA BY PERIOD

colnames(DMarihuana_1998_2016.c.female)
unique(DMarihuana_1998_2016.c.female$Cohort)
unique(DMarihuana_1998_2016.c.female$Cohort.dec)
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
#str(summary(np.fit.ic.c.female_1998ONLY))
#summary(np.fit.ic.c.female_1998ONLY)
# -log(S) = H
Ha_female_1930ONLY <- -log(np.fit.ic.c.female_1930ONLY$surv)
Ha_female_1930ONLY_df <- data.frame(CumHaz =Ha_female_1930ONLY)
Ha_female_1930ONLY_df$Cohort  <- "1930"
Ha_female_1930ONLY_df$Sex      <- "Female"
Ha_female_1930ONLY_df$Init.Age <- np.fit.ic.c.female_1930ONLY$time

Ha_female_1940ONLY <- -log(np.fit.ic.c.female_1940ONLY$surv)
Ha_female_1940ONLY_df <- data.frame(CumHaz =Ha_female_1940ONLY)
Ha_female_1940ONLY_df$Cohort   <- "1940"
Ha_female_1940ONLY_df$Sex      <- "Female"
Ha_female_1940ONLY_df$Init.Age <- np.fit.ic.c.female_1940ONLY$time

Ha_female_1950ONLY <- -log(np.fit.ic.c.female_1950ONLY$surv)
Ha_female_1950ONLY_df <- data.frame(CumHaz =Ha_female_1950ONLY)
Ha_female_1950ONLY_df$Cohort    <- "1950"
Ha_female_1950ONLY_df$Sex      <- "Female"
Ha_female_1950ONLY_df$Init.Age <- np.fit.ic.c.female_1950ONLY$time

Ha_female_1960ONLY <- -log(np.fit.ic.c.female_1960ONLY$surv)
Ha_female_1960ONLY_df <- data.frame(CumHaz =Ha_female_1960ONLY)
Ha_female_1960ONLY_df$Cohort    <- "1960"
Ha_female_1960ONLY_df$Sex      <- "Female"
Ha_female_1960ONLY_df$Init.Age <- np.fit.ic.c.female_1960ONLY$time

Ha_female_1970ONLY <- -log(np.fit.ic.c.female_1970ONLY$surv)
Ha_female_1970ONLY_df <- data.frame(CumHaz =Ha_female_1970ONLY)
Ha_female_1970ONLY_df$Cohort    <- "1970"
Ha_female_1970ONLY_df$Sex      <- "Female"
Ha_female_1970ONLY_df$Init.Age <- np.fit.ic.c.female_1970ONLY$time

Ha_female_1980ONLY <- -log(np.fit.ic.c.female_1980ONLY$surv)
Ha_female_1980ONLY_df <- data.frame(CumHaz =Ha_female_1980ONLY)
Ha_female_1980ONLY_df$Cohort    <- "1980"
Ha_female_1980ONLY_df$Sex      <- "Female"
Ha_female_1980ONLY_df$Init.Age <- np.fit.ic.c.female_1980ONLY$time

Ha_female_1990ONLY <- -log(np.fit.ic.c.female_1990ONLY$surv)
Ha_female_1990ONLY_df <- data.frame(CumHaz =Ha_female_1990ONLY)
Ha_female_1990ONLY_df$Cohort    <- "1990"
Ha_female_1990ONLY_df$Sex      <- "Female"
Ha_female_1990ONLY_df$Init.Age <- np.fit.ic.c.female_1990ONLY$time

Cum_haz_female_cohort <- rbind(Ha_female_1930ONLY_df,
                               Ha_female_1940ONLY_df,
                               Ha_female_1950ONLY_df,
                               Ha_female_1960ONLY_df,
                               Ha_female_1970ONLY_df,
                               Ha_female_1980ONLY_df,
                               Ha_female_1990ONLY_df )
                               
                               
                               
                               
Cum_haz_all_cohort <- rbind(Cum_haz_female_cohort ,Cum_haz_male_cohort )

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

# Plots
#plot(log(np.fit.ic.c.female_1930ONLY$time),log(Ha_female_1930ONLY), type = "s", ylim =c(-10.5,0),  xlim =c(0,4.5),xlab = "log(Time in Years)", ylab = "Log H(t)", lty = 5)
plot( log(np.fit.ic.c.female_1930ONLY$time),log(Ha_female_1930ONLY), type = "s",lty = 5, col = "black",
      ylim =c(-10.5,-.5),  xlim =c(-0,5),xlab = "Log(Age)", ylab = "Log H(t)",
      main = "Anderson Plot-Proportionality Test of Cohort, for Females"
      , sub= "Log Cumulative Hazard vs Log Age by Sex and Decennial Birth -Cohort")
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



# Kaplan-Meier estimates (Non-parametric) Hazards            ============= #
# ======================================================================== #

#Male

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
head(df_surv.c.male)
#summary(df_surv.c.male)

#  ------------------------- #
## ADDING h(t), f(t) 
## 
#head(df_surv.c.male)
df_surv.c.male <- df_surv.c.male %>%
  group_by(Cohort) %>%
  mutate(f_t = c(F_t[1], diff(F_t)),
         h_t = f_t/S_t, 
         H_t = -log(S_t))%>%
  dplyr::select(Cohort, Init.Age, `At Risk`, Events, S_t, LB, UB, F_t, H_t, h_t, f_t)

head(df_surv.c.male)

# ---------------------------------------------------------------------------------------------------------- #
#Plotting Surv 

# Interval and Right-Censored Data (1998-2016)
# Survival function
library(dampack)
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
# hazard function


# plot2.km.haz.male<-ggplot(df_surv.c.male, aes(x = Init.Age, y = h_t, color = Cohort)) +
#   labs(title="Kaplan-Meier Initiation Rates Estimates by Birth-Cohort for Males") +
#   labs(y=" Rate of Cannabis Initiation, h(a)", x = "Age")+
#   geom_point(aes(color = Cohort), alpha=0.3)+
#   #geom_line( size =0.5)+
#   ylim(c(0,0.037))+
#   scale_x_continuous(breaks = number_ticks(13)) +
#   theme_bw()
# plot2.km.haz.male
#Saving km plots

#save(plot1.km.surv.male, plot2.km.haz.male, file="km.plots.cohort.male.RData")



  # ---------------------------------------------------------------------------------------------------------- #
  # ---------------------------------------------------------------------------------------------------------- #
 
head(df_surv.c.male)

age.peak.c.male.np <-df_surv.c.male%>%
  mutate( age.maxr = ifelse(h_t == max(h_t), Init.Age, NA))%>%
  group_by(Cohort, age.maxr)%>%
  summarize( rate.max = max(h_t))
age.peak.c.male.np


 # ---------------------------------------------------------------------------------------------------------- #
  # ---------------------------------------------------------------------------------------------------------- #
  
#female

  df_surv.c.female <- data.frame(Cohort = c( rep('1930',   np.fit.ic.c.female$strata[1]) , 
                                           rep('1940',   np.fit.ic.c.female$strata[2]) ,
                                           rep('1950',   np.fit.ic.c.female$strata[3]) ,
                                           rep('1960',   np.fit.ic.c.female$strata[4]) ,
                                           rep('1970',   np.fit.ic.c.female$strata[5]) ,
                                           rep('1980',   np.fit.ic.c.female$strata[6]) ,
                                           rep('1990',   np.fit.ic.c.female$strata[7])) ,
                                       Sex = c(rep("Female" ,  np.fit.ic.c.female$strata[1]) , 
                                           rep("Female",   np.fit.ic.c.female$strata[2])    , 
                                           rep("Female",  np.fit.ic.c.female$strata[3]) , 
                                           rep("Female", np.fit.ic.c.female$strata[4])    ,
                                           rep("Female",  np.fit.ic.c.female$strata[5]) , 
                                           rep("Female", np.fit.ic.c.female$strata[6])    ,
                                           rep("Female", np.fit.ic.c.female$strata[7])),
                               Init.Age       =            np.fit.ic.c.female$time, 
                               `At Risk`      = as.integer(np.fit.ic.c.female$n.risk), 
                               `Events`       = as.integer(np.fit.ic.c.female$n.event), 
                               S_t            =    np.fit.ic.c.female$surv,
                               LB             =    np.fit.ic.c.female$lower,
                               UB             =    np.fit.ic.c.female$upper,
                               F_t            =  1-np.fit.ic.c.female$surv, 
                               H_t            =    NA,
                               check.names    = FALSE)
  head(df_surv.c.female)
  summary(df_surv.c.female)
  
  #  ------------------------- #
  ## ADDING h(t), f(t) 
  ## 
  head(df_surv.c.female)
  df_surv.c.female <- df_surv.c.female %>%
    group_by(Cohort) %>%
    mutate(f_t = c(F_t[1], diff(F_t)),
           h_t = f_t/S_t,
           H_t = -log(S_t))%>%
    dplyr::select(Cohort, Init.Age, `At Risk`, Events, S_t, LB, UB, F_t, H_t, h_t, f_t)
  
  head(df_surv.c.female)
  
  # ---------------------------------------------------------------------------------------------------------- #
  # ---------------------------------------------------------------------------------------------------------- #
  
  head(df_surv.c.female)
  
  age.peak.c.female.np <-df_surv.c.female%>%
    mutate( age.maxr = ifelse(h_t == max(h_t), Init.Age, NA))%>%
    group_by(Cohort, age.maxr)%>%
    summarize( rate.max = max(h_t))
  age.peak.c.female.np
  
  
  # ---------------------------------------------------------------------------------------------------------- #
  # ---------------------------------------------------------------------------------------------------------- #
  
  # ---------------------------------------------------------------------------------------------------------- #
  #Plotting Surv 
  
  # Interval and Right-Censored Data (1998-2016)
  # Survival function
  library(dampack)
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

#facet plot all cohort
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

# ggsave(
#   "plot_surv_all_cohort.png",
#   plot = plot_surv_all_cohort,
#   #device = "png",
#   #path = "~/HR_plots",
#   scale = 1,
#   width = 10,
#   height = 6,
#   units = "in",
#   dpi = 300,
#   limitsize = F
# )

# load("plot_surv_all_cohort.png")
#   # hazard function
# library(OpenImageR)
# 
# img<-OpenImageR::readImage("plot_surv_all_cohort.png")
#   imageShow(img)
  
# plot2.km.haz.female <-  ggplot(df_surv.c.female, aes(x = Init.Age, y = h_t, color = Cohort)) +
#     labs(title = "Kaplan-Meier Initiation Rates Estimates by Birth-Cohorts of Females") +
#     labs(y=" Rate of Cannabis Initiation, h(a)", x = "Age")+
#     geom_point(aes(color = Cohort), alpha=0.3)+
#     #geom_line( size =0.5)+
#     ylim(c(0,.037))+
#     scale_x_continuous(breaks = number_ticks(13)) +
#     theme_bw()
# plot2.km.haz.female

# Saving Km plots

#save(plot1.km.surv.female, plot2.km.haz.female, file="km.plots.cohort.female.RData")


  # # ===================================================== #

 #  # ---------------------------------------------------------------------------------------------------------- #
 #  # Spline Models (flexible -parametric) ============= #
 #  # ===================================================== #
 #  # MALES
 #  # BEST FIT MODELS, BY KNOTS, BY SATURATED VS NOT SATURATED, BY QUADRATIC TERM, BY AGE-VARYING EFFECTS
 #  
 #  #----------------------------------#
 #  # not saturated, by year
 #  sp.fit.mod1.ic.c.male.h_year_k1 <-                   flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec),
 #                                                                      k = 1, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  # AIC= 71414.48
 #  sp.fit.mod1.ic.c.male.h_year_k1
 #  sp.fit.mod1.ic.c.male.h_year_k1$coefficients[6]
 #  sp.fit.mod1.ic.c.male.h_year_k1$AIC
 #  
 #  #---#
 #  sp.fit.mod1.ic.c.male.h_year_k2 <-                   flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec),
 #                                                                      k = 2, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  # AIC = 71230
 #  sp.fit.mod1.ic.c.male.h_year_k2
 #  sp.fit.mod1.ic.c.male.h_year_k2$coefficients[6]
 #  sp.fit.mod1.ic.c.male.h_year_k2$AIC
 #  
 #  #---#
 #  sp.fit.mod1.ic.c.male.h_year_k3 <-                   flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec),
 #                                                                      k = 3, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  # AIC = 70057.23
 #  sp.fit.mod1.ic.c.male.h_year_k3
 #  sp.fit.mod1.ic.c.male.h_year_k3$coefficients[6]
 #  sp.fit.mod1.ic.c.male.h_year_k3$AIC
 #  plot(sp.fit.mod1.ic.c.male.h_year_k3)
 #  
 #  #---#
 #  sp.fit.mod1.ic.c.male.h_year_k5 <-                   flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec),
 #                                                                      k = 5, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  # AIC = 69700.08
 #  sp.fit.mod1.ic.c.male.h_year_k5
 #  sp.fit.mod1.ic.c.male.h_year_k5$coefficients[6]
 #  sp.fit.mod1.ic.c.male.h_year_k5$AIC
 #  plot(sp.fit.mod1.ic.c.male.h_year_k5)
 #  #---#
 #  sp.fit.mod1.ic.c.male.h_year_k6 <-                   flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec),
 #                                                                      k = 6, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  # AIC = 69973.25
 #  sp.fit.mod1.ic.c.male.h_year_k6
 #  sp.fit.mod1.ic.c.male.h_year_k6$coefficients[6]
 #  sp.fit.mod1.ic.c.male.h_year_k6$AIC
 #  
 #  #---#
 #  sp.fit.mod1.ic.c.male.h_year_k7 <-                   flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec),
 #                                                                      k = 7, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  # AIC = 69066.32
 #  sp.fit.mod1.ic.c.male.h_year_k7
 #  sp.fit.mod1.ic.c.male.h_year_k7$coefficients[6]
 #  sp.fit.mod1.ic.c.male.h_year_k7$AIC
 #  plot(sp.fit.mod1.ic.c.male.h_year_k7)
 #  #---#  
 #  sp.fit.mod1.ic.c.male.h_year_k8 <-                   flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec),
 #                                                                      k = 8, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  # AIC = 69482.62
 #  sp.fit.mod1.ic.c.male.h_year_k8
 #  sp.fit.mod1.ic.c.male.h_year_k8$coefficients[6]
 #  sp.fit.mod1.ic.c.male.h_year_k8$AIC
 #  plot(sp.fit.mod1.ic.c.male.h_year_k8)
 #  
 #  
 #  # testing non-linearity through the splines on Cohort
 #  
 #  sp.fit.mod1.ic.c.male.h_year_k3_k2 <-                   flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec, knots = 2, intercept = FALSE),
 #                                                                      k = 3, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  # AIC = 69066.32
 #  sp.fit.mod1.ic.c.male.h_year_k3_k2
 #  sp.fit.mod1.ic.c.male.h_year_k3_k2$coefficients
 #  sp.fit.mod1.ic.c.male.h_year_k3_k2$AIC
 #  plot(sp.fit.mod1.ic.c.male.h_year_k3_k2)
 #  
 #  
 #  
 #  
 #  #----------------------------------#
 #  # Proportional hazard, saturated
 #  sp.fit.mod1.ic.c.male.h_sat_k1 <-                    flexsurvspline(my.surv.ic.c.male ~ factor(Cohort.dec),
 #                                                                      k = 1, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  # -> does not converge
 #  sp.fit.mod1.ic.c.male.h_sat_k2 <-                    flexsurvspline(my.surv.ic.c.male ~ factor(Cohort.dec),
 #                                                                      k = 2, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  # AIC =  71788.83
 #  sp.fit.mod1.ic.c.male.h_sat_k2
 #  sp.fit.mod1.ic.c.male.h_sat_k2$AIC
 #  #---#
 #  sp.fit.mod1.ic.c.male.h_sat_k3 <-                    flexsurvspline(my.surv.ic.c.male ~ factor(Cohort.dec),
 #                                                                      k = 3, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  # AIC = 69976.72
 #  sp.fit.mod1.ic.c.male.h_sat_k3
 #  sp.fit.mod1.ic.c.male.h_sat_k3$AIC
 # 
 #   #---#
 #  sp.fit.mod1.ic.c.male.h_sat_k5 <-                    flexsurvspline(my.surv.ic.c.male ~ factor(Cohort.dec),
 #                                                                      k = 5, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  #AIC = 69966.43
 #  sp.fit.mod1.ic.c.male.h_sat_k5
 #  sp.fit.mod1.ic.c.male.h_sat_k5$AIC
 #  
 #  #---#
 #  sp.fit.mod1.ic.c.male.h_sat_k6 <-                    flexsurvspline(my.surv.ic.c.male ~ factor(Cohort.dec),
 #                                                                      k = 6, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  #AIC = 69784.3
 #  sp.fit.mod1.ic.c.male.h_sat_k6
 #  sp.fit.mod1.ic.c.male.h_sat_k6$AIC
 #  
 #  #---#
 #  sp.fit.mod1.ic.c.male.h_sat_k7 <-                    flexsurvspline(my.surv.ic.c.male ~ factor(Cohort.dec),
 #                                                                      k = 7, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  #AIC = 69048.44, very good approximation, see the plot
 #  sp.fit.mod1.ic.c.male.h_sat_k7
 #  sp.fit.mod1.ic.c.male.h_sat_k7$AIC
 #  plot(sp.fit.mod1.ic.c.male.h_sat_k7)
 #  #---#
 #  sp.fit.mod1.ic.c.male.h_sat_k8 <-                    flexsurvspline(my.surv.ic.c.male ~ factor(Cohort.dec),
 #                                                                      k = 8, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  #AIC = 69423.78
 #  sp.fit.mod1.ic.c.male.h_sat_k8
 #  sp.fit.mod1.ic.c.male.h_sat_k8$AIC
 #  
 #  
 #  
 #  
 #  
 #  #----------------------------------#
 #  # Proportional hazard, NOT SATURATED, ADDING AGE-VARYING EFFECTS, BY KNOTS
 #  
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_k1 <-                flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec) + gamma1(Cohort.dec),
 #                                                                      k = 1, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  #AIC = 82078.37
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_k1$AIC
 #  
 #  #---#
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_k2 <-                flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec) + gamma1(Cohort.dec)+gamma2(Cohort.dec),
 #                                                                      k = 2, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  
 #  #-> AIC = 70702.69
 #  
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_k2
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_k2$coefficients[6]
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_k2$AIC
 #  
 #  
 #  #---#
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_k3 <-                flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec) + gamma1(Cohort.dec) + gamma2(Cohort.dec)  ,
 #                                                                      k = 3, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  #AIC = 69348.43
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_k3
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_k3$coefficients[6]
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_k3$AIC
 #  
 #  #---#
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_k5 <-                flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec)+ gamma1(Cohort.dec) + gamma2(Cohort.dec) ,
 #                                                                      k = 5, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  # AIC = 69601.54
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_k5
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_k5$coefficients[6]
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_k5$AIC
 #  
 #  plot(sp.fit.mod1.ic.c.male.h_AGEVARY_k5)
 #  
 #  #---#
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_k6 <-                flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec)+ gamma1(Cohort.dec) + gamma2(Cohort.dec) ,
 #                                                                      k = 6, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  # AIC = 69807.72
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_k6
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_k6$coefficients[6]
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_k6$AIC
 #  
 #  #---#
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_k7 <-                flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec)+ gamma1(Cohort.dec) + gamma2(Cohort.dec) ,
 #                                                                      k = 7, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  # AIC = 70099.79
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_k7
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_k7$coefficients[6]
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_k7$AIC
 #  
 #  # exploring if with saturated model we got somethign better 
 #  
 #  #----------------------------------#
 #  #---#
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k3 <-                flexsurvspline(my.surv.ic.c.male ~ factor(Cohort.dec) + gamma1(Cohort.dec) + gamma2(Cohort.dec)  ,
 #                                                                      k = 3, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  #AIC = 69269.07
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k3
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k3$coefficients[6]
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k3$AIC
 #  
 #  
 #  #---#
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k5 <-                flexsurvspline(my.surv.ic.c.male ~ factor(Cohort.dec) + gamma1(Cohort.dec) + gamma2(Cohort.dec)  ,
 #                                                                          k = 5, scale = "hazard",
 #                                                                          data = DMarihuana_1998_2016.c.male)
 #  #AIC = 69538.54
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k5
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k5$coefficients[6]
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k5$AIC
 #  plot(sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k5)
 #  
 #  #---#
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k7 <-                flexsurvspline(my.surv.ic.c.male ~ factor(Cohort.dec) + gamma1(Cohort.dec) + gamma2(Cohort.dec)  ,
 #                                                                          k = 7, scale = "hazard",
 #                                                                          data = DMarihuana_1998_2016.c.male)
 #  #AIC = 69269.07
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k7
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k7$coefficients[6]
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k7$AIC
 #  plot(sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k7)
 #  
 #  
 #  #---#
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k3_1 <-                flexsurvspline(my.surv.ic.c.male ~ factor(Cohort.dec) + gamma1(factor(Cohort.dec)),
 #                                                                          k = 3, scale = "hazard",
 #                                                                          data = DMarihuana_1998_2016.c.male)
 #  #AIC = 69250.45
 #  #sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k3
 #  #sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k3$coefficients[6]
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k3_1$AIC
 #  #plot(sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k3)
 #  
 #  #---#
 #  
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k5_1 <-                flexsurvspline(my.surv.ic.c.male ~ factor(Cohort.dec) + gamma1(factor(Cohort.dec)),
 #                                                                          k = 5, scale = "hazard",
 #                                                                          data = DMarihuana_1998_2016.c.male)
 #  #AIC = 69555.34
 #  #sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k5
 #  #sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k5$coefficients[6]
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k5_1$AIC
 #  #plot(sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k5)
 #  
 #  #---#
 #  
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k7_1 <-                flexsurvspline(my.surv.ic.c.male ~ factor(Cohort.dec) + gamma1(factor(Cohort.dec)) ,
 #                                                                          k = 7, scale = "hazard",
 #                                                                          data = DMarihuana_1998_2016.c.male)
 #  # AIC =  69049.51 !
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k7_1$AIC
 # plot(sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k7_1)
 #  #----------------------------------#
 #  
 #  # Proportional hazard, NOT SATURATED, ADDING QUADRATIC TERM, BY KNOTS
 #  
 #  sp.fit.mod1.ic.c.male.h_quadratic_k1 <-              flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec)+ ns(Cohort.dec*Cohort.dec),
 #                                                                      k = 1, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  # -> does not converge
 #  #sp.fit.mod1.ic.c.male.h_quadratic_k1$AIC
 #  
 #  #---#
 #  sp.fit.mod1.ic.c.male.h_quadratic_k2 <-              flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec)+ ns(Cohort.dec*Cohort.dec),
 #                                                                      k = 2, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  # -> AIC=71235.31
 #  sp.fit.mod1.ic.c.male.h_quadratic_k2$AIC
 # 
 #   #---#
 #  sp.fit.mod1.ic.c.male.h_quadratic_k3 <-              flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec)+ ns(Cohort.dec*Cohort.dec),
 #                                                                      k = 3, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  # AIC= 69413.77
 #  sp.fit.mod1.ic.c.male.h_quadratic_k3$AIC
 # 
 #   #---#
 #  sp.fit.mod1.ic.c.male.h_quadratic_k5 <-              flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec)+ ns(Cohort.dec*Cohort.dec),
 #                                                                      k = 5, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  # AIC = 69833.64
 #  sp.fit.mod1.ic.c.male.h_quadratic_k5$AIC
 #  
 #  #---#
 #  sp.fit.mod1.ic.c.male.h_quadratic_k7 <-              flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec)+ ns(Cohort.dec*Cohort.dec),
 #                                                                      k = 7, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  # AIC = 69067.49
 #  sp.fit.mod1.ic.c.male.h_quadratic_k7$AIC
 #  plot(sp.fit.mod1.ic.c.male.h_quadratic_k7)
 #  
 #  #---# now with a saturated effect on age
 #  sp.fit.mod1.ic.c.male.h_quadratic_sat_k7 <-              flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec)+ ns(Cohort.dec*Cohort.dec) + gamma1(factor(Cohort.dec)),
 #                                                                      k = 7, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  # AIC = 95740.22
 #  sp.fit.mod1.ic.c.male.h_quadratic_sat_k7$AIC
 #  plot(sp.fit.mod1.ic.c.male.h_quadratic_sat_k7)
 #  
 #  
 #  
 #  #----------------------------------#
 #  
 #  # Proportional hazard, NOT SATURATED, ADDING AGE-VARYING EFFECTS, ADDING QUADRATIC TERM, BY KNOTS
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_quadratic_k1 <-      flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec) + gamma1(Cohort.dec) + ns(Cohort.dec*Cohort.dec),
 #                                                                      k = 1, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  #-> AIC = 71353.92
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_quadratic_k1$AIC
 #  
 #  #---#
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_quadratic_k2 <-      flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec) + gamma1(Cohort.dec) + gamma2(Cohort.dec) + ns(Cohort.dec*Cohort.dec),
 #                                                                      k = 2, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  
 #  #-> AIC= 70842.4
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_quadratic_k2$AIC
 #  
 #  #---#
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_quadratic_k3 <-      flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec) + gamma1(Cohort.dec) + gamma2(Cohort.dec) + ns(Cohort.dec*Cohort.dec) ,
 #                                                                      k = 3, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  # AIC=  69323.22
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_quadratic_k3$AIC
 #  
 #  
 #  #---#
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_quadratic_k5 <-      flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec)+ gamma1(Cohort.dec) + gamma2(Cohort.dec) + ns(Cohort.dec*Cohort.dec) ,
 #                                                                      k = 5, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  # AIC= 69592.86
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_quadratic_k5$AIC
 #  
 #  #---#
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_quadratic_k7 <-      flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec)+ gamma1(Cohort.dec) + gamma2(Cohort.dec) + ns(Cohort.dec*Cohort.dec) ,
 #                                                                      k = 7, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  # AIC= 69064.21
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_quadratic_k7$AIC
 #  plot(sp.fit.mod1.ic.c.male.h_AGEVARY_quadratic_k7$AIC)
 #  
 #  #---#
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_quadratic_k7_2 <-      flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec)+ gamma1(Cohort.dec)  + ns(Cohort.dec*Cohort.dec) ,
 #                                                                      k = 7, scale = "hazard",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  # AIC= 70065.16
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_quadratic_k7_2$AIC
 #  
 #  
 #  # top four under hazard scale
 #  sp.fit.mod1.ic.c.male.h_sat_k7
 #  #AIC = 69048.44
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k7
 #  # AIC =  69049.51
 #  sp.fit.mod1.ic.c.male.h_AGEVARY_quadratic_k7
 #  # AIC= 69064.21
 #  sp.fit.mod1.ic.c.male.h_year_k7
 #  # AIC = 69066.32
 #  sp.fit.mod1.ic.c.male.h_quadratic_k7
 #  # AIC = 69067.49
 # 
 #  #--------------------------------------------#
 #  #-----------ODDS SCALE-----------------------#
 #  
 #  #Best performing models in hazard scale now in odds scale
 #  
 #  sp.fit.mod1.ic.o.c.male.h_sat_k7<-         flexsurvspline(my.surv.ic.c.male ~ factor(Cohort.dec),
 #                                                    k = 7, scale = "odds",
 #                                                    data = DMarihuana_1998_2016.c.male)
 #  
 #  #AIC = 69117.08
 #  sp.fit.mod1.ic.o.c.male.h_sat_k7$AIC
 #  
 #  #---#
 #  
 #  sp.fit.mod1.ic.o.c.male.h_AGEVARY_sat_k7<- flexsurvspline(my.surv.ic.c.male ~ factor(Cohort.dec) + gamma1(factor(Cohort.dec)) ,
 #                                                            k = 7, scale = "odds",
 #                                                            data = DMarihuana_1998_2016.c.male)
 #  
 #  
 #  #AIC = 71147.13
 #  sp.fit.mod1.ic.o.c.male.h_AGEVARY_sat_k7$AIC
 #  
 #  #---#  
 #  sp.fit.mod1.ic.o.c.male.h_AGEVARY_quadratic_k7<- flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec)+ gamma1(Cohort.dec) + gamma2(Cohort.dec) + ns(Cohort.dec*Cohort.dec) ,
 #                                                                  k = 7, scale = "odds",
 #                                                                  data = DMarihuana_1998_2016.c.male)
 #  
 #  #AIC = 69976.46
 #  sp.fit.mod1.ic.o.c.male.h_AGEVARY_quadratic_k7$AIC
 #  
 #  #---#  
 #  sp.fit.mod1.ic.o.c.male.h_year_k7<-              flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec),
 #                                                     k = 7, scale = "odds",
 #                                                     data = DMarihuana_1998_2016.c.male)
 #  
 #  #AIC = 69064.59
 #  sp.fit.mod1.ic.o.c.male.h_year_k7$AIC
 #  
 #  #---#  
 #  sp.fit.mod1.ic.o.c.male.h_quadratic_k7<- flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec)+ ns(Cohort.dec*Cohort.dec),
 #                                                          k = 7, scale = "odds",
 #                                                          data = DMarihuana_1998_2016.c.male)
 #  
 #    
 #  #AIC = 69051.95
 #  sp.fit.mod1.ic.o.c.male.h_quadratic_k7$AIC
 #  
 #    
 #  #--------------------------------------------#
 #  
 #  
 #  sp.fit.mod1.ic.o.c.male.h_year_k1 <-                   flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec),
 #                                                                      k = 1, scale = "odds",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  #AIC = 71342.88
 #  sp.fit.mod1.ic.o.c.male.h_year_k1$AIC
 # 
 #   #---#
 #  sp.fit.mod1.ic.o.c.male.h_year_k2 <-                   flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec),
 #                                                                      k = 2, scale = "odds",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  #AIC = 71213.91
 #  sp.fit.mod1.ic.o.c.male.h_year_k2
 #  sp.fit.mod1.ic.o.c.male.h_year_k2$coefficients[6]
 #  sp.fit.mod1.ic.o.c.male.h_year_k2$AIC
 #  
 #  
 #  #---#
 #  sp.fit.mod1.ic.o.c.male.h_year_k3 <-                   flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec),
 #                                                                      k = 3, scale = "odds",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  #AIC = 69437.74
 #  sp.fit.mod1.ic.o.c.male.h_year_k3
 #  sp.fit.mod1.ic.o.c.male.h_year_k3$coefficients[6]
 #  sp.fit.mod1.ic.o.c.male.h_year_k3$AIC
 #  
 #  #---#
 #  sp.fit.mod1.ic.o.c.male.h_year_k5 <-                   flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec),
 #                                                                      k = 5, scale = "odds",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  #AIC = 69701.5
 #  sp.fit.mod1.ic.o.c.male.h_year_k5
 #  sp.fit.mod1.ic.o.c.male.h_year_k5$coefficients[6]
 #  sp.fit.mod1.ic.o.c.male.h_year_k5$AIC
 #  
 #  #---#
 #  sp.fit.mod1.ic.o.c.male.h_year_k7 <-                   flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec),
 #                                                                        k = 7, scale = "odds",
 #                                                                        data = DMarihuana_1998_2016.c.male)
 #  #AIC = 69064.59
 #  sp.fit.mod1.ic.o.c.male.h_year_k7
 #  sp.fit.mod1.ic.o.c.male.h_year_k7$coefficients[6]
 #  sp.fit.mod1.ic.o.c.male.h_year_k7$AIC
 #  
 #  
 #  
 #  #----------------------------------#
 #  #
 #  sp.fit.mod1.ic.o.c.male.h_AGEVARY_k3 <-                flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec)+ gamma1(Cohort.dec) + gamma2(Cohort.dec) ,
 #                                                                      k = 3, scale = "odds",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  # AIC =  69475.91
 #  sp.fit.mod1.ic.o.c.male.h_AGEVARY_k3
 #  sp.fit.mod1.ic.o.c.male.h_AGEVARY_k3$coefficients[6]
 #  sp.fit.mod1.ic.o.c.male.h_AGEVARY_k3$AIC
 #  
 #  #---#
 #  sp.fit.mod1.ic.o.c.male.h_AGEVARY_k5 <-                flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec)+ gamma1(Cohort.dec) + gamma2(Cohort.dec) ,
 #                                                                      k = 5, scale = "odds",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  # AIC = 69561.69
 #  sp.fit.mod1.ic.o.c.male.h_AGEVARY_k5
 #  sp.fit.mod1.ic.o.c.male.h_AGEVARY_k5$coefficients[6]
 #  sp.fit.mod1.ic.o.c.male.h_AGEVARY_k5$AIC
 #  
 #  #---#
 #  
 #  sp.fit.mod1.ic.o.c.male.h_AGEVARY_k7 <-                flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec)+ gamma1(Cohort.dec) + gamma2(Cohort.dec) ,
 #                                                                      k = 7, scale = "odds",
 #                                                                      data = DMarihuana_1998_2016.c.male)
 #  # AIC = 69022.8 !!
 #  sp.fit.mod1.ic.o.c.male.h_AGEVARY_k7
 #  sp.fit.mod1.ic.o.c.male.h_AGEVARY_k7$coefficients[6]
 #  sp.fit.mod1.ic.o.c.male.h_AGEVARY_k7$AIC
 #  
 #  plot(sp.fit.mod1.ic.o.c.male.h_AGEVARY_k7)
 #  
 #  #---#
 #  
 #  sp.fit.mod1.ic.o.c.male.h_AGEVARY_k7_2 <-                flexsurvspline(my.surv.ic.c.male ~ ns(Cohort.dec)+ gamma1(Cohort.dec),
 #                                                                        k = 7, scale = "odds",
 #                                                                        data = DMarihuana_1998_2016.c.male)
 #  # AIC = 69070.27
 #  sp.fit.mod1.ic.o.c.male.h_AGEVARY_k7_2
 #  sp.fit.mod1.ic.o.c.male.h_AGEVARY_k7_2$coefficients[6]
 #  sp.fit.mod1.ic.o.c.male.h_AGEVARY_k7_2$AIC
 #  
 #  plot(sp.fit.mod1.ic.o.c.male.h_AGEVARY_k7_2)
 #  
 #  
 #  
 #  # SUMMARY, BEST FIT MODELS FOR MALES, cohort
 #  # Top three models
 #  # No1 : , AIC = 
 #  plot()
 #  # Assumptions: 
 #  # No2 : , AIC = 
 #  plot()
 #  # Assumptions: 
 #  #No3  : , AIC = 
 #  plot()
 #  # Assumptions: 
  
  # #------- SAVING RESULTS-TOP THREE MODELS Males-Period---------#
  # topthree_males_cohort_splines <- list(fit.object = list( first = sp.fit.mod1.ic.o.c.male.h_AGEVARY_k6, second = sp.fit.mod1.ic.c.male.h_AGEVARY_k5, third = sp.fit.mod1.ic.c.male.h_year_k3),
  #                                       AIC = list( first = sp.fit.mod1.ic.o.c.male.h_AGEVARY_k6$AIC, second = sp.fit.mod1.ic.c.male.h_AGEVARY_k5$AIC, third = sp.fit.mod1.ic.c.male.h_year_k3$AIC),
  #                                       Coeffs = list( first = sp.fit.mod1.ic.o.c.male.h_AGEVARY_k6$coefficients, second = sp.fit.mod1.ic.c.male.h_AGEVARY_k5$coefficients, third = sp.fit.mod1.ic.c.male.h_year_k3$coefficients))
  # save(topthree_males_cohort_splines, file = "/Users/mauriciolopezm/Dropbox (Brown)/Troubleshooting-Parametric Models and Splines/topthree_males_cohort_splines.RData")
  # 
  # # Saving all
  # 
  # all_males_cohort_splines<- list( 
  #   sp.fit.mod1.ic.c.male.h_year_k1,
  #   sp.fit.mod1.ic.c.male.h_year_k2,
  #   sp.fit.mod1.ic.c.male.h_year_k3,
  #   sp.fit.mod1.ic.c.male.h_year_k5,
  #   sp.fit.mod1.ic.c.male.h_year_k6,
  #   sp.fit.mod1.ic.c.male.h_year_k7,
  #   sp.fit.mod1.ic.c.male.h_year_k8,
  #   
  #   "sp.fit.mod1.ic.c.male.h_sat_k1-notconverge",
  #   sp.fit.mod1.ic.c.male.h_sat_k2,
  #   sp.fit.mod1.ic.c.male.h_sat_k3,
  #   sp.fit.mod1.ic.c.male.h_sat_k5,
  #   sp.fit.mod1.ic.c.male.h_sat_k6,
  #   sp.fit.mod1.ic.c.male.h_sat_k7, #13
  #   sp.fit.mod1.ic.c.male.h_sat_k8,
  #   
  #   sp.fit.mod1.ic.c.male.h_AGEVARY_k1,
  #   sp.fit.mod1.ic.c.male.h_AGEVARY_k2,
  #   sp.fit.mod1.ic.c.male.h_AGEVARY_k3,
  #   sp.fit.mod1.ic.c.male.h_AGEVARY_k5,
  #   sp.fit.mod1.ic.c.male.h_AGEVARY_k6,
  #   sp.fit.mod1.ic.c.male.h_AGEVARY_k7,
  #   
  #   sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k3,
  #   sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k5,
  #   sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k7,
  #   
  #   sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k3_1,
  #   sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k5_1,
  #   sp.fit.mod1.ic.c.male.h_AGEVARY_sat_k7_1,
  #   
  #   "sp.fit.mod1.ic.c.male.h_quadratic_k1-notconverge",
  #   sp.fit.mod1.ic.c.male.h_quadratic_k2,
  #   sp.fit.mod1.ic.c.male.h_quadratic_k3,
  #   sp.fit.mod1.ic.c.male.h_quadratic_k5,
  #   sp.fit.mod1.ic.c.male.h_quadratic_k7,
  #   
  #   sp.fit.mod1.ic.c.male.h_AGEVARY_quadratic_k1,
  #   sp.fit.mod1.ic.c.male.h_AGEVARY_quadratic_k2,
  #   sp.fit.mod1.ic.c.male.h_AGEVARY_quadratic_k3,
  #   sp.fit.mod1.ic.c.male.h_AGEVARY_quadratic_k5,
  #   sp.fit.mod1.ic.c.male.h_AGEVARY_quadratic_k7,
  #   
  #   sp.fit.mod1.ic.c.male.h_AGEVARY_quadratic_k7_2,
  #   
  #   sp.fit.mod1.ic.o.c.male.h_year_k1,
  #   sp.fit.mod1.ic.o.c.male.h_year_k2,
  #   sp.fit.mod1.ic.o.c.male.h_year_k3,
  #   sp.fit.mod1.ic.o.c.male.h_year_k5,
  #   sp.fit.mod1.ic.o.c.male.h_year_k7,
  #   
  #   sp.fit.mod1.ic.o.c.male.h_AGEVARY_k3,
  #   sp.fit.mod1.ic.o.c.male.h_AGEVARY_k5,
  #   sp.fit.mod1.ic.o.c.male.h_AGEVARY_k7,
  #   
  #   sp.fit.mod1.ic.o.c.male.h_AGEVARY_k7_2, #best
  #   
  #   sp.fit.mod1.ic.o.c.male.h_sat_k7,
  #   sp.fit.mod1.ic.o.c.male.h_AGEVARY_sat_k7,
  #   sp.fit.mod1.ic.o.c.male.h_AGEVARY_quadratic_k7,
  #   sp.fit.mod1.ic.o.c.male.h_year_k7,
  #   sp.fit.mod1.ic.o.c.male.h_quadratic_k7 #51
  #   
  #   )
  # save(all_males_cohort_splines, file = "/Users/mauriciolopezm/Dropbox (Brown)/Troubleshooting-Parametric Models and Splines/all_males_cohort_splines.RData")
  # 
  # 
  #----------------------------------#
  #-----------FEMALES-----------------------#
  
  # BEST FIT MODELS, BY KNOTS, BY SATURATED VS NOT SATURATED, BY QUADRATIC TERM, BY AGE-VARYING EFFECTS
  
  #----------------------------------#
  # not saturated, by year
  # sp.fit.mod1.ic.c.female.h_year_k1 <-                   flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec),
  #                                                                       k = 1, scale = "hazard",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # #AIC = 21308.06
  # sp.fit.mod1.ic.c.female.h_year_k1
  # sp.fit.mod1.ic.c.female.h_year_k1$coefficients[6]
  # sp.fit.mod1.ic.c.female.h_year_k1$AIC
  # 
  # #---#
  # sp.fit.mod1.ic.c.female.h_year_k2 <-                   flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec),
  #                                                                       k = 2, scale = "hazard",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # #AIC = 21085.28
  # sp.fit.mod1.ic.c.female.h_year_k2
  # sp.fit.mod1.ic.c.female.h_year_k2$coefficients[6]
  # sp.fit.mod1.ic.c.female.h_year_k2$AIC
  # 
  # #---#
  # sp.fit.mod1.ic.c.female.h_year_k3 <-                   flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec),
  #                                                                       k = 3, scale = "hazard",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # #AIC = 20911.1
  # sp.fit.mod1.ic.c.female.h_year_k3
  # sp.fit.mod1.ic.c.female.h_year_k3$coefficients[6]
  # sp.fit.mod1.ic.c.female.h_year_k3$AIC
  # plot(sp.fit.mod1.ic.c.female.h_year_k3)
  # 
  # #---#
  # sp.fit.mod1.ic.c.female.h_year_k5 <-                   flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec),
  #                                                                       k = 5, scale = "hazard",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # #AIC = 20936.73
  # sp.fit.mod1.ic.c.female.h_year_k5
  # sp.fit.mod1.ic.c.female.h_year_k5$coefficients[6]
  # sp.fit.mod1.ic.c.female.h_year_k5$AIC
  # 
  # #---#
  # sp.fit.mod1.ic.c.female.h_year_k7 <-                   flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec),
  #                                                                       k = 7, scale = "hazard",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # #not converge
  # sp.fit.mod1.ic.c.female.h_year_k7
  # sp.fit.mod1.ic.c.female.h_year_k7$coefficients[6]
  # sp.fit.mod1.ic.c.female.h_year_k7$AIC
  # 
  # 
  # #----------------------------------#
  # # Proportional hazard, saturated
  # sp.fit.mod1.ic.c.female.h_sat_k1 <-                    flexsurvspline(my.surv.ic.c.female ~ factor(Cohort.dec),
  #                                                                       k = 1, scale = "hazard",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # #not converge
  # #sp.fit.mod1.ic.c.female.h_sat_k1$AIC
  # 
  # #---#
  # sp.fit.mod1.ic.c.female.h_sat_k2 <-                    flexsurvspline(my.surv.ic.c.female ~ factor(Cohort.dec),
  #                                                                       k = 2, scale = "hazard",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # #ot converge
  # #sp.fit.mod1.ic.c.female.h_sat_k2$AIC
  # #---#
  # sp.fit.mod1.ic.c.female.h_sat_k3 <-                    flexsurvspline(my.surv.ic.c.female ~ factor(Cohort.dec),
  #                                                                       k = 3, scale = "hazard",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # #ot converge
  # #sp.fit.mod1.ic.c.female.h_sat_k3$AIC
  # #---#
  # sp.fit.mod1.ic.c.female.h_sat_k5 <-                    flexsurvspline(my.surv.ic.c.female ~ factor(Cohort.dec),
  #                                                                       k = 5, scale = "hazard",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # #ot converge
  # #sp.fit.mod1.ic.c.female.h_sat_k5$AIC
  # 
  # #---#
  #  
  # sp.fit.mod1.ic.c.female.h_sat_k7 <-                    flexsurvspline(my.surv.ic.c.female ~ factor(Cohort.dec),
  #                                                                       k = 7, scale = "hazard",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # #ot converge 
  # #sp.fit.mod1.ic.c.female.h_sat_k7$AIC
  # 
  # 
  # #----------------------------------#
  # # Proportional hazard, NOT SATURATED, ADDING AGE-VARYING EFFECTS, BY KNOTS
  # 
  # sp.fit.mod1.ic.c.female.h_AGEVARY_k1 <-                flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec) + gamma1(Cohort.dec),
  #                                                                       k = 1, scale = "hazard",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # #AIC = 21253.58
  # sp.fit.mod1.ic.c.female.h_AGEVARY_k1$AIC
  # 
  # #---#
  # sp.fit.mod1.ic.c.female.h_AGEVARY_k2 <-                flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec) + gamma1(Cohort.dec)+gamma2(Cohort.dec),
  #                                                                       k = 2, scale = "hazard",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # #AIC = 21024.18
  # 
  # sp.fit.mod1.ic.c.female.h_AGEVARY_k2
  # sp.fit.mod1.ic.c.female.h_AGEVARY_k2$coefficients[6]
  # sp.fit.mod1.ic.c.female.h_AGEVARY_k2$AIC
  # 
  # 
  # #---#
  # sp.fit.mod1.ic.c.female.h_AGEVARY_k3 <-                flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec) + gamma1(Cohort.dec) + gamma2(Cohort.dec)  ,
  #                                                                       k = 3, scale = "hazard",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # #AIC = 20889.62
  # sp.fit.mod1.ic.c.female.h_AGEVARY_k3
  # sp.fit.mod1.ic.c.female.h_AGEVARY_k3$coefficients[6]
  # sp.fit.mod1.ic.c.female.h_AGEVARY_k3$AIC
  # 
  # #---#
  # sp.fit.mod1.ic.c.female.h_AGEVARY_k5 <-                flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec)+ gamma1(Cohort.dec) + gamma2(Cohort.dec) ,
  #                                                                       k = 5, scale = "hazard",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # # AIC = 20928.33 
  # sp.fit.mod1.ic.c.female.h_AGEVARY_k5$AIC
  # #plot(sp.fit.mod1.ic.c.female.h_AGEVARY_k5)
  # 
  # #---#
  # sp.fit.mod1.ic.c.female.h_AGEVARY_k6 <-                flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec)+ gamma1(Cohort.dec) + gamma2(Cohort.dec) ,
  #                                                                       k = 6, scale = "hazard",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # # AIC = 20962.15
  # sp.fit.mod1.ic.c.female.h_AGEVARY_k6
  # sp.fit.mod1.ic.c.female.h_AGEVARY_k6$coefficients[6]
  # sp.fit.mod1.ic.c.female.h_AGEVARY_k6$AIC
  # 
  # 
  # #---#
  # sp.fit.mod1.ic.c.female.h_AGEVARY_k7 <-                flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec)+ gamma1(Cohort.dec) + gamma2(Cohort.dec) ,
  #                                                                       k = 7, scale = "hazard",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # # notconverge
  # sp.fit.mod1.ic.c.female.h_AGEVARY_k7
  # sp.fit.mod1.ic.c.female.h_AGEVARY_k7$coefficients[6]
  # sp.fit.mod1.ic.c.female.h_AGEVARY_k7$AIC
  # 
  # 
  # #----------------------------------#
  # 
  # # Proportional hazard, NOT SATURATED, ADDING QUADRATIC TERM, BY KNOTS
  # 
  # sp.fit.mod1.ic.c.female.h_quadratic_k1 <-              flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec)+ ns(Cohort.dec*Cohort.dec),
  #                                                                       k = 1, scale = "hazard",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # # AIC =  21182.68
  # sp.fit.mod1.ic.c.female.h_quadratic_k1$AIC
  # #---#
  # sp.fit.mod1.ic.c.female.h_quadratic_k2 <-              flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec)+ ns(Cohort.dec*Cohort.dec),
  #                                                                       k = 2, scale = "hazard",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # # AIC = 20987.47
  # sp.fit.mod1.ic.c.female.h_quadratic_k2$AIC
  # 
  # #---#
  # sp.fit.mod1.ic.c.female.h_quadratic_k3 <-              flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec)+ ns(Cohort.dec*Cohort.dec),
  #                                                                       k = 3, scale = "hazard",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # # AIC = 20822.39 , second best
  # sp.fit.mod1.ic.c.female.h_quadratic_k3$AIC
  # 
  # #---#
  # sp.fit.mod1.ic.c.female.h_quadratic_k5 <-              flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec)+ ns(Cohort.dec*Cohort.dec),
  #                                                                       k = 5, scale = "hazard",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # # AIC = 20848.99, 
  # sp.fit.mod1.ic.c.female.h_quadratic_k5$AIC
  # 
  # #---#
  # sp.fit.mod1.ic.c.female.h_quadratic_k7 <-              flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec)+ ns(Cohort.dec*Cohort.dec),
  #                                                                       k = 7, scale = "hazard",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # # not converge
  # #sp.fit.mod1.ic.c.female.h_quadratic_k7$AIC
  # 
  # #========================================================== #
  # # TESTING NOLINEARITY OF COH DEC VIA THE SPLINE ON COH DEC
  # 
  # sp.fit.mod1.ic.c.female.h_quadratic_k3_k2 <-              flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec, knots = 6, intercept = FALSE),
  #                                                                       k = 3, scale = "hazard",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # # AIC = 20822.39 , second best
  # sp.fit.mod1.ic.c.female.h_quadratic_k3_k2$AIC
  # 
  # 
  # 
  # 
  # 
  # #----------------------------------#
  # 
  # # Proportional hazard, NOT SATURATED, ADDING AGE-VARYING EFFECTS, ADDING QUADRATIC TERM, BY KNOTS
  # sp.fit.mod1.ic.c.female.h_AGEVARY_quadratic_k1 <-      flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec) + gamma1(Cohort.dec) + ns(Cohort.dec*Cohort.dec),
  #                                                                       k = 1, scale = "hazard",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # # AIC = 21164.79
  # sp.fit.mod1.ic.c.female.h_AGEVARY_quadratic_k1$AIC
  # 
  # #---#
  # sp.fit.mod1.ic.c.female.h_AGEVARY_quadratic_k2 <-      flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec) + gamma1(Cohort.dec) + gamma2(Cohort.dec) + ns(Cohort.dec*Cohort.dec),
  #                                                                       k = 2, scale = "hazard",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # 
  # # AIC = 20956.92
  # sp.fit.mod1.ic.c.female.h_AGEVARY_quadratic_k2$AIC
  # 
  #---#
  sp.fit.mod1.ic.c.female.h_AGEVARY_quadratic_k3 <-      flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec) + gamma1(Cohort.dec) + gamma2(Cohort.dec) + as.numeric(Cohort.dec*Cohort.dec) ,
                                                                        k = 3, scale = "hazard",
                                                                        data = DMarihuana_1998_2016.c.female)
  
  sp.fit.mod1.ic.c.female.h_AGEVARY_quadratic_k3 <-      flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec) + gamma1(Cohort.dec) + gamma2(Cohort.dec)  ,
                                                                        k = 3, scale = "hazard",
                                                                        data = DMarihuana_1998_2016.c.female)
  
  #AIC = 20821.69!!
  # sp.fit.mod1.ic.c.female.h_AGEVARY_quadratic_k3$AIC
  # 
  # 
  # #---#
  # sp.fit.mod1.ic.c.female.h_AGEVARY_quadratic_k5 <-      flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec)+ gamma1(Cohort.dec) + gamma2(Cohort.dec) + ns(Cohort.dec*Cohort.dec) ,
  #                                                                       k = 5, scale = "hazard",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # # not converge
  # sp.fit.mod1.ic.p.female.h_AGEVARY_quadratic_k5$AIC
  # 
  # #---#
  # 
  # sp.fit.mod1.ic.c.female.h_AGEVARY_quadratic_k7 <-      flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec)+ gamma1(Cohort.dec) + gamma2(Cohort.dec) + ns(Cohort.dec*Cohort.dec) ,
  #                                                                       k = 7, scale = "hazard",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # # not converge
  # sp.fit.mod1.ic.p.female.h_AGEVARY_quadratic_k7$AIC
  # 
  # 
  #  #----------------------------------#
  # #-----------ODDS SCALE-----------------------#
  # # Trying best fit models in hazard scale now in odds
  # sp.fit.mod1.ic.o.c.female.h_AGEVARY_quadratic_k3 <-      flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec) + gamma1(Cohort.dec) + gamma2(Cohort.dec) + ns(Cohort.dec*Cohort.dec) ,
  #                                                                       k = 3, scale = "odds",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # #AIC= 20821.75, second best
  # sp.fit.mod1.ic.o.c.female.h_AGEVARY_quadratic_k3$AIC
  # 
  # #--#
  # 
  # sp.fit.mod1.ic.o.c.female.h_quadratic_k3 <-              flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec)+ ns(Cohort.dec*Cohort.dec),
  #                                                                       k = 3, scale = "odds",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # #AIC= 20822.03 !
  # sp.fit.mod1.ic.o.c.female.h_quadratic_k3$AIC
  # 
  # #--#
  # 
  # sp.fit.mod1.ic.o.c.female.h_quadratic_k5 <-              flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec)+ ns(Cohort.dec*Cohort.dec),
  #                                                                       k = 5, scale = "odds",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # 
  # #AIC = 20856.43
  # sp.fit.mod1.ic.o.c.female.h_quadratic_k5$AIC
  # 
  # #----------------------------------#
  # 
  # 
  # sp.fit.mod1.ic.o.c.female.h_year_k1 <-                   flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec),
  #                                                                       k = 1, scale = "odds",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # #AIC = 21305.33
  # sp.fit.mod1.ic.o.c.female.h_year_k1
  # sp.fit.mod1.ic.o.c.female.h_year_k1$coefficients[6]
  # sp.fit.mod1.ic.o.c.female.h_year_k1$AIC
  # 
  # #---#
  # sp.fit.mod1.ic.o.c.female.h_year_k2 <-                   flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec),
  #                                                                       k = 2, scale = "odds",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # #AIC = 21088.74
  # sp.fit.mod1.ic.o.c.female.h_year_k2
  # sp.fit.mod1.ic.o.c.female.h_year_k2$coefficients[6]
  # sp.fit.mod1.ic.o.c.female.h_year_k2$AIC
  # 
  # 
  # #---#
  # sp.fit.mod1.ic.o.c.female.h_year_k3 <-                   flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec),
  #                                                                       k = 3, scale = "odds",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # #AIC = 20915.06
  # sp.fit.mod1.ic.o.c.female.h_year_k3
  # sp.fit.mod1.ic.o.c.female.h_year_k3$coefficients[6]
  # sp.fit.mod1.ic.o.c.female.h_year_k3$AIC
  # 
  # #---#
  # sp.fit.mod1.ic.o.c.female.h_year_k5 <-                   flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec),
  #                                                                       k = 5, scale = "odds",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # #AIC = 20939.29
  # sp.fit.mod1.ic.o.c.female.h_year_k5
  # sp.fit.mod1.ic.o.c.female.h_year_k5$coefficients[6]
  # sp.fit.mod1.ic.o.c.female.h_year_k5$AIC
  # 
  # #---#
  # sp.fit.mod1.ic.o.c.female.h_year_k7 <-                   flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec),
  #                                                                         k = 7, scale = "odds",
  #                                                                         data = DMarihuana_1998_2016.c.female)
  # #not converge 
  # sp.fit.mod1.ic.o.c.female.h_year_k7
  # sp.fit.mod1.ic.o.c.female.h_year_k7$coefficients[6]
  # sp.fit.mod1.ic.o.c.female.h_year_k7$AIC
  # 
  # 
  # 
  # #----------------------------------#
  # sp.fit.mod1.ic.o.c.female.h_AGEVARY_k3 <-                flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec)+ gamma1(Cohort.dec) + gamma2(Cohort.dec) ,
  #                                                                       k = 3, scale = "odds",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # # AIC = 20895.02
  # sp.fit.mod1.ic.o.c.female.h_AGEVARY_k3
  # sp.fit.mod1.ic.o.c.female.h_AGEVARY_k3$coefficients[6]
  # sp.fit.mod1.ic.o.c.female.h_AGEVARY_k3$AIC
  # 
  # #---#
  # sp.fit.mod1.ic.o.c.female.h_AGEVARY_k5 <-                flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec)+ gamma1(Cohort.dec) + gamma2(Cohort.dec) ,
  #                                                                       k = 5, scale = "odds",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # # AIC = 20934.05
  # sp.fit.mod1.ic.o.c.female.h_AGEVARY_k5
  # sp.fit.mod1.ic.o.c.female.h_AGEVARY_k5$coefficients[6]
  # sp.fit.mod1.ic.o.c.female.h_AGEVARY_k5$AIC
  # 
  # #---#
  # 
  # sp.fit.mod1.ic.o.c.female.h_AGEVARY_k6 <-                flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec)+ gamma1(Cohort.dec) + gamma2(Cohort.dec) ,
  #                                                                       k = 6, scale = "odds",
  #                                                                       data = DMarihuana_1998_2016.c.female)
  # # AIC = 21004.97
  # sp.fit.mod1.ic.o.c.female.h_AGEVARY_k6
  # sp.fit.mod1.ic.o.c.female.h_AGEVARY_k6$coefficients[6]
  # sp.fit.mod1.ic.o.c.female.h_AGEVARY_k6$AIC
  # 
  # plot(sp.fit.mod1.ic.o.c.female.h_AGEVARY_k6)
  # 
  # #---#
  # 
  # sp.fit.mod1.ic.o.c.female.h_AGEVARY_k7 <-                flexsurvspline(my.surv.ic.c.female ~ ns(Cohort.dec)+ gamma1(Cohort.dec) + gamma2(Cohort.dec) ,
  #                                                                         k = 7, scale = "odds",
  #                                                                         data = DMarihuana_1998_2016.c.female)
  # # not converge
  # sp.fit.mod1.ic.o.c.female.h_AGEVARY_k7
  # sp.fit.mod1.ic.o.c.female.h_AGEVARY_k7$coefficients[6]
  # sp.fit.mod1.ic.o.c.female.h_AGEVARY_k7$AIC
  # 
  # plot(sp.fit.mod1.ic.o.c.female.h_AGEVARY_k7)
  # 
  # 
  # 
  # # ------- Summary of Models ----------- #
  # # Top three models
  # # No1 : 
  # # Assumptions: 
  # plot()
  # # No2 : 
  # # Assupmtions: 
  # plot()
  # # No3 : 
  # # Assumptions: 
  # plot()
  # 
  # # ---------SAVING BEST FIT MODELS, FEMALES Cohort ------------#
  # 
  # # topthree_females_period_splines <- list(fit.object = list( first = sp.fit.mod1.ic.o.female.h_year_k3, second = sp.fit.mod1.ic.p.female.h_year_k5, third = sp.fit.mod1.ic.p.female.h_year_k3),
  # #                                         AIC = list( first = sp.fit.mod1.ic.o.female.h_year_k3$AIC, second = sp.fit.mod1.ic.p.female.h_year_k5$AIC, third = sp.fit.mod1.ic.p.female.h_year_k3$AIC),
  # #                                         Coeffs = list( first = sp.fit.mod1.ic.o.female.h_year_k3$coefficients, second = sp.fit.mod1.ic.p.female.h_year_k5$coefficients, third = sp.fit.mod1.ic.p.female.h_year_k3$coefficients))
  # # save(topthree_females_period_splines, file = "/Users/mauriciolopezm/Dropbox (Brown)/Troubleshooting-Parametric Models and Splines/topthree_females_period_splines.RData")
  # 
  # 
  #  #saving all, later save the additional models
  # 
  # all_females_cohort_splines <- list(
  #   sp.fit.mod1.ic.c.female.h_year_k1,
  #   sp.fit.mod1.ic.c.female.h_year_k2,
  #   sp.fit.mod1.ic.c.female.h_year_k3,
  #   sp.fit.mod1.ic.c.female.h_year_k5,
  #   "sp.fit.mod1.ic.c.female.h_year_k7-notconverge",
  #   
  #   "sp.fit.mod1.ic.c.female.h_sat_k1-notconverge",
  #   "sp.fit.mod1.ic.c.female.h_sat_k2-notconverge",
  #   "sp.fit.mod1.ic.c.female.h_sat_k3-notconverge",
  #   "sp.fit.mod1.ic.c.female.h_sat_k5-notconverge",
  #   "sp.fit.mod1.ic.c.female.h_sat_k7-notconverge",
  #   
  #   "sp.fit.mod1.ic.c.female.h_AGEVARY_k1-notconverge",
  #   sp.fit.mod1.ic.c.female.h_AGEVARY_k2,
  #   sp.fit.mod1.ic.c.female.h_AGEVARY_k3,
  #   sp.fit.mod1.ic.c.female.h_AGEVARY_k5,
  #   sp.fit.mod1.ic.c.female.h_AGEVARY_k6,
  #   "sp.fit.mod1.ic.c.female.h_AGEVARY_k7-notconverge",
  #   
  #   sp.fit.mod1.ic.c.female.h_quadratic_k1,
  #   sp.fit.mod1.ic.c.female.h_quadratic_k2,
  #   sp.fit.mod1.ic.c.female.h_quadratic_k3, #19
  #   sp.fit.mod1.ic.c.female.h_quadratic_k5,
  #   "sp.fit.mod1.ic.c.female.h_quadratic_k7-notconverge",
  #   
  #   sp.fit.mod1.ic.c.female.h_AGEVARY_quadratic_k1,
  #   sp.fit.mod1.ic.c.female.h_AGEVARY_quadratic_k2,
  #   sp.fit.mod1.ic.c.female.h_AGEVARY_quadratic_k3, #best, index =24
  #   sp.fit.mod1.ic.c.female.h_AGEVARY_quadratic_k5,
  #   "sp.fit.mod1.ic.c.female.h_AGEVARY_quadratic_k7-notconverge",
  #   
  #   sp.fit.mod1.ic.o.c.female.h_year_k1,
  #   sp.fit.mod1.ic.o.c.female.h_year_k2,
  #   sp.fit.mod1.ic.o.c.female.h_year_k3,
  #   sp.fit.mod1.ic.o.c.female.h_year_k5,
  #   "sp.fit.mod1.ic.o.c.female.h_year_k7-notconverge",
  #   
  #   sp.fit.mod1.ic.o.c.female.h_AGEVARY_k3,
  #   sp.fit.mod1.ic.o.c.female.h_AGEVARY_k5,
  #   sp.fit.mod1.ic.o.c.female.h_AGEVARY_k6,
  #   "sp.fit.mod1.ic.o.c.female.h_AGEVARY_k7-notconverge"
  #   
  #   
  # )
  # save(all_females_cohort_splines, file = "/Users/mauriciolopezm/Dropbox (Brown)/Troubleshooting-Parametric Models and Splines/all_females_cohort_splines.RData")
  # #load("/Users/mauriciolopezm/Dropbox (Brown)/Troubleshooting-Parametric Models and Splines/all_females_cohort_splines.RData")
  # 
  # additional_odds_models_females_cohort <- list(sp.fit.mod1.ic.o.c.female.h_AGEVARY_quadratic_k3,
  #                                               sp.fit.mod1.ic.o.c.female.h_quadratic_k3,
  #                                               sp.fit.mod1.ic.o.c.female.h_quadratic_k5)
  # save(additional_odds_models_females_cohort, file = "/Users/mauriciolopezm/Dropbox (Brown)/Troubleshooting-Parametric Models and Splines/additional_odds_models_females_cohort.RData")
  # 
  #   data.frame(Model = seq(1:35), AIC = 
  #              c(
  # all_females_cohort_splines[[1]]    ,
  # all_females_cohort_splines[[2]]$AIC     ,
  # all_females_cohort_splines[[3]]$AIC     ,
  # all_females_cohort_splines[[4]]$AIC     ,
  # "#all_females_cohort_splines[[5]]$AIC"     ,
  # "#all_females_cohort_splines[[6]]$AIC "    ,
  # "#all_females_cohort_splines[[7]]$AIC "    ,
  # "#all_females_cohort_splines[[8]]$AIC "    ,
  # "#all_females_cohort_splines[[9]]$AIC "    ,
  # "#all_females_cohort_splines[[10]]$AIC"    ,
  # "#all_females_cohort_splines[[11]]$AIC"    ,
  # all_females_cohort_splines[[12]]$AIC    ,
  # all_females_cohort_splines[[13]]$AIC    ,
  # all_females_cohort_splines[[14]]$AIC    ,
  # all_females_cohort_splines[[15]]$AIC    ,
  # "#all_females_cohort_splines[[16]]$AIC"    ,
  # all_females_cohort_splines[[17]]$AIC    ,
  # all_females_cohort_splines[[18]]$AIC    ,
  # all_females_cohort_splines[[19]]$AIC    ,
  # all_females_cohort_splines[[20]]$AIC    ,
  # "#all_females_cohort_splines[[21]]$AIC "   ,
  # all_females_cohort_splines[[22]]$AIC    ,
  # all_females_cohort_splines[[23]]$AIC    ,
  # all_females_cohort_splines[[24]]$AIC    ,
  # all_females_cohort_splines[[25]]$AIC    ,
  # "#all_females_cohort_splines[[26]]$AIC"    ,
  # all_females_cohort_splines[[27]]$AIC    ,
  # all_females_cohort_splines[[28]]$AIC    ,
  # all_females_cohort_splines[[29]]$AIC    ,
  # all_females_cohort_splines[[30]]$AIC    ,
  # "#all_females_cohort_splines[[31]]$AIC"    ,
  # all_females_cohort_splines[[32]]$AIC    ,
  # all_females_cohort_splines[[33]]$AIC    ,
  # all_females_cohort_splines[[34]]$AIC    ,
  # "#all_females_cohort_splines[[35]]$AIC"
  # ))
  # 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # ---------------------------------------------------------------------------------------------------------- #
  # Spline Models | Estimates within-sample by decennial Birth-Cohorts ============================= #
  # ================================================================================================ #
  
  #Males, using this model: sp.fit.mod1.ic.c.male.h
  v.cohorts.ext.ic <- c(1,2,3,4,5,6,7)
  df.haz.ext.ic.c.male <- expand.grid(Cohort.dec = v.cohorts.ext.ic, 
                                 stringsAsFactors = T)
  df.haz.ext.ic.c.male
  haz.sp.fit.mod1.ic.c.male <- as.data.frame(summary(sp.fit.mod1.ic.c.male.h, 
                                                newdata = df.haz.ext.ic.c.male,
                                                type = "hazard", ci = TRUE,
                                                tidy = TRUE))
  
  colnames(haz.sp.fit.mod1.ic.c.male)
  val.cohort<- unique(haz.sp.fit.mod1.ic.c.male$`ns(Cohort.dec)`)
  
  
  haz.sp.fit.mod1.ic.c.male <- haz.sp.fit.mod1.ic.c.male %>%
    dplyr::rename(Cohort = `ns(Cohort.dec)`)%>%
    mutate(Cohort = as.numeric(Cohort))%>%
    mutate(Cohort = ifelse(Cohort ==  val.cohort[1],'1930',
                           ifelse(Cohort== val.cohort[2],'1940',
                                  ifelse(Cohort== val.cohort[3], '1950',
                                         ifelse(Cohort==  val.cohort[4], '1960',
                                                ifelse(Cohort== val.cohort[5], '1970',
                                                       ifelse(Cohort== val.cohort[6], '1980','1990')))))))%>%
    dplyr::rename("h_t" = est,
           "Init.Age" = time)
  
  #unique(haz.sp.fit.mod1.ic.c.male$Cohort)
  #colnames(haz.sp.fit.mod1.ic.c.male)
  #head(haz.sp.fit.mod1.ic.c.male)
  plot_df_male  <- haz.sp.fit.mod1.ic.c.male
  plot_df_male$Cohort <- as.factor(plot_df_male$Cohort)
  plot_df_male$Cohort <-factor(plot_df_male$Cohort , levels = c("1990","1980","1970","1960","1950","1940","1930"))
  levels(plot_df_male$Cohort)
  
  # ---------------------- GENERATING BASELINE CUM HAZARDS 
  # 
  v.cohorts.ext.ic <- c(1,2,3,4,5,6,7)
  
  df.survival.ext.ic.c.male <- expand.grid(Cohort.dec = v.cohorts.ext.ic, 
                                        stringsAsFactors = T)
  df.survival.ext.ic.c.male
  survival.sp.fit.mod1.ic.c.male <- as.data.frame(summary(sp.fit.mod1.ic.c.male.h_alt, 
                                                       newdata = df.survival.ext.ic.c.male,
                                                       type = "survival", ci = TRUE,
                                                       tidy = TRUE))
  #colnames(haz.sp.fit.mod1.ic.p.male)
  val.cohort<- unique(survival.sp.fit.mod1.ic.c.male$`ns(Cohort.dec)`)
  
  survival.sp.fit.mod1.ic.c.male<- survival.sp.fit.mod1.ic.c.male%>%
    dplyr::rename(Cohort = `ns(Cohort.dec)`)%>%
    mutate(Cohort = as.numeric(Cohort))%>%
    mutate(Cohort = ifelse(Cohort ==  val.cohort[1],'1930',
                           ifelse(Cohort== val.cohort[2],'1940',
                                  ifelse(Cohort== val.cohort[3], '1950',
                                         ifelse(Cohort==  val.cohort[4], '1960',
                                                ifelse(Cohort== val.cohort[5], '1970',
                                                       ifelse(Cohort== val.cohort[6], '1980','1990')))))))%>%
    dplyr::rename("S_t" = est,
                  "Init.Age" = time) %>%
  mutate("logminuslogS_T" =  log(-log(S_t)))
  colnames(survival.sp.fit.mod1.ic.c.male)
  
  head(survival.sp.fit.mod1.ic.c.male)
  
  unique(survival.sp.fit.mod1.ic.c.male$Cohort)
  
  plot_df_male_logcumhaz  <- survival.sp.fit.mod1.ic.c.male
  plot_df_male_logcumhaz$Cohort <- as.factor(plot_df_male_logcumhaz$Cohort)
  plot_df_male_logcumhaz$Cohort <-factor(plot_df_male_logcumhaz$Cohort , levels = c("1990","1980","1970","1960","1950","1940","1930"))
  levels(plot_df_male_logcumhaz$Cohort)
  range(plot_df_male_logcumhaz$logminuslogS_T)
  
  colnames(plot_df_male_logcumhaz)
  head(plot_df_male_logcumhaz)
  plot3.mod1.ic.c.male_logcumhaz <- ggplot(data = plot_df_male_logcumhaz, aes(x = (Init.Age), y =logminuslogS_T, color = Cohort)) + 
    geom_line(linetype="solid", size=0.5)+  
    #geom_point(data = df_surv.p.male, shape= df_surv.p.male$Period, size = 0.7) +
    labs(title = "Log Cumulative Hazard by Cohort, for Males",
         subtitle = "Flexible Parametric Model - Natural Cubic Spline (knots = 3)")+    
    xlab("log[Age (years)]") +
    ylab(bquote("Log Cumulative Hazard, log[H(a)]")) +
    ylim(c(-15, .05))+
    geom_vline( xintercept = 16)+
    #xlim(c(0,65))+
    theme_bw()+
    #theme(legend.position = c(.9, .68),
    #      legend.box.background = element_rect(colour = "white", size=2))+
    scale_x_continuous(breaks = number_ticks(13)) 
  #scale_y_continuous(breaks = number_ticks(10)) 
  
  plot3.mod1.ic.c.male_logcumhaz
  
# the hazard_alt1

  v.cohorts.years.ext.ic <- list(Year = c(1998,2002,2008,2011,2016), Cohort.dec= c(1,2,3,4,5,6,7))
  df.haz_alt.ext.ic.c.male <-expand.grid(Year = v.cohorts.years.ext.ic$Year, Cohort.dec = v.cohorts.years.ext.ic$Cohort.dec,
                                           stringsAsFactors = T)
  df.haz_alt.ext.ic.c.male
  haz_alt1.sp.fit.mod1.ic.c.male <- as.data.frame(summary(sp.fit.mod1.ic.c.male.h_alt1, 
                                                          newdata = df.haz_alt.ext.ic.c.male,
                                                          type = "hazard", ci = TRUE,
                                                          tidy = TRUE))
  #colnames(haz.sp.fit.mod1.ic.p.male)
  val.cohort<- unique(haz_alt1.sp.fit.mod1.ic.c.male$`ns(Cohort.dec)`)
  
  haz_alt1.sp.fit.mod1.ic.c.male<- haz_alt1.sp.fit.mod1.ic.c.male%>%
    dplyr::rename(Cohort = `ns(Cohort.dec)`)%>%
    mutate(Cohort = as.numeric(Cohort))%>%
    mutate(Cohort = ifelse(Cohort ==  val.cohort[1],'1930',
                           ifelse(Cohort== val.cohort[2],'1940',
                                  ifelse(Cohort== val.cohort[3], '1950',
                                         ifelse(Cohort==  val.cohort[4], '1960',
                                                ifelse(Cohort== val.cohort[5], '1970',
                                                       ifelse(Cohort== val.cohort[6], '1980','1990')))))))%>%
    dplyr::rename("h_t" = est,
                  "Init.Age" = time) #%>%
    #mutate("logminuslogS_T" =  log(-log(S_t)))
  colnames(haz_alt1.sp.fit.mod1.ic.c.male)
  
  head(haz_alt1.sp.fit.mod1.ic.c.male)
  
  unique(haz_alt1.sp.fit.mod1.ic.c.male$Cohort)
  
  plot_df_male_haz_alt1  <- haz_alt1.sp.fit.mod1.ic.c.male
  plot_df_male_haz_alt1$Cohort <- as.factor(plot_df_male_haz_alt1$Cohort)
  plot_df_male_haz_alt1$Cohort <-factor(plot_df_male_haz_alt1$Cohort , levels = c("1990","1980","1970","1960","1950","1940","1930"))
  levels(plot_df_male_haz_alt1$Cohort)
  range(plot_df_male_haz_alt1$logminuslogS_T)
  plot_df_male_haz_alt1$Cohort.dec
  plot_df_male_haz_alt1 <- plot_df_male_haz_alt1 %>%
    mutate(Init.Age = ifelse(Cohort.dec ==1, (1930 + Init.Age),
                             ifelse(Cohort.dec ==2, (1940 + Init.Age), 
                                     ifelse(Cohort.dec==3, (1950 + Init.Age),  
                                            ifelse(Cohort.dec==4, (1960 + Init.Age), 
                                                   ifelse(Cohort.dec==5,(1970 + Init.Age),
                                                          ifelse(Cohort.dec==6, (1980 + Init.Age), 1990 + Init.Age
                                                                  )))))  ) )
  df_surv.c.male_shift <- df_surv.c.male %>%
    mutate(Init.Age = ifelse(Cohort =="1930", (1930 + Init.Age),
                             ifelse(Cohort =="1940", (1940 + Init.Age), 
                                    ifelse(Cohort=="1950", (1950 + Init.Age),  
                                           ifelse(Cohort =="1960", (1960 + Init.Age), 
                                                  ifelse(Cohort =="1970",(1970 + Init.Age),
                                                         ifelse(Cohort =="1980", (1980 + Init.Age), 1990 + Init.Age
                                                         )))))  ) )
  
  plot_complete <- plot_df_male_haz_alt1[,1:5]
  
  plot3.mod1.ic.c.male_haz_alt1 <- ggplot(data = plot_complete, aes(x = Init.Age, y =h_t, color = Cohort)) + 
    geom_point(linetype="solid", size=0.5)+  
    #geom_point(data = df_surv.c.male_shift) +
    labs(title = "Hazard by Cohort, for Males",
         subtitle = "Flexible Parametric Model - Natural Cubic Spline (knots = 3)")+    
    xlab("Period") +
    ylab(bquote("Hazard, h(a)")) +
    ylim(c(0, .04))+
    xlim(c(1930,2055))+
    #geom_vline( xintercept = 16)+
    #xlim(c(0,65))+
    theme_bw()+
    #geom_smooth()+
    #theme(legend.position = c(.9, .68),
     #     legend.box.background = element_rect(colour = "white", size=2))+
    scale_x_continuous(breaks = number_ticks(10)) 
  #scale_y_continuous(breaks = number_ticks(10)) 
  
  plot3.mod1.ic.c.male_haz_alt1
  
  head(df_surv.c.male)
  sum(is.na(plot_df_male_haz_alt1$Init.Age))
  
  # quick graph 
  
  # ggplot(data = haz.sp.fit.mod1.ic.c.male, aes(x=Init.Age, y=h_t))+
  #   geom_line(linetype="solid", size=0.5, color="black")+
  #   facet_grid(~Cohort)
  
  # ---------------------------------------------------------------------------------------------------------- #
  
  #Females, using model sp.fit.mod1.ic.c.female.h
  
  df.haz.ext.ic.c.female <- expand.grid(Cohort.dec = v.cohorts.ext.ic, 
                                 stringsAsFactors = T)
  df.haz.ext.ic.c.female
  haz.sp.fit.mod1.ic.c.female <- as.data.frame(summary(sp.fit.mod1.ic.c.female.h, 
                                                newdata = df.haz.ext.ic.c.female,
                                                type = "hazard", ci = TRUE,
                                                tidy = TRUE))
  
  colnames(haz.sp.fit.mod1.ic.c.female)
  val.cohort<- unique(haz.sp.fit.mod1.ic.c.female$`ns(Cohort.dec)`)
  
  
  haz.sp.fit.mod1.ic.c.female <- haz.sp.fit.mod1.ic.c.female %>%
    dplyr::rename(Cohort = `ns(Cohort.dec)`)%>%
    mutate(Cohort = as.numeric(Cohort))%>%
    mutate(Cohort = ifelse(Cohort ==  val.cohort[1],'1930',
                           ifelse(Cohort== val.cohort[2],'1940',
                                  ifelse(Cohort== val.cohort[3], '1950',
                                         ifelse(Cohort==  val.cohort[4], '1960',
                                                ifelse(Cohort== val.cohort[5], '1970',
                                                       ifelse(Cohort== val.cohort[6], '1980','1990')))))))%>%
    dplyr::rename("h_t" = est,
           "Init.Age" = time)
  
  unique(haz.sp.fit.mod1.ic.c.female$Cohort)
  #colnames(haz.sp.fit.mod1.ic.c.female)
  plot_df_female  <- haz.sp.fit.mod1.ic.c.female
  plot_df_female$Cohort <- as.factor(plot_df_female$Cohort)
  plot_df_female$Cohort <-factor(plot_df_female$Cohort , levels = c("1990","1980","1970","1960","1950","1940","1930"))
  levels(plot_df_female$Cohort)
  
  
  
  
  
  # quick graph 
  
  # ggplot(data = haz.sp.fit.mod1.ic.c.female, aes(x=Init.Age, y=h_t))+
  #   geom_line(linetype="solid", size=0.5, color="black")+
  #   facet_grid(~Cohort)
  
  # ---------------------------------------------------------------------------------------------------------- #
  # Plots from KM versus Splines, by Decennial Cohorts ============================================= #
  # ================================================================================================ #
  # Within Sample, Interval and Right-censored data (1998-2016)
  # Male
  
  #head(df_surv.c.male)
  #head(haz.sp.fit.mod1.ic.c.male)
  
  #Hazard rates plot by Sex and Cohort
  # plot1.mod1.ic.c.male <- ggplot(data = haz.sp.fit.mod1.ic.c.male, aes(x = Init.Age, y = h_t, color= Cohort)) + 
  #   geom_line(linetype="solid", size=0.5)+  
  #   geom_step(data = df_surv.c.male, linetype="solid", size = 0.2, color="blue") +
  #   facet_grid(~Cohort)+
  #   labs(title = "Age-specific Cannabis Initiation Rates by Birth-Cohort of Males:",
  #        subtitle = "Non-parametric vs flexible parametric estimates")+
  #   ylab("h(a)") +
  #   ylim(c(0, 0.045))+
  #   theme_bw()
  # plot1.mod1.ic.c.male
  # 
  # plot2.mod1.ic.c.male <- ggplot(data = haz.sp.fit.mod1.ic.c.male, aes(x = Init.Age, y = h_t, color= Cohort)) + 
  #   geom_line(linetype="solid", size=0.5)+  
  #   geom_point(data = df_surv.c.male, shape= df_surv.c.male$Cohort, size = 1) +
  #   facet_grid(~Cohort)+
  #   labs(title = "Age-specific Cannabis Initiation Rates by Birth-Cohort of Males:",
  #        subtitle = "Non-parametric vs flexible parametric estimates")+
  #   theme(legend.position = "bottom") +
  #   xlab("Age (years)") +
  #   ylab("h(a)") +
  #   #scale_x_continuous(breaks = number_ticks(13))+
  #   ylim(c(0, 0.037))+
  #   theme_bw()
  # plot2.mod1.ic.c.male
  
  plot3.mod1.ic.c.male_1 <- ggplot(data = plot_df_male, aes(x = Init.Age, y = h_t, color= Cohort)) + 
    geom_line(linetype="solid", size=0.5)+  
    #geom_point(data = df_surv.c.male, shape= df_surv.c.male$Cohort, size = 0.7) +
    labs(title = "PANEL-A: Age-Specific Rates of Onset of Cannabis Use by Birth-Cohort of Males",
         subtitle = "Flexible Parametric Model - Natural Cubic Spline (knots = 3)")+    
    xlab("Age (years)") +
    ylab(bquote("Rates of Onset, h(a)")) +
    ylim(c(0, 0.035))+
    #xlim(c(0,65))+
    theme_bw()+
    #theme(legend.position = c(.927, .5),
     #     legend.box.background = element_rect(colour = "white", size=8))+
    scale_x_continuous(breaks = number_ticks(13)) 
    #scale_y_continuous(breaks = number_ticks(10)) 
  
  plot3.mod1.ic.c.male_1
  
  plot3.mod1.ic.c.male_2 <- ggplot(data = plot_df_male, aes(x = Init.Age, y = h_t, color= Cohort)) + 
    geom_line(linetype="solid", size=0.5)+  
    geom_point(data = df_surv.c.male, shape= df_surv.c.male$Cohort, size = 0.7) +
    labs(title = "PANEL-A: Age-Specific Rates of Onset of Cannabis Use by Birth-Cohort of Males",
         subtitle = "Non-parametric vs Flexible Parametric Model")+    
    xlab("Age (years)") +
    ylab(bquote( "Rates of Onset, h(a)")) +
    ylim(c(0, 0.035))+
    #xlim(c(0,65))+
    theme_bw()+
    #theme(legend.position = c(.927, .5),
     #     legend.box.background = element_rect(colour = "white", size=8))+
    scale_x_continuous(breaks = number_ticks(13)) 
  #scale_y_continuous(breaks = number_ticks(10)) 
  
  plot3.mod1.ic.c.male_2
  
  
  #save(plot2.mod1.ic.c.male, plot3.mod1.ic.c.male, file="spline.plots.cohort.male.RData")
  
  
  # Female
  
  #head(df_surv.c.female)
  #head(haz.sp.fit.mod1.ic.c.female)
  
  #Hazard rates plot by Sex and Cohort
  # plot1.mod1.ic.c.female <- ggplot(data = haz.sp.fit.mod1.ic.c.female, aes(x = Init.Age, y = h_t, color= Cohort)) + 
  #   geom_line(linetype="solid", size=0.5)+  
  #   geom_step(data = df_surv.c.female, linetype="solid", size = 0.2, color="blue") +
  #   facet_grid(~Cohort)+
  #   labs(title = "Age-specific Cannabis Initiation Rates by Birth-Cohort of Females:",
  #        subtitle = "Flexible parametric estimates, natural cubic spline (3 internal knots)")+
  #   xlab("Age (years)") +
  #   ylab("h(a)") +
  #   ylim(c(0, 0.015))+
  #   theme_bw()
  # plot1.mod1.ic.c.female
  # 
  # plot2.mod1.ic.c.female <- ggplot(data = haz.sp.fit.mod1.ic.c.female, aes(x = Init.Age, y = h_t, color= Cohort)) + 
  #   geom_line(linetype="solid", size=0.5)+  
  #   geom_point(data = df_surv.c.female, shape= df_surv.c.female$Cohort, size = 1) +
  #   facet_grid(~Cohort)+
  #   theme(legend.position = "bottom") +
  #   labs(title = "Age-specific Cannabis Initiation Rates by Birth-Cohort of Females:",
  #        subtitle = "Non-parametric vs flexible parametric estimates")+
  #   xlab("Age (years)") +
  #   ylab("h(a)") +
  #   #scale_x_continuous(breaks = number_ticks(13)) +
  #   ylim(c(0, 0.037))+
  #   theme_bw()
  # plot2.mod1.ic.c.female
  
  
  plot3.mod1.ic.c.female_1 <- ggplot(data =  plot_df_female, aes(x = Init.Age, y = h_t, color= Cohort)) + 
    geom_line(linetype="solid", size=0.5)+  
    #geom_point(data = df_surv.c.female, shape= df_surv.c.female$Cohort, size = 0.7) +
    labs(title = "PANEL-B: Age-Specific Rates of Onset of Cannabis Use by Birth-Cohort of Females",
         subtitle = "Flexible Parametric Model - Natural Cubic Spline (knots = 3)")+
    xlab("Age (years)") +
    ylab(bquote( "Rates of Onset, h(a)")) +
    ylim(c(0, 0.035))+
    theme_bw()+
    #theme(legend.position = c(.927, .5),
     #     legend.box.background = element_rect(colour = "white", size=8))+
    scale_x_continuous(breaks = number_ticks(13)) 
    #scale_y_continuous(breaks = number_ticks(10)) 
    
  
  plot3.mod1.ic.c.female_1
  
  #save(plot2.mod1.ic.c.female, plot3.mod1.ic.c.female, file="spline.plots.cohort.female.RData")
  
  plot3.mod1.ic.c.female_2 <- ggplot(data =  plot_df_female, aes(x = Init.Age, y = h_t, color= Cohort)) + 
    geom_line(linetype="solid", size=0.5)+  
    #geom_point(data = df_surv.c.female, shape= df_surv.c.female$Cohort, size = 0.7) +
    labs(title = "PANEL-B: Age-Specific Rates of Onset of Cannabis Use by Birth-Cohort of Females",
         subtitle = "Non-parametric vs Flexible Parametric Model")+
    xlab("Age (years)") +
    ylab(bquote( "Rates of Onset, h(a)")) +
    ylim(c(0, 0.035))+
    theme_bw()+
    #theme(legend.position = c(.927, .5),
     #     legend.box.background = element_rect(colour = "white", size=8))+
    scale_x_continuous(breaks = number_ticks(13)) 
  #scale_y_continuous(breaks = number_ticks(10)) 
  
  
  plot3.mod1.ic.c.female_2
  
  