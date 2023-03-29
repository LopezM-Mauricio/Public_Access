# Conditional Mean Age of Onset
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

# --- LOADING DATA-COHORT ------------------------- 
# ================================================== #

# Interval and Right-Censored Data (1998-2016)
DMarihuana_1998_2016 <- read_excel("/Users/mauriciolopezm/Dropbox (Brown)/Troubleshooting-Parametric Models and Splines/DS_SURV_MARIHUANA_1998_2016.xlsx")

#
#DMarihuana_1998_2016 <- read_excel("/Users/mauriciolopezm/Documents/GitHub/Ilicit-Drug-Use-Analysis-Mexico/Output/DS_SURV_MARIHUANA_1998_2016.xlsx")
#DMarihuana_1998_2016 <- read_excel("/Users/mauriciolopezm/Documents/GitHub/Ilicit-Drug-Use-Analysis-Mexico/Output/DS_SURV_MARIHUANA_1998_2016_v2.xlsx")
#

DMarihuana_1998_2016$Sex  <- factor(DMarihuana_1998_2016$Sex, labels = c("Male", "Female"))

# ---------------------------------------------------------------------------------------------------------- #
# Creating decennial (10yrs) birth-cohorts ============================= 
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
  mutate(Cohort.dec = cut(Cohort, c(1932,1939, 1949, 1959, 1969, 1979, 1989, 2004)))%>%mutate(
    MInd_AgeL =  as.numeric(is.na(AgeL_Marihuana)),
    MInd_AgeR =  as.numeric(is.na(AgeR_Marihuana)),
  )
levels(DMarihuana_1998_2016.c$Cohort.dec) = v_names_cohort_groups 
DMarihuana_1998_2016.c <- DMarihuana_1998_2016.c%>%
  mutate(Cohort.dec = as.numeric(Cohort.dec))

# ===========  CONDITIONING ON THOSE WHO EVER CONSUMED =========== 

table((DMarihuana_1998_2016$Consumed), useNA = "always")

table(filter(DMarihuana_1998_2016.c, Consumed == 1)$Event)
tail(DMarihuana_1998_2016.c)
head(filter(DMarihuana_1998_2016.c, MInd_AgeL==1) %>% group_by(Response)%>%summarize(sum_miss = sum(MInd_AgeL)))
table(DMarihuana_1998_2016.c$Age_Marihuana, useNA = "always")
table(DMarihuana_1998_2016.c$Response, useNA = "always")

# =========== DATA USED FOR ESTIMATION ========= 
#DMarihuana_1998_2016.c.male and DMarihuana_1998_2016.c.female
#
# Non parametric KM,  Male population
DMarihuana_1998_2016.c.male <- DMarihuana_1998_2016.c%>%
  filter(Sex == "Male" & Consumed ==1)
#61658/141342
summary(DMarihuana_1998_2016.c.male)
head(DMarihuana_1998_2016.c.male)
tail(DMarihuana_1998_2016.c.male)
DMarihuana_1998_2016.c.female <- DMarihuana_1998_2016.c%>%
  filter(Sex == "Female" & Consumed ==1)
#79684/141342
summary(DMarihuana_1998_2016.c.female)
head(DMarihuana_1998_2016.c.female)


# ---------------------------------------------------------------------------------------------------------- #

# =========== SURVIVAL OBJETS =========== 
#male
my.surv.ic.c.male <- Surv(time  = DMarihuana_1998_2016.c.male$AgeL_Marihuana, 
                          time2 = DMarihuana_1998_2016.c.male$AgeR_Marihuana, 
                          event = DMarihuana_1998_2016.c.male$Event, 
                          type  = "interval")

sum(is.na(DMarihuana_1998_2016.c.male$AgeL_Marihuana))
sum(is.na(DMarihuana_1998_2016.c.male$AgeR_Marihuana))
sum(is.na(DMarihuana_1998_2016.c.male$Event))
miss <- data.frame(AgeL_Marihuana=DMarihuana_1998_2016.c.male$AgeL_Marihuana, 
                   AgeR_Marihuana=DMarihuana_1998_2016.c.male$AgeR_Marihuana, 
                   Event=DMarihuana_1998_2016.c.male$Event)%>%mutate(
                   MInd_AgeL =  as.numeric(is.na(AgeL_Marihuana)),
                   MInd_AgeR =  as.numeric(is.na(AgeR_Marihuana)),
                                           )

nrow(filter(miss, MInd_AgeL==1))
table(miss[,2], useNA = "always")
miss[complete.cases(miss),]
#head(my.surv.ic.c.male)

np.fit.ic.c.male <- survfit(my.surv.ic.c.male ~ Cohort.dec, data = DMarihuana_1998_2016.c.male, type="fleming-harrington")
print(np.fit.ic.c.male, rm=18) #restricted mean survival
print(np.fit.ic.c.male, rm=25) #restricted mean survival
print(np.fit.ic.c.male, rm=65) #restricted mean survival

plot(np.fit.ic.c.male, main="Kaplan-Meier estimate with 95% confidence bounds",
     xlab="time", ylab="survival function")

# female
my.surv.ic.c.female <- Surv(time  = DMarihuana_1998_2016.c.female$AgeL_Marihuana, 
                            time2 = DMarihuana_1998_2016.c.female$AgeR_Marihuana, 
                            event = DMarihuana_1998_2016.c.female$Event, 
                            type  = "interval")
#head(my.surv.ic.c.female)

np.fit.ic.c.female <- survfit(my.surv.ic.c.female ~ Cohort.dec, data = DMarihuana_1998_2016.c.female, type="fleming-harrington")
print(np.fit.ic.c.female, rm=18) #restricted mean survival
print(np.fit.ic.c.female, rm=25) #restricted mean survival
print(np.fit.ic.c.female, rm=65) #restricted mean survival

plot(np.fit.ic.c.female, main="Kaplan-Meier estimate with 95% confidence bounds",
     xlab="time", ylab="survival function")

# =========== Mean/Median Age of Onset =========== #

print(np.fit.ic.c.female, rm=65, digits = 3) 
print(np.fit.ic.c.male, rm=65, digits = 3)


# ------------------------------------PERIOD----------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------- #

# Interval and Right-Censored Data (1998-2016)
DMarihuana_1998_2016 <- read_excel("/Users/mauriciolopezm/Dropbox (Brown)/Troubleshooting-Parametric Models and Splines/DS_SURV_MARIHUANA_1998_2016.xlsx")
DMarihuana_1998_2016$Sex  <- factor(DMarihuana_1998_2016$Sex, labels = c("Male", "Female"))
head(DMarihuana_1998_2016)
nrow(DMarihuana_1998_2016)#, total observations

# Male population
DMarihuana_1998_2016.p.male <- DMarihuana_1998_2016%>%
  filter(Sex == "Male"& Consumed ==1)
nrow(DMarihuana_1998_2016.p.male) #61658
str(DMarihuana_1998_2016.p.male)
# Female population
DMarihuana_1998_2016.p.female <- DMarihuana_1998_2016%>%
  filter(Sex == "Female"& Consumed ==1)
nrow(DMarihuana_1998_2016.p.female) #79684

# =========== SURVIVAL OBJETS =========== 


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

# =========== Mean/Median Age of Onset =========== 

print(np.fit.ic.p.female, rm=65) 
print(np.fit.ic.p.male, rm=65)

print(np.fit.ic.c.female, rm=65) 
print(np.fit.ic.c.male, rm=65)



