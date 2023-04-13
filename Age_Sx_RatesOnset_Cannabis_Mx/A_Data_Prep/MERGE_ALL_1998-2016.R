#-------------------------------------------------------------------------------- #
# MERGING | ENAH 1998-2012 & ENCODAT 2016            #
# Author: Mauricio, Lopez Mendez
# Date: 03/07/2019
#-------------------------------------------------------------------------------- #
options(max.print=100000000)
#-------------------------------------------------------------------------------- #
# 0. Libraries ----
library(tidyverse)
library(tidyr)
library(readr)
library(haven)
library(dplyr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(questionr)
library(excel.link)
library(readxl)
library(zipR)
library(openxlsx)
library(epitools)
library(foreign)
#-------------------------------------------------------------------------------- #
# 1. LOAD DATA SETS ----
Data_1998 <- read_excel("D_Output/DS_ENAH 1998_MASTER_LONG.xlsx")
#1998 is interval censored, we need to create the lower and upper bounds of the intervals to be able to estimate the NPMLE
#We call them Age_l_[DrugName], Age_r_[DrugName]
Data_2002 <- read_excel("D_Output/DS_ENAH 2002_MASTER_LONG.xlsx")
#head(Data_2002)
Data_2008 <- read_excel("D_Output/DS_ENCODAT 2007-08_MASTER_LONG.xlsx")
#head(Data_2008)
Data_2011 <- read_excel("D_Output/DS_ENCODAT 2011-12_MASTER_LONG.xlsx")
#head(Data_2011)
Data_2016 <- read_excel("D_Output/DS_ENCODAT 2016-17_MASTER_LONG.xlsx")
#head(Data_2016)
# Note: The individual data sets are available only by request. 
# Please contact the corresponding author at mlopezme@gmail.com
#-------------------------------------------------------------------------------- #
# 2. FINAL RECODE ----
# STANDARDIZING COLUMN FORMATTING
# 1998
#upper bounds
Data_1998 <- Data_1998 %>%
  mutate(Age_Response      =  as.numeric(Age_Response),
         Response          =  as.numeric(Response),
         Consumed          =  as.numeric(Consumed          ),
         NoConsumed        =  as.numeric(NoConsumed        ),
         NoResponse        =  as.numeric(NoResponse        ),
         AgeR_Marihuana    =  as.numeric(ifelse(Age_r_Marihuana     ==  "DE 10 A 14 AðOS", 14, 
                                      ifelse( Age_r_Marihuana     ==  "DE 15 A 19 AðOS", 19, 
                                      ifelse( Age_r_Marihuana     ==  "DE 20 A 24 AðOS", 24,
                                      ifelse( Age_r_Marihuana     ==  "DE 25 A 29 AðOS", 29,
                                      ifelse( Age_r_Marihuana     ==  "DE 30 A 34 AðOS", 34,
                                      ifelse( Age_r_Marihuana     ==  "DE 35 A 39 AðOS", 39,
                                      ifelse( Age_r_Marihuana     ==  "40 AðOS O M-S", 65, NA)))))))),
         AgeR_Cocaine      =as.numeric(ifelse(  Age_r_Cocaine     ==  "DE 10 A 14 AðOS", 14, 
                                      ifelse(   Age_r_Cocaine     ==  "DE 15 A 19 AðOS", 19, 
                                      ifelse(   Age_r_Cocaine     ==  "DE 20 A 24 AðOS", 24,
                                      ifelse(   Age_r_Cocaine     ==  "DE 25 A 29 AðOS", 29,
                                      ifelse(   Age_r_Cocaine     ==  "DE 30 A 34 AðOS", 34,
                                      ifelse(   Age_r_Cocaine     ==  "DE 35 A 39 AðOS", 39,
                                      ifelse(   Age_r_Cocaine     ==  "40 AðOS O M-S", 65,NA)))))))),
         AgeR_Crack        =as.numeric(ifelse(    Age_r_Crack     ==  "DE 10 A 14 AðOS", 14, 
                                      ifelse(     Age_r_Crack     ==  "DE 15 A 19 AðOS", 19, 
                                      ifelse(     Age_r_Crack     ==  "DE 20 A 24 AðOS", 24,
                                      ifelse(     Age_r_Crack     ==  "DE 25 A 29 AðOS", 29,
                                      ifelse(     Age_r_Crack     ==  "DE 30 A 34 AðOS", 34,
                                      ifelse(     Age_r_Crack     ==  "DE 35 A 39 AðOS", 39,
                                      ifelse(     Age_r_Crack     ==  "40 AðOS O M-S", 65,NA)))))))),
         AgeR_Hallucinogens=as.numeric(ifelse(Age_r_Hallucinogens ==  "DE 10 A 14 AðOS", 14, 
                                      ifelse( Age_r_Hallucinogens ==  "DE 15 A 19 AðOS", 19, 
                                      ifelse( Age_r_Hallucinogens ==  "DE 20 A 24 AðOS", 24,
                                      ifelse( Age_r_Hallucinogens ==  "DE 25 A 29 AðOS", 29,
                                      ifelse( Age_r_Hallucinogens ==  "DE 30 A 34 AðOS", 34,
                                      ifelse( Age_r_Hallucinogens ==  "DE 35 A 39 AðOS", 39,
                                      ifelse( Age_r_Hallucinogens ==  "40 AðOS O M-S", 65,NA)))))))),
         AgeR_Inhalants    =as.numeric(ifelse(    Age_r_Inhalants ==  "DE 10 A 14 AðOS", 14, 
                                      ifelse(     Age_r_Inhalants ==  "DE 15 A 19 AðOS", 19, 
                                      ifelse(     Age_r_Inhalants ==  "DE 20 A 24 AðOS", 24,
                                      ifelse(     Age_r_Inhalants ==  "DE 25 A 29 AðOS", 29,
                                      ifelse(     Age_r_Inhalants ==  "DE 30 A 34 AðOS", 34,
                                      ifelse(     Age_r_Inhalants ==  "DE 35 A 39 AðOS", 39,
                                      ifelse(     Age_r_Inhalants ==  "40 AðOS O M-S", 65,NA)))))))),
         AgeR_Heroin       =as.numeric(Age_Heroin       ),
         AgeR_Stimulants   =as.numeric(ifelse(   Age_r_Stimulants ==  "DE 10 A 14 AðOS", 14, 
                                      ifelse(    Age_r_Stimulants ==  "DE 15 A 19 AðOS", 19, 
                                      ifelse(    Age_r_Stimulants ==  "DE 20 A 24 AðOS", 24,
                                      ifelse(    Age_r_Stimulants ==  "DE 25 A 29 AðOS", 29,
                                      ifelse(    Age_r_Stimulants ==  "DE 30 A 34 AðOS", 34,
                                      ifelse(    Age_r_Stimulants ==  "DE 35 A 39 AðOS", 39,
                                      ifelse(    Age_r_Stimulants ==  "40 AðOS O M-S", 65,NA)))))))),
         AgeR_Other        =as.numeric(Age_Other      ))
#lower bounds
Data_1998 <- Data_1998 %>%
  mutate(AgeL_Marihuana         = ifelse(AgeR_Marihuana     != 65, AgeR_Marihuana - 4, 40),
         AgeL_Cocaine           = ifelse(AgeR_Cocaine       != 65, AgeR_Cocaine - 4, 40),
         AgeL_Crack             = ifelse(AgeR_Crack         != 65, AgeR_Crack - 4, 40),
         AgeL_Hallucinogens     = ifelse(AgeR_Hallucinogens != 65, AgeR_Hallucinogens - 4, 40),
         AgeL_Inhalants         = ifelse(AgeR_Inhalants     != 65, AgeR_Inhalants - 4, 40),
         AgeL_Stimulants        = ifelse(AgeR_Stimulants    != 65, AgeR_Stimulants - 4, 40),
         AgeL_Heroin            = NA,
         AgeL_Other             = NA)

# table(Data_1998$Age_Marihuana, useNA = "always")
# table(Data_1998$AgeL_Marihuana, useNA = "always")
# table(Data_1998$AgeR_Marihuana, useNA = "always")
# table(Data_1998$Response, useNA = "always")
# Adding Event variable to facilitate survival object creation
Data_1998 <- Data_1998 %>%
  mutate( Event =  ifelse(Response == 1, 3, ifelse(Response == 2, 0, NA) ) )
#head(Data_1998)
table(Data_1998$Response, useNA = "always")
table(Data_1998$Event, useNA = "always")
#-------------------------------------------------------------------------------- #

# 2008
Data_2008 <- Data_2008%>%
  mutate(Sex = as.numeric(Sex),
         Response          =  as.numeric(Response),
         Consumed     =as.numeric(Consumed          ),
         NoConsumed   =as.numeric(NoConsumed        ),
         NoResponse   =as.numeric(NoResponse        ),
         Age_Marihuana    =as.numeric(Age_Marihuana    ),
         Age_Cocaine      =as.numeric(Age_Cocaine      ),
         Age_Crack        =as.numeric(Age_Crack        ),
         Age_Hallucinogens=as.numeric(Age_Hallucinogens),
         Age_Inhalants    =as.numeric(Age_Inhalants    ),
         Age_Heroin       =as.numeric(Age_Heroin       ),
         Age_Stimulants   =as.numeric(Age_Stimulants   ),
         Age_Other        =as.numeric(Age_Other      ),
         Age_r_Marihuana   = as.character(Age_r_Marihuana   ),
         Age_r_Cocaine     = as.character(Age_r_Cocaine     ),
         Age_r_Hallucinogens= as.character(Age_r_Hallucinogens),
         Age_r_Inhalants   = as.character(Age_r_Inhalants   ),
         Age_r_Heroin      = as.character(Age_r_Heroin      ),
         Age_r_Crack       = as.character(Age_r_Crack       ),
         Age_r_Stimulants  = as.character(Age_r_Stimulants  ),
         Age_r_Other       = as.character(Age_r_Other       ),
         AgeR_Marihuana         = Age_r_Marihuana     ,
         AgeR_Cocaine           = Age_r_Cocaine       ,
         AgeR_Hallucinogens     = Age_r_Hallucinogens ,
         AgeR_Inhalants         = Age_r_Inhalants     ,
         AgeR_Heroin            = Age_r_Heroin        ,
         AgeR_Crack             = Age_r_Crack         ,
         AgeR_Stimulants        = Age_r_Stimulants    ,
         AgeR_Other             = Age_r_Other         ,
         AgeL_Marihuana         = NA,
         AgeL_Cocaine           = NA,
         AgeL_Crack             = NA,
         AgeL_Hallucinogens     = NA,
         AgeL_Inhalants         = NA,
         AgeL_Stimulants        = NA,
         AgeL_Heroin            = NA,
         AgeL_Other             = NA)

# Adding Event variable to facilitate survival object creation
#str(Data_2008)
Data_2008 <- Data_2008 %>%
  mutate( Event =  ifelse(Response == 1, 1, ifelse(Response == 2, 0, NA) ) )
table(Data_2008$Response, useNA = "always")

#str(Data_2008)
#-------------------------------------------------------------------------------- #
# 2011
Data_2011 <- Data_2011 %>%
  mutate(Sex = as.numeric(Sex),
         Response          =  as.numeric(Response),
         Consumed     =as.numeric(Consumed          ),
         NoConsumed   =as.numeric(NoConsumed        ),
         NoResponse   =as.numeric(NoResponse        ),
         Age_Marihuana    =as.numeric(Age_Marihuana    ),
         Age_Cocaine      =as.numeric(Age_Cocaine      ),
         Age_Crack        =as.numeric(Age_Crack        ),
         Age_Hallucinogens=as.numeric(Age_Hallucinogens),
         Age_Inhalants    =as.numeric(Age_Inhalants    ),
         Age_Heroin       =as.numeric(Age_Heroin       ),
         Age_Stimulants   =as.numeric(Age_Stimulants   ),
         Age_Other        =as.numeric(Age_Other      ),
         Age_r_Marihuana   = as.character(Age_r_Marihuana   ),
         Age_r_Cocaine     = as.character(Age_r_Cocaine     ),
         Age_r_Hallucinogens= as.character(Age_r_Hallucinogens),
         Age_r_Inhalants   = as.character(Age_r_Inhalants   ),
         Age_r_Heroin      = as.character(Age_r_Heroin      ),
         Age_r_Crack       = as.character(Age_r_Crack       ),
         Age_r_Stimulants  = as.character(Age_r_Stimulants  ),
         Age_r_Other       = as.character(Age_r_Other       ),
         AgeR_Marihuana         = Age_r_Marihuana     ,
         AgeR_Cocaine           = Age_r_Cocaine       ,
         AgeR_Hallucinogens     = Age_r_Hallucinogens ,
         AgeR_Inhalants         = Age_r_Inhalants     ,
         AgeR_Heroin            = Age_r_Heroin        ,
         AgeR_Crack             = Age_r_Crack         ,
         AgeR_Stimulants        = Age_r_Stimulants    ,
         AgeR_Other             = Age_r_Other         ,
         AgeL_Marihuana         = NA,
         AgeL_Cocaine           = NA,
         AgeL_Crack             = NA,
         AgeL_Hallucinogens     = NA,
         AgeL_Inhalants         = NA,
         AgeL_Stimulants        = NA,
         AgeL_Heroin            = NA,
         AgeL_Other             = NA)
# str(Data_2011)
# table(Data_2011$Age_Marihuana)

# Adding Event variable to facilitate survival object creation
#str(Data_2011)
Data_2011 <- Data_2011 %>%
  mutate( Event =  ifelse(Response == 1, 1, ifelse(Response == 2, 0, NA) ) )
#str(Data_2011)
table(Data_2011$Response, useNA = "always")

#-------------------------------------------------------------------------------- #
# 2002

Data_2002 <- Data_2002 %>%
  mutate(Sex = as.numeric(Sex),
         Response          =  as.numeric(Response),
         Consumed     =as.numeric(Consumed          ),
         NoConsumed   =as.numeric(NoConsumed        ),
         NoResponse   =as.numeric(NoResponse        ),
         Age_Marihuana          =as.numeric(Age_Marihuana    ),
         Age_Cocaine            =as.numeric(Age_Cocaine      ),
         Age_Crack              =as.numeric(Age_Crack        ),
         Age_Hallucinogens      =as.numeric(Age_Hallucinogens),
         Age_Inhalants          =as.numeric(Age_Inhalants    ),
         Age_Heroin             =as.numeric(Age_Heroin       ),
         Age_Stimulants         =as.numeric(Age_Stimulants   ),
         Age_Other              =as.numeric(Age_Other      ),
         Age_r_Marihuana        = as.character(Age_r_Marihuana   ),
         Age_r_Cocaine          = as.character(Age_r_Cocaine     ),
         Age_r_Hallucinogens    = as.character(Age_r_Hallucinogens),
         Age_r_Inhalants        = as.character(Age_r_Inhalants   ),
         Age_r_Heroin           = as.character(Age_r_Heroin      ),
         Age_r_Crack            = as.character(Age_r_Crack       ),
         Age_r_Stimulants       = as.character(Age_r_Stimulants  ),
         Age_r_Other            = as.character(Age_r_Other       ),
         AgeR_Marihuana         = Age_r_Marihuana     ,
         AgeR_Cocaine           = Age_r_Cocaine       ,
         AgeR_Hallucinogens     = Age_r_Hallucinogens ,
         AgeR_Inhalants         = Age_r_Inhalants     ,
         AgeR_Heroin            = Age_r_Heroin        ,
         AgeR_Crack             = Age_r_Crack         ,
         AgeR_Stimulants        = Age_r_Stimulants    ,
         AgeR_Other             = Age_r_Other         ,
         AgeL_Marihuana         = NA,
         AgeL_Cocaine           = NA,
         AgeL_Crack             = NA,
         AgeL_Hallucinogens     = NA,
         AgeL_Inhalants         = NA,
         AgeL_Stimulants        = NA,
         AgeL_Heroin            = NA,
         AgeL_Other             = NA)

# Adding Event variable to facilitate survival object creation
#str(Data_2002)
Data_2002 <- Data_2002 %>%
  mutate( Event =  ifelse(Response == 1, 1, ifelse(Response == 2, 0, NA) ) )
#str(Data_2002)
table(Data_2002$Response, useNA = "always")

#-------------------------------------------------------------------------------- #
#2016
Data_2016 <- Data_2016%>%
  mutate(Sex = as.numeric(Sex),
         Response          =  as.numeric(Response),
         Consumed     =as.numeric(Consumed          ),
         NoConsumed   =as.numeric(NoConsumed        ),
         NoResponse   =as.numeric(NoResponse        ),
         Age_Marihuana    =as.numeric(Age_Marihuana    ),
         Age_Cocaine      =as.numeric(Age_Cocaine      ),
         Age_Crack        =as.numeric(Age_Crack        ),
         Age_Hallucinogens=as.numeric(Age_Hallucinogens),
         Age_Inhalants    =as.numeric(Age_Inhalants    ),
         Age_Heroin       =as.numeric(Age_Heroin       ),
         Age_Stimulants   =as.numeric(Age_Stimulants   ),
         Age_Other        =as.numeric(Age_Other      ),
         Age_r_Marihuana   = as.character(Age_r_Marihuana   ),
         Age_r_Cocaine     = as.character(Age_r_Cocaine     ),
         Age_r_Hallucinogens= as.character(Age_r_Hallucinogens),
         Age_r_Inhalants   = as.character(Age_r_Inhalants   ),
         Age_r_Heroin      = as.character(Age_r_Heroin      ),
         Age_r_Crack       = as.character(Age_r_Crack       ),
         Age_r_Stimulants  = as.character(Age_r_Stimulants  ),
         Age_r_Other       = as.character(Age_r_Other       ),
         AgeR_Marihuana         = Age_r_Marihuana     ,
         AgeR_Cocaine           = Age_r_Cocaine       ,
         AgeR_Hallucinogens     = Age_r_Hallucinogens ,
         AgeR_Inhalants         = Age_r_Inhalants     ,
         AgeR_Heroin            = Age_r_Heroin        ,
         AgeR_Crack             = Age_r_Crack         ,
         AgeR_Stimulants        = Age_r_Stimulants    ,
         AgeR_Other             = Age_r_Other         ,
         AgeL_Marihuana         = NA,
         AgeL_Cocaine           = NA,
         AgeL_Crack             = NA,
         AgeL_Hallucinogens     = NA,
         AgeL_Inhalants         = NA,
         AgeL_Stimulants        = NA,
         AgeL_Heroin            = NA,
         AgeL_Other             = NA)

# Adding Event variable to facilitate survival object creation
#str(Data_2016)
Data_2016 <- Data_2016 %>%
  mutate( Event =  ifelse(Response == 1, 1, ifelse(Response == 2, 0, NA) ) )
#str(Data_2016)
#table(Data_2016$Response, useNA = "always")

#-------------------------------------------------------------------------------- #
# 3. MERGE 1 1998-2016 -----
 
# # SAVING 1998, for individual surv analysis
library(zipR)
openxlsx::write.xlsx(Data_1998, file = "D_Output/DS_CLEAN_CINT_1998.xlsx")

#### 1998-2016 ####
# Incomplete information for 1998:
# -not all drugs included
# - only data for adults
# - no weights
# Incomplete infromation for 2002:
# - some drugs are missing
# MERGING 


Master_98_16_df <- as.data.frame(bind_rows(list(Data_1998,
                                                Data_2002,
                                                Data_2008,
                                                Data_2011,
                                                Data_2016)))

Master_98_16_df$Age_Marihuana[Master_98_16_df$Age_Marihuana == 112] <- NA #this recoding was left out in cleaning
unique(Master_98_16_df$Age_Marihuana)

# sAVING
openxlsx::write.xlsx(Master_98_16_df, file = "D_Output/DS_CLEAN_ALL_1998-2016.xlsx")

#-------------------------------------------------------------------------------- #
# 4. MERGE 2  2002-2016 -----

#### 2002-2016 ####

# Incomplete infromation for 2002:
# - some drugs are missing
# MERGING 


Master_02_16_df <- as.data.frame(bind_rows(list(
                                  Data_2002,
                                  Data_2008,
                                  Data_2011,
                                  Data_2016)))
Master_02_16_df$Age_Marihuana[Master_02_16_df$Age_Marihuana == 112] <- NA #this recoding was left out in cleaning
# Saving
openxlsx::write.xlsx(Master_02_16_df, file = "D_Output/DS_CLEAN_ALL_2002-2016.xlsx")

#-------------------------------------------------------------------------------- #
# 5. Marihuana Data Sets ----

## 5.1 1998-2016| INTERVAL CENSORED STRUCTURE ----
# The surv object generated follows syntax 
# for analysis of interval censored data 

# SUBSETTING TO MARIHUANA USE ONLY

DMarihuana_1998_2016 <- as.data.frame(Master_98_16_df)%>% 
  subset(Ever == "Marihuana")

DMarihuana_1998_2016 <- DMarihuana_1998_2016 %>%
  dplyr ::select(ID_PERS,
                 ponde_ss,
                 State,
                 Year,
                 Cohort,
                 Age_Response,
                 Sex,
                 Response,
                 Consumed,
                 NoConsumed,
                 Age_Marihuana,
                 AgeL_Marihuana,
                 AgeR_Marihuana, Event
  )

table(DMarihuana_1998_2016$Age_Marihuana, useNA = "always")
#head(DMarihuana_1998_2016)
#colnames(DMarihuana_1998_2016)
#
# ACCOMODATE DF TO  SURVIVAL OBJECT 
# The structure that will be used is "interval" for all observations
# this allows for both right censored, and interval censores obs in the same
# data set
# In this structure two time columns are required,
# as well as an event-status column
# -under right censoring: time1 = t, time2 =NA, Event = 0
# -under no censoring: time1 = t, time2 =t, Event = 1
# -under interval censoring: time1 = t1, time2= t2, Event =3
# Note under right censoring, we use the age at time of observation as
# the censoring time, e.g: in 1998 an obs responded at age 23 that has
#  not consumed marihuana, for this observation we would code: 
#  <Event= 0, Time1= 23, Time2=NA>
#-------------------------------------------------------------------------------- #


DMarihuana_1998_2016 <- DMarihuana_1998_2016 %>%
  mutate(AgeL_Marihuana = ifelse(Event == 1, Age_Marihuana, ifelse(Event == 0, Age_Response, ifelse(Event == 3, AgeL_Marihuana,NA))),
         AgeR_Marihuana = ifelse(Event == 1, Age_Marihuana, ifelse(Event == 0, NA          , ifelse(Event == 3, AgeR_Marihuana, NA))))


DMarihuana_1998_2016_v2 <- DMarihuana_1998_2016 %>%
  mutate(AgeL_Marihuana = ifelse(Event == 1, Age_Marihuana, ifelse(Event == 0, Age_Response, ifelse(Event == 3, AgeL_Marihuana,NA))),
         AgeR_Marihuana = ifelse(Event == 1, Age_Marihuana, ifelse(Event == 0, Age_Response, ifelse(Event == 3, AgeR_Marihuana, NA))))

#head(DMarihuana_1998_2016)
#tail(DMarihuana_1998_2016)[,11:14]

openxlsx::write.xlsx(DMarihuana_1998_2016, file    = "D_Output/DS_SURV_MARIHUANA_1998_2016.xlsx")
openxlsx::write.xlsx(DMarihuana_1998_2016_v2, file = "D_Output/DS_SURV_MARIHUANA_1998_2016_v2.xlsx")

#-------------------------------------------------------------------------------- #

## 5.2 2002-2016| RIGHT CENSORED STRUCTURE ----

# Subset without interval censored data from 1998 survey, 

DMarihuana_2002_2016 <- DMarihuana_1998_2016%>%
  filter(Year != 1998)
head(DMarihuana_2002_2016)
unique(DMarihuana_2002_2016$Event)

openxlsx::write.xlsx(DMarihuana_2002_2016, file = "D_Output/DS_SURV_MARIHUANA_2002_2016.xlsx")
