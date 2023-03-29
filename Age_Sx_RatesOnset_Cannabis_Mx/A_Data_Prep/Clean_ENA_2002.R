#-------------------------------------------------------------------------------- #
# Cleaning| ENAH 2002
# Author: Mauricio, Lopez Mendez
# Date: 07/27/2019
#-------------------------------------------------------------------------------- #
# NOTES IN SPANGLISH #
# THIS DATA SETS NEED A LOT OF WORK:
# 1. Data sets is divided in TWO sections, no clear reason why
# 2. No clear id for each entry
# 3. Different data sets contain different information
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

# 1. LOAD DATA ----
data_020_df <- as.data.frame(read_excel("~/Data/DS_ENAH 2002_RAW.xlsx"))
data_02b_df <- as.data.frame(foreign::read.dbf("~/Data/DS_ENAH 2002_b_RAW.DBF"))
# Note, this data set is split in two part, unfortunately each part was provided in different formats

#-------------------------------------------------------------------------------- #
# 2. RECODING  ----
# NOTES IN SPANGLISH #
# Seleccion de variables para analisis  exploratorio {<nombre de variables en  "data.df">}

#Identificadores y Ponderadores
#{ID_PERS-> creado concatenando {CVE_MUN, ESTRATO, CONTROL,CVE_VIV}, ponde_ss-> FACTOREXP}

# Datos Socio-demograficos
#{entidad -> obtenido del merge con data_020_df,
# edad-> P7, sexo -> P5  ambas de data_020_df}
    
# Consumo de Drogas Ilegales

  # Alguna vez
    # Marihuana {P46}
    # Cocaina {P47}
    # Crack {P49}
    # Alucinogenos {P50}
    # Inhalables {P51}
    # Heroina {P52}
    # Estimulantes(anfetaminicos) -> NOT AVAILABLE IN THIS DATA SET
    # Otras {}-> NOT AVAILABLE IN THIS DATA SET

#    -> todas de data_02b_df 

  # Edad Primera vez, Uso ultimos 12 meses, uso ultimos 30 dias
    # Marihuana {P46c, P46g}
    # Cocaina {P47b, P47f}
    # Crack {P49b, P49f}
    # Alucinogenos {P50c,P50g}
    # Inhalables {P51c, P51g}
    # Heroina {P52c, P52g}
    # Estimulantes(anfetaminicos) {}-> NOT AVAILABLE IN THIS DATA SET
    # Otras {}-> NOT AVAILABLE IN THIS DATA SET

#   -> todas de data_02b_df 

#-------------------------------------------------------------------------------- #
# ID AND SOCIOECONOMIC STATUS
data_020_df <- data_020_df %>% 
  mutate(ID_PERS      = paste(MUN, ESTRATO, CONTROL, VIV_SELEC,sep = "", collapse = NULL),
         Region       = NA,
         Year         = 2002) %>%
  rename(State        = ENT,
         Sex          = P5,
         Age_Response = P7, 
         ponde_ss     = FACTOREXP)%>%
  mutate(Age_Response = as.numeric(Age_Response),
         Cohort            = Year - Age_Response)%>%
  select(ID_PERS, ponde_ss, Year, State, Sex, Age_Response, Region, Cohort)

head(data_020_df)
#-------------------------------------------------------------------------------- #
# DRUG USE
data_02b_df <- data_02b_df %>% 
  mutate(ID_PERS           = paste(MUN, ESTRATO, CONTROL, VIV_SELEC,sep = "", collapse = NULL),
         Stimulants        = NA, # stimulants not included in data base 2002
         Other             = NA,
         Age_Stimulants    = NA,
         Age_Other         = NA,
         Age_Marihuana     = as.numeric(P46C),
         Age_Cocaine       = as.numeric(P47B),
         Age_Crack         = as.numeric(P49B),
         Age_Hallucinogens = as.numeric(P50C),
         Age_Inhalants     = as.numeric(P51C),
         Age_Heroin        = as.numeric(P52C)) %>%  # others not included in data base 2002
  rename(Marihuana         = P46,
         Cocaine           = P47,
         Crack             = P49,
         Hallucinogens     = P50 ,
         Inhalants         = P51,
         Heroin            = P52)%>%
  select(ID_PERS, 
         Marihuana,
         Cocaine,
         Crack,
         Hallucinogens,
         Inhalants,
         Heroin,
         Stimulants,
         Other,
         Age_Marihuana    ,
         Age_Cocaine      ,
         Age_Crack        ,
         Age_Hallucinogens,
         Age_Inhalants    ,
         Age_Heroin       ,
         Age_Stimulants   ,
         Age_Other        )%>%
  mutate(Age_r_Marihuana    = NA,
         Age_r_Cocaine      = NA,
         Age_r_Hallucinogens= NA,
         Age_r_Inhalants    = NA,
         Age_r_Heroin       = NA,
         Age_r_Crack        = NA,
         Age_r_Stimulants   = NA,
         Age_r_Other        = NA)

head(data_02b_df)
#-------------------------------------------------------------------------------- #

# 3. MERGING ----

Master_02_df <- merge(data_020_df,data_02b_df,by="ID_PERS")
head(Master_02_df)

#-------------------------------------------------------------------------------- #
# 4.MISSING VALUES ------ 
# 4.1 NOTES ON NAs ----
# Checking missing values for selected variables
sum(as.numeric(is.na(Master_02_df$ID_PERS)))
sum(as.numeric(is.na(Master_02_df$ponde_ss)))
sum(as.numeric(is.na(Master_02_df$Year)))
sum(as.numeric(is.na(Master_02_df$State)))
sum(as.numeric(is.na(Master_02_df$Sex)))
sum(as.numeric(is.na(Master_02_df$Age)))
sum(as.numeric(is.na(Master_02_df$Age_groups)))
sum(as.numeric(is.na(Master_02_df$Age_groups1)))
sum(as.numeric(is.na(Master_02_df$Age_groups_bin)))
sum(as.numeric(is.na(Master_02_df$Region)))
sum(as.numeric(is.na(Master_02_df$Marihuana)))   
which(is.na(Master_02_df$Marihuana))
sum(as.numeric(is.na(Master_02_df$Cocaine     ))) 
which(is.na(Master_02_df$Cocaine))
sum(as.numeric(is.na(Master_02_df$Crack        )))
which(is.na(Master_02_df$Crack))
sum(as.numeric(is.na(Master_02_df$Hallucinogens)))
which(is.na(Master_02_df$Hallucinogens))
sum(as.numeric(is.na(Master_02_df$Inhalants    )))
which(is.na(Master_02_df$Inhalants))
sum(as.numeric(is.na(Master_02_df$Heroin      )))
which(is.na(Master_02_df$Heroin))
sum(as.numeric(is.na(Master_02_df$Stimulants   )))
which(is.na(Master_02_df$Stimulants))
sum(as.numeric(is.na(Master_02_df$Other         )))

# -> only missing values, row 9960 , consider dropping out

#Checking value ranges
#unique(Master_16_df$ID_PERS       )
#unique(Master_16_df$ponde_ss      )
#unique(Master_16_df$Year          )
unique(Master_02_df$State         )
unique(Master_02_df$Sex           )
unique(Master_02_df$Age           )
unique(Master_02_df$Age_groups    )
unique(Master_02_df$Age_groups1   )
unique(Master_02_df$Age_groups_bin)
unique(Master_02_df$Region        )
unique(Master_02_df$Marihuana     )    
unique(Master_02_df$Cocaine       ) 
unique(Master_02_df$Crack         )
unique(Master_02_df$Hallucinogens )
unique(Master_02_df$Inhalants     )
unique(Master_02_df$Heroin        )
unique(Master_02_df$Stimulants    )
unique(Master_02_df$Other         )  

# -> we need to recode State
# -> we need to recode NA in Marihuana-Stimulants


# Response on age of ever consumed, missing values
# Marihuana
# Number of reponses of having ever used
table(Master_02_df$Marihuana)

#  1     2     9 
# 472 10802     3  


# we lost response for 15 people, 15/472 = 3.5%
# however not allr esponses are meaningful, "1" and "99", need to be recoded to NA
table(Master_02_df1$Age_Marihuana)
str(Master_02_df$Age_Marihuana)

# unique(Master_02_df$Age_Marihuana)
# unique(Master_02_df$Age_Cocaine)
# unique(Master_02_df$Age_Crack)
# unique(Master_02_df$Age_Hallucinogens)
# unique(Master_02_df$Age_Inhalants)
# unique(Master_02_df$Age_Heroin)
#-------------------------------------------------------------------------------- #
# 4.2 Recoding NAs ----
Master_02_df1 <- Master_02_df
Master_02_df1$Age_Marihuana[Master_02_df1$Age_Marihuana == 1 ] <- NA
Master_02_df1$Age_Marihuana[Master_02_df1$Age_Marihuana == 999 ] <- NA

Master_02_df1$Age_Cocaine[Master_02_df1$Age_Cocaine == 1 ] <- NA
Master_02_df1$Age_Cocaine[Master_02_df1$Age_Cocaine == 999 ] <- NA

Master_02_df1$Age_Crack[Master_02_df1$Age_Crack == 1 ] <- NA
Master_02_df1$Age_Crack[Master_02_df1$Age_Crack == 999 ] <- NA

Master_02_df1$Age_Hallucinogens[Master_02_df1$Age_Hallucinogens== 1 ] <- NA
Master_02_df1$Age_Hallucinogens[Master_02_df1$Age_Hallucinogens== 999 ] <- NA

Master_02_df1$Age_Inhalants[Master_02_df1$Age_Inhalants == 1 ] <- NA
Master_02_df1$Age_Inhalants[Master_02_df1$Age_Inhalants == 999 ] <- NA

Master_02_df1$Age_Heroin[Master_02_df1$Age_Heroin == 1 ] <- NA
Master_02_df1$Age_Heroin[Master_02_df1$Age_Heroin == 999 ] <- NA


# Of those who responded ever have consumed, number of succesful response on age of first consumption
sum(as.numeric(!is.na(Master_02_df1$Age_Marihuana)))
# 457

#-------------------------------------------------------------------------------- #
#5. State Labels----

Master_02_df1 <- Master_02_df1 %>% mutate(State = recode(State, "01" = "AGUASCALIENTES","02" = "BAJA CALIFORNIA","03" = "BAJA CALIFORNIA SUR", "04" = "CAMPECHE",
                                                       "05" = "COAHUILA","06" = "COLIMA","07" = "CHIAPAS", "08" = "CHIHUAHUA", "09" = "MEXICO CITY",
                                                       "10" = "DURANGO","11" = "GUANAJUATO", "12" = "GUERRERO", "13" = "HIDALGO", "14" = "JALISCO", "15" = "MEXICO",
                                                       "16"= "MICHOACAN", "17" = "MORELOS", "18" = "NAYARIT", "19" = "NUEVO LEON", "20" = "OAXACA", "21" = "PUEBLA",
                                                       "22" = "QUERETARO", "23" = "QUINTANA ROO", "24" = "SAN LUIS POTOSI", "25" = "SINALOA", 
                                                       "26" = "SONORA", "27" = "TABASCO", "28" = "TAMAULIPAS", "29" = "TLAXCALA", "30" = "VERACRUZ", 
                                                       "31" = "YUCATAN", "32" = "ZACATECAS"),
                                        Sex = as.numeric(Sex))     
#REVISAR ORDEN
head(Master_02_df1$State)
Master_02_df1$State
head(Master_02_df1)

#-------------------------------------------------------------------------------- #
# 6. WIDE 2 LONG ---- 

Master_02_df_lng <- reshape2::melt(Master_02_df1, 
                                   id.vars       = c("ID_PERS","ponde_ss", "State","Region", "Year", "Cohort", "Age_Response", "Sex", 
                                                     "Age_Marihuana", 
                                                     "Age_Cocaine",
                                                     "Age_Crack", 
                                                     "Age_Hallucinogens",
                                                     "Age_Inhalants", 
                                                     "Age_Heroin", 
                                                     "Age_Stimulants", 
                                                     "Age_Other",
                                                     "Age_r_Marihuana"    ,
                                                     "Age_r_Cocaine"      ,
                                                     "Age_r_Hallucinogens",
                                                     "Age_r_Inhalants"    ,
                                                     "Age_r_Heroin"       ,
                                                     "Age_r_Crack"        ,
                                                     "Age_r_Stimulants"   ,
                                                     "Age_r_Other"        ), 
                                   variable.name = "Ever", value.name = "Response")



Master_02_df_lng <- Master_02_df_lng %>%
  
  mutate(Consumed      = ifelse(Response == 1, 1, 0),
         NoConsumed    = ifelse(Response == 2, 1, 0),
         NoResponse    = ifelse(Response == 9, 1, 0))
         # Consumed_wt   = as.integer(Consumed   * ponde_ss),  
         # NoConsumed_wt = as.integer(NoConsumed * ponde_ss),
         # NoResponse_wt = as.integer(NoResponse * ponde_ss))


#head(Master_02_df_lng)

openxlsx::write.xlsx(Master_02_df_lng, file = "/Data/DS_ENAH 2002_MASTER_LONG.xlsx")

