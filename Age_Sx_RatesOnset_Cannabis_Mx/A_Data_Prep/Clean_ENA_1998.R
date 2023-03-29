#-------------------------------------------------------------------------------- #
# CLEANING | ENAH 1998
# Author: Mauricio, Lopez Mendez
# Date: 07/23/2019
#-------------------------------------------------------------------------------- #
options(max.print=100000000)
# 0. LIBRARIES ----
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
library(openxlexcel)
library(epitools)
library(foreign)
#-------------------------------------------------------------------------------- #

# 1. LOAD DATA ----
data_98_df <- as.data.frame(read_dta("/Users/mauriciolopezm/Documents/GitHub/Ilicit-Drug-Use-Analysis-Mexico/Data/DS_ENA_1998_RAW.dta", encoding = NULL)) # merged data set as given
#-------------------------------------------------------------------------------- #
# 2. RECODING  ----
# NOTES IN SPANGLISH #
# Seleccion de variables para analisis  exploratorio {<nombre de variables en  "data.df">}

#Identificadores y Ponderadores
#{ID_PERS-> "clave", ponde_ss-> falta!!!!!!!!, sospecha de que le llamaron "folio"}

# Datos Socio-demograficos
#{entidad -> "clavedelaentidad",
# edad-> "p2", sexo -> "p1"     }

# Consumo de Drogas Ilegales

  # Alguna vez
      # Marihuana {p45_1}
      # Cocaina {p45_2}
      # Crack -> NOT AVAILABLE IN THIS DATA SET
      # Alucinogenos {p45_3}
      # Inhalables {p45_4}
      # Heroina {p45_5}
      # Estimulantes(anfetaminicos) -> NOT AVAILABLE IN THIS DATA SET
      # Otras -> NOT AVAILABLE IN THIS DATA SET
      
  # Edad Primera vez, Uso ultimos 12 meses, uso ultimos 30 dias
      # Marihuana {p48_1 , p50_1}
      # Cocaina {p48_2 , p50_2}
      # Crack -> NOT AVAILABLE IN THIS DATA SET
      # Alucinogenos 
      # Inhalables {p48_3 , p50_3}
      # Heroina {p48_4 , p50_4}
      # Estimulantes(anfetaminicos) -> NOT AVAILABLE IN THIS DATA SET
      # Otras -> NOT AVAILABLE IN THIS DATA SET
#-------------------------------------------------------------------------------- #

Master_98_df <- data_98_df %>%
  mutate(Year                  = 1998,
         ponde_ss              = NA,
         Region                = NA,
         State                 = factor(clavedelaentidad),
         Crack                 = NA,
         Stimulants            = NA,  
         Other                 = NA,
         Age_r_Crack           = NA,
         Age_r_Stimulants      = NA,
         Age_r_Other           = NA, 
         Age_Marihuana         = NA,
         Age_Cocaine           = NA,
         Age_Hallucinogens     = NA,
         Age_Inhalants         = NA,
         Age_Heroin            = NA,
         Age_Crack             = NA,
         Age_Stimulants        = NA,
         Age_Other             = NA) %>%
  rename(ID_PERS               = clave,
         Sex                   = p1, 
         Age_Response          = p2,
         Marihuana             = p45_1, 
         Cocaine               = p45_2, 
         Hallucinogens         = p45_3, 
         Inhalants             = p45_4, 
         Heroin                = p45_5,
         Age_r_Marihuana       = p48_1,
         Age_r_Cocaine         = p48_2,
         Age_r_Hallucinogens   = p48_3,
         Age_r_Inhalants       = p48_4,
         Age_r_Heroin          = p48_5,) %>%
  select(ID_PERS, ponde_ss, Year, State, Sex, Age_Response, Region, 
         Marihuana          ,
         Cocaine            ,
         Crack              ,
         Hallucinogens      ,
         Inhalants          ,
         Heroin             ,
         Stimulants         ,
         Other              ,
         Age_r_Marihuana    ,
         Age_r_Cocaine      ,
         Age_r_Hallucinogens,
         Age_r_Inhalants    ,
         Age_r_Heroin       ,
         Age_r_Crack        ,
         Age_r_Stimulants   ,
         Age_r_Other        ,
         Age_Marihuana      ,
         Age_Cocaine        ,
         Age_Hallucinogens  ,
         Age_Inhalants      ,
         Age_Heroin         ,
         Age_Crack          ,
         Age_Stimulants     ,
         Age_Other        )%>%
  mutate(Age_Response = as.numeric(Age_Response),
         Cohort            = Year - Age_Response)

str(Master_98_df)
head(Master_98_df)
# Recoding Ever used variables#
# Response on age of ever consumed, missing values
# Marihuana
# Number of reponses of having ever used
table(Master_98_df$Marihuana)

#  1 (SI)    2(NO)     -> this needs recoding 
#   328       5384    

Master_98_df1 <- Master_98_df

Master_98_df1$Marihuana[Master_98_df1$Marihuana == "SI" ] <- 1
Master_98_df1$Marihuana[Master_98_df1$Marihuana == "NO" ] <- 2

Master_98_df1$Cocaine[Master_98_df1$Cocaine == "SI" ] <- 1
Master_98_df1$Cocaine[Master_98_df1$Cocaine == "NO" ] <- 2

Master_98_df1$Hallucinogens[Master_98_df1$Hallucinogens == "SI" ] <- 1
Master_98_df1$Hallucinogens[Master_98_df1$Hallucinogens == "NO" ] <- 2

Master_98_df1$Inhalants[Master_98_df1$Inhalants == "SI" ] <- 1
Master_98_df1$Inhalants[Master_98_df1$Inhalants == "NO" ] <- 2

Master_98_df1$Heroin[Master_98_df1$Heroin == "SI" ] <- 1
Master_98_df1$Heroin[Master_98_df1$Heroin == "NO" ] <- 2

head(Master_98_df1)

# Recoding State labels

Master_98_df1 <- Master_98_df1 %>% mutate(State = recode(State, "1" = "AGUASCALIENTES","2" = "BAJA CALIFORNIA","3" = "BAJA CALIFORNIA SUR", "4" = "CAMPECHE",
                                                         "5" = "COAHUILA","6" = "COLIMA","7" = "CHIAPAS", "8" = "CHIHUAHUA", "9" = "MEXICO CITY",
                                                         "10" = "DURANGO","11" = "GUANAJUATO", "12" = "GUERRERO", "13" = "HIDALGO", "14" = "JALISCO", "15" = "MEXICO",
                                                         "16"= "MICHOACAN", "17" = "MORELOS", "18" = "NAYARIT", "19" = "NUEVO LEON", "20" = "OAXACA", "21" = "PUEBLA",
                                                         "22" = "QUERETARO", "23" = "QUINTANA ROO", "24" = "SAN LUIS POTOSI", "25" = "SINALOA", 
                                                         "26" = "SONORA", "27" = "TABASCO", "28" = "TAMAULIPAS", "29" = "TLAXCALA", "30" = "VERACRUZ",  "31" = "YUCATAN", "32" = "ZACATECAS"),
                                          Sex = recode(Sex, "MASCULINO" = 1, "FEMENINO" = 2),
                                          Age_Response = as.character(Age_Response)) 

head(Master_98_df1$State)
head(Master_98_df1)
Master_98_df$State

#-------------------------------------------------------------------------------- #
# 3. MISSING VALUES ------ 
# NOTES IN SPANGLISH #
#  NOTES ON NAs 
sum(as.numeric(is.na(Master_98_df$ID_PERS)))
sum(as.numeric(is.na(Master_98_df$ponde_ss)))
sum(as.numeric(is.na(Master_98_df$Year)))
sum(as.numeric(is.na(Master_98_df$State)))
sum(as.numeric(is.na(Master_98_df$Sex)))
sum(as.numeric(is.na(Master_98_df$Age)))
sum(as.numeric(is.na(Master_98_df$Age_groups)))
sum(as.numeric(is.na(Master_98_df$Age_groups1)))
sum(as.numeric(is.na(Master_98_df$Age_groups_bin)))
sum(as.numeric(is.na(Master_98_df$Region)))
sum(as.numeric(is.na(Master_98_df$Marihuana)))   
which(is.na(Master_98_df$Marihuana))
sum(as.numeric(is.na(Master_98_df$Cocaine     ))) 
which(is.na(Master_98_df$Cocaine))
sum(as.numeric(is.na(Master_98_df$Crack        )))
which(is.na(Master_98_df$Crack))
sum(as.numeric(is.na(Master_98_df$Hallucinogens)))
which(is.na(Master_98_df$Hallucinogens))
sum(as.numeric(is.na(Master_98_df$Inhalants    )))
which(is.na(Master_98_df$Inhalants))
sum(as.numeric(is.na(Master_98_df$Heroin      )))
which(is.na(Master_98_df$Heroin))
sum(as.numeric(is.na(Master_98_df$Stimulants   )))
which(is.na(Master_98_df$Stimulants))
sum(as.numeric(is.na(Master_98_df$Other         )))

# -> only missing values, row 9960 , consider dropping out

#Checking value ranges
#unique(Master_16_df$ID_PERS       )
#unique(Master_16_df$ponde_ss      )# not included in db
#unique(Master_16_df$Year          )
unique(Master_98_df$State         ) #-> algunos stados no fueron censados,e.g: Aguascalientes 
unique(Master_98_df$Sex           ) # -> need to recode sex to numeric
unique(Master_98_df$Age           ) # just adults?
unique(Master_98_df$Age_groups    )
unique(Master_98_df$Age_groups1   )
unique(Master_98_df$Age_groups_bin)
unique(Master_98_df$Region        )
unique(Master_98_df$Marihuana     )    
unique(Master_98_df$Cocaine       ) 
unique(Master_98_df$Crack         ) # not included in db
unique(Master_98_df$Hallucinogens ) 
unique(Master_98_df$Inhalants     )
unique(Master_98_df$Heroin        )
unique(Master_98_df$Stimulants    )# not included in db
unique(Master_98_df$Other         )  # not included in db



# Of those who responded ever have consumed, number of succesful response on age of first consumption
unique(Master_98_df1$Age_Marihuana)

# ""                "DE 20 A 24 AðOS" "DE 15 A 19 AðOS" "DE 10 A 14 AðOS" "DE 25 A 29 AðOS"
# "DE 30 A 34 AðOS" "DE 35 A 39 AðOS" "40 AðOS O M-S"  
#
sum(as.numeric(!is.na(Master_98_df1$Age_Marihuana)))
#  323 , no NA coded, instead " ". 
# we lost response for 5 people, 5/328 = 1.5%
# however, responses are coded weirdly, in ranges  
table(Master_98_df1$Age_Marihuana)
#age_init_range: ""    "40 AðOS O M-S" "DE 10 A 14 AðOS" "DE 15 A 19 AðOS" "DE 20 A 24 AðOS" "DE 25 A 29 AðOS" 
# freq:         5389         3              23             188                  76                  22 
#"DE 30 A 34 AðOS" "DE 35 A 39 AðOS" 
#        9               2
# Im gonna leave the age of initiation variables as they are for now, will clean them in the recoding R file
# create for now, empty variables for age of initiation and these ones leave them as age ranges of initiation, 
# also create empty age ranges of initiation in the other datasets. 

#-------------------------------------------------------------------------------- #
# 4. WIDE 2 LONG ---- 

Master_98_df_lng <- reshape2::melt(Master_98_df1, 
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

#Master_98_df_lng$Response <- ifelse(Master_98_df_lng$Response == "SI", "1", ifelse(Master_98_df_lng$Response == "NO","2","9"))

Master_98_df_lng <- Master_98_df_lng %>%
  
  mutate(Consumed      = ifelse(Response == 1, 1, 0),
         NoConsumed    = ifelse(Response == 2, 1, 0),
         NoResponse    = NA)
         #NoResponse    = ifelse(Response == 9, 1,0 )) #, ifelse(Response == NA,1, 0)),
         # Consumed_wt   = NA, #as.integer(Consumed   * ponde_ss),  
         # NoConsumed_wt = NA, #as.integer(NoConsumed * ponde_ss),
         # NoResponse_wt = NA) #as.integer(NoResponse * ponde_ss))

#Master_98_df_lng <- Master_98_df_lng %>%
  
openxlsx::write.xlsx(Master_98_df_lng, file = "Data/DS_ENAH 1998_MASTER_LONG.xlsx")

