#-------------------------------------------------------------------------------- #
#       CELANING|  ENAH 2008-09             #
# Author: Mauricio, Lopez Mendez
# Date: 07/27/2019
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
# 1. LOAD DATA ----
data_08_df <- read_stata("~/Data/DS_ENA_2008_RAW.dta", encoding = NULL)
#-------------------------------------------------------------------------------- #
# 2. RECODING  ----
# NOTES IN SPANGLISH #
# Seleccion de variables para analisis  exploratorio {<nombre de variables en  "data.df">}

#Identificador y Ponderadores
#{"id_pers",  "p_adicc"}

# Datos Socio-demograficos
#{entidad??, region ??  ,edad-> a003, sexo ->"a002", "a006" ,"a008" , "a011", "a014a"}


##PARA DESCUBRIR QUE ESTADO SE HIZO LA ENCUESTA, HAY QUE VER LA VARIABLE "id_upm"
## LOS PRIMEROS 3 characteres SON LA CLAVE POR EDO###!!!!! ->  substr(data_08_df$id_upm, 1, 3)

# Consumo de Drogas Ilegales
# Alguna vez
#     Marihuana {"a067e"}
#     Cocaina {"a067f"}
#     Crack {"a067g"}
#     Alucinogenos {"a067h"}
#     Inhalables {"a067i"}
#     Heroina {"a067j"}
#     Estimulantes(anfetaminicos) {"a067k"}
#     Otras {"a067l"}
# Edad Primera vez, Uso ultimos 12 meses, uso ultimos 30 dias
#     Marihuana {"a070e",	"a072e","a075e"}
#     Cocaina {"a070f",	"a072f","a075f"}
#     Crack {"a070g",	"a072g","a075g"}
#     Alucinogenos {"a070h",	"a072h","a075h"}
#     Inhalables {"a070i",	"a072i","a075i"}
#     Heroina {"a070j",	"a072j","a075j"}
#     Estimulantes(anfetaminicos) {"a070k",	"a072k","a075k"}
#     Otras {"a070l",	"a072l","a075l"}

#-------------------------------------------------------------------------------- #
Master_08_df <- as.data.frame(data_08_df) %>%
  mutate(Year              = 2008,
         Region            = NA,
         State             = substr(data_08_df$id_upm, 1, 3)) %>%
  rename(Sex               = a002, 
         Age_Response      = a003,
         Marihuana         = a067e, 
         Cocaine           = a067f, 
         Crack             = a067g, 
         Hallucinogens     = a067h, 
         Inhalants         = a067i, 
         Heroin            = a067j,  
         Stimulants        = a067k,  
         Other             = a067l,
         ponde_ss          = p_adicc,
         ID_PERS           = id_pers,
         Age_Marihuana     = a070e,
         Age_Cocaine       = a070f,
         Age_Crack         = a070g,
         Age_Hallucinogens = a070h,
         Age_Inhalants     = a070i,
         Age_Heroin        = a070j,
         Age_Stimulants    = a070k,
         Age_Other         = a070l) %>%
  select(ID_PERS, ponde_ss, Year, State, Sex, Age_Response, Region,
         Marihuana        ,
         Cocaine          ,
         Crack            ,
         Hallucinogens    ,
         Inhalants        ,
         Heroin           ,
         Stimulants       ,
         Other            ,
         Age_Marihuana    ,
         Age_Cocaine      ,
         Age_Crack        ,
         Age_Hallucinogens,
         Age_Inhalants    ,
         Age_Heroin       ,
         Age_Stimulants   ,
         Age_Other        )%>%
  mutate(Age_Response = as.numeric(Age_Response),
         Cohort            = Year - Age_Response,
         Age_r_Marihuana    = NA,
         Age_r_Cocaine      = NA,
         Age_r_Hallucinogens= NA,
         Age_r_Inhalants    = NA,
         Age_r_Heroin       = NA,
         Age_r_Crack        = NA,
         Age_r_Stimulants   = NA,
         Age_r_Other        = NA)
head(Master_08_df)
str(Master_08_df)


#-------------------------------------------------------------------------------- #
# 3.MISSING VALUES ------ 
# 3.1 NOTES ON NAs ----
# Checking missing values for selected variables
sum(as.numeric(is.na(Master_08_df$ID_PERS)))
sum(as.numeric(is.na(Master_08_df$ponde_ss)))
sum(as.numeric(is.na(Master_08_df$Year)))
sum(as.numeric(is.na(Master_08_df$State)))
sum(as.numeric(is.na(Master_08_df$Sex)))
sum(as.numeric(is.na(Master_08_df$Age)))
sum(as.numeric(is.na(Master_08_df$Age_groups)))
sum(as.numeric(is.na(Master_08_df$Age_groups1)))
sum(as.numeric(is.na(Master_08_df$Age_groups_bin)))
sum(as.numeric(is.na(Master_08_df$Region)))
sum(as.numeric(is.na(Master_08_df$Marihuana)))   
which(is.na(Master_08_df$Marihuana))
sum(as.numeric(is.na(Master_08_df$Cocaine     ))) 
which(is.na(Master_08_df$Cocaine))
sum(as.numeric(is.na(Master_08_df$Crack        )))
which(is.na(Master_08_df$Crack))
sum(as.numeric(is.na(Master_08_df$Hallucinogens)))
which(is.na(Master_08_df$Hallucinogens))
sum(as.numeric(is.na(Master_08_df$Inhalants    )))
which(is.na(Master_08_df$Inhalants))
sum(as.numeric(is.na(Master_08_df$Heroin      )))
which(is.na(Master_08_df$Heroin))
sum(as.numeric(is.na(Master_08_df$Stimulants   )))
which(is.na(Master_08_df$Stimulants))
sum(as.numeric(is.na(Master_08_df$Other         )))

# -> no missing values, row 9960

#Checking value ranges
#unique(Master_16_df$ID_PERS       )
#unique(Master_16_df$ponde_ss      )
#unique(Master_16_df$Year          )
unique(Master_08_df$State         )
unique(Master_08_df$Sex           )
unique(Master_08_df$Age           )
unique(Master_08_df$Age_groups    )
unique(Master_08_df$Age_groups1   )
unique(Master_08_df$Age_groups_bin)
unique(Master_08_df$Region        )
unique(Master_08_df$Marihuana     )    
unique(Master_08_df$Cocaine       ) 
unique(Master_08_df$Crack         )
unique(Master_08_df$Hallucinogens )
unique(Master_08_df$Inhalants     )
unique(Master_08_df$Heroin        )
unique(Master_08_df$Stimulants    )
unique(Master_08_df$Other         )  

# -> we need to recode State

# Response on age of ever consumed, missing values
# Marihuana
# Number of reponses of having ever used
table(Master_08_df$Marihuana)

#  1     2     9 
# 2097 48826   304  

# Of those who responded ever have consumed, number of succesful response on age of first consumption
sum(as.numeric(!is.na(Master_08_df$Age_Marihuana)))
# 2080
# we lost response for 17 people, 17/2097 = 0.8%
# however not allr esponses are meaningful 
table(Master_08_df$Age_Marihuana)
str(Master_08_df$Age_Marihuana)

# Age_Ever: 5   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  32  33  34  35  36  37  38  40  42  45  46  47  50  62 112 119 120 999 
#freq :     1   2   4   6   7  11  60  99 155 265 261 244 243 115 164  24  46  33  26  48  20  13  19   5  27   3   3   2   8   3   1   1   6   2   2   2   1   2   1   9 106  22   8 

# -> we need to consider that the response is mixed in actual age values and ranges of ages:
# 6 años o menos (<= 6yrs)................006 freq =0
# Antes de los (< 12).....................112 freq =9
# Antes de los (<20)......................119 freq =106
# A los 20 años o más(>= 20)..............120 freq =22
# No sabe/No contesta.....................999 freq =8
# we have 145 ~ 6.9% of responses in these categories


#-------------------------------------------------------------------------------- #
# 3.2 Recoding NAs ----

Master_08_df1$Age_Marihuana[Master_08_df1$Age_Marihuana == 006 ] <- NA
Master_08_df1$Age_Marihuana[Master_08_df1$Age_Marihuana == 112 ] <- NA
Master_08_df1$Age_Marihuana[Master_08_df1$Age_Marihuana == 119 ] <- NA
Master_08_df1$Age_Marihuana[Master_08_df1$Age_Marihuana == 120 ] <- NA
Master_08_df1$Age_Marihuana[Master_08_df1$Age_Marihuana == 999 ] <- NA

Master_08_df1$Age_Cocaine[Master_08_df1$Age_Cocaine == 006 ] <- NA
Master_08_df1$Age_Cocaine[Master_08_df1$Age_Cocaine == 112 ] <- NA
Master_08_df1$Age_Cocaine[Master_08_df1$Age_Cocaine == 119 ] <- NA
Master_08_df1$Age_Cocaine[Master_08_df1$Age_Cocaine == 120 ] <- NA
Master_08_df1$Age_Cocaine[Master_08_df1$Age_Cocaine == 999 ] <- NA

Master_08_df1$Age_Crack[Master_08_df1$Age_Crack == 006 ] <- NA
Master_08_df1$Age_Crack[Master_08_df1$Age_Crack == 112 ] <- NA
Master_08_df1$Age_Crack[Master_08_df1$Age_Crack == 119 ] <- NA
Master_08_df1$Age_Crack[Master_08_df1$Age_Crack == 120 ] <- NA
Master_08_df1$Age_Crack[Master_08_df1$Age_Crack == 999 ] <- NA

Master_08_df1$Age_Hallucinogens[Master_08_df1$Age_Hallucinogens== 006 ] <- NA
Master_08_df1$Age_Hallucinogens[Master_08_df1$Age_Hallucinogens== 112 ] <- NA
Master_08_df1$Age_Hallucinogens[Master_08_df1$Age_Hallucinogens== 119 ] <- NA
Master_08_df1$Age_Hallucinogens[Master_08_df1$Age_Hallucinogens== 120 ] <- NA
Master_08_df1$Age_Hallucinogens[Master_08_df1$Age_Hallucinogens== 999 ] <- NA

Master_08_df1$Age_Inhalants[Master_08_df1$Age_Inhalants == 006 ] <- NA
Master_08_df1$Age_Inhalants[Master_08_df1$Age_Inhalants == 112 ] <- NA
Master_08_df1$Age_Inhalants[Master_08_df1$Age_Inhalants == 119 ] <- NA
Master_08_df1$Age_Inhalants[Master_08_df1$Age_Inhalants == 120 ] <- NA
Master_08_df1$Age_Inhalants[Master_08_df1$Age_Inhalants == 999 ] <- NA

Master_08_df1$Age_Heroin[Master_08_df1$Age_Heroin == 006 ] <- NA
Master_08_df1$Age_Heroin[Master_08_df1$Age_Heroin == 112 ] <- NA
Master_08_df1$Age_Heroin[Master_08_df1$Age_Heroin == 119 ] <- NA
Master_08_df1$Age_Heroin[Master_08_df1$Age_Heroin == 120 ] <- NA
Master_08_df1$Age_Heroin[Master_08_df1$Age_Heroin == 999 ] <- NA

Master_08_df1$Age_Stimulants[Master_08_df1$Age_Stimulants == 006 ] <- NA
Master_08_df1$Age_Stimulants[Master_08_df1$Age_Stimulants == 112 ] <- NA
Master_08_df1$Age_Stimulants[Master_08_df1$Age_Stimulants == 119 ] <- NA
Master_08_df1$Age_Stimulants[Master_08_df1$Age_Stimulants == 120 ] <- NA
Master_08_df1$Age_Stimulants[Master_08_df1$Age_Stimulants == 999 ] <- NA

Master_08_df1$Age_Other[Master_08_df1$Age_Other == 006 ] <- NA
Master_08_df1$Age_Other[Master_08_df1$Age_Other == 112 ] <- NA
Master_08_df1$Age_Other[Master_08_df1$Age_Other == 119 ] <- NA
Master_08_df1$Age_Other[Master_08_df1$Age_Other == 120 ] <- NA
Master_08_df1$Age_Other[Master_08_df1$Age_Other == 999 ] <- NA

unique(Master_08_df1$Age_Marihuana)
unique(Master_08_df1$Age_Cocaine)
unique(Master_08_df1$Age_Crack)
unique(Master_08_df1$Age_Hallucinogens)
unique(Master_08_df1$Age_Inhalants)
unique(Master_08_df1$Age_Heroin)
#There is a problem with the variables for heroin!!
unique(Master_08_df1$Age_Stimulants)
unique(Master_08_df1$Age_Other)
#There is a problem with the variables for others!!

head(Master_08_df1)
#-------------------------------------------------------------------------------- #
#4. State Labels----
Master_08_df1 <- Master_08_df
Master_08_df1 <- Master_08_df1 %>% mutate(State = recode(State, "AGS" = "AGUASCALIENTES","BCN" = "BAJA CALIFORNIA","BCS" = "BAJA CALIFORNIA SUR", "CAM" = "CAMPECHE",
                                                         "COA" = "COAHUILA","COL" = "COLIMA","CHS" = "CHIAPAS", "CHI" = "CHIHUAHUA", "DFE" = "MEXICO CITY",
                                                         "DGO" = "DURANGO","GTO" = "GUANAJUATO", "GRO" = "GUERRERO", "HGO" = "HIDALGO", "JAL" = "JALISCO", "MEX" = "MEXICO",
                                                         "MIC"= "MICHOACAN", "MOR" = "MORELOS", "NAY" = "NAYARIT", "NLE" = "NUEVO LEON", "OAX" = "OAXACA", "PUE" = "PUEBLA",
                                                         "QRO" = "QUERETARO", "ROO" = "QUINTANA ROO", "SLP" = "SAN LUIS POTOSI", "SIN" = "SINALOA", 
                                                         "SON" = "SONORA", "TAB" = "TABASCO", "TAM" = "TAMAULIPAS", "TLA" = "TLAXCALA", "VER" = "VERACRUZ", 
                                                         "YUC" = "YUCATAN", "ZAC" = "ZACATECAS"))
head(Master_08_df1)
#-------------------------------------------------------------------------------- #
# 5. WIDE 2 LONG ---- 

df_master_08_lng <- reshape2::melt(Master_08_df1, 
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
                                   variable.name = "Ever", value.name = "Response")%>%
                                   mutate(Response = as.character(Response))



df_master_08_lng <- df_master_08_lng %>%
  mutate(Consumed      = ifelse(Response == 1, 1, 0),
         NoConsumed    = ifelse(Response == 2, 1, 0),
         NoResponse    = ifelse(Response == 9, 1, 0))
         # Consumed_wt   = as.integer(Consumed   * ponde_ss),  
         # NoConsumed_wt = as.integer(NoConsumed * ponde_ss),
         # NoResponse_wt = as.integer(NoResponse * ponde_ss))

str(df_master_08_lng)
head(df_master_08_lng)

openxlsx::write.xlsx("Data/DS_ENCODAT 2007-08_MASTER_LONG.xlsx")
