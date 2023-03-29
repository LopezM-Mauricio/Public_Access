#-------------------------------------------------------------------------------- #
# Cleaning| ENAH 2011-12             #
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
data_11_df <- as.data.frame(read_csv("~/Data/DS_ENA 2011_12_RAW.csv"))
#str(data_11_df$a070k) # NOTE: se importa como logic
head(data_11_df)
#-------------------------------------------------------------------------------- #
# 2. RECODING  ----
# NOTES IN SPANGLISH #
# Seleccion de variables para analisis  exploratorio {<nombre de variables en  "data.df">}

#Identificador y Ponderadores
#{"folio",  "ponde_indiv_final"}

# Datos Socio-demograficos
#{entidad??, "region","regiong"~8 regiones del país ,edad-> a003, sexo ->"a002", "a006" ,"a008" , "a011", "a014a"}

# REGIÓN ESTADOS
# 1-Norcentral: Coahuila, Chihuahua, Durango
# 2-Noroccidental: Baja California, Baja California Sur, Sonora, Sinaloa
# 3-Nororiental: Nuevo León, Tamaulipas, San Luis Potosí Luis Potosí
# 4-Occidental: Zacatecas, Aguascalientes,Jalisco, Colima y Nayarit
# 5-Centro Puebla Tlaxcala Morelos Puebla, Tlaxcala, Morelos,Estado de México, Hidalgo,Querétaro, Guanajuato
# 6-Ciudad de Mx:  Distrito Federal
# 7-Centro Sur: Veracruz, Oaxaca, Guerrero,Michoacán,
# 8-Sur: Yucatán, Quintana Roo, Campeche, Chiapas, Tabasco

##PARA DESCUBRIR QUE ESTADO SE HIZO LA ENCUESTA, HAY QUE VER LA VARIABLE "code_upm"
## LOS PRIMEROS DOS DIGITOS SON LA CLAVE POR EDO###

# lo podemos verificar con : unique( substr(data_11_df$code_upm, 1, 2)) 

# Ingreso, por region 

#  {"a015a", "a015b", a015c"}

# Tabaco			
# { "a030", "a031a", "a033", alguna vez, edad primera vez, ultima vez que fumo}

# Exposicion a Drogas Ilegales			
# { marihuana("a054b","a054c"), otra("a054bb","a054cc")}

# Consumo de Drogas Medicas, fuera de prescripcion			
# opiaceos("a055a","a055a1","a059a", "a061a", "a064a"), 
# tranquilizantes(a055b","a055b1","a059b", "a061b", "a064b"), 
# Sedantes y barbitiricos(a055c","a055c1","a059c", "a061c", "a064c"), 
# Anfetaminas o estimulantes(a055d","a055d1","a059d", "a061d", "a064d") }

# Consumo de Drogas Ilegales
# Alguna vez
#     Marihuana {"a067e", "a067e1"}
#     Cocaina {"a067f", "a067f1"}
#     Crack {"a067g", "a067g1"}
#     Alucinogenos {"a067h", "a067h1"}
#     Inhalables {"a067i", "a067i1"}
#     Heroina {"a067j", "a067j1"}
#     Estimulantes(anfetaminicos) {"a067k", "a067k1"}
#     Otras {"a067l", "a067l1"}
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
Master_11_df <- data_11_df %>%
  mutate(Year               = 2011,
         Region             = factor(regiong),
         State              = substr(code_upm, 1, 2)) %>%
  rename(Sex                = a002, 
         Age_Response       = a003,
         Marihuana          = a067e, 
         Cocaine            = a067f, 
         Crack              = a067g, 
         Hallucinogens      = a067h, 
         Inhalants          = a067i, 
         Heroin             = a067j,  
         Stimulants         = a067k,  
         Other              = a067l,
         ponde_ss           = ponde_indiv_final,
         ID_PERS            = folio,
         Age_Marihuana      = a070e,
         Age_Cocaine        = a070f,
         Age_Crack          = a070g,
         Age_Hallucinogens  = a070h,
         Age_Inhalants      = a070i,
         Age_Heroin         = a070j,
         Age_Stimulants     = a070k,
         Age_Other          = a070l   )%>%
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

head(Master_11_df)
str(Master_11_df)
str(Master_11_df$Age_Marihuana)
#-------------------------------------------------------------------------------- #
# 3.MISSING VALUES ------ 
# 3.1 NOTES ON NAs ----
# Checking missing values for selected variables
sum(as.numeric(is.na(Master_11_df$ID_PERS)))
sum(as.numeric(is.na(Master_11_df$ponde_ss)))
sum(as.numeric(is.na(Master_11_df$Year)))
sum(as.numeric(is.na(Master_11_df$State)))
sum(as.numeric(is.na(Master_11_df$Sex)))
sum(as.numeric(is.na(Master_11_df$Age)))
sum(as.numeric(is.na(Master_11_df$Age_groups)))
sum(as.numeric(is.na(Master_11_df$Age_groups1)))
sum(as.numeric(is.na(Master_11_df$Age_groups_bin)))
sum(as.numeric(is.na(Master_11_df$Region)))
sum(as.numeric(is.na(Master_11_df$Marihuana)))   
which(is.na(Master_11_df$Marihuana))
sum(as.numeric(is.na(Master_11_df$Cocaine     ))) 
which(is.na(Master_11_df$Cocaine))
sum(as.numeric(is.na(Master_11_df$Crack        )))
which(is.na(Master_11_df$Crack))
sum(as.numeric(is.na(Master_11_df$Hallucinogens)))
which(is.na(Master_11_df$Hallucinogens))
sum(as.numeric(is.na(Master_11_df$Inhalants    )))
which(is.na(Master_11_df$Inhalants))
sum(as.numeric(is.na(Master_11_df$Heroin      )))
which(is.na(Master_11_df$Heroin))
sum(as.numeric(is.na(Master_11_df$Stimulants   )))
which(is.na(Master_11_df$Stimulants))
sum(as.numeric(is.na(Master_11_df$Other         )))

# -> only missing values, row 9960 , consider dropping out

#Checking value ranges
#unique(Master_16_df$ID_PERS       )
#unique(Master_16_df$ponde_ss      )
#unique(Master_16_df$Year          )
unique(Master_11_df$State         )
unique(Master_11_df$Sex           )
unique(Master_11_df$Age           )
unique(Master_11_df$Age_groups    )
unique(Master_11_df$Age_groups1   )
unique(Master_11_df$Age_groups_bin)
unique(Master_11_df$Region        )
unique(Master_11_df$Marihuana     )    
unique(Master_11_df$Cocaine       ) 
unique(Master_11_df$Crack         )
unique(Master_11_df$Hallucinogens )
unique(Master_11_df$Inhalants     )
unique(Master_11_df$Heroin        )
unique(Master_11_df$Stimulants    )
unique(Master_11_df$Other         )  

# -> we need to recode State
# -> we need to recode NA in Marihuana-Stimulants

# Response on age of ever consumed, missing values
# Marihuana
# Number of reponses of having ever used
table(Master_11_df$Marihuana)

#  1     2     9 
# 860 15364    24 

# Of those who responded ever have consumed, number of succesful response on age of first consumption
sum(as.numeric(!is.na(Master_11_df$Age_Marihuana)))
# 850
# we lost response for 10 people, 10/860 = 1.17%
# however not allr esponses are meaningful , 999
table(Master_11_df$Age_Marihuana)
str(Master_11_df$Age_Marihuana)

# Age_ever: 8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  30  32  34  35  36  37  38  40  48  51 999 
# freq:     1   2   4   4  20  44  70 116  96 124 106  49  79  17  25  12  12  13   4   7   8   8   4   2   7   1   1   5   6   1   1   1 
#-------------------------------------------------------------------------------- #
# NOTE: The coding of the response variable is different to previous surveys
# 3.2 Recoding NAs ----

Master_11_df1 <- Master_11_df

Master_11_df1$Age_Marihuana[Master_11_df1$Age_Marihuana        == 999 ] <- NA
Master_11_df1$Age_Marihuana[Master_11_df1$Age_Marihuana        == 120 ] <- 12
Master_11_df1$Age_Marihuana[Master_11_df1$Age_Marihuana        == 119 ] <- 19
Master_11_df1$Age_Marihuana[Master_11_df1$Age_Marihuana        == 120 ] <- 20
       
Master_11_df1$Age_Cocaine[Master_11_df1$Age_Cocaine            == 999 ] <- NA
Master_11_df1$Age_Cocaine[Master_11_df1$Age_Cocaine            == 112 ] <- 12
Master_11_df1$Age_Cocaine[Master_11_df1$Age_Cocaine            == 119 ] <- 19
Master_11_df1$Age_Cocaine[Master_11_df1$Age_Cocaine            == 120 ] <- 20

Master_11_df1$Age_Crack[Master_11_df1$Age_Crack                == 999 ] <- NA
Master_11_df1$Age_Crack[Master_11_df1$Age_Crack                == 112 ] <- 12
Master_11_df1$Age_Crack[Master_11_df1$Age_Crack                == 119 ] <- 19
Master_11_df1$Age_Crack[Master_11_df1$Age_Crack                == 120 ] <- 20

Master_11_df1$Age_Hallucinogens[Master_11_df1$Age_Hallucinogens== 999 ] <- NA
Master_11_df1$Age_Hallucinogens[Master_11_df1$Age_Hallucinogens== 112 ] <- 12
Master_11_df1$Age_Hallucinogens[Master_11_df1$Age_Hallucinogens== 119 ] <- 19
Master_11_df1$Age_Hallucinogens[Master_11_df1$Age_Hallucinogens== 120 ] <- 20

Master_11_df1$Age_Inhalants[Master_11_df1$Age_Inhalants        == 999 ] <- NA
Master_11_df1$Age_Inhalants[Master_11_df1$Age_Inhalants        == 112 ] <- 12
Master_11_df1$Age_Inhalants[Master_11_df1$Age_Inhalants        == 119 ] <- 19
Master_11_df1$Age_Inhalants[Master_11_df1$Age_Inhalants        == 120 ] <- 20

Master_11_df1$Age_Heroin[Master_11_df1$Age_Heroin              == 999 ] <- NA
Master_11_df1$Age_Heroin[Master_11_df1$Age_Heroin              == 112 ] <- 12
Master_11_df1$Age_Heroin[Master_11_df1$Age_Heroin              == 119 ] <- 19
Master_11_df1$Age_Heroin[Master_11_df1$Age_Heroin              == 120 ] <- 20
             
Master_11_df1$Age_Stimulants[Master_11_df1$Age_Stimulants      == 999 ] <- NA
Master_11_df1$Age_Stimulants[Master_11_df1$Age_Stimulants      == 112 ] <- 12
Master_11_df1$Age_Stimulants[Master_11_df1$Age_Stimulants      == 119 ] <- 19
Master_11_df1$Age_Stimulants[Master_11_df1$Age_Stimulants      == 120 ] <- 20

Master_11_df1$Age_Other[Master_11_df1$Age_Other                == 999 ] <- NA
Master_11_df1$Age_Other[Master_11_df1$Age_Other                == 112 ] <- 12
Master_11_df1$Age_Other[Master_11_df1$Age_Other                == 119 ] <- 19
Master_11_df1$Age_Other[Master_11_df1$Age_Other                == 120 ] <- 20

unique(Master_11_df1$Age_Marihuana)
table(Master_11_df1$Age_Marihuana)
unique(Master_11_df1$Age_Cocaine)
unique(Master_11_df1$Age_Crack)
unique(Master_11_df1$Age_Hallucinogens)
unique(Master_11_df1$Age_Inhalants)
unique(Master_11_df1$Age_Heroin)
#There is a problem with the variables for heroin!!
unique(Master_11_df1$Age_Stimulants)
unique(Master_11_df1$Age_Other)
#There is a problem with the variables for others!!
#-------------------------------------------------------------------------------- #
#4. State Labels----
Master_11_df1 <- Master_11_df1 %>% mutate(State = recode(State, "01" = "AGUASCALIENTES","02" = "BAJA CALIFORNIA","03" = "BAJA CALIFORNIA SUR", "04" = "CAMPECHE",
                                                       "05" = "COAHUILA","06" = "COLIMA","07" = "CHIAPAS", "08" = "CHIHUAHUA", "09" = "MEXICO CITY",
                                                       "10" = "DURANGO","11" = "GUANAJUATO", "12" = "GUERRERO", "13" = "HIDALGO", "14" = "JALISCO", "15" = "MEXICO",
                                                       "16"= "MICHOACAN", "17" = "MORELOS", "18" = "NAYARIT", "19" = "NUEVO LEON", "20" = "OAXACA", "21" = "PUEBLA",
                                                       "22" = "QUERETARO", "23" = "QUINTANA ROO", "24" = "SAN LUIS POTOSI", "25" = "SINALOA", 
                                                       "26" = "SONORA", "27" = "TABASCO", "28" = "TAMAULIPAS", "29" = "TLAXCALA", "30" = "VERACRUZ", 
                                                       "31" = "YUCATAN", "32" = "ZACATECAS"))     
#REVISAR ORDEN
head(Master_11_df1)
Master_11_df1$State
#-------------------------------------------------------------------------------- #
# 5. WIDE 2 LONG ---- 

df_master_11_lng <- reshape2::melt(Master_11_df1, 
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



df_master_11_lng <- df_master_11_lng %>%
  
  mutate(Consumed      = ifelse(Response == 1, 1, 0),
         NoConsumed    = ifelse(Response == 2, 1, 0),
         NoResponse    = ifelse(Response == 9, 1, 0))
         # Consumed_wt   = as.integer(Consumed   * ponde_ss),  
         # NoConsumed_wt = as.integer(NoConsumed * ponde_ss),
         # NoResponse_wt = as.integer(NoResponse * ponde_ss))

str(df_master_11_lng)
head(df_master_11_lng)

openxlsx::write.xlsx(df_master_11_lng, file = "Data/DS_ENCODAT 2011-12_MASTER_LONG.xlsx")

  

