#-------------------------------------------------------------------------------- #
# Cleaning| ENCODAT 2016
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
# 1. LOAD DATA ----
data_16_df <- as.data.frame(read_excel("~Data/DS_ENCODAT 2016-17_RAW.xlsx"))
str(data_16_df)
#-------------------------------------------------------------------------------- #
# 2. RECODING  ----
# NOTES IN SPANGLISH #
# Seleccion de variables para analisis  exploratorio {<nombre de variables en  "data.df">}

  #Identificadores y Ponderadores
      #{ID_PERS, ponde_ss}

  # Datos Socio-demograficos
      #{entidad, ds2, ds3, ds4a, ds4b, ds4c, ds6, ds8, ds9, ds10, ds13, ds21,}

  # Tabaco			
      # { tb01, tb02, tb03, tb04, tb05, tb06,}

  # Exposicion a Drogas Ilegales			
      # { marihuana(ed1,ed2,ed4), otra(ed5,ed6a,ed7a,ed6b,ed7b,ed6c,ed7c,ed6d,ed7d)}

  # Consumo de Drogas Medicas, fuera de prescripcion			
      # opiaceos(dm4a,	dm4a_1,	dm5a_1,	dm6a,	dm8a), 
      # tranquilizantes(dm4b,	dm4b_1,	dm5b_1,	dm6b,	dm8b), 
      # Sedantes y barbitiricos(dm4c,	dm4c_1,	dm5c_1,	dm6c,	dm8c), 
      # Anfetaminas o estimulantes(dm4d,	dm4d_1,	dm5d_1,	dm6d,	dm8d) }

  # Consumo de Drogas Ilegales
      # Alguna vez
          # Marihuana {di1a}
          # Cocaina {di1b}
          # Crack {di1c}
          # Alucinogenos {di1d}
          # Inhalables {di1e}
          # Heroina {di1f}
          # Estimulantes(anfetaminicos) {di1g}
          # Otras {di1i}
      # Edad Primera vez, Uso ultimos 12 meses, uso ultimos 30 dias
          # Marihuana {di4a,	di6a,	di8a}
          # Cocaina {di4b,	di6a,	di8a}
          # Crack {di4c,	di6c,	di8c}
          # Alucinogenos {di4d,	di6d,	di8d}
          # Inhalables {di4e,	di6e,	di8e}
          # Heroina {di4f,	di4f_2,	di8f} -> there is a problem with the heroin variables, 
          # Estimulantes(anfetaminicos) {di4g,	di4g_2,	di6g,	di8g}
          # Otras {di4h,	di4h_2,	di6h,	di8h}
#-------------------------------------------------------------------------------- #

Master_16_df <- data_16_df %>%
             mutate(Year              = 2016,
                    Region            = NA,
                    State             = factor(desc_ent)) %>%
             rename(Sex               = ds2, 
                    Age_Response      = ds3,
                    Marihuana         = di1a, 
                    Cocaine           = di1b, 
                    Crack             = di1c, 
                    Hallucinogens     = di1d, 
                    Inhalants         = di1e, 
                    Heroin            = di1f,  
                    Stimulants        = di1g,  
                    Other             = di1h,
                    Age_Marihuana     = di4a,
                    Age_Cocaine       = di4b,
                    Age_Crack         = di4c,
                    Age_Hallucinogens = di4d,
                    Age_Inhalants     = di4e,
                    Age_Heroin        = di4f,
                    Age_Stimulants    = di4g,
                    Age_Other         = di4h ) %>%
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
              Age_r_Other        = NA )

str(Master_16_df)
head(Master_16_df)
#-------------------------------------------------------------------------------- #
# 3.MISSING VALUES ------ 
# 3.1 NOTES ON NAs ----
# Checking missing values for selected variables
#       # sum(as.numeric(is.na(Master_16_df$ID_PERS)))
#       # sum(as.numeric(is.na(Master_16_df$ponde_ss)))
#       # sum(as.numeric(is.na(Master_16_df$Year)))
#       # sum(as.numeric(is.na(Master_16_df$State)))
#       # sum(as.numeric(is.na(Master_16_df$Sex)))
#       # sum(as.numeric(is.na(Master_16_df$Age)))
#       # sum(as.numeric(is.na(Master_16_df$Age_groups)))
#       # sum(as.numeric(is.na(Master_16_df$Age_groups1)))
#       # sum(as.numeric(is.na(Master_16_df$Age_groups_bin)))
#       # sum(as.numeric(is.na(Master_16_df$Region)))
#       # sum(as.numeric(is.na(Master_16_df$Marihuana)))    
#       # sum(as.numeric(is.na(Master_16_df$Cocaine     ))) 
#       # sum(as.numeric(is.na(Master_16_df$Crack        )))
#       # sum(as.numeric(is.na(Master_16_df$Hallucinogens)))
#       # sum(as.numeric(is.na(Master_16_df$Inhalants    )))
#       # sum(as.numeric(is.na(Master_16_df$Heroin      )))
#       # sum(as.numeric(is.na(Master_16_df$Stimulants   )))
#       # sum(as.numeric(is.na(Master_16_df$Other         )))
# 
#       # -> No missing values , possibly recorded as no response (9)
# 
# #Checking value ranges
#       #unique(Master_16_df$ID_PERS       )
#       #unique(Master_16_df$ponde_ss      )
#       #unique(Master_16_df$Year          )
#       unique(Master_16_df$State         )
#       unique(Master_16_df$Sex           )
#       unique(Master_16_df$Age           )
#       unique(Master_16_df$Age_groups    )
#       unique(Master_16_df$Age_groups1   )
#       unique(Master_16_df$Age_groups_bin)
#       unique(Master_16_df$Region        )
#       unique(Master_16_df$Marihuana     )    
#       unique(Master_16_df$Cocaine       ) 
#       unique(Master_16_df$Crack         )
#       unique(Master_16_df$Hallucinogens )
#       unique(Master_16_df$Inhalants     )
#       unique(Master_16_df$Heroin        )
#       unique(Master_16_df$Stimulants    )
#       unique(Master_16_df$Other         )  
#       
      # -> we need to recode State
# Response on age of ever consumed, missing values
  # Marihuana
    # Number of reponses of having ever used
    table(Master_16_df$Marihuana)

  #  1     2     9 
  # 4217 52417   243 
  
  # Of those who responded ever have consumed, number of succesful response on age of first consumption
    sum(as.numeric(!is.na(Master_16_df$Age_Marihuana)))
    # 4145
    # we lost response for 72 people, 72/4217 = 1.7%
    # however not allr esponses are meaningful 
    table(Master_16_df$Age_Marihuana)
    str(Master_16_df$Age_Marihuana)
    # Age_ever_1:  1   2   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36 
    # freq :       5   1   8   7  10   8  22  53 188 247 399 535 516 443 477 185 265  71 130  58  49  77  28  24  26  11  48   6   6   8   9  18   1
    # Age_ever_2:  37  38  39  40  41  42  43  44  45  47  48  50  53  55  56  59 111 119 120 999
    # freq :       4   4   1  10   1   2   1   2   6   1   4   6   1   2   1   1  22  72  27  38
    
    # -> we need to consider that the response is mixed in actual age values and ranges of ages:
    # 6 años o menos (<= 6yrs)................006 freq =0
    # Antes de los (< 12).....................111 freq =22
    # Antes de los (<20)......................119 freq =72
    # A los 20 años o más(>= 20)..............120 freq =27
    # No sabe/No contesta.....................999 freq =38
    # we have 22+72+27+38 ~ 4% of responses in these categories
    # 
    # We have 2 options, ignore these responses, or assign them an actual agee value based on the empirical distribution of age response
    # For now we will ignore them and code them as NAs
#-------------------------------------------------------------------------------- #
# 3.2 Recoding NAs ----
Master_16_df1 <- Master_16_df
Master_16_df1$Age_Marihuana[Master_16_df1$Age_Marihuana == 006 ] <- NA
Master_16_df1$Age_Marihuana[Master_16_df1$Age_Marihuana == 111 ] <- NA
Master_16_df1$Age_Marihuana[Master_16_df1$Age_Marihuana == 119 ] <- NA
Master_16_df1$Age_Marihuana[Master_16_df1$Age_Marihuana == 120 ] <- NA
Master_16_df1$Age_Marihuana[Master_16_df1$Age_Marihuana == 999 ] <- NA

Master_16_df1$Age_Cocaine[Master_16_df1$Age_Cocaine == 006 ] <- NA
Master_16_df1$Age_Cocaine[Master_16_df1$Age_Cocaine == 111 ] <- NA
Master_16_df1$Age_Cocaine[Master_16_df1$Age_Cocaine == 119 ] <- NA
Master_16_df1$Age_Cocaine[Master_16_df1$Age_Cocaine == 120 ] <- NA
Master_16_df1$Age_Cocaine[Master_16_df1$Age_Cocaine == 999 ] <- NA

Master_16_df1$Age_Crack[Master_16_df1$Age_Crack == 006 ] <- NA
Master_16_df1$Age_Crack[Master_16_df1$Age_Crack == 111 ] <- NA
Master_16_df1$Age_Crack[Master_16_df1$Age_Crack == 119 ] <- NA
Master_16_df1$Age_Crack[Master_16_df1$Age_Crack == 120 ] <- NA
Master_16_df1$Age_Crack[Master_16_df1$Age_Crack == 999 ] <- NA

Master_16_df1$Age_Hallucinogens[Master_16_df1$Age_Hallucinogens== 006 ] <- NA
Master_16_df1$Age_Hallucinogens[Master_16_df1$Age_Hallucinogens== 111 ] <- NA
Master_16_df1$Age_Hallucinogens[Master_16_df1$Age_Hallucinogens== 119 ] <- NA
Master_16_df1$Age_Hallucinogens[Master_16_df1$Age_Hallucinogens== 120 ] <- NA
Master_16_df1$Age_Hallucinogens[Master_16_df1$Age_Hallucinogens== 999 ] <- NA

Master_16_df1$Age_Inhalants[Master_16_df1$Age_Inhalants == 006 ] <- NA
Master_16_df1$Age_Inhalants[Master_16_df1$Age_Inhalants == 111 ] <- NA
Master_16_df1$Age_Inhalants[Master_16_df1$Age_Inhalants == 119 ] <- NA
Master_16_df1$Age_Inhalants[Master_16_df1$Age_Inhalants == 120 ] <- NA
Master_16_df1$Age_Inhalants[Master_16_df1$Age_Inhalants == 999 ] <- NA

Master_16_df1$Age_Heroin[Master_16_df1$Age_Heroin == 006 ] <- NA
Master_16_df1$Age_Heroin[Master_16_df1$Age_Heroin == 111 ] <- NA
Master_16_df1$Age_Heroin[Master_16_df1$Age_Heroin == 119 ] <- NA
Master_16_df1$Age_Heroin[Master_16_df1$Age_Heroin == 120 ] <- NA
Master_16_df1$Age_Heroin[Master_16_df1$Age_Heroin == 999 ] <- NA

Master_16_df1$Age_Stimulants[Master_16_df1$Age_Stimulants == 006 ] <- NA
Master_16_df1$Age_Stimulants[Master_16_df1$Age_Stimulants == 111 ] <- NA
Master_16_df1$Age_Stimulants[Master_16_df1$Age_Stimulants == 119 ] <- NA
Master_16_df1$Age_Stimulants[Master_16_df1$Age_Stimulants == 120 ] <- NA
Master_16_df1$Age_Stimulants[Master_16_df1$Age_Stimulants == 999 ] <- NA

Master_16_df1$Age_Other[Master_16_df1$Age_Other == 006 ] <- NA
Master_16_df1$Age_Other[Master_16_df1$Age_Other == 111 ] <- NA
Master_16_df1$Age_Other[Master_16_df1$Age_Other == 119 ] <- NA
Master_16_df1$Age_Other[Master_16_df1$Age_Other == 120 ] <- NA
Master_16_df1$Age_Other[Master_16_df1$Age_Other == 999 ] <- NA

unique(Master_16_df1$Age_Marihuana)
unique(Master_16_df1$Age_Cocaine)
unique(Master_16_df1$Age_Crack)
unique(Master_16_df1$Age_Hallucinogens)
unique(Master_16_df1$Age_Inhalants)
unique(Master_16_df1$Age_Heroin)
#There is a problem with the variables for heroin!!
unique(Master_16_df1$Age_Stimulants)
unique(Master_16_df1$Age_Other)
#There is a problem with the variables for others!!
str( Master_16_df$Age_Marihuana)
#-------------------------------------------------------------------------------- #
#4. State Labels----

Master_16_df1 <- Master_16_df1 %>% mutate(State = recode(Master_16_df$State, "01 AGUASCALIENTES" = "AGUASCALIENTES","02 BAJA CALIFORNIA" = "BAJA CALIFORNIA","03 BAJA CALIFORNIA SUR" = "BAJA CALIFORNIA SUR", "04 CAMPECHE" = "CAMPECHE",
                                         "05 COAHUILA DE ZARAGOZA" = "COAHUILA","06 COLIMA" = "COLIMA","07 CHIAPAS" = "CHIAPAS", "08 CHIHUAHUA" = "CHIHUAHUA", "09 DISTRITO FEDERAL" = "MEXICO CITY",
                                          "10 DURANGO" = "DURANGO","11 GUANAJUATO" = "GUANAJUATO", "12 GUERRERO" = "GUERRERO", "13 HIDALGO" = "HIDALGO", "14 JALISCO" = "JALISCO", "15 M_XICO" = "MEXICO",
                                          "16 MICHOAC_N DE OCAMPO"= "MICHOACAN", "17 MORELOS" = "MORELOS", "18 NAYARIT" = "NAYARIT", "19 NUEVO LE_N" = "NUEVO LEON", "20 OAXACA" = "OAXACA", "21 PUEBLA" = "PUEBLA",
                                          "22 QUER_TARO" = "QUERETARO", "23 QUINTANA ROO" = "QUINTANA ROO", "24 SAN LUIS POTOS_" = "SAN LUIS POTOSI", "25 SINALOA" = "SINALOA", 
                                          "26 SONORA" = "SONORA", "27 TABASCO" = "TABASCO", "28 TAMAULIPAS" = "TAMAULIPAS", "29 TLAXCALA" = "TLAXCALA", "30 VERACRUZ DE IGNACIO DE LA LLAVE" = "VERACRUZ", 
                                          "31 YUCAT_N" = "YUCATAN", "32 ZACATECAS" = "ZACATECAS"))



head(Master_16_df1)
#-------------------------------------------------------------------------------- #
# 5. WIDE 2 LONG ---- 

df_master_16_lng <- reshape2::melt(Master_16_df1, 
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



df_master_16_lng <- df_master_16_lng %>%
  
  mutate(Consumed      = ifelse(Response == 1, 1, 0),
         NoConsumed    = ifelse(Response == 2, 1, 0),
         NoResponse    = ifelse(Response == 9, 1, 0))
         # Consumed_wt   = as.integer(Consumed   * ponde_ss),  
         # NoConsumed_wt = as.integer(NoConsumed * ponde_ss),
         # NoResponse_wt = as.integer(NoResponse * ponde_ss))

str(df_master_16_lng)
head(df_master_16_lng)

openxlsx::write.xlsx(df_master_16_lng, file = "Data/DS_ENCODAT 2016-17_MASTER_LONG.xlsx")



