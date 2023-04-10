library(survival)
library(splines)
library(flexsurv)
library(eha)
library(epitools)

library(tidyverse)
library(tidyr)
library(dplyr)
library(rlist)
library(ggplot2)

library(reshape2)
library(readxl)
library(openxlsx)
#library(dampack)
#library(boot)





# Big Tables of rates of onset
load("haz_male_cohort_poly3.h_pred_bsummary.RData")
load("haz_male_period_poly3.h_pred_bsummary.RData")




haz_male_cohort_poly3_pred_bsummary
#View(haz_male_cohort_poly3_pred_bsummary)
#View(haz_male_period_poly3_pred_bsummary)


# --- Cohort Table-Males -----
#load(file = "plot_df_male_c.RData")
#plot_df_male_c <- dplyr::select(plot_df_male_c, Cohort, Init.Age, h_t, lcl, ucl)
#head(plot_df_male_c)
#round((plot_df_male_c[1:20,2:5]*1000),3)
#head(haz_male_cohort_poly3_pred_bsummary)
#View(round(haz_male_cohort_poly3_pred_bsummary[1:20,3:6]*1000,3))

#cbind(round((plot_df_male_c[,2:5]*1000),3), round(haz_male_cohort_poly3_pred_bsummary[,3:6]*1000,3))

# haz_male_cohort_poly3_pred_bsummary[, 3:6]<- haz_male_cohort_poly3_pred_bsummary[, 3:6]*1000
#colnames(haz_male_cohort_poly3_pred_bsummary)
colnames(haz_male_cohort_poly3_pred_bsummary)
male_cohort_panel <- cbind(as.data.frame(haz_male_cohort_poly3_pred_bsummary[,1:2]), as.data.frame(haz_male_cohort_poly3_pred_bsummary[,c(5,8,9)]*1000))
names(male_cohort_panel)<- c("Cohort", "Age","Rate_Onset","LCI","UCI")
male_cohort_panel$Cohort <- as.numeric(male_cohort_panel$Cohort)
male_cohort_panel$Age <- as.numeric(male_cohort_panel$Age)

male_cohort_panel <- round(male_cohort_panel,3)
male_cohort_panel[1:65,]

male_cohort_panel$LCI <- ifelse(male_cohort_panel$LCI <0, 0, male_cohort_panel$LCI)

male_cohort_panel$Rate_Onset_chr <- as.character(format(male_cohort_panel$Rate_Onset, digits = 3))
male_cohort_panel$LCI_chr        <- as.character(format(male_cohort_panel$LCI, digits = 3))
male_cohort_panel$UCI_chr        <- as.character(format(male_cohort_panel$UCI, digits = 3))
#male_cohort_panel$CI_chr         <- paste("(",male_cohort_panel$LCI_chr, ",",  male_cohort_panel$UCI_chr,")")
#male_cohort_panel$text           <- paste(male_cohort_panel$Rate_Onset_chr, " ","(",male_cohort_panel$LCI_chr, ",",  male_cohort_panel$UCI_chr,")")

head(male_cohort_panel)
write.xlsx(male_cohort_panel, file = "male_cohort_panel.xlsx")
save(male_cohort_panel, file = "male_cohort_panel.RData")


# ----

# --- Period Table-Males -----

head(haz_male_period_poly3_pred_bsummary)
#haz_male_period_poly3_pred_bsummary[, 3:6]<- haz_male_period_poly3_pred_bsummary[, 3:6]*1000
male_period_panel <- cbind(as.data.frame(haz_male_period_poly3_pred_bsummary[,1:2]), as.data.frame(haz_male_period_poly3_pred_bsummary[,c(5,8,9)]*1000))

names(male_period_panel)<- c("Period", "Age","Rate_Onset","LCI","UCI")
male_period_panel <- round(male_period_panel,3)

male_period_panel$LCI <- ifelse(male_period_panel$LCI <0.000, 0, male_period_panel$LCI)
head(male_period_panel)
male_period_panel$Rate_Onset_chr <- as.character(format(male_period_panel$Rate_Onset, digits = 3))
male_period_panel$LCI_chr        <- as.character(format(male_period_panel$LCI, digits = 3))
male_period_panel$UCI_chr        <- as.character(format(male_period_panel$UCI, digits = 3))

#male_period_panel %>% mutate(CI_chr         = paste("(",CI_chr, ",", UCI_chr,")", collapse = ""))
#male_period_panel %>% mutate(CI_chr         = gsub(" ", "", paste("(",CI_chr, ",", UCI_chr,")", collapse = "")))

#male_period_panel$text           <- paste(male_period_panel$Rate_Onset_chr," ",male_period_panel$CI_chr)

head(male_period_panel)

write.xlsx(male_period_panel, file = "male_period_panel.xlsx")
save(male_period_panel, file = "male_period_panel.RData")


# --- Cohort Table - Females

# --- Cohort predictions Females ------

# Best proportional odds models
load(file="f_c_o_comparisons.RData")
load( file="final_comparison_all_models.RData")
final_comparison_all_models$cubic$d3_f_c_1     <- f_c_o_comparisons$f_c_o_p3
final_comparison_all_models$quadratic$d2_f_c_1 <- f_c_o_comparisons$f_c_o_p2
final_comparison_all_models$linear$d1_f_c_1    <- f_c_o_comparisons$f_c_o_p1
save(final_comparison_all_models,file="final_comparison_all_models.RData")

mod_odds_males_period   <-  final_comparison_all_models$cubic$d3_m_p_1
mod_odds_females_period <-  final_comparison_all_models$linear$d1_f_p_1
mod_odds_males_cohort   <-  final_comparison_all_models$cubic$d3_m_c_1
mod_odds_females_cohort  <-  final_comparison_all_models$cubic$d3_f_c_1

final_odds_models <- list(mod_odds_males_period  =mod_odds_males_period  ,
                          mod_odds_females_period=mod_odds_females_period,
                          mod_odds_males_cohort  =mod_odds_males_cohort  ,
                          mod_odds_females_cohort=mod_odds_females_cohort)
#save(final_odds_models, file="final_odds_models.RData")
# ---------------------------------------------------------------------------------------------------------- #
load("model_inputs_cohort.RData")
load("model_inputs_period.RData")
# ---------------------------------------------------------------------------------------------------------- #
years_male   <- model_inputs_period$data_dfs$dF_male_p$Year
years_female <- model_inputs_period$data_dfs$df_frmale_p$Year


model_inputs_cohort$data_dfs$dF_male_c$Cohort.dec <- ifelse(model_inputs_cohort$data_dfs$dF_male_c$Cohort.dec== 1, 1930,
                                                            ifelse(model_inputs_cohort$data_dfs$dF_male_c$Cohort.dec==2,  1940,
                                                                   ifelse(model_inputs_cohort$data_dfs$dF_male_c$Cohort.dec==3,  1950,
                                                                          ifelse(model_inputs_cohort$data_dfs$dF_male_c$Cohort.dec==4,  1960,
                                                                                 ifelse(model_inputs_cohort$data_dfs$dF_male_c$Cohort.dec==5,  1970,
                                                                                        ifelse(model_inputs_cohort$data_dfs$dF_male_c$Cohort.dec==6,  1980,
                                                                                               1990))))))

cohorts_female <- model_inputs_cohort$data_dfs$df_frmale_c$Cohort.dec

#------------ TABLES| FEMALES-Cohort -------------- # 


mod_odds_females_cohort
pred_1930_f<- as.data.frame(summary(mod_odds_females_cohort, 
                                    newdata = data.frame(Cohort.dec= 1930, F=0) ,
                                    type = "hazard", ci = TRUE,
                                    tidy = TRUE))%>%mutate(Cohort = "1930")

#head(pred_1930_f)
plot(y=pred_1930_f$est,x=pred_1930_f$time)
pred_1940_f<- as.data.frame(summary(mod_odds_females_cohort, 
                                    newdata = data.frame(Cohort.dec= 1940, F=0) ,
                                    type = "hazard", ci = TRUE,
                                    tidy = TRUE))%>%mutate(Cohort = "1940")

pred_1950_f<- as.data.frame(summary(mod_odds_females_cohort, 
                                    newdata = data.frame(Cohort.dec= 1950, F=0) ,
                                    type = "hazard", ci = TRUE,
                                    tidy = TRUE))%>%mutate(Cohort = "1950")

pred_1960_f<- as.data.frame(summary(mod_odds_females_cohort, 
                                    newdata = data.frame(Cohort.dec= 1960, F=0) ,
                                    type = "hazard", ci = TRUE,
                                    tidy = TRUE))%>%mutate(Cohort = "1960")

pred_1970_f<- as.data.frame(summary(mod_odds_females_cohort, 
                                    newdata = data.frame(Cohort.dec= 1970, F=0) ,
                                    type = "hazard", ci = TRUE,
                                    tidy = TRUE))%>%mutate(Cohort = "1970")
pred_1980_f<- as.data.frame(summary(mod_odds_females_cohort, 
                                    newdata = data.frame(Cohort.dec= 1980, F=0) ,
                                    type = "hazard", ci = TRUE,
                                    tidy = TRUE))%>%mutate(Cohort = "1980")

pred_1990_f<- as.data.frame(summary(mod_odds_females_cohort, 
                                    newdata = data.frame(Cohort.dec= 1990, F=0) ,
                                    type = "hazard", ci = TRUE,
                                    tidy = TRUE))%>%mutate(Cohort = "1990")
plot_df_female_c<-as.data.frame(
  rbind(pred_1930_f[,c(1,2,3,4,6)],pred_1940_f[,c(1,2,3,4,6)],
        pred_1950_f[,c(1,2,3,4,6)],pred_1960_f[,c(1,2,3,4,6)],
        pred_1970_f[,c(1,2,3,4,6)],pred_1980_f[,c(1,2,3,4,6)],
        pred_1990_f[,c(1,2,3,4,6)]))

names(plot_df_female_c) <- c("Age", "Rate_Onset","LCI","UCI","Cohort") 

colnames(plot_df_female_c)
head(plot_df_female_c)

female_cohort_panel     <- select(plot_df_female_c, "Cohort","Age", "Rate_Onset","LCI","UCI") %>% mutate(Cohort = as.numeric(Cohort))
head(female_cohort_panel)
female_cohort_panel$Cohort <- as.numeric(female_cohort_panel$Cohort)
female_cohort_panel$Age    <- as.numeric(female_cohort_panel$Age)
female_cohort_panel$Rate_Onset    <- as.numeric(female_cohort_panel$Rate_Onset)
female_cohort_panel$LCI    <- as.numeric(female_cohort_panel$LCI)
female_cohort_panel$UCI    <- as.numeric(female_cohort_panel$UCI)


female_cohort_panel[,3:5]     <- format(round(female_cohort_panel[,3:5]*1000,3), digits = 3)
head(female_cohort_panel)
female_cohort_panel$Rate_Onset_chr <- as.character(female_cohort_panel$Rate_Onset)
female_cohort_panel$LCI_chr <- as.character(female_cohort_panel$LCI)
female_cohort_panel$UCI_chr <- as.character(female_cohort_panel$UCI)
head(female_cohort_panel)

write.xlsx(female_cohort_panel, file = "female_cohort_panel.xlsx")
save(female_cohort_panel, file = "female_cohort_panel.RData")


# --- Period Table - Females


mod_odds_females_period
pred_1998_f<- as.data.frame(summary(mod_odds_females_period, 
                                    newdata = data.frame(Year= 1998, F=0) ,
                                    type = "hazard", ci = TRUE,
                                    tidy = TRUE))%>%mutate(Year = "1998")
#plot(y=pred_1998$est,x=pred_1998$time)

pred_2002_f<-as.data.frame(summary(mod_odds_females_period, 
                                   newdata = data.frame(Year= 2002, F=0) ,
                                   type = "hazard", ci = TRUE,
                                   tidy = TRUE))%>%mutate(Year = "2002")
pred_2008_f<-as.data.frame(summary(mod_odds_females_period, 
                                   newdata = data.frame(Year= 2002, F=0) ,
                                   type = "hazard", ci = TRUE,
                                   tidy = TRUE))%>%mutate(Year = "2008")
pred_2011_f<-as.data.frame(summary(mod_odds_females_period, 
                                   newdata = data.frame(Year= 2011, F=0) ,
                                   type = "hazard", ci = TRUE,
                                   tidy = TRUE))%>%mutate(Year = "2011")
pred_2016_f<-as.data.frame(summary(mod_odds_females_period, 
                                   newdata = data.frame(Year= 2016, F=0) ,
                                   type = "hazard", ci = TRUE,
                                   tidy = TRUE))%>%mutate(Year = "2016")

plot_df_female_p<-as.data.frame(rbind(pred_1998_f[,c(1,2,3,4,6)],pred_2002_f[,c(1,2,3,4,6)],
                                      pred_2008_f[,c(1,2,3,4,6)],pred_2011_f[,c(1,2,3,4,6)],
                                      pred_2016_f[,c(1,2,3,4,6)]))
names(plot_df_female_p)<- c("Age", "Rate_Onset","LCI","UCI","Period")
colnames(plot_df_female_p)
head(plot_df_female_p)

female_period_panel     <- select(plot_df_female_p, "Period","Age", "Rate_Onset","LCI","UCI") %>% mutate(Period = as.numeric(Period))

female_period_panel[, 3:5] <- female_period_panel[, 3:5]*1000

female_period_panel$Period <- as.numeric(female_period_panel$Period)
female_period_panel$Age <- as.numeric(female_period_panel$Age)

female_period_panel     <- format(round(female_period_panel,3), digits = 3)
head(female_period_panel)
female_period_panel$Rate_Onset_chr <- as.character(female_period_panel$Rate_Onset)
female_period_panel$LCI_chr <- as.character(female_period_panel$LCI)
female_period_panel$UCI_chr <- as.character(female_period_panel$UCI)



write.xlsx(female_period_panel, file = "female_period_panel.xlsx")
save(female_period_panel, file = "female_period_panel.RData")

