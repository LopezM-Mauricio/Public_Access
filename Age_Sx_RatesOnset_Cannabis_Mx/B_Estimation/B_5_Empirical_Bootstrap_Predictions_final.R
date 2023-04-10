# Estimates Using Boostrap estimates 
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
library(dampack)
library(boot)
 
# loading boot estimates
# period, males
load("output_splines_male_bootsamples_period_bestpoly3_haz.RData")
# cohort, males
load("output_splines_male_bootsamples_cohort1_bestpoly3_haz.RData")


#list of coefficients, cubic model, male period
coefs_male.p3 <- list()
for ( i in seq_along(temp.m.p3.h)){
  if (typeof(temp.m.p3.h[[i]]) == "character"){
    coefs_male.p3[[i]] <- rep(NA, 11)} else{
      coefs_male.p3[[i]] <- temp.m.p3.h[[i]]$coefficients
    }}
names_list3.coeffs <- paste0("fit_coefs_",as.character(1:300)) 
names(coefs_male.p3) <- names_list3.coeffs
coefs_male.p3 <- as.matrix(bind_rows((coefs_male.p3)))
col_index_p <- as.numeric(which(!is.na(t(coefs_male.p3)[,1])))
coefs_male.p3<- coefs_male.p3[, col_index_p]


# --------------------
# --------------------
# Generating Predictions, male, period

#v.periods.ext.ic <- data.frame(Year=1998,2002,2008,2011,2016)
df.haz.ext.ic.p.male <- expand.grid(Age = 1:65,Year = c(1998,2002,2008,2011,2016))
all_boot_male_p_good <- temp.m.p3.h[col_index_p]

haz_male_period_poly3_pred<- list()
for (i in seq_along(all_boot_male_p_good)){
  haz_male_period_poly3_pred[[i]] <- try(as.data.frame(summary( all_boot_male_p_good[[i]], 
                                                          newdata = df.haz.ext.ic.p.male,
                                                          type = "hazard",
                                                          tidy = TRUE)))
  
}

# checking
view_pred<-haz_male_period_poly3_pred[[1]]
unique(view_pred$`poly(Year, 3)`)

save(haz_male_period_poly3_pred, file = "haz_male_period_poly3.h_pred.RData")
load(file = "haz_male_period_poly3.h_pred.RData")
#haz_male_period_pred[[1]]

#plot(haz_male_period_poly3_pred[[1]]$time,haz_male_period_poly3_pred[[1]]$est)

# generating bootstrap estimate, percentiles by year


for ( i in seq_along(haz_male_period_poly3_pred)){
  if (typeof(haz_male_period_poly3_pred[[i]]) == "character"){
    haz_male_period_poly3_pred[[i]] <- haz_male_period_poly3_pred[[1]][1,]
    haz_male_period_poly3_pred[[i]]$it <- 0
  } else{
    haz_male_period_poly3_pred[[i]]$it <- i
    
  }
}

head(haz_male_period_poly3_pred)
nrow(haz_male_period_poly3_pred[[1]])
#poly_index <- c(unique(haz_male_period_poly3_pred[[1]]$`poly(Year, 3)`[,1]))

poly3_pred_est<- list()
for ( i in seq_along(haz_male_period_poly3_pred)){
  temp_est<- c(haz_male_period_poly3_pred[[i]]$est)
  temp_it<-  c(haz_male_period_poly3_pred[[i]]$it)
  temp_poly_Year1 <- (haz_male_period_poly3_pred[[i]]$`poly(Year, 3)`)
  # temp_poly_Year1 <- ifelse(temp_poly_Year1 == poly_index [1], 1998, 
  #                           ifelse(temp_poly_Year1==poly_index [2], 2002,
  #                                  ifelse(temp_poly_Year1==poly_index [3],2008,
  #                                         ifelse(temp_poly_Year1==poly_index [4],2011,2016))))
  #temp_Year <-  c(rep(1998,64),rep(2002,64),rep(2008,64),rep(2011,64),rep(2016,64))
  
  temp_time <- c(haz_male_period_poly3_pred[[i]]$time)
  poly3_pred_est[[i]]<- data.frame(est =temp_est, time =temp_time, 
                                   Poly_period = temp_poly_Year1, 
                                   it = temp_it) %>% filter(it !=0)
  poly3_pred_est[[i]]$Year <- ifelse(poly3_pred_est[[i]][,c(3)]==unique(poly3_pred_est[[i]][,c(3:5)])[1,1],1998, 
                              ifelse(poly3_pred_est[[i]][,c(3)]==unique(poly3_pred_est[[i]][,c(3:5)])[2,1],2002,
                              ifelse(poly3_pred_est[[i]][,c(3)]==unique(poly3_pred_est[[i]][,c(3:5)])[3,1],2008,
                              ifelse(poly3_pred_est[[i]][,c(3)]==unique(poly3_pred_est[[i]][,c(3:5)])[4,1],2011,2016))))
}
nrow(poly3_pred_est[[1]])
poly3_pred_est
colnames(poly3_pred_est[[1]])
unique(poly3_pred_est[[2]][,c(3:5)])[1,1]
unique(poly3_pred_est[[2]]$Year)

#bind_rows(poly3_pred_est) 

haz_male_period_poly3_pred_df<- distinct(bind_rows(poly3_pred_est)) 

nrow(haz_male_period_poly3_pred_df)
nrow(distinct(haz_male_period_poly3_pred_df))
#unique(haz_male_period_poly3_pred_df$Poly_period)
colnames(haz_male_period_poly3_pred_df)

unique(haz_male_period_poly3_pred_df$Year)

load("plot_df_male_p.RData")
haz_male_period_poly3_pred_df$h_t <- NA

for ( i in 1:nrow(haz_male_period_poly3_pred_df)){
  haz_male_period_poly3_pred_df$h_t[i] <- filter(plot_df_male_p, Period == haz_male_period_poly3_pred_df$Year[i] & Init.Age == haz_male_period_poly3_pred_df$time[i])$h_t
  print(i)
}
length(haz_male_period_poly3_pred_df$h_t)

haz_male_period_poly3_pred_df$delta_h_t <- haz_male_period_poly3_pred_df$est - haz_male_period_poly3_pred_df$h_t 

haz_male_period_poly3_pred_bsummary <- haz_male_period_poly3_pred_df %>% group_by(Year,time) %>% 
  summarise(
    b_estimate = mean(est),
    avg_delta  = mean(delta_h_t),
    h_t        = unique(h_t),
    b_lcl      = abs(quantile(sort(delta_h_t), probs = 0.95,na.rm = T)),
    b_ucl      = quantile(sort(delta_h_t), probs = 0.05,na.rm = T)) %>% 
  mutate(
  emp_b_lcl      = h_t-  b_lcl,
  emp_b_ucl      = h_t - b_ucl) 


View(bind_cols(haz_male_period_poly3_pred_bsummary[,1:2], round(haz_male_period_poly3_pred_bsummary[,3:9]*1000, 3)))

save(haz_male_period_poly3_pred_bsummary, file = "haz_male_period_poly3.h_pred_bsummary.RData")
haz_male_period_poly3_pred_bsummary

# -----------------------------------------------------------------------------------------------------------
# # Generating Predictions, male, cohort
names_list3.coeffs <- paste0("fit_coefs_",as.character(1:300)) 

coefs_male.c3 <- list()
for ( i in seq_along(temp.m.c3.h)){
  if (typeof(temp.m.c3.h[[i]]) == "character"){
    coefs_male.c3[[i]] <- rep(NA, 12)} else{
      coefs_male.c3[[i]] <- temp.m.c3.h[[i]]$coefficients
    }}

#names_list3.coeffs <- paste0("fit_coefs_",as.character(1:100)) 
names(coefs_male.c3) <- names_list3.coeffs
coefs_male.c3 <- as.matrix(bind_rows((coefs_male.c3)))
col_index_c <- as.numeric(which(!is.na(t(coefs_male.c3)[,1])))


v.cohorts.ext.ic <- c(1930,1940,1950,1960,1970,1980,1990)
df.haz.ext.ic.c.male <- expand.grid(Age = 1:65,Cohort = (v.cohorts.ext.ic))

all_boot_male_c_good <- temp.m.c3.h[col_index_c]


haz_male_cohort_pred<- list()
for (i in seq_along(all_boot_male_c_good)){
  haz_male_cohort_pred[[i]] <- try(as.data.frame(summary( all_boot_male_c_good[[i]], 
                                                          newdata = df.haz.ext.ic.c.male,
                                                          type = "hazard",
                                                          tidy = TRUE)))
  print(i)
  
}
#haz_male_cohort_pred[[2]]

save(haz_male_cohort_pred, file = "haz_male_cohort_poly3.h_pred.RData")
#load(file = "haz_male_cohort_poly3.h_pred.RData")

#load(file = "/Users/mauriciolopezm/Dropbox (Brown)/Troubleshooting-Parametric Models and Splines/Code/haz_male_cohort_pred.RData")
haz_male_cohort_pred[[2]]
View(haz_male_cohort_pred)

# generating bootstrap estimate, percentiles by cohort


for ( i in seq_along(haz_male_cohort_pred)){
  if (typeof(haz_male_cohort_pred[[i]]) == "character"){
    haz_male_cohort_pred[[i]] <- haz_male_cohort_pred[[2]][1,]
    haz_male_cohort_pred[[i]]$it <- 0
  } else{
    haz_male_cohort_pred[[i]]$it <- i
  }
}

#--

poly3_pred_est_c<- list()
for ( i in seq_along( haz_male_cohort_pred)){
  temp_est<- c( haz_male_cohort_pred[[i]]$est)
  temp_it<-  c( haz_male_cohort_pred[[i]]$it)
  temp_poly_Cohort1 <- ( haz_male_cohort_pred[[i]]$`poly(Cohort, 3)`)
  # temp_poly_Year1 <- ifelse(temp_poly_Year1 == poly_index [1], 1998, 
  #                           ifelse(temp_poly_Year1==poly_index [2], 2002,
  #                                  ifelse(temp_poly_Year1==poly_index [3],2008,
  #                                         ifelse(temp_poly_Year1==poly_index [4],2011,2016))))
  #temp_Year <-  c(rep(1998,64),rep(2002,64),rep(2008,64),rep(2011,64),rep(2016,64))
  
  temp_time <- c( haz_male_cohort_pred[[i]]$time)
  poly3_pred_est_c[[i]]<- data.frame(est =temp_est, time =temp_time, 
                                   Poly_Cohort = temp_poly_Cohort1, 
                                   it = temp_it) %>% filter(it !=0)
  
  poly3_pred_est_c[[i]]$Cohort <- ifelse(poly3_pred_est_c[[i]][,c(3)]==unique(poly3_pred_est_c[[i]][,c(3:5)])[1,1],1930, 
                                     ifelse(poly3_pred_est_c[[i]][,c(3)]==unique(poly3_pred_est_c[[i]][,c(3:5)])[2,1],1940,
                                            ifelse(poly3_pred_est_c[[i]][,c(3)]==unique(poly3_pred_est_c[[i]][,c(3:5)])[3,1],1950,
                                                   ifelse(poly3_pred_est_c[[i]][,c(3)]==unique(poly3_pred_est_c[[i]][,c(3:5)])[4,1],1960,
                                                          ifelse(poly3_pred_est_c[[i]][,c(3)]==unique(poly3_pred_est_c[[i]][,c(3:5)])[5,1],1970,
                                                                 ifelse(poly3_pred_est_c[[i]][,c(3)]==unique(poly3_pred_est_c[[i]][,c(3:5)])[6,1],1980,1990))))))
print(i)
  }
nrow(poly3_pred_est_c[[2]])
poly3_pred_est_c
colnames(poly3_pred_est_c[[2]])
unique(poly3_pred_est_c[[2]][,c(3:5)])[1,1]
unique(poly3_pred_est_c[[2]]$Cohort)
#---
haz_male_cohort_poly3_pred_df<- distinct(bind_rows(poly3_pred_est_c)) 

nrow(haz_male_cohort_poly3_pred_df)
nrow(distinct(haz_male_cohort_poly3_pred_df))
#unique(haz_male_period_poly3_pred_df$Poly_period)
colnames(haz_male_cohort_poly3_pred_df)

unique(haz_male_cohort_poly3_pred_df$Cohort)
length(unique(haz_male_cohort_poly3_pred_df$it))
head(haz_male_cohort_poly3_pred_df)
View(haz_male_cohort_poly3_pred_df)

haz_male_cohort_poly3_pred_df %>% group_by(it)%>%summarise(n = n())
view(filter(haz_male_cohort_poly3_pred_df, it ==23))
#---
#filter(plot_df_male_c, Cohort == haz_male_cohort_poly3_pred_bsummary$Cohort & Init.Age == haz_male_cohort_poly3_pred_bsummary$time)$h_t
typeof(haz_male_cohort_poly3_pred_df$time)
colnames(haz_male_cohort_poly3_pred_df)
load("plot_df_male_c.RData")

haz_male_cohort_poly3_pred_df$h_t <- NA

for ( i in 1:nrow(haz_male_cohort_poly3_pred_df)){
  haz_male_cohort_poly3_pred_df$h_t[i] <- filter(plot_df_male_c, Cohort == haz_male_cohort_poly3_pred_df$Cohort[i] & Init.Age == haz_male_cohort_poly3_pred_df$time[i])$h_t
}
length(haz_male_cohort_poly3_pred_df$h_t)

haz_male_cohort_poly3_pred_df$delta_h_t <- haz_male_cohort_poly3_pred_df$est - haz_male_cohort_poly3_pred_df$h_t 

haz_male_cohort_poly3_pred_bsummary <- haz_male_cohort_poly3_pred_df %>%group_by(Cohort,time)%>%summarise(b_estimate = mean(est),
                                                                                                          avg_delta  = mean(delta_h_t),
                                                                                                          h_t        = unique(h_t),
                                                                                                          b_lcl      = abs(quantile(sort(delta_h_t), probs = 0.95,na.rm = T)),
                                                                                                          
                                                                                                          b_ucl      = quantile(sort(delta_h_t), probs = 0.05,na.rm = T)
                                                                                                            )%>% mutate(
                                                                                                         
                                                                                                          #b_sd       = sd(est),
                                                                                                         
                                                                                                          emp_b_lcl      = h_t -  b_lcl,
                                                                                                         
                                                                                                          emp_b_ucl      = h_t -  b_ucl) 


View(bind_cols(haz_male_cohort_poly3_pred_bsummary[,1:2], round(haz_male_cohort_poly3_pred_bsummary[,3:9]*1000, 3)))

save(haz_male_cohort_poly3_pred_bsummary, file = "haz_male_cohort_poly3.h_pred_bsummary.RData")




# ---------------------
# ---------------------
load(file = "haz_male_cohort_poly3.h_pred.RData")
load(file = "haz_male_period_poly3.h_pred.RData")

# Peak lcl, ucl

# summarize 

all_haz_m_p <- haz_male_period_poly3_pred_bsummary%>% group_by(Year)%>%summarize(`Peak Rate of Onset` = max(h_t)*1000)
all_haz_m_p

all_hazlcl_m_p <- haz_male_period_poly3_pred_bsummary%>% group_by(Year)%>%summarize(`Peak Rate of Onset` = max(emp_b_lcl)*1000)
all_hazlcl_m_p

all_hazucl_m_p <- haz_male_period_poly3_pred_bsummary%>% group_by(Year)%>%summarize(`Peak Rate of Onset` = max(emp_b_ucl)*1000)
all_hazucl_m_p

round(cbind(all_haz_m_p$Year, all_haz_m_p$`Peak Rate of Onset`,all_hazlcl_m_p$`Peak Rate of Onset`,all_hazucl_m_p$`Peak Rate of Onset` ),3)

all_haz_m_c <-haz_male_cohort_poly3_pred_bsummary%>% group_by(Cohort)%>%summarize(`Peak Rate of Onset` = max(h_t)*1000)
all_haz_m_c

all_hazlcl_m_c <-haz_male_cohort_poly3_pred_bsummary%>% group_by(Cohort)%>%summarize(`Peak Rate of Onset` = max(emp_b_lcl)*1000)
all_hazlcl_m_c

all_hazucl_m_c <-haz_male_cohort_poly3_pred_bsummary%>% group_by(Cohort)%>%summarize(`Peak Rate of Onset` = max(emp_b_ucl)*1000)
all_hazucl_m_c

round(cbind(all_haz_m_c$Cohort,all_haz_m_c$`Peak Rate of Onset`,all_hazlcl_m_c$`Peak Rate of Onset`,all_hazucl_m_c$`Peak Rate of Onset` ),3)

# ---------------------
# ---------------------

# Load models
load(file = "/Users/mauriciolopezm/Dropbox (Brown)/Troubleshooting-Parametric Models and Splines/Best_fit_Splines/best_fit_splines.RData")
best_fit_splines

mod_male_p   <- best_fit_splines$male_period
mod_female_p <- best_fit_splines$female_period
mod_male_c   <- best_fit_splines$male_cohort_dec
mod_female_c <- best_fit_splines$female_cohort_dec


exp(mod_male_p$aux$knots)
exp(mod_female_p$aux$knots)
exp(mod_male_c$aux$knots   )
exp(mod_female_c$aux$knots )

