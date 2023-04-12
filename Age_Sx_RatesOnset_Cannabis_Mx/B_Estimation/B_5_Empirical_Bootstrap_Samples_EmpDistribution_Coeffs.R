#-------------------------------------------------------------------------------- #
# Uncertainty 1| generating Bootstrap Samples   and empirical distribution of coefficients        #
# In this program we generate bootstrap samples to generate empirical bootstrap estimates of the 95% confidence intervals around the coefficients of the best fit models
# This is necessary because it was not possible to obtain the confidence intervals for all models using the flexsurv routines.
# Note: For users who wish to reproduce the results from the paper, take note that here we only present an example for one of the 4 models that we present in the paper
# and that in this code we only run 3 replication samples, hence the results obtained from running this code do not represent the final results shown in the paper. 
# Author: Mauricio, Lopez Mendez
# Date: 01/10/2021
#-------------------------------------------------------------------------------- #
# 0. Libraries ----
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
#-------------------------------------------------------------------------------- #
# 1. LOAD INPUTS  ----
# Interval and Right-Censored Data (1998-2016)
DMarihuana_1998_2016 <- read_excel("~/Dropbox (Brown)/Troubleshooting-Parametric Models and Splines/DS_SURV_MARIHUANA_1998_2016.xlsx")
#-------------------------------------------------------------------------------- #
# 2. Data prep for Bootstrap Sampling -----
DMarihuana_1998_2016$Sex  <- factor(DMarihuana_1998_2016$Sex, labels = c("Male", "Female"))
# Subsets by Strata
# Strata 1 | Female
original_sample_female <- DMarihuana_1998_2016%>%
  filter(Sex == "Female")%>%
  dplyr::select(Year, Cohort, AgeL_Marihuana, AgeR_Marihuana, Event)%>%
  mutate(Year = as.numeric(Year), 
         Cohort = as.factor(Cohort),
         AgeL_Marihuana = as.character(AgeL_Marihuana), 
         AgeR_Marihuana = as.character(AgeR_Marihuana ),
         Event =  as.character(Event))
#Decennial Birth-cohorts 
v_names_cohort_groups <- c('1930', #1933-1939
                           '1940', 
                           '1950', 
                           '1960', 
                           '1970',
                           '1980', 
                           '1990') #1990-2004

original_sample_female <- DMarihuana_1998_2016%>%
  mutate(Cohort.dec = cut(Cohort, c(1932,1939, 1949, 1959, 1969, 1979, 1989, 2004)))
levels(original_sample_female$Cohort.dec) = v_names_cohort_groups 
original_sample_female <- original_sample_female%>%
  mutate(Cohort.dec = as.numeric(Cohort.dec))
# Strata 2 | Male
original_sample_male <- DMarihuana_1998_2016%>%
  filter(Sex == "Male")%>%
  dplyr::select(Year, Cohort, AgeL_Marihuana, AgeR_Marihuana, Event)%>%
  mutate(Year = as.factor(Year), 
         Cohort = as.factor(Cohort),
         AgeL_Marihuana = as.character(AgeL_Marihuana), 
         AgeR_Marihuana = as.character(AgeR_Marihuana ),
         Event =  as.character(Event))
#Decennial Birth-cohorts 
original_sample_male <- DMarihuana_1998_2016%>%
  mutate(Cohort.dec = cut(Cohort, c(1932,1939, 1949, 1959, 1969, 1979, 1989, 2004)))
levels(original_sample_male$Cohort.dec) = v_names_cohort_groups 
original_sample_male <- original_sample_male%>%
  mutate(Cohort.dec = as.numeric(Cohort.dec))
#-------------------------------------------------------------------------------- #
# 3.Bootstrap Samples -----
#-------------------------------------------------------------------------------- #
# Function to generate bootstrap samples
# function  samples "m" times from "original sample"  with replacement
# returns a survival object  for each bootstrap sample that can be used for fitting survival models
boot_sample_surv <- function(m,original_sample){
  # create lists to store samples and surv objects
  names_list                    <- paste0("sample_",as.character(1:m)) 
  l_boostrap_samples            <- list()
  names_list_surv               <- paste0("surv_object_",as.character(1:m)) 
  l_surv_objects                <- list()
  # m samples with replacement
  for (i in 1:m){
    l_boostrap_samples[[i]]     <- as.data.frame(original_sample[sample(nrow(original_sample), nrow(original_sample), replace=T),])
    l_surv_objects[[i]]         <- Surv(time  = as.numeric(l_boostrap_samples[[i]]$AgeL_Marihuana),
                                        time2 = as.numeric(l_boostrap_samples[[i]]$AgeR_Marihuana),
                                        event = as.numeric(l_boostrap_samples[[i]]$Event),
                                        type  = "interval")
  }
  # Assign names to list objects
  names(l_boostrap_samples)     <- names_list
  names(l_surv_objects)         <- names_list_surv
  
  l_boostrap_objects            <- list(l_boostrap_samples=l_boostrap_samples, l_surv_objects=l_surv_objects)
  return(l_boostrap_objects)
} 
#-------------------------------------------------------------------------------- #

# Bootstrap Samples by Strata
m <- 3 # In the paper we use 300 replication samples, 
#beware that it is computationally intensive to fit data to the flexsurv models, 
#it may take several hours to obtain estimates for more than 20 samples. 
# Strata 1 | Female
ptime <- system.time(
bootstrap_sample_female <- boot_sample_surv(m,original_sample_female)
)
ptime
# Strata 2 | Male
ptime <- system.time(
bootstrap_sample_male   <- boot_sample_surv(m,original_sample_male) 
)# 
ptime
#-------------------------------------------------------------------------------- #
# 4. Empirical Distribution of Coefficients ----

## 4.1 Running Models ----
names_list_fit     <- paste0("fit_object_",as.character(1:m)) 
l_fitted_objects   <- list()
ptime <- system.time(for (i in 1:(m)){
  # This is an example, in the paper we do this for each of the best fit models
  # Running the best fit model for males, odds scale, 6 internal knots, cubic trend for period
  l_fitted_objects[[i]] <-try(flexsurvspline(bootstrap_sample_male$l_surv_objects[[i]] ~ poly(as.numeric(Year),3, raw = F),
                                    k = 6, scale = "odds",
                                    data = bootstrap_sample_male$l_boostrap_samples[[i]])
  )
})
ptime # beware of the time
#View(l_fitted_objects)
#-------------------------------------------------------------------------------- #
## 4.2 Bootstrap Estimates ----
# Function to obtain empirical distribution of coefficients for each model
# takes as input the list of fitted objects from the best fit models
# returns a data frame, 1 column per coefficient, rows correspond to the coeff estimates for each bootstrap run
# also returns a data frame with summary statistics, mean. median, 5% quantile, 95% quantile
#-------------------------------------------------------------------------------- #
empdist_coeffs<- function(l_fitted_objects){
# list to store coefficients
l_coefs             <- list()
names_list3.1       <- paste0("fit_coefs_",as.character(1:m)) 
# looping over fitted objects
for ( i in seq_along(l_fitted_objects)){
  if (typeof(l_fitted_objects[[i]]) == "character"){
    l_coefs[[i]]    <- rep(NA, 11)} else{
      l_coefs[[i]]  <- l_fitted_objects[[i]]$coefficients
    }}
#naming 
names(l_coefs)      <- names_list3.1
#consolidating    
l_coefs             <- as.matrix(bind_rows((l_coefs)))
# extracting only the runs that converged
col_index           <- as.numeric(which(!is.na(t(l_coefs)[,1])))
l_coefs             <- l_coefs[, col_index]
dim(l_coefs)
l_coefs<- as.data.frame(l_coefs)
# statistics from the empirical distribution
v_means_coeffs      <- apply(l_coefs,MARGIN=2, mean)
v_median_coeffs     <- apply(l_coefs,MARGIN=2, median)
v_5quantile_coeffs  <- apply(l_coefs,MARGIN=2, FUN = function(v){return(quantile(v,0.05))})
v_95quantile_coeffs <- apply(l_coefs,MARGIN=2, FUN = function(v){return(quantile(v,0.95))})
summary             <- cbind(rbind(v_means_coeffs,v_median_coeffs,v_5quantile_coeffs,v_95quantile_coeffs))
l_summary_coeffs    <- list(l_coefs=l_coefs, summary=summary[,9:11])
return(l_summary_coeffs)
}
#-------------------------------------------------------------------------------- #
## 4.3 Bootstrap Confidence Intervals ----
# For this step we need the point estimates of the coefficients using the original data
# coeffs from original sample for model as in 4. (Please note these are from 3 replication samples, not the 300 used for the paper)
load(file="D_Output/best_fit_prop_models_final.RData")
v_point_est_coeffs <- best_fit_prop_models_final$mod8_m_p_1$coefficients

#-------------------------------------------------------------------------------- #
# Function to Compute Bootstrap CIs
bootstrap_ci  <- function(v_point_est, l_coefs, cp_lb = 0.25, cp_ub = 0.975){
  # bootstrap replication coefficients
  m_2 <-as.matrix((l_coefs))
  #extending point estimates to a matrix to perform differences
  m_point_est_coeffs <- as.data.frame(t(v_point_est)) 
  m_point_est_coeffs <- as.matrix(m_point_est_coeffs %>% slice(rep(1:nrow(m_2), each =nrow(m_2) )))
  # Computing differences
  m_diffs<- m_2 - m_point_est_coeffs
  # computing percentiles of the distribution of the differences
  v_5quantile_diffs  <- apply(m_diffs,MARGIN=2, FUN = function(v){return(quantile(v,cp_lb))})
  v_95quantile_diffs <- apply(m_diffs,MARGIN=2, FUN = function(v){return(quantile(v,cp_ub))})
  # CIs
  v_CI_UB            <- (v_point_est_coeffs - v_5quantile_diffs)
  v_CI_LB            <- (v_point_est_coeffs - v_95quantile_diffs)
  bootstrap_95pct_ci <- rbind(v_point_est_coeffs,v_CI_LB,v_CI_UB)[,9:11]
  return(bootstrap_95pct_ci)
}
#-------------------------------------------------------------------------------- #

# 6. Example with 50 replication samples----
m <-50
## 6.1 Bootstrap samples ----
bootstrap_sample_male   <- boot_sample_surv(m,original_sample_male) 
## 6.2 Running model over m replication samples ----
names_list_fit     <- paste0("fit_object_",as.character(1:m)) 
l_fitted_objects   <- list()
for (i in 1:(m)){
  # best fit model for males, odds scale, 6 internal knots, cubic trend for period
  l_fitted_objects[[i]] <-try(flexsurvspline(bootstrap_sample_male$l_surv_objects[[i]] ~ poly(as.numeric(Year),3, raw = F),
                                             k = 6, scale = "odds",
                                             data = bootstrap_sample_male$l_boostrap_samples[[i]])
  )
}
## 6.3 Extracting replication coefficients ----
l_summary_coeffs <- empdist_coeffs(l_fitted_objects)
l_summary_coeffs$summary # percentiles of coefficients are not confidence intervals 
## 6.4 Bootstrap 95% confidence intervals -----
bootstrap_ci(v_point_est_coeffs,l_summary_coeffs$l_coefs)

