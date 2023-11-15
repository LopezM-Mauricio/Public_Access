# Microsimulation Functions
# project: Model-based Analysis of ADPs that use hs-cTn for detection of NSTE-MI in the ED
# Author: MLM
# Notes: pending issues include perform code performance profiling, finish unit-testing.
# ----------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------- #
gen_MI_specific_trajectories_df <- function(
    v_cparams,
    w = 1, 
    N_sim = 5000,
    v_time = seq(0,60,0.5),
    v_mean, 
    m_sigma
){
  # sample for specific subpopulation
  n            <- round(w*N_sim, 0)
  # unlist mean vec and cov matrix
  v_mean       <- unlist(v_mean)
  m_sigma      <- m_sigma
  # unpack calibration param initial values
  c            <- as.numeric(v_cparams[1])
  #l           <- as.numeric(c(v_cparams[2],1,v_cparams[3],1))                  # vector <l1,1,l2,1>, scaling constants variances
  l            <- as.numeric(c(v_cparams[2:5]))                                 # vector <l1,1,l2,1>, scaling constants variances
  k            <- as.numeric(c(v_cparams[5],v_cparams[7]))                         # vector <k1,k2>, scaling constant for means
  # scaling mean vector
  v_mean[1]    <- v_mean[1]*k[1]                                                #c_s1
  v_mean[3]    <- v_mean[3]*k[2]                                                #c_u1
  
  # new diagonal for cov matrix
  new_diag  <- diag(m_sigma) * l
  # Sample sets of known parameters for each observation
  temp     <- as.data.frame(
    rtmvnorm(n, 
             mean  = v_mean,
             sigma = m_sigma -(diag(m_sigma)*diag(length(new_diag))) + (new_diag* diag( length(new_diag))), # element-wise multiplication 
             lower= c(0,0,0,0),
             #upper= c(25,25),
             algorithm="rejection"                               
    ))
  colnames(temp)      <- c("s1","s2","u1","u2")
  # Expand data frame with parameters for each time point in the trajectories
  params_df                                 <- as_tibble(temp)
  params_df$ID                              <- 1:nrow(params_df)
  params_df_long                            <- params_df %>% slice(rep(1:n, each = length(v_time)))
  params_df_long$time                       <- rep(v_time,n)
  
  # Testing ---#
  #return(params_df_long)
  # -----      #
  
  # Generate trajectories
  df_out <- params_df_long %>% mutate(
    cTn_concentration = c * ( (time^s1)/( (u1^s1) + (time^s1)) ) * ( (u2^s2)/( (u2^s2) + (time^s2)) ))

  data <- df_out %>% dplyr::select(ID,time, cTn_concentration)
  names(data) <- c("ID","time", "y")
  data$ID <- as.factor(data$ID)
  return(data)
}

# ----------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------- #
gen_mixture_trajectories_df <-function(
    N_sim = N_sim  ,    # total sample size
    v_time = 0:60, 
    l_optim_params ,    # list input parameters for each subpopulation
    v_mixture_w         # optimal mixture weights
){
  # samples sizes based on mixture weights 
  n_typeI_MI   <- as.numeric(round(v_mixture_w[1]*N_sim) )
  n_typeII_MI  <- as.numeric(round(v_mixture_w[2]*N_sim) )
  n_acute_MI   <- as.numeric(round(v_mixture_w[3]*N_sim))
  n_chronic_MI <- as.numeric(round(v_mixture_w[4]*N_sim))
  n_No_MI      <- as.numeric(round(v_mixture_w[5]*N_sim))
  
  #TypeI
  k  <- optim_params_l[[1]]$k
  v_mean_typeI_MI <- tibble(
    s1  = as.numeric(optim_params_l[[1]]$s1 * k[1]),
    s2  = as.numeric(optim_params_l[[1]]$s2 ),
    u1  = as.numeric(optim_params_l[[1]]$u1 * k[2]),
    u2  = as.numeric(optim_params_l[[1]]$u2),
    c   = as.numeric(optim_params_l[[1]]$c  ))
  #View(optim_params_l)
  m_sigma_typeI_MI              <- cTn_I_sample_params$Sigma_cTn_I               # original covariance matrix
  v_k_typeI_MI                  <- optim_params_l[[1]]$l                         # vector of calibrated constant to scale individual variances
  v_new_diag_typeI_MI           <- diag(m_sigma_typeI_MI) * v_k_typeI_MI         # new diagonal of cov matrix
  m_new_sigma_typeI_MI          <- m_sigma_typeI_MI -(diag(m_sigma_typeI_MI)*diag(length(v_new_diag_typeI_MI))) + (v_new_diag_typeI_MI* diag( length(v_new_diag_typeI_MI))) # replacing diagonal in the cov matrix with the scaled version 
  # adding tau to cov matrix
  m_new_sigma_typeI_MI          <- rbind(m_new_sigma_typeI_MI, c(0,0,0,0))
  m_new_sigma_typeI_MI          <- cbind(m_new_sigma_typeI_MI, c(0,0,0,0,1))
  rownames(m_new_sigma_typeI_MI)[5] <- "c"
  colnames(m_new_sigma_typeI_MI)[5] <- "c"
  # simulating trajectory parameters for type I MI sub population
  temp_typeI_MI     <- as.data.frame(
    rtmvnorm(n     = n_typeI_MI, 
             mean  = as.numeric(v_mean_typeI_MI),
             sigma = m_new_sigma_typeI_MI, 
             lower = c(0,0,0,0,0),
             #upper= c(25,25),
             algorithm="rejection"                               
    ))
  colnames(temp_typeI_MI)      <- c("s1","s2","u1","u2","c")
  # Expand data frame with parameters for each time point in the trajectories
  params_df_typeI_MI                            <- as_tibble(temp_typeI_MI)
  # time of presentation
  params_df_typeI_MI$time_presentation          <- 6  # temporary, we need the simulated waiting times 
  # expanding 
  params_df_typeI_MI$ID                         <- 1:nrow(params_df_typeI_MI)
  params_df_typeI_MI_long                       <- params_df_typeI_MI %>% slice(rep(1:n_typeI_MI, each = length(v_time)))
  params_df_typeI_MI_long$time                  <- rep(v_time,n_typeI_MI)
  # Generate trajectories
  df_out_typeI_MI <- params_df_typeI_MI_long %>% mutate(
    cTn_concentration = c * ( (time^s1)/( (u1^s1) + (time^s1)) ) * ( (u2^s2)/( (u2^s2) + (time^s2)) ))
  df_out_typeI_MI$MI   <- "Type I"
  
  # TYPE II
  k  <- optim_params_l[[2]]$k
  v_mean_typeII_MI <- tibble(
    s1  = optim_params_l[[2]]$s1 * k[1],
    s2  = optim_params_l[[2]]$s2,
    u1 = optim_params_l[[2]]$u1 * k[2],
    u2  = as.numeric(optim_params_l[[1]]$u2),
    c = optim_params_l[[2]]$c)
  
  m_sigma_typeII_MI              <- cTn_I_sample_params$Sigma_cTn_I          # original covariance matrix
  v_k_typeII_MI                  <- optim_params_l[[2]]$l                    # vector of calibrated constant to scale individual variances
  v_new_diag_typeII_MI           <- diag(m_sigma_typeII_MI) * v_k_typeII_MI  # new diagonal of cov matrix
  m_new_sigma_typeII_MI          <- m_sigma_typeII_MI -(diag(m_sigma_typeII_MI)*diag(length(v_new_diag_typeII_MI))) + (v_new_diag_typeII_MI* diag( length(v_new_diag_typeII_MI))) # replacing diagonal in the cov matrix with the scaled version 
  # adding tau to cov matrix
  m_new_sigma_typeII_MI          <- rbind(m_new_sigma_typeII_MI, c(0,0,0,0))
  m_new_sigma_typeII_MI          <- cbind(m_new_sigma_typeII_MI, c(0,0,0,0,1))
  rownames(m_new_sigma_typeII_MI)[5] <- "c"
  colnames(m_new_sigma_typeII_MI)[5] <- "c"
  # simulating trajectory parameters for type II MI sub population, 
  temp_typeII_MI     <- as.data.frame(
    rtmvnorm(n     = n_typeII_MI, 
             mean  = as.numeric(v_mean_typeII_MI),
             sigma = m_new_sigma_typeII_MI, 
             lower = c(0,0,0,0,0),
             #upper= c(25,25),
             algorithm="rejection"                               
    ))
  colnames(temp_typeII_MI)      <- c("s1","s2","u1","u2","c")
  #View(temp_typeII_MI)  
  # Expand data frame with parameters for each time point in the trajectories
  params_df_typeII_MI                                 <- as_tibble(temp_typeII_MI)
  # time of presentation,
  params_df_typeII_MI$time_presentation          <- 6# temporary
  # expanding 
  params_df_typeII_MI$ID                         <- 1:nrow(params_df_typeII_MI)
  params_df_typeII_MI_long                       <- params_df_typeII_MI %>% slice(rep(1:n_typeII_MI, each = length(v_time)))
  params_df_typeII_MI_long$time                  <- rep(v_time,n_typeII_MI)
    # Generate trajectories
  df_out_typeII_MI <- params_df_typeII_MI_long %>% mutate(
    cTn_concentration = c * ( (time^s1)/( (u1^s1) + (time^s1)) ) * ( (u2^s2)/( (u2^s2) + (time^s2)) ))
  df_out_typeII_MI$MI   <- "Type II"
  
  # ACUTE
  k  <- optim_params_l[[3]]$k
  v_mean_acute_MI <- tibble(
    s1  = optim_params_l[[3]]$s1 * k[1],
    s2  = optim_params_l[[3]]$s2,
    u1  = optim_params_l[[3]]$u1 * k[2],
    u2  = as.numeric(optim_params_l[[1]]$u2),
    c   = optim_params_l[[3]]$c)
  
  m_sigma_acute_MI              <- cTn_I_sample_params$Sigma_cTn_I        # original covariance matrix
  v_k_acute_MI                  <- optim_params_l[[3]]$l                  # vector of calibrated constant to scale individual variances
  v_new_diag_acute_MI           <- diag(m_sigma_acute_MI) * v_k_acute_MI  # new diagonal of cov matrix
  m_new_sigma_acute_MI          <- m_sigma_acute_MI -(diag(m_sigma_acute_MI)*diag(length(v_new_diag_acute_MI))) + (v_new_diag_acute_MI* diag( length(v_new_diag_acute_MI))) # replacing diagonal in the cov matrix with the scaled version 
  # adding tau to cov matrix
  m_new_sigma_acute_MI          <- rbind(m_new_sigma_acute_MI, c(0,0,0,0))
  m_new_sigma_acute_MI          <- cbind(m_new_sigma_acute_MI, c(0,0,0,0,1))
  rownames(m_new_sigma_acute_MI)[5] <- "c"
  colnames(m_new_sigma_acute_MI)[5] <- "c"
  # simulating trajectory parameters for acute MI sub population
  temp_acute_MI     <- as.data.frame(
    rtmvnorm(n     = n_acute_MI, 
             mean  = as.numeric(v_mean_acute_MI),
             sigma = m_new_sigma_acute_MI, 
             lower = c(0,0,0,0,0),
             #upper= c(25,25),
             algorithm="rejection"                               
    ))
  colnames(temp_acute_MI)      <- c("s1","s2","u1","u2","c")
  # Expand data frame with parameters for each time point in the trajectories
  params_df_acute_MI                                 <- as_tibble(temp_acute_MI)
  # time of presentation,
  params_df_acute_MI$time_presentation          <- 6#temporary
  # expanding 
  params_df_acute_MI$ID                         <- 1:nrow(params_df_acute_MI)
  params_df_acute_MI_long                       <- params_df_acute_MI %>% slice(rep(1:n_acute_MI, each = length(v_time)))
  params_df_acute_MI_long$time                  <- rep(v_time,n_acute_MI)
  # Generate trajectories
  df_out_acute_MI <- params_df_acute_MI_long %>% mutate(
    cTn_concentration = c * ( (time^s1)/( (u1^s1) + (time^s1)) ) * ( (u2^s2)/( (u2^s2) + (time^s2)) ))
  df_out_acute_MI$MI   <- "Acute"
  
  # CHRONIC
  k  <- optim_params_l[[4]]$k
  v_mean_chronic_MI <- tibble(
    s1  = optim_params_l[[4]]$s1 * k[1],
    s2  = optim_params_l[[4]]$s2,
    u1  = optim_params_l[[4]]$u1 * k[2],
    u2  = as.numeric(optim_params_l[[1]]$u2),
    c   = optim_params_l[[4]]$c)
  
  m_sigma_chronic_MI              <- cTn_I_sample_params$Sigma_cTn_I            # original covariance matrix
  v_k_chronic_MI                  <- optim_params_l[[4]]$l                      # vector of calibrated constant to scale individual variances
  v_new_diag_chronic_MI           <- diag(m_sigma_chronic_MI) * v_k_chronic_MI  # new diagonal of cov matrix
  m_new_sigma_chronic_MI          <- m_sigma_chronic_MI -(diag(m_sigma_chronic_MI)*diag(length(v_new_diag_chronic_MI))) + (v_new_diag_chronic_MI* diag( length(v_new_diag_chronic_MI))) # replacing diagonal in the cov matrix with the scaled version 
  # adding tau to cov matrix
  m_new_sigma_chronic_MI          <- rbind(m_new_sigma_chronic_MI, c(0,0,0,0))
  m_new_sigma_chronic_MI          <- cbind(m_new_sigma_chronic_MI, c(0,0,0,0,1))
  rownames(m_new_sigma_chronic_MI)[5] <- "c"
  colnames(m_new_sigma_chronic_MI)[5] <- "c"
  # simulating trajectory parameters for chronic MI sub population,
  temp_chronic_MI     <- as.data.frame(
    rtmvnorm(n     = n_chronic_MI, 
             mean  = as.numeric(v_mean_chronic_MI),
             sigma = m_new_sigma_chronic_MI, 
             lower = c(0,0,0,0,0),
             #upper= c(25,25),
             algorithm="rejection"                               
    ))
  colnames(temp_chronic_MI)      <- c("s1","s2","u1","u2","c")
  # Expand data frame with parameters for each time point in the trajectories
  params_df_chronic_MI                                 <- as_tibble(temp_chronic_MI)
  # time of presentation,
  params_df_chronic_MI$time_presentation          <- 6#temporary
  # expanding 
  params_df_chronic_MI$ID                         <- 1:nrow(params_df_chronic_MI)
  params_df_chronic_MI_long                       <- params_df_chronic_MI %>% slice(rep(1:n_chronic_MI, each = length(v_time)))
  params_df_chronic_MI_long$time                  <- rep(v_time,n_chronic_MI)
  # Generate trajectories
  df_out_chronic_MI <- params_df_chronic_MI_long %>% mutate(
    cTn_concentration = c * ( (time^s1)/( (u1^s1) + (time^s1)) ) * ( (u2^s2)/( (u2^s2) + (time^s2)) ))
  df_out_chronic_MI$MI   <- "Chronic"
  
  # NO MI
  k  <- optim_params_l[[5]]$k
  v_mean_No_MI <- tibble(
    s1  = optim_params_l[[5]]$s1 * k[1],
    s2  = optim_params_l[[5]]$s2,
    u1  = optim_params_l[[5]]$u1 * k[2],
    u2  = as.numeric(optim_params_l[[1]]$u2 * 1.5),
    c   = optim_params_l[[5]]$c)
  
  m_sigma_No_MI              <- cTn_I_sample_params$Sigma_cTn_I       # original covariance matrix
  v_k_No_MI                  <- optim_params_l[[5]]$l                 # vector of calibrated constant to scale individual variances
  v_new_diag_No_MI           <- diag(m_sigma_No_MI) * v_k_No_MI       # new diagonal of cov matrix
  m_new_sigma_No_MI          <- m_sigma_No_MI -(diag(m_sigma_No_MI)*diag(length(v_new_diag_No_MI))) + (v_new_diag_No_MI* diag( length(v_new_diag_No_MI))) # replacing diagonal in the cov matrix with the scaled version 
  # adding tau to cov matrix
  m_new_sigma_No_MI          <- rbind(m_new_sigma_No_MI, c(0,0,0,0))
  m_new_sigma_No_MI          <- cbind(m_new_sigma_No_MI, c(0,0,0,0,1))
  rownames(m_new_sigma_No_MI)[5] <- "c"
  colnames(m_new_sigma_No_MI)[5] <- "c"
  # simulating trajectory parameters for No MI sub population,
  temp_No_MI     <- as.data.frame(
    rtmvnorm(n     = n_No_MI, 
             mean  = as.numeric(v_mean_No_MI),
             sigma = m_new_sigma_No_MI, 
             lower = c(0,0,0,0,0),
             #upper= c(25,25),
             algorithm="rejection"                               
    ))
  colnames(temp_No_MI)      <- c("s1","s2","u1","u2","c")
  # Expand data frame with parameters for each time point in the trajectories
  params_df_No_MI                                 <- as_tibble(temp_No_MI)
  # time of presentation, 
  params_df_No_MI$time_presentation          <- 6 #temporary
  # expanding 
  params_df_No_MI$ID                         <- 1:nrow(params_df_No_MI)
  params_df_No_MI_long                       <- params_df_No_MI %>% slice(rep(1:n_No_MI, each = length(v_time)))
  params_df_No_MI_long$time                  <- rep(v_time,n_No_MI)
  # Generate trajectories
  df_out_No_MI <- params_df_No_MI_long %>% mutate(
    cTn_concentration = c * ( (time^s1)/( (u1^s1) + (time^s1)) ) * ( (u2^s2)/( (u2^s2) + (time^s2)) ))
  df_out_No_MI$MI   <- "No MI"
  
  # all trajectories
  df_out_all_MI<- rbind(df_out_typeI_MI, df_out_typeII_MI, df_out_acute_MI, df_out_chronic_MI,df_out_No_MI)
  df_out_all_MI$ID_MI <- paste(df_out_all_MI$ID,df_out_all_MI$MI, sep="_")
  return(df_out_all_MI)
}
# ----------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------- #

# ADP performance 
# This function computes several metrics for ADP classification performance ( sensitivity, specificity, negative predictive value, positive predictive value )

performance_fun <- function(data,           # tibble with true labels from simulated trajectories and clinical characteristics, and labels predicted by the ADPs
                            observe = T,    # if true, ADP includes clinicians decision to put patient into observation
                            n               # sample size ( number of individual observations)
                            ){
  if(observe==TRUE){
    # Event indicators
    data<- data %>% mutate(
      TP_Ind   = ifelse(MI_Ind == 1 & (rule_in == 1 | observe == 1),1,0),
      TN_Ind   = ifelse(MI_Ind == 0 & rule_out ==1,1,0),
      FN_Ind   = ifelse(MI_Ind == 1 &  rule_out ==1,1,0),
      FP_Ind   = ifelse(MI_Ind == 0 & (rule_in == 1 | observe == 1),1,0),
    )} else if(observe==FALSE){
      data<- data %>% mutate(
        TP_Ind   = ifelse(MI_Ind == 1 & (rule_in == 1),1,0),
        TN_Ind   = ifelse(MI_Ind == 0 & rule_out == 1,1,0),
        FN_Ind   = ifelse(MI_Ind == 1 &  rule_out ==1,1,0),
        FP_Ind   = ifelse(MI_Ind == 0 & (rule_in == 1),1,0),
      ) 
    } 
  # Performance Metrics, e.g.: NPV,PPV,Sensitivity,Specificity 
  
  # Set CI alpha level (1-alpha/2)*100%
  alpha <-0.05
  # Calculate the critical z-score
  z <- qnorm(1-alpha/2)
  n <- N_sim_plot
  # Compute quantities of interest
  df_ADP_performance <- data %>% summarise(
    MI_Prevalence         = mean(MI_Ind, na.rm = T),
    Rule_out_prop         = mean(rule_out,na.rm = T),
    Observation_prop      = mean(observe, na.rm = T),
    Rule_in_prop          = mean(rule_in, na.rm = T),
    NPV                   = sum(TN_Ind, na.rm=T)/(sum(FN_Ind, na.rm=T)+sum(TN_Ind, na.rm=T))  ,
    PPV                   = sum(TP_Ind, na.rm=T)/(sum(TP_Ind, na.rm=T)+sum(FP_Ind, na.rm=T))  ,
    Sensitivity           = sum(TP_Ind, na.rm=T)/(sum(TP_Ind, na.rm=T)+sum(FN_Ind, na.rm=T))  ,
    Specificity           = sum(TN_Ind, na.rm=T)/(sum(FP_Ind, na.rm=T)+sum(TN_Ind, na.rm=T))  ,
  ) %>% mutate(
    MI_Prevalence_LCL     = MI_Prevalence + (-1*z)*sqrt(MI_Prevalence*(1-MI_Prevalence)/n),
    MI_Prevalence_UCL     = MI_Prevalence + (1*z)*sqrt(MI_Prevalence*(1-MI_Prevalence)/n),
    Rule_out_prop_LCL     = Rule_out_prop + (-1*z)*sqrt(Rule_out_prop*(1-Rule_out_prop)/n),
    Rule_out_prop_UCL     = Rule_out_prop + (1*z)*sqrt(Rule_out_prop*(1-Rule_out_prop)/n),
    Observation_prop_LCL  = Observation_prop + (-1*z)*sqrt(Observation_prop*(1-Observation_prop)/n),
    Observation_prop_UCL  = Observation_prop + (1*z)*sqrt(Observation_prop*(1-Observation_prop)/n),
    Rule_in_prop_LCL      = Rule_in_prop + (-1*z)*sqrt(Rule_in_prop*(1-Rule_in_prop)/n),
    Rule_in_prop_UCL      = Rule_in_prop + (1*z)*sqrt(Rule_in_prop*(1-Rule_in_prop)/n),
    NPV_LCL               = NPV + (-1*z)*sqrt(NPV*(1-NPV)/n)  ,
    NPV_UCL               = NPV + (1*z)*sqrt(NPV*(1-NPV)/n)  ,
    PPV_LCL               = PPV + (-1*z)*sqrt(PPV*(1-PPV)/n)  ,
    PPV_UCL               = PPV + (1*z)*sqrt(PPV*(1-PPV)/n)  ,
    Sensitivity_LCL       = Sensitivity + (-1*z)*sqrt(Sensitivity*(1-Sensitivity)/n)  ,
    Sensitivity_UCL       = Sensitivity + (1*z)*sqrt(Sensitivity*(1-Sensitivity)/n)  ,
    Specificity_LCL       = Specificity + (-1*z)*sqrt(Specificity*(1-Specificity)/n)  ,
    Specificity_UCL       = Specificity + (1*z)*sqrt(Specificity*(1-Specificity)/n) 
    
  )
  output <- list(df_ADP_performance=df_ADP_performance,
                 data=data)
  return(output)}

# ----------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------- #
# ESC 0/1 ADP function 
# needs its own function
# Rule out: if  (hs_cTn_T(t_0) < 5 ng/L) *** OR (if hs_cTn_T(t_0) <12 ng/L AND abs[hs_cTn_T(t_1)-hs_cTn_T(t_0)] < 3 ng/L )
# Rule in : if  (hs_cTn_T(t_0) > 52 ng/L) OR (abs[hs_cTn_T(t_1) - hs_cTn_T(t_0)] >= 5 ng/L )
# Observe : otherwise 
# 1h-change= absolute (unsigned) change of high-sensitivity cardiac troponin within the first hour. 
# *** Only if chest pain onset >3 hours before presentation to the emergency department.
sim_esc_0_1 <- function(
  trajectories,
  time2present,
  thresholds,
  n
  ){
# Unpack Inputs
  # thresholds
  esc_0_1_thresh_ro_1<- thresholds$esc_0_1_thresh_ro_1
  esc_0_1_thresh_ro_2<- thresholds$esc_0_1_thresh_ro_2
  esc_0_1_thresh_ro_3<- thresholds$esc_0_1_thresh_ro_3
  
  esc_0_1_thresh_ri_1<- thresholds$esc_0_1_thresh_ri_1
  esc_0_1_thresh_ri_2<- thresholds$esc_0_1_thresh_ri_2
  # time 2 presentation to ED
  shape <-time2present$shape
  scale <-time2present$scale
# Analytic tibble
trajectories<-trajectories %>% group_by(ID_MI) %>% 
  mutate(time_presentation2 = round(rweibull(1, shape = shape, scale=scale),1))
trajectories <-trajectories %>% dplyr::select(ID_MI,MI,time,time_presentation2,cTn_concentration)
names(trajectories)<- c("ID","MI","t","t_p","y_t")
df_adp_esc <- trajectories %>% group_by(ID) %>% 
  mutate( y_t_1 = lead(y_t,1),
  y_t_2 = lead(y_t,2),
  y_t_3 = lead(y_t,3),
) %>% mutate(
  delta_1 = abs(y_t_1 - y_t),
  delta_2 = abs(y_t_2 - y_t),
  delta_3 = abs(y_t_3 - y_t)
)
df_adp_esc <- df_adp_esc[df_adp_esc$t == df_adp_esc$t_p,]
# Indicator for Rule out threshold to use for early presenters
df_adp_esc <- df_adp_esc %>% mutate( 
  early_presentation_Ind = ifelse(t_p > 3, 1,0)
)
# Indicators of any MI
df_adp_esc <- df_adp_esc %>% mutate( 
  MI_Ind = ifelse(MI != "No MI", 1,0)
)
df_adp_esc
# ADP classification rules
# rule out - rule in - observe
df_adp_esc_0_1 <- df_adp_esc %>% mutate( 
  rule_out_Ind   = ifelse(early_presentation_Ind == 1 & y_t < esc_0_1_thresh_ro_1,1,
                          ifelse(early_presentation_Ind == 0 &(y_t < esc_0_1_thresh_ro_2 & delta_1 < esc_0_1_thresh_ro_3),1,0))  ,
  rule_in_Ind    = ifelse((y_t >= esc_0_1_thresh_ri_1 | delta_1>+ esc_0_1_thresh_ri_2)&rule_out_Ind==0,1,0)  ,
  observe_Ind    = ifelse(rule_out_Ind == 0 & rule_in_Ind ==0,1,0)
)
#View(df_adp_esc_0_1_3)
# ESC rule validation 
# df_adp_esc_0_1_3$sum_check <- df_adp_esc_0_1_3$rule_out_Ind + df_adp_esc_0_1_3$rule_in_Ind + df_adp_esc_0_1_3$observe_Ind
# table(df_adp_esc_0_1_3$sum_check)
# index <- which(df_adp_esc_0_1_3$sum_check ==2)
# df_adp_esc_0_1_3[index,]
df_results_esc_0_1 <- tibble(ID = df_adp_esc_0_1$ID,
                             MI = df_adp_esc_0_1$MI, 
                             MI_Ind =df_adp_esc_0_1$MI_Ind,
                             rule_out=df_adp_esc_0_1$rule_out_Ind, 
                             observe=df_adp_esc_0_1$observe_Ind, 
                             rule_in=df_adp_esc_0_1$rule_in_Ind, )
# ADP Performance
performance_0_1 <- performance_fun(data = df_results_esc_0_1, n = n)
return(performance_0_1)
}
# ----------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------- #
# ESC 0/2 ADP function 
sim_esc_0_2 <- function(
    trajectories,
    time2present,
    thresholds,
    n
){
  # Unpack Inputs
  # thresholds
  esc_0_2_thresh_ro_1<- thresholds$esc_0_2_thresh_ro_1
  esc_0_2_thresh_ro_2<- thresholds$esc_0_2_thresh_ro_2
  esc_0_2_thresh_ro_3<- thresholds$esc_0_2_thresh_ro_3
  
  esc_0_2_thresh_ri_1<- thresholds$esc_0_2_thresh_ri_1
  esc_0_2_thresh_ri_2<- thresholds$esc_0_2_thresh_ri_2
  # time 2 presentation to ED
  shape <-time2present$shape
  scale <-time2present$scale
  # Analytic tibble
  trajectories<-trajectories %>% group_by(ID_MI) %>% 
    mutate(time_presentation2 = round(rweibull(1, shape = shape, scale=scale),1))
  trajectories <-trajectories %>% dplyr::select(ID_MI,MI,time,time_presentation2,cTn_concentration)
  names(trajectories)<- c("ID","MI","t","t_p","y_t")
  df_adp_esc <- trajectories %>% group_by(ID) %>% 
    mutate( y_t_1 = lead(y_t,1),
            y_t_2 = lead(y_t,2),
            y_t_3 = lead(y_t,3),
    ) %>% mutate(
      delta_1 = abs(y_t_1 - y_t),
      delta_2 = abs(y_t_2 - y_t),
      delta_3 = abs(y_t_3 - y_t)
    )
  df_adp_esc <- df_adp_esc[df_adp_esc$t == df_adp_esc$t_p,]
  # Indicator for Rule out threshold to use for early presenters
  df_adp_esc <- df_adp_esc %>% mutate( 
    early_presentation_Ind = ifelse(t_p > 3, 1,0)
  )
  # Indicators of any MI
  df_adp_esc <- df_adp_esc %>% mutate( 
    MI_Ind = ifelse(MI != "No MI", 1,0)
  )
  df_adp_esc
  # ADP classification rules
  # rule out - rule in - observe
  df_adp_esc_0_2 <- df_adp_esc %>% mutate( 
    rule_out_Ind   = ifelse(early_presentation_Ind == 1 & y_t < esc_0_2_thresh_ro_1,1,
                            ifelse(early_presentation_Ind == 0 &(y_t < esc_0_2_thresh_ro_2 & delta_2 < esc_0_2_thresh_ro_3),1,0))  ,
    
    rule_in_Ind    = ifelse((y_t >= esc_0_2_thresh_ri_1 | delta_2 >= esc_0_2_thresh_ri_2)&rule_out_Ind==0,1,0)  ,
    
    observe_Ind    = ifelse(rule_out_Ind == 0 & rule_in_Ind ==0,1,0)
  )
  df_results_esc_0_2 <- tibble(ID = df_adp_esc_0_2$ID,
                               MI = df_adp_esc_0_2$MI, 
                               MI_Ind =df_adp_esc_0_2$MI_Ind,
                               rule_out=df_adp_esc_0_2$rule_out_Ind, 
                               observe=df_adp_esc_0_2$observe_Ind, 
                               rule_in=df_adp_esc_0_2$rule_in_Ind, )
  # ADP Performance
  performance_0_2 <- performance_fun(data = df_results_esc_0_2, n = n)
  return(performance_0_2)
}
# ----------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------- #
# ESC 0/1/3 ADP function 
sim_esc_0_1_3 <- function(
    trajectories,
    time2present,
    thresholds,
    n
){
  # Unpack Inputs
  # thresholds
  esc_0_1_thresh_ro_1  = thresholds$esc_0_1_thresh_ro_1    
  esc_0_1_thresh_ro_2  = thresholds$esc_0_1_thresh_ro_2    
  esc_0_1_thresh_ro_3  = thresholds$esc_0_1_thresh_ro_3    
    
  esc_0_1_thresh_ri_1  = thresholds$esc_0_1_thresh_ri_1    
  esc_0_1_thresh_ri_2  = thresholds$esc_0_1_thresh_ri_2    
  # auxiliary thresholds
  esc_0_3_thresh_ro_1  = thresholds$esc_0_3_thresh_ro_1    
  esc_0_3_thresh_ri_1  = thresholds$esc_0_3_thresh_ri_1    
  
  # time 2 presentation to ED
  shape <-time2present$shape
  scale <-time2present$scale
  # Analytic tibble
  trajectories<-trajectories %>% group_by(ID_MI) %>% 
    mutate(time_presentation2 = round(rweibull(1, shape = shape, scale=scale),1))
  trajectories <-trajectories %>% dplyr::select(ID_MI,MI,time,time_presentation2,cTn_concentration)
  names(trajectories)<- c("ID","MI","t","t_p","y_t")
  df_adp_esc <- trajectories %>% group_by(ID) %>% 
    mutate( y_t_1 = lead(y_t,1),
            y_t_2 = lead(y_t,2),
            y_t_3 = lead(y_t,3),
    ) %>% mutate(
      delta_1 = abs(y_t_1 - y_t),
      delta_2 = abs(y_t_2 - y_t),
      delta_3 = abs(y_t_3 - y_t)
    )
  df_adp_esc <- df_adp_esc[df_adp_esc$t == df_adp_esc$t_p,]
  # Indicator for Rule out threshold to use for early presenters
  df_adp_esc <- df_adp_esc %>% mutate( 
    early_presentation_Ind = ifelse(t_p > 3, 1,0)
  )
  # Indicators of any MI
  df_adp_esc <- df_adp_esc %>% mutate( 
    MI_Ind = ifelse(MI != "No MI", 1,0)
  )
  df_adp_esc
  # ADP classification rules
  # rule out - rule in - observe
  df_adp_esc_0_1_3 <- df_adp_esc %>% mutate( 
    rule_out_Ind   = ifelse(early_presentation_Ind == 1 & y_t < esc_0_1_thresh_ro_1,1,
                            ifelse(early_presentation_Ind == 0 &(y_t < esc_0_1_thresh_ro_2 & delta_1 < esc_0_1_thresh_ro_3),1,0))  ,
    
    rule_in_Ind    = ifelse((y_t >= esc_0_1_thresh_ri_1 | delta_1>+ esc_0_1_thresh_ri_2)&rule_out_Ind==0,1,0)  ,
    
    observe_Ind    = ifelse(rule_out_Ind == 0 & rule_in_Ind ==0,1,0))%>% 
    # second stage of triage, gray zone, rule out, rule in
    mutate(
      rule_out2_Ind  = ifelse(observe_Ind == 1 & delta_3 < esc_0_3_thresh_ro_1, 1,0),
      rule_in2_Ind   = ifelse(observe_Ind == 1 & delta_3 >= esc_0_3_thresh_ri_1, 1,0)
    ) %>%
    ## consolidating rule out, rule in
    mutate(
      rule_out_tot_Ind = rule_out_Ind+ rule_out2_Ind,
      rule_in_tot_Ind  = rule_in_Ind + rule_in2_Ind ,
      validation       = rule_out_tot_Ind+rule_in_tot_Ind,
      observe2_Ind     = observe_Ind - rule_out2_Ind -rule_in2_Ind
    )
  df_results_esc_0_1_3 <- tibble( ID = df_adp_esc_0_1_3$ID,
                                  MI = df_adp_esc_0_1_3$MI, 
                                  MI_Ind = df_adp_esc_0_1_3$MI_Ind,
                                  rule_out_Ind = df_adp_esc_0_1_3$rule_out_Ind,
                                  rule_out2_Ind = df_adp_esc_0_1_3$rule_out2_Ind,
                                  rule_out=df_adp_esc_0_1_3$rule_out_tot_Ind, 
                                  observe_Ind=df_adp_esc_0_1_3$observe_Ind,
                                  observe2_Ind=df_adp_esc_0_1_3$observe2_Ind,
                                  observe=df_adp_esc_0_1_3$observe2_Ind, 
                                  rule_in_Ind=df_adp_esc_0_1_3$rule_in_Ind,
                                  rule_in2_Ind=df_adp_esc_0_1_3$rule_in2_Ind,
                                  rule_in=df_adp_esc_0_1_3$rule_in_tot_Ind,validation = df_adp_esc_0_1_3$validation )
  # ADP Performance
  performance_0_1_3 <- performance_fun(data = df_results_esc_0_1_3, n = n, observe = F) 
  # note: esc 0/1/3 doesn't classify patients for "observation" at the ED, only "rule-in" or "rule-out"
  return(performance_0_1_3)
}

# ----------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------- #
# ED-LoS funs

edlos_esc_0_1   <- function(
  performance_data,
  wating_times_params,
  n
){
  # Rule out
  ## unpack params
  shape_ro_0_1             <- wating_times_params$rule_out$shape
  rate_ro_0_1              <- wating_times_params$rule_out$rate
  ## determine proportion rule in, rule out, observe
  n_ro_0_1                 <- round(performance_data$df_ADP_performance$Rule_out_prop*n)
  ## simulate waiting times
  EDLOS_rule_out_0_1_gamma <- round(rgamma(n_ro_0_1, rate = rate_ro_0_1, shape = shape_ro_0_1))
  
  # Rule in
  ## unpack params
  shape_ri_0_1              <- wating_times_params$rule_in$shape
  rate_ri_0_1               <- wating_times_params$rule_in$rate
  ## determine proportion rule in, rule out, observe
  n_ri_0_1                  <- round(performance_data$df_ADP_performance$Rule_in_prop*n)
  ## simulate waiting times
  EDLOS_rule_in_0_1_gamma   <- round(rgamma(n_ri_0_1, shape = shape_ri_0_1, rate=rate_ri_0_1))
  
  # Observation
  ## unpack params
  shape_obs_0_1             <- wating_times_params$observation$shape
  rate_obs_0_1              <- wating_times_params$observation$rate
  ## determine proportion rule in, rule out, observe
  n_obs_0_1                 <- round(performance_data$df_ADP_performance$Observation_prop*n)
  ## simulate waiting times
  EDLOS_obs_0_1_gamma       <- round(rgamma(n_obs_0_1, shape = shape_obs_0_1, rate=rate_obs_0_1))
  
  # consolidate edlos
  EDLOS_esc_0_1             <- c(EDLOS_rule_out_0_1_gamma,EDLOS_rule_in_0_1_gamma,EDLOS_obs_0_1_gamma)
  # return summary in hours
  return(summary(EDLOS_esc_0_1/60))
}
# ----------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------- #

edlos_esc_0_2   <- function(
  performance_data,
  wating_times_params,
  n
){
  # Rule out
  ## unpack params
  shape_ro_0_2             <- wating_times_params$rule_out$shape
  rate_ro_0_2              <- wating_times_params$rule_out$rate
  ## determine proportion rule in, rule out, observe
  n_ro_0_2                 <- round(performance_data$df_ADP_performance$Rule_out_prop*n)
  ## simulate waiting times
  EDLOS_rule_out_0_2_gamma <- round(rgamma(n_ro_0_2, rate = rate_ro_0_2, shape = shape_ro_0_2))
  
  # Rule in
  ## unpack params
  shape_ri_0_2              <- wating_times_params$rule_in$shape
  rate_ri_0_2               <- wating_times_params$rule_in$rate
  ## determine proportion rule in, rule out, observe
  n_ri_0_2                  <- round(performance_data$df_ADP_performance$Rule_in_prop*n)
  ## simulate waiting times
  EDLOS_rule_in_0_2_gamma   <- round(rgamma(n_ri_0_2, shape = shape_ri_0_2, rate=rate_ri_0_2))
  
  # Observation
  ## unpack params
  shape_obs_0_2             <- wating_times_params$observation$shape
  rate_obs_0_2              <- wating_times_params$observation$rate
  ## determine proportion rule in, rule out, observe
  n_obs_0_2                 <- round(performance_data$df_ADP_performance$Observation_prop*n)
  ## simulate waiting times
  EDLOS_obs_0_2_gamma       <- round(rgamma(n_obs_0_2, shape = shape_obs_0_2, rate=rate_obs_0_2))
  
  # consolidate edlos
  EDLOS_esc_0_2             <- c(EDLOS_rule_out_0_2_gamma,EDLOS_rule_in_0_2_gamma,EDLOS_obs_0_2_gamma)
  # return summary in hours
  return(summary(EDLOS_esc_0_2/60))
}
# ----------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------- #

edlos_esc_0_1_3   <- function(
  performance_data,
  wating_times_params,
  n
){
  # Rule out stage 1
  #unpack params
  shape_ro1_0_1_3             <- EDLOS_0_1_3_params$rule_out1$shape
  rate_ro1_0_1_3              <- EDLOS_0_1_3_params$rule_out1$rate
  #determine proportion rule in, rule out, observe
  n_ro1_0_1_3                 <- round(mean(esc_0_1_3_performance$data$rule_out_Ind, na.rm=T)*n)
  #simulate waiting times
  EDLOS_rule_out1_0_1_3_gamma <- round(rgamma(n_ro1_0_1_3, rate = rate_ro1_0_1_3, shape = shape_ro1_0_1_3))
  
  # Rule out stage 2
  #unpack params
  shape_ro2_0_1_3             <- EDLOS_0_1_3_params$rule_out2$shape
  rate_ro2_0_1_3              <- EDLOS_0_1_3_params$rule_out2$rate
  #determine proportion rule in, rule out, observe
  n_ro2_0_1_3                 <- round(mean(esc_0_1_3_performance$data$rule_out2_Ind, na.rm=T)*n)
  #simulate waiting times
  EDLOS_rule_out2_0_1_3_gamma <- round(rgamma(n_ro2_0_1_3, shape = shape_ro2_0_1_3, rate=rate_ro2_0_1_3))
  
  # Rule in stage 1
  #unpack params
  shape_ri1_0_1_3             <- EDLOS_0_1_3_params$rule_in1$shape
  rate_ri1_0_1_3              <- EDLOS_0_1_3_params$rule_in1$rate
  #determine proportion rule in, rule out, observe
  n_ri1_0_1_3                 <- round(mean(esc_0_1_3_performance$data$rule_in_Ind, na.rm=T)*n)
  #simulate waiting times
  EDLOS_rule_in1_0_1_3_gamma  <- round(rgamma(n_ri1_0_1_3, shape = shape_ri1_0_1_3, rate=rate_ri1_0_1_3))
  
  # Rule in stage 2
  #unpack params
  shape_ri2_0_1_3             <- EDLOS_0_1_3_params$rule_in2$shape
  rate_ri2_0_1_3              <- EDLOS_0_1_3_params$rule_in2$rate
  #determine proportion rule in, rule out, observe
  n_ri2_0_1_3 <- round(mean(esc_0_1_3_performance$data$rule_in2_Ind, na.rm=T)*n)
  #simulate waiting times
  EDLOS_rule_in2_0_1_3_gamma  <- round(rgamma(n_ri2_0_1_3, shape = shape_ri2_0_1_3, rate=rate_ri2_0_1_3))
  
  # consolidate edlos
  EDLOS_esc_0_1_3             <- c(EDLOS_rule_out1_0_1_3_gamma,EDLOS_rule_out2_0_1_3_gamma,
                                   EDLOS_rule_in1_0_1_3_gamma,EDLOS_rule_in2_0_1_3_gamma)
  # return summary in hours
  return(summary(EDLOS_esc_0_1_3/60))
}


# ----------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------- #

short_summary <- function(
    performance,
    ED_LoS,
    ADP){
  #unpack 
  ED_LoS<- t(as.numeric(ED_LoS))
  ED_LoS_median<-ED_LoS[3]
  ED_LoS_25pct <-ED_LoS[2]
  ED_LoS_75pct <-ED_LoS[5]
  
  performance             <- round(t(performance),3)
  #Values
  
  MI_Prevalence           <- paste0(as.character(performance["MI_Prevalence",]),"(",as.character(performance["MI_Prevalence_LCL",]),",",as.character(performance["MI_Prevalence_UCL",]),")")
  Rule_out_proportion     <- paste0(as.character(performance["Rule_out_prop",]),"(",as.character(performance["Rule_out_prop_LCL",]),",",as.character(performance["Rule_out_prop_UCL",]),")")
  Rule_in_proportion      <- paste0(as.character(performance["Rule_in_prop",]),"(",as.character(performance["Rule_in_prop_LCL",]),",",as.character(performance["Rule_in_prop_UCL",]),")")
  Observation_proportion  <- paste0(as.character(performance["Observation_prop",]),"(",as.character(performance["Observation_prop_LCL",]),",",as.character(performance["Observation_prop_UCL",]),")")
  
  NPV                     <- paste0(as.character(performance["NPV",]),"(",as.character(performance["NPV_LCL",]),",",as.character(performance["NPV_UCL",]),")")
  PPV                     <- paste0(as.character(performance["PPV",]),"(",as.character(performance["PPV_LCL",]),",",as.character(performance["PPV_UCL",]),")")
  
  ED_LOS                  <- paste0(as.character(round(ED_LoS_median,3)),"(",as.character(round(ED_LoS_25pct,3)),",",as.character(round(ED_LoS_75pct,3)),")")
  # Tibble
  adp_short_summary <- tibble(
    `ADP`                = as.character(ADP),
    "Performance Metric" = c("MI-Prevalence","Rule-out (%)", "Rule-in (%)", "Observation (%)", "NPV (%)", "PPV (%)", "ED-LoS (IQR)"),
    `Value`              = c(MI_Prevalence, Rule_out_proportion, Rule_in_proportion, Observation_proportion, NPV, PPV, ED_LOS),
  )
  return(adp_short_summary)
  
}


