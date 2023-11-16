# Calibration Functions
# project: Model-based Analysis of ADPs that use hs-cTn for detection of NSTE-MI in the ED
# Author: MLM
# Notes: pending issues include: perform code performance profiling, finish unit-testing.
# ----------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------- #

# Objective function, Stochastic Optimization
# Inputs: 
## Initial values for calibration params
## weights for weighted sampling
## sample size
## time horizon
## vector of means of param distribution
## covariance matrix of param distribution
## Calibration Targets
# Output: returns quadratic loss computed from  observed targets and predicted targets

stochastic_calibration_fun <- function(
    v_cparams,                                                                  # Initial values for calibration params
    w           = 1,                                                            # option for weighted sampling
    N_sim       = 500,                                                          # simulated sample size
    v_time      = 0:60,                                                         # time horizon
    v_mean,                                                                     # vector of means,  params  
    m_sigma,                                                                    # covariance matrix of distribution of  params
    l_targets                                                                   # calibration targets
){
  #-----------------------#
  # Pre-processing inputs
  #-----------------------#
  # sample for specific subpopulation
  n            <- round(w*N_sim, 0)
  # unlist mean vec and cov matrix
  v_mean       <- unlist(v_mean)
  m_sigma      <- m_sigma
  # unpack calibration param initial values
  c            <- as.numeric(v_cparams[1])
  #l           <- as.numeric(c(v_cparams[2],1,v_cparams[3],1))                  # vector <l1,1,l2,1>, scaling constants variances
  l            <- as.numeric(c(v_cparams[2:5]))                                 # vector <l1,1,l2,1>, scaling constants variances
  k            <- as.numeric(c(v_cparams[4],v_cparams[5]))                      # vector <k1,k2>, scaling constant for means
  # scaling mean vector
  v_mean[1]    <- v_mean[1]*k[1]                                                #c_s1
  v_mean[3]    <- v_mean[3]*k[2]                                                #c_u1
  
  # new diagonal for cov matrix
  new_diag  <- diag(m_sigma) * l
  #-----------------------#
  # Generate Trajectories
  #-----------------------#
  # Sample sets of parameters for each observation
  temp     <- as.data.frame(
    rtmvnorm(n, 
             mean  = v_mean,
             sigma = m_sigma -(diag(m_sigma)*diag(length(new_diag))) + (new_diag* diag( length(new_diag))), #scaled covariance matrix
             lower = c(0,0,0,0),
             algorithm="rejection"                               
    ))
  colnames(temp)      <- c("s1","s2","u1","u2")
  # Expand data frame with parameters for each time point in the trajectories
  params_df                                 <- as_tibble(temp)
  params_df$ID                              <- 1:nrow(params_df)
  params_df_long                            <- params_df %>% slice(rep(1:n, each = length(v_time)))
  params_df_long$time                       <- rep(v_time,n)
  #unpacking time of presentation, using median value here
  params_df_long$time_presentation          <- l_targets$time_presentation$median# rdunif(n,3,8)
  # Testing ---#
  #return(params_df_long)
  # -----      #
  # Trajectories
  df_out <- params_df_long %>% mutate(
    cTn_concentration = c * ( (time^s1)/( (u1^s1) + (time^s1)) ) * ( (u2^s2)/( (u2^s2) + (time^s2)) ))
  # Testing ---#
  #return(df_out)
  # -----------#
  # GOF
  # Target 1, predicted Peak  
  df_gof_predicted_peak  <- df_out %>% group_by(ID) %>% 
    summarise(max_cTn_concentration = max(cTn_concentration))
  # Target 2, predicted presentation concentration
  df_gof_predicted_presentation                           <- df_out[df_out$time_presentation == df_out$time, ] %>% dplyr::select(ID,cTn_concentration)
  names(df_gof_predicted_presentation)                    <- c("ID","presentation_cTn_concentration")
  # Target 3, predicted delta 12hrs
  df_gof_predicted_delta_12hrs                            <- df_out[df_out$time_presentation +12 == df_out$time, ] %>% dplyr::select(ID,cTn_concentration)
  names(df_gof_predicted_delta_12hrs)                     <- c("ID","delta_12hrs_cTn_concentration")
  #all are now summarized by ID
  # -----------#
  # -----------#
  # # Target 1, Distribution(IQR) of predicted Peak 
  mean_max_cTn_concentration                              <- mean(df_gof_predicted_peak$max_cTn_concentration, na.rm = T)
  median_max_cTn_concentration                            <- median(df_gof_predicted_peak$max_cTn_concentration, na.rm = T)
  quant_25th_max_cTn_concentration                        <- as.numeric(quantile(df_gof_predicted_peak$max_cTn_concentration, .25, na.rm = T))
  quant_75th_max_cTn_concentration                        <- as.numeric(quantile(df_gof_predicted_peak$max_cTn_concentration, .75, na.rm = T))
  
  l_gof_predicted_peak <- list(
    median_max_cTn_concentration      = median_max_cTn_concentration,
    quant_25th_max_cTn_concentration  = quant_25th_max_cTn_concentration,
    quant_75th_max_cTn_concentration  = quant_75th_max_cTn_concentration  )
  # Testing ---#
  #return(l_gof_predicted_peak)
  # -----------#
  # Target 2, Distribution(IQR) of predicted presentation concentration
  mean_presentation_cTn_concentration                     <- mean(df_gof_predicted_presentation$presentation_cTn_concentration, na.rm = T)
  median_presentation_cTn_concentration                   <- median(df_gof_predicted_presentation$presentation_cTn_concentration, na.rm = T)
  quant_25th_presentation_cTn_concentration               <- as.numeric(quantile(df_gof_predicted_presentation$presentation_cTn_concentration, .25, na.rm = T))
  quant_75th_presentation_cTn_concentration               <- as.numeric(quantile(df_gof_predicted_presentation$presentation_cTn_concentration, .75, na.rm = T))
  
  l_gof_predicted_presentation <- list(
    median_presentation_cTn_concentration=median_presentation_cTn_concentration,
    quant_25th_presentation_cTn_concentration=quant_25th_presentation_cTn_concentration,
    quant_75th_presentation_cTn_concentration=quant_75th_presentation_cTn_concentration)
  # -----------#
  # Target 3, Distribution(IQR) of predicted delta 12hrs concentration
  median_presentation_cTn_concentration                  <- median(df_gof_predicted_delta_12hrs$delta_12hrs_cTn_concentration, na.rm = T)
  quant_25th_presentation_cTn_concentration              <- as.numeric(quantile(df_gof_predicted_delta_12hrs$delta_12hrs_cTn_concentration, .25, na.rm = T))
  quant_75th_presentation_cTn_concentration              <- as.numeric(quantile(df_gof_predicted_delta_12hrs$delta_12hrs_cTn_concentration, .75, na.rm = T))
  
  l_gof_predicted_delta_12hrs <- list(
    median_presentation_cTn_concentration=median_presentation_cTn_concentration,
    quant_25th_presentation_cTn_concentration=quant_25th_presentation_cTn_concentration,
    quant_75th_presentation_cTn_concentration=quant_75th_presentation_cTn_concentration)
  
  # Testing ---#
  #return(l_gof_predicted_presentation)
  # -----------#
  # -----------#
  # Objective Function, using Quadratic loss a distance metric
  quad_loss_peak_presentation_delta <- 
    #peak
    (l_gof_predicted_peak$median_max_cTn_concentration                         -  l_targets$peak$median      )^2         +    
    #(l_gof_predicted_peak$quant_25th_max_cTn_concentration                     -  l_targets$peak$quant_25pct )^2         +
    #(l_gof_predicted_peak$quant_75th_max_cTn_concentration                     -  l_targets$peak$quant_75pct )^2         +
    #presentation
    (l_gof_predicted_presentation$median_presentation_cTn_concentration        - l_targets$presentation$median     )^2   + 
    (l_gof_predicted_presentation$quant_25th_presentation_cTn_concentration    - l_targets$presentation$quant_25pct)^2   +
    (l_gof_predicted_presentation$quant_75th_presentation_cTn_concentration    - l_targets$presentation$quant_75pct)^2   +
    # delta 12hrs
    (abs(l_gof_predicted_delta_12hrs$median_presentation_cTn_concentration     - l_targets$delta_12hrs$median)     )^2   #+
    (abs(l_gof_predicted_delta_12hrs$quant_25th_presentation_cTn_concentration - l_targets$delta_12hrs$quant_25pct))^2   +
    (abs(l_gof_predicted_delta_12hrs$quant_75th_presentation_cTn_concentration - l_targets$delta_12hrs$quant_75pct))^2
  
  # Testing ---#
  #return(quad_loss_peak_presentation_delta)
  # -----------#
  return(quad_loss_peak_presentation_delta)
}

# -----------#
# test
# NEW_uncertain_calibration_fun(
#   v_cparams = v_cparams                             ,
#   v_mean    = unlist(cTn_I_sample_params$mu_cTn_I)  ,
#   m_sigma   = cTn_I_sample_params$Sigma_cTn_I       ,
#   l_targets = l_targets_typeI_MI                            )
# -----------#

# ----------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------- #
# Box function
# I wrote this auxiliary function used for calibrating the mixture distribution of MI-specific trajectories
# the function creates grid-boxes (lattice) around parameter initial values constrained to the space [0,1]
# Dealing with Simplex: function keeps the parameter configurations that sum to one, or up to a user-specified error-threshold 
# is a combinatorial brute-force approach
# inputs: 
## vector/scalar of initial values all in the interval (0,1) and must sum to one, 
## vector/scalar with  bandwidth of boxes , 
## vector/scalar with number of bins for grid within boxes(has to be odd for now), 
## scalar with size of error epsilon
# output: matrix of possible configurations that sum to one, or up to a user-specified error-threshold (epsilon)

box_optim <- function(
    v_init_values,
    bandwidth     = 0.01,
    bins          = 10,
    epsilon       = (bandwidth/bins)/2){
  #create container
  boxes <- matrix(NA, nrow = bins, ncol = length(v_init_values))
  names <- c(paste("param",as.character(1:length(v_init_values)), sep= "_"))
  colnames(boxes) <- names
  #boxes
  #create grid-boxes (lattice) around initial values, store them in tibble
  for (i in seq_along(v_init_values)) {
    min <- v_init_values[i]-(bandwidth/2)
    max <- v_init_values[i]+(bandwidth/2)
    if (min < 0)  {min <- 0+(epsilon/2)}
    if (max >= 1) {max <- 1-(epsilon/2)}
    box <- seq(min, max, length.out = bins)
    boxes[,i] <- box
    boxes <- as_tibble(boxes)
  }
  # if number of bins is even we need to add the init value as row 
  # to be included in the possible configs
  if((bins%%2) == 0){ boxes <- rbind(boxes,v_init_values)}
  #boxes
  # all possible configurations
  boxes_configs <- expand.grid(boxes[,1:dim(boxes)[2]])
  # rowsums of configurations
  boxes_configs$sum <- rowSums(boxes_configs)
  #boxes_configs
  # keep those that sum to 1
  boxes_simplex     <- boxes_configs[boxes_configs$sum > 1-epsilon & boxes_configs$sum < 1 + epsilon,]
  # check we don't have crazy results
  sum(boxes_simplex < 0)
  sum(boxes_simplex[,1:length(v_init_values)] >1 )
  return(boxes_simplex)
  # check everything went alright
  if (sum(boxes_simplex < 0) !=0 || sum(boxes_simplex[,1:length(v_init_values)] >1 ) !=0){
    return("error: you have negative values or values above 1")
  }
}
# # test function
# v_init_values <- c((4064/46092),(1116/46092),(1676/46092),(1287/46092),(37904/46092))
# sum(v_init_values)
# bandwidth     <- 0.1
# bins          <- 20
# epsilon       <- (bandwidth/bins)/2
# box_optim (
#     v_init_values=v_init_values,
#     bandwidth=bandwidth,
#     bins=bins,
#     epsilon=epsilon)
#   

# ----------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------- #

# 6.1 Mixture Calibration 
# This function takes the potential configurations of params in the simplex generated by function box_optim
# generates trajectories with each configuration and computes the quadratic loss of observed and predicted targets
# returns a  list of param configurations based and the corresponding  quadratic loss 
# inputs
## same arguments as box_optim
## sample size
## time horizon
## list of optimal parameters for MI-specific trajectories
# output: tibble with param configuration (set of mixture weights), and their corresponding quadratic loss 

mixture_calibration_fun <-function(
    v_init_values,    # vector of initial values for weights
    l_targets,        # list of targets for calibration
    bandwidth ,
    bins      ,
    epsilon   ,
    N_sim = 500  ,   # total sample size
    v_time = 0:60, 
    optim_params_l    # list input parameters for each subpopulation
){
  # possible configs, grid-box
  possible_configs <- box_optim(v_init_values = v_init_values,
                                bandwidth = bandwidth,
                                bins = bins,
                                epsilon = epsilon)
  # container/output 
  # must have same nrows as possible configs
  output_loss <- cbind(possible_configs, 
                       target_1      = l_targets$cTn_presentation$median,
                       target_1_pred = NA,
                       target_2      = l_targets$cTn_1hr$median,
                       target_2_pred  = NA,
                       target_3      = l_targets$cTn_delta_1hr$median,
                       target_3_pred = NA,
                       comb_loss     = NA
  )
  # iteration 1 
  # simulate each subpopulation, according to each possible configuration of weights
  for (i in 1:nrow(output_loss)){
    # determine samples sizes each iteration
    n_typeI_MI   <- round(possible_configs[i,1]*N_sim) 
    n_typeII_MI  <- round(possible_configs[i,2]*N_sim) 
    n_acute_MI   <- round(possible_configs[i,3]*N_sim)
    n_chronic_MI <- round(possible_configs[i,4]*N_sim)
    n_No_MI      <- round(possible_configs[i,5]*N_sim)
    # check
    # sum(n_typeI_MI,n_typeII_MI,n_acute_MI ,n_chronic_MI,n_No_MI) ==N_sim
    
    # Type I MI 
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
    # simulating trajectory parameters for type I MI sub population, now we simulate all 5 parameters fo the hill functions
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
    # time of presentation, using fitted gamma to twerenbold here
    params_df_typeI_MI$time_presentation          <- 6 #rgamma(n_typeI_MI, shape = shape, rate=rate)
    # expanding 
    params_df_typeI_MI$ID                         <- 1:nrow(params_df_typeI_MI)
    params_df_typeI_MI_long                       <- params_df_typeI_MI %>% slice(rep(1:n_typeI_MI, each = length(v_time)))
    params_df_typeI_MI_long$time                  <- rep(v_time,n_typeI_MI)
    # Generate trajectories
    df_out_typeI_MI <- params_df_typeI_MI_long %>% mutate(
      cTn_concentration = c * ( (time^s1)/( (u1^s1) + (time^s1)) ) * ( (u2^s2)/( (u2^s2) + (time^s2)) ))
    df_out_typeI_MI$MI   <- "Type I"
    # Test -------------------- #
    #   } 
    #   return(df_out_typeI_MI)
    # }
    # ids <- sample(size=10, unique(df_out_typeI_MI$ID))
    # index <- which(df_out_typeI_MI$ID %in% ids)
    # ggplot(data= df_out_typeI_MI[index,], aes(x = time, y = cTn_concentration, color = factor(ID)))+
    #   geom_line()
    # ggplot(data= df_out_typeI_MI, aes(x = time, y = cTn_concentration, color = factor(ID)))+
    #   geom_line()
    # -----------#
    
    #Type II 
    k  <- optim_params_l[[2]]$k
    v_mean_typeII_MI <- tibble(
      s1  = optim_params_l[[2]]$s1 * k[1],
      s2  = optim_params_l[[2]]$s2,
      u1 = optim_params_l[[2]]$u1 * k[2],
      u2  = as.numeric(optim_params_l[[1]]$u2),
      c = optim_params_l[[2]]$c)
    
    m_sigma_typeII_MI              <- cTn_I_sample_params$Sigma_cTn_I        # original covariance matrix
    v_k_typeII_MI                  <- optim_params_l[[2]]$l                 # vector of calibrated constant to scale individual variances
    v_new_diag_typeII_MI           <- diag(m_sigma_typeII_MI) * v_k_typeII_MI  # new diagonal of cov matrix
    m_new_sigma_typeII_MI          <- m_sigma_typeII_MI -(diag(m_sigma_typeII_MI)*diag(length(v_new_diag_typeII_MI))) + (v_new_diag_typeII_MI* diag( length(v_new_diag_typeII_MI))) # replacing diagonal in the cov matrix with the scaled version 
    # adding tau to cov matrix
    m_new_sigma_typeII_MI          <- rbind(m_new_sigma_typeII_MI, c(0,0,0,0))
    m_new_sigma_typeII_MI          <- cbind(m_new_sigma_typeII_MI, c(0,0,0,0,1))
    rownames(m_new_sigma_typeII_MI)[5] <- "c"
    colnames(m_new_sigma_typeII_MI)[5] <- "c"
    # simulating trajectory parameters for type I MI sub population, now we simulate all 5 parameters fo the hill functions
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
    # time of presentation, using fitted gamma to twerenbold here
    params_df_typeII_MI$time_presentation          <- 6# rgamma(n_typeII_MI, shape = shape, rate=rate)
    # expanding 
    params_df_typeII_MI$ID                         <- 1:nrow(params_df_typeII_MI)
    params_df_typeII_MI_long                       <- params_df_typeII_MI %>% slice(rep(1:n_typeII_MI, each = length(v_time)))
    params_df_typeII_MI_long$time                  <- rep(v_time,n_typeII_MI)
    
    # View(params_df_typeII_MI_long)  
    # summary(params_df_typeII_MI_long$time_presentation)
    
    # Generate trajectories
    df_out_typeII_MI <- params_df_typeII_MI_long %>% mutate(
      cTn_concentration = c * ( (time^s1)/( (u1^s1) + (time^s1)) ) * ( (u2^s2)/( (u2^s2) + (time^s2)) ))
    df_out_typeII_MI$MI   <- "Type II"
    #   return(df_out_typeI_MI)
    # } 
    # ids <- sample(size=10, unique(df_out_typeII_MI$ID))
    # index <- which(df_out_typeII_MI$ID %in% ids)
    # ggplot(data= df_out_typeII_MI[index,], aes(x = time, y = cTn_concentration, color = factor(ID)))+
    #   geom_line()
    # ggplot(data= df_out_typeII_MI, aes(x = time, y = cTn_concentration, color = factor(ID)))+
    #   geom_line()
    # -----------#
    
    #Acute MI  
    k  <- optim_params_l[[3]]$k
    v_mean_acute_MI <- tibble(
      s1  = optim_params_l[[3]]$s1 * k[1],
      s2  = optim_params_l[[3]]$s2,
      u1  = optim_params_l[[3]]$u1 * k[2],
      u2  = as.numeric(optim_params_l[[1]]$u2),
      c   = optim_params_l[[3]]$c)
    
    m_sigma_acute_MI              <- cTn_I_sample_params$Sigma_cTn_I        # original covariance matrix
    v_k_acute_MI                  <- optim_params_l[[3]]$l                 # vector of calibrated constant to scale individual variances
    v_new_diag_acute_MI           <- diag(m_sigma_acute_MI) * v_k_acute_MI  # new diagonal of cov matrix
    m_new_sigma_acute_MI          <- m_sigma_acute_MI -(diag(m_sigma_acute_MI)*diag(length(v_new_diag_acute_MI))) + (v_new_diag_acute_MI* diag( length(v_new_diag_acute_MI))) # replacing diagonal in the cov matrix with the scaled version 
    # adding tau to cov matrix
    m_new_sigma_acute_MI          <- rbind(m_new_sigma_acute_MI, c(0,0,0,0))
    m_new_sigma_acute_MI          <- cbind(m_new_sigma_acute_MI, c(0,0,0,0,1))
    rownames(m_new_sigma_acute_MI)[5] <- "c"
    colnames(m_new_sigma_acute_MI)[5] <- "c"
    # simulating trajectory parameters for type I MI sub population, now we simulate all 5 parameters fo the hill functions
    temp_acute_MI     <- as.data.frame(
      rtmvnorm(n     = n_acute_MI, 
               mean  = as.numeric(v_mean_acute_MI),
               sigma = m_new_sigma_acute_MI, 
               lower = c(0,0,0,0,0),
               #upper= c(25,25),
               algorithm="rejection"                               
      ))
    colnames(temp_acute_MI)      <- c("s1","s2","u1","u2","c")
    #View(temp_acute_MI)  
    
    # Expand data frame with parameters for each time point in the trajectories
    params_df_acute_MI                                 <- as_tibble(temp_acute_MI)
    # time of presentation, using fitted gamma to twerenbold here
    params_df_acute_MI$time_presentation          <- 6#rgamma(n_acute_MI, shape = shape, rate=rate)
    # expanding 
    params_df_acute_MI$ID                         <- 1:nrow(params_df_acute_MI)
    params_df_acute_MI_long                       <- params_df_acute_MI %>% slice(rep(1:n_acute_MI, each = length(v_time)))
    params_df_acute_MI_long$time                  <- rep(v_time,n_acute_MI)
    
    # View(params_df_acute_MI_long)  
    # summary(params_df_acute_MI_long$time_presentation)
    
    # Generate trajectories
    df_out_acute_MI <- params_df_acute_MI_long %>% mutate(
      cTn_concentration = c * ( (time^s1)/( (u1^s1) + (time^s1)) ) * ( (u2^s2)/( (u2^s2) + (time^s2)) ))
    df_out_acute_MI$MI   <- "Acute"
    
    #   return(df_out_acute_MI)
    # }
    # ids <- sample(size=10, unique(df_out_acute_MI$ID))
    # index <- which(df_out_acute_MI$ID %in% ids)
    # ggplot(data= df_out_acute_MI[index,], aes(x = time, y = cTn_concentration, color = factor(ID)))+
    #   geom_line()
    # ggplot(data= df_out_acute_MI, aes(x = time, y = cTn_concentration, color = factor(ID)))+
    #   geom_line()
    # -----------#
    
    #No MI  
    k  <- optim_params_l[[5]]$k
    v_mean_No_MI <- tibble(
      s1  = optim_params_l[[5]]$s1 * k[1],
      s2  = optim_params_l[[5]]$s2,
      u1  = optim_params_l[[5]]$u1 * k[2],
      u2  = as.numeric(optim_params_l[[1]]$u2 * 1.5),
      c   = optim_params_l[[5]]$c)
    
    m_sigma_No_MI              <- cTn_I_sample_params$Sigma_cTn_I        # original covariance matrix
    v_k_No_MI                  <- optim_params_l[[5]]$l                 # vector of calibrated constant to scale individual variances
    v_new_diag_No_MI           <- diag(m_sigma_No_MI) * v_k_No_MI  # new diagonal of cov matrix
    m_new_sigma_No_MI          <- m_sigma_No_MI -(diag(m_sigma_No_MI)*diag(length(v_new_diag_No_MI))) + (v_new_diag_No_MI* diag( length(v_new_diag_No_MI))) # replacing diagonal in the cov matrix with the scaled version 
    # adding tau to cov matrix
    m_new_sigma_No_MI          <- rbind(m_new_sigma_No_MI, c(0,0,0,0))
    m_new_sigma_No_MI          <- cbind(m_new_sigma_No_MI, c(0,0,0,0,1))
    rownames(m_new_sigma_No_MI)[5] <- "c"
    colnames(m_new_sigma_No_MI)[5] <- "c"
    # simulating trajectory parameters for type I MI sub population, now we simulate all 5 parameters fo the hill functions
    temp_No_MI     <- as.data.frame(
      rtmvnorm(n     = n_No_MI, 
               mean  = as.numeric(v_mean_No_MI),
               sigma = m_new_sigma_No_MI, 
               lower = c(0,0,0,0,0),
               #upper= c(25,25),
               algorithm="rejection"                               
      ))
    colnames(temp_No_MI)      <- c("s1","s2","u1","u2","c")
    #View(temp_No_MI)  
    
    # Expand data frame with parameters for each time point in the trajectories
    params_df_No_MI                                 <- as_tibble(temp_No_MI)
    # time of presentation, using fitted gamma to twerenbold here
    params_df_No_MI$time_presentation          <- 6 #rgamma(n_No_MI, shape = shape, rate=rate)
    # hist(params_df_No_MI$time_presentation)
    # table(round(params_df_No_MI$time_presentation))
    
    # expanding 
    params_df_No_MI$ID                         <- 1:nrow(params_df_No_MI)
    params_df_No_MI_long                       <- params_df_No_MI %>% slice(rep(1:n_No_MI, each = length(v_time)))
    params_df_No_MI_long$time                  <- rep(v_time,n_No_MI)
    
    # View(params_df_No_MI_long)  
    # summary(params_df_No_MI_long$time_presentation)
    
    # Generate trajectories
    df_out_No_MI <- params_df_No_MI_long %>% mutate(
      cTn_concentration = c * ( (time^s1)/( (u1^s1) + (time^s1)) ) * ( (u2^s2)/( (u2^s2) + (time^s2)) ))
    df_out_No_MI$MI   <- "No MI"
    
    #   return(df_out_No_MI)
    # }
    # ids <- sample(size=20, unique(df_out_No_MI$ID))
    # index <- which(df_out_No_MI$ID %in% ids)
    # ggplot(data= df_out_No_MI[index,], aes(x = time, y = cTn_concentration, color = factor(ID)))+
    #   geom_line()
    # ggplot(data= df_out_No_MI, aes(x = time, y = cTn_concentration, color = factor(ID)))+
    #   geom_line()
    # -----------#
    
    #Chronic MI 
    k  <- optim_params_l[[4]]$k
    v_mean_chronic_MI <- tibble(
      s1  = optim_params_l[[4]]$s1 * k[1],
      s2  = optim_params_l[[4]]$s2,
      u1  = optim_params_l[[4]]$u1 * k[2],
      u2  = as.numeric(optim_params_l[[1]]$u2),
      c   = optim_params_l[[4]]$c)
    
    m_sigma_chronic_MI              <- cTn_I_sample_params$Sigma_cTn_I        # original covariance matrix
    v_k_chronic_MI                  <- optim_params_l[[4]]$l                 # vector of calibrated constant to scale individual variances
    v_new_diag_chronic_MI           <- diag(m_sigma_chronic_MI) * v_k_chronic_MI  # new diagonal of cov matrix
    m_new_sigma_chronic_MI          <- m_sigma_chronic_MI -(diag(m_sigma_chronic_MI)*diag(length(v_new_diag_chronic_MI))) + (v_new_diag_chronic_MI* diag( length(v_new_diag_chronic_MI))) # replacing diagonal in the cov matrix with the scaled version 
    # adding tau to cov matrix
    m_new_sigma_chronic_MI          <- rbind(m_new_sigma_chronic_MI, c(0,0,0,0))
    m_new_sigma_chronic_MI          <- cbind(m_new_sigma_chronic_MI, c(0,0,0,0,1))
    rownames(m_new_sigma_chronic_MI)[5] <- "c"
    colnames(m_new_sigma_chronic_MI)[5] <- "c"
    # simulating trajectory parameters for type I MI sub population, now we simulate all 5 parameters fo the hill functions
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
    # time of presentation, using fitted gamma to twerenbold here
    params_df_chronic_MI$time_presentation          <- 6#rgamma(n_chronic_MI, shape = shape, rate=rate)
    # expanding 
    params_df_chronic_MI$ID                         <- 1:nrow(params_df_chronic_MI)
    params_df_chronic_MI_long                       <- params_df_chronic_MI %>% slice(rep(1:n_chronic_MI, each = length(v_time)))
    params_df_chronic_MI_long$time                  <- rep(v_time,n_chronic_MI)
    
    # Generate trajectories
    df_out_chronic_MI <- params_df_chronic_MI_long %>% mutate(
      cTn_concentration = c * ( (time^s1)/( (u1^s1) + (time^s1)) ) * ( (u2^s2)/( (u2^s2) + (time^s2)) ))
    df_out_chronic_MI$MI   <- "Chronic"
    # -----------#
    
    # all trajectories
    
    df_out_all_MI<- rbind(df_out_typeI_MI, df_out_typeII_MI, df_out_acute_MI, df_out_chronic_MI,df_out_No_MI)
    df_out_all_MI$ID_MI <- paste(df_out_all_MI$ID,df_out_all_MI$MI, sep="_")
    #   }
    #   return(df_out_all_MI)
    # }
    
    # -----------#
    # GOF
    ## Target 1, presentation cTn
    df_out_all_MI$time_presentation <- round(df_out_all_MI$time_presentation)
    #table(df_out_all_MI$time_presentation)
    df_gof_predicted_presentation                   <- df_out_all_MI[df_out_all_MI$time_presentation == df_out_all_MI$time, ] %>%
      dplyr::select(ID,ID_MI,time_presentation,cTn_concentration)#%>%arrange(ID)
    names(df_gof_predicted_presentation)            <- c("ID","ID_MI","time_presentation","presentation_cTn_concentration")
    # -----------#
    # Distribution of predicted presentation concentration
    mean_presentation_cTn_concentration        <- mean(df_gof_predicted_presentation$presentation_cTn_concentration, na.rm = T)
    median_presentation_cTn_concentration      <- median(df_gof_predicted_presentation$presentation_cTn_concentration, na.rm = T)
    quant_25th_presentation_cTn_concentration  <- as.numeric(quantile(df_gof_predicted_presentation$presentation_cTn_concentration, .25, na.rm = T))
    quant_75th_presentation_cTn_concentration  <- as.numeric(quantile(df_gof_predicted_presentation$presentation_cTn_concentration, .75, na.rm = T))
    
    l_gof_predicted_presentation <- list(mean_presentation_cTn_concentration=mean_presentation_cTn_concentration,
                                         median_presentation_cTn_concentration=median_presentation_cTn_concentration,
                                         quant_25th_presentation_cTn_concentration=quant_25th_presentation_cTn_concentration,
                                         quant_75th_presentation_cTn_concentration=quant_75th_presentation_cTn_concentration)
    l_gof_predicted_presentation
    # -----------#
    
    ## Target 2, 1hr cTn
    df_out_all_MI$time_presentation_plus_1hr <- df_out_all_MI$time_presentation+1
    df_gof_predicted_1hr                     <- df_out_all_MI[(df_out_all_MI$time_presentation_plus_1hr)== df_out_all_MI$time, ] %>%
      dplyr::select(ID,ID_MI,time_presentation_plus_1hr,cTn_concentration)%>%arrange(ID)
    names(df_gof_predicted_1hr)              <- c("ID","ID_MI","time_1hr","1hr_cTn_concentration")
    # df_gof_predicted_1hr
    # summary(df_gof_predicted_1hr$'1hr_cTn_concentration')
    # dim(df_gof_predicted_1hr)
    
    # -----------#
    # Distribution of predicted 1hr concentration
    mean_1hr_cTn_concentration        <- mean(df_gof_predicted_1hr$'1hr_cTn_concentration', na.rm = T)
    median_1hr_cTn_concentration      <- median(df_gof_predicted_1hr$'1hr_cTn_concentration', na.rm = T)
    quant_25th_1hr_cTn_concentration  <- as.numeric(quantile(df_gof_predicted_1hr$'1hr_cTn_concentration', .25, na.rm = T))
    quant_75th_1hr_cTn_concentration  <- as.numeric(quantile(df_gof_predicted_1hr$'1hr_cTn_concentration', .75, na.rm = T))
    
    l_gof_predicted_1hr <- list(mean_1hr_cTn_concentration=mean_1hr_cTn_concentration,
                                median_1hr_cTn_concentration=median_1hr_cTn_concentration,
                                quant_25th_1hr_cTn_concentration=quant_25th_1hr_cTn_concentration,
                                quant_75th_1hr_cTn_concentration=quant_75th_1hr_cTn_concentration)
    l_gof_predicted_1hr
    # -----------#
    
    # Target 3, 1hr delta cTn
    delta_1hr                       <- abs(df_gof_predicted_1hr$`1hr_cTn_concentration` - df_gof_predicted_presentation$presentation_cTn_concentration)
    # left joins
    
    df_gof_predicted_1hr_delta      <- cbind(df_gof_predicted_presentation,
                                             df_gof_predicted_1hr$time_1hr,df_gof_predicted_1hr$`1hr_cTn_concentration`,
                                             delta_1hr)
    df_gof_predicted_1hr_delta
    
    # -----------#
    # Distribution of predicted 1hr delta
    mean_1hr_delta_cTn_concentration        <- mean(df_gof_predicted_1hr_delta$delta_1hr, na.rm = T)
    median_1hr_delta_cTn_concentration      <- median(df_gof_predicted_1hr_delta$delta_1hr, na.rm = T)
    quant_25th_1hr_delta_cTn_concentration  <- as.numeric(quantile(df_gof_predicted_1hr_delta$delta_1hr, .25, na.rm = T))
    quant_75th_1hr_delta_cTn_concentration  <- as.numeric(quantile(df_gof_predicted_1hr_delta$delta_1hr, .75, na.rm = T))
    
    l_gof_predicted_1hr_delta <- list(mean_1hr_delta_cTn_concentration=mean_1hr_delta_cTn_concentration,
                                      median_1hr_delta_cTn_concentration=median_1hr_delta_cTn_concentration,
                                      quant_25th_1hr_delta_cTn_concentration=quant_25th_1hr_delta_cTn_concentration,
                                      quant_75th_1hr_delta_cTn_concentration=quant_75th_1hr_delta_cTn_concentration)
    l_gof_predicted_1hr_delta
    
    #Mean Absolute Error (MAE)
    # target 1
    mae_loss_presentation <- abs(l_gof_predicted_presentation$median_presentation_cTn_concentration     - l_targets$cTn_presentation$median  )        +
      abs(l_gof_predicted_presentation$quant_25th_presentation_cTn_concentration - l_targets$cTn_presentation$pct25)     +
      abs(l_gof_predicted_presentation$quant_75th_presentation_cTn_concentration - l_targets$cTn_presentation$pct75)
    
    # target 2
    mae_loss_1hr <- abs(l_gof_predicted_1hr$median_1hr_cTn_concentration     - l_targets$cTn_1hr$median  )        +
      abs(l_gof_predicted_1hr$quant_25th_1hr_cTn_concentration - l_targets$cTn_1hr$pct25)     +
      abs(l_gof_predicted_1hr$quant_75th_1hr_cTn_concentration - l_targets$cTn_1hr$pct75)
    
    # target 3
    mae_loss_1hr_delta <- abs(l_gof_predicted_1hr_delta$median_1hr_delta_cTn_concentration     - l_targets$cTn_delta_1hr$median  )        +
      abs(l_gof_predicted_1hr_delta$quant_25th_1hr_delta_cTn_concentration - l_targets$cTn_delta_1hr$pct25)     +
      abs(l_gof_predicted_1hr_delta$quant_75th_1hr_delta_cTn_concentration - l_targets$cTn_delta_1hr$pct75)
    
    #combined targets, 1,2, and 3
    
    mae_loss_combined   <-
      #presentation
      abs(l_gof_predicted_presentation$median_presentation_cTn_concentration     - l_targets$cTn_presentation$median  )  +
      abs(l_gof_predicted_presentation$quant_25th_presentation_cTn_concentration - l_targets$cTn_presentation$pct25)     +
      abs(l_gof_predicted_presentation$quant_75th_presentation_cTn_concentration - l_targets$cTn_presentation$pct75)     +
      # 1hr
      # abs(l_gof_predicted_1hr$median_1hr_cTn_concentration     - l_targets$cTn_1hr$median  )  +
      # abs(l_gof_predicted_1hr$quant_25th_1hr_cTn_concentration - l_targets$cTn_1hr$pct25)     +
      # abs(l_gof_predicted_1hr$quant_75th_1hr_cTn_concentration - l_targets$cTn_1hr$pct75)     #+
      # 1hr delta
      abs(l_gof_predicted_1hr_delta$median_1hr_delta_cTn_concentration     - l_targets$cTn_delta_1hr$median  )  +
      abs(l_gof_predicted_1hr_delta$quant_25th_1hr_delta_cTn_concentration - l_targets$cTn_delta_1hr$pct25)     +
      abs(l_gof_predicted_1hr_delta$quant_75th_1hr_delta_cTn_concentration - l_targets$cTn_delta_1hr$pct75)
    
    #str(l_targets)
    
    
    # ------ # 
    # Quad Loss
    # target 1
    quad_loss_presentation <- (l_gof_predicted_presentation$median_presentation_cTn_concentration     - l_targets$cTn_presentation$median  )^2        +
      (l_gof_predicted_presentation$quant_25th_presentation_cTn_concentration - l_targets$cTn_presentation$pct25)^2     +
      (l_gof_predicted_presentation$quant_75th_presentation_cTn_concentration - l_targets$cTn_presentation$pct75)^2
    
    # target 2
    quad_loss_1hr <- (l_gof_predicted_1hr$median_1hr_cTn_concentration     - l_targets$cTn_1hr$median  )^2        +
      (l_gof_predicted_1hr$quant_25th_1hr_cTn_concentration - l_targets$cTn_1hr$pct25)^2     +
      (l_gof_predicted_1hr$quant_75th_1hr_cTn_concentration - l_targets$cTn_1hr$pct75)^2
    
    # target 3
    quad_loss_1hr_delta <- (l_gof_predicted_1hr_delta$median_1hr_delta_cTn_concentration     - l_targets$cTn_delta_1hr$median  )^2        +
      (l_gof_predicted_1hr_delta$quant_25th_1hr_delta_cTn_concentration - l_targets$cTn_delta_1hr$pct25)^2     +
      (l_gof_predicted_1hr_delta$quant_75th_1hr_delta_cTn_concentration - l_targets$cTn_delta_1hr$pct75)^2
    
    #combined targets, 1,2, and 3
    
    quad_loss_combined   <-
      #presentation
      (l_gof_predicted_presentation$median_presentation_cTn_concentration     - l_targets$cTn_presentation$median  )^2  +
      (l_gof_predicted_presentation$quant_25th_presentation_cTn_concentration - l_targets$cTn_presentation$pct25)^2     +
      (l_gof_predicted_presentation$quant_75th_presentation_cTn_concentration - l_targets$cTn_presentation$pct75)^2     +
      # 1hr
      # (l_gof_predicted_1hr$median_1hr_cTn_concentration     - l_targets$cTn_1hr$median  )^2  +
      # (l_gof_predicted_1hr$quant_25th_1hr_cTn_concentration - l_targets$cTn_1hr$pct25)^2     +
      # (l_gof_predicted_1hr$quant_75th_1hr_cTn_concentration - l_targets$cTn_1hr$pct75)^2     #+
      # 1hr delta
      (l_gof_predicted_1hr_delta$median_1hr_delta_cTn_concentration     - l_targets$cTn_delta_1hr$median  )^2  +
      (l_gof_predicted_1hr_delta$quant_25th_1hr_delta_cTn_concentration - l_targets$cTn_delta_1hr$pct25)^2     +
      (l_gof_predicted_1hr_delta$quant_75th_1hr_delta_cTn_concentration - l_targets$cTn_delta_1hr$pct75)^2
    
    # assign loss  to output of each iteration
    output_loss$comb_loss1[i]    <- quad_loss_combined
    output_loss$comb_loss2[i]    <- mae_loss_combined
    # assign other quantities of interest to output of each iteration
    output_loss$target_1_pred[i]  <- l_gof_predicted_presentation$median_presentation_cTn_concentration
    output_loss$target_2_pred[i]  <- l_gof_predicted_1hr$median_1hr_cTn_concentration
    output_loss$target_3_pred[i]  <- l_gof_predicted_1hr_delta$median_1hr_delta_cTn_concentration
  }
  return(output_loss)
}
