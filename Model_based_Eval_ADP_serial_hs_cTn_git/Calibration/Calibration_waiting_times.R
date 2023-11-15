# Calibration of Waiting Times
# project: Model-based Analysis of ADPs that use hs-cTn for detection of NSTE-MI in the ED
# Author: MLM
# Notes: pending issues include sensitivity to different parametric families, and parameter uncertainty.
# ----------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------- #
# Time to presentation after symptom onset ----
# We fit a weibull distribution to match the 25th, 36th , 50th and 75th percentiles as reported in twerenbold
c<-10^3
# these values are obtained from twerenbold page 486, Table 1
# IQR: 6(2,15)
# 36% <= 3hrs
quantiles<- c(2,3,6,15,23)
# 23 hrs is the highest reported time to presentation
pct25    <- runif(25*c,0,quantiles[1])
pct25_36 <- runif(11*c,quantiles[1],quantiles[2])
pct36_50 <- runif(14*c,quantiles[2],quantiles[3])
pct50_75 <- runif(25*c,quantiles[3],quantiles[4])
pct75_100 <-runif(25*c,quantiles[4],quantiles[5])
data_t_present<- c(pct25    ,
                   pct25_36 ,
                   pct36_50 ,
                   pct50_75 ,
                   pct75_100
)
summary(data_t_present)
time2presentation_mixture_params_fit_weib <- fitdist(data_t_present, distr = "weibull", method = "mle")
# draw times of presentation
shape     <- time2presentation_mixture_params_fit_weib[[1]][[1]]
scale     <- time2presentation_mixture_params_fit_weib[[1]][[2]]
N_sim_aux <- 10^3
times_presentation_weib <- rweibull(N_sim_aux, shape = shape+0.60, scale=scale-1)
# Quick Diagnostic
ggplot(data = tibble(x=times_presentation_weib), aes(x=x)) +
  geom_boxplot()+
  geom_vline(xintercept = c(quantiles[1],quantiles[3],quantiles[4]), col = "red")
plot(ecdf(times_presentation_weib))
plot(ecdf(data_t_present))
plot(density(times_presentation_weib))
summary(times_presentation_weib)
summary(data_t_present)
time2presentation_mixture_params_fit_weib <-  list(shape=shape+0.60,scale=scale-1)
save(time2presentation_mixture_params_fit_weib, file="Outputs/time2presentation_mixture_params_fit_weib.RData")

# ----------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------- #
# ED-Length of Stay (ED-LoS) ----
# Waiting times at ED are specific to the ADP being evaluated and to the management decision taken (rule-in,rule-out, observe)
## 0/1 algorithm ----
# Targets obtained from Twerenbold et al 2019, page 488 Table 2
### rule out ----
# targets
quantiles_ro <-c(69,130,150,215,300)
c<-100
pct10     <- runif(10*c,0,quantiles_ro[1])
pct10_25  <- runif(15*c,quantiles_ro[1],quantiles_ro[2])
pct25_50  <- runif(25*c,quantiles_ro[2],quantiles_ro[3])
pct50_75  <- runif(25*c,quantiles_ro[3],quantiles_ro[4])
pct75_90  <- runif(15*c,quantiles_ro[4],quantiles_ro[5])
pct90_100 <- runif(10*c,quantiles_ro[5],19*60)

data_t_present_ro_0_1<- c(pct10    ,
                          pct10_25 ,
                          pct25_50 ,
                          pct50_75 ,
                          pct75_90 ,
                          pct90_100
)
summary(data_t_present_ro_0_1)
# gamma
EDLOS_ro_0_1_params_fit_gamma <- fitdist(data_t_present_ro_0_1, distr = "gamma", method = "mle")
# draw times of presentation
EDLOS_ro_0_1_params_fit_gamma
shape_ro_0_1            <- EDLOS_ro_0_1_params_fit_gamma$estimate[1]
rate_ro_0_1             <- EDLOS_ro_0_1_params_fit_gamma$estimate[2]
n_ro_0_1                <- 10^3
EDLOS_rule_out_0_1_gamma <- round(rgamma(n_ro_0_1, rate = rate_ro_0_1, shape = shape_ro_0_1-0.22))
#, scale=scale_ro_0_1-45))
# Quick Diagnostic
ggplot(data = tibble(x=EDLOS_rule_out_0_1_gamma), aes(x=x)) +
  geom_boxplot()+
  geom_vline(xintercept = c(quantiles_ro[2],quantiles_ro[3],quantiles_ro[4]), col = "red") + coord_cartesian(xlim=c(0,500))
summary(EDLOS_rule_out_0_1_gamma) 
summary(data_t_present_ro_0_1)
plot(ecdf(EDLOS_rule_out_0_1_gamma))
plot(ecdf(data_t_present_ro_0_1))
EDLOS_ro_0_1_params_fit_gamma <- list(rate =rate_ro_0_1,shape =shape_ro_0_1-0.22)
save(EDLOS_ro_0_1_params_fit_gamma, file="Outputs/EDLOS_ro_0_1_params_fit_gamma.RData")

# ----#
### rule in ----
quantiles_ri <-c(1.9*60,140,150,265,408) 
c<-100
# these values are obtained from twerenbold page 486, Table 1
pct10     <- runif(10*c,0,quantiles_ri[1])
pct10_25  <- runif(15*c,quantiles_ri[1],quantiles_ri[2])
pct25_50  <- runif(25*c,quantiles_ri[2],quantiles_ri[3])
pct50_75  <- runif(25*c,quantiles_ri[3],quantiles_ri[4])
pct75_90  <- runif(15*c,quantiles_ri[4],quantiles_ri[5])
pct90_100 <- runif(10*c,quantiles_ri[5],19*60)

data_t_present_ri_0_1<- c(pct10    ,
                          pct10_25 ,
                          pct25_50 ,
                          pct50_75 ,
                          pct75_90 ,
                          pct90_100
)
summary(data_t_present_ri_0_1)
EDLOS_ri_0_1_params_fit_gamma <- fitdist(data_t_present_ri_0_1, distr = "gamma", method = "mle")
# draw times of presentation
shape_ri_0_1        <- EDLOS_ri_0_1_params_fit_gamma$estimate[1]
rate_ri_0_1         <- EDLOS_ri_0_1_params_fit_gamma$estimate[2]
n_ri_0_1            <- 10^3
EDLOS_rule_in_0_1_gamma <- round(rgamma(n_ri_0_1, shape = shape_ri_0_1-0.365, rate=rate_ri_0_1))
# Quick Diagnostic
ggplot(data = tibble(x=EDLOS_rule_in_0_1_gamma), aes(x=x)) +
  geom_boxplot()+
  geom_vline(xintercept = c(quantiles_ri[2],quantiles_ri[3],quantiles_ri[4]), col = "red") + coord_cartesian(xlim=c(0,500))
summary(EDLOS_rule_in_0_1_gamma)/60 
summary(data_t_present_ri_0_1)/60
plot(ecdf(EDLOS_rule_in_0_1_gamma))
plot(ecdf(data_t_present_ri_0_1))
EDLOS_ri_0_1_params_fit_gamma <- list(rate= rate_ri_0_1, shape= shape_ri_0_1-0.365)
save(EDLOS_ri_0_1_params_fit_gamma, file="Outputs/EDLOS_ri_0_1_params_fit_gamma.RData")

# ----#
### observe ----
quantiles_obs<-c(2.2*60,143,156,273,6.8*60) 
c<-100
# these values are obtained from twerenbold page 486, Table 1
pct10     <- runif(10*c,0,quantiles_obs[1])
pct10_25  <- runif(15*c,quantiles_obs[1],quantiles_obs[2])
pct25_50  <- runif(25*c,quantiles_obs[2],quantiles_obs[3])
pct50_75  <- runif(25*c,quantiles_obs[3],quantiles_obs[4])
pct75_90  <- runif(15*c,quantiles_obs[4],quantiles_obs[5])
pct90_100 <- runif(10*c,quantiles_obs[5],19*60)

data_t_present_obs_0_1<- c(pct10    ,
                           pct10_25 ,
                           pct25_50 ,
                           pct50_75 ,
                           pct75_90 ,
                           pct90_100
)
summary(data_t_present_obs_0_1)
EDLOS_obs_0_1_params_fit_gamma <- fitdist(data_t_present_obs_0_1, distr = "gamma", method = "mle")
# draw times of presentation
shape_obs_0_1 <- EDLOS_obs_0_1_params_fit_gamma$estimate[1]
rate_obs_0_1  <- EDLOS_obs_0_1_params_fit_gamma$estimate[2]
n_obs_0_1     <- 10^3
EDLOS_obs_0_1_gamma <- round(rgamma(n_obs_0_1, shape = shape_obs_0_1-.014, rate=rate_obs_0_1+0.0024))
# Quick Diagnostic
ggplot(data = tibble(x=EDLOS_obs_0_1_gamma), aes(x=x)) +
  geom_boxplot()+
  geom_vline(xintercept = c(quantiles_obs[2],quantiles_obs[3],quantiles_obs[4]), col = "red") + coord_cartesian(xlim=c(0,500))
summary(EDLOS_obs_0_1_gamma)/60 
summary(data_t_present_obs_0_1)/60
plot(ecdf(EDLOS_obs_0_1_gamma))
plot(ecdf(data_t_present_obs_0_1))
EDLOS_obs_0_1_params_fit_gamma <- list(rate=rate_obs_0_1+0.0024,shape=shape_obs_0_1-.014)
save(EDLOS_ro_0_1_params_fit_gamma, file="Outputs/EDLOS_obs_0_1_params_fit_gamma.RData")

# ---- #
### overall ----
# combining ED-LoS of different buckets
summary(c(EDLOS_rule_out_0_1_gamma,
          EDLOS_rule_in_0_1_gamma,
          EDLOS_obs_0_1_gamma
          ) )#/60 
EDLOS_0_1_params <- list(
  rule_out    = EDLOS_ro_0_1_params_fit_gamma,
  rule_in     = EDLOS_ri_0_1_params_fit_gamma,
  observation = EDLOS_obs_0_1_params_fit_gamma
)
save(EDLOS_0_1_params, file="Outputs/EDLOS_0_1_params.RData")

# ----------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------- #
## 0/2 algorithm---- 
### Rule out ----
# targets
quantiles_ro <-c(69,130,150,215,300)+60
c<-100
pct10     <- runif(10*c,0,quantiles_ro[1])
pct10_25  <- runif(15*c,quantiles_ro[1],quantiles_ro[2])
pct25_50  <- runif(25*c,quantiles_ro[2],quantiles_ro[3])
pct50_75  <- runif(25*c,quantiles_ro[3],quantiles_ro[4])
pct75_90  <- runif(15*c,quantiles_ro[4],quantiles_ro[5])
pct90_100 <- runif(10*c,quantiles_ro[5],19*60)

data_t_present_ro_0_2<- c(pct10    ,
                          pct10_25 ,
                          pct25_50 ,
                          pct50_75 ,
                          pct75_90 ,
                          pct90_100
)
summary(data_t_present_ro_0_2)
# gamma
EDLOS_ro_0_2_params_fit_gamma <- fitdist(data_t_present_ro_0_2, distr = "gamma", method = "mle")
# draw times of presentation
EDLOS_ro_0_2_params_fit_gamma
shape_ro_0_2            <- EDLOS_ro_0_2_params_fit_gamma$estimate[1]
rate_ro_0_2             <- EDLOS_ro_0_2_params_fit_gamma$estimate[2]
EDLOS_ro_0_2_params_fit_gamma
n_ro_0_2                <- 10^3
EDLOS_rule_out_0_2_gamma <- round(rgamma(n_ro_0_2, rate = rate_ro_0_2, shape = shape_ro_0_2-0.13))
#, scale=scale_ro_0_2-45))
# Quick Diagnostic
ggplot(data = tibble(x=EDLOS_rule_out_0_2_gamma), aes(x=x)) +
  geom_boxplot()+
  geom_vline(xintercept = c(quantiles_ro[2],quantiles_ro[3],quantiles_ro[4]), col = "red") + coord_cartesian(xlim=c(0,500))
summary(EDLOS_rule_out_0_2_gamma)/60 
summary(data_t_present_ro_0_2)/60
plot(ecdf(EDLOS_rule_out_0_2_gamma))
plot(ecdf(data_t_present_ro_0_2))
EDLOS_ro_0_2_params_fit_gamma <- list(shape=shape_ro_0_2-0.13,rate=rate_ro_0_2)
save(EDLOS_ro_0_2_params_fit_gamma, file="Outputs/EDLOS_ro_0_2_params_fit_gamma.RData")

# ----#
### rule in ----
quantiles_ri <-c(1.9*60,140,150,265,408) +60
c<-100
# these values are obtained from twerenbold page 486, Table 1
pct10     <- runif(10*c,0,quantiles_ri[1])
pct10_25  <- runif(15*c,quantiles_ri[1],quantiles_ri[2])
pct25_50  <- runif(25*c,quantiles_ri[2],quantiles_ri[3])
pct50_75  <- runif(25*c,quantiles_ri[3],quantiles_ri[4])
pct75_90  <- runif(15*c,quantiles_ri[4],quantiles_ri[5])
pct90_100 <- runif(10*c,quantiles_ri[5],19*60)

data_t_present_ri_0_2<- c(pct10    ,
                          pct10_25 ,
                          pct25_50 ,
                          pct50_75 ,
                          pct75_90 ,
                          pct90_100
)
summary(data_t_present_ri_0_2)
EDLOS_ri_0_2_params_fit_gamma <- fitdist(data_t_present_ri_0_2, distr = "gamma", method = "mle", lower = c(0, 0))
# draw times of presentation
shape_ri_0_2        <- EDLOS_ri_0_2_params_fit_gamma$estimate[1]
rate_ri_0_2         <- EDLOS_ri_0_2_params_fit_gamma$estimate[2]
n_ri_0_2            <- 10^3
EDLOS_rule_in_0_2_gamma <- round(rgamma(n_ri_0_2, shape = shape_ri_0_2-0.29, rate=rate_ri_0_2+0.0005))
# Quick Diagnostic
ggplot(data = tibble(x=EDLOS_rule_in_0_2_gamma), aes(x=x)) +
  geom_boxplot()+
  geom_vline(xintercept = c(quantiles_ri[2],quantiles_ri[3],quantiles_ri[4]), col = "red") + coord_cartesian(xlim=c(0,500))
summary(EDLOS_rule_in_0_2_gamma)/60 
summary(data_t_present_ri_0_2)/60
plot(ecdf(EDLOS_rule_in_0_2_gamma))
plot(ecdf(data_t_present_ri_0_2))
EDLOS_ri_0_2_params_fit_gamma <- list(shape=shape_ri_0_2-0.29,rate=rate_ri_0_2+0.0005)
save(EDLOS_ri_0_2_params_fit_gamma, file="Outputs/EDLOS_ri_0_2_params_fit_gamma.RData")

# ----#
### observe ----
quantiles_obs<-c(2.2*60,143,156,273,6.8*60) +60
c<-100
# these values are obtained from twerenbold page 486, Table 1
pct10     <- runif(10*c,0,quantiles_obs[1])
pct10_25  <- runif(15*c,quantiles_obs[1],quantiles_obs[2])
pct25_50  <- runif(25*c,quantiles_obs[2],quantiles_obs[3])
pct50_75  <- runif(25*c,quantiles_obs[3],quantiles_obs[4])
pct75_90  <- runif(15*c,quantiles_obs[4],quantiles_obs[5])
pct90_100 <- runif(10*c,quantiles_obs[5],19*60)

data_t_present_obs_0_2<- c(pct10    ,
                           pct10_25 ,
                           pct25_50 ,
                           pct50_75 ,
                           pct75_90 ,
                           pct90_100
)
summary(data_t_present_obs_0_2)
EDLOS_obs_0_2_params_fit_gamma <- fitdist(data_t_present_obs_0_2, distr = "gamma", method = "mle")
# draw times of presentation
shape_obs_0_2 <- EDLOS_obs_0_2_params_fit_gamma$estimate[1]
rate_obs_0_2  <- EDLOS_obs_0_2_params_fit_gamma$estimate[2]
n_obs_0_2     <- 10^3
EDLOS_obs_0_2_gamma <- round(rgamma(n_obs_0_2, shape = shape_obs_0_2+0.003, rate=rate_obs_0_2+0.0023))
# Quick Diagnostic
ggplot(data = tibble(x=EDLOS_obs_0_2_gamma), aes(x=x)) +
  geom_boxplot()+
  geom_vline(xintercept = c(quantiles_obs[2],quantiles_obs[3],quantiles_obs[4]), col = "red") + coord_cartesian(xlim=c(0,500))
summary(EDLOS_obs_0_2_gamma)/60 
summary(data_t_present_obs_0_2)/60
plot(ecdf(EDLOS_obs_0_2_gamma))
plot(ecdf(data_t_present_obs_0_2))
EDLOS_obs_0_2_params_fit_gamma <- list(shape=shape_obs_0_2+0.003,rate=rate_obs_0_2+0.0023)
save(EDLOS_ro_0_2_params_fit_gamma, file="Outputs/EDLOS_obs_0_2_params_fit_gamma.RData")

# ---- #
### overall ----
# combining ED-LoS of different buckets
summary(c(EDLOS_rule_out_0_2_gamma,
          EDLOS_rule_in_0_2_gamma,
          EDLOS_obs_0_2_gamma
) )#/60 
EDLOS_0_2_params <- list(
  rule_out    = EDLOS_ro_0_2_params_fit_gamma,
  rule_in     = EDLOS_ri_0_2_params_fit_gamma,
  observation = EDLOS_obs_0_2_params_fit_gamma
)
save(EDLOS_0_2_params, file="Outputs/EDLOS_0_2_params.RData")

# ----------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------- #
## 0/1/3 algorithm ----
# the calculation of n's is a bit different here
### Rule out,stage 1 ----
quantiles_ro <-c(69,130,150,215,300)
c<-100
# these values are obtained from twerenbold page 486, Table 1
pct10     <- runif(10*c,0,quantiles_ro[1])
pct10_25  <- runif(15*c,quantiles_ro[1],quantiles_ro[2])
pct25_50  <- runif(25*c,quantiles_ro[2],quantiles_ro[3])
pct50_75  <- runif(25*c,quantiles_ro[3],quantiles_ro[4])
pct75_90  <- runif(15*c,quantiles_ro[4],quantiles_ro[5])
pct90_100 <- runif(10*c,quantiles_ro[5],19*60)

data_t_present_ro1_0_1_3<- c(pct10    ,
                            pct10_25 ,
                            pct25_50 ,
                            pct50_75 ,
                            pct75_90 ,
                            pct90_100
)
# gamma
EDLOS_ro1_0_1_3_params_fit_gamma <- fitdist(data_t_present_ro1_0_1_3, distr = "gamma", method = "mle")
# draw times of presentation
EDLOS_ro1_0_1_3_params_fit_gamma
shape_ro1_0_1_3             <- EDLOS_ro1_0_1_3_params_fit_gamma$estimate[1]
rate_ro1_0_1_3            <- EDLOS_ro1_0_1_3_params_fit_gamma$estimate[2]
n_ro1_0_1_3                <- 10^3
EDLOS_rule_out1_0_1_3_gamma <- round(rgamma(n_ro1_0_1_3, rate = rate_ro1_0_1_3, shape = shape_ro1_0_1_3-.2))
# Quick Diagnostic
ggplot(data = tibble(x=EDLOS_rule_out1_0_1_3_gamma), aes(x=x)) +
  geom_boxplot()+
  geom_vline(xintercept = c(quantiles_ro[2],quantiles_ro[3],quantiles_ro[4]), col = "red") + coord_cartesian(xlim=c(0,500))
summary(EDLOS_rule_out1_0_1_3_gamma)/60 
summary(data_t_present_ro_0_1_3)/60
plot(ecdf(EDLOS_rule_out1_0_1_3_gamma))
plot(ecdf(data_t_present_ro_0_1_3))
EDLOS_ro1_0_1_3_params_fit_gamma <- list(shape=shape_ro1_0_1_3-.2,rate=rate_ro1_0_1_3)
save(EDLOS_ro1_0_1_3_params_fit_gamma, file="Outputs/EDLOS_ro_0_1_3_params_fit_gamma.RData")

#---#
### Rule out,stage 2 ----
quantiles_ro <-c(69,130,150,215,300)+120
c<-100
# these values are obtained from twerenbold page 486, Table 1
pct10     <- runif(10*c,0,quantiles_ro[1])
pct10_25  <- runif(15*c,quantiles_ro[1],quantiles_ro[2])
pct25_50  <- runif(25*c,quantiles_ro[2],quantiles_ro[3])
pct50_75  <- runif(25*c,quantiles_ro[3],quantiles_ro[4])
pct75_90  <- runif(15*c,quantiles_ro[4],quantiles_ro[5])
pct90_100 <- runif(10*c,quantiles_ro[5],19*60)

data_t_present_ro2_0_1_3<- c(pct10    ,
                            pct10_25 ,
                            pct25_50 ,
                            pct50_75 ,
                            pct75_90 ,
                            pct90_100
)
summary(data_t_present_ro2_0_1_3)
EDLOS_ro2_0_1_3_params_fit_gamma <- fitdist(data_t_present_ro2_0_1_3, distr = "gamma", method = "mle")
# draw times of presentation
shape_ro2_0_1_3 <- EDLOS_ro2_0_1_3_params_fit_gamma$estimate[1]
rate_ro2_0_1_3  <- EDLOS_ro2_0_1_3_params_fit_gamma$estimate[2]
n_ro2_0_1_3     <- 10^3
EDLOS_rule_out2_0_1_3_gamma <- round(rgamma(n_ro2_0_1_3, shape = shape_ro2_0_1_3-0.2, rate=rate_ro2_0_1_3))
# Quick Diagnostic
ggplot(data = tibble(x=EDLOS_rule_out2_0_1_3_gamma), aes(x=x)) +
  geom_boxplot()+
  geom_vline(xintercept = c(quantiles_ro[2],quantiles_ro[3],quantiles_ro[4]), col = "red") + coord_cartesian(xlim=c(0,500))
summary(EDLOS_rule_out2_0_1_3_gamma)/60 
summary(data_t_present_ro2_0_1_3)/60
plot(ecdf(EDLOS_rule_out2_0_1_3_gamma))
plot(ecdf(data_t_present_ro2_0_1_3))
EDLOS_ro2_0_1_3_params_fit_gamma <- list(shape=shape_ro2_0_1_3-0.2,rate=rate_ro2_0_1_3)
save(EDLOS_ro2_0_1_3_params_fit_gamma,file="Outputs/EDLOS_ro2_0_1_3_params_fit_gamma.RData")
# ----#
### Rule in, stage 1 ----
quantiles_ri <-c(1.9*60,140,150,265,408) 
c<-100
# these values are obtained from twerenbold page 486, Table 1
pct10     <- runif(10*c,0,quantiles_obs[1])
pct10_25  <- runif(15*c,quantiles_ri[1],quantiles_ri[2])
pct25_50  <- runif(25*c,quantiles_ri[2],quantiles_ri[3])
pct50_75  <- runif(25*c,quantiles_ri[3],quantiles_ri[4])
pct75_90  <- runif(15*c,quantiles_ri[4],quantiles_ri[5])
pct90_100 <- runif(10*c,quantiles_ri[5],19*60)

data_t_present_ri1_0_1_3<- c(pct10    ,
                            pct10_25 ,
                            pct25_50 ,
                            pct50_75 ,
                            pct75_90 ,
                            pct90_100
)
summary(data_t_present_ri1_0_1_3)
EDLOS_ri1_0_1_3_params_fit_gamma <- fitdist(data_t_present_ri1_0_1_3, distr = "gamma", method = "mle")
# draw times of presentation
shape_ri1_0_1_3 <- EDLOS_ri1_0_1_3_params_fit_gamma$estimate[1]
rate_ri1_0_1_3  <- EDLOS_ri1_0_1_3_params_fit_gamma$estimate[2]
n_ri_0_1_3_stage1 <- round(mean(df_adp_esc_0_1_3$rule_in_Ind, na.rm=T)*N_sim_plot)
EDLOS_rule_in1_0_1_3_gamma <- round(rgamma(n_ri_0_1_3_stage1, shape = shape_ri1_0_1_3+0.0009, rate=rate_ri1_0_1_3+0.0035))
# Quick Diagnostic
ggplot(data = tibble(x=EDLOS_rule_in1_0_1_3_gamma), aes(x=x)) +
  geom_boxplot()+
  geom_vline(xintercept = c(quantiles_ri[2],quantiles_ri[3],quantiles_ri[4]), col = "red") + coord_cartesian(xlim=c(0,500))
summary(EDLOS_rule_in1_0_1_3_gamma)/60 
summary(data_t_present_ri1_0_1_3)/60
plot(ecdf(EDLOS_rule_in1_0_1_3_gamma))
plot(ecdf(data_t_present_ri1_0_1_3))
EDLOS_ri1_0_1_3_params_fit_gamma <- list(shape=shape_ri1_0_1_3+0.0009,rate=rate_ri1_0_1_3+0.0035)
save(EDLOS_ri1_0_1_3_params_fit_gamma,file="Outputs/EDLOS_ri1_0_1_3_params_fit_gamma.RData")

# --- #
### Rule in, stage 2 ----
quantiles_ri <-c(1.9*60,140,150,265,408) +120
c<-100
# these values are obtained from twerenbold page 486, Table 1
pct10     <- runif(10*c,0,quantiles_ri[1])
pct10_25  <- runif(15*c,quantiles_ri[1],quantiles_ri[2])
pct25_50  <- runif(25*c,quantiles_ri[2],quantiles_ri[3])
pct50_75  <- runif(25*c,quantiles_ri[3],quantiles_ri[4])
pct75_90  <- runif(15*c,quantiles_ri[4],quantiles_ri[5])
pct90_100 <- runif(10*c,quantiles_ri[5],19*60)

data_t_present_ri2_0_1_3<- c(pct10    ,
                            pct10_25 ,
                            pct25_50 ,
                            pct50_75 ,
                            pct75_90 ,
                            pct90_100
)
EDLOS_ri2_0_1_3_params_fit_gamma <- fitdist(data_t_present_ri2_0_1_3, distr = "gamma", method = "mle")
# draw times of presentation
shape_ri2_0_1_3 <- EDLOS_ri2_0_1_3_params_fit_gamma$estimate[1]
rate_ri2_0_1_3  <- EDLOS_ri2_0_1_3_params_fit_gamma$estimate[2]
n_ri2_0_1_3_stage2 <- 10^3
EDLOS_rule_in2_0_1_3_gamma <- round(rgamma(n_ri2_0_1_3_stage2, shape = shape_ri2_0_1_3, rate=rate_ri2_0_1_3+0.00185))
# Quick Diagnostic
ggplot(data = tibble(x=EDLOS_rule_in2_0_1_3_gamma), aes(x=x)) +
  geom_boxplot()+
  geom_vline(xintercept = c(quantiles_ri[2],quantiles_ri[3],quantiles_ri[4]), col = "red") + coord_cartesian(xlim=c(0,650))
summary(EDLOS_rule_in2_0_1_3_gamma)/60 
summary(data_t_present_ri2_0_1_3)/60
plot(ecdf(EDLOS_rule_in2_0_1_3_gamma))
plot(ecdf(data_t_present_ri2_0_1_3))
EDLOS_ri2_0_1_3_params_fit_gamma <- list(shape=shape_ri2_0_1_3,rate=rate_ri2_0_1_3+0.00185)
save(EDLOS_ri2_0_1_3_params_fit_gamma,file="Outputs/EDLOS_ri2_0_1_3_params_fit_gamma.RData")

#----#
### Overall-----
summary(c(ED_wait_rule_out_0_1_3_stage1,
          ED_wait_rule_out_0_1_3_stage2,
          ED_wait_rule_in_0_1_3_stage1,
          ED_wait_rule_in_0_1_3_stage2
) )/60 
EDLOS_0_1_3_params <- list(
  rule_out1    = EDLOS_ro1_0_1_3_params_fit_gamma,
  rule_out2    = EDLOS_ro2_0_1_3_params_fit_gamma,
  rule_in1     = EDLOS_ri1_0_1_3_params_fit_gamma,
  rule_in2     = EDLOS_ri2_0_1_3_params_fit_gamma
)
save(EDLOS_0_1_3_params, file="Outputs/EDLOS_0_1_3_params.RData")


