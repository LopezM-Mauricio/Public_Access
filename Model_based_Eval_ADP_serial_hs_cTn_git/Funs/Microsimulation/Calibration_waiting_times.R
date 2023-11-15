# Calibration of Waiting Times
# project: Model-based Analysis of ADPs that use hs-cTn for detection of NSTE-MI in the ED
# Author: MLM
# Notes: pending issues include perform code performance profiling, finish unit-testing.
# ----------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------- #
# Time to presentation after symptom onset ----
# We fit a weibull distribution to match the 25th, 36th , 50th and 75th percentiles as reported in twerenbold
c<-10^2
# these values are obtained from twerenbold page 486, Table 1
# IQR: 6(2,15)
# 36% <= 3hrs
# 23 hrs is the highest reported time to presentation
pct25    <- runif(25*c,0,2)
pct25_36 <- runif(11*c,2,3)
pct36_50 <- runif(14*c,3,6)
pct50_75 <- runif(25*c,6,15)
pct75_100 <-runif(25*c,15,23)
data_t_present<- c(pct25    ,
                   pct25_36 ,
                   pct36_50 ,
                   pct50_75 ,
                   pct75_100
)
summary(data_t_present)
time2presentation_mixture_params_fit_weib <- fitdist(data_t_present, distr = "weibull", method = "mle", lower = c(0, 0))
save(time2presentation_mixture_params_fit_weib, file="Outputs/time2presentation_mixture_params_fit_weib.RData")

# draw times of presentation
shape <- fit.weib$estimate[1]
scale  <- fit.weib$estimate[2]
# Check fit of time to presentation gamma model 
N_sim_aux <- length(unique(temp$ID_MI))
times_presentation_weib <- rweibull(N_sim_aux, shape = shape+0.60, scale=scale-1)
# Quick Diagnostic
ggplot(data = tibble(x=times_presentation_weib), aes(x=x)) +
  geom_boxplot()+
  geom_vline(xintercept = c(2,6,15), col = "red")
plot(ecdf(times_presentation_weib))
plot(density(times_presentation_weib))
summary(times_presentation_weib)
summary(data_t_present)

