#-----------------------
# Survival analysis in R
#-----------------------

library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)

data(veteran)
head(veteran)

km = with(veteran, Surv(time, status))
head(km,80)

#  Kaplan-Meier estimates of the probability of survival over time
km_fit = survfit(Surv(time, status) ~ 1, data=veteran)
summary(km_fit, times = c(1,30,60,90*(1:10)))
autoplot(km_fit)

# Kaplan-Meier survival curves by treatment
km_trt_fit <- survfit(Surv(time, status) ~ trt, data=veteran)
autoplot(km_trt_fit)

# by age (<60)
vet = mutate(veteran, AG = ifelse((age < 60), "LT60", "OV60"),
              AG = factor(AG),
              trt = factor(trt,labels=c("standard","test")),
              prior = factor(prior,labels=c("N0","Yes")))

km_AG_fit = survfit(Surv(time, status) ~ AG, data=vet)
autoplot(km_AG_fit)

# Cox proportional hazard model
cox <- coxph(Surv(time, status) ~ trt + celltype + karno  + diagtime + age + prior , data = vet)
summary(cox)

aa_fit <-aareg(Surv(time, status) ~ trt + celltype +
                 karno + diagtime + age + prior , 
               data = vet)
aa_fit

## Call:
## aareg(formula = Surv(time, status) ~ trt + celltype + karno + 
##     diagtime + age + prior, data = vet)
## 
##   n= 137 
##     75 out of 97 unique event times used
## 
##                       slope      coef se(coef)      z        p
## Intercept          0.083400  3.81e-02 1.09e-02  3.490 4.79e-04
## trttest            0.006730  2.49e-03 2.58e-03  0.967 3.34e-01
## celltypesmallcell  0.015000  7.30e-03 3.38e-03  2.160 3.09e-02
## celltypeadeno      0.018400  1.03e-02 4.20e-03  2.450 1.42e-02
## celltypelarge     -0.001090 -6.21e-04 2.71e-03 -0.229 8.19e-01
## karno             -0.001180 -4.37e-04 8.77e-05 -4.980 6.28e-07
## diagtime          -0.000243 -4.92e-05 1.64e-04 -0.300 7.65e-01
## age               -0.000246 -6.27e-05 1.28e-04 -0.491 6.23e-01
## priorYes           0.003300  1.54e-03 2.86e-03  0.539 5.90e-01
## 
## Chisq=41.62 on 8 df, p=1.6e-06; test weights=aalen

#summary(aa_fit)  # provides a more complete summary of results
autoplot(aa_fit)



# random forest model

# ranger model
r_fit <- ranger(Surv(time, status) ~ trt + celltype + 
                  karno + diagtime + age + prior,
                data = vet,
                mtry = 4,
                importance = "permutation",
                splitrule = "extratrees",
                verbose = TRUE)

# Average the survival models
death_times <- r_fit$unique.death.times 
surv_prob <- data.frame(r_fit$survival)
avg_prob <- sapply(surv_prob,mean)

# Plot the survival models for each patient
plot(r_fit$unique.death.times,r_fit$survival[1,], type = "l", ylim = c(0,1),
     col = "red",xlab = "Days",ylab = "survival", main = "Patient Survival Curves")
cols <- colors()
for (n in sample(c(2:dim(vet)[1]), 20)){
  lines(r_fit$unique.death.times, r_fit$survival[n,], type = "l", col = cols[n])}
lines(death_times, avg_prob, lwd = 2)
legend(500, 0.7, legend = c('Average = black'))