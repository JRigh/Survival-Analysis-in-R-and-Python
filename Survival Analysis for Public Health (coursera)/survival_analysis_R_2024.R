#------------------------------------------#
# Week 1                                   #
# Survival Analysis for public health in R #
#------------------------------------------#

library(survival) # this is the cornerstone command for survival analysis in R
library(ggplot2) # newer package that does nice plots

g = read.csv2(file = 'C:/Users/julia/OneDrive/Desktop/Coursera/Survival Analysis R/survival_data_coursera.csv',
              header = TRUE, sep=',')

gender <- as.factor(g[,"gender"]) # R calls categorical variables factors
fu_time <- g[,"fu_time"] # continuous variable (numeric) 
death <- g[,"death"] # binary variable (numeric) 

km_fit <- survfit(Surv(fu_time, death) ~ 1)
plot(km_fit)

summary(km_fit, times = c(1:7,30,60,90*(1:10))) 

km_gender_fit <- survfit(Surv(fu_time, death) ~ gender) 

plot(km_gender_fit)

survdiff(Surv(fu_time, death) ~ gender, rho=0) 

survdiff(formula = Surv(fu_time, death) ~ gender, rho = 0) 

age_65plus <- ifelse(g[,"age"]>=65,1,0) # dichotomise age
table(age_65plus, exclude = NULL) # inspect the numbers - always a good idea

table(age,age_65plus, exclude = NULL) # check - an even better idea...
age_65plus

#-------------------------------------------#
# Week 2                                    #
# Cox proportional hazard regression models #
#-------------------------------------------#

# load libraries
library(survival)
library(survminer)

# run cox regression
cox = coxph(Surv(fu_time, death) ~ ethnicgroup, data = g) # take variables straight from g
summary(cox)

# second model
ethnicgroup <- factor(g[,"ethnicgroup"]) # can also use “as.factor” rather than “factor”
fu_time <- g[,"fu_time"]
death <- g[,"death"]

cox = coxph(Surv(fu_time, death) ~ ethnicgroup)
summary(cox)

# add a factor
levels(ethnicgroup)<-c(levels(ethnicgroup),"8") # add level 8 to the factor
ethnicgroup[is.na(ethnicgroup)] <- "8" # Change NA to "None"

# third model
cox = coxph(Surv(fu_time, death) ~ ethnicgroup) 
summary(cox) 


#-------------------------------------------#
# Week 3                                    #
# Running descriptives                      #
#-------------------------------------------#

summary(g$age)

t <- table(gender, exclude=NULL)
addmargins(t) # adds the total (a "sum" column)
round(100*prop.table(t),digits=1) # get %s rounded to 1dp

t <- table(g$copd, exclude=NULL)
addmargins(t) # adds the total (a "sum" column)
round(100*prop.table(t),digits=1) # get %s rounded to 1dp

t <- table(g$prior_dnas, exclude=NULL) 
addmargins(t) # adds the total (a "sum" column) 
round(100*prop.table(t),digits=1) # get %s rounded to 1dp 
# So nearly three in four patients had missed no appointments, 
# but nearly three percent had missed five or more, with a maximum of ten. 

t <- table(ethnicgroup, exclude=NULL) 
addmargins(t) # adds the total (a "sum" column) 
round(100*prop.table(t),digits=1) # get %s rounded to 1dp 

# Cox models
cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + prior_dnas + ethnicgroup, data = g)
summary(cox)

cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile + ethnicgroup, data = g)
summary(cox)

cox <- coxph(formula = Surv(fu_time, death) ~ age + gender + copd +  
               quintile + ethnicgroup, data = g) 
summary(cox)


#-------------------------------------------#
# Week 4                                    #
# Checking the Proportionality assumption   #
#-------------------------------------------#

cox.zph(cox, transform="km", global=TRUE)

fit <- coxph(Surv(fu_time, death) ~ gender) # fit the desired model

temp <- cox.zph(fit)# apply the cox.zph function to the desired model
print(temp) # display the results
plot(temp) # plot the curves


#-----#
# end #
#-----#