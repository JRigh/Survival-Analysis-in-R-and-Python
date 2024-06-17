#----------------------------#
# Kaplan-Meier analysis in R #
#----------------------------#

#load libraries
library(survival)
library(ggplot2)
library(gridExtra)

# load data and head of the dataset
data(veteran)
head(veteran)

# "+" indicates censoring (incomplete information due to the event of interest not being observed)
km = with(veteran, Surv(time, status))
head(km, 100)

# Kaplan-Meier estimates of the probability of survival over time
kma_1 = survfit(Surv(time, status) ~ 1, data=veteran)
# max time: 999 days (about 33 months (30 days))
survival.30.dataset = summary(kma_1, times = c(1, (1:33)*30))
# convert summary to data.frame for plotting
cols = lapply(1:15 , function(x) survival.30.dataset[x])
df = do.call(data.frame, cols) 
# clean initial data frame
df = df[, 1:6]
# table to be displayed next to the graph as a second graph
df2 = df[1:20, c(2,6)]
df2$surv = round(df2$surv, 4)

# Plot using 'autoplot()' and ggplot2 customization (and with information about censoring)
p3 = autoplot(kma_1) +
  labs(title = 'Kaplan-Meier analysis',
       subtitle = 'Veteran data',
       y="Survival rate", x="time") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

p4 = tableGrob(df2) # to have a table with time and survival rate 
grid.arrange(p3, p4, ncol = 2, nrow = 1, widths = c(6, 2))

# Analysis by treatment

# Kaplan-Meier estimates of the probability of survival over time
kma_3 = survfit(Surv(time, status) ~ trt, data=veteran)
# max time: 999 days (about 33 months (30 days))
survival.30.dataset.celltype = summary(kma_3, times = c(1, (1:33)*30))

# plotting
autoplot(kma_3) +
  labs(title = 'Kaplan-Meier analysis by treatment',
       subtitle = 'Veteran data',
       y="Survival rate", x="time") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# Analysis by cell type

# Kaplan-Meier estimates of the probability of survival over time
kma_2 = survfit(Surv(time, status) ~ celltype, data=veteran)
# max time: 999 days (about 33 months (30 days))
survival.30.dataset.celltype = summary(kma_2, times = c(1, (1:33)*30))

# plotting
autoplot(kma_2) +
  labs(title = 'Kaplan-Meier analysis by cell type',
       subtitle = 'Veteran data',
       y="Survival rate", x="time") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))


#-----#
# end #
#-----#