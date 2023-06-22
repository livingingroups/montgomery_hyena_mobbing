######################################################################
##### 13.0 Modeling mobbing participation - individual traits #####
######################################################################

########## 13.1 Set working directory & download packages/tables ##########

rm(list = ls())
setwd("~/Documents/R/LionHyena")
options(stringsAsFactors = FALSE)
library(GGally)
library(glmmTMB)
library(MuMIn)
library(multcomp)
library(DHARMa)
library(see)
library(performance)
library(viridis)
library(sjPlot)
library(patchwork)
library(tidyverse)
hyenadata::update_tables("1.2.88")
library(hyenadata)

#Load data
load("12.id_by_mob.Rdata")

#Set plot colors using viridis
viridis_2 <- viridis(7)[-c(1,3,5,6,7)]
viridis_3 <- viridis(7)[-c(2,4,6,7)]


########## 13.2 Looking at combined data ##########

nrow(id.by.mob)     #4740
length(unique(id.by.mob$hyena))     #492
length(unique(id.by.mob$mobID))     #344
length(unique(id.by.mob$session))     #119
nrow(filter(id.by.mob, mobber == TRUE))     #1577
nrow(filter(id.by.mob, mobber == FALSE))     #3163
round(nrow(filter(id.by.mob, mobber == TRUE))/nrow(id.by.mob), dig = 2)   #0.33
round(nrow(filter(id.by.mob, mobber == FALSE))/nrow(id.by.mob), dig = 2)   #0.67

#Participants
participants <- filter(id.by.mob, mobber == TRUE)
nrow(participants)     #1577
nrow(filter(participants, sex == "f"))     #1212
nrow(filter(participants, sex == "m"))     #363
round(nrow(filter(participants, sex == "f"))/nrow(participants), dig = 2)   #0.77
round(nrow(filter(participants, sex == "m"))/nrow(participants), dig = 2)   #0.23

nrow(filter(participants, age.class == "adult"))     #1398
nrow(filter(participants, age.class == "juvenile"))     #179
round(nrow(filter(participants, age.class == "adult"))/nrow(participants), dig = 2)   #0.89
round(nrow(filter(participants, age.class == "juvenile"))/nrow(participants), dig = 2)   #0.11
nrow(filter(participants, !is.na(age)))     #1507
round(mean(participants$age, na.rm = T), digits = 2)     #5.07
round(sd(participants$age, na.rm = T), digits = 2)     #3.07
round(median(participants$age, na.rm = T), digits = 2)     #4.3
round(min(participants$age, na.rm = T), digits = 2)     #0.75
round(max(participants$age, na.rm = T), digits = 2)     #18.56

nrow(filter(participants, rank.cat == "high"))     #1099
nrow(filter(participants, rank.cat == "medium"))     #324
nrow(filter(participants, rank.cat == "low"))     #115
nrow(filter(participants, !is.na(stan.rank)))     #1538
round(mean(participants$stan.rank, na.rm = T), digits = 2)     #0.51
round(sd(participants$stan.rank, na.rm = T), digits = 2)     #0.47
round(median(participants$stan.rank, na.rm = T), digits = 2)     #0.69
round(min(participants$stan.rank, na.rm = T), digits = 2)     #-1
round(max(participants$stan.rank, na.rm = T), digits = 2)     #1

#Non-participants
present <- filter(id.by.mob, mobber == FALSE)
nrow(present)     #3163
nrow(filter(present, sex == "f"))     #1804
nrow(filter(present, sex == "m"))     #1339
round(nrow(filter(present, sex == "f"))/nrow(present), dig = 2)   #0.57
round(nrow(filter(present, sex == "m"))/nrow(present), dig = 2)   #0.42

nrow(filter(present, age.class == "adult"))     #2167
nrow(filter(present, age.class == "juvenile"))     #991
round(nrow(filter(present, age.class == "adult"))/nrow(present), dig = 2)   #0.69
round(nrow(filter(present, age.class == "juvenile"))/nrow(present), dig = 2)   #0.31
nrow(filter(present, !is.na(age)))     #2893
round(mean(present$age, na.rm = T), digits = 2)     #4.18
round(sd(present$age, na.rm = T), digits = 2)     #3.54
round(median(present$age, na.rm = T), digits = 2)     #2.79
round(min(present$age, na.rm = T), digits = 2)     #0.17
round(max(present$age, na.rm = T), digits = 2)     #21.19

nrow(filter(present, rank.cat == "high"))     #1774
nrow(filter(present, rank.cat == "medium"))     #819
nrow(filter(present, rank.cat == "low"))     #527
nrow(filter(present, !is.na(stan.rank)))     #3120
round(mean(present$stan.rank, na.rm = T), digits = 2)     #0.32
round(sd(present$stan.rank, na.rm = T), digits = 2)     #0.57
round(median(present$stan.rank, na.rm = T), digits = 2)     #0.47
round(min(present$stan.rank, na.rm = T), digits = 2)     #-1
round(max(present$stan.rank, na.rm = T), digits = 2)     #1


########## 13.3 Looking at variability across hyenas ##########

#Variation in mobbing by mob
hyenas.mob <- data.frame(hyena = unique(id.by.mob$hyena), num.mobs = NA, tot.mobs = NA)
for(i in 1:nrow(hyenas.mob)){
  hyena.i <- hyenas.mob$hyena[i]
  hyenas.mob$num.mobs[i] <- length(unique(filter(id.by.mob, hyena == hyena.i & mobber == T)$mobID))
  hyenas.mob$tot.mobs[i] <- length(unique(filter(id.by.mob, hyena == hyena.i)$mobID))
}
hyenas.mob$prop.mob <- round(hyenas.mob$num.mobs/hyenas.mob$tot.mobs, dig = 2)

nrow(filter(hyenas.mob, prop.mob == 0))     #189 hyenas never mobbed
nrow(filter(hyenas.mob, prop.mob == 1))     #44 hyenas mobbed at every opportunity
nrow(filter(hyenas.mob, prop.mob != 0 & prop.mob != 1))     #259 hyenas mobbed at some but not all opportunities
round(mean(filter(hyenas.mob, prop.mob != 0 & prop.mob != 1)$prop.mob), dig = 2)     #0.38
round(median(filter(hyenas.mob, prop.mob != 0 & prop.mob != 1)$prop.mob), dig = 2)    #0.33
#hyenas with variable mobbing behavior mobbed in a mean of 38% (median = 33%) of mobbing opportunities

round(mean(filter(hyenas.mob, prop.mob == 0)$tot.mobs), dig = 1)     #4.4
round(median(filter(hyenas.mob, prop.mob == 0)$tot.mobs), dig = 1)     #3
round(range(filter(hyenas.mob, prop.mob == 0)$tot.mobs), dig = 1)     #1 44
#hyenas who never mobbed had a mean of 4.4 (median = 3, range = 1-44) opportunities to mob
round(mean(filter(hyenas.mob, prop.mob == 1)$tot.mobs), dig = 1)    #1.8
round(median(filter(hyenas.mob, prop.mob == 1)$tot.mobs), dig = 1)     #1
round(range(filter(hyenas.mob, prop.mob == 1)$tot.mobs), dig = 1)     #1 5
#hyenas who always mobbed had a mean of 1.8 (median = 1, range = 1-5) opportunities to mob
round(mean(filter(hyenas.mob, prop.mob != 0 & prop.mob != 1)$tot.mobs), dig = 1)     #14.8
round(median(filter(hyenas.mob, prop.mob != 0 & prop.mob != 1)$tot.mobs), dig = 1)     #9
round(range(filter(hyenas.mob, prop.mob != 0 & prop.mob != 1)$tot.mobs), dig = 1)     #2 94
#hyenas who sometimes mobbed had a mean of 14.8 (median = 9, range = 2-94) opportunities to mob

#Variation in mobbing by session
hyenas.mob <- data.frame(hyena = unique(id.by.mob$hyena), num.mobs = NA, tot.mobs = NA, num.grts = NA, tot.grts = NA)
for(i in 1:nrow(hyenas.mob)){
  hyena.i <- hyenas.mob$hyena[i]
  hyenas.mob$num.mobs[i] <- length(unique(filter(id.by.mob, hyena == hyena.i & mobber == T)$session))
  hyenas.mob$tot.mobs[i] <- length(unique(filter(id.by.mob, hyena == hyena.i)$session))
}
hyenas.mob$prop.mob <- round(hyenas.mob$num.mobs/hyenas.mob$tot.mobs, dig = 2)

nrow(filter(hyenas.mob, prop.mob == 0))     #189 hyenas never mobbed
nrow(filter(hyenas.mob, prop.mob == 1))     #96 hyenas mobbed at every session-opportunity
nrow(filter(hyenas.mob, prop.mob != 0 & prop.mob != 1))     #207 hyenas mobbed at some but not all sessions
round(mean(filter(hyenas.mob, prop.mob != 0 & prop.mob != 1)$prop.mob), dig = 2)     #0.53
round(median(filter(hyenas.mob, prop.mob != 0 & prop.mob != 1)$prop.mob), dig = 2)    #0.5
#hyenas with variable mobbing behavior mobbed in a mean of 53% (median = 50%) of mobbing sessions

round(mean(filter(hyenas.mob, prop.mob == 0)$tot.mobs), dig = 1)     #1.6
round(median(filter(hyenas.mob, prop.mob == 0)$tot.mobs), dig = 1)     #1
round(range(filter(hyenas.mob, prop.mob == 0)$tot.mobs), dig = 1)     #1 6
#hyenas who never mobbed had a mean of 1.6 (median = 1, range = 1-6) sessions in which to mob
round(mean(filter(hyenas.mob, prop.mob == 1)$tot.mobs), dig = 1)    #1.4
round(median(filter(hyenas.mob, prop.mob == 1)$tot.mobs), dig = 1)     #1
round(range(filter(hyenas.mob, prop.mob == 1)$tot.mobs), dig = 1)     #1 5
#hyenas who always mobbed had a mean of 1.4 (median = 1, range = 1-5) sessions in which to mob
round(mean(filter(hyenas.mob, prop.mob != 0 & prop.mob != 1)$tot.mobs), dig = 1)     #5.6
round(median(filter(hyenas.mob, prop.mob != 0 & prop.mob != 1)$tot.mobs), dig = 1)     #4
round(range(filter(hyenas.mob, prop.mob != 0 & prop.mob != 1)$tot.mobs), dig = 1)     #2 24
#hyenas who sometimes mobbed had a mean of 5.6 (median = 4, range = 2-24) sessions in which to mob


########## 13.4 Choose random effects ##########

##### Only random effects #####

head(id.by.mob[,c(1:8)])
id.by.mob.basic <- filter(id.by.mob, complete.cases(id.by.mob[,c(1:8)]))   #n=4740
summary(id.by.mob.basic)

mod.basic <- glmmTMB(mobber ~ (1|session/mobID) + (1|hyena),
                     data = id.by.mob.basic, family = binomial(link = 'logit'))
icc(mod.basic)
# (1|session)       0.295
# (1|mobID)         0.246
# (1|hyena)         0.309
# (1|clan)          0.022
# (1|session/mobID) 0.320
# (1|clan/hyena)     NA
# (1|session) + (1|mobID) + (1|hyena)  0.574
# (1|session/mobID) + (1|hyena)        0.574
# (1|session/mobID/hyena)               NA


########## 13.5 Data exploration ##########

#Session-level 
table(id.by.mob$mobber)
hist(id.by.mob$date, breaks = 30)    #peak at session with 40 mobs
table(id.by.mob$clan)   #skewed to talek
table(id.by.mob$context)

#Outliers
dotchart(id.by.mob$age)

#Distribution/zeros
hist(id.by.mob$age)
table(id.by.mob$sex)
table(id.by.mob$age.class)

#Collinear X variables
head(id.by.mob[,c(11,14)])
ggpairs(id.by.mob[,c(11,14)])


########## 13.6 Age and sex ##########

##### Create dataset #####

head(id.by.mob[,c(11,14)])
id.by.mob.all <- filter(id.by.mob, complete.cases(id.by.mob[,c(11,14)]))   #n=4383
id.by.mob.all$stan.age <- as.numeric(scale(id.by.mob.all$age, center = T, scale = T))
id.by.mob.all$sex <- as.character(id.by.mob.all$sex)
summary(id.by.mob.all)

##### Global model #####

#Full model
mod.all <- glmmTMB(mobber ~ sex + poly(stan.age, 2) + 
                     (1 | session/mobID) + (1 | hyena), 
                   data = id.by.mob.all, family = binomial(link = 'logit'))
check_collinearity(mod.all)     #all below 3
check_model(mod.all)

#Dredge
options(na.action = "na.fail")
mod.all <- glmmTMB(mobber ~ sex + poly(stan.age, 2) + 
                     (1 | session/mobID) + (1 | hyena), 
                   data = id.by.mob.all, family = binomial(link = 'logit'))
results.all <- dredge(mod.all)
get.models(subset(results.all, delta == 0), subset = T)
# mobber ~ poly(stan.age, 2) + sex + (1 | session/mobID) + (1 | hyena)
importance(subset(results.all, delta <= 6 & !nested(.)))    
#1 model
options(na.action = "na.omit")


########## 13.7 Final model ##########

head(id.by.mob[,c(11,14)])
id.by.mob.all <- filter(id.by.mob, complete.cases(id.by.mob[,c(11,14)]))   #n=4383
id.by.mob.all$stan.age <- as.numeric(scale(id.by.mob.all$age, center = T, scale = T))
summary(id.by.mob.all)

##### Best model #####

mod.all <- glmmTMB(mobber ~ poly(stan.age, 2) + sex + (1 | session/mobID) + (1 | hyena), 
                   data = id.by.mob.all, family = binomial(link = 'logit'))
summary(mod.all)
#                     Estimate Std. Error z value Pr(>|z|)    
# poly(stan.age, 2)1  12.80772    4.20320   3.047  0.00231 ** 
# poly(stan.age, 2)2 -39.73733    5.02086  -7.914 2.48e-15 ***
# sexm                -1.03564    0.16257  -6.370 1.88e-10 ***
check_collinearity(mod.all)    #all below 3
check_model(mod.all)
binned_residuals(mod.all)
# Warning: Probably bad model fit. Only about 62% of the residuals are inside the error bounds.
simulationOutput <- simulateResiduals(fittedModel = mod.all, n = 250)
plot(simulationOutput)   #KS test: p = 0, Dispersion test: p = 0, Outlier test: p = 0.06159
model_performance(mod.all)
# AIC      |      BIC | R2 (cond.) | R2 (marg.) |   ICC |  RMSE | Sigma | Log_loss |
# 4609.329 | 4654.027 |      0.566 |      0.090 | 0.523 | 0.357 | 1.000 |    0.403 |

##### Plot models #####

id.by.mob.all$mobber <- as.factor(id.by.mob.all$mobber)
mod.all <- glmmTMB(mobber ~ poly(stan.age, 2) + sex + (1 | session/mobID) + (1 | hyena), 
                   data = id.by.mob.all, family = binomial(link = 'logit'))
summary(mod.all)

#Plot with age and sex
summary(id.by.mob.all$age)
dat.intx.sex.age <- ggeffects::ggpredict(mod.all, terms = c("stan.age [all]", "sex"), full.data = FALSE)
dat.intx.sex.age$group <- factor(dat.intx.sex.age$group, labels = c("female", "male"))
plot.sex.age <- ggplot(dat.intx.sex.age, aes(x = dat.intx.sex.age$x*sd(id.by.mob.all$age, na.rm = T) + 
                                               mean(id.by.mob.all$age, na.rm = T), 
                                             y = predicted, color = group, fill = group)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = 0.2, linetype = 0) +
  geom_line(size = 1) + 
  xlab('Age') +
  ylab("Predicted probability of mobbing")+
  theme_classic() + 
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 12), 
        axis.title = element_text(size = 20), axis.text = element_text(size = 16), 
        legend.position = "none") +   #"top"
  scale_color_manual(values = viridis_3, name = "Sex", labels = c("Female", "Male")) +
  scale_fill_manual(values = viridis_3, name = "Sex", labels = c("Female", "Male")) +
  ylim(c(0,1))
pdf('13.plot.age.sex.pdf', width = 7, height = 5)
plot.sex.age
dev.off()

#Most likely to mob at this age
round(filter(dat.intx.sex.age, predicted == max(dat.intx.sex.age$predicted))$
        x*sd(id.by.mob.all$age, na.rm = T) + mean(id.by.mob.all$age, na.rm = T), dig = 1)     #7.6
round(range(id.by.mob.all$age), dig = 1)     #0.2 21.2

#Plot model
id.by.mob.all$stan.age.sq <- (id.by.mob.all$stan.age)^2
mod.all <- glmmTMB(mobber ~ stan.age + stan.age.sq + sex + (1 | session/mobID) + (1 | hyena), 
                   data = id.by.mob.all, family = binomial(link = 'logit'))
summary(mod.all)
#             Estimate Std. Error z value Pr(>|z|)    
# stan.age     0.72318    0.08559   8.450  < 2e-16 ***
# stan.age.sq -0.39626    0.05007  -7.915 2.48e-15 ***
# sexm        -1.03566    0.16257  -6.371 1.88e-10 ***
set_theme(base = theme_classic(), axis.textcolor = "black", axis.title.color = "black", 
          axis.textsize.y = 1.5, axis.textsize.x = 1.2, axis.title.size = 1.7)
plot.model <- plot_model(mod.all, type = "est", transform = NULL,
                         axis.labels = c("Sex [male]", "Age^2", "Age"),
                         vline.color = "black", title = "", dot.size = 4.5, line.size = 1.5,
                         show.values = TRUE, show.p = TRUE, digits = 2, value.offset = 0.3, 
                         value.size = 6, colors = viridis_2, axis.lim = c(-2,2))
pdf('13.plot.all.model.pdf', width = 7, height = 5)
plot.model
dev.off()

#Table
sjPlot::tab_model(mod.all, show.se = T, show.ci = F, show.re.var = F, show.intercept = F, 
                  pred.labels = c("Age", "Age^2", "Sex [male]"),
                  dv.labels = c("Probability of mobbing participation"), 
                  string.se = "SE", transform = NULL, file = "13.table.all.doc")






