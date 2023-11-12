######################################################################
##### 9.0 Modeling mobbing participation - session traits #####
######################################################################

########## 9.1 Set working directory & download packages/tables ##########

rm(list = ls())
setwd("~/Documents/R/LionHyena")
options(stringsAsFactors = FALSE)
library(GGally)
library(glmmTMB)
library(MuMIn)
library(DHARMa)
library(see)
library(performance)
library(broom.mixed)
library(viridis)
library(patchwork)
library(sjPlot)
library(tidyverse)
hyenadata::update_tables("1.2.88")
library(hyenadata)

#Load data
load("08.sessions.intx.Rdata")

#Set plot colors
viridis_2 <- viridis(7)[-c(1,3,5,6,7)]
viridis_3 <- viridis(7)[-c(2,4,6,7)]


########## 9.2 Descriptive statistics for mobbing ##########

#How many interaction sessions with mobbing?
nrow(sessions.intx)     #325
round(nrow(filter(sessions.intx, mobbing == T))/nrow(sessions.intx), dig = 3)     #0.418

#Proportion of interaction sessions with different contexts in which there was mobbing
round(nrow(filter(sessions.intx, mobbing == T & context == "fd"))/
        nrow(filter(sessions.intx, context == "fd")), dig = 2)     # 0.44
round(nrow(filter(sessions.intx, mobbing == T & context == "den"))/
        nrow(filter(sessions.intx, context == "den")), dig = 2)     #0.35
round(nrow(filter(sessions.intx, mobbing == T & context == "other"))/
        nrow(filter(sessions.intx, context == "other")), dig = 2)     #0.39

#Calculate number of sessions with mobbing per month
sessions.mob <- filter(sessions.intx, mobbing == T)
sessions.mob$year <- format(sessions.mob$date, "%Y")
sessions.mob$month <- format(sessions.mob$date, "%m")
count <- sessions.mob %>% group_by(year, month) %>% summarise(num.month = length(month))
mean(count$num.month)     #1.333333
median(count$num.month)     #1
min(count$num.month)     #1
max(count$num.month)     #3
rm(count)

#Number of mobs per session
round(mean(sessions.mob$num_mobs), dig = 1)     #3.1
median(sessions.mob$num_mobs)     #2
min(sessions.mob$num_mobs)     #1
max(sessions.mob$num_mobs)     #40

#Mob size
nrow(filter(sessions.mob, is.na(size_mobs_mean)))    #16 sessions with unknown size mobs
round(mean(filter(sessions.mob, !is.na(size_mobs_mean))$size_mobs_mean), dig = 1)     #5.1
median(filter(sessions.mob, !is.na(size_mobs_median))$size_mobs_median)     #4
min(filter(sessions.mob, !is.na(size_mobs_mean))$size_mobs_mean)     #2
round(max(filter(sessions.mob, !is.na(size_mobs_mean))$size_mobs_mean), dig = 0)     #16
rm(sessions.mob)

#Number of lions at sessions with and without mobbing
round(median(filter(sessions.intx, mobbing == F)$total_lions), dig = 0)    #2
round(mean(filter(sessions.intx, mobbing == F)$total_lions), dig = 1)    #3.4
min(filter(sessions.intx, mobbing == F)$total_lions)    #1
max(filter(sessions.intx, mobbing == F)$total_lions)    #20
round(median(filter(sessions.intx, mobbing == T)$total_lions), dig = 0)    #2
round(mean(filter(sessions.intx, mobbing == T)$total_lions), dig = 1)    #3.7
min(filter(sessions.intx, mobbing == T)$total_lions)    #1
max(filter(sessions.intx, mobbing == T)$total_lions)    #14


########## 9.4 Look at correlated variables ##########

#Compare correlated variables - prey density
ggpairs(sessions.intx[,c(3:5)])
mod.prob.mob <- glmmTMB(mobbing ~ prey_density,
                        data = filter(sessions.intx, !is.na(num_sd_year)), family = binomial(link = 'logit'))
AIC(mod.prob.mob)     #439.4525
mod.prob.mob <- glmmTMB(mobbing ~ num_sd_year,
                        data = filter(sessions.intx, !is.na(num_sd_year)), family = binomial(link = 'logit'))
AIC(mod.prob.mob)     #437.6313 - pick this
mod.prob.mob <- glmmTMB(mobbing ~ migration,
                        data = filter(sessions.intx, !is.na(num_sd_year)), family = binomial(link = 'logit'))
AIC(mod.prob.mob)     #439.1021

#Compare correlated variables - session context
ggpairs(sessions.intx[,c(6,7)])
mod.prob.mob <- glmmTMB(mobbing ~ context,
                        data = sessions.intx, family = binomial(link = 'logit'))
AIC(mod.prob.mob)     #446.8876 - pick this
mod.prob.mob <- glmmTMB(mobbing ~ location,
                        data = sessions.intx, family = binomial(link = 'logit'))
AIC(mod.prob.mob)     #448.4671

#Compare correlated variables - male lions
ggpairs(sessions.intx[,c(14:15)])
mod.prob.mob <- glmmTMB(mobbing ~ male_lions_present,
                        data = sessions.intx, family = binomial(link = 'logit'))
AIC(mod.prob.mob)     #440.9956 - pick this
mod.prob.mob <- glmmTMB(mobbing ~ total_male_lions,
                        data = sessions.intx, family = binomial(link = 'logit'))
AIC(mod.prob.mob)     #443.5921

#Compare correlated variables - food
ggpairs(sessions.intx[,c(17:18)])
mod.prob.mob <- glmmTMB(mobbing ~ food_present,
                        data = filter(sessions.intx, !is.na(carc_cat)), family = binomial(link = 'logit'))
AIC(mod.prob.mob)     #393.3245 - pick this
mod.prob.mob <- glmmTMB(mobbing ~ carc_cat,
                        data = filter(sessions.intx, !is.na(carc_cat)), family = binomial(link = 'logit'))
AIC(mod.prob.mob)     #399.2552

#Compare correlated variables - behavior
sessions.intx$prop_indv_agg <- sessions.intx$num_indv_agg/sessions.intx$hyena_count
sessions.intx$prop_indv_grts <- sessions.intx$num_indv_grts/sessions.intx$hyena_count

ggpairs(sessions.intx[,c(19:22)])

#aggressions
mod.prob.mob <- glmmTMB(mobbing ~ agg_occ,
                        data = sessions.intx, family = binomial(link = 'logit'))
AIC(mod.prob.mob)     #416.3243
mod.prob.mob <- glmmTMB(mobbing ~ num_indv_agg,
                        data = sessions.intx, family = binomial(link = 'logit'))
AIC(mod.prob.mob)     #410.9453 - pick this
mod.prob.mob <- glmmTMB(mobbing ~ prop_indv_agg,
                        data = sessions.intx, family = binomial(link = 'logit'))
AIC(mod.prob.mob)     #438.3415
#greetings
mod.prob.mob <- glmmTMB(mobbing ~ grts_occ,
                        data = sessions.intx, family = binomial(link = 'logit'))
AIC(mod.prob.mob)     #411.8934
mod.prob.mob <- glmmTMB(mobbing ~ num_indv_grts,
                        data = sessions.intx, family = binomial(link = 'logit'))
AIC(mod.prob.mob)     #403.7926 - pick this
mod.prob.mob <- glmmTMB(mobbing ~ prop_indv_grts,
                        data = sessions.intx, family = binomial(link = 'logit'))
AIC(mod.prob.mob)     #428.3808
# proportion also doesn't make sense for interactions (esp. with num_hyenas)

#Look at random effect
summary(as.factor(sessions.intx$clan))
mod.prob.mob <- glmmTMB(mobbing ~ (1|clan),
                        data = sessions.intx, family = binomial(link = 'logit'))
icc(mod.prob.mob)   #NA


########## 9.5 Look at data distribution ##########

#Outliers
dotchart(sessions.intx$num_sd_year)
dotchart(sessions.intx$session_length)
dotchart(sessions.intx$hyena_count)
dotchart(sessions.intx$total_lions)
dotchart(sessions.intx$num_indv_grts)
dotchart(sessions.intx$session.ai.avg)

#Distribution/zeros - everything is skewed left
hist(sessions.intx$num_sd_year)
hist(sessions.intx$session_length)
hist(sessions.intx$hyena_count)
hist(sessions.intx$total_lions)
hist(sessions.intx$num_indv_grts)
hist(sessions.intx$session.ai.avg)
table(sessions.intx$context)
table(sessions.intx$male_lions_present)
table(sessions.intx$food_present)
# don't include food_present - nearly 100% overlap with context

#Collinear X variables
ggpairs(sessions.intx[,c(4,11,12,13,20,23)])


########## 9.6 Probability of mobbing - additive ##########

##### Global model #####

#Standardize model variables
head(sessions.intx[,c(4,7,11,12,13,15,20,23)])
sessions.intx.tmp <- filter(sessions.intx, complete.cases(sessions.intx[,c(4,7,11,12,13,15,20,23)]))   #n=311
sessions.intx.tmp$stan.num_sd_year <- as.numeric(scale(sessions.intx.tmp$num_sd_year, center = T, scale = T))
sessions.intx.tmp$stan.session_length <- as.numeric(scale(sessions.intx.tmp$session_length, center = T, scale = T))
sessions.intx.tmp$stan.hyena_count <- as.numeric(scale(sessions.intx.tmp$hyena_count, center = T, scale = T))
sessions.intx.tmp$stan.total_lions <- as.numeric(scale(sessions.intx.tmp$total_lions, center = T, scale = T))
sessions.intx.tmp$stan.num_indv_grts <- as.numeric(scale(sessions.intx.tmp$num_indv_grts, center = T, scale = T))
sessions.intx.tmp$stan.session.ai.avg <- as.numeric(scale(sessions.intx.tmp$session.ai.avg, center = T, scale = T))
summary(sessions.intx.tmp)

#Full model
mod.prob.mob <- glmmTMB(mobbing ~ stan.session_length + context + stan.num_sd_year + 
                          stan.hyena_count + stan.total_lions + male_lions_present + 
                          stan.num_indv_grts + stan.session.ai.avg,
                        data = sessions.intx.tmp, family = binomial(link = 'logit'))
check_collinearity(mod.prob.mob)     #all below 3
check_model(mod.prob.mob)

#Dredge
options(na.action = "na.fail")
mod.prob.mob <- glmmTMB(mobbing ~ stan.session_length + context + stan.num_sd_year + 
                          stan.hyena_count + stan.total_lions + male_lions_present + 
                          stan.num_indv_grts + stan.session.ai.avg,
                        data = sessions.intx.tmp, family = binomial(link = 'logit'))
results.prob.mob <- dredge(mod.prob.mob)
get.models(subset(results.prob.mob, delta == 0), subset = T)
# mobbing ~ male_lions_present + stan.hyena_count + stan.num_indv_grts + stan.num_sd_year
importance(subset(results.prob.mob, delta <= 6 & !nested(.)))    
#                      cond(stan.hyena_count) cond(stan.num_indv_grts) cond(male_lions_present) cond(stan.num_sd_year)
# Sum of weights:      1.00                   1.00                     0.85                     0.85                  
# N containing models:    3                      3                        2                        2                  
options(na.action = "na.omit")


########## 9.7 Probability of mobbing - interactions ##########

##### Global model #####

#Full model
mod.prob.mob <- glmmTMB(mobbing ~ stan.session_length + context + stan.num_sd_year + 
                          stan.hyena_count + stan.total_lions + male_lions_present + 
                          stan.num_indv_grts + stan.session.ai.avg + 
                          stan.session_length * stan.hyena_count + 
                          stan.session_length * stan.num_indv_grts + 
                          stan.hyena_count * stan.total_lions + 
                          stan.hyena_count * male_lions_present + 
                          stan.hyena_count * stan.num_indv_grts + 
                          stan.hyena_count * stan.session.ai.avg + 
                          stan.total_lions * stan.num_indv_grts + 
                          stan.total_lions * stan.session.ai.avg + 
                          male_lions_present * stan.num_indv_grts + 
                          male_lions_present * stan.session.ai.avg,
                        data = sessions.intx.tmp, family = binomial(link = 'logit'))
check_collinearity(mod.prob.mob)     #all <= 3
check_model(mod.prob.mob)

#Dredge
options(na.action = "na.fail")
mod.prob.mob <- glmmTMB(mobbing ~ stan.session_length + context + stan.num_sd_year + 
                          stan.hyena_count + stan.total_lions + male_lions_present + 
                          stan.num_indv_grts + stan.session.ai.avg + 
                          stan.session_length * stan.hyena_count + 
                          stan.session_length * stan.num_indv_grts + 
                          stan.hyena_count * stan.total_lions + 
                          stan.hyena_count * male_lions_present + 
                          stan.hyena_count * stan.num_indv_grts + 
                          stan.hyena_count * stan.session.ai.avg + 
                          stan.total_lions * stan.num_indv_grts + 
                          stan.total_lions * stan.session.ai.avg + 
                          male_lions_present * stan.num_indv_grts + 
                          male_lions_present * stan.session.ai.avg,
                        data = sessions.intx.tmp, family = binomial(link = 'logit'))
results.prob.mob <- dredge(mod.prob.mob)
get.models(subset(results.prob.mob, delta == 0), subset = T)
# mobbing ~ male_lions_present + stan.hyena_count + stan.num_indv_grts + stan.num_sd_year +  
#   male_lions_present:stan.num_indv_grts + stan.hyena_count:stan.num_indv_grts
importance(subset(results.prob.mob, delta <= 6 & !nested(.)))   
# cond(stan.hyena_count) cond(stan.num_indv_grts) cond(male_lions_present) cond(stan.num_sd_year)
# Sum of weights:      1.00                   1.00                     0.96                     0.85                  
# N containing models:   13                     13                       11                        8                  
# cond(stan.hyena_count:stan.num_indv_grts) cond(male_lions_present:stan.num_indv_grts)
# Sum of weights:      0.67                                      0.45                                       
# N containing models:    7                                         4                                       
# cond(male_lions_present:stan.hyena_count)
# Sum of weights:      0.28                                     
# N containing models:    3                                     
importance(subset(results.prob.mob, delta <= 2 & !nested(.)))   
# cond(male_lions_present) cond(stan.hyena_count) cond(stan.num_indv_grts) cond(stan.num_sd_year)
# Sum of weights:      1.00                     1.00                   1.00                     1.00                  
# N containing models:    4                        4                      4                        4                  
# cond(stan.hyena_count:stan.num_indv_grts) cond(male_lions_present:stan.num_indv_grts)
# Sum of weights:      0.80                                      0.56                                       
# N containing models:    3                                         2                                       
# cond(male_lions_present:stan.hyena_count)
# Sum of weights:      0.27                                     
# N containing models:    1                                     
options(na.action = "na.omit")


########## 9.8 Probability of mobbing - best model ##########

##### Best model #####

#Standardize model variables
head(sessions.intx[,c(1,4,12,15,20)])
sessions.intx.final <- filter(sessions.intx, complete.cases(sessions.intx[,c(4,12,15,20)]))
sessions.intx.final$stan.hyena_count <- as.numeric(scale(sessions.intx.final$hyena_count, center = T, scale = T))
sessions.intx.final$stan.num_sd_year <- as.numeric(scale(sessions.intx.final$num_sd_year, center = T, scale = T))
sessions.intx.final$stan.num_indv_grts <- as.numeric(scale(sessions.intx.final$num_indv_grts, center = T, scale = T))
summary(sessions.intx.final)

mod.prob.mob <- glmmTMB(mobbing ~ male_lions_present + stan.hyena_count + stan.num_indv_grts +  
                          stan.num_sd_year + male_lions_present:stan.num_indv_grts + 
                          stan.hyena_count:stan.num_indv_grts,
                        data = sessions.intx.final, family = binomial(link = 'logit'))
summary(mod.prob.mob)
#                                          Estimate Std. Error z value Pr(>|z|)    
# male_lions_presentTRUE                    -0.72836    0.29525  -2.467   0.0136 *  
# stan.hyena_count                           0.87157    0.16770   5.197 2.02e-07 ***
# stan.num_indv_grts                         0.52709    0.22028   2.393   0.0167 *  
# stan.num_sd_year                           0.27403    0.13194   2.077   0.0378 *  
# male_lions_presentTRUE:stan.num_indv_grts  0.66571    0.35233   1.889   0.0588 .  
# stan.hyena_count:stan.num_indv_grts       -0.31681    0.16904  -1.874   0.0609 .  
check_collinearity(mod.prob.mob)     #all below 3
check_model(mod.prob.mob)
simulationOutput <- simulateResiduals(fittedModel = mod.prob.mob, n = 250)
plot(simulationOutput)
model_performance(mod.prob.mob)
# AIC     |    AICc |     BIC | R2 (cond.) | R2 (marg.) |  RMSE | Sigma | Log_loss | Score_log | Score_spherical
# --------------------------------------------------------------------------------------------------------------
# 356.533 | 356.890 | 382.933 |            |      0.348 | 0.425 | 1.000 |    0.534 |   -87.496 |           0.012
round(r2_tjur(mod.prob.mob), digits = 3)     #0.261


##### Plot model #####

sessions.intx.final$mobbing <- as.factor(sessions.intx.final$mobbing)
sessions.intx.final$male_lions_present <- as.factor(sessions.intx.final$male_lions_present)
mod.prob.mob <- glmmTMB(mobbing ~ male_lions_present + stan.hyena_count + stan.num_indv_grts +  
                          stan.num_sd_year + male_lions_present:stan.num_indv_grts + 
                          stan.hyena_count:stan.num_indv_grts,
                        data = sessions.intx.final, family = binomial(link = 'logit'))
summary(mod.prob.mob)

#Interaction between number of hyenas who greet and number of hyenas
dat.intx.hy.grt <- ggeffects::ggpredict(mod.prob.mob, terms = c("stan.hyena_count [all]", "stan.num_indv_grts"), full.data = FALSE)
plot.hy.grt <- ggplot(dat.intx.hy.grt, aes(x = dat.intx.hy.grt$x*sd(sessions.intx.final$hyena_count, na.rm = T) + 
                                             mean(sessions.intx.final$hyena_count, na.rm = T), 
                                           y = predicted, color = group)) + 
  geom_ribbon(aes(x = dat.intx.hy.grt$x*sd(sessions.intx.final$hyena_count, na.rm = T) + mean(sessions.intx.final$hyena_count, na.rm = T), 
                  ymin = conf.low, ymax = conf.high, fill = dat.intx.hy.grt$group), alpha = 0.2, inherit.aes = FALSE) +
  geom_line(size = 1) + 
  xlab('Number of hyenas present') +
  ylab("Predicted probability of mobbing")+
  theme_classic() + 
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 12), 
        axis.title = element_text(size = 20), axis.text = element_text(size = 16), 
        legend.position = "none") +   #"right"
  scale_color_manual(values = viridis_3, name = "Number of hyenas who greet", labels = c("Few", "Average", "Many")) +
  scale_fill_manual(values = viridis_3, name = "Number of hyenas who greet", labels = c("Few", "Average", "Many")) +
  ylim(c(0,1))
pdf('09.plot.grts.hyenas.pdf', width = 7, height = 5)
plot.hy.grt
dev.off()

#Interaction between male lions and number of hyenas who greet
summary(sessions.intx.final$num_indv_grts)
dat.intx.grt.ml <- ggeffects::ggpredict(mod.prob.mob, type = "fe", terms = c("male_lions_present", "stan.num_indv_grts"), full.data = FALSE)
dat.intx.grt.ml <- arrange(dat.intx.grt.ml, x, group)
dat.intx.grt.ml$x2 <- c(0.9, 1, 1.1, 1.9, 2, 2.1)
plot.grt.ml <- ggplot(dat.intx.grt.ml, aes(x = x2, y = predicted, color = as.character(group))) + 
  geom_point(size = 5) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, size = 1.5) +
  xlab('Male lions present') +
  ylab("Predicted probability of mobbing")+
  theme_classic() + 
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 12), 
        axis.title = element_text(size = 20), axis.text = element_text(size = 16), 
        legend.position = "none") +   #"top"
  scale_color_manual(values = viridis_3, name = "Number of hyenas who greet", labels = c("Few", "Average", "Many")) +
  scale_fill_manual(values = viridis_3, name = "Number of hyenas who greet", labels = c("Few", "Average", "Many")) +
  scale_x_continuous(breaks = c(1, 2), limits = c(0.6, 2.4), label = c("FALSE", "TRUE")) +
  ylim(c(0,1))
pdf('09.plot.grts.male.lions.pdf', width = 7, height = 5)
plot.grt.ml
dev.off()

#Prey density
summary(sessions.intx.final$num_sd_year)
dat.prey <- ggeffects::ggpredict(mod.prob.mob, terms = c("stan.num_sd_year [all]"), full.data = FALSE)
plot.prey <- ggplot(dat.prey, aes(x = dat.prey$x*sd(sessions.intx.final$num_sd_year, na.rm = T) + mean(sessions.intx.final$num_sd_year, na.rm = T),
                                  y = predicted)) +
  geom_line() +
  geom_ribbon(aes(x = dat.prey$x*sd(sessions.intx.final$num_sd_year, na.rm = T) + mean(sessions.intx.final$num_sd_year, na.rm = T),
                  ymin = conf.low, ymax = conf.high), alpha = 0.2, inherit.aes = FALSE) +
  xlab('Prey density') +
  ylab("Predicted probability of mobbing")+
  theme_classic() +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12),
        axis.title = element_text(size = 20), axis.text = element_text(size = 16)) +
  ylim(c(0,1))
pdf('09.plot.prey.pdf', width = 7, height = 5)
plot.prey
dev.off()

#Number of individuals who greet
summary(sessions.intx.final$num_indv_grts)
dat.grt <- ggeffects::ggpredict(mod.prob.mob, terms = c("stan.num_indv_grts [all]"), full.data = FALSE)
plot.grt <- ggplot(dat.grt, aes(x = dat.grt$x*sd(sessions.intx.final$num_indv_grts, na.rm = T) + mean(sessions.intx.final$num_indv_grts, na.rm = T),
                                y = predicted)) +
  geom_line() +
  geom_ribbon(aes(x = dat.grt$x*sd(sessions.intx.final$num_indv_grts, na.rm = T) + mean(sessions.intx.final$num_indv_grts, na.rm = T),
                  ymin = conf.low, ymax = conf.high), alpha = 0.2, inherit.aes = FALSE) +
  xlab('Number of greeters') +
  ylab("Predicted probability of mobbing")+
  theme_classic() +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12),
        axis.title = element_text(size = 20), axis.text = element_text(size = 16), 
        axis.title.y = element_blank()) +
  ylim(c(0,1))
pdf('09.plot.grts.pdf', width = 7, height = 5)
plot.grt
dev.off()

#Number of hyenas present
summary(sessions.intx.final$hyena_count)
dat.hy <- ggeffects::ggpredict(mod.prob.mob, terms = c("stan.hyena_count [all]"), full.data = FALSE)
plot.hy <- ggplot(dat.hy, aes(x = dat.hy$x*sd(sessions.intx.final$hyena_count, na.rm = T) + mean(sessions.intx.final$hyena_count, na.rm = T),
                              y = predicted)) +
  geom_line() +
  geom_ribbon(aes(x = dat.hy$x*sd(sessions.intx.final$hyena_count, na.rm = T) + mean(sessions.intx.final$hyena_count, na.rm = T),
                  ymin = conf.low, ymax = conf.high), alpha = 0.2, inherit.aes = FALSE) +
  xlab('Number of hyenas') +
  ylab("Predicted probability of mobbing")+
  theme_classic() +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12),
        axis.title = element_text(size = 20), axis.text = element_text(size = 16)) +
  ylim(c(0,1))
pdf('09.plot.hyenas.pdf', width = 7, height = 5)
plot.hy
dev.off()

#Male lions present
dat.lions <- ggeffects::ggpredict(mod.prob.mob, type = "fe", terms = c("male_lions_present"), full.data = FALSE)
plot.lions <- ggplot(dat.lions, aes(x = x, y = predicted)) + 
  geom_point(size = 5) + 
  geom_line(data = data.frame(x = c(dat.lions$x, dat.lions$x),
                              predicted = c(dat.lions$predicted, dat.lions$predicted),
                              ci = c(dat.lions$conf.low, dat.lions$conf.high)),
            aes(x = x, y = ci, group= x), size = 1.5)+
  xlab('Male lions present') +
  ylab("Predicted probability of mobbing")+
  theme_classic() + 
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        axis.title = element_text(size = 20), axis.text = element_text(size = 16), 
        axis.title.y = element_blank())+
  ylim(c(0,1))
pdf('09.plot.lions.pdf', width = 7, height = 5)
plot.lions
dev.off()

#Plot of single variables
layout <- "AB
           CD"
plots <- plot.hy + plot.lions + plot.prey + plot.grt + plot_layout()
pdf('09.plots.all.pdf', width = 7, height = 5)
plots
dev.off()

#Plot model
mod.prob.mob <- glmmTMB(mobbing ~ stan.hyena_count + male_lions_present + stan.num_sd_year + #rerun to reorder terms
                          stan.num_indv_grts + stan.hyena_count:stan.num_indv_grts +
                          male_lions_present:stan.num_indv_grts,
                        data = sessions.intx.final, family = binomial(link = 'logit'))
summary(mod.prob.mob)
set_theme(base = theme_classic(), axis.textcolor = "black", axis.title.color = "black", 
          axis.textsize.y = 1.5, axis.textsize.x = 1.2, axis.title.size = 1.7)
plot.model.intx <- plot_model(mod.prob.mob, type = "est", transform = "exp",
                              axis.labels = c("Male lions present x Number of greeters",
                                              "Number of hyenas present x Number of greeters",
                                              "Number of hyenas who greet (greeters)", "Prey density",
                                              "Male lions present [TRUE]", "Number of hyenas present"),
                              vline.color = "black", title = "", dot.size = 4.5, line.size = 1.5,
                              show.values = TRUE, show.p = TRUE, digits = 2, value.offset = 0.3, 
                              value.size = 6, colors = viridis_2, axis.lim = c(0.1,10))
pdf('09.plot.session.model.pdf', width = 7, height = 5)
plot.model.intx
dev.off()

#Make table
sjPlot::tab_model(mod.prob.mob, show.intercept = F, show.se = T, show.ci = 0.95,
                  pred.labels = c("Number of hyenas present", "Male lions present [TRUE]",
                                  "Prey density", "Number of hyenas who greet (greeters)",
                                  "Number of hyenas present x Number of greeters",
                                  "Male lions present x Number of greeters"),
                  dv.labels = c("Probability of mobbing occurrence"), 
                  string.se = "SE", transform = "exp", file = "09.table_session.doc")

#Calculate CIs using the likelihood profile
tidy.mod.prob.mob <- broom.mixed::tidy(mod.prob.mob, conf.method = "profile", 
                                       conf.int = T, conf.level = 0.95,exponentiate = T)
tidy.mod.prob.mob
#   effect component term                                      estimate std.error statistic     p.value conf.low conf.high
# 2 fixed  cond      stan.hyena_count                             2.39      0.401     5.20  0.000000202    1.74      3.36 
# 3 fixed  cond      male_lions_presentTRUE                       0.483     0.143    -2.47  0.0136         0.267     0.854
# 4 fixed  cond      stan.num_sd_year                             1.32      0.174     2.08  0.0378         1.02      1.71 
# 5 fixed  cond      stan.num_indv_grts                           1.69      0.373     2.39  0.0167         1.11      2.64 
# 6 fixed  cond      stan.hyena_count:stan.num_indv_grts          0.728     0.123    -1.87  0.0609         0.527     1.03 
# 7 fixed  cond      male_lions_presentTRUE:stan.num_indv_grts    1.95      0.686     1.89  0.0588         1.01      4.07 


##### Check plots #####

plot_model(mod.prob.mob, type = "pred", terms = c("stan.num_indv_grts", "male_lions_present"))
plot_model(mod.prob.mob, type = "pred", terms = c("male_lions_present", "stan.num_indv_grts"))

plot_model(mod.prob.mob, type = "pred", terms = c("stan.num_indv_grts", "stan.hyena_count"))
plot_model(mod.prob.mob, type = "pred", terms = c("stan.hyena_count", "stan.num_indv_grts"))


########## 9.9 Probability of mobbing - no adult male lions ##########

##### Create dataset #####

#Filter to no adult male lions
sessions.intx.fem <- filter(sessions.intx, male_lions_present == F)      #212 sessions

#Standardize model variables
sessions.intx.fem$stan.session_length <- as.numeric(scale(sessions.intx.fem$session_length, center = T, scale = T))
sessions.intx.fem$stan.hyena_count <- as.numeric(scale(sessions.intx.fem$hyena_count, center = T, scale = T))
sessions.intx.fem$stan.total_lions <- as.numeric(scale(sessions.intx.fem$total_lions, center = T, scale = T))
sessions.intx.fem$stan.num_sd_year <- as.numeric(scale(sessions.intx.fem$num_sd_year, center = T, scale = T))
sessions.intx.fem$stan.num_indv_grts <- as.numeric(scale(sessions.intx.fem$num_indv_grts, center = T, scale = T))
sessions.intx.fem$stan.session.ai.avg <- as.numeric(scale(sessions.intx.fem$session.ai.avg, center = T, scale = T))
summary(sessions.intx.fem)

##### Global model #####

#Full model
mod.prob.mob <- glmmTMB(mobbing ~ stan.session_length + context + stan.num_sd_year + 
                          stan.hyena_count + stan.total_lions + 
                          stan.num_indv_grts + stan.session.ai.avg + 
                          stan.session_length * stan.hyena_count + 
                          stan.session_length * stan.num_indv_grts + 
                          stan.hyena_count * stan.total_lions + 
                          stan.hyena_count * stan.num_indv_grts + 
                          stan.hyena_count * stan.session.ai.avg + 
                          stan.total_lions * stan.num_indv_grts + 
                          stan.total_lions * stan.session.ai.avg,
                        data = sessions.intx.fem, family = binomial(link = 'logit'))
check_model(mod.prob.mob)

#Dredge
options(na.action = "na.fail")
tmp <- filter(sessions.intx.fem, !is.na(stan.num_sd_year) & !is.na(stan.session.ai.avg))
mod.prob.mob <- glmmTMB(mobbing ~ stan.session_length + context + stan.num_sd_year + 
                          stan.hyena_count + stan.total_lions + 
                          stan.num_indv_grts + stan.session.ai.avg + 
                          stan.session_length * stan.hyena_count + 
                          stan.session_length * stan.num_indv_grts + 
                          stan.hyena_count * stan.total_lions + 
                          stan.hyena_count * stan.num_indv_grts + 
                          stan.hyena_count * stan.session.ai.avg + 
                          stan.total_lions * stan.num_indv_grts + 
                          stan.total_lions * stan.session.ai.avg,
                        data = tmp, family = binomial(link = 'logit'))
results.prob.mob <- dredge(mod.prob.mob)
get.models(subset(results.prob.mob, delta == 0), subset = T)
# mobbing ~ stan.hyena_count + stan.num_indv_grts + stan.hyena_count:stan.num_indv_grts
importance(subset(results.prob.mob, delta <= 6 & !nested(.)))   
#                      cond(stan.hyena_count) cond(stan.num_indv_grts) cond(stan.hyena_count:stan.num_indv_grts)
# Sum of weights:      1.00                   0.90                     0.62                                     
# N containing models:    3                      2                        1                                     
options(na.action = "na.omit")

##### Best model #####

#Standardize model variables
head(sessions.intx.fem[,c(1,12,20)])
sessions.intx.fem.final <- filter(sessions.intx.fem, complete.cases(sessions.intx.fem[,c(1,12,20)]))     #212
sessions.intx.fem.final$stan.hyena_count <- as.numeric(scale(sessions.intx.fem.final$hyena_count, center = T, scale = T))
sessions.intx.fem.final$stan.num_indv_grts <- as.numeric(scale(sessions.intx.fem.final$num_indv_grts, center = T, scale = T))
summary(sessions.intx.fem.final)

mod.prob.mob <- glmmTMB(mobbing ~ stan.hyena_count + stan.num_indv_grts + 
                          stan.hyena_count:stan.num_indv_grts,
                        data = sessions.intx.fem.final, family = binomial(link = 'logit'))
summary(mod.prob.mob)
#                                     Estimate Std. Error z value Pr(>|z|)   
# stan.hyena_count                     0.68893    0.19315   3.567 0.000361 ***
# stan.num_indv_grts                   0.65717    0.26088   2.519 0.011767 *  
# stan.hyena_count:stan.num_indv_grts -0.40514    0.20433  -1.983 0.047391 *  
check_collinearity(mod.prob.mob)     #all below 3
check_model(mod.prob.mob)
simulationOutput <- simulateResiduals(fittedModel = mod.prob.mob, n = 250)
plot(simulationOutput)   
model_performance(mod.prob.mob)
# AIC     |    AICc |     BIC | R2 (cond.) | R2 (marg.) |  RMSE | Sigma | Log_loss | Score_log | Score_spherical
# --------------------------------------------------------------------------------------------------------------
# 257.981 | 258.174 | 271.407 |            |      0.234 | 0.450 | 1.000 |    0.590 |   -68.959 |           0.010
round(r2_tjur(mod.prob.mob), digits = 3)     #0.188

#Make table
sjPlot::tab_model(mod.prob.mob, show.se = T, show.ci = 0.95, show.re.var = F, show.intercept = F, 
                  pred.labels = c("Number of hyenas present", "Number of hyenas who greet (greeters)",
                                  "Number of hyenas present x Number of greeters"),
                  dv.labels = c("Probability of mobbing occurrence"), 
                  string.se = "SE", transform = "exp", file = "09.table_session_femlions.doc")

#Calculate CIs using the likelihood profile
tidy.mod.prob.mob <- broom.mixed::tidy(mod.prob.mob, conf.method = "profile", 
                                       conf.int = T, conf.level = 0.95,exponentiate = T)
tidy.mod.prob.mob
#   effect component term                                estimate std.error statistic     p.value conf.low conf.high
# 2 fixed  cond      stan.hyena_count                       1.99      0.385     3.57  0.000361    1.38       2.95
# 3 fixed  cond      stan.num_indv_grts                     1.93      0.503     2.52  0.0118      1.18       3.29
# 4 fixed  cond      stan.hyena_count:stan.num_indv_grts    0.667     0.136    -1.98  0.0474      0.447      1.01


########## 9.10 Probability of mobbing - with adult male lions ##########

##### Create dataset #####

#Filter to only sessions with adult male lions present
sessions.intx.male <- filter(sessions.intx, male_lions_present == T)      #113 sessions

#Standardize model variables
sessions.intx.male$stan.session_length <- as.numeric(scale(sessions.intx.male$session_length, center = T, scale = T))
sessions.intx.male$stan.hyena_count <- as.numeric(scale(sessions.intx.male$hyena_count, center = T, scale = T))
sessions.intx.male$stan.total_lions <- as.numeric(scale(sessions.intx.male$total_lions, center = T, scale = T))
sessions.intx.male$stan.total_male_lions <- as.numeric(scale(sessions.intx.male$total_male_lions, center = T, scale = T))
sessions.intx.male$stan.num_sd_year <- as.numeric(scale(sessions.intx.male$num_sd_year, center = T, scale = T))
sessions.intx.male$stan.num_indv_grts <- as.numeric(scale(sessions.intx.male$num_indv_grts, center = T, scale = T))
sessions.intx.male$stan.session.ai.avg <- as.numeric(scale(sessions.intx.male$session.ai.avg, center = T, scale = T))
summary(sessions.intx.male)

##### Global model #####

#Full model
mod.prob.mob <- glmmTMB(mobbing ~ stan.session_length + context + stan.num_sd_year + 
                          stan.hyena_count + stan.total_lions + stan.total_male_lions + 
                          stan.num_indv_grts + stan.session.ai.avg + 
                          stan.session_length * stan.hyena_count + 
                          stan.session_length * stan.num_indv_grts + 
                          stan.hyena_count * stan.total_lions + 
                          stan.hyena_count * stan.total_male_lions + 
                          stan.hyena_count * stan.num_indv_grts + 
                          stan.hyena_count * stan.session.ai.avg + 
                          stan.total_lions * stan.num_indv_grts + 
                          stan.total_lions * stan.session.ai.avg + 
                          stan.total_male_lions * stan.num_indv_grts + 
                          stan.total_male_lions * stan.session.ai.avg,
                        data = sessions.intx.male, family = binomial(link = 'logit'))
check_model(mod.prob.mob)

#Dredge
options(na.action = "na.fail")
tmp <- filter(sessions.intx.male, !is.na(stan.session.ai.avg) & !is.na(stan.num_sd_year))     #112 sessions
mod.prob.mob <- glmmTMB(mobbing ~ stan.session_length + context + stan.num_sd_year + 
                          stan.hyena_count + stan.total_lions + stan.total_male_lions + 
                          stan.num_indv_grts + stan.session.ai.avg + 
                          stan.session_length * stan.hyena_count + 
                          stan.session_length * stan.num_indv_grts + 
                          stan.hyena_count * stan.total_lions + 
                          stan.hyena_count * stan.total_male_lions + 
                          stan.hyena_count * stan.num_indv_grts + 
                          stan.hyena_count * stan.session.ai.avg + 
                          stan.total_lions * stan.num_indv_grts + 
                          stan.total_lions * stan.session.ai.avg + 
                          stan.total_male_lions * stan.num_indv_grts + 
                          stan.total_male_lions * stan.session.ai.avg,
                        data = tmp, family = binomial(link = 'logit'))
results.prob.mob <- dredge(mod.prob.mob)
get.models(subset(results.prob.mob, delta == 0), subset = T)
# mobbing ~ stan.hyena_count + stan.num_indv_grts + stan.num_sd_year
importance(subset(results.prob.mob, delta <= 6 & !nested(.)))   
#               cond(stan.hyena_count) cond(stan.num_indv_grts) cond(stan.num_sd_year) cond(context)
# Sum of weights:      1.00                   1.00                     0.85                   0.08         
# N containing models:    3                      3                        1                      1         
importance(subset(results.prob.mob, delta <= 4 & !nested(.)))   
# Error in sw.model.selection(subset(results.prob.mob, delta <= 4 & !nested(.))) : 
#   argument consists of only one model
options(na.action = "na.omit")

##### Best model #####

#Standardize model variables
head(sessions.intx.male[,c(1,4,12,20)])
sessions.intx.male.final <- filter(sessions.intx.male, complete.cases(sessions.intx.male[,c(1,4,12,20)]))     #113
sessions.intx.male.final$stan.hyena_count <- as.numeric(scale(sessions.intx.male.final$hyena_count, center = T, scale = T))
sessions.intx.male.final$stan.num_sd_year <- as.numeric(scale(sessions.intx.male.final$num_sd_year, center = T, scale = T))
sessions.intx.male.final$stan.num_indv_grts <- as.numeric(scale(sessions.intx.male.final$num_indv_grts, center = T, scale = T))
summary(sessions.intx.male.final)

mod.prob.mob <- glmmTMB(mobbing ~ stan.hyena_count + stan.num_indv_grts + stan.num_sd_year,
                        data = sessions.intx.male.final, family = binomial(link = 'logit'))
summary(mod.prob.mob)
#                    Estimate Std. Error z value Pr(>|z|)    
# stan.hyena_count     1.2628     0.3456   3.654 0.000259 ***
# stan.num_indv_grts   0.9288     0.3219   2.885 0.003909 ** 
# stan.num_sd_year     0.6314     0.2456   2.571 0.010138 *  
check_collinearity(mod.prob.mob)     #all below 3
check_model(mod.prob.mob)
simulationOutput <- simulateResiduals(fittedModel = mod.prob.mob, n = 250)
plot(simulationOutput)
model_performance(mod.prob.mob)
# AIC     |    AICc |     BIC | R2 (cond.) | R2 (marg.) |  RMSE | Sigma | Log_loss | Score_log | Score_spherical
# --------------------------------------------------------------------------------------------------------------
# 103.908 | 104.278 | 114.817 |            |      0.541 | 0.379 | 1.000 |    0.424 |   -23.634 |           0.047
round(r2_tjur(mod.prob.mob), digits = 3)     #0.371

#Make table
mod.prob.mob <- glmmTMB(mobbing ~ stan.hyena_count + stan.num_sd_year + stan.num_indv_grts,
                        data = sessions.intx.male.final, family = binomial(link = 'logit'))
summary(mod.prob.mob)
sjPlot::tab_model(mod.prob.mob, show.se = T, show.ci = .95, show.re.var = F, show.intercept = F, 
                  pred.labels = c("Number of hyenas present", "Prey density", 
                                  "Number of hyenas who greet (greeters)"),
                  dv.labels = c("Probability of mobbing occurrence"), 
                  string.se = "SE", transform = "exp", file = "09.table_session_malelions.doc")

#Calculate CIs using the likelihood profile
tidy.mod.prob.mob <- broom.mixed::tidy(mod.prob.mob, conf.method = "profile", 
                                       conf.int = T, conf.level = 0.95, exponentiate = T)
tidy.mod.prob.mob
#   effect component term               estimate std.error statistic     p.value conf.low conf.high
# 2 fixed  cond      stan.hyena_count      3.54      1.22       3.65 0.000259    1.88      7.37 
# 3 fixed  cond      stan.num_sd_year      1.88      0.462      2.57 0.0101      1.18      3.13 
# 4 fixed  cond      stan.num_indv_grts    2.53      0.815      2.89 0.00391     1.39      4.96 





