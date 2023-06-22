######################################################################
##### 14.0 Modeling mobbing participation - adult females #####
######################################################################

########## 14.1 Set working directory & download packages/tables ##########

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

#Function glht : https://cran.r-project.org/web/packages/glmmTMB/vignettes/model_evaluation.html
glht_glmmTMB <- function (model, ..., component="cond") {
  glht(model, ...,
       coef. = function(x) fixef(x)[[component]],
       vcov. = function(x) vcov(x)[[component]],
       df = NULL)
}

#Create adult female dataset
id.by.mob.fem <- filter(id.by.mob, sex == "f" & age.class == "adult")     #2403


########## 14.2 Looking at combined data ##########

#grts measure
table(id.by.mob.fem$grt_occ)     #pick this because less skewed
table(id.by.mob.fem$grt_occ_mobbers)

#agg measure
table(id.by.mob.fem$agg_occ)     #pick this because less skewed
table(id.by.mob.fem$agg_occ_mobbers) 

#AI measure
hist(id.by.mob.fem$mob.ai.avg)
hist(id.by.mob.fem$present.ai.avg)
ggpairs(id.by.mob.fem[,c(25,26)])    #highly correlated

mod.prob.mob <- glmmTMB(mobber ~ mob.ai.avg,
                        data = filter(id.by.mob.fem, !is.na(mob.ai.avg)), family = binomial(link = 'logit'))
AIC(mod.prob.mob)     #3205.553 - pick this
mod.prob.mob <- glmmTMB(mobber ~ present.ai.avg,
                        data = filter(id.by.mob.fem, !is.na(mob.ai.avg)), family = binomial(link = 'logit'))
AIC(mod.prob.mob)     #3219.009

#Relatedness measure
hist(id.by.mob.fem$prop.mob.related)
hist(id.by.mob.fem$prop.present.related)
ggpairs(id.by.mob.fem[,c(23,24)])    #highly correlated

mod.prob.mob <- glmmTMB(mobber ~ prop.mob.related,
                        data = filter(id.by.mob.fem, !is.na(prop.mob.related)), family = binomial(link = 'logit'))
AIC(mod.prob.mob)     #3190.027 - pick this
mod.prob.mob <- glmmTMB(mobber ~ prop.present.related,
                        data = filter(id.by.mob.fem, !is.na(prop.mob.related)), family = binomial(link = 'logit'))
AIC(mod.prob.mob)     #3203.023

#Relative rank measure
hist(id.by.mob.fem$present.prop.rank.higher)
hist(id.by.mob.fem$mob.prop.rank.higher)
ggpairs(id.by.mob.fem[,c(27,28)])    #highly correlated

mod.prob.mob <- glmmTMB(mobber ~ mob.prop.rank.higher,
                        data = filter(id.by.mob.fem, !is.na(mob.prop.rank.higher)), family = binomial(link = 'logit'))
AIC(mod.prob.mob)     #3250.546
mod.prob.mob <- glmmTMB(mobber ~ present.prop.rank.higher,
                        data = filter(id.by.mob.fem, !is.na(mob.prop.rank.higher)), family = binomial(link = 'logit'))
AIC(mod.prob.mob)     #3217.775 - pick this

#Session-level 
table(id.by.mob.fem$mobber)
table(id.by.mob.fem$context)

#Outliers
dotchart(id.by.mob.fem$age)
dotchart(id.by.mob.fem$stan.rank)
dotchart(id.by.mob.fem$parity) 
dotchart(id.by.mob.fem$prop.mob.related)
dotchart(id.by.mob.fem$mob.ai.avg)
dotchart(id.by.mob.fem$present.prop.rank.higher)

#Distribution/zeros
hist(id.by.mob.fem$age)
hist(id.by.mob.fem$stan.rank)
hist(id.by.mob.fem$parity)
hist(id.by.mob.fem$prop.mob.related)
hist(id.by.mob.fem$mob.ai.avg)
hist(id.by.mob.fem$present.prop.rank.higher)
table(id.by.mob.fem$repro.state)
table(id.by.mob.fem$grt_occ)
table(id.by.mob.fem$agg_occ)

#Collinear X variables
ggpairs(id.by.mob.fem[,c(14:17,18,20,23,25,28)])

#Create female dataset
head(id.by.mob.fem[,c(1:10,14:17,18,20,23,25,28)])
summary(id.by.mob.fem[,c(1:10,14:17,18,20,23,25,28)])
id.by.mob.fem.all <- id.by.mob.fem[,c(1:10,14:17,18,20,23,25,28)]     #2403


########## 14.3 Additive model ##########

##### Create dataset #####

head(id.by.mob.fem.all[,c(11:19)])
ggcorr(id.by.mob.fem.all[,c(11:19)], label = T)
# age and parity are too correlated - drop parity
# stan.rank and present.prop.rank.higher are too correlated - drop present.prop.rank.higher

head(id.by.mob.fem.all[,c(11:13,15:18)])
id.by.mob.fem.tmp <- filter(id.by.mob.fem.all, complete.cases(id.by.mob.fem.all[,c(11:13,15:18)]))   #n=2278
id.by.mob.fem.tmp$stan.age <- as.numeric(scale(id.by.mob.fem.tmp$age, center = T, scale = T))
id.by.mob.fem.tmp$stan.rank.og <- id.by.mob.fem.tmp$stan.rank
id.by.mob.fem.tmp$stan.rank <- as.numeric(scale(id.by.mob.fem.tmp$stan.rank.og, center = T, scale = T))
id.by.mob.fem.tmp$repro.state <- as.factor(as.character(id.by.mob.fem.tmp$repro.state))
id.by.mob.fem.tmp$repro.state <- factor(id.by.mob.fem.tmp$repro.state, levels = c("n", "p", "l", "o"))
id.by.mob.fem.tmp$stan.prop.mob.related <- as.numeric(scale(id.by.mob.fem.tmp$prop.mob.related, center = T, scale = T))
id.by.mob.fem.tmp$stan.mob.ai.avg <- as.numeric(scale(id.by.mob.fem.tmp$mob.ai.avg, center = T, scale = T))
summary(id.by.mob.fem.tmp)
ggcorr(id.by.mob.fem.tmp[,c(11:13,15:18)], label = T)    #all below 7

##### Global model - additive #####

#Full model
mod.fem <- glmmTMB(mobber ~ poly(stan.age, 2) + stan.rank + repro.state + 
                     grt_occ + stan.mob.ai.avg + stan.prop.mob.related + #agg_occ + 
                     (1|session/mobID) + (1|hyena), 
                   data = id.by.mob.fem.tmp, family = binomial(link = 'logit'))
check_collinearity(mod.fem)    #all below 3
check_model(mod.fem)

#Dredge
options(na.action = "na.fail")
mod.fem <- glmmTMB(mobber ~ poly(stan.age, 2) + stan.rank + repro.state + 
                     grt_occ + stan.mob.ai.avg + stan.prop.mob.related + #agg_occ + 
                     (1|session/mobID) + (1|hyena), 
                   data = id.by.mob.fem.tmp, family = binomial(link = 'logit'))
results.fem <- dredge(mod.fem)
get.models(subset(results.fem, delta == 0), subset = T)
# mobber ~ grt_occ + poly(stan.age, 2) + stan.mob.ai.avg + stan.prop.mob.related +  
#   (1 | session/mobID) + (1 | hyena)
# same model with agg_occ in global model
importance(subset(results.fem, delta <= 6 & !nested(.)))    
#                      cond(grt_occ) cond(stan.mob.ai.avg) cond(stan.prop.mob.related) cond(poly(stan.age, 2))
# Sum of weights:      1.00          1.00                  1.00                        0.84                   
# N containing models:    2             2                     2                           1                   
options(na.action = "na.omit")


########## 14.4 Interactive model ##########

##### Global model - interactive #####

#Full model
mod.fem <- glmmTMB(mobber ~ poly(stan.age, 2) + stan.rank + repro.state + 
                     grt_occ + stan.mob.ai.avg + stan.prop.mob.related + #agg_occ + 
                     poly(stan.age, 2) * stan.rank + 
                     stan.rank * repro.state + 
                     stan.rank * grt_occ + 
                     #stan.rank * agg_occ + 
                     stan.rank * stan.mob.ai.avg + 
                     stan.rank * stan.prop.mob.related + 
                     (1|session/mobID) + (1|hyena), 
                   data = id.by.mob.fem.tmp, family = binomial(link = 'logit'))
check_collinearity(mod.fem)     #remove stan.rank * repro.state
mod.fem <- glmmTMB(mobber ~ poly(stan.age, 2) + stan.rank + repro.state + 
                     grt_occ + stan.mob.ai.avg + stan.prop.mob.related + #agg_occ + 
                     poly(stan.age, 2) * stan.rank + 
                     stan.rank * grt_occ + 
                     #stan.rank * agg_occ + 
                     stan.rank * stan.mob.ai.avg + 
                     stan.rank * stan.prop.mob.related + 
                     (1|session/mobID) + (1|hyena), 
                   data = id.by.mob.fem.tmp, family = binomial(link = 'logit'))
check_collinearity(mod.fem)     #all below 3
check_model(mod.fem)

#Dredge
options(na.action = "na.fail")
mod.fem <- glmmTMB(mobber ~ poly(stan.age, 2) + stan.rank + repro.state + 
                     grt_occ + stan.mob.ai.avg + stan.prop.mob.related + #agg_occ + 
                     poly(stan.age, 2) * stan.rank + 
                     stan.rank * grt_occ + 
                     #stan.rank * agg_occ + 
                     stan.rank * stan.mob.ai.avg + 
                     stan.rank * stan.prop.mob.related + 
                     (1|session/mobID) + (1|hyena), 
                   data = id.by.mob.fem.tmp, family = binomial(link = 'logit'))
results.fem <- dredge(mod.fem)
get.models(subset(results.fem, delta == 0), subset = T)
# mobber ~ grt_occ + poly(stan.age, 2) + stan.mob.ai.avg + stan.prop.mob.related +  
#   stan.rank + (1 | session/mobID) + (1 | hyena) + grt_occ:stan.rank + stan.mob.ai.avg:stan.rank
# same model with agg_occ in global model
importance(subset(results.fem, delta <= 6 & !nested(.)))    
#                      cond(grt_occ) cond(stan.mob.ai.avg) cond(stan.rank) cond(grt_occ:stan.rank)
# Sum of weights:      1.00          1.00                  1.00            0.95                   
# N containing models:    6             6                     6               5                   
#                      cond(stan.prop.mob.related) cond(poly(stan.age, 2)) cond(stan.mob.ai.avg:stan.rank)
# Sum of weights:      0.92                        0.91                    0.82                           
# N containing models:    5                           4                       4                           
options(na.action = "na.omit")


########## 14.5 Adult females - final model and plots ##########

head(id.by.mob.fem.all[,c(11:12,15,17:18)])
id.by.mob.fem.final <- filter(id.by.mob.fem.all, complete.cases(id.by.mob.fem.all[,c(11:12,15,17:18)]))   #n=2280
id.by.mob.fem.final$stan.age <- as.numeric(scale(id.by.mob.fem.final$age, center = T, scale = T))
id.by.mob.fem.final$stan.rank.og <- id.by.mob.fem.final$stan.rank
id.by.mob.fem.final$stan.rank <- as.numeric(scale(id.by.mob.fem.final$stan.rank.og, center = T, scale = T))
id.by.mob.fem.final$stan.prop.mob.related <- as.numeric(scale(id.by.mob.fem.final$prop.mob.related, center = T, scale = T))
id.by.mob.fem.final$stan.mob.ai.avg <- as.numeric(scale(id.by.mob.fem.final$mob.ai.avg, center = T, scale = T))
summary(id.by.mob.fem.final)
ggcorr(id.by.mob.fem.final[,c(11:12,15,17:18)], label = T)    #all below 7

##### Best model #####

mod.fem <- glmmTMB(mobber ~ grt_occ + poly(stan.age, 2) + stan.mob.ai.avg + stan.prop.mob.related +  
                     stan.rank + (1 | session/mobID) + (1 | hyena) + 
                     grt_occ:stan.rank + stan.mob.ai.avg:stan.rank,
                   data = id.by.mob.fem.final, family = binomial(link = 'logit'))
summary(mod.fem)
#                            Estimate Std. Error z value Pr(>|z|)    
# grt_occTRUE                 1.16675    0.25143   4.640 3.48e-06 ***
# poly(stan.age, 2)1         -5.17427    3.74777  -1.381  0.16739    
# poly(stan.age, 2)2        -10.12276    4.10741  -2.465  0.01372 *  
# stan.mob.ai.avg             0.38704    0.13384   2.892  0.00383 ** 
# stan.prop.mob.related       0.22759    0.09139   2.490  0.01277 *  
# stan.rank                   0.16532    0.10445   1.583  0.11346    
# grt_occTRUE:stan.rank      -0.74612    0.28565  -2.612  0.00900 ** 
# stan.mob.ai.avg:stan.rank   0.21304    0.09463   2.251  0.02437 *  
check_collinearity(mod.fem)    #all before 3
check_model(mod.fem)
binned_residuals(mod.fem)
# Warning: Probably bad model fit. Only about 56% of the residuals are inside the error bounds.
simulationOutput <- simulateResiduals(fittedModel = mod.fem, n = 250)
plot(simulationOutput)   #KS test: p = 0, Dispersion test: p = 0.952, Outlier test: p = 0.47843
model_performance(mod.fem)
# AIC      |      BIC | R2 (cond.) | R2 (marg.) |   ICC |  RMSE | Sigma | Log_loss | 
# 2653.265 | 2722.048 |      0.554 |      0.092 | 0.509 | 0.372 | 1.000 |    0.434 |

##### Plot models #####

id.by.mob.fem.final$mobber <- as.factor(id.by.mob.fem.final$mobber)
id.by.mob.fem.final$grt_occ <- as.factor(id.by.mob.fem.final$grt_occ)
mod.fem <- glmmTMB(mobber ~ grt_occ + poly(stan.age, 2) + stan.mob.ai.avg + stan.prop.mob.related +  
                     stan.rank + (1 | session/mobID) + (1 | hyena) + 
                     grt_occ:stan.rank + stan.mob.ai.avg:stan.rank,
                   data = id.by.mob.fem.final, family = binomial(link = 'logit'))
summary(mod.fem)

#Age
summary(id.by.mob.fem.final$age)
dat.age <- ggeffects::ggpredict(mod.fem, terms = c("stan.age [all]"), full.data = FALSE)
plot.age <- ggplot(dat.age, aes(x = dat.age$x*sd(id.by.mob.fem.final$age, na.rm = T) + mean(id.by.mob.fem.final$age, na.rm = T), 
                                y = predicted)) + 
  geom_line() + 
  geom_ribbon(aes(x = dat.age$x*sd(id.by.mob.fem.final$age, na.rm = T) + mean(id.by.mob.fem.final$age, na.rm = T), 
                  ymin = conf.low, ymax = conf.high), alpha = 0.2, inherit.aes = FALSE) +
  xlab('Age') +
  ylab("Predicted probability of mobbing")+
  theme_classic() + 
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        axis.title = element_text(size = 20), axis.text = element_text(size = 16))+
  ylim(c(0,1))
pdf('14.plot.fem.age.pdf', width = 7, height = 5)
plot.age
dev.off()

#Calculate ages
round(filter(dat.age, predicted == max(dat.age$predicted))$
        x*sd(id.by.mob.fem.final$age, na.rm = T) + mean(id.by.mob.fem.final$age, na.rm = T), dig = 1)     #6.7
round(range(id.by.mob.fem.final$age), dig = 1)     #2.0 21.2

#Rank
summary(id.by.mob.fem.final$stan.rank.og)
dat.rank <- ggeffects::ggpredict(mod.fem, terms = c("stan.rank [all]"), full.data = FALSE)
plot.rank <- ggplot(dat.rank, aes(x = dat.rank$x*sd(id.by.mob.fem.final$stan.rank.og, na.rm = T) + 
                                    mean(id.by.mob.fem.final$stan.rank.og, na.rm = T), y = predicted)) + 
  geom_line() + 
  geom_ribbon(aes(x = dat.rank$x*sd(id.by.mob.fem.final$stan.rank.og, na.rm = T) + mean(id.by.mob.fem.final$stan.rank.og, na.rm = T), 
                  ymin = conf.low, ymax = conf.high), alpha = 0.2, inherit.aes = FALSE) +
  xlab('Social rank') +
  ylab("Predicted probability of mobbing")+
  theme_classic() + 
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        axis.title = element_text(size = 20), axis.text = element_text(size = 16))+
  ylim(c(0,1))
pdf('14.plot.fem.rank.pdf', width = 7, height = 5)
plot.rank
dev.off()

#Greetings
dat.grt <- ggeffects::ggpredict(mod.fem, type = "fe", terms = c("grt_occ [all]"), full.data = FALSE)
plot.grt <- ggplot(dat.grt, aes(x = x, y = predicted)) + 
  geom_point(size = 5) + 
  geom_line(data = data.frame(x = c(dat.grt$x, dat.grt$x),
                              predicted = c(dat.grt$predicted, dat.grt$predicted),
                              ci = c(dat.grt$conf.low, dat.grt$conf.high)),
            aes(x = x, y = ci, group= x), size = 1.5)+
  xlab('Greeted') +
  ylab("Predicted probability of mobbing") +
  theme_classic() + 
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        axis.title = element_text(size = 20), axis.text = element_text(size = 16), 
        axis.title.y = element_blank()) +
  ylim(c(0,1))
pdf('14.plot.fem.grt.pdf', width = 7, height = 5)
plot.grt
dev.off()

#Association index
summary(id.by.mob.fem.final$mob.ai.avg)
dat.ai <- ggeffects::ggpredict(mod.fem, terms = c("stan.mob.ai.avg [all]"), full.data = FALSE)
plot.ai <- ggplot(dat.ai, aes(x = dat.ai$x*sd(id.by.mob.fem.final$mob.ai.avg, na.rm = T) + 
                                mean(id.by.mob.fem.final$mob.ai.avg, na.rm = T), y = predicted)) + 
  geom_line() + 
  geom_ribbon(aes(x = dat.ai$x*sd(id.by.mob.fem.final$mob.ai.avg, na.rm = T) + mean(id.by.mob.fem.final$mob.ai.avg, na.rm = T), 
                  ymin = conf.low, ymax = conf.high), alpha = 0.2, inherit.aes = FALSE) +
  xlab('Association index') +
  ylab("Predicted probability of mobbing")+
  theme_classic() + 
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        axis.title = element_text(size = 20), axis.text = element_text(size = 16))+
  ylim(c(0,1))
pdf('14.plot.fem.ai.pdf', width = 7, height = 5)
plot.ai
dev.off()

#Relatedness
summary(id.by.mob.fem.final$prop.mob.related)
dat.rel <- ggeffects::ggpredict(mod.fem, terms = c("stan.prop.mob.related [all]"), full.data = FALSE)
plot.rel <- ggplot(dat.rel, aes(x = dat.rel$x*sd(id.by.mob.fem.final$prop.mob.related, na.rm = T) + 
                                  mean(id.by.mob.fem.final$prop.mob.related, na.rm = T), y = predicted)) + 
  geom_line() + 
  geom_ribbon(aes(x = dat.rel$x*sd(id.by.mob.fem.final$prop.mob.related, na.rm = T) + mean(id.by.mob.fem.final$prop.mob.related, na.rm = T), 
                  ymin = conf.low, ymax = conf.high), alpha = 0.2, inherit.aes = FALSE) +
  xlab('Maternal relatedness') +
  ylab("Predicted probability of mobbing")+
  theme_classic() + 
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        axis.title = element_text(size = 20), axis.text = element_text(size = 16), 
        axis.title.y = element_blank())+
  ylim(c(0,1))
pdf('14.plot.fem.rel.pdf', width = 7, height = 5)
plot.rel
dev.off()

#Plot of single variables
layout <- "A#
           BC
           DE"
plots <- plot.age + plot.rank + plot.grt + plot.ai + plot.rel + plot_layout(design = layout)
pdf('14.plots.all.pdf', width = 7, height = 7.5)
plots
dev.off()

#Look at interaction plots
plot_model(mod.fem, type = "pred", terms = c("stan.rank", "stan.mob.ai.avg"))
plot_model(mod.fem, type = "pred", terms = c("stan.rank", "grt_occ"))

#Interaction between AI and rank
dat.intx.rank.ai <- ggeffects::ggpredict(mod.fem, terms = c("stan.rank [all]", "stan.mob.ai.avg "), full.data = FALSE)
plot.rank.ai <- ggplot(dat.intx.rank.ai, aes(x = dat.intx.rank.ai$x*sd(id.by.mob.fem.final$stan.rank.og, na.rm = T) + 
                                               mean(id.by.mob.fem.final$stan.rank.og, na.rm = T), 
                                             y = predicted, color = group)) + 
  geom_ribbon(aes(x = dat.intx.rank.ai$x*sd(id.by.mob.fem.final$stan.rank.og, na.rm = T) + 
                    mean(id.by.mob.fem.final$stan.rank.og, na.rm = T), 
                  ymin = conf.low, ymax = conf.high, fill = dat.intx.rank.ai$group), alpha = 0.2, inherit.aes = FALSE) +
  geom_line(size = 1) + 
  xlab('Social rank') +
  ylab("Predicted probability of mobbing")+
  theme_classic() + 
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 12), 
        axis.title = element_text(size = 20), axis.text = element_text(size = 16), 
        legend.position = "none") +   #"top"
  scale_color_manual(values = viridis_3, name = "Association index", labels = c("Weak", "Average", "Strong")) +
  scale_fill_manual(values = viridis_3, name = "Association index", labels = c("Weak", "Average", "Strong")) +
  ylim(c(0,1))
pdf('14.plot.rank.ai.pdf', width = 7, height = 5)
plot.rank.ai
dev.off()

#Interaction between greeted and rank
dat.intx.rank.grt <- ggeffects::ggpredict(mod.fem, terms = c("stan.rank [all]", "grt_occ "), full.data = FALSE)
plot.rank.grt <- ggplot(dat.intx.rank.grt, aes(x = dat.intx.rank.grt$x*sd(id.by.mob.fem.final$stan.rank.og, na.rm = T) + 
                                                 mean(id.by.mob.fem.final$stan.rank.og, na.rm = T), 
                                               y = predicted, color = group)) + 
  geom_ribbon(aes(x = dat.intx.rank.grt$x*sd(id.by.mob.fem.final$stan.rank.og, na.rm = T) + 
                    mean(id.by.mob.fem.final$stan.rank.og, na.rm = T), 
                  ymin = conf.low, ymax = conf.high, fill = dat.intx.rank.grt$group), alpha = 0.2, inherit.aes = FALSE) +
  geom_line(size = 1) + 
  xlab('Social rank') +
  ylab("Predicted probability of mobbing")+
  theme_classic() + 
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 12), 
        axis.title = element_text(size = 20), axis.text = element_text(size = 16), 
        legend.position = "none") +   #"top"
  scale_color_manual(values = viridis_3, name = "Greeted") +
  scale_fill_manual(values = viridis_3, name = "Greeted") +
  ylim(c(0,1))
pdf('14.plot.rank.grt.pdf', width = 7, height = 5)
plot.rank.grt
dev.off()

#Model
id.by.mob.fem.final$stan.age.sq <- (id.by.mob.fem.final$stan.age)^2
mod.fem <- glmmTMB(mobber ~ stan.age + stan.age.sq + stan.rank + 
                     grt_occ + stan.mob.ai.avg + stan.prop.mob.related + 
                     (1 | session/mobID) + (1 | hyena) + 
                     grt_occ:stan.rank + stan.mob.ai.avg:stan.rank,
                   data = id.by.mob.fem.final, family = binomial(link = 'logit'))
summary(mod.fem)
#                           Estimate Std. Error z value Pr(>|z|)    
# stan.age                   0.08273    0.10042   0.824  0.41001    
# stan.age.sq               -0.13066    0.05302  -2.464  0.01372 *  
# stan.rank                  0.16532    0.10445   1.583  0.11346    
# grt_occTRUE                1.16674    0.25143   4.640 3.48e-06 ***
# stan.mob.ai.avg            0.38705    0.13384   2.892  0.00383 ** 
# stan.prop.mob.related      0.22758    0.09139   2.490  0.01277 *  
# stan.rank:grt_occTRUE     -0.74611    0.28564  -2.612  0.00900 ** 
# stan.rank:stan.mob.ai.avg  0.21304    0.09463   2.251  0.02437 *  
set_theme(base = theme_classic(), axis.textcolor = "black", axis.title.color = "black", 
            axis.textsize.y = 1.5, axis.textsize.x = 1.2, axis.title.size = 1.7)
plot.model <- plot_model(mod.fem, type = "est", transform = NULL,
                         axis.labels = c("Association index x Social rank", "Greeted x Social rank",
                                         "Maternal relatedness (participants)", "Association index (participants)",
                                         "Greeted [TRUE]", "Social rank", "Age^2", "Age"),
                         vline.color = "black", title = "", dot.size = 4.5, line.size = 1.5,
                         show.values = TRUE, show.p = TRUE, digits = 2, value.offset = 0.3, 
                         value.size = 6, colors = viridis_2, axis.lim = c(-2,2))
pdf('14.plot.fem.model.pdf', width = 7, height = 7.5)
plot.model
dev.off()

#Table
sjPlot::tab_model(mod.fem, show.se = T, show.ci = F, show.re.var = F, show.intercept = F, 
                  pred.labels = c("Age", "Age^2", "Social rank", "Greeted [TRUE]", 
                                  "Association index (participants)", "Maternal relatedness (participants)",
                                  "Greeted x Social rank", "Association index x Social rank"),
                  dv.labels = c("Probability of mobbing participation"), 
                  string.se = "SE", transform = NULL, file = "14.table_fem.doc")






