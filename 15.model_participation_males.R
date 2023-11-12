######################################################################
##### 15.0 Modeling mobbing participation - adult males #####
######################################################################

########## 15.1 Set working directory & download packages/tables ##########

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
library(broom.mixed)
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

#Create male dataset
id.by.mob.male <- filter(id.by.mob, sex == "m" & age.class == "adult")     #1162


########## 15.2 Looking at combined data ##########

#drop prop.mob.related because don't have paternity data for immigrant males

#Session-level 
table(id.by.mob.male$mobber)
table(id.by.mob.male$context)

#Outliers
dotchart(id.by.mob.male$age)
dotchart(id.by.mob.male$stan.rank)
dotchart(id.by.mob.male$mob.ai.avg)
dotchart(id.by.mob.male$present.prop.rank.higher)

#Distribution/zeros
hist(id.by.mob.male$age)
hist(id.by.mob.male$stan.rank)
hist(id.by.mob.male$mob.ai.avg)
hist(id.by.mob.male$present.prop.rank.higher)
table(id.by.mob.male$status)
table(id.by.mob.male$grt_occ)
table(id.by.mob.male$agg_occ)

#Collinear X variables
ggpairs(id.by.mob.male[,c(12,14:15,18,20,25,28)])

#Create male dataset
head(id.by.mob.male[,c(1:10,12,14:15,18,20,25,28)])
summary(id.by.mob.male[,c(1:10,12,14:15,18,20,25,28)])
id.by.mob.male.all <- id.by.mob.male[,c(1:10,12,14:15,18,20,25,28)]     #1162


########## 15.3 Models ##########

##### Create dataset #####

head(id.by.mob.male.all[,c(11:17)])
ggcorr(id.by.mob.male.all[,c(11:17)], label = T)
# stan.rank and present.prop.rank.higher are too correlated - drop present.prop.rank.higher

head(id.by.mob.male.all[,c(11:16)])
id.by.mob.male.tmp <- filter(id.by.mob.male.all, complete.cases(id.by.mob.male.all[,c(11:16)]))   #n=893
id.by.mob.male.tmp$stan.age <- as.numeric(scale(id.by.mob.male.tmp$age, center = T, scale = T))
id.by.mob.male.tmp$stan.rank.og <- id.by.mob.male.tmp$stan.rank
id.by.mob.male.tmp$stan.rank <- as.numeric(scale(id.by.mob.male.tmp$stan.rank.og, center = T, scale = T))
id.by.mob.male.tmp$stan.mob.ai.avg <- as.numeric(scale(id.by.mob.male.tmp$mob.ai.avg, center = T, scale = T))
summary(id.by.mob.male.tmp)
ggcorr(id.by.mob.male.tmp[,c(11:16)], label = T)    #all below 0.7

##### Global model - additive #####

#Full model
mod.male <- glmmTMB(mobber ~ poly(stan.age, 2) + stan.rank + status +
                      grt_occ + stan.mob.ai.avg + (1|session/mobID) + (1|hyena), 
                    data = id.by.mob.male.tmp, family = binomial(link = 'logit'))
check_collinearity(mod.male)     #all below 5
check_model(mod.male)

#Dredge
options(na.action = "na.fail")
mod.male <- glmmTMB(mobber ~ poly(stan.age, 2) + stan.rank + status +
                      grt_occ + stan.mob.ai.avg + (1|session/mobID) + (1|hyena), 
                    data = id.by.mob.male.tmp, family = binomial(link = 'logit'))
results.male <- dredge(mod.male)
get.models(subset(results.male, delta == 0), subset = T)
#  mobber ~ poly(stan.age, 2) + stan.mob.ai.avg + stan.rank + (1 | session/mobID) + (1 | hyena)
importance(subset(results.male, delta <= 6 & !nested(.)))    
#                      cond(stan.rank) cond(poly(stan.age, 2)) cond(stan.mob.ai.avg)
# Sum of weights:      1.00            0.82                    0.71                 
# N containing models:    4               2                       2                 
options(na.action = "na.omit")

##### Global model - interactive #####

#Full model
mod.male <- glmmTMB(mobber ~ poly(stan.age, 2) + stan.rank + status + 
                      grt_occ + stan.mob.ai.avg + 
                      poly(stan.age, 2) * stan.rank + 
                      stan.rank * grt_occ + 
                      stan.rank * stan.mob.ai.avg + 
                      (1|session/mobID) + (1|hyena), 
                    data = id.by.mob.male.tmp, family = binomial(link = 'logit'))
check_collinearity(mod.male)
# remove age * rank
mod.male <- glmmTMB(mobber ~ poly(stan.age, 2) + stan.rank + status + 
                      grt_occ + stan.mob.ai.avg + 
                      stan.rank * grt_occ + 
                      stan.rank * stan.mob.ai.avg + 
                      (1|session/mobID) + (1|hyena), 
                    data = id.by.mob.male.tmp, family = binomial(link = 'logit'))
check_collinearity(mod.male)    #all below 5
check_model(mod.male)

#Dredge
options(na.action = "na.fail")
mod.male <- glmmTMB(mobber ~ poly(stan.age, 2) + stan.rank + status + 
                      grt_occ + stan.mob.ai.avg + 
                      stan.rank * grt_occ + 
                      stan.rank * stan.mob.ai.avg + 
                      (1|session/mobID) + (1|hyena), 
                    data = id.by.mob.male.tmp, family = binomial(link = 'logit'))
results.male <- dredge(mod.male)
get.models(subset(results.male, delta == 0), subset = T)
# mobber ~ poly(stan.age, 2) + stan.mob.ai.avg + stan.rank + (1 | session/mobID) + (1 | hyena)
importance(subset(results.male, delta <= 6 & !nested(.)))    
#                      cond(stan.rank) cond(poly(stan.age, 2)) cond(stan.mob.ai.avg)
# Sum of weights:      1.00            0.82                    0.71                 
# N containing models:    4               2                       2                 
options(na.action = "na.omit")


########## 15.4 Final model and plots ##########

head(id.by.mob.male.all[,c(12:13,16)])
id.by.mob.male.final <- filter(id.by.mob.male.all, complete.cases(id.by.mob.male.all[,c(12:13,16)]))   #n=893
id.by.mob.male.final$stan.age <- as.numeric(scale(id.by.mob.male.final$age, center = T, scale = T))
id.by.mob.male.final$stan.rank.og <- id.by.mob.male.final$stan.rank
id.by.mob.male.final$stan.rank <- as.numeric(scale(id.by.mob.male.final$stan.rank.og, center = T, scale = T))
id.by.mob.male.final$stan.mob.ai.avg <- as.numeric(scale(id.by.mob.male.final$mob.ai.avg, center = T, scale = T))
summary(id.by.mob.male.final)
ggcorr(id.by.mob.male.final[,c(12:13,16)], label = T)    #all below 0.7

##### Best model #####

mod.male <- glmmTMB(mobber ~ poly(stan.age, 2) + stan.mob.ai.avg + stan.rank + 
                      (1 | session/mobID) + (1 | hyena),
                    data = id.by.mob.male.final, family = binomial(link = 'logit'))
summary(mod.male)
#                    Estimate Std. Error z value Pr(>|z|)    
# poly(stan.age, 2)1  -5.0172     5.9484  -0.843   0.3990    
# poly(stan.age, 2)2 -13.2603     5.9088  -2.244   0.0248 *  
# stan.mob.ai.avg      0.3566     0.1780   2.003   0.0452 *  
# stan.rank            0.9743     0.2152   4.528 5.94e-06 ***
check_collinearity(mod.male)    #all below 3
check_model(mod.male)
simulationOutput <- simulateResiduals(fittedModel = mod.male, n = 250)
plot(simulationOutput)
model_performance(mod.male)
# AIC     |    AICc |     BIC | R2 (cond.) | R2 (marg.) |   ICC |  RMSE | Sigma | Log_loss | Score_log | Score_spherical
# ----------------------------------------------------------------------------------------------------------------------
# 861.775 | 861.938 | 900.132 |      0.696 |      0.106 | 0.660 | 0.278 | 1.000 |    0.264 |      -Inf |           0.009

##### Plot models #####

id.by.mob.male.final$mobber <- as.factor(id.by.mob.male.final$mobber)
summary(id.by.mob.male.final)
mod.male <- glmmTMB(mobber ~ poly(stan.age, 2) + stan.mob.ai.avg + stan.rank + 
                      (1 | session/mobID) + (1 | hyena),
                    data = id.by.mob.male.final, family = binomial(link = 'logit'))
summary(mod.male)

#Age
summary(id.by.mob.male.final$age)
dat.age <- ggeffects::ggpredict(mod.male, terms = c("stan.age [all]"), full.data = FALSE)
plot.age <- ggplot(dat.age, aes(x = dat.age$x*sd(id.by.mob.male.final$age, na.rm = T) + mean(id.by.mob.male.final$age, na.rm = T), 
                                y = predicted)) + 
  geom_line() + 
  geom_ribbon(aes(x = dat.age$x*sd(id.by.mob.male.final$age, na.rm = T) + mean(id.by.mob.male.final$age, na.rm = T), 
                  ymin = conf.low, ymax = conf.high), alpha = 0.2, inherit.aes = FALSE) +
  xlab('Age') +
  ylab("Predicted probability of mobbing")+
  theme_classic() + 
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        axis.title = element_text(size = 20), axis.text = element_text(size = 16))+
  ylim(c(0,1))
pdf('15.plot.male.age.pdf', width = 7, height = 5)
plot.age
dev.off()

#Calculate ages
round(filter(dat.age, predicted == max(dat.age$predicted))$
        x*sd(id.by.mob.male.final$age, na.rm = T) + mean(id.by.mob.male.final$age, na.rm = T), dig = 1)     #6.2
round(range(id.by.mob.male.final$age), dig = 1)     #2.0 16.9

#Rank
summary(id.by.mob.male.final$stan.rank.og)
dat.rank <- ggeffects::ggpredict(mod.male, terms = c("stan.rank [all]"), full.data = FALSE)
plot.rank <- ggplot(dat.rank, aes(x = dat.rank$x*sd(id.by.mob.male.final$stan.rank.og, na.rm = T) + 
                                    mean(id.by.mob.male.final$stan.rank.og, na.rm = T), y = predicted)) + 
  geom_line() + 
  geom_ribbon(aes(x = dat.rank$x*sd(id.by.mob.male.final$stan.rank.og, na.rm = T) + 
                    mean(id.by.mob.male.final$stan.rank.og, na.rm = T), 
                  ymin = conf.low, ymax = conf.high), alpha = 0.2, inherit.aes = FALSE) +
  xlab('Social rank') +
  ylab("Predicted probability of mobbing")+
  theme_classic() + 
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        axis.title = element_text(size = 20), axis.text = element_text(size = 16))+
  ylim(c(0,1))
pdf('15.plot.male.rank.pdf', width = 7, height = 5)
plot.rank
dev.off()

#Association index
summary(id.by.mob.male.final$mob.ai.avg)
dat.ai <- ggeffects::ggpredict(mod.male, terms = c("stan.mob.ai.avg [all]"), full.data = FALSE)
plot.ai <- ggplot(dat.ai, aes(x = dat.ai$x*sd(id.by.mob.male.final$mob.ai.avg, na.rm = T) + 
                                mean(id.by.mob.male.final$mob.ai.avg, na.rm = T), y = predicted)) + 
  geom_line() + 
  geom_ribbon(aes(x = dat.ai$x*sd(id.by.mob.male.final$mob.ai.avg, na.rm = T) + mean(id.by.mob.male.final$mob.ai.avg, na.rm = T), 
                  ymin = conf.low, ymax = conf.high), alpha = 0.2, inherit.aes = FALSE) +
  xlab('Association index') +
  ylab("Predicted probability of mobbing")+
  theme_classic() + 
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        axis.title = element_text(size = 20), axis.text = element_text(size = 16), 
        axis.title.y = element_blank())+
  ylim(c(0,1))
pdf('15.plot.male.ai.pdf', width = 7, height = 5)
plot.ai
dev.off()

#Plot of single variables
layout <- "A#
           BC"
plots <- plot.age + plot.rank + plot.ai + plot_layout(design = layout)
pdf('15.plots.all.pdf', width = 7, height = 5)
plots
dev.off()

#Model
id.by.mob.male.final$stan.age.sq <- (id.by.mob.male.final$stan.age)^2
mod.male <- glmmTMB(mobber ~ stan.age + stan.age.sq + stan.rank + stan.mob.ai.avg + 
                      (1 | session/mobID) + (1 | hyena),
                    data = id.by.mob.male.final, family = binomial(link = 'logit'))
summary(mod.male)
#                 Estimate Std. Error z value Pr(>|z|)    
# stan.age          0.1232     0.2455   0.502  0.61582    
# stan.age.sq      -0.3747     0.1670  -2.244  0.02482 *  
# stan.rank         0.9743     0.2152   4.528 5.94e-06 ***
# stan.mob.ai.avg   0.3566     0.1780   2.003  0.04520 *  
set_theme(base = theme_classic(), axis.textcolor = "black", axis.title.color = "black", 
          axis.textsize.y = 1.5, axis.textsize.x = 1.2, axis.title.size = 1.7)
plot.model <- plot_model(mod.male, type = "est", transform = "exp",
                         axis.labels = c("Association index (participants)", 
                                         "Social rank", "Age^2", "Age"),
                         vline.color = "black", title = "", dot.size = 4.5, line.size = 1.5,
                         show.values = TRUE, show.p = TRUE, digits = 2, value.offset = 0.3, 
                         value.size = 6, colors = viridis_2, axis.lim = c(0.1,10))
pdf('15.plot.male.model.pdf', width = 7, height = 5)
plot.model
dev.off()

#Table
sjPlot::tab_model(mod.male, show.se = T, show.ci = 0.95, show.re.var = F, show.intercept = F, 
                  pred.labels = c("Age", "Age^2", "Social rank", 
                                  "Association index (participants)"),
                  dv.labels = c("Probability of mobbing participation"), 
                  string.se = "SE", transform = "exp", file = "15.table_male.doc")

#Calculate CIs using the likelihood profile
tidy.mod.male <- broom.mixed::tidy(mod.male, conf.method = "profile",
                                   conf.int = T, conf.level = 0.95, exponentiate = T)
tidy.mod.male
#   effect   component group         term            estimate std.error statistic     p.value conf.low conf.high
# 2 fixed    cond      NA            stan.age           1.13      0.278     0.502  0.616         0.699     1.85 
# 3 fixed    cond      NA            stan.age.sq        0.687     0.115    -2.24   0.0248        0.483     0.934
# 4 fixed    cond      NA            stan.rank          2.65      0.570     4.53   0.00000594    1.77      4.15 
# 5 fixed    cond      NA            stan.mob.ai.avg    1.43      0.254     2.00   0.0452        1.01      2.05 
# 6 ran_pars cond      mobID:session sd__(Intercept)    1.30     NA        NA     NA            -0.203     0.620
# 7 ran_pars cond      session       sd__(Intercept)    2.05     NA        NA     NA             0.362     1.05 
# 8 ran_pars cond      hyena         sd__(Intercept)    0.722    NA        NA     NA            -1.31      0.192




