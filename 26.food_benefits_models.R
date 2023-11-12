######################################################################
##### 26.0 Modeling benefits based on mobbing #####
######################################################################

########## 26.1 Set working directory & download packages/tables ##########

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
load("25.feeding_benefits.Rdata")

#Function glht : https://cran.r-project.org/web/packages/glmmTMB/vignettes/model_evaluation.html
glht_glmmTMB <- function (model, ..., component="cond") {
  glht(model, ...,
       coef. = function(x) fixef(x)[[component]],
       vcov. = function(x) vcov(x)[[component]],
       df = NULL)
}

#Set plot colors using viridis
viridis_2 <- viridis(7)[-c(1,3,5,6,7)]
viridis_3 <- viridis(7)[-c(2,4,6,7)]


########## 26.2 Do mobbers feed? Analysis by mob ##########

##### Create dataset #####

head(id.by.mob.food[,c(6,11,13,14,21,23)])
id.by.mob.food.tmp <- filter(id.by.mob.food, complete.cases(id.by.mob.food[,c(6,11,13,14,21,23)]))   #n=1028
id.by.mob.food.tmp$stan.age <- as.numeric(scale(id.by.mob.food.tmp$age, center = T, scale = T))
id.by.mob.food.tmp$stan.rank.og <- id.by.mob.food.tmp$stan.rank
id.by.mob.food.tmp$stan.rank <- as.numeric(scale(id.by.mob.food.tmp$stan.rank.og, center = T, scale = T))
summary(id.by.mob.food.tmp)
ggcorr(id.by.mob.food.tmp[,c(6,11,13,14,21,23)], label = T)     #all below 0.7

##### Global model - additive #####

#Full model
mod.feeds <- glmmTMB(feeds ~ poly(stan.age, 2) + sex + stan.rank + carc_cat + 
                       location + mobber + (1 | session/mobID) + (1 | hyena), 
                     data=id.by.mob.food.tmp, family = binomial(link = 'logit'))
check_collinearity(mod.feeds)    #all below 3
check_model(mod.feeds)

#Dredge
options(na.action = "na.fail")
mod.feeds <- glmmTMB(feeds ~ poly(stan.age, 2) + sex + stan.rank + carc_cat + 
                       location + mobber + (1 | session/mobID) + (1 | hyena), 
                     data=id.by.mob.food.tmp, family = binomial(link = 'logit'))
results.feeds <- dredge(mod.feeds)
get.models(subset(results.feeds, delta == 0), subset = T)
#  feeds ~ mobber + poly(stan.age, 2) + stan.rank + (1 | session/mobID) + (1 | hyena)
importance(subset(results.feeds, delta <= 6 & !nested(.)))
#                      cond(poly(stan.age, 2)) cond(mobber) cond(stan.rank) cond(sex)
# Sum of weights:      1.00                    0.95         0.79            0.17     
# N containing models:    4                       3            2               1     
options(na.action = "na.omit")

##### Global model - interactive #####

#Full model
mod.feeds <- glmmTMB(feeds ~ poly(stan.age, 2) + sex + stan.rank + carc_cat + 
                       location + mobber + 
                       mobber * stan.rank + 
                       mobber * carc_cat + 
                       mobber * location + 
                       (1 | session/mobID) + (1 | hyena), 
                     data=id.by.mob.food.tmp, family = binomial(link = 'logit'))
check_collinearity(mod.feeds)    #all <= 3
check_model(mod.feeds)
r2(mod.feeds)    #remove mobID random effect

#Dredge
options(na.action = "na.fail")
mod.feeds <- glmmTMB(feeds ~ poly(stan.age, 2) + sex + stan.rank + carc_cat + 
                       location + mobber + 
                       mobber * stan.rank + 
                       mobber * carc_cat + 
                       mobber * location + 
                       (1 | session) + (1 | hyena), 
                     data=id.by.mob.food.tmp, family = binomial(link = 'logit'))
results.feeds <- dredge(mod.feeds)
get.models(subset(results.feeds, delta == 0), subset = T)
# feeds ~ mobber + poly(stan.age, 2) + stan.rank + (1 | session) + (1 | hyena)
importance(subset(results.feeds, delta <= 6 & !nested(.)))
#                      cond(poly(stan.age, 2)) cond(mobber) cond(stan.rank) cond(sex)
# Sum of weights:      1.00                    0.95         0.79            0.17     
# N containing models:    4                       3            2               1     
importance(subset(results.feeds, delta <= 2 & !nested(.)))
#1 model
options(na.action = "na.omit")


########## 26.3 Do mobbers feed? Analysis by mob final model ##########

#Create dataset
head(id.by.mob.food[,c(6,13,14)])
id.by.mob.food.final <- filter(id.by.mob.food, complete.cases(id.by.mob.food[,c(6,13,14)]))   #n=1049
id.by.mob.food.final$stan.age <- as.numeric(scale(id.by.mob.food.final$age, center = T, scale = T))
id.by.mob.food.final$stan.rank.og <- id.by.mob.food.final$stan.rank
id.by.mob.food.final$stan.rank <- as.numeric(scale(id.by.mob.food.final$stan.rank.og, center = T, scale = T))

##### Best model #####

mod.feeds <- glmmTMB(feeds ~ mobber + poly(stan.age, 2) + stan.rank + 
                       (1 | session) + (1 | hyena), 
                     data=id.by.mob.food.final, family = binomial(link = 'logit'))
summary(mod.feeds)
#                    Estimate Std. Error z value Pr(>|z|)    
# mobberTRUE           0.5590     0.2036   2.745  0.00605 ** 
# poly(stan.age, 2)1  -9.3747     4.7908  -1.957  0.05037 .  
# poly(stan.age, 2)2 -13.3588     4.8006  -2.783  0.00539 ** 
# stan.rank            0.4219     0.1540   2.740  0.00615 ** 
check_collinearity(mod.feeds)    #all below 3
check_model(mod.feeds) 
simulationOutput <- simulateResiduals(fittedModel = mod.feeds, n = 250)
plot(simulationOutput)
model_performance(mod.feeds)
# AIC      |     AICc |      BIC | R2 (cond.) | R2 (marg.) |   ICC |  RMSE | Sigma | Log_loss | Score_log | Score_spherical
# -------------------------------------------------------------------------------------------------------------------------
# 1041.450 | 1041.558 | 1076.139 |      0.519 |      0.082 | 0.476 | 0.334 | 1.000 |    0.364 |      -Inf |           0.003

##### Plot model #####

id.by.mob.food.final$mobber <- as.factor(id.by.mob.food.final$mobber)
id.by.mob.food.final$feeds <- as.factor(id.by.mob.food.final$feeds)
mod.feeds <- glmmTMB(feeds ~ mobber + poly(stan.age, 2) + stan.rank + 
                       (1 | session) + (1 | hyena), 
                     data=id.by.mob.food.final, family = binomial(link = 'logit'))

#Age
summary(id.by.mob.food.final$age)
dat.age <- ggeffects::ggpredict(mod.feeds, terms = c("stan.age [all]"), full.data = FALSE)
plot.age <- ggplot(dat.age, aes(x = dat.age$x*sd(id.by.mob.food.final$age, na.rm = T) + mean(id.by.mob.food.final$age, na.rm = T), 
                                y = predicted)) + 
  geom_line() + 
  geom_ribbon(aes(x = dat.age$x*sd(id.by.mob.food.final$age, na.rm = T) + mean(id.by.mob.food.final$age, na.rm = T), 
                  ymin = conf.low, ymax = conf.high), alpha = 0.2, inherit.aes = FALSE) +
  xlab('Age') +
  ylab("Predicted probability of feeding")+
  theme_classic() + 
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        axis.title = element_text(size = 20), axis.text = element_text(size = 16)) +
  ylim(c(0,0.6))
pdf('26.plot.age.feeds.pdf', width = 7, height = 5)
plot.age
dev.off()

#Calculate ages
round(filter(dat.age, predicted == max(dat.age$predicted))$
        x*sd(id.by.mob.food.final$age, na.rm = T) + mean(id.by.mob.food.final$age, na.rm = T), dig = 1)     #6.9
round(range(id.by.mob.food.final$age, na.rm = T), dig = 1)     #2.0 21.2

#Rank
summary(id.by.mob.food.final$stan.rank.og)
dat.rank <- ggeffects::ggpredict(mod.feeds, terms = c("stan.rank [all]"), full.data = FALSE)
plot.rank <- ggplot(dat.rank, aes(x = dat.rank$x*sd(id.by.mob.food.final$stan.rank.og, na.rm = T) + 
                                    mean(id.by.mob.food.final$stan.rank.og, na.rm = T), y = predicted)) + 
  geom_line() + 
  geom_ribbon(aes(x = dat.rank$x*sd(id.by.mob.food.final$stan.rank.og, na.rm = T) + mean(id.by.mob.food.final$stan.rank.og, na.rm = T), 
                  ymin = conf.low, ymax = conf.high), alpha = 0.2, inherit.aes = FALSE) +
  xlab('Social rank') +
  ylab("Predicted probability of feeding")+
  theme_classic() + 
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        axis.title = element_text(size = 20), axis.text = element_text(size = 16)) +
  ylim(c(0,0.6))
pdf('26.plot.rank.feeds.pdf', width = 7, height = 5)
plot.rank
dev.off()

#Mobber
dat.mobber <- ggeffects::ggpredict(mod.feeds, type = "fe", terms = c("mobber [all]"), full.data = FALSE)
plot.mobber <- ggplot(dat.mobber, aes(x = x, y = predicted)) + 
  geom_point(size = 5) + 
  geom_line(data = data.frame(x = c(dat.mobber$x, dat.mobber$x),
                              predicted = c(dat.mobber$predicted, dat.mobber$predicted),
                              ci = c(dat.mobber$conf.low, dat.mobber$conf.high)),
            aes(x = x, y = ci, group= x), size = 1.5)+
  xlab('Participant') +
  ylab("Predicted probability of feeding")+
  theme_classic() + 
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        axis.title = element_text(size = 20), axis.text = element_text(size = 16), 
        axis.title.y = element_blank()) +
  ylim(c(0,0.6))
pdf('26.plot.mobber.feeds.pdf', width = 7, height = 5)
plot.mobber
dev.off()

#Plot of single variables
layout <- "A#
           BC"
plots <- wrap_plots(A = plot.age, B = plot.rank, C = plot.mobber, design = layout)
pdf('26.plots.all.pdf', width = 7, height = 5)
plots
dev.off()

#Model
id.by.mob.food.final$stan.age.sq <- (id.by.mob.food.final$stan.age)^2
mod.feeds <- glmmTMB(feeds ~ stan.age + stan.age.sq + stan.rank + mobber + 
                       (1 | session) + (1 | hyena), 
                     data=id.by.mob.food.final, family = binomial(link = 'logit'))
summary(mod.feeds)
#             Estimate Std. Error z value Pr(>|z|)    
# stan.age     0.07502    0.17515   0.428  0.66842    
# stan.age.sq -0.26345    0.09468  -2.783  0.00539 ** 
# stan.rank    0.42190    0.15401   2.740  0.00615 ** 
# mobberTRUE   0.55898    0.20362   2.745  0.00605 ** 
set_theme(base = theme_classic(), axis.textcolor = "black", axis.title.color = "black", 
          axis.textsize.y = 1.5, axis.textsize.x = 1.2, axis.title.size = 1.7)
plot.model <- plot_model(mod.feeds, type = "est", transform = "exp",
                         axis.labels = c("Participant [TRUE]",
                                         "Social rank", "Age^2", "Age"),
                         vline.color = "black", title = "", dot.size = 4.5, line.size = 1.5,
                         show.values = TRUE, show.p = TRUE, digits = 2, value.offset = 0.3, 
                         value.size = 6, colors = viridis_2, axis.lim = c(0.1,10))
pdf('26.plot.model.feeds.pdf', width = 7, height = 5)
plot.model
dev.off()

#Table
sjPlot::tab_model(mod.feeds, show.se = T, show.ci = 0.95, show.re.var = F, show.intercept = F, 
                  pred.labels = c("Age", "Age^2", "Social rank", 
                                  "Participant [TRUE]"),
                  dv.labels = c("Probability of feeding"), 
                  string.se = "SE", transform = "exp", file = "26.table_feed.doc")

#Calculate CIs using the likelihood profile
tidy.mod.feeds <- broom.mixed::tidy(mod.feeds, conf.method = "profile",
                                    conf.int = T, conf.level = 0.95, exponentiate = T)
tidy.mod.feeds
#   effect   component group   term            estimate std.error statistic     p.value conf.low conf.high
# 2 fixed    cond      NA      stan.age           1.08     0.189      0.428  0.668       0.764      1.52 
# 3 fixed    cond      NA      stan.age.sq        0.768    0.0727    -2.78   0.00539     0.624      0.911
# 4 fixed    cond      NA      stan.rank          1.52     0.235      2.74   0.00615     1.13       2.09 
# 5 fixed    cond      NA      mobberTRUE         1.75     0.356      2.75   0.00605     1.17       2.61 
# 6 ran_pars cond      session sd__(Intercept)    1.10    NA         NA     NA          -0.276      0.454
# 7 ran_pars cond      hyena   sd__(Intercept)    1.33    NA         NA     NA           0.0173     0.553


########## 26.4 Do mobbers feed? Analysis by session ##########

##### Create dataset #####

head(sessions.mob.food[,c(6:8,10,11,14)]) 
sessions.mob.food.tmp <- filter(sessions.mob.food, complete.cases(sessions.mob.food[,c(6:8,10,11,14)]))   #n=586
sessions.mob.food.tmp$stan.age <- as.numeric(scale(sessions.mob.food.tmp$age, center = T, scale = T))
sessions.mob.food.tmp$stan.rank.og <- sessions.mob.food.tmp$stan.rank
sessions.mob.food.tmp$stan.rank <- as.numeric(scale(sessions.mob.food.tmp$stan.rank.og, center = T, scale = T))
summary(sessions.mob.food.tmp)
ggcorr(sessions.mob.food.tmp[,c(6:8,10,11,14)], label = T)     #all below 0.7

##### Global model - additive #####

#Full model
mod.feeds <- glmmTMB(feeds ~ poly(stan.age, 2) + sex + stan.rank + carc_cat + 
                       location + mobber.session + (1 | session) + (1 | hyena), 
                     data=sessions.mob.food.tmp, family = binomial(link = 'logit'))
check_collinearity(mod.feeds)    #all below 3
check_model(mod.feeds)

#Dredge
options(na.action = "na.fail")
mod.feeds <- glmmTMB(feeds ~ poly(stan.age, 2) + sex + stan.rank + carc_cat + 
                       location + mobber.session + (1 | session) + (1 | hyena), 
                     data=sessions.mob.food.tmp, family = binomial(link = 'logit'))
results.feeds <- dredge(mod.feeds)
get.models(subset(results.feeds, delta == 0), subset = T)
# feeds ~ sex + stan.rank + (1 | session)
importance(subset(results.feeds, delta <= 6 & !nested(.)))
#                      cond(stan.rank) cond(sex) cond(location) cond(poly(stan.age, 2))
# Sum of weights:      0.72            0.66      0.24           0.12                   
# N containing models:    4               2         2              1                   
options(na.action = "na.omit")

##### Global model - interactive #####

#Full model
mod.feeds <- glmmTMB(feeds ~ poly(stan.age, 2) + sex + stan.rank + carc_cat + 
                       location + mobber.session + 
                       mobber.session * stan.rank + 
                       mobber.session * carc_cat + 
                       mobber.session * location + 
                       (1 | session) + (1 | hyena), 
                     data=sessions.mob.food.tmp, family = binomial(link = 'logit'))
check_collinearity(mod.feeds)
#remove mobber.session * location
mod.feeds <- glmmTMB(feeds ~ poly(stan.age, 2) + sex + stan.rank + carc_cat + 
                       location + mobber.session + 
                       mobber.session * stan.rank + 
                       mobber.session * carc_cat + 
                       (1 | session) + (1 | hyena), 
                     data=sessions.mob.food.tmp, family = binomial(link = 'logit'))
check_collinearity(mod.feeds)    #all below 3
check_model(mod.feeds)
r2(mod.feeds)    #remove hyena random effect

#Dredge
options(na.action = "na.fail")
mod.feeds <- glmmTMB(feeds ~ poly(stan.age, 2) + sex + stan.rank + carc_cat + 
                       location + mobber.session + 
                       mobber.session * stan.rank + 
                       mobber.session * carc_cat + 
                       (1 | session), 
                     data=sessions.mob.food.tmp, family = binomial(link = 'logit'))
results.feeds <- dredge(mod.feeds)
get.models(subset(results.feeds, delta == 0), subset = T)
# feeds ~ sex + stan.rank + (1 | session) + (1 | hyena)
importance(subset(results.feeds, delta <= 6 & !nested(.)))
#                      cond(stan.rank) cond(sex) cond(location) cond(poly(stan.age, 2))
# Sum of weights:      0.72            0.66      0.24           0.13                   
# N containing models:    4               2         2              1                   
importance(subset(results.feeds, delta <= 2 & !nested(.)))
#                      cond(sex) cond(stan.rank)
# Sum of weights:      1.00      0.58           
# N containing models:    2         1           
options(na.action = "na.omit")

##### Best model #####

head(sessions.mob.food[,c(8,11)]) 
sessions.mob.food.final <- filter(sessions.mob.food, complete.cases(sessions.mob.food[,c(8,11)]))   #n=673
sessions.mob.food.final$stan.rank.og <- sessions.mob.food.final$stan.rank
sessions.mob.food.final$stan.rank <- as.numeric(scale(sessions.mob.food.final$stan.rank.og, center = T, scale = T))
summary(sessions.mob.food.final)

mod.feeds <- glmmTMB(feeds ~ sex + stan.rank + (1 | session), 
                     data=sessions.mob.food.final, family = binomial(link = 'logit'))
summary(mod.feeds)
#             Estimate Std. Error z value Pr(>|z|)  
# sexm        -0.65888    0.26376  -2.498   0.0125 *
# stan.rank    0.24196    0.13319   1.817   0.0693 .
check_collinearity(mod.feeds)    #all below 3
check_model(mod.feeds) 
simulationOutput <- simulateResiduals(fittedModel = mod.feeds, n = 250)
plot(simulationOutput)
model_performance(mod.feeds)
# AIC     |    AICc |     BIC | R2 (cond.) | R2 (marg.) |   ICC |  RMSE | Sigma | Log_loss | Score_log | Score_spherical
# ----------------------------------------------------------------------------------------------------------------------
# 782.416 | 782.475 | 800.462 |      0.409 |      0.048 | 0.379 | 0.395 | 1.000 |    0.479 |      -Inf |           0.004

#Table
sjPlot::tab_model(mod.feeds, show.se = T, show.ci = 0.95, show.re.var = F, show.intercept = F, 
                  pred.labels = c("Sex", "Social rank"),
                  dv.labels = c("Probability of feeding"), 
                  string.se = "SE", transform = "exp", file = "26.table_feed_all.doc")

#Calculate CIs using the likelihood profile
tidy.mod.feeds <- broom.mixed::tidy(mod.feeds, conf.method = "profile",
                                    conf.int = T, conf.level = 0.95, exponentiate = T)
tidy.mod.feeds
#   effect   component group   term            estimate std.error statistic     p.value conf.low conf.high
# 2 fixed    cond      NA      sexm               0.517     0.136    -2.50   0.0125   0.306      0.863
# 3 fixed    cond      NA      stan.rank          1.27      0.170     1.82   0.0693   0.982      1.66 
# 4 ran_pars cond      session sd__(Intercept)    1.42     NA        NA     NA        0.0700     0.630

