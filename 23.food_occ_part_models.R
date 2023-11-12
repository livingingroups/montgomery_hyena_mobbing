######################################################################
##### 23.0 Food-related mobbing models ##### 
######################################################################

########## 23.1 Set working directory & download packages/tables ##########

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

load("22.food_mobs.Rdata")

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


########## 23.2 Probability of mobbing occurrence - additive ##########

nrow(filter(sessions.food, carc_cat == "xl"))     #15
nrow(filter(sessions.food, carc_cat == "l"))     #131
nrow(filter(sessions.food, carc_cat == "m"))     #35

nrow(filter(sessions.food, carc_cat == "xl" & mobbing == T))     #7
nrow(filter(sessions.food, carc_cat == "l" & mobbing == T))     #58
nrow(filter(sessions.food, carc_cat == "m" & mobbing == T))     #17


########## 23.2 Probability of mobbing occurrence - additive ##########

##### Global model #####

#Standardize model variables
head(sessions.food[,c(4,6,11,12,13,15,18,20,23)])
sessions.food.tmp <- filter(sessions.food, complete.cases(sessions.food[,c(4,6,11,12,13,15,18,20,23)]))   #n=176
sessions.food.tmp$stan.num_sd_year <- as.numeric(scale(sessions.food.tmp$num_sd_year, center = T, scale = T))
sessions.food.tmp$stan.session_length <- as.numeric(scale(sessions.food.tmp$session_length, center = T, scale = T))
sessions.food.tmp$stan.hyena_count <- as.numeric(scale(sessions.food.tmp$hyena_count, center = T, scale = T))
sessions.food.tmp$stan.total_lions <- as.numeric(scale(sessions.food.tmp$total_lions, center = T, scale = T))
sessions.food.tmp$stan.num_indv_grts <- as.numeric(scale(sessions.food.tmp$num_indv_grts, center = T, scale = T))
sessions.food.tmp$stan.session.ai.avg <- as.numeric(scale(sessions.food.tmp$session.ai.avg, center = T, scale = T))
summary(sessions.food.tmp)
ggcorr(sessions.food.tmp[,c(4,6,11,12,13,15,18,20,23)], label = T)     #all below 0.7

#Full model
mod.prob.mob <- glmmTMB(mobbing ~ stan.session_length + stan.num_sd_year + 
                          stan.hyena_count + stan.total_lions + male_lions_present + 
                          stan.num_indv_grts + stan.session.ai.avg + location + carc_cat, 
                        data = sessions.food.tmp, family = binomial(link = 'logit'))
check_collinearity(mod.prob.mob)    #all below 3
check_model(mod.prob.mob)

#Dredge
options(na.action = "na.fail")
mod.prob.mob <- glmmTMB(mobbing ~ stan.session_length + stan.num_sd_year + 
                          stan.hyena_count + stan.total_lions + male_lions_present + 
                          stan.num_indv_grts + stan.session.ai.avg + location + carc_cat, 
                        data = sessions.food.tmp, family = binomial(link = 'logit'))
results.mob <- dredge(mod.prob.mob)
get.models(subset(results.mob, delta == 0), subset = T)
# mobbing ~ male_lions_present + stan.hyena_count + stan.num_indv_grts + stan.session.ai.avg
importance(subset(results.mob, delta <= 6 & !nested(.)))    
#                      cond(stan.hyena_count) cond(stan.num_indv_grts) cond(male_lions_present) cond(stan.session.ai.avg)
# Sum of weights:      0.98                   0.97                     0.72                     0.52                     
# N containing models:    5                      5                        4                        3                     
options(na.action = "na.omit")


########## 23.3 Probability of mobbing occurrence - interactions ##########

##### Global model #####

#Full model
mod.prob.mob <- glmmTMB(mobbing ~ stan.session_length + stan.num_sd_year + 
                          stan.hyena_count + stan.total_lions + male_lions_present + 
                          stan.num_indv_grts + stan.session.ai.avg + location + carc_cat + 
                          location * carc_cat + 
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
                        data = sessions.food.tmp, family = binomial(link = 'logit'))
check_collinearity(mod.prob.mob)
# remove interaction between location and carc_cat
mod.prob.mob <- glmmTMB(mobbing ~ stan.session_length + stan.num_sd_year + 
                          stan.hyena_count + stan.total_lions + male_lions_present + 
                          stan.num_indv_grts + stan.session.ai.avg + location + carc_cat + 
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
                        data = sessions.food.tmp, family = binomial(link = 'logit'))
check_collinearity(mod.prob.mob)     #all <=3
check_model(mod.prob.mob)

#Dredge
options(na.action = "na.fail")
mod.prob.mob <- glmmTMB(mobbing ~ stan.session_length + stan.num_sd_year + 
                          stan.hyena_count + stan.total_lions + male_lions_present + 
                          stan.num_indv_grts + stan.session.ai.avg + location + carc_cat + 
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
                        data = sessions.food.tmp, family = binomial(link = 'logit'))
results.prob.mob <- dredge(mod.prob.mob)
get.models(subset(results.prob.mob, delta == 0), subset = T)
# mobbing ~ location + male_lions_present + stan.hyena_count +  
#   stan.num_indv_grts + male_lions_present:stan.num_indv_grts
importance(subset(results.prob.mob, delta <= 6 & !nested(.)))    
#                      cond(stan.hyena_count) cond(stan.num_indv_grts) cond(male_lions_present)
# Sum of weights:      1.00                   1.00                     0.95                    
# N containing models:   11                     11                        8                    
#                      cond(male_lions_present:stan.num_indv_grts) cond(male_lions_present:stan.hyena_count)
# Sum of weights:      0.45                                        0.42                                     
# N containing models:    2                                           4                                     
#                      cond(location) cond(stan.hyena_count:stan.num_indv_grts) cond(stan.session.ai.avg)
# Sum of weights:      0.35           0.22                                      0.18                     
# N containing models:    2              3                                         3                     
importance(subset(results.prob.mob, delta <= 2 & !nested(.)))    
#                      cond(male_lions_present) cond(stan.hyena_count) cond(stan.num_indv_grts)
# Sum of weights:      1.00                     1.00                   1.00                    
# N containing models:    6                        6                      6                    
#                      cond(male_lions_present:stan.num_indv_grts) cond(male_lions_present:stan.hyena_count)
# Sum of weights:      0.52                                        0.48                                     
# N containing models:    2                                           4                                     
#                      cond(location) cond(stan.hyena_count:stan.num_indv_grts) cond(stan.session.ai.avg)
# Sum of weights:      0.41           0.24                                      0.14                     
# N containing models:    2              2                                         1                     
options(na.action = "na.omit")


########## 23.4 Probability of mobbing occurrence - best model ##########

##### Best model #####

head(sessions.food[,c(6,12,15,20)])
sessions.food.final <- filter(sessions.food, complete.cases(sessions.food[,c(6,12,15,20)]))   #n=218
sessions.food.final$stan.hyena_count <- as.numeric(scale(sessions.food.final$hyena_count, center = T, scale = T))
sessions.food.final$stan.num_indv_grts <- as.numeric(scale(sessions.food.final$num_indv_grts, center = T, scale = T))
summary(sessions.food.final)
ggcorr(sessions.food.final[,c(6,12,15,20)], label = T)    #all below 7

mod.prob.mob <- glmmTMB(mobbing ~ location + male_lions_present + stan.hyena_count +  
                          stan.num_indv_grts + male_lions_present:stan.num_indv_grts,
                        data = sessions.food.final, family = binomial(link = 'logit'))
summary(mod.prob.mob)
#                                         Estimate Std. Error z value Pr(>|z|)   
# locationk                                 -0.32333    0.42120  -0.768   0.4427    
# male_lions_presentTRUE                    -0.65082    0.34758  -1.872   0.0611 .  
# stan.hyena_count                           0.83815    0.19652   4.265    2e-05 ***
# stan.num_indv_grts                         0.09636    0.20484   0.470   0.6381    
# male_lions_presentTRUE:stan.num_indv_grts  0.99175    0.44131   2.247   0.0246 *  
check_collinearity(mod.prob.mob)
check_model(mod.prob.mob)
simulationOutput <- simulateResiduals(fittedModel = mod.prob.mob, n = 250)
plot(simulationOutput)
model_performance(mod.prob.mob)
# AIC     |    AICc |     BIC | R2 (cond.) | R2 (marg.) |  RMSE | Sigma | Log_loss | Score_log | Score_spherical
# --------------------------------------------------------------------------------------------------------------
# 257.689 | 258.087 | 277.996 |            |      0.327 | 0.440 | 1.000 |    0.564 |   -66.789 |           0.014
round(r2_tjur(mod.prob.mob), digits = 3)     #0.216

#Plot model
mod.prob.mob <- glmmTMB(mobbing ~ stan.hyena_count + male_lions_present + 
                          stan.num_indv_grts + location + male_lions_present:stan.num_indv_grts,
                        data = sessions.food.final, family = binomial(link = 'logit'))
summary(mod.prob.mob)

#Table
sjPlot::tab_model(mod.prob.mob, show.se = T, show.ci = 0.95, show.re.var = F, show.intercept = F, 
                  pred.labels = c("Number of hyenas present", "Male lions present [TRUE]", 
                                  "Number of hyenas who greet (greeters)", 
                                  "Carcass freshness [FRESH]",
                                  "Male lions present x Number of greeters"),
                  dv.labels = c("Probability of mobbing participation"), 
                  string.se = "SE", transform = "exp", file = "23.table_occ_food.doc")

#Calculate CIs using the likelihood profile
tidy.mod.prob.mob <- broom.mixed::tidy(mod.prob.mob, conf.method = "profile",
                                       conf.int = T, conf.level = 0.95, exponentiate = T)
tidy.mod.prob.mob
#  effect  component term                                      estimate std.error statistic     p.value conf.low conf.high
# 2 fixed  cond      stan.hyena_count                             2.31      0.454     4.26  0.0000200    1.60       3.46
# 3 fixed  cond      male_lions_presentTRUE                       0.522     0.181    -1.87  0.0611       0.261      1.03
# 4 fixed  cond      stan.num_indv_grts                           1.10      0.226     0.470 0.638        0.744      1.68
# 5 fixed  cond      locationk                                    0.724     0.305    -0.768 0.443        0.317      1.67
# 6 fixed  cond      male_lions_presentTRUE:stan.num_indv_grts    2.70      1.19      2.25  0.0246       1.20       6.89


########## 23.5 Probability of mobbing participation - additive ##########

##### Global model #####

#Standardize model variables
head(mobs.body[,c(6:8,10,11,13)])
mobs.body.tmp <- filter(mobs.body, complete.cases(mobs.body[,c(6:8,10,11,13)]))    #407
mobs.body.tmp$stan.age <- as.numeric(scale(mobs.body.tmp$age, center = T, scale = T))
mobs.body.tmp$stan.rank.og <- mobs.body.tmp$stan.rank
mobs.body.tmp$stan.rank <- as.numeric(scale(mobs.body.tmp$stan.rank.og, center = T, scale = T))
ggcorr(mobs.body.tmp[,c(6:8,10,11,13)], label = T)     #all below 0.7

#Full model
mod.body <- glmmTMB(mobber.session ~ poly(stan.age, 2) + sex + stan.rank + 
                      body.cond + location + carc_cat + (1|session) + (1|hyena),
                    data = mobs.body.tmp, family = binomial(link = 'logit'))
check_collinearity(mod.body)    #all below 3
check_model(mod.body)

#Dredge
options(na.action = "na.fail")
mod.body <- glmmTMB(mobber.session ~ poly(stan.age, 2) + sex + stan.rank + 
                      body.cond + location + carc_cat + (1|session) + (1|hyena),
                    data = mobs.body.tmp, family = binomial(link = 'logit'))
results.body <- dredge(mod.body)
get.models(subset(results.body, delta == 0), subset = T)
# mobber.session ~ body.cond + carc_cat + poly(stan.age, 2) + stan.rank + (1 | session) + (1 | hyena)
importance(subset(results.body, delta <= 6 & !nested(.)))    
#                      cond(stan.rank) cond(poly(stan.age, 2)) cond(carc_cat) cond(body.cond)
# Sum of weights:      1.00            0.88                    0.81           0.68           
# N containing models:    6               4                       4              3           


########## 23.6 Probability of mobbing participation - interactions ##########

#Full model
mod.body <- glmmTMB(mobber.session ~ poly(stan.age, 2) + sex + stan.rank + 
                      body.cond + location + carc_cat + 
                      body.cond * stan.rank + 
                      location * stan.rank + 
                      carc_cat * stan.rank + 
                      (1|session) + (1|hyena),
                    data = mobs.body.tmp, family = binomial(link = 'logit'))
check_collinearity(mod.body)
#remove terms based on collinearity - remove location * stan.rank
mod.body <- glmmTMB(mobber.session ~ poly(stan.age, 2) + sex + stan.rank + 
                      body.cond + location + carc_cat + 
                      body.cond * stan.rank + 
                      carc_cat * stan.rank + 
                      (1|session) + (1|hyena),
                    data = mobs.body.tmp, family = binomial(link = 'logit'))
check_collinearity(mod.body)    #all below 3
check_model(mod.body)

#Dredge
mod.body <- glmmTMB(mobber.session ~ poly(stan.age, 2) + sex + stan.rank + 
                      body.cond + location + carc_cat + 
                      body.cond * stan.rank + 
                      carc_cat * stan.rank + 
                      (1|session) + (1|hyena),
                    data = mobs.body.tmp, family = binomial(link = 'logit'))
results.body <- dredge(mod.body)
get.models(subset(results.body, delta == 0), subset = T)
# mobber.session ~ body.cond + carc_cat + poly(stan.age, 2) + stan.rank + (1 | session) + (1 | hyena)
importance(subset(results.body, delta <= 6 & !nested(.)))    
#                      cond(stan.rank) cond(poly(stan.age, 2)) cond(carc_cat) cond(body.cond)
# Sum of weights:      1.00            0.88                    0.81           0.68           
# N containing models:    6               4                       4              3           


########## 23.7 Probability of mobbing participation - best model ##########

##### Best model #####

head(mobs.body[,c(7,10,11,13)])
mobs.body.final <- filter(mobs.body, complete.cases(mobs.body[,c(7,10,11,13)]))    #407
mobs.body.final$stan.age <- as.numeric(scale(mobs.body.final$age, center = T, scale = T))
mobs.body.final$stan.rank.og <- mobs.body.final$stan.rank
mobs.body.final$stan.rank <- as.numeric(scale(mobs.body.final$stan.rank.og, center = T, scale = T))

mod.body <- glmmTMB(mobber.session ~ body.cond + carc_cat + poly(stan.age, 2) + stan.rank + 
                      (1 | session) + (1 | hyena), 
                    data = mobs.body.final, family = binomial(link = 'logit'))
summary(mod.body)
#                    Estimate Std. Error z value Pr(>|z|)    
# body.condfat        0.04427    0.34468   0.128   0.8978    
# body.condobese     -2.49946    1.12972  -2.212   0.0269 *  
# carc_catm           1.11553    1.00107   1.114   0.2651    
# carc_catxl         -2.63149    1.12703  -2.335   0.0195 *  
# poly(stan.age, 2)1  4.77917    2.65680   1.799   0.0720 .  
# poly(stan.age, 2)2 -5.68527    2.66221  -2.136   0.0327 *  
# stan.rank           0.72646    0.15664   4.638 3.52e-06 ***
check_collinearity(mod.body)   #all below 3
check_model(mod.body)
simulationOutput <- simulateResiduals(fittedModel = mod.body, n = 250)
plot(simulationOutput) 
model_performance(mod.body)
# AIC     |    AICc |     BIC | R2 (cond.) | R2 (marg.) |   ICC |  RMSE | Sigma | Log_loss | Score_log | Score_spherical
# ----------------------------------------------------------------------------------------------------------------------
# 502.070 | 502.626 | 542.158 |      0.434 |      0.181 | 0.309 | 0.399 | 1.000 |    0.486 |  -191.153 |           0.005
summary(glht_glmmTMB(mod.body, linfct = mcp(body.cond = "Tukey")))
#                     Estimate Std. Error z value Pr(>|z|)  
# fat - normal == 0    0.04427    0.34468   0.128   0.9902  
# obese - normal == 0 -2.49946    1.12972  -2.212   0.0603 .
# obese - fat == 0    -2.54373    1.13835  -2.235   0.0569 .
summary(glht_glmmTMB(mod.body, linfct = mcp(carc_cat = "Tukey")))
#             Estimate Std. Error z value Pr(>|z|)  
# m - l == 0     1.116      1.001   1.114   0.4928  
# xl - l == 0   -2.631      1.127  -2.335   0.0484 *
# xl - m == 0   -3.747      1.466  -2.555   0.0270 *
  
##### Plot model #####

mobs.body.final$mobber.session <- as.factor(mobs.body.final$mobber.session)
mod.body <- glmmTMB(mobber.session ~ body.cond + carc_cat + poly(stan.age, 2) + stan.rank + 
                      (1 | session) + (1 | hyena), 
                    data = mobs.body.final, family = binomial(link = 'logit'))
summary(mod.body)

#Age
summary(mobs.body.final$age)
dat.age <- ggeffects::ggpredict(mod.body, terms = c("stan.age [all]"), full.data = FALSE)
plot.age <- ggplot(dat.age, aes(x = dat.age$x*sd(mobs.body.final$age, na.rm = T) + mean(mobs.body.final$age, na.rm = T), 
                                y = predicted)) + 
  geom_line() + 
  geom_ribbon(aes(x = dat.age$x*sd(mobs.body.final$age, na.rm = T) + mean(mobs.body.final$age, na.rm = T), 
                  ymin = conf.low, ymax = conf.high), alpha = 0.2, inherit.aes = FALSE) +
  xlab('Age') +
  ylab("Predicted probability of mobbing")+
  ylab("")+
  theme_classic() + 
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        axis.title = element_text(size = 20), axis.text = element_text(size = 16))+
  ylim(c(0,1))
pdf('23.plot.age.pdf', width = 7, height = 5)
plot.age
dev.off()

#Rank
summary(mobs.body.final$stan.rank.og)
dat.rank <- ggeffects::ggpredict(mod.body, terms = c("stan.rank [all]"), full.data = FALSE)
plot.rank <- ggplot(dat.rank, aes(x = dat.rank$x*sd(mobs.body.final$stan.rank.og, na.rm = T) + 
                                    mean(mobs.body.final$stan.rank.og, na.rm = T), y = predicted)) + 
  geom_line() + 
  geom_ribbon(aes(x = dat.rank$x*sd(mobs.body.final$stan.rank.og, na.rm = T) + mean(mobs.body.final$stan.rank.og, na.rm = T), 
                  ymin = conf.low, ymax = conf.high), alpha = 0.2, inherit.aes = FALSE) +
  xlab('Social rank') +
  ylab("Predicted probability of mobbing")+
  theme_classic() + 
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        axis.title = element_text(size = 20), axis.text = element_text(size = 16))+
  ylim(c(0,1))
pdf('23.plot.rank.pdf', width = 7, height = 5)
plot.rank
dev.off()

#Body condition
dat.body <- ggeffects::ggpredict(mod.body, type = "fe", terms = c("body.cond [all]"), full.dat.rsa = FALSE)
dat.body$x <- factor(dat.body$x, levels = c("normal", "fat", "obese"))
plot.body <- ggplot(dat.body, aes(x = x, y = predicted)) + 
  geom_point(size = 5) + 
  geom_line(data = data.frame(x = c(dat.body$x, dat.body$x),
                              predicted = c(dat.body$predicted, dat.body$predicted),
                              ci = c(dat.body$conf.low, dat.body$conf.high)),
            aes(x = x, y = ci, group= x), size = 1.5)+
  xlab('Belly size') +
  ylab("Predicted probability of mobbing")+
  theme_classic() + 
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        axis.title = element_text(size = 20), axis.text = element_text(size = 16))+
  ylim(c(0,1))
pdf('23.plot.body.pdf', width = 7, height = 5)
plot.body
dev.off()

#Carcass size
dat.carc <- ggeffects::ggpredict(mod.body, type = "fe", terms = c("carc_cat [all]"), full.dat.rsa = FALSE)
dat.carc$x <- factor(dat.carc$x, levels = c("m", "l", "xl"), labels = c("medium", "large", "extra-large"))
plot.carc <- ggplot(dat.carc, aes(x = x, y = predicted)) + 
  geom_point(size = 5) + 
  geom_line(data = data.frame(x = c(dat.carc$x, dat.carc$x),
                              predicted = c(dat.carc$predicted, dat.carc$predicted),
                              ci = c(dat.carc$conf.low, dat.carc$conf.high)),
            aes(x = x, y = ci, group= x), size = 1.5)+
  xlab('Carcass size') +
  ylab("Predicted probability of mobbing") +
  theme_classic() + 
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        axis.title = element_text(size = 20), axis.text = element_text(size = 16))+
  ylim(c(0,1))
pdf('23.plot.carc.pdf', width = 7, height = 5)
plot.carc
dev.off()

#Model
mobs.body.final$stan.age.sq <- (mobs.body.final$stan.age)^2
mod.body <- glmmTMB(mobber.session ~ stan.age + stan.age.sq + stan.rank + body.cond + carc_cat +
                      (1 | session) + (1 | hyena), 
                    data = mobs.body.final, family = binomial(link = 'logit'))
summary(mod.body)
#                Estimate Std. Error z value Pr(>|z|)    
# stan.age        0.45344    0.16861   2.689  0.00716 ** 
# stan.age.sq    -0.18846    0.08825  -2.136  0.03272 *  
# stan.rank       0.72646    0.15664   4.638 3.52e-06 ***
# body.condfat    0.04427    0.34468   0.128  0.89780    
# body.condobese -2.49946    1.12971  -2.212  0.02693 *  
# carc_catm       1.11553    1.00107   1.114  0.26513    
# carc_catxl     -2.63150    1.12703  -2.335  0.01955 *  
set_theme(base = theme_classic(), axis.textcolor = "black", axis.title.color = "black", 
          axis.textsize.y = 1.5, axis.textsize.x = 1.2, axis.title.size = 1.7)
plot.model <- plot_model(mod.body, type = "est", transform = "exp",
                         axis.labels = c("Carcass size [xl]", "Carcass size [m]",
                                         "Belly size [obese]", "Belly size [fat]",
                                         "Social rank", "Age^2", "Age"),
                         vline.color = "black", title = "", dot.size = 4.5, line.size = 1.5,
                         show.values = TRUE, show.p = TRUE, digits = 2, value.offset = 0.3, 
                         value.size = 6, colors = viridis_2, axis.lim = c(0.001,1000))
pdf('23.plot.body.cond.model.pdf', width = 7, height = 5)
plot.model
dev.off()

#Table
sjPlot::tab_model(mod.body, show.se = T, show.ci = 0.95, show.re.var = F, show.intercept = F, 
                  pred.labels = c("Age", "Age^2", "Social rank",
                                  "Belly size [fat]", "Belly size [obese]", 
                                  "Carcass size [medium]", "Carcass size [extra-large]"),
                  dv.labels = c("Probability of mobbing participation"), 
                  string.se = "SE", transform = "exp", file = "23.table_body.cond.doc")

#Calculate CIs using the likelihood profile
tidy.mod.body <- broom.mixed::tidy(mod.body, conf.method = "profile",
                                   conf.int = T, conf.level = 0.95, exponentiate = T)
tidy.mod.body
#   effect   component group   term            estimate std.error statistic     p.value conf.low conf.high
# 2 fixed    cond      NA      stan.age          1.57      0.265      2.69   0.00716     1.13       2.21  
# 3 fixed    cond      NA      stan.age.sq       0.828     0.0731    -2.14   0.0327      0.685      0.975 
# 4 fixed    cond      NA      stan.rank         2.07      0.324      4.64   0.00000352  1.55       2.89  
# 5 fixed    cond      NA      body.condfat      1.05      0.360      0.128  0.898       0.530      2.07  
# 6 fixed    cond      NA      body.condobese    0.0821    0.0928    -2.21   0.0269      0.00676    0.641 
# 7 fixed    cond      NA      carc_catm         3.05      3.05       1.11   0.265       0.419     24.5   
# 8 fixed    cond      NA      carc_catxl        0.0720    0.0811    -2.33   0.0195      0.00635    0.630 
# 9 ran_pars cond      session sd__(Intercept)   1.14     NA         NA     NA          -0.324      0.559 
# 10 ran_pars cond      hyena   sd__(Intercept)   0.418    NA         NA     NA          NA         -0.0849




