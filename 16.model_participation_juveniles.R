######################################################################
##### 16.0 Modeling mobbing participation - juveniles #####
######################################################################

########## 16.1 Set working directory & download packages/tables ##########

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

#Create dataset
id.by.mob.juv <- filter(id.by.mob, age.class == "juvenile")     #1170


########## 16.2 Looking at combined data ##########

#Session-level 
table(id.by.mob.juv$mobber)
table(id.by.mob.juv$context)

#Outliers
dotchart(id.by.mob.juv$age)
dotchart(id.by.mob.juv$stan.rank)
dotchart(id.by.mob.juv$prop.mob.related)
dotchart(id.by.mob.juv$mob.ai.avg)
dotchart(id.by.mob.juv$present.prop.rank.higher)

#Distribution/zeros
hist(id.by.mob.juv$age)
hist(id.by.mob.juv$stan.rank)
hist(id.by.mob.juv$prop.mob.related)
hist(id.by.mob.juv$mob.ai.avg)
hist(id.by.mob.juv$present.prop.rank.higher)
table(id.by.mob.juv$grt_occ)
table(id.by.mob.juv$agg_occ)

#Collinear X variables
ggpairs(id.by.mob.juv[,c(11,14:15,19,21,23,25,28)])

#Create juvenile dataset
head(id.by.mob.juv[,c(1:10,11,14:15,18,20,23,25,28)])
id.by.mob.juv.all <- id.by.mob.juv[,c(1:10,11,14:15,18,20,23,25,28)]    #1170

#Fix outliers in AI - hk & sd littermates, wr & mh littermates 
id.by.mob.juv.all[id.by.mob.juv.all$hyena == "hk" & id.by.mob.juv.all$mobID == 336,]$mob.ai.avg <- NA
id.by.mob.juv.all[id.by.mob.juv.all$hyena == "sd" & id.by.mob.juv.all$mobID == 336,]$mob.ai.avg <- NA
id.by.mob.juv.all[id.by.mob.juv.all$hyena == "wr" & id.by.mob.juv.all$mobID == 205,]$mob.ai.avg <- NA
id.by.mob.juv.all[id.by.mob.juv.all$hyena == "mh" & id.by.mob.juv.all$mobID == 205,]$mob.ai.avg <- NA


########## 16.3 Additive model ##########

##### Create dataset #####

head(id.by.mob.juv.all[,c(11:18)])
ggcorr(id.by.mob.juv.all[,c(11:18)], label = T)
# stan.rank and present.prop.rank.higher are too correlated - drop present.prop.rank.higher

head(id.by.mob.juv.all[,c(11:17)])
id.by.mob.juv.tmp <- filter(id.by.mob.juv.all, complete.cases(id.by.mob.juv.all[,c(11:17)]))   #n=1139
id.by.mob.juv.tmp$stan.age <- as.numeric(scale(id.by.mob.juv.tmp$age, center = T, scale = T))
id.by.mob.juv.tmp$stan.rank.og <- id.by.mob.juv.tmp$stan.rank
id.by.mob.juv.tmp$stan.rank <- as.numeric(scale(id.by.mob.juv.tmp$stan.rank.og, center = T, scale = T))
id.by.mob.juv.tmp$stan.prop.mob.related <- as.numeric(scale(id.by.mob.juv.tmp$prop.mob.related, 
                                                        center = T, scale = T))
id.by.mob.juv.tmp$stan.mob.ai.avg <- as.numeric(scale(id.by.mob.juv.tmp$mob.ai.avg, 
                                                      center = T, scale = T))
summary(id.by.mob.juv.tmp)
ggcorr(id.by.mob.juv.tmp[,c(11:17)], label = T)     #all below 0.7

##### Global model - additive #####

#Full model
mod.juv <- glmmTMB(mobber ~ stan.age + stan.rank + sex + grt_occ + 
                     stan.prop.mob.related + stan.mob.ai.avg + (1|session/mobID) + (1|hyena), 
                   data = id.by.mob.juv.tmp, family = binomial(link = 'logit'))
check_collinearity(mod.juv)     #all below 3
check_model(mod.juv)

#Dredge
options(na.action = "na.fail")
mod.juv <- glmmTMB(mobber ~ stan.age + stan.rank + sex + grt_occ + 
                     stan.prop.mob.related + stan.mob.ai.avg + (1|session/mobID) + (1|hyena), 
                   data = id.by.mob.juv.tmp, family = binomial(link = 'logit'))
results.juv <- dredge(mod.juv)
get.models(subset(results.juv, delta == 0), subset = T)
# mobber ~ sex + stan.age + (1 | session/mobID) + (1 | hyena)
importance(subset(results.juv, delta <= 6 & !nested(.)))    
#                      cond(stan.age) cond(sex)
# Sum of weights:      1.00           0.53     
# N containing models:    2              1     
options(na.action = "na.omit")


########## 16.4 Interactive model ##########

##### Global model - interactive #####

#Full model
mod.juv <- glmmTMB(mobber ~ stan.age + stan.rank + sex + grt_occ + 
                     stan.prop.mob.related + stan.mob.ai.avg + 
                     stan.age * stan.rank + 
                     stan.age * sex + 
                     stan.rank * sex + (1|session/mobID) + (1|hyena), 
                   data = id.by.mob.juv.tmp, family = binomial(link = 'logit'))
check_collinearity(mod.juv)    #all below 3
check_model(mod.juv)

#Dredge
options(na.action = "na.fail")
mod.juv <- glmmTMB(mobber ~ stan.age + stan.rank + sex + grt_occ + 
                     stan.prop.mob.related + stan.mob.ai.avg + 
                     stan.age * stan.rank + 
                     stan.age * sex + 
                     stan.rank * sex + (1|session/mobID) + (1|hyena), 
                   data = id.by.mob.juv.tmp, family = binomial(link = 'logit'))
results.juv <- dredge(mod.juv)
get.models(subset(results.juv, delta == 0), subset = T)
# mobber ~ grt_occ + sex + stan.age + (1 | session/mobID) + (1 | hyena) + sex:stan.age
importance(subset(results.juv, delta <= 6 & !nested(.)))    
#             cond(stan.age) cond(sex) cond(sex:stan.age) cond(grt_occ)
# Sum of weights:      1.00           0.87      0.72               0.39         
# N containing models:    4              3         2                  1         
options(na.action = "na.omit")


########## 16.5 Final model and plots ##########

head(id.by.mob.juv.all[,c(11:12,14)])
id.by.mob.juv.final <- filter(id.by.mob.juv.all, complete.cases(id.by.mob.juv.all[,c(11:12,14)]))   #n=1153
id.by.mob.juv.final$stan.age <- as.numeric(scale(id.by.mob.juv.final$age, center = T, scale = T))
summary(id.by.mob.juv.final)

##### Best model #####

mod.juv <- glmmTMB(mobber ~ grt_occ + sex + stan.age + (1 | session/mobID) + (1 | hyena) + sex:stan.age,
                   data = id.by.mob.juv.final, family = binomial(link = 'logit'))
summary(mod.juv)
#               Estimate Std. Error z value Pr(>|z|)    
# grt_occTRUE     1.1673     0.7618   1.532   0.1255    
# sexm           -0.3550     0.3894  -0.912   0.3619    
# stan.age        1.7483     0.3335   5.243 1.58e-07 ***
# sexm:stan.age  -0.7127     0.4303  -1.656   0.0977 .  
check_collinearity(mod.juv)   #all below 3
check_model(mod.juv)
simulationOutput <- simulateResiduals(fittedModel = mod.juv, n = 250)
plot(simulationOutput)
model_performance(mod.juv)
# AIC     |    AICc |     BIC | R2 (cond.) | R2 (marg.) |   ICC |  RMSE | Sigma | Log_loss | Score_log | Score_spherical
# ----------------------------------------------------------------------------------------------------------------------
# 795.042 | 795.168 | 835.443 |      0.722 |      0.178 | 0.662 | 0.227 | 1.000 |    0.178 |      -Inf |           0.015


##### Plot models #####

id.by.mob.juv.final$mobber <- as.factor(id.by.mob.juv.final$mobber)
id.by.mob.juv.final$sex <- as.factor(id.by.mob.juv.final$sex)
id.by.mob.juv.final$grt_occ <- as.factor(id.by.mob.juv.final$grt_occ)
mod.juv <- glmmTMB(mobber ~ grt_occ + sex + stan.age + (1 | session/mobID) + (1 | hyena) + sex:stan.age,
                   data = id.by.mob.juv.final, family = binomial(link = 'logit'))
summary(mod.juv)

#Sex
dat.sex <- ggeffects::ggpredict(mod.juv, type = "fe", terms = c("sex"), full.data = FALSE)
dat.sex$x <- factor(dat.sex$x, labels = c("female", "male"))
plot.sex <- ggplot(dat.sex, aes(x = x, y = predicted)) + 
  geom_point(size = 5) + 
  geom_line(data = data.frame(x = c(dat.sex$x, dat.sex$x),
                              predicted = c(dat.sex$predicted, dat.sex$predicted),
                              ci = c(dat.sex$conf.low, dat.sex$conf.high)),
            aes(x = x, y = ci, group= x), size = 1.5)+
  xlab('Sex') +
  ylab("Predicted probability of mobbing")+
  theme_classic() + 
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        axis.title = element_text(size = 20), axis.text = element_text(size = 16))+
  ylim(c(0,1))
pdf('16.plot.juv.sex.pdf', width = 7, height = 5)
plot.sex
dev.off()

#Age
summary(id.by.mob.juv.final$age)
dat.age <- ggeffects::ggpredict(mod.juv, terms = c("stan.age [all]"), full.data = FALSE)
plot.age <- ggplot(dat.age, aes(x = dat.age$x*sd(id.by.mob.juv.final$age, na.rm = T) + 
                                  mean(id.by.mob.juv.final$age, na.rm = T), 
                                y = predicted)) + 
  geom_line(size = 1) + 
  geom_ribbon(aes(x = dat.age$x*sd(id.by.mob.juv.final$age, na.rm = T) + mean(id.by.mob.juv.final$age, na.rm = T), 
                  ymin = conf.low, ymax = conf.high), alpha = 0.2, inherit.aes = FALSE) +
  xlab('Age') +
  ylab("Predicted probability of mobbing")+
  theme_classic() + 
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        axis.title = element_text(size = 20), axis.text = element_text(size = 16))+
  ylim(c(0,1))
pdf('16.plot.juv.age.pdf', width = 7, height = 5)
plot.age
dev.off()

#Greetings
dat.grt <- ggeffects::ggpredict(mod.juv, type = "fe", terms = c("grt_occ"), full.data = FALSE)
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
pdf('16.plot.juv.grt.pdf', width = 7, height = 5)
plot.grt
dev.off()

#Plot of single variables
layout <- "A#
           BC"
plots <- plot.age + plot.sex + plot.grt + plot_layout(design = layout)
pdf('16.plots.all.pdf', width = 7, height = 5)
plots
dev.off()

#Interaction between age and sex
summary(id.by.mob.juv.final$age)
dat.intx.sex.age <- ggeffects::ggpredict(mod.juv, terms = c("stan.age [all]", "sex"), full.data = FALSE)
dat.intx.sex.age$group <- factor(dat.intx.sex.age$group, labels = c("female", "male"))
plot.sex.age <- ggplot(dat.intx.sex.age, aes(x = dat.intx.sex.age$x*sd(id.by.mob.juv.final$age, na.rm = T) + 
                                               mean(id.by.mob.juv.final$age, na.rm = T), 
                                             y = predicted, color = group, fill = group)) + 
  geom_ribbon(aes(x = dat.intx.sex.age$x*sd(id.by.mob.juv.final$age, na.rm = T) + 
                    mean(id.by.mob.juv.final$age, na.rm = T), 
                  ymin = conf.low, ymax = conf.high, fill = dat.intx.sex.age$group), alpha = 0.2, inherit.aes = FALSE) +
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
pdf('16.plot.juv.age.sex.pdf', width = 7, height = 5)
plot.sex.age
dev.off()

#Model
mod.juv <- glmmTMB(mobber ~ stan.age + sex + grt_occ + (1 | session/mobID) + (1 | hyena) + sex:stan.age,
                   data = id.by.mob.juv.final, family = binomial(link = 'logit'))
summary(mod.juv)
set_theme(base = theme_classic(), axis.textcolor = "black", axis.title.color = "black", 
          axis.textsize.y = 1.5, axis.textsize.x = 1.2, axis.title.size = 1.7)
plot.model <- plot_model(mod.juv, type = "est", transform = "exp",
                         axis.labels = c("Age x Sex", "Greeted [TRUE]",
                                         "Sex [male]", "Age"),
                         vline.color = "black", title = "", dot.size = 4.5, line.size = 1.5,
                         show.values = TRUE, show.p = TRUE, digits = 2, value.offset = 0.3, 
                         value.size = 6, colors = viridis_2, axis.lim = c(0.01,100))
pdf('16.plot.juv.model.pdf', width = 7, height = 5)
plot.model
dev.off()

#Table
sjPlot::tab_model(mod.juv, show.se = T, show.ci = 0.95, show.re.var = F, show.intercept = F, 
                  pred.labels = c("Age", "Sex [male]", "Greeted [TRUE]",
                                  "Age x Sex"),
                  dv.labels = c("Probability of mobbing participation"), 
                  string.se = "SE", transform = "exp", file = "16.table_juv.doc")

#Calculate CIs using the likelihood profile
tidy.mod.juv <- broom.mixed::tidy(mod.juv, conf.method = "profile",
                                  conf.int = T, conf.level = 0.95, exponentiate = T)
tidy.mod.juv
#   effect   component group         term           estimate std.error statistic     p.value conf.low conf.high
# 2 fixed    cond      NA            stan.age          5.75      1.92       5.24   1.58e-7   3.16      12.0  
# 3 fixed    cond      NA            sexm              0.701     0.273     -0.912  3.62e-1   0.317      1.51 
# 4 fixed    cond      NA            grt_occTRUE       3.21      2.45       1.53   1.25e-1   0.710     14.7  
# 5 fixed    cond      NA            stan.age:sexm     0.490     0.211     -1.66   9.77e-2   0.202      1.13 
# 6 ran_pars cond      mobID:session sd__(Intercept)   1.08     NA         NA     NA        -0.803      0.610
# 7 ran_pars cond      session       sd__(Intercept)   1.88     NA         NA     NA         0.151      1.03 
# 8 ran_pars cond      hyena         sd__(Intercept)   1.32     NA         NA     NA        -0.188      0.703





