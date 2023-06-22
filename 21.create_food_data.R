######################################################################
##### 21.0 Create food datasets for mobbing occurrence and participation #####
######################################################################

########## 21.1 Set working directory & download packages/tables ##########

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
load("08.sessions.intx.Rdata")
load("12.id_by_mob.Rdata")

#Remove datasets not used here
rm(sessions.all)


########## 21.2 Load additional required data ##########

#tblReproStates
data("tblReproStates")
tblReproStates$mom <- tolower(gsub(" ", "", tblReproStates$mom))
tblReproStates <- tblReproStates[,1:9]


########## 21.3 Mobbing occurrence - Sessions with food present ##########

#Sessions with food (use both food_present & context to eliminate 4 den sessions where food present)
sessions.food <- filter(sessions.intx, food_present == T & context == "fd")     #218 sessions
summary(sessions.food)

#Combine s and m carcasses due to sample sizes
sessions.food[sessions.food$carc_cat == "s" & !is.na(sessions.food$carc_cat),]$carc_cat <- "m"

#Re-level factors
sessions.food$carc_cat <- factor(sessions.food$carc_cat, ordered = F)
sessions.food$carc_cat <- relevel(sessions.food$carc_cat, ref = "l")
sessions.food$location <- as.factor(as.character(sessions.food$location))
summary(sessions.food)


########## 21.4 Mobbing participation - Adults with body condition information ##########

#Restrict to only food sessions
mobs.body <- filter(id.by.mob, session %in% sessions.food$Session)

#Add carcass size information
mobs.body <- left_join(mobs.body, sessions.food[,c(1,6,18)], by = c("session" = "Session"))

#Re-format data by session instead of by mob
#Calculate number of mobs per individual within each session
mobs.body$num.mobs.indv <- NA
mobs.body$total.mobs.present <- NA
mobs.body$total.mobs.session <- NA
for(i in 1:nrow(mobs.body)){
  session.i <- mobs.body$session[i]
  hyena.i <- mobs.body$hyena[i]
  id.by.mob.i <- filter(mobs.body, session == session.i & hyena == hyena.i)
  #Number of times that individual mobbed
  mobs.body$num.mobs.indv[i] <- length(unique(filter(id.by.mob.i, mobber == T)$mobID))
  #Number of mobs that individual was present for
  mobs.body$total.mobs.present[i] <- length(unique(id.by.mob.i$mobID)) 
  #Number of mobs in session overall
  mobs.body$total.mobs.session[i] <- length(unique(filter(mobs.body, session == session.i)$mobID))
}
rm(session.i)
rm(hyena.i)
rm(id.by.mob.i)

mobs.body$mobber.session <- ifelse(mobs.body$num.mobs.indv > 0, TRUE, FALSE)
mobs.body$prop.mob.present <- mobs.body$num.mobs.indv/mobs.body$total.mobs.present
mobs.body$prop.mob.session <- mobs.body$num.mobs.indv/mobs.body$total.mobs.session

#Only keep one line per hyena in each sessions - eliminate mob-specific columns
head(mobs.body[,c(1,3,7:9,34,35,11,12,14:16,22,29,39:41)])
mobs.body <- mobs.body[,c(1,3,7:9,34,35,11,12,14:16,22,29,39:41)]
mobs.body <- unique(mobs.body)    #1133
nrow(unique(mobs.body[,c(1:2)]))   #1133 

#Only keep adults
mobs.body <- filter(mobs.body, age.class == "adult")

#Eliminate "fat" hyenas who are really pregnant
summary(mobs.body$body.cond)
for(i in 1:nrow(mobs.body)){
  #If hyena is pregnant
  if(!is.na(mobs.body$repro.state[i]) & mobs.body$repro.state[i] == "p"){
    hyena.i <- mobs.body$hyena[i]
    date.i <- mobs.body$date[i]
    #If hyena is in the third trimester
    if(filter(tblReproStates, mom == hyena.i & 
              cycle.start <= date.i & 
              cycle.stop >= date.i)$trimester == 3){
      #Change body condition from fat to normal
      if(mobs.body$body.cond[i] == "fat"){
        mobs.body$body.cond[i] <- "normal"
      }
    }
    rm(hyena.i)
    rm(date.i)
  }
}
summary(mobs.body$body.cond)   #changed 2 lines

#Fix unbalanced factor levels for body.cond (only 2 gaunt hyenas)
mobs.body[mobs.body$body.cond == "gaunt" & !is.na(mobs.body$body.cond),]$body.cond <- "normal"

#Only keep sessions where observers noted body condition
keep <- filter(mobs.body, body.cond != "normal")
length(unique(keep$session))    #37 sessions
mobs.body <- filter(mobs.body, session %in% keep$session)
rm(keep)
summary(mobs.body)    #484

#Relevel factors
mobs.body$body.cond <- factor(mobs.body$body.cond, ordered = F)
mobs.body$body.cond <- relevel(mobs.body$body.cond, ref = "normal")
summary(mobs.body)


########## 21.5 Save data ##########

#Save data
save(file = "22.food_mobs.Rdata", list = c("sessions.food", "mobs.body"))



