######################################################################
##### 24.0 Create mobbing/feeding datasets #####
######################################################################

########## 24.1 Set working directory & download packages/tables ##########

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
load("06.lh_data_behav_food.Rdata")
load("08.sessions.intx.Rdata")
load("12.id_by_mob.Rdata")

#Remove datasets not used here
rm(sessions.all)


########## 24.2 Load additional required data ##########

#tblFeedingScans
data("tblFeedingScans")
tblFeedingScans$session <- gsub(" ", "", tblFeedingScans$session)
tblFeedingScans$unidentified_scrap <- as.logical(tblFeedingScans$unidentified_scrap)
tblFeedingScans$no_scans_with_feeding <- as.logical(tblFeedingScans$no_scans_with_feeding)
tblFeedingScans$scan_time <- as.POSIXct(tblFeedingScans$scan_time, format = "%H:%M:%S")
tblFeedingScans$food_size <- gsub(" ", "", tblFeedingScans$food_size)
tblFeedingScans$food_size <- as.factor(tblFeedingScans$food_size)
tblFeedingScans$food_size <- factor(tblFeedingScans$food_size, order = TRUE, levels = c("xs","s","m","l","xl"))

#Fix time columns in all datasets
id.by.mob$time <- format(id.by.mob$time, "%H:%M:%S")
id.by.mob$time <- as.POSIXct(id.by.mob$time, format = "%H:%M:%S")

lh.indvbehav.food$time <- format(lh.indvbehav.food$time, "%H:%M:%S")
lh.indvbehav.food$time <- as.POSIXct(lh.indvbehav.food$time, format = "%H:%M:%S")

tblFeedingScans$scan_time <- format(tblFeedingScans$scan_time, "%H:%M:%S")
tblFeedingScans$scan_time <- as.POSIXct(tblFeedingScans$scan_time, format = "%H:%M:%S")


########## 24.3 Combine feeding datasets to maximize observations of feeding ##########

#lh.indvbehav.food = feeding.occ
#This dataset is all instances of feeding recorded while lions are present

#Clean and remove duplicates
feeding.occ <- filter(lh.indvbehav.food, behav_cat == "fd" & session %in% id.by.mob$session)
head(feeding.occ[,c(2,3,4,13:16,18)])
feeding.occ <- feeding.occ[,c(2,3,4,13:16,18)]
feeding.occ <- filter(feeding.occ, !is.na(session) & !is.na(hyena) & !is.na(time) & 
                        !is.na(behav_cat) & !is.na(food_size))
colnames(feeding.occ)[4] <- "behavior"
feeding.occ <- unique(feeding.occ)     #remove 39
summary(feeding.occ)     #816

#tblFeedingScans = feeding.sc
#This dataset is all scans performed at kill or carcass sessions

#Clean and format
feeding.sc <- filter(tblFeedingScans, !is.na(behavior) & (behavior == "fd" | behavior == "scrap"
                                                          | behavior == "bone" | behavior == "carry"))
feeding.sc <- filter(feeding.sc, session %in% id.by.mob$session)
head(feeding.sc[,c(1,5,4,7:9)])
feeding.sc <- feeding.sc[,c(1,5,4,7:9)]
colnames(feeding.sc)[2] <- "hyena"
colnames(feeding.sc)[3] <- "time"
colnames(feeding.sc)[5] <- "food_size.og"

#Add prey data
feeding.sc <- left_join(feeding.sc, lh.sessions.food[,c(1,14:16)], by = c("session" = "Session"))
summary(feeding.sc)

#Food_size in tblFeedingScans is the size of the remaining carcass, not the size of the food that each hyena has
#Fix food_size for tblFeedingScans so it matches feeding.occ
# Behavior:     small 	medium 	large 	extra-large
# scrap / bone  xs      xs    	xs      xs
# carry 	      xs      s	      s	      m
# fd	          s       m	      l	      xl
feeding.sc$food_size <- NA
for(i in 1:nrow(feeding.sc)){
  #Extra-large carcasses (buffalo, giraffe, hippo of all ages)
  if(!is.na(feeding.sc$carc_cat[i]) & feeding.sc$carc_cat[i] == "xl" &
     !is.na(feeding.sc$behavior[i]) & feeding.sc$behavior[i] == "fd"){
    feeding.sc$food_size[i] <- "xl"
  }
  if(!is.na(feeding.sc$carc_cat[i]) & feeding.sc$carc_cat[i] == "xl" &
     !is.na(feeding.sc$behavior[i]) & feeding.sc$behavior[i] == "carry"){
    feeding.sc$food_size[i] <- "m"
  }
  if(!is.na(feeding.sc$carc_cat[i]) & feeding.sc$carc_cat[i] == "xl" &
     !is.na(feeding.sc$behavior[i]) & (feeding.sc$behavior[i] == "scrap" | feeding.sc$behavior[i] == "bone")){
    feeding.sc$food_size[i] <- "xs"
  }
  #Large carcasses (cow, topi, wildebeest, zebra)
  if(!is.na(feeding.sc$carc_cat[i]) & feeding.sc$carc_cat[i] == "l" &
     !is.na(feeding.sc$behavior[i]) & feeding.sc$behavior[i] == "fd"){
    feeding.sc$food_size[i] <- "l"
  }
  if(!is.na(feeding.sc$carc_cat[i]) & feeding.sc$carc_cat[i] == "l" &
     !is.na(feeding.sc$behavior[i]) & feeding.sc$behavior[i] == "carry"){
    feeding.sc$food_size[i] <- "s"
  }
  if(!is.na(feeding.sc$carc_cat[i]) & feeding.sc$carc_cat[i] == "l" &
     !is.na(feeding.sc$behavior[i]) & (feeding.sc$behavior[i] == "scrap" | feeding.sc$behavior[i] == "bone")){
    feeding.sc$food_size[i] <- "xs"
  }
  #Medium carcasses (gazelle, impala, shoat, warthog; also juvenile: cow, topi, wildebeest, zebra)
  if(!is.na(feeding.sc$carc_cat[i]) & feeding.sc$carc_cat[i] == "m" &
     !is.na(feeding.sc$behavior[i]) & feeding.sc$behavior[i] == "fd"){
    feeding.sc$food_size[i] <- "m"
  }
  if(!is.na(feeding.sc$carc_cat[i]) & feeding.sc$carc_cat[i] == "m" &
     !is.na(feeding.sc$behavior[i]) & feeding.sc$behavior[i] == "carry"){
    feeding.sc$food_size[i] <- "s"
  }
  if(!is.na(feeding.sc$carc_cat[i]) & feeding.sc$carc_cat[i] == "m" &
     !is.na(feeding.sc$behavior[i]) & (feeding.sc$behavior[i] == "scrap" | feeding.sc$behavior[i] == "bone")){
    feeding.sc$food_size[i] <- "xs"
  }
  #Small carcasses (juvenile: gazelle, impala, shoat, warthog)
  if(!is.na(feeding.sc$carc_cat[i]) & feeding.sc$carc_cat[i] == "s" &
     !is.na(feeding.sc$behavior[i]) & feeding.sc$behavior[i] == "fd"){
    feeding.sc$food_size[i] <- "s"
  }
  if(!is.na(feeding.sc$carc_cat[i]) & feeding.sc$carc_cat[i] == "s" &
     !is.na(feeding.sc$behavior[i]) & feeding.sc$behavior[i] == "carry"){
    feeding.sc$food_size[i] <- "xs"
  }
  if(!is.na(feeding.sc$carc_cat[i]) & feeding.sc$carc_cat[i] == "s" &
     !is.na(feeding.sc$behavior[i]) & (feeding.sc$behavior[i] == "scrap" | feeding.sc$behavior[i] == "bone")){
    feeding.sc$food_size[i] <- "xs"
  }
  #Unknown carcasses (assume carc_cat = m)
  if(is.na(feeding.sc$carc_cat[i]) & feeding.sc$prey_type[i] == "unknown" &
     !is.na(feeding.sc$behavior[i]) & feeding.sc$behavior[i] == "fd"){
    feeding.sc$food_size[i] <- "m"
  }
  if(is.na(feeding.sc$carc_cat[i]) & feeding.sc$prey_type[i] == "unknown" &
     !is.na(feeding.sc$behavior[i]) & feeding.sc$behavior[i] == "carry"){
    feeding.sc$food_size[i] <- "s"
  }
  if(is.na(feeding.sc$carc_cat[i]) & feeding.sc$prey_type[i] == "unknown" &
     !is.na(feeding.sc$behavior[i]) & (feeding.sc$behavior[i] == "scrap" | feeding.sc$behavior[i] == "bone")){
    feeding.sc$food_size[i] <- "xs"
  }
}
feeding.sc$food_size <- as.factor(feeding.sc$food_size)
feeding.sc$food_size <- ordered(feeding.sc$food_size, levels = c("xs", "s", "m", "l", "xl"))
cor.test(as.numeric(feeding.sc$food_size), as.numeric(feeding.sc$food_size.og))    #0.8215914

#Clean and remove duplicates
feeding.sc <- filter(feeding.sc, !is.na(session) & !is.na(hyena) & !is.na(time) & 
                       !is.na(behavior) & !is.na(food_size))
feeding.sc <- feeding.sc[,c(1:4,7:10)]
feeding.sc <- unique(feeding.sc)
summary(feeding.sc)     #620

#Combine feeding datasets = feeding.all
feeding.all <- rbind(feeding.occ, feeding.sc)
feeding.all <- feeding.all[,c(1:3,8)]
feeding.all <- unique(feeding.all)
feeding.all$food_size_num <- as.numeric(feeding.all$food_size)
summary(feeding.all)     #1287

rm(feeding.occ)
rm(feeding.sc)


########## 24.4 Mobbing benefits - create id.by.mob.food (feeding per mob) ##########

#Join datasets
head(id.by.mob[,c(1:10,11,12,14:16,18,20,23,25,29)])
head(sessions.intx[,c(1,6,17,18)])
id.by.mob.food <- left_join(id.by.mob[,c(1:10,11,12,14:16,18,20,23,25,29)], sessions.intx[,c(1,6,17,18)], 
                                by = c("session" = "Session"))

#Only adults
id.by.mob.food <- filter(id.by.mob.food, age.class != "juvenile")       #3565

#Add food_size eaten within 5 minutes of mob
id.by.mob.food$food_size <- NA
for(i in 1:nrow(id.by.mob.food)){
  session.i <- id.by.mob.food$session[i]
  hyena.i <- id.by.mob.food$hyena[i]
  time.i <- id.by.mob.food$time[i]
  #Filter to feeding within 5 minutes of the mobbing time
  feeding.i <- filter(feeding.all, session == session.i & hyena == hyena.i & 
                        (time >= time.i & time <= (time.i + 300)))   #300 seconds = 5 minutes
  if(nrow(feeding.i) > 0){
    #Assign largest food size 
    food_size.i <- sort(feeding.i$food_size_num, decreasing = T)
    id.by.mob.food$food_size[i] <- food_size.i[1]
    rm(food_size.i)
  }
  if(nrow(feeding.i) == 0){
    id.by.mob.food$food_size[i] <- 0
  }
  rm(feeding.i)
}
rm(session.i)
rm(hyena.i)
rm(time.i)

#Clean food_size column
id.by.mob.food[id.by.mob.food$food_size == 0 & !is.na(id.by.mob.food$food_size),]$food_size <- "none"
id.by.mob.food[id.by.mob.food$food_size == 1 & !is.na(id.by.mob.food$food_size),]$food_size <- "xs"
id.by.mob.food[id.by.mob.food$food_size == 2 & !is.na(id.by.mob.food$food_size),]$food_size <- "s"
id.by.mob.food[id.by.mob.food$food_size == 3 & !is.na(id.by.mob.food$food_size),]$food_size <- "m"
id.by.mob.food[id.by.mob.food$food_size == 4 & !is.na(id.by.mob.food$food_size),]$food_size <- "l"
id.by.mob.food[id.by.mob.food$food_size == 5 & !is.na(id.by.mob.food$food_size),]$food_size <- "xl"
id.by.mob.food$food_size <- as.factor(id.by.mob.food$food_size)
id.by.mob.food$food_size <- ordered(id.by.mob.food$food_size, levels = c("none", "xs", "s", "m", "l", "xl"))

#Add T/F for if hyena feeds 
id.by.mob.food$feeds <- NA
for(i in 1:nrow(id.by.mob.food)){
  if(id.by.mob.food$food_size[i] == "none"){
    id.by.mob.food$feeds[i] <- FALSE
  }
  if(id.by.mob.food$food_size[i] == "xs" | id.by.mob.food$food_size[i] == "s" |
     id.by.mob.food$food_size[i] == "m" | id.by.mob.food$food_size[i] == "l" |
     id.by.mob.food$food_size[i] == "xl"){
    id.by.mob.food$feeds[i] <- TRUE
  }
}
summary(id.by.mob.food)

#Only keep mobs where at least one hyena eats something
keep <- filter(id.by.mob.food, feeds == TRUE)   #319 feeding instances
id.by.mob.food <- filter(id.by.mob.food, mobID %in% keep$mobID)    #1187
rm(keep)
summary(id.by.mob.food)

#Fix unbalanced factor levels for carc_cat - collapse small (3 observations) into medium
id.by.mob.food[id.by.mob.food$carc_cat == "s" & !is.na(id.by.mob.food$carc_cat),]$carc_cat <- "m"
id.by.mob.food$carc_cat <- as.factor(as.character(id.by.mob.food$carc_cat))
id.by.mob.food$carc_cat <- relevel(id.by.mob.food$carc_cat, ref = "l")

#Relevel location
id.by.mob.food$location <- as.factor(as.character(id.by.mob.food$location))
summary(id.by.mob.food)


########## 24.5 Create sessions.mob.food (feeding per session) ##########

sessions.mob.food <- left_join(id.by.mob, sessions.intx[,c(1,6,17,18)], 
                            by = c("session" = "Session"))

#Only adults
sessions.mob.food <- filter(sessions.mob.food, age.class != "juvenile")       #3565

#Re-format data by session instead of by mob
#Calculate number of mobs per individual within each session
sessions.mob.food$num.mobs.indv <- NA
sessions.mob.food$total.mobs.present <- NA
sessions.mob.food$total.mobs.session <- NA
for(i in 1:nrow(sessions.mob.food)){
  session.i <- sessions.mob.food$session[i]
  hyena.i <- sessions.mob.food$hyena[i]
  sessions.mob.food.i <- filter(sessions.mob.food, session == session.i & hyena == hyena.i)
  #Number of times that individual mobbed
  sessions.mob.food$num.mobs.indv[i] <- length(unique(filter(sessions.mob.food.i, mobber == T)$mobID))
  #Number of mobs that individual was present for
  sessions.mob.food$total.mobs.present[i] <- length(unique(sessions.mob.food.i$mobID)) 
  #Number of mobs in session overall
  sessions.mob.food$total.mobs.session[i] <- length(unique(filter(sessions.mob.food, session == session.i)$mobID))
}
rm(session.i)
rm(hyena.i)
rm(sessions.mob.food.i)

sessions.mob.food$mobber.session <- ifelse(sessions.mob.food$num.mobs.indv > 0, TRUE, FALSE)
sessions.mob.food$prop.mob.present <- sessions.mob.food$num.mobs.indv/sessions.mob.food$total.mobs.present
sessions.mob.food$prop.mob.session <- sessions.mob.food$num.mobs.indv/sessions.mob.food$total.mobs.session

#Time window of mobs
sessions.mob.food$start.mob <- NA
sessions.mob.food$start.mob <- as.POSIXct(sessions.mob.food$start.mob, format = "%Y-%m-%d %H:%M:%S")
sessions.mob.food$stop.mob <- NA
sessions.mob.food$stop.mob <- as.POSIXct(sessions.mob.food$stop.mob, format = "%Y-%m-%d %H:%M:%S")
for(i in 1:nrow(sessions.mob.food)){
  session.i <- sessions.mob.food$session[i]
  sessions.mob.food.i <- filter(sessions.mob.food, session == session.i)
  sessions.mob.food.start <- sort(sessions.mob.food.i$time, decreasing = F)
  sessions.mob.food$start.mob[i] <- sessions.mob.food.start[1]
  sessions.mob.food.stop <- sort(sessions.mob.food.i$time, decreasing = T)
  sessions.mob.food$stop.mob[i] <- sessions.mob.food.stop[1]
}
rm(session.i)
rm(sessions.mob.food.i)
rm(sessions.mob.food.start)
rm(sessions.mob.food.stop)

#Only keep one line per hyena in each sessions
head(sessions.mob.food[,c(1,3,7:9,34,36,11,12,14:16,29,40:44)])
sessions.mob.food <- sessions.mob.food[,c(1,3,7:9,34,36,11,12,14:16,29,40:44)]
sessions.mob.food <- unique(sessions.mob.food)    #1231
summary(sessions.mob.food)

#Add food_size (largest food_size individual recorded as eating in that session)
sessions.mob.food$food_size <- NA
for(i in 1:nrow(sessions.mob.food)){
  session.i <- sessions.mob.food$session[i]
  hyena.i <- sessions.mob.food$hyena[i]
  start.time.i <- sessions.mob.food$start.mob[i]
  stop.time.i <- sessions.mob.food$stop.mob[i]
  #Any feeding within 30min after the mob
  behav.food.i <- filter(feeding.all, session == session.i & hyena == hyena.i & 
                           (time >= start.time.i & time <= stop.time.i + 1800))   #1800 seconds = 30 minutes
  if(nrow(behav.food.i) > 0){ 
    if(any(!is.na(behav.food.i$food_size_num))){
      food_size.i <- sort(behav.food.i$food_size_num, decreasing = T)
      sessions.mob.food$food_size[i] <- food_size.i[1]
      rm(food_size.i)
    }
  }
  if(nrow(behav.food.i) == 0){ 
    sessions.mob.food$food_size[i] <- 0
  }
  rm(behav.food.i)
}
rm(session.i)
rm(hyena.i)
rm(start.time.i)
rm(stop.time.i)

#Clean factor data for food_size
sessions.mob.food[sessions.mob.food$food_size == 0 & !is.na(sessions.mob.food$food_size),]$food_size <- "none"
sessions.mob.food[sessions.mob.food$food_size == 1 & !is.na(sessions.mob.food$food_size),]$food_size <- "xs"
sessions.mob.food[sessions.mob.food$food_size == 2 & !is.na(sessions.mob.food$food_size),]$food_size <- "s"
sessions.mob.food[sessions.mob.food$food_size == 3 & !is.na(sessions.mob.food$food_size),]$food_size <- "m"
sessions.mob.food[sessions.mob.food$food_size == 4 & !is.na(sessions.mob.food$food_size),]$food_size <- "l"
sessions.mob.food[sessions.mob.food$food_size == 5 & !is.na(sessions.mob.food$food_size),]$food_size <- "xl"
sessions.mob.food$food_size <- as.factor(sessions.mob.food$food_size)
sessions.mob.food$food_size <- ordered(sessions.mob.food$food_size, levels = c("none", "xs", "s", "m", "l", "xl"))

#Add T/F for if hyena feeds based on food_size
sessions.mob.food$feeds <- NA
for(i in 1:nrow(sessions.mob.food)){
  if(sessions.mob.food$food_size[i] == "none"){
    sessions.mob.food$feeds[i] <- FALSE
  }
  if(sessions.mob.food$food_size[i] == "xs" | sessions.mob.food$food_size[i] == "s" |
     sessions.mob.food$food_size[i] == "m" | sessions.mob.food$food_size[i] == "l" |
     sessions.mob.food$food_size[i] == "xl"){
    sessions.mob.food$feeds[i] <- TRUE
  }
}

#Only keep sessions where at least one hyena eats something
keep <- filter(sessions.mob.food, feeds == T)     #283
sessions.mob.food <- filter(sessions.mob.food, session %in% keep$session)    #694
rm(keep)
summary(sessions.mob.food)

#Fix unbalanced factor levels for carc_cat - collapse small (3 observations) into medium
sessions.mob.food[sessions.mob.food$carc_cat == "s" & !is.na(sessions.mob.food$carc_cat),]$carc_cat <- "m"
sessions.mob.food$carc_cat <- as.factor(as.character(sessions.mob.food$carc_cat))
sessions.mob.food$carc_cat <- relevel(sessions.mob.food$carc_cat, ref = "l")

#Relevel location
sessions.mob.food$location <- as.factor(as.character(sessions.mob.food$location))
summary(sessions.mob.food)


########## 24.5 Save data ##########

#Save data
save(file = "25.feeding_benefits.Rdata", list = c("id.by.mob.food", "sessions.mob.food"))



