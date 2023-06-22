################################################################################
##### 5.0 Cleaning lion-hyena behavior and feeding data #####
################################################################################

########## 5.1 Set working directory & download packages/tables ##########

rm(list = ls())
setwd("~/Documents/R/LionHyena")
options(stringsAsFactors = FALSE)
library(tidyverse)
hyenadata::update_tables("1.2.88")
library(hyenadata)

#Load data
load("02.cleaned_lh_data.Rdata")
load("04.lh_data_mob.Rdata")

#Remove unwanted datasets
rm(lh.indvinfo)
rm(lh.indvsessions)
rm(lh.intx)
rm(lh.intx.all)
rm(lh.intx.checked)
rm(lh.intx.mob)
rm(lh.sessions.all)
rm(lh.sessions.checked)

#tblEats
data("tblEats")
tblEats <- tblEats[,1:5]


########## 5.2 Assign prey_type for all sessions with food_present ##########

#Join with tblEats (prey_type, prey_age, etc)
lh.indvbehav <- left_join(lh.indvbehav, tblEats, by = c("Session" = "session"))

#Check sessions with prey_type that have food_present == F
check.prey <- filter(lh.indvbehav, !is.na(prey_type))
check.prey <- filter(check.prey, Session %in% filter(lh.sessions, food_present == F)$Session)
length(unique(check.prey$Session))         #5 sessions
unique(check.prey[,c(2,6,11,12,15)])     #all scraps, not real food - okay
rm(check.prey)

#Fill in prey_type for sessions missing prey_type
unique(filter(lh.indvbehav, is.na(lh.indvbehav$prey_type))$Food)
unique(filter(lh.indvbehav, is.na(prey_type) & 
                grepl("top", lh.indvbehav$Food))$Food)
lh.indvbehav[is.na(lh.indvbehav$prey_type) & 
               grepl("top", lh.indvbehav$Food),]$prey_type <- "topi"

unique(filter(lh.indvbehav, is.na(prey_type) & 
                grepl("cow", lh.indvbehav$Food))$Food)
lh.indvbehav[is.na(lh.indvbehav$prey_type) & 
               grepl("cow", lh.indvbehav$Food),]$prey_type <- "cow"

unique(filter(lh.indvbehav, is.na(prey_type) & 
                grepl("gnu", lh.indvbehav$Food))$Food)
lh.indvbehav[is.na(lh.indvbehav$prey_type) & 
               grepl("gnu", lh.indvbehav$Food),]$prey_type <- "wildebeest"

unique(filter(lh.indvbehav, is.na(prey_type) & 
                grepl("wildebeast", lh.indvbehav$Food))$Food)
lh.indvbehav[is.na(lh.indvbehav$prey_type) & 
               grepl("wildebeast", lh.indvbehav$Food),]$prey_type <- "wildebeest"

unique(filter(lh.indvbehav, is.na(prey_type) & 
                grepl("hippo", lh.indvbehav$Food))$Food)
lh.indvbehav[is.na(lh.indvbehav$prey_type) & 
               grepl("hippo", lh.indvbehav$Food),]$prey_type <- "hippo"

unique(filter(lh.indvbehav, is.na(prey_type) & 
                grepl("zebra", lh.indvbehav$Food))$Food)
lh.indvbehav[is.na(lh.indvbehav$prey_type) & 
               grepl("zebra", lh.indvbehav$Food),]$prey_type <- "zebra"

unique(filter(lh.indvbehav, is.na(prey_type) & 
                grepl("tommy", lh.indvbehav$Food))$Food)
lh.indvbehav[is.na(lh.indvbehav$prey_type) & 
               grepl("tommy", lh.indvbehav$Food),]$prey_type <- "gazelle"

unique(filter(lh.indvbehav, is.na(prey_type) & 
                (grepl("small unbgualte", lh.indvbehav$Food) | 
                   grepl("small ungualte", lh.indvbehav$Food)))$Food)
lh.indvbehav[is.na(lh.indvbehav$prey_type) & 
               (grepl("small unbgualte", lh.indvbehav$Food) | 
                  grepl("small ungualte", lh.indvbehav$Food)),]$prey_type <- "gazelle"

#Check if same prey_type for all lines in a session
check.prey_type <- filter(lh.indvbehav, !is.na(prey_type))
check.prey_type <- check.prey_type %>% group_by(Session) %>% 
  summarise(num.carc = length(unique(prey_type)))
nrow(filter(check.prey_type, num.carc > 1))     #0
rm(check.prey_type)

#Fill in all lines for the session
for(i in 1:nrow(lh.indvbehav)){
  session.i <- lh.indvbehav$Session[i]
  feeding.i <- filter(lh.indvbehav, Session == session.i)
  if(nrow(filter(feeding.i, is.na(prey_type))) > 0){
    lh.indvbehav[lh.indvbehav$Session == session.i & 
                   is.na(lh.indvbehav$prey_type),]$prey_type <- 
      lh.indvbehav[lh.indvbehav$Session == session.i & 
                     !is.na(lh.indvbehav$prey_type),]$prey_type[1]
  }
  if(nrow(filter(feeding.i, is.na(prey_age))) > 0){
    lh.indvbehav[lh.indvbehav$Session == session.i & 
                   is.na(lh.indvbehav$prey_age),]$prey_age <- 
      lh.indvbehav[lh.indvbehav$Session == session.i & 
                     !is.na(lh.indvbehav$prey_age),]$prey_age[1]
  }
}
rm(session.i)
rm(feeding.i)

#Fill in prey_type for sessions that are still unknown
check.prey <- filter(lh.indvbehav, prey_type == "unknown" | is.na(prey_type))
check.prey <- filter(check.prey, Session %in% filter(lh.sessions, food_present == T)$Session)
length(unique(check.prey$Session))    #71 - everything not listed below is unknown, checked by TMM

#Serena sessions
# session	    prey_type	    prey_age
# "s10894"	  zebra	
# "s11279.2"	wildebeest	
# "s11340"  	wildebeest	
# "s13695"  	topi	        adult
# "s13829"  	zebra	
# "s13944"  	zebra	        adult
# "s13964"  	zebra	
# "s16267"  	elephant	    juvenile
# "s16358"  	buffalo	      adult
# "s3179"	    wildebeest	
# "s3672"	    hippopotamus	juvenile
# "s5434"	    wildebeest	
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "s10894",]$prey_type <- "zebra"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "s11279.2",]$prey_type <- "wildebeest"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "s11340",]$prey_type <- "wildebeest"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "s13695",]$prey_type <- "topi"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "topi") & 
               lh.indvbehav$Session == "s13695",]$prey_age <- "adult"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "s13829",]$prey_type <- "zebra"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "s13944",]$prey_type <- "zebra"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "zebra") & 
               lh.indvbehav$Session == "s13944",]$prey_age <- "adult"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "s13964",]$prey_type <- "zebra"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "s16267",]$prey_type <- "elephant"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "elephant") & 
               lh.indvbehav$Session == "s16267",]$prey_age <- "juvenile"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "s16358",]$prey_type <- "buffalo"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "buffalo") & 
               lh.indvbehav$Session == "s16358",]$prey_age <- "adult"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "s3179",]$prey_type <- "wildebeest"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "s3672",]$prey_type <- "hippo"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "hippo") & 
               lh.indvbehav$Session == "s3672",]$prey_age <- "juvenile"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "s5434",]$prey_type <- "wildebeest"

#Talek sessions
# "t1245"     wildebeest
# "t1569.3"   buffalo
# "t6106"     wildebeest    adult
# "t6626"     gazelle
# "t10764.2"	zebra	        adult
# "t19572"  	wildebeest	
# "t21394"  	buffalo	
# "t24594"  	wildebeest	
# "t29720"  	zebra	
# "t3586"	    gazelle	
# "t56284"	  warthog	
# "t7228"	    topi    	    adult
# "t77250"	  topi	
# "t8012"     wildebeest
# "t80207"  	topi	
# "t82549"  	buffalo	      juvenile
# "t9114"   	gazelle	
# "t91670"  	wildebeest	
# "t93710"  	warthog	
# "t94073"  	zebra	
# "t96103"  	wildebeest	  juvenile
# "t96608"  	wildebeest	
# "t96685"  	wildebeest	
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "t1245",]$prey_type <- "wildebeest"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "t1569.3",]$prey_type <- "buffalo"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "t6106",]$prey_type <- "wildebeest"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "wildebeest") & 
               lh.indvbehav$Session == "t6106",]$prey_age <- "adult"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "t6626",]$prey_type <- "gazelle"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "t10764.2",]$prey_type <- "zebra"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "zebra") & 
               lh.indvbehav$Session == "t10764.2",]$prey_age <- "adult"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "t19572",]$prey_type <- "wildebeest"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "t21394",]$prey_type <- "buffalo"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "t24594",]$prey_type <- "wildebeest"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "t29720",]$prey_type <- "zebra"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "t3586",]$prey_type <- "gazelle"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "t56284",]$prey_type <- "warthog"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "t7228",]$prey_type <- "topi"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "topi") & 
               lh.indvbehav$Session == "t7228",]$prey_age <- "adult"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "t77250",]$prey_type <- "topi"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "t8012",]$prey_type <- "wildebeest"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "t80207",]$prey_type <- "topi"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "t82549",]$prey_type <- "buffalo"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "buffalo") & 
               lh.indvbehav$Session == "t82549",]$prey_age <- "juvenile"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "t9114",]$prey_type <- "gazelle"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "t91670",]$prey_type <- "wildebeest"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "t93710",]$prey_type <- "warthog"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "t94073",]$prey_type <- "zebra"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "t96103",]$prey_type <- "wildebeest"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "wildebeest") & 
               lh.indvbehav$Session == "t96103",]$prey_age <- "juvenile"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "t96608",]$prey_type <- "wildebeest"
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "unknown") & 
               lh.indvbehav$Session == "t96685",]$prey_type <- "wildebeest"

#Recheck unknown sessions
check.prey <- filter(lh.indvbehav, prey_type == "unknown" | is.na(prey_type))
check.prey <- filter(check.prey, Session %in% filter(lh.sessions, food_present == T)$Session)
length(unique(check.prey$Session))    #37 left - all unknown, checked by TMM

#Fill in prey_type as unknown for these sessions
for(i in 1:nrow(lh.indvbehav)){
  session.i <- lh.indvbehav$Session[i]
  if(session.i %in% check.prey$Session){
    lh.indvbehav$prey_type[i] <- "unknown"
  }
}
rm(session.i)

#Double-check for remaining NAs
check.prey <- filter(lh.indvbehav, is.na(prey_type))
check.prey <- filter(check.prey, Session %in% filter(lh.sessions, food_present == T)$Session)
length(unique(check.prey$Session))    #0
rm(check.prey)


########## 5.3 Assign prey_age for all sessions with food_present ##########

#Fill in prey_age
unique(filter(lh.indvbehav, is.na(prey_age) & 
                grepl("subadult", lh.indvbehav$Food))$Food)
unique(filter(lh.indvbehav, is.na(prey_age) & 
                grepl("adult", lh.indvbehav$Food))$Food)
unique(filter(lh.indvbehav, is.na(prey_age) & 
                grepl("juvenile", lh.indvbehav$Food))$Food)
unique(filter(lh.indvbehav, is.na(prey_age) & 
                grepl("baby", lh.indvbehav$Food))$Food)
lh.indvbehav[(is.na(lh.indvbehav$prey_type) | lh.indvbehav$prey_type == "gazelle") & 
               lh.indvbehav$Session == "t91208",]$prey_age <- "juvenile"


########## 5.4 Assign carc_cat for all sessions with food_present ##########

#Categorize carcasses by species into the categories used for tblFeedingScans
lh.indvbehav$carc_cat <- NA

summary(as.factor(lh.indvbehav$prey_type))
summary(as.factor(lh.indvbehav$prey_age))
#Assume adult when prey_age is NA

#Adults
lh.indvbehav[(lh.indvbehav$prey_type == "buffalo" | lh.indvbehav$prey_type == "elephant" | 
                lh.indvbehav$prey_type == "giraffe" | lh.indvbehav$prey_type == "hippo") & 
               !is.na(lh.indvbehav$prey_type) &
               (lh.indvbehav$prey_age != "juvenile" | is.na(lh.indvbehav$prey_age)),]$carc_cat <- "xl"
lh.indvbehav[(lh.indvbehav$prey_type == "cow" | lh.indvbehav$prey_type == "hartebeest" | 
                lh.indvbehav$prey_type == "topi" | lh.indvbehav$prey_type == "wildebeest" | 
                lh.indvbehav$prey_type == "zebra") & !is.na(lh.indvbehav$prey_type) &
               (lh.indvbehav$prey_age != "juvenile" | is.na(lh.indvbehav$prey_age)),]$carc_cat <- "l"
lh.indvbehav[(lh.indvbehav$prey_type == "gazelle" | lh.indvbehav$prey_type == "impala" | 
                lh.indvbehav$prey_type == "shoat"| lh.indvbehav$prey_type == "warthog") &
               !is.na(lh.indvbehav$prey_type) &
               (lh.indvbehav$prey_age != "juvenile" | is.na(lh.indvbehav$prey_age)),]$carc_cat <- "m"

#Juveniles
lh.indvbehav[(lh.indvbehav$prey_type == "buffalo" | lh.indvbehav$prey_type == "elephant" | 
                lh.indvbehav$prey_type == "giraffe" | lh.indvbehav$prey_type == "hippo") & 
               !is.na(lh.indvbehav$prey_type) &
               (lh.indvbehav$prey_age == "juvenile" & !is.na(lh.indvbehav$prey_age)),]$carc_cat <- "xl"
lh.indvbehav[(lh.indvbehav$prey_type == "cow" | lh.indvbehav$prey_type == "hartebeest" | 
                lh.indvbehav$prey_type == "topi" | lh.indvbehav$prey_type == "wildebeest" | 
                lh.indvbehav$prey_type == "zebra") & !is.na(lh.indvbehav$prey_type) &
               (lh.indvbehav$prey_age == "juvenile" & !is.na(lh.indvbehav$prey_age)),]$carc_cat <- "m"
lh.indvbehav[(lh.indvbehav$prey_type == "gazelle" | lh.indvbehav$prey_type == "impala" | 
                lh.indvbehav$prey_type == "shoat"| lh.indvbehav$prey_type == "warthog") &
               !is.na(lh.indvbehav$prey_type) &
               (lh.indvbehav$prey_age == "juvenile" & !is.na(lh.indvbehav$prey_age)),]$carc_cat <- "s"

summary(as.factor(lh.indvbehav$carc_cat))

#Any remaining to be assigned? 
nrow(filter(lh.indvbehav, prey_type != "unknown" & !is.na(prey_type) & is.na(carc_cat)))    #0

#Final check - be sure prey_type, prey_age, and carc_cat are the same for all lines in a session
check.prey_type <- lh.indvbehav %>% group_by(Session) %>% 
  summarise(num.carc = length(unique(prey_type)))
nrow(filter(check.prey_type, num.carc > 1))   #0
rm(check.prey_type)

check.prey_age <- lh.indvbehav %>% group_by(Session) %>% 
  summarise(num.carc = length(unique(prey_age)))
nrow(filter(check.prey_age, num.carc > 1))   #0
rm(check.prey_age)

check.carc_cat <- lh.indvbehav %>% group_by(Session) %>% 
  summarise(num.carc = length(unique(carc_cat)))
nrow(filter(check.carc_cat, num.carc > 1))   #0
rm(check.carc_cat)


########## 5.5 Create final dataset lh.sessions.food ##########

lh.indvbehav.food <- lh.indvbehav[,c(2,12,13,16)]
lh.indvbehav.food <- unique(lh.indvbehav.food)
lh.sessions.food <- left_join(lh.sessions, lh.indvbehav.food, by = "Session")

#Check that food_present matches prey_type and carc_cat columns
nrow(filter(lh.sessions.food, food_present == T & is.na(prey_type)))    #0
nrow(filter(lh.sessions.food, food_present == T & is.na(carc_cat)))    #37 unknown prey_type
nrow(filter(lh.sessions.food, food_present == F & !is.na(prey_type)))    #5 - checked earlier, okay
lh.sessions.food[lh.sessions.food$food_present == F  & !is.na(lh.sessions.food$prey_type),]$prey_type <- NA
lh.sessions.food[lh.sessions.food$food_present == F  & !is.na(lh.sessions.food$prey_age),]$prey_age <- NA
lh.sessions.food[lh.sessions.food$food_present == F  & !is.na(lh.sessions.food$carc_cat),]$carc_cat <- NA

#Clean columns
lh.sessions.food$prey_type <- as.factor(lh.sessions.food$prey_type)
lh.sessions.food$prey_age <- as.factor(lh.sessions.food$prey_age)
lh.sessions.food$carc_cat <- as.factor(lh.sessions.food$carc_cat)
lh.sessions.food$carc_cat <- ordered(lh.sessions.food$carc_cat, levels = c("s", "m", "l", "xl"))

#Remove lh.indvbehav.food - used to determine food at session level
rm(lh.indvbehav.food)


########## 5.6 Categorize behaviors in mobbing sessions ##########

#Filter lh.indvbehav to only sessions with mobbing
lh.indvbehav.behav <- filter(lh.indvbehav, Session %in% lh.indvbehav.mob$session)
lh.indvbehav.behav <- left_join(lh.indvbehav.behav, lh.indvbehav.mob[c(1,12)], by = c("ID" = "event"))

#Create behav_cat and assign mobs
lh.indvbehav.behav$behav_cat <- NA
lh.indvbehav.behav[!is.na(lh.indvbehav.behav$mobID) & 
                     is.na(lh.indvbehav.behav$behav_cat),]$behav_cat <- "mob"

##### Assign behaviors to feeding category #####

unique(filter(lh.indvbehav.behav, grepl("fd", lh.indvbehav.behav$Behavior) & 
                !grepl("drop", lh.indvbehav.behav$Behavior) & 
                !grepl("abandon", lh.indvbehav.behav$Behavior) & 
                !grepl("lv fd", lh.indvbehav.behav$Behavior) & 
                !grepl("attempt", lh.indvbehav.behav$Behavior) & 
                is.na(lh.indvbehav.behav$behav_cat))$Behavior)
lh.indvbehav.behav[grepl("fd", lh.indvbehav.behav$Behavior) & 
                     !grepl("drop", lh.indvbehav.behav$Behavior) & 
                     !grepl("abandon", lh.indvbehav.behav$Behavior) & 
                     !grepl("lv fd", lh.indvbehav.behav$Behavior) & 
                     !grepl("attempt", lh.indvbehav.behav$Behavior) & 
                     is.na(lh.indvbehav.behav$behav_cat),]$behav_cat <- "fd"

unique(filter(lh.indvbehav.behav, grepl("feed", lh.indvbehav.behav$Behavior) & 
                !grepl("other", lh.indvbehav.behav$Behavior) & 
                is.na(lh.indvbehav.behav$behav_cat))$Behavior)
lh.indvbehav.behav[grepl("feed", lh.indvbehav.behav$Behavior) & 
                     !grepl("other", lh.indvbehav.behav$Behavior) & 
                     is.na(lh.indvbehav.behav$behav_cat),]$behav_cat <- "fd"

unique(filter(lh.indvbehav.behav, grepl("food", lh.indvbehav.behav$Behavior) & 
                !grepl("find", lh.indvbehav.behav$Behavior) & 
                !grepl("tussel", lh.indvbehav.behav$Behavior) & 
                is.na(lh.indvbehav.behav$behav_cat))$Behavior)
lh.indvbehav.behav[grepl("food", lh.indvbehav.behav$Behavior) & 
                     !grepl("find", lh.indvbehav.behav$Behavior) & 
                     !grepl("tussel", lh.indvbehav.behav$Behavior) & 
                     is.na(lh.indvbehav.behav$behav_cat),]$behav_cat <- "fd"

unique(filter(lh.indvbehav.behav, grepl("chew", lh.indvbehav.behav$Behavior) & 
                is.na(lh.indvbehav.behav$behav_cat))$Behavior)
lh.indvbehav.behav[grepl("chew", lh.indvbehav.behav$Behavior) & 
                     is.na(lh.indvbehav.behav$behav_cat),]$behav_cat <- "fd"

unique(filter(lh.indvbehav.behav, grepl("scrap", lh.indvbehav.behav$Behavior) & 
                !grepl("lose", lh.indvbehav.behav$Behavior) & 
                !grepl("abandon", lh.indvbehav.behav$Behavior) & 
                is.na(lh.indvbehav.behav$behav_cat))$Behavior)
lh.indvbehav.behav[grepl("scrap", lh.indvbehav.behav$Behavior) & 
                     !grepl("lose", lh.indvbehav.behav$Behavior) & 
                     !grepl("abandon", lh.indvbehav.behav$Behavior) & 
                     is.na(lh.indvbehav.behav$behav_cat),]$behav_cat <- "fd"

unique(filter(lh.indvbehav.behav, grepl("carr", lh.indvbehav.behav$Behavior) & 
                is.na(lh.indvbehav.behav$behav_cat))$Behavior)
lh.indvbehav.behav[grepl("carr", lh.indvbehav.behav$Behavior) & 
                     is.na(lh.indvbehav.behav$behav_cat),]$behav_cat <- "fd"

unique(filter(lh.indvbehav.behav, grepl("steal", lh.indvbehav.behav$Behavior) & 
                !grepl("attempt", lh.indvbehav.behav$Behavior) &
                is.na(lh.indvbehav.behav$behav_cat))$Behavior)
lh.indvbehav.behav[grepl("steal", lh.indvbehav.behav$Behavior) & 
                     !grepl("attempt", lh.indvbehav.behav$Behavior) &
                     is.na(lh.indvbehav.behav$behav_cat),]$behav_cat <- "fd"

unique(filter(lh.indvbehav.behav, grepl("carc", lh.indvbehav.behav$Behavior) & 
                (grepl("get", lh.indvbehav.behav$Behavior) | 
                   grepl("take", lh.indvbehav.behav$Behavior) | 
                   grepl("grab", lh.indvbehav.behav$Behavior) | 
                   grepl("w/", lh.indvbehav.behav$Behavior)  | 
                   grepl("has", lh.indvbehav.behav$Behavior) | 
                   grepl("tow", lh.indvbehav.behav$Behavior) | 
                   grepl("move", lh.indvbehav.behav$Behavior) |
                   grepl("tug", lh.indvbehav.behav$Behavior) |
                   grepl("return", lh.indvbehav.behav$Behavior) |
                   grepl("reclaim", lh.indvbehav.behav$Behavior) |
                   grepl("drag", lh.indvbehav.behav$Behavior)) &
                !grepl("rushes to grab carc", lh.indvbehav.behav$Behavior) &
                is.na(lh.indvbehav.behav$behav_cat))$Behavior)
lh.indvbehav.behav[grepl("carc", lh.indvbehav.behav$Behavior) & 
                     (grepl("get", lh.indvbehav.behav$Behavior) | 
                        grepl("take", lh.indvbehav.behav$Behavior) | 
                        grepl("grab", lh.indvbehav.behav$Behavior) | 
                        grepl("w/", lh.indvbehav.behav$Behavior)  | 
                        grepl("has", lh.indvbehav.behav$Behavior) | 
                        grepl("tow", lh.indvbehav.behav$Behavior) | 
                        grepl("move", lh.indvbehav.behav$Behavior) |
                        grepl("tug", lh.indvbehav.behav$Behavior) |
                        grepl("return", lh.indvbehav.behav$Behavior) |
                        grepl("reclaim", lh.indvbehav.behav$Behavior) |
                        grepl("drag", lh.indvbehav.behav$Behavior)) &
                     !grepl("rushes to grab carc", lh.indvbehav.behav$Behavior) &
                     is.na(lh.indvbehav.behav$behav_cat),]$behav_cat <- "fd"

unique(filter(lh.indvbehav.behav, (grepl("spine", lh.indvbehav.behav$Behavior) | 
                                     grepl("pelvis", lh.indvbehav.behav$Behavior) | 
                                     grepl("leg", lh.indvbehav.behav$Behavior) | 
                                     grepl("bone", lh.indvbehav.behav$Behavior) | 
                                     grepl("steal", lh.indvbehav.behav$Behavior) | 
                                     grepl("scapula", lh.indvbehav.behav$Behavior) | 
                                     grepl("skin", lh.indvbehav.behav$Behavior)) & 
                !grepl("foreleg", lh.indvbehav.behav$Behavior) & 
                !grepl("drop", lh.indvbehav.behav$Behavior) & 
                !grepl("attempt", lh.indvbehav.behav$Behavior) & 
                is.na(lh.indvbehav.behav$behav_cat))$Behavior)
lh.indvbehav.behav[(grepl("spine", lh.indvbehav.behav$Behavior) | 
                      grepl("pelvis", lh.indvbehav.behav$Behavior) | 
                      grepl("leg", lh.indvbehav.behav$Behavior) | 
                      grepl("bone", lh.indvbehav.behav$Behavior) | 
                      grepl("steal", lh.indvbehav.behav$Behavior) | 
                      grepl("scapula", lh.indvbehav.behav$Behavior) | 
                      grepl("skin", lh.indvbehav.behav$Behavior)) & 
                     !grepl("foreleg", lh.indvbehav.behav$Behavior) & 
                     !grepl("drop", lh.indvbehav.behav$Behavior) & 
                     !grepl("attempt", lh.indvbehav.behav$Behavior) & 
                     is.na(lh.indvbehav.behav$behav_cat),]$behav_cat <- "fd"

unique(filter(lh.indvbehav.behav, grepl("picks up kill", lh.indvbehav.behav$Behavior) & 
                is.na(lh.indvbehav.behav$behav_cat))$Behavior)
lh.indvbehav.behav[grepl("picks up kill", lh.indvbehav.behav$Behavior) & 
                     is.na(lh.indvbehav.behav$behav_cat),]$behav_cat <- "fd"

unique(filter(lh.indvbehav.behav, grepl("tug of war", lh.indvbehav.behav$Behavior))$Behavior)
lh.indvbehav.behav[grepl("tug of war", lh.indvbehav.behav$Behavior),]$behav_cat <- "fd"

#Remove feeding on introduced foods (dog biscuit)
unique(filter(lh.indvbehav.behav, grepl("dog", lh.indvbehav.behav$Food))$Food)
lh.indvbehav.behav[grepl("dog", lh.indvbehav.behav$Food),]$behav_cat <- NA

#Check for mismatch between behav_cat and Food columns
unique(filter(lh.indvbehav.behav, behav_cat == "fd" & is.na(Food))$Behavior)     #ok - all feeding
unique(filter(lh.indvbehav.behav, (is.na(behav_cat) | behav_cat != "fd") & !is.na(Food))$Behavior)     #ok - all NOT feeding

##### Assign affiliative behaviors to categories #####

unique(filter(lh.indvbehav.behav, (grepl("soc", lh.indvbehav.behav$Behavior) | 
                                     grepl("son snf", lh.indvbehav.behav$Behavior) | 
                                     grepl("so snif", lh.indvbehav.behav$Behavior)) & 
                is.na(lh.indvbehav.behav$behav_cat))$Behavior)
lh.indvbehav.behav[(grepl("soc", lh.indvbehav.behav$Behavior) | 
                      grepl("son snf", lh.indvbehav.behav$Behavior) | 
                      grepl("so snif", lh.indvbehav.behav$Behavior)) & 
                     is.na(lh.indvbehav.behav$behav_cat),]$behav_cat <- "soc snf"

lh.indvbehav.behav[lh.indvbehav.behav$Session == "t61001" & 
                     grepl("brt snf spot", lh.indvbehav.behav$Behavior),]$behav_cat <- "soc snf"
lh.indvbehav.behav[lh.indvbehav.behav$Session == "t31592" & 
                     grepl("brt snf", lh.indvbehav.behav$Behavior),]$behav_cat <- "soc snf"

unique(filter(lh.indvbehav.behav, grepl("parallel", lh.indvbehav.behav$Behavior) & 
                !grepl("t2", lh.indvbehav.behav$Behavior) & 
                is.na(lh.indvbehav.behav$behav_cat))$Behavior)
lh.indvbehav.behav[grepl("parallel", lh.indvbehav.behav$Behavior) & 
                     !grepl("t2", lh.indvbehav.behav$Behavior) & 
                     is.na(lh.indvbehav.behav$behav_cat),]$behav_cat <- "parallel wlk"

unique(filter(lh.indvbehav.behav, grepl("pres", lh.indvbehav.behav$Behavior) & 
                !grepl("hyenas", lh.indvbehav.behav$Behavior) & 
                is.na(lh.indvbehav.behav$behav_cat))$Behavior)
lh.indvbehav.behav[grepl("pres", lh.indvbehav.behav$Behavior) & 
                     !grepl("hyenas", lh.indvbehav.behav$Behavior) & 
                     is.na(lh.indvbehav.behav$behav_cat),]$behav_cat <- "pres"

##### Assign vocal behaviors to categories #####

unique(filter(lh.indvbehav.behav, grepl("low", lh.indvbehav.behav$Behavior) & 
                !grepl("follow", lh.indvbehav.behav$Behavior) & 
                is.na(lh.indvbehav.behav$behav_cat))$Behavior)
lh.indvbehav.behav[grepl("low", lh.indvbehav.behav$Behavior) & 
                     !grepl("follow", lh.indvbehav.behav$Behavior) & 
                     is.na(lh.indvbehav.behav$behav_cat),]$behav_cat <- "low"

unique(filter(lh.indvbehav.behav, (grepl("whoop", lh.indvbehav.behav$Behavior) | 
                                     grepl("Whoop", lh.indvbehav.behav$Behavior) | 
                                     grepl("whp", lh.indvbehav.behav$Behavior) | 
                                     grepl("woop", lh.indvbehav.behav$Behavior)  | 
                                     grepl("wp", lh.indvbehav.behav$Behavior)) & 
                is.na(lh.indvbehav.behav$behav_cat))$Behavior)
lh.indvbehav.behav[(grepl("whoop", lh.indvbehav.behav$Behavior) | 
                      grepl("Whoop", lh.indvbehav.behav$Behavior) | 
                      grepl("whp", lh.indvbehav.behav$Behavior) | 
                      grepl("woop", lh.indvbehav.behav$Behavior)  | 
                      grepl("wp", lh.indvbehav.behav$Behavior)) & 
                     is.na(lh.indvbehav.behav$behav_cat),]$behav_cat <- "whoop"

unique(filter(lh.indvbehav.behav, grepl("alarm", lh.indvbehav.behav$Behavior) & 
                is.na(lh.indvbehav.behav$behav_cat))$Behavior)
lh.indvbehav.behav[grepl("alarm", lh.indvbehav.behav$Behavior) & 
                     is.na(lh.indvbehav.behav$behav_cat),]$behav_cat <- "alarm rumble"

unique(filter(lh.indvbehav.behav, (grepl("gig", lh.indvbehav.behav$Behavior) | 
                                     grepl("yikker", lh.indvbehav.behav$Behavior)) & 
                is.na(lh.indvbehav.behav$behav_cat))$Behavior)
lh.indvbehav.behav[(grepl("gig", lh.indvbehav.behav$Behavior) | 
                      grepl("yikker", lh.indvbehav.behav$Behavior)) & 
                     is.na(lh.indvbehav.behav$behav_cat),]$behav_cat <- "giggle"

unique(filter(lh.indvbehav.behav, grepl("groan", lh.indvbehav.behav$Behavior) & 
                is.na(lh.indvbehav.behav$behav_cat))$Behavior)
lh.indvbehav.behav[grepl("groan", lh.indvbehav.behav$Behavior) & 
                     is.na(lh.indvbehav.behav$behav_cat),]$behav_cat <- "groan"

unique(filter(lh.indvbehav.behav, grepl("growl", lh.indvbehav.behav$Behavior) & 
                is.na(lh.indvbehav.behav$behav_cat))$Behavior)
lh.indvbehav.behav[grepl("growl", lh.indvbehav.behav$Behavior) & 
                     is.na(lh.indvbehav.behav$behav_cat),]$behav_cat <- "growl"

unique(filter(lh.indvbehav.behav, (grepl("squeal", lh.indvbehav.behav$Behavior)  | 
                                     grepl("squel", lh.indvbehav.behav$Behavior)   | 
                                     grepl("sqeal", lh.indvbehav.behav$Behavior)) & 
                is.na(lh.indvbehav.behav$behav_cat))$Behavior)
lh.indvbehav.behav[(grepl("squeal", lh.indvbehav.behav$Behavior)  | 
                      grepl("squel", lh.indvbehav.behav$Behavior)   | 
                      grepl("sqeal", lh.indvbehav.behav$Behavior)) & 
                     is.na(lh.indvbehav.behav$behav_cat),]$behav_cat <- "squeal"

##### Assign remaining behaviors to categories #####

unique(filter(lh.indvbehav.behav, grepl("paste", lh.indvbehav.behav$Behavior) & 
                is.na(lh.indvbehav.behav$behav_cat))$Behavior)
lh.indvbehav.behav[grepl("paste", lh.indvbehav.behav$Behavior) & 
                     is.na(lh.indvbehav.behav$behav_cat),]$behav_cat <- "paste"

#Distances recorded during scans (often scanned behavior is "here")
unique(filter(lh.indvbehav.behav, grepl("scan", lh.indvbehav.behav$Behavior) & 
                !grepl("fd", lh.indvbehav.behav$Behavior) & 
                is.na(lh.indvbehav.behav$behav_cat))$Behavior)
lh.indvbehav.behav[grepl("scan", lh.indvbehav.behav$Behavior) & 
                     !grepl("fd", lh.indvbehav.behav$Behavior) & 
                     is.na(lh.indvbehav.behav$behav_cat),]$behav_cat <- "scan"

unique(filter(lh.indvbehav.behav, grepl("dis", lh.indvbehav.behav$Behavior) & 
                !grepl("displace", lh.indvbehav.behav$Behavior) & 
                is.na(lh.indvbehav.behav$behav_cat))$Behavior)
lh.indvbehav.behav[grepl("dis", lh.indvbehav.behav$Behavior) & 
                     !grepl("displace", lh.indvbehav.behav$Behavior) & 
                     is.na(lh.indvbehav.behav$behav_cat),]$behav_cat <- "dist"

#Add any distances from lions that were missing a behavior
unique(filter(lh.indvbehav.behav, is.na(behav_cat) & !is.na(Distance) & 
                grepl("lion", lh.indvbehav.behav$Target) & 
                !grepl("lion", lh.indvbehav.behav$Hyena))$Behavior)
lh.indvbehav.behav[is.na(lh.indvbehav.behav$behav_cat) & !is.na(lh.indvbehav.behav$Distance) & 
                     grepl("lion", lh.indvbehav.behav$Target) & 
                     !grepl("lion", lh.indvbehav.behav$Hyena),]$behav_cat <- "dist"

##### Uncategorized behaviors ##### 

sort(table(filter(lh.indvbehav.behav, is.na(behav_cat))$Behavior), decreasing = T)
unique(filter(lh.indvbehav.behav, is.na(behav_cat))$Behavior)
#Checked by TMM

#Check categorized behaviors
sort(table(lh.indvbehav.behav$behav_cat), decreasing = T)


########## 5.7 Categorize feeding by size of piece in mobbing sessions ##########

lh.indvbehav.behav$size_cat <- NA

##### Look in lh.indvbehav$Food #####

#Pieces
unique(filter(lh.indvbehav.behav, lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
                !(grepl("scrap", lh.indvbehav.behav$Food) | grepl("skin", lh.indvbehav.behav$Food)) &
                (grepl("leg", lh.indvbehav.behav$Food) | grepl("rib", lh.indvbehav.behav$Food) | 
                   grepl("scapula", lh.indvbehav.behav$Food) | grepl("spin", lh.indvbehav.behav$Food) | 
                   grepl("pelvis", lh.indvbehav.behav$Food) | grepl("vertibrae", lh.indvbehav.behav$Food) | 
                   grepl("head", lh.indvbehav.behav$Food) | grepl("riib", lh.indvbehav.behav$Food) | 
                   grepl("meat", lh.indvbehav.behav$Food)))$Food)
lh.indvbehav.behav[lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
                     !(grepl("scrap", lh.indvbehav.behav$Food) | grepl("skin", lh.indvbehav.behav$Food)) &
                     (grepl("leg", lh.indvbehav.behav$Food) | grepl("rib", lh.indvbehav.behav$Food) | 
                        grepl("scapula", lh.indvbehav.behav$Food) | grepl("spin", lh.indvbehav.behav$Food) | 
                        grepl("pelvis", lh.indvbehav.behav$Food) | grepl("vertibrae", lh.indvbehav.behav$Food) | 
                        grepl("head", lh.indvbehav.behav$Food) | grepl("riib", lh.indvbehav.behav$Food) | 
                        grepl("meat", lh.indvbehav.behav$Food)),]$size_cat <- "piece"

lh.indvbehav.behav[is.na(lh.indvbehav.behav$size_cat) & 
                     lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
                     lh.indvbehav.behav$Food == "spine and skull scrap" & 
                     !is.na(lh.indvbehav.behav$Food),]$size_cat <- "piece"
lh.indvbehav.behav[is.na(lh.indvbehav.behav$size_cat) & 
                     lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
                     lh.indvbehav.behav$Food == "subadult topi carcass piece" & 
                     !is.na(lh.indvbehav.behav$Food),]$size_cat <- "piece"
lh.indvbehav.behav[is.na(lh.indvbehav.behav$size_cat) & 
                     lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
                     lh.indvbehav.behav$Food == "gnu \"something\"" & 
                     !is.na(lh.indvbehav.behav$Food),]$size_cat <- "piece"

#Scraps
unique(filter(lh.indvbehav.behav, is.na(lh.indvbehav.behav$size_cat) &
                lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
                (grepl("scrap", lh.indvbehav.behav$Food) | grepl("bone", lh.indvbehav.behav$Food) | 
                   grepl("skin", lh.indvbehav.behav$Food) | grepl("old", lh.indvbehav.behav$Food) | 
                   grepl("jaw", lh.indvbehav.behav$Food) | grepl("offal", lh.indvbehav.behav$Food) | 
                   grepl("rumen", lh.indvbehav.behav$Food) | grepl("horn", lh.indvbehav.behav$Food) |
                   grepl("skull", lh.indvbehav.behav$Food) | grepl("scap", lh.indvbehav.behav$Food) |
                   grepl("srap", lh.indvbehav.behav$Food)))$Food)
lh.indvbehav.behav[is.na(lh.indvbehav.behav$size_cat) &
                     lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
                     (grepl("scrap", lh.indvbehav.behav$Food) | grepl("bone", lh.indvbehav.behav$Food) | 
                        grepl("skin", lh.indvbehav.behav$Food) | grepl("old", lh.indvbehav.behav$Food) | 
                        grepl("jaw", lh.indvbehav.behav$Food) | grepl("offal", lh.indvbehav.behav$Food) | 
                        grepl("rumen", lh.indvbehav.behav$Food) | grepl("horn", lh.indvbehav.behav$Food) |
                        grepl("skull", lh.indvbehav.behav$Food) | grepl("scap", lh.indvbehav.behav$Food) |
                        grepl("srap", lh.indvbehav.behav$Food)),]$size_cat <- "scrap"
lh.indvbehav.behav[is.na(lh.indvbehav.behav$size_cat) & 
                     lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
                     lh.indvbehav.behav$Food == "few mouthfulls" & 
                     !is.na(lh.indvbehav.behav$Food),]$size_cat <- "scrap"
lh.indvbehav.behav[is.na(lh.indvbehav.behav$size_cat) & 
                     lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
                     lh.indvbehav.behav$Food == "something with feathers" & 
                     !is.na(lh.indvbehav.behav$Food),]$size_cat <- "scrap"

#Carcasses
unique(filter(lh.indvbehav.behav, is.na(lh.indvbehav.behav$size_cat) & !grepl("site", lh.indvbehav.behav$Food) &
                lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
                (grepl("carcass", lh.indvbehav.behav$Food) | grepl("carc", lh.indvbehav.behav$Food) | 
                   grepl("kill", lh.indvbehav.behav$Food)))$Food)
lh.indvbehav.behav[is.na(lh.indvbehav.behav$size_cat) & !grepl("site", lh.indvbehav.behav$Food) &
                     lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
                     (grepl("carcass", lh.indvbehav.behav$Food) | grepl("carc", lh.indvbehav.behav$Food) | 
                        grepl("kill", lh.indvbehav.behav$Food)),]$size_cat <- "carcass"
lh.indvbehav.behav[is.na(lh.indvbehav.behav$size_cat) & 
                     lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
                     lh.indvbehav.behav$Food == "buffalo" & 
                     !is.na(lh.indvbehav.behav$Food),]$size_cat <- "carcass"
lh.indvbehav.behav[is.na(lh.indvbehav.behav$size_cat) & 
                     lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
                     lh.indvbehav.behav$Food == "cow" & 
                     !is.na(lh.indvbehav.behav$Food),]$size_cat <- "carcass"
lh.indvbehav.behav[is.na(lh.indvbehav.behav$size_cat) & 
                     lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
                     lh.indvbehav.behav$Food == "zebra" & 
                     !is.na(lh.indvbehav.behav$Food),]$size_cat <- "carcass"
lh.indvbehav.behav[is.na(lh.indvbehav.behav$size_cat) & 
                     lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
                     lh.indvbehav.behav$Food == "giraffe" &  
                     !is.na(lh.indvbehav.behav$Food),]$size_cat <- "carcass"
lh.indvbehav.behav[is.na(lh.indvbehav.behav$size_cat) & 
                     lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
                     lh.indvbehav.behav$Food == "impala" & 
                     !is.na(lh.indvbehav.behav$Food),]$size_cat <- "carcass"
lh.indvbehav.behav[is.na(lh.indvbehav.behav$size_cat) & 
                     lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
                     lh.indvbehav.behav$Food == "wildebeast" & 
                     !is.na(lh.indvbehav.behav$Food),]$size_cat <- "carcass"
lh.indvbehav.behav[is.na(lh.indvbehav.behav$size_cat) & 
                     lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
                     lh.indvbehav.behav$Food == "topi cascass" & 
                     !is.na(lh.indvbehav.behav$Food),]$size_cat <- "carcass"


##### For ones still blank, get info from lh.indvbehav$Behavior/Notes #####

View(filter(lh.indvbehav.behav, (is.na(lh.indvbehav.behav$size_cat) | lh.indvbehav.behav$size_cat == "carcass") &
              lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
              (grepl("scrap", lh.indvbehav.behav$Behavior) | grepl("scrap", lh.indvbehav.behav$Notes)) & 
              !grepl("MIKE", lh.indvbehav.behav$Notes)))
lh.indvbehav.behav[(is.na(lh.indvbehav.behav$size_cat) | lh.indvbehav.behav$size_cat == "carcass") &
                     lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
                     (grepl("scrap", lh.indvbehav.behav$Behavior) | grepl("scrap", lh.indvbehav.behav$Notes)) & 
                     !grepl("MIKE", lh.indvbehav.behav$Notes),]$size_cat <- "scrap"

View(filter(lh.indvbehav.behav, (is.na(lh.indvbehav.behav$size_cat) | lh.indvbehav.behav$size_cat == "carcass") &
              lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
              (grepl("leg", lh.indvbehav.behav$Behavior) | grepl("piece", lh.indvbehav.behav$Behavior))))
lh.indvbehav.behav[(is.na(lh.indvbehav.behav$size_cat) | lh.indvbehav.behav$size_cat == "carcass") &
                     lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
                     (grepl("leg", lh.indvbehav.behav$Behavior) | grepl("piece", lh.indvbehav.behav$Behavior)),]$size_cat <- "piece"

View(filter(lh.indvbehav.behav, is.na(lh.indvbehav.behav$size_cat) & 
              lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
              (grepl("carc", lh.indvbehav.behav$Behavior) | grepl("kill", lh.indvbehav.behav$Behavior))))
lh.indvbehav.behav[is.na(lh.indvbehav.behav$size_cat) & 
                     lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
                     (grepl("carc", lh.indvbehav.behav$Behavior) | grepl("kill", lh.indvbehav.behav$Behavior)),]$size_cat <- "carcass"

View(filter(lh.indvbehav.behav, (is.na(lh.indvbehav.behav$size_cat) | lh.indvbehav.behav$size_cat == "carcass") & 
              lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
              !grepl("carc", lh.indvbehav.behav$Behavior) & !grepl("carry", lh.indvbehav.behav$Notes) & 
              !grepl("pick", lh.indvbehav.behav$Notes) &
              (grepl("w/", lh.indvbehav.behav$Behavior) | grepl("with", lh.indvbehav.behav$Behavior)) & 
              (grepl("av", lh.indvbehav.behav$Behavior) | grepl("bo", lh.indvbehav.behav$Behavior))))
lh.indvbehav.behav[(is.na(lh.indvbehav.behav$size_cat) | lh.indvbehav.behav$size_cat == "carcass") & 
                     lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
                     !grepl("carc", lh.indvbehav.behav$Behavior) & !grepl("carry", lh.indvbehav.behav$Notes) & 
                     !grepl("pick", lh.indvbehav.behav$Notes) &
                     (grepl("w/", lh.indvbehav.behav$Behavior) | grepl("with", lh.indvbehav.behav$Behavior)) & 
                     (grepl("av", lh.indvbehav.behav$Behavior) | grepl("bo", lh.indvbehav.behav$Behavior)),]$size_cat <- "piece"

View(filter(lh.indvbehav.behav, is.na(lh.indvbehav.behav$size_cat) & !grepl("dog", lh.indvbehav.behav$Food) &
              lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
              (grepl("steal", lh.indvbehav.behav$Behavior) | grepl("stl", lh.indvbehav.behav$Behavior) | 
                 grepl("steal", lh.indvbehav.behav$Behavior) | grepl("stl", lh.indvbehav.behav$Behavior) | 
                 grepl("app", lh.indvbehav.behav$Behavior))))
lh.indvbehav.behav[is.na(lh.indvbehav.behav$size_cat) & !grepl("dog", lh.indvbehav.behav$Food) &
                     lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
                     (grepl("steal", lh.indvbehav.behav$Behavior) | grepl("stl", lh.indvbehav.behav$Behavior) | 
                        grepl("steal", lh.indvbehav.behav$Behavior) | grepl("stl", lh.indvbehav.behav$Behavior) | 
                        grepl("app", lh.indvbehav.behav$Behavior)),]$size_cat <- "piece"

View(filter(lh.indvbehav.behav, is.na(lh.indvbehav.behav$size_cat) & !grepl("dog", lh.indvbehav.behav$Food) & 
              lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
              !grepl("little", lh.indvbehav.behav$Notes) &
              (grepl("fd", lh.indvbehav.behav$Behavior) | grepl("feed", lh.indvbehav.behav$Behavior))))
lh.indvbehav.behav[is.na(lh.indvbehav.behav$size_cat) & !grepl("dog", lh.indvbehav.behav$Food) & 
                     lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
                     !grepl("little", lh.indvbehav.behav$Notes) &
                     (grepl("fd", lh.indvbehav.behav$Behavior) | grepl("feed", lh.indvbehav.behav$Behavior)),]$size_cat <- "carcass"

lh.indvbehav.behav[is.na(lh.indvbehav.behav$size_cat) & 
                     lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
                     lh.indvbehav.behav$Notes == "lion abandon carc hyenas rush in, very little left" & 
                     !is.na(lh.indvbehav.behav$Notes),]$size_cat <- "scrap"
lh.indvbehav.behav[lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat) &
                     lh.indvbehav.behav$notes == "gazelle?, skin, lion present" & 
                     !is.na(lh.indvbehav.behav$notes),]$size_cat <- "scrap"

nrow(filter(lh.indvbehav.behav, is.na(size_cat) & behav_cat == "fd"))     #0
nrow(filter(lh.indvbehav.behav, !is.na(size_cat) & behav_cat != "fd"))    #0


########## 5.8 Assign final food size in mobbing sessions ##########

lh.indvbehav.behav$food_size <- NA
summary(as.factor(lh.indvbehav.behav$carc_cat))
summary(as.factor(lh.indvbehav.behav$size_cat))

#Clear size_cat and behav_cat for t78815 "old hardened piece of hide" - not real food
lh.indvbehav.behav[is.na(lh.indvbehav.behav$prey_type) & is.na(lh.indvbehav.behav$carc_cat) & 
                     lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat),]$size_cat <- NA
lh.indvbehav.behav[is.na(lh.indvbehav.behav$prey_type) & is.na(lh.indvbehav.behav$carc_cat) & 
                     lh.indvbehav.behav$behav_cat == "fd" & !is.na(lh.indvbehav.behav$behav_cat),]$behav_cat <- NA

#Assign food_size based on this table
# Carcass size: small 	medium 	large 	extra-large 
# Scraps        xs      xs    	xs      xs
# Pieces 	      xs      s	      s	      m
# Carcass	      s       m	      l	      xl

for(i in 1:nrow(lh.indvbehav.behav)){
  #Extra-large carcasses (buffalo, giraffe, hippo of all ages)
  if(!is.na(lh.indvbehav.behav$carc_cat[i]) & lh.indvbehav.behav$carc_cat[i] == "xl" & 
     !is.na(lh.indvbehav.behav$size_cat[i]) & lh.indvbehav.behav$size_cat[i] == "carcass"){
    lh.indvbehav.behav$food_size[i] <- "xl"
  }
  if(!is.na(lh.indvbehav.behav$carc_cat[i]) & lh.indvbehav.behav$carc_cat[i] == "xl" & 
     !is.na(lh.indvbehav.behav$size_cat[i]) & lh.indvbehav.behav$size_cat[i] == "piece"){
    lh.indvbehav.behav$food_size[i] <- "m"
  }
  if(!is.na(lh.indvbehav.behav$carc_cat[i]) & lh.indvbehav.behav$carc_cat[i] == "xl" & 
     !is.na(lh.indvbehav.behav$size_cat[i]) & lh.indvbehav.behav$size_cat[i] == "scrap"){
    lh.indvbehav.behav$food_size[i] <- "xs"
  }
  #Large carcasses (cow, topi, wildebeest, zebra)
  if(!is.na(lh.indvbehav.behav$carc_cat[i]) & lh.indvbehav.behav$carc_cat[i] == "l" & 
     !is.na(lh.indvbehav.behav$size_cat[i]) & lh.indvbehav.behav$size_cat[i] == "carcass"){
    lh.indvbehav.behav$food_size[i] <- "l"
  }
  if(!is.na(lh.indvbehav.behav$carc_cat[i]) & lh.indvbehav.behav$carc_cat[i] == "l" & 
     !is.na(lh.indvbehav.behav$size_cat[i]) & lh.indvbehav.behav$size_cat[i] == "piece"){
    lh.indvbehav.behav$food_size[i] <- "s"
  }
  if(!is.na(lh.indvbehav.behav$carc_cat[i]) & lh.indvbehav.behav$carc_cat[i] == "l" & 
     !is.na(lh.indvbehav.behav$size_cat[i]) & lh.indvbehav.behav$size_cat[i] == "scrap"){
    lh.indvbehav.behav$food_size[i] <- "xs"
  }
  #Medium carcasses (gazelle, impala, shoat, warthog; also juvenile: cow, topi, wildebeest, zebra)
  if(!is.na(lh.indvbehav.behav$carc_cat[i]) & lh.indvbehav.behav$carc_cat[i] == "m" & 
     !is.na(lh.indvbehav.behav$size_cat[i]) & lh.indvbehav.behav$size_cat[i] == "carcass"){
    lh.indvbehav.behav$food_size[i] <- "m"
  }
  if(!is.na(lh.indvbehav.behav$carc_cat[i]) & lh.indvbehav.behav$carc_cat[i] == "m" & 
     !is.na(lh.indvbehav.behav$size_cat[i]) & lh.indvbehav.behav$size_cat[i] == "piece"){
    lh.indvbehav.behav$food_size[i] <- "s"
  }
  if(!is.na(lh.indvbehav.behav$carc_cat[i]) & lh.indvbehav.behav$carc_cat[i] == "m" & 
     !is.na(lh.indvbehav.behav$size_cat[i]) & lh.indvbehav.behav$size_cat[i] == "scrap"){
    lh.indvbehav.behav$food_size[i] <- "xs"
  }
  #Small carcasses (juvenile: gazelle, impala, shoat, warthog)
  if(!is.na(lh.indvbehav.behav$carc_cat[i]) & lh.indvbehav.behav$carc_cat[i] == "s" & 
     !is.na(lh.indvbehav.behav$size_cat[i]) & lh.indvbehav.behav$size_cat[i] == "carcass"){
    lh.indvbehav.behav$food_size[i] <- "s"
  }
  if(!is.na(lh.indvbehav.behav$carc_cat[i]) & lh.indvbehav.behav$carc_cat[i] == "s" & 
     !is.na(lh.indvbehav.behav$size_cat[i]) & lh.indvbehav.behav$size_cat[i] == "piece"){
    lh.indvbehav.behav$food_size[i] <- "xs"
  }
  if(!is.na(lh.indvbehav.behav$carc_cat[i]) & lh.indvbehav.behav$carc_cat[i] == "s" & 
     !is.na(lh.indvbehav.behav$size_cat[i]) & lh.indvbehav.behav$size_cat[i] == "scrap"){
    lh.indvbehav.behav$food_size[i] <- "xs"
  }
  #Unknown carcasses (assume carc_cat = m)
  if(is.na(lh.indvbehav.behav$carc_cat[i]) & lh.indvbehav.behav$prey_type[i] == "unknown" & 
     !is.na(lh.indvbehav.behav$size_cat[i]) & lh.indvbehav.behav$size_cat[i] == "carcass"){
    lh.indvbehav.behav$food_size[i] <- "m"
  }
  if(is.na(lh.indvbehav.behav$carc_cat[i]) & lh.indvbehav.behav$prey_type[i] == "unknown" & 
     !is.na(lh.indvbehav.behav$size_cat[i]) & lh.indvbehav.behav$size_cat[i] == "piece"){
    lh.indvbehav.behav$food_size[i] <- "s"
  }
  if(is.na(lh.indvbehav.behav$carc_cat[i]) & lh.indvbehav.behav$prey_type[i] == "unknown" & 
     !is.na(lh.indvbehav.behav$size_cat[i]) & lh.indvbehav.behav$size_cat[i] == "scrap"){
    lh.indvbehav.behav$food_size[i] <- "xs"
  }
}


########## 5.9 Create final dataset lh.indvbehav.food ##########

#Clean and create final dataset
lh.indvbehav.food <- lh.indvbehav.behav[,c(1:11,17:18,12:13,16,19:20)]
colnames(lh.indvbehav.food)[1] <- "event"
colnames(lh.indvbehav.food)[2] <- "session"
colnames(lh.indvbehav.food)[3] <- "hyena"
colnames(lh.indvbehav.food)[4] <- "time"
colnames(lh.indvbehav.food)[5] <- "order"
colnames(lh.indvbehav.food)[6] <- "behavior"
colnames(lh.indvbehav.food)[7] <- "target"
colnames(lh.indvbehav.food)[8] <- "distance"
colnames(lh.indvbehav.food)[9] <- "leader"
colnames(lh.indvbehav.food)[10] <- "food"
colnames(lh.indvbehav.food)[11] <- "notes"
lh.indvbehav.food$behav_cat <- as.factor(lh.indvbehav.food$behav_cat)
lh.indvbehav.food$prey_type <- as.factor(lh.indvbehav.food$prey_type)
lh.indvbehav.food$prey_age <- as.factor(lh.indvbehav.food$prey_age)
lh.indvbehav.food$carc_cat <- as.factor(lh.indvbehav.food$carc_cat)
lh.indvbehav.food$carc_cat <- ordered(lh.indvbehav.food$carc_cat, levels = c("s", "m", "l", "xl"))
lh.indvbehav.food$size_cat <- as.factor(lh.indvbehav.food$size_cat)
lh.indvbehav.food$food_size <- as.factor(lh.indvbehav.food$food_size)
lh.indvbehav.food$food_size <- ordered(lh.indvbehav.food$food_size, levels = c("xs", "s", "m", "l", "xl"))
summary(lh.indvbehav.food)
summary(filter(lh.indvbehav.food, behav_cat == "fd"))

save(file = "06.lh_data_behav_food.Rdata", list = c("lh.sessions.food", "lh.indvbehav.food"))




