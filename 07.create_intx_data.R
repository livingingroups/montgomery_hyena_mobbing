################################################################################
##### 7.0 Create lion-hyena interaction sessions dataset #####
################################################################################

########## 7.1 Set working directory & download packages/tables ##########

rm(list = ls())
setwd("~/Documents/R/LionHyena")
options(stringsAsFactors = FALSE)
library(asnipe)
library(tidyverse)
hyenadata::update_tables("1.2.88")
library(hyenadata)

#Load data
load("02.cleaned_lh_data.Rdata")
load("06.lh_data_behav_food.Rdata")

#Remove  datasets not used here
rm(lh.indvbehav)
rm(lh.indvbehav.food)
rm(lh.indvinfo)
rm(lh.indvsessions)
rm(lh.intx.all)
rm(lh.intx)
rm(lh.sessions)


########## 7.2 Load and clean core demography tables ##########

#tblHyenas
data("tblHyenas")
tblHyenas$mom <- gsub(" ", "", tblHyenas$mom)
tblHyenas$dad <- gsub(" ", "", tblHyenas$dad)
tblHyenas$sex <- as.factor(tblHyenas$sex)
tblHyenas$status <- as.factor(tblHyenas$status)
tblHyenas <- tblHyenas[,c(1,7:11,15)]
tblHyenas <- filter(tblHyenas, !is.na(id))  
tblHyenas <- unique(tblHyenas)

#Add in estimated birthdates (Van Horn) for hyenas without birthdates in tblHyenas
load("00.raw_data/birthdates_estimated.Rdata")
tblHyenas[tblHyenas$id == "gaza",]$birthdate <- NA
for(i in 1:nrow(tblHyenas)){
  id.i <- tblHyenas$id[i]
  if(is.na(tblHyenas$birthdate[i]) & id.i %in% birthdates_estimated$id){
    tblHyenas$birthdate[i] <- filter(birthdates_estimated, id == id.i)$birthdate_estimated
  }
}
rm(birthdates_estimated)
rm(id.i)
rm(i)

#Remove non-real hyenas
filter(tblHyenas, grepl("/", tblHyenas$id) | grepl("\\?", tblHyenas$id) | grepl("unid", tblHyenas$id) | 
         +          grepl("alien", tblHyenas$id) | grepl("cub", tblHyenas$id) | grepl("hyena", tblHyenas$id))
tblHyenas <- filter(tblHyenas, id != "cubs" & id != "unid3" & id != "lop?")

#tblSessions
data("tblSessions")
tblSessions$clan <- as.factor(tblSessions$clan)
tblSessions$location <- as.factor(tblSessions$location)
tblSessions$start <- as.POSIXct(tblSessions$start, format = "%H:%M:%S")
tblSessions$stop <- as.POSIXct(tblSessions$stop, format = "%H:%M:%S")
tblSessions$hyenas <- tolower(gsub(" ", "", tblSessions$hyenas))
tblSessions$unidhyenas <- tolower(tblSessions$unidhyenas)
tblSessions <- tblSessions[,1:9]
tblSessions <- filter(tblSessions, !is.na(session)) 
tblSessions <- unique(tblSessions)

#tblHyenasPerSession
data("tblHyenasPerSession")
tblHyenasPerSession <- tblHyenasPerSession[,1:2]
tblHyenasPerSession <- filter(tblHyenasPerSession, !is.na(session) & !is.na(id))
tblHyenasPerSession <- unique(tblHyenasPerSession)
sessions.hyenas <- left_join(tblSessions, tblHyenasPerSession, by = "session")

#tblPredatorsPerSession
data("tblPredatorsPerSession")
tblPredatorsPerSession$predator <- tolower(gsub(" ", "", tblPredatorsPerSession$predator))
summary(as.factor(tblPredatorsPerSession$predator))
tblPredatorsPerSession <- filter(tblPredatorsPerSession, predator == "lion")     #filter to only lions - 8522 rows
tblPredatorsPerSession$sex <- tolower(gsub(" ", "", tblPredatorsPerSession$sex))
tblPredatorsPerSession[tblPredatorsPerSession$sex == "u" & !is.na(tblPredatorsPerSession$sex),]$sex <- NA
tblPredatorsPerSession$sex <- as.factor(tblPredatorsPerSession$sex)
tblPredatorsPerSession$age <- tolower(gsub(" ", "", tblPredatorsPerSession$age))
tblPredatorsPerSession[(tblPredatorsPerSession$age == "cub" |  tblPredatorsPerSession$age == "subadult") &
                         !is.na(tblPredatorsPerSession$age),]$age <- "juvenile"     #combine cubs and subs into juvenile age class
tblPredatorsPerSession[tblPredatorsPerSession$age == "unknown" &
                         !is.na(tblPredatorsPerSession$age),]$age <- NA
tblPredatorsPerSession$age <- as.factor(tblPredatorsPerSession$age)
tblPredatorsPerSession$num_individuals <- as.numeric(tblPredatorsPerSession$num_individuals)
tblPredatorsPerSession <- tblPredatorsPerSession[,1:6]

#Filter to sessions in lh.sessions.all & fix num_individuals where possible
tblPredatorsPerSession <- filter(tblPredatorsPerSession, session %in% lh.sessions.all$Session)     #1460
filter(tblPredatorsPerSession, !(!is.na(num_individuals) & num_individuals > 0))[,c(1,6)]
tblPredatorsPerSession[tblPredatorsPerSession$session == "t1636.1",]
tblPredatorsPerSession[tblPredatorsPerSession$session == "t1637.1",]$sex <- "f"
tblPredatorsPerSession[tblPredatorsPerSession$session == "t1637.1",]$age <- "adult"
tblPredatorsPerSession[tblPredatorsPerSession$session == "t1637.1",]$num_individuals <- 3
tblPredatorsPerSession <- rbind(tblPredatorsPerSession, 
                                data.frame(session = "t1637.1", predator = "lion", 
                                           sex = NA, age = "juvenile", num_individuals = 8, 
                                           notes = "these are probably still the same lions mentioned in session 1636.1"))
tblPredatorsPerSession <- rbind(tblPredatorsPerSession, 
                                data.frame(session = "t1238", predator = "lion", 
                                           sex = NA, age = NA, num_individuals = 6, 
                                           notes = "total of 6 lions, two types (lionesses & big cubs) present; not told how many of each"))
tblPredatorsPerSession <- filter(tblPredatorsPerSession, !is.na(num_individuals) & num_individuals > 0)     #remove 52
tblPredatorsPerSession <- unique(tblPredatorsPerSession)     #remove 3

#Add total lions column
tblPredatorsPerSession <- filter(tblPredatorsPerSession, predator == "lion")
tblPredatorsPerSession$total_lions <- NA
for(i in 1:nrow(tblPredatorsPerSession)){
  all <- filter(tblPredatorsPerSession, session == tblPredatorsPerSession$session[i])
  #sum all lion columns
  tblPredatorsPerSession$total_lions[i] <- sum(all$num_individuals)
}
rm(all)
rm(i)

#tblClanMembership
data("tblClanMembership")
tblClanMembership$id <- tolower(gsub(" ", "", tblClanMembership$id))
tblClanMembership$clan <- tolower(gsub(" ", "", tblClanMembership$clan))

#tblGreetings
load("00.raw_data/tblGreetings_fixed_20210617.Rdata")
tblGreetings <- tblGreetings.save
rm(tblGreetings.save)
tblGreetings$time <- format(tblGreetings$time, format = "%H:%M")
tblGreetings$time <- as.POSIXct(tblGreetings$time, format = "%H:%M")
tblGreetings <- tblGreetings[,c(1,4:6)]
tblGreetings <- unique(tblGreetings)

#tblAggression
load("00.raw_data/tblAggression_fixed_20210617.Rdata")
tblAggression <- tblAggression.save
rm(tblAggression.save)
tblAggression$time <- format(tblAggression$time, format = "%H:%M")
tblAggression$time <- as.POSIXct(tblAggression$time, format = "%H:%M")
tblAggression <- tblAggression[,c(3,5:7)]
tblAggression <- unique(tblAggression)

#tblEats
data('tblEats')
tblEats <- tblEats[,1:5]

#tblPreyCensus
data("tblPreyCensus")
tblPreyCensus <- filter(tblPreyCensus, clan == "happy.zebra" | clan == "kcm" | 
                          clan == "serena.n" | clan == "serena.s" | clan == "talek.w")
tblPreyCensus[tblPreyCensus$clan == "kcm" & !is.na(tblPreyCensus$clan),]$clan <- "talek.w"    #kcm = talek.w for this analysis (see above)
tblPreyCensus[tblPreyCensus$clan == "talek.w" & !is.na(tblPreyCensus$clan),]$clan <- "talek" 
tblPreyCensus$year <- as.numeric(format(tblPreyCensus$date, "%Y"))
tblPreyCensus <- filter(tblPreyCensus, year <= 2016)     #only years of study
tblPreyCensus$month <- as.numeric(format(tblPreyCensus$date, "%m"))
tblPreyCensus[,c(4,9:39)] <- sapply(tblPreyCensus[,c(4,9:39)], as.numeric)

#Data: each clan has 2-4 transects
# Transects are 1.45-5.40 km in length
# Prey is surveyed for 100m on either side of transect
# Transects are surveyed 2x per month (once in first half of month, once in second half)

#Set up column categories
prey_all <- c("thomsons", "impala", "zebra", "wildebeest", "topi", "warthog", "hartebeest", "grants", 
              "buffalo", "hippo", "giraffe", "ostrich", "eland", "elephant", "oribi", "reedbuck", 
              "waterbuck", "baboon", "bushbuck")
to.sum <- c("distance", prey_all)

#Calculate prey density data
prey.summary <- tblPreyCensus %>% group_by(region, clan, year, month) %>%
  summarise_at(vars(all_of(to.sum)), sum)     #prey density calculated per month 
prey.summary$total_prey_count <- rowSums(prey.summary[,prey_all])
prey.summary$area <- prey.summary$distance*0.2    #prey censused for 100m on either side of road (e.g., census of distance = 1km has an area of 0.2 km2)
prey.summary$prey_density <- as.numeric(prey.summary$total_prey_count/prey.summary$area)     #prey density calculated as # animals per km2

#Calculate prey relative to entire calendar year (SD from the mean; following Lehmann et al. 2017)
prey.summary$num_sd_year <- NA
for(i in 1:nrow(prey.summary)){
  clan.i <- prey.summary$clan[i]
  year.i <- prey.summary$year[i]
  month.i <- prey.summary$month[i]
  census.i <- filter(prey.summary, clan == clan.i & year == year.i)
  census.i$num_sd_year <- scale(census.i$prey_density, center = T, scale = T)
  if(nrow(census.i) > 0){
    prey.summary$num_sd_year[i] <- as.numeric(filter(census.i, month == month.i)$num_sd_year)
  }
  rm(census.i)
}
tblPreyDensity <- prey.summary[,c(1:5,25:28)]
rm(prey.summary)
rm(tblPreyCensus)


########## 7.3 Create dataset - sessions.all = potential interaction sessions (all lion-hyena sessions ) ##########

#Create dataset
sessions.all <- lh.sessions.all[,c(1:5,9:13,7)]
sessions.all <- left_join(sessions.all, tblSessions[,c(2:6)], by = c("Session" = "session"))
sessions.all <- left_join(sessions.all, unique(tblPredatorsPerSession[,c(1,7)]),
                          by = c("Session" = "session"))

#Calculate session length
sessions.all$session_length <- as.numeric(sessions.all$stop - sessions.all$start)
sessions.all[sessions.all$session_length < 5 &      #all sessions are min 5 min long 
               !is.na(sessions.all$session_length),]$session_length <- 5

#Talek clan fissioned in mid-2016; consider them all to be Talek clan for this analysis
sessions.all[sessions.all$clan == "kcm",]$clan <- "talek"
sessions.all[sessions.all$clan == "pond",]$clan <- "talek"
sessions.all[sessions.all$clan == "talek.w",]$clan <- "talek"
sessions.all$clan <- as.factor(as.character(sessions.all$clan))

#Combine session locations to include only carcass, kill, den, and other
sessions.all[sessions.all$location == "m",]$location <- "o"     #mating = other
sessions.all[sessions.all$location == "n",]$location <- "d"     #natal den = den
sessions.all$location <- as.factor(as.character(sessions.all$location))
sessions.all$location <- relevel(sessions.all$location, ref = "o")

#Create context column for resource presence (food, den, other)
sessions.all$context <- NA
sessions.all[sessions.all$location == "k" | sessions.all$location == "c" &
               !is.na(sessions.all$location),]$context <- "fd"
sessions.all[sessions.all$location == "d" & !is.na(sessions.all$location),]$context <- "den"
sessions.all[sessions.all$location == "o" & !is.na(sessions.all$location),]$context <- "other"
sessions.all$context <- as.factor(as.character(sessions.all$context))
sessions.all$context <- relevel(sessions.all$context, ref = "other")

#Create columns for male lion presence & number
sessions.all$num.rows <- NA
sessions.all$total_male_lions <- 0
for(i in 1:nrow(sessions.all)){
  session.i <- sessions.all$Session[i]
  predators.i <- filter(tblPredatorsPerSession, session == session.i & sex == "m" & age == "adult")
  sessions.all$num.rows[i] <- nrow(predators.i)
  if(nrow(predators.i) > 0){
    sessions.all$total_male_lions[i] <- predators.i$num_individuals
  }
  if(nrow(predators.i) == 0){
    sessions.all$total_male_lions[i] <- 0
  }
}
rm(predators.i)

#Look at sessions with multiple male_lion rows
filter(sessions.all, num.rows > 1)[,1]     #2
sessions.all[sessions.all$Session == "t3357",]$total_male_lions <- 2
sessions.all[sessions.all$Session == "t3357",]$total_lions <- 2
sessions.all[sessions.all$Session == "t5728",]$total_male_lions <- 4
sessions.all[sessions.all$Session == "t5728",]$total_lions <- 9

#Calculate presence/absence of male lions
sessions.all$male_lions_present <- NA
for(i in 1:nrow(sessions.all)){
  if(sessions.all$total_male_lions[i] != 0){
    sessions.all$male_lions_present[i] <- TRUE
  }
  if(sessions.all$total_male_lions[i] == 0){
    sessions.all$male_lions_present[i] <- FALSE
  }
}
sessions.all$num.rows <- NULL

#Check lion count - mismatch between total_lions and lion_count [will keep total_lions column]
filter(sessions.all, total_lions != lion_count)[,c("Session", "date", "lion_count", 
                                                   "total_lions", "total_male_lions", "male_lions_present")]
#     Session       date lion_count total_lions total_male_lions male_lions_present    correct:
#       t2183 1992-06-08          7           8                0              FALSE    8 lions
#       t2194 1992-06-12          3           6                0              FALSE
#       t3357 1992-12-24          3           2                2               TRUE
#       t8358 1994-12-28          2           3                0              FALSE
#  t30553.161 2001-12-25          8           4                0              FALSE
#      t57106 2006-04-02          4           3                0              FALSE    4 lions - 1 adult female, 3 juveniles
#      t62438 2008-04-16         10           7                1               TRUE    10 lions - 1 adult male, 3 adult female, 6 juveniles
sessions.all[sessions.all$Session == "t57106",]$total_lions <- 4
sessions.all[sessions.all$Session == "t62438",]$total_lions <- 10

#Check lion count - missing in total_lions but not lion_count [will keep total_lions column]
filter(sessions.all, is.na(total_lions) & !is.na(lion_count))[,c("Session", "date", "lion_count", 
                                                                 "total_lions", "total_male_lions", "male_lions_present")]
#  Session       date lion_count total_lions total_male_lions male_lions_present    correct:
#   t22025 2000-02-12          1          NA                0              FALSE    1 adult male lion
#   t31033 2002-02-09          1          NA                0              FALSE    1 adult female lion
#   t53055 2005-06-15          1          NA                0              FALSE    same as t53057 - 4 adult females, 5 juveniles
sessions.all[sessions.all$Session == "t22025",]$total_lions <- 1
sessions.all[sessions.all$Session == "t22025",]$total_male_lions <- 1
sessions.all[sessions.all$Session == "t22025",]$male_lions_present <- TRUE
sessions.all[sessions.all$Session == "t31033",]$total_lions <- 1
sessions.all[sessions.all$Session == "t53055",]$total_lions <- 9

#Calculate presence/absence of cubs < 1 year of age
sessions.all$cubs_present <- NA
for(i in 1:nrow(sessions.all)){
  hyenas.i <- filter(tblHyenasPerSession, session == sessions.all$Session[i])
  hyenas.i <- left_join(hyenas.i, tblHyenas[,c(1,3)], by = "id")
  hyenas.i$age <- as.numeric(sessions.all$date[i] - hyenas.i$birthdate)/365
  if(any(hyenas.i$age <= 1, na.rm = TRUE)){
    sessions.all$cubs_present[i] <- TRUE
  } else if(all(hyenas.i$age > 1 | is.na(hyenas.i$age), na.rm = TRUE)){
    sessions.all$cubs_present[i] <- FALSE
  }
}
rm(hyenas.i)

#Add prey density data
sessions.all$year <- as.numeric(format(sessions.all$date, "%Y"))
sessions.all$month <- as.numeric(format(sessions.all$date, "%m"))
sessions.all <- left_join(sessions.all, tblPreyDensity, by = c("clan", "year", "month"))

#Migration presence/absence - do for each side of the Mara (pick highest 4 prey-months)
serena.prey <- filter(tblPreyDensity, region == "Conservancy")
boxplot(serena.prey$num_sd_year ~ serena.prey$month)    #July, Aug, Sep, Oct
abline(h = median(serena.prey$num_sd_year))
talek.prey <- filter(tblPreyDensity, region == "Narok")
boxplot(talek.prey$num_sd_year ~ talek.prey$month)    #June, July, Aug, Sep
abline(h = median(talek.prey$num_sd_year))
rm(serena.prey)
rm(talek.prey)

sessions.all$migration <- NA
for(i in 1:nrow(sessions.all)){
  if(sessions.all$clan[i] == "talek"){
    sessions.all$migration[i] <- ifelse(sessions.all$month[i] == 6 | sessions.all$month[i] == 7 |
                                          sessions.all$month[i] == 8 | sessions.all$month[i] == 9, TRUE, FALSE)
  }
  if(sessions.all$clan[i] != "talek"){
    sessions.all$migration[i] <- ifelse(sessions.all$month[i] == 7 | sessions.all$month[i] == 8 |
                                          sessions.all$month[i] == 9 | sessions.all$month[i] == 10, TRUE, FALSE)
  }
}

#Reorder columns
sessions.all.full <- sessions.all
sessions.all <- sessions.all[,c(1,10,28:30,12,18,13:15,17,4,9,2,16,19,20,21,5:8,11)]
summary(sessions.all)


########## 7.4 Create dataset - sessions.intx = potential mobbing sessions ##########
########## (lion-hyena interaction sessions with more than one hyena present and high-quality notes) ##########

sessions.intx <- filter(sessions.all, hyena_count > 1 & Session %in% lh.sessions.checked$Session)
summary(sessions.intx)

#Check food_present vs context
check.context.fd <- rbind(filter(sessions.intx, context == "fd" & food_present == F),
                          filter(sessions.intx, context != "fd" & food_present == T))      #8 sessions
check.context.fd[,c("Session", "clan", "context", "date", "who_starts_with_food", "who_ends_with_food", "do_hyenas_eat")]
#  Session  clan context       date who_starts_with_food who_ends_with_food do_hyenas_eat
#   t50183 talek      fd 2004-12-01                 <NA>               <NA>          <NA>     #should be other
#   t51596 talek      fd 2005-03-05                 <NA>               <NA>          <NA>     #should be other
#   t82406 talek      fd 2013-05-20                 <NA>               <NA>          <NA>     #should be other
#    t3006 talek     den 1992-06-16               hyenas              lions          feed     #den trumps food contexts
#   t58038 talek     den 2006-06-04               hyenas               both          feed     #den trumps food contexts
#   t89945 talek     den 2015-01-18               hyenas             hyenas        scraps     #den trumps food contexts
#    t9117 talek     den 1996-03-11                lions             hyenas          feed     #den trumps food contexts
#    t9140 talek     den 1996-03-26               hyenas              lions          feed     #den trumps food contexts
sessions.intx[sessions.intx$Session == "t50183",]$context <- "other"
sessions.intx[sessions.intx$Session == "t50183",]$location <- "o"
sessions.intx[sessions.intx$Session == "t51596",]$context <- "other"
sessions.intx[sessions.intx$Session == "t51596",]$location <- "o"
sessions.intx[sessions.intx$Session == "t82406",]$context <- "other"
sessions.intx[sessions.intx$Session == "t82406",]$location <- "o"
rm(check.context.fd)

#Check cubs_present vs context
check.context.den <- filter(sessions.intx, context == "den" & cubs_present == F)     #6 sessions
check.context.den[,c("Session", "clan", "context", "date", "cubs_present")]
#  Session  clan context       date cubs_present
#   t135.2 talek     den 1988-09-12        FALSE     #den without cubs
#    t3064 talek     den 1992-07-24        FALSE     #den without cubs
#   t58038 talek     den 2006-06-04        FALSE     #should be food
#   t72258 talek     den 2011-02-28        FALSE     #should be other
#    t8626 talek     den 1995-03-25        FALSE     #den without cubs
#   t88150 talek     den 2014-09-26        FALSE     #den without cubs
sessions.intx[sessions.intx$Session == "t72258",]$context <- "other"
sessions.intx[sessions.intx$Session == "t72258",]$location <- "o"
sessions.intx[sessions.intx$Session == "t58038",]$context <- "fd"
sessions.intx[sessions.intx$Session == "t58038",]$location <- "k"
rm(check.context.den)

#Add prey_type data from lh.sessions.food
sessions.intx <- left_join(sessions.intx, lh.sessions.food[,c(1,14:16)],  by = "Session")
sessions.intx$carc_cat <- as.character(sessions.intx$carc_cat)
sessions.intx[sessions.intx$food_present == F & is.na(sessions.intx$carc_cat),]$carc_cat <- "none"     #replace NA with none for food_present = F

#Add prey_type data for remaining sessions from tblEats
for(i in 1:nrow(sessions.intx)){
  session.i <- sessions.intx$Session[i]
  if(sessions.intx$food_present[i] == T & is.na(sessions.intx$prey_type[i])){
    if(session.i %in% tblEats$session){
      if(!is.na(filter(tblEats, session == session.i)$prey_type)){
        sessions.intx$prey_type[i] <- filter(tblEats, session == session.i)$prey_type
      }
    }
  }
}
filter(sessions.intx, is.na(prey_type) & food_present == T)
sessions.intx[sessions.intx$Session == "t3006" & !is.na(sessions.intx$Session),]$prey_type <- "gazelle"

#Add prey_age data for remaining sessions
filter(sessions.intx, !is.na(prey_type) & prey_type != "unknown" & is.na(carc_cat))[,c("Session", "clan", "context", "date", 
                                                                                       "food_present", "prey_type", "prey_age", "carc_cat")]
#  Session  clan context       date food_present  prey_type prey_age carc_cat
#    t3006 talek     den 1992-06-16         TRUE    gazelle     <NA>     <NA>    #tommy carcass
#   t13174 talek      fd 1997-08-08         TRUE wildebeest     <NA>     <NA>    #adult gnu kill
#   t13436 talek      fd 1997-09-07         TRUE wildebeest     <NA>     <NA>    #ad gnu carc-head,spine&ribs
#   t19366 talek      fd 1999-08-06         TRUE wildebeest     <NA>     <NA>    #adult gnu carcass
#   t21104 talek      fd 1999-12-06         TRUE       topi     <NA>     <NA>    #baby topi
#   t53581 talek      fd 2005-07-30         TRUE wildebeest     <NA>     <NA>    #should be unknown
#   t78564 talek      fd 2012-09-17         TRUE wildebeest     <NA>     <NA>    #relatively fresh wildebeest (head and some skin from neck remain)
sessions.intx[sessions.intx$Session == "t13174" & !is.na(sessions.intx$Session),]$prey_age <- "adult"
sessions.intx[sessions.intx$Session == "t13436" & !is.na(sessions.intx$Session),]$prey_age <- "adult"
sessions.intx[sessions.intx$Session == "t19366" & !is.na(sessions.intx$Session),]$prey_age <- "adult"
sessions.intx[sessions.intx$Session == "t21104" & !is.na(sessions.intx$Session),]$prey_age <- "juvenile"
sessions.intx[sessions.intx$Session == "t53581" & !is.na(sessions.intx$Session),]$prey_type <- "unknown"

#Add carc_cat data for remaining sessions (categories from 05.clean_lh_data_behav_feed.R)
filter(sessions.intx, !is.na(prey_type) & prey_type != "unknown" & is.na(carc_cat))[,c("Session", "clan", "context", "date", 
                                                                                       "food_present", "prey_type", "prey_age", "carc_cat")]
# Session  clan context       date food_present  prey_type prey_age carc_cat
#    t3006 talek     den 1992-06-16         TRUE    gazelle     <NA>     <NA>
#   t13174 talek      fd 1997-08-08         TRUE wildebeest    adult     <NA>
#   t13436 talek      fd 1997-09-07         TRUE wildebeest    adult     <NA>
#   t19366 talek      fd 1999-08-06         TRUE wildebeest    adult     <NA>
#   t21104 talek      fd 1999-12-06         TRUE       topi juvenile     <NA>
#   t78564 talek      fd 2012-09-17         TRUE wildebeest     <NA>     <NA>
sessions.intx[sessions.intx$Session == "t3006" & !is.na(sessions.intx$Session),]$carc_cat <- "m"    #assume adult when prey_age is NA
sessions.intx[sessions.intx$Session == "t13174" & !is.na(sessions.intx$Session),]$carc_cat <- "l"
sessions.intx[sessions.intx$Session == "t13436" & !is.na(sessions.intx$Session),]$carc_cat <- "l"
sessions.intx[sessions.intx$Session == "t19366" & !is.na(sessions.intx$Session),]$carc_cat <- "l"
sessions.intx[sessions.intx$Session == "t21104" & !is.na(sessions.intx$Session),]$carc_cat <- "m"
sessions.intx[sessions.intx$Session == "t78564" & !is.na(sessions.intx$Session),]$carc_cat <- "l"    #assume adult when prey_age is NA

#Format prey and carc categories
sessions.intx$carc_cat <- as.factor(sessions.intx$carc_cat)
sessions.intx$carc_cat <- ordered(sessions.intx$carc_cat, levels = c("none", "s", "m", "l", "xl"))

#Add greetings data
sessions.intx$grts_occ <- NA
sessions.intx$num_indv_grts <- NA
for(i in 1:nrow(sessions.intx)){
  grts.i <- filter(tblGreetings, session == sessions.intx$Session[i])
  if(nrow(grts.i) > 0){
    #Does anyone greet?
    sessions.intx$grts_occ[i] <- TRUE
    #How many different individuals engage in greeting behavior?
    sessions.intx$num_indv_grts[i] <- sum(unique(c(grts.i$id1, grts.i$id2)) %in% tblHyenas$id)
  }
  if(nrow(grts.i) == 0){
    sessions.intx$grts_occ[i] <- FALSE
    sessions.intx$num_indv_grts[i] <-0
  }
}
rm(grts.i)

#Add aggression data
sessions.intx$agg_occ <- NA
sessions.intx$num_indv_agg <- NA
for(i in 1:nrow(sessions.intx)){
  agg.i <- filter(tblAggression, session == sessions.intx$Session[i])
  if(nrow(agg.i) > 0){
    #Does anyone aggress?
    sessions.intx$agg_occ[i] <- TRUE
    #How many different individuals engage in aggressive behavior?
    sessions.intx$num_indv_agg[i] <- sum(unique(c(agg.i$aggressor, agg.i$recip)) %in% tblHyenas$id)
  }
  if(nrow(agg.i) == 0){
    sessions.intx$agg_occ[i] <- FALSE
    sessions.intx$num_indv_agg[i] <-0
  }
}
rm(agg.i)

#Add association data
ai.list = list()
counter = 1
sessions.intx$session.ai.avg <- NA
for(i in 1:nrow(sessions.intx)){
  session.i <- sessions.intx$Session[i]
  date.i <- sessions.intx$date[i]
  hyena.count.i <- sessions.intx$hyena_count[i]
  clan.i <- sessions.intx$clan[i]
  #Filter to all individuals in the clan
  clan.ids <- filter(tblClanMembership, clan == clan.i, date.i >= start_date,
                     (date.i <= end_date | is.na(end_date)))
  #Filter to all clan-individuals in the session
  session.ids <- filter(clan.ids, id %in% filter(sessions.hyenas, session == session.i)$id)
  #Fix for KCM and Pond sessions
  if(clan.i == "talek" & date.i > "2016-05-01" & nrow(session.ids) <= 1){
    clan.i <- unique(filter(sessions.hyenas, session == session.i)$clan)
    if(length(clan.i) == 1){
      #Filter to all individuals in the clan
      clan.ids <- filter(tblClanMembership, clan == clan.i, date.i >= start_date,
                         (date.i <= end_date | is.na(end_date)))
      #Filter to all clan-individuals in the session
      session.ids <- rbind(session.ids, filter(clan.ids, id %in% filter(sessions.hyenas, session == session.i)$id))
    }
  }
  #Filter to all sessions with session-individuals within 1 year of the lh.session
  sessions.ai <- filter(sessions.hyenas, id %in% session.ids$id, date <= date.i, date >= (date.i - 365))
  ids <- session.ids$id
  if(nrow(session.ids) > 0){
    #Association network
    sessions.gbi <- get_group_by_individual(association_data = data.frame(id = sessions.ai$id,
                                                                          group = sessions.ai$session),
                                            identities = ids,
                                            data_format = 'individuals')
    association.network <- get_network(sessions.gbi, 'GBI', 'SRI')
    ai.list[[counter]] <- association.network
    
    #Calculate average AI scores for the individuals at the session
    session.ids$session.ai.avg <- NA
    for(j in 1:nrow(session.ids)){
      id.j = session.ids$id[j]
      session.ids[session.ids$id == id.j,]$session.ai.avg <-
        mean(association.network[id.j, session.ids[session.ids$id != id.j,]$id])
    }
    
    #Average those scores across all individuals present at the session
    sessions.intx$session.ai.avg[i] <- mean(session.ids$session.ai.avg)
  }
  counter <- counter+1
}
sessions.intx[is.nan(sessions.intx$session.ai.avg),]$session.ai.avg <- NA
sessions.intx[sessions.intx$session.ai.avg > 0.5 & !is.na(sessions.intx$session.ai.avg),]$session.ai.avg <- NA    #remove 1 - cubs & littermates
rm(session.i)
rm(date.i)
rm(hyena.count.i)
rm(clan.i)
rm(clan.ids)
rm(session.ids)
rm(sessions.ai)
rm(ids)

#Check sessions with NA in session.ai.avg
check.ai <- filter(sessions.hyenas, session %in% filter(sessions.intx, is.na(session.ai.avg))$Session) %>% 
  group_by(session) %>% summarise(num.hyenas = length(id))
filter(check.ai, num.hyenas > 1)
#  session  num.hyenas     i  notes
#  t1870.02          2    91  hk & sd - cubs & littermates
#  t38556           15    163 talek.e session - 1 study animal 
#  t38590            3    164 1 study animal with MR and AL hyenas
#  t53074           17    184 talek.e session - no study animals 
#  t54390           13    188 talek.e session - 1 study animal
#  t75684            2    252 talek.e session - no study animals

#Add mobbing behavior - number of mobs and size of mobs (#hyenas/mob)
sessions.intx$num_mobs <- NA
sessions.intx$size_mobs_mean <- NA
sessions.intx$size_mobs_median <- NA
for(i in 1:nrow(sessions.intx)){
  session.i <- sessions.intx$Session[i]
  mobs.i <- filter(lh.intx.checked, session == session.i & mobbing == T)
  if(nrow(mobs.i) > 0){
    sessions.intx$num_mobs[i] <- nrow(mobs.i)
    sessions.intx$size_mobs_mean[i] <- mean(mobs.i$num_hyenas, na.rm = T)
    sessions.intx$size_mobs_median[i] <- median(mobs.i$num_hyenas, na.rm = T)
  }
  if(nrow(mobs.i) == 0){
    sessions.intx$num_mobs[i] <- 0
  }
}
rm(mobs.i)

#Add whether or not mobbing occurred (T/F)
sessions.intx$mobbing <- NA
for(i in 1:nrow(sessions.intx)){
  if(sessions.intx$num_mobs[i] == 0 | is.na(sessions.intx$num_mobs[i])){
    sessions.intx$mobbing[i] <- FALSE
  }
  if(sessions.intx$num_mobs[i] != 0 & !is.na(sessions.intx$num_mobs[i])){
    sessions.intx$mobbing[i] <- TRUE
  }
}

#Reorder columns
sessions.intx.full <- sessions.intx
sessions.intx <- sessions.intx[,c(1:11,14:19,26:31,23,32:35)]
summary(sessions.intx)

save(file = "08.sessions.intx.Rdata", list = c("sessions.all", "sessions.intx"))



