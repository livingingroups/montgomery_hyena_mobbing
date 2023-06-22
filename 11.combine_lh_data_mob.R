######################################################################
##### 11.0 Combining lion-hyena mob data #####
######################################################################

########## 11.1 Set working directory & download packages/tables ##########

rm(list = ls())
setwd("~/Documents/R/LionHyena")
options(stringsAsFactors = FALSE)
library(asnipe)
library(tidyverse)
hyenadata::update_tables("1.2.88")
library(hyenadata)

#Load data
load("02.cleaned_lh_data.Rdata")
load("04.lh_data_mob.Rdata")
load("08.sessions.intx.Rdata")

#Remove datasets not used here
rm(lh.indvbehav)
rm(lh.indvsessions)
rm(lh.intx)
rm(lh.intx.all)
rm(lh.intx.checked)
rm(lh.sessions)
rm(lh.sessions.all)
rm(lh.sessions.checked)
rm(sessions.all)


########## 11.2 Load and clean core demography tables ##########

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
sessions.hyenas <- full_join(tblSessions, tblHyenasPerSession, by = "session")
sessions.hyenas$year <- as.numeric(format(sessions.hyenas$date, "%Y"))

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

#tblClanMembership
data("tblClanMembership")
tblClanMembership$id <- tolower(gsub(" ", "", tblClanMembership$id))
tblClanMembership$clan <- tolower(gsub(" ", "", tblClanMembership$clan))
tblClanMembership[is.na(tblClanMembership$end_date),]$end_date <- "2020-01-01"   #Assign end_date of 2020 to anyone without an end_date

#tblFemaleRanks
data("tblFemaleRanks")
tblFemaleRanks$id <- tolower(gsub(" ", "", tblFemaleRanks$id))
tblFemaleRanks$clan <- tolower(gsub(" ", "", tblFemaleRanks$clan))
tblFemaleRanks$sex <- 1
tblFemaleRanks$stan_rank <- NULL
tblFemaleRanks[tblFemaleRanks$id == "2" & !is.na(tblFemaleRanks$id),]$id <- "02"
tblFemaleRanks[tblFemaleRanks$id == "3" & !is.na(tblFemaleRanks$id),]$id <- "03"

#Remove disappeared females
tblFemaleRanks$seen <- NA
tblFemaleRanks$first <- NA
tblFemaleRanks$last <- NA
for(i in 1:nrow(tblFemaleRanks)){
  sessions.i <- filter(sessions.hyenas, id == tblFemaleRanks$id[i])
  if(nrow(filter(sessions.i, year == tblFemaleRanks$year[i])) > 0){
    tblFemaleRanks$seen[i] <- TRUE
  } else {
    tblFemaleRanks$seen[i] <- FALSE
  }
  if(tblFemaleRanks$seen[i] == FALSE){
    tblFemaleRanks$first[i] <- min(sessions.i$year)
    tblFemaleRanks$last[i] <- max(sessions.i$year)
    
  }
}
rm(sessions.i)
remove <- filter(tblFemaleRanks, seen == FALSE & (year < first | year > last))     #16 females
tblFemaleRanks <- anti_join(tblFemaleRanks, remove)
rm(remove)

#tblMaleRanks
load("00.raw_data/4.male_ranks.RData")
tblMaleRanks <- ranks
colnames(tblMaleRanks)[1] <- "year"
tblMaleRanks$id <- tolower(gsub(" ", "", tblMaleRanks$id))
tblMaleRanks$clan <- tolower(gsub(" ", "", tblMaleRanks$clan))
tblMaleRanks$sex <- 2
tblMaleRanks <- tblMaleRanks[,c(6,1,2,3,7)]
rm(ranks)

#Add PENE and ZITI
##Born 2010-04-03, should be added to male ranks when they are 5 years old = on Jan 1 2016
##PENE dominant to ZITI according to litrank
tblMaleRanks <- rbind(tblMaleRanks, data.frame(clan = "talek", year = 2016, id = "pene", 
                                               rank = 0.4, sex = 2), 
                      data.frame(clan = "talek", year = 2016, id = "ziti", 
                                 rank = 0.6, sex = 2))

#Remove disappeared males
tblMaleRanks$seen <- NA
tblMaleRanks$first <- NA
tblMaleRanks$last <- NA
for(i in 1:nrow(tblMaleRanks)){
  sessions.i <- filter(sessions.hyenas, id == tblMaleRanks$id[i])
  if(nrow(filter(sessions.i, year == tblMaleRanks$year[i])) > 0){
    tblMaleRanks$seen[i] <- TRUE
  } else {
    tblMaleRanks$seen[i] <- FALSE
  }
  if(tblMaleRanks$seen[i] == FALSE){
    tblMaleRanks$first[i] <- min(sessions.i$year)
    tblMaleRanks$last[i] <- max(sessions.i$year)
    
  }
}
rm(sessions.i)
remove <- filter(tblMaleRanks, seen == FALSE & (year < first | year > last))     #remove 30 males
tblMaleRanks <- anti_join(tblMaleRanks, remove)
rm(remove)

#Combine female and male ranks
tblRanks <- rbind(tblFemaleRanks, tblMaleRanks)
tblRanks <- tblRanks %>%
  group_by(clan, year) %>%
  arrange(sex, rank) %>%
  mutate(rank = 1:length(id),
         stan_rank = 1 + -2*(rank-1)/(length(rank)-1)) %>%
  arrange(clan, year, rank)
tblRanks$stan_rank <- as.numeric(tblRanks$stan_rank)
rm(tblFemaleRanks)
rm(tblMaleRanks)

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

#tblReproStates
data("tblReproStates")
tblReproStates$mom <- tolower(gsub(" ", "", tblReproStates$mom))
tblReproStates <- tblReproStates[,1:9]
#Add parity to state = "other" 
for(i in 1:nrow(tblReproStates)){
  if(tblReproStates$state[i] == "o" & is.na(tblReproStates$parity[i])){
    if(tblReproStates$mom[i] == tblReproStates$mom[i-1]){
      tblReproStates$parity[i] <- tblReproStates$parity[i-1]
    }
  }
}
rm(i)


########## 11.3 Fix/sync time columns in all datasets ##########

lh.intx.mob$time <- format(lh.intx.mob$time, "%H:%M:%S")
lh.intx.mob$time <- as.POSIXct(lh.intx.mob$time, format = "%H:%M:%S")
summary(lh.intx.mob$time)

lh.indvinfo$TimeFirstSeen <- format(lh.indvinfo$TimeFirstSeen, "%H:%M:%S")
lh.indvinfo$TimeFirstSeen <- as.POSIXct(lh.indvinfo$TimeFirstSeen, format = "%H:%M:%S")
summary(lh.indvinfo$TimeFirstSeen)

lh.indvinfo$LastSeenOld.Confirmed <- format(lh.indvinfo$LastSeenOld.Confirmed, "%H:%M:%S")
lh.indvinfo$LastSeenOld.Confirmed <- as.POSIXct(lh.indvinfo$LastSeenOld.Confirmed, format = "%H:%M:%S")
summary(lh.indvinfo$LastSeenOld.Confirmed)

lh.indvbehav.mob$time <- format(lh.indvbehav.mob$time, "%H:%M:%S")
lh.indvbehav.mob$time <- as.POSIXct(lh.indvbehav.mob$time, format = "%H:%M:%S")
summary(lh.indvbehav.mob$time)

tblGreetings$time <- as.POSIXct(tblGreetings$time, format = "%H:%M:%S")
summary(tblGreetings$time)

tblAggression$time <- as.POSIXct(tblAggression$time, format = "%H:%M:%S")
summary(tblAggression$time)


########## 11.4 Combine mobbing & demography datasets ##########

lh.indvbehav.mob <- filter(lh.indvbehav.mob, !grepl("unid", lh.indvbehav.mob$hyena))

#Create dataframe
id.by.mob.list = list()
counter = 1
nm.missing <- 0
all.ids <- data.frame(session=character(), mobID=character(), hyena=character(), 
                      time=character(), order=character(), mobber=logical())

#Epic for-loop
for(mobid in unique(lh.indvbehav.mob$mobID)){
  mobbers = filter(lh.indvbehav.mob, mobID == mobid)
  mobbers$mobber <- TRUE
  
  #Add other hyenas present at session
  non_mobbers <- filter(sessions.hyenas, session == mobbers$session[1])$id
  non_mobbers <- non_mobbers[!non_mobbers %in% mobbers$hyena]
  
  #Check if non_mobbers present for mob time
  if(length(non_mobbers)){
    non_mobbers_time <- filter(lh.indvinfo, (Hyena %in% non_mobbers) & (Session == mobbers$session[1])) 
    non_mobbers_time$present <- NA
    non_mobbers_time$present <-  ifelse((non_mobbers_time$LastSeenOld.Confirmed > mobbers$time[1]) & 
                                          (non_mobbers_time$TimeFirstSeen < mobbers$time[1]), 1, 0)
    nm.missing <- nm.missing + (length(non_mobbers) - nrow(non_mobbers_time))   #hyenas in sessions.hyenas but not lh.indvinfo
    non_mobbers <- non_mobbers_time[!is.na(non_mobbers_time$present) & non_mobbers_time$present == 1,]$Hyena
    
    #Create all.ids
    if(length(non_mobbers)){
      all.ids <- data.frame(session = mobbers$session[1], mobID = mobbers$mobID[1], 
                            hyena = non_mobbers, time = mobbers$time[1], 
                            order = mobbers$order[1], mobber = FALSE) %>%
        rbind(mobbers[,c('session', 'mobID', 'hyena', 'time', 'order', 'mobber')])
    }else{
      all.ids <- mobbers[,c('session', 'mobID', 'hyena', 'time', 'order', 'mobber')]
    }
  }else{
    all.ids <- mobbers[,c('session', 'mobID', 'hyena', 'time', 'order', 'mobber')]
  }
  rm(mobbers)
  rm(non_mobbers)
  rm(non_mobbers_time)
  
  #Remove non-study hyenas
  all.ids$times.seen <- NA
  for(i in 1:nrow(all.ids)){
    id.i <- all.ids$hyena[i]
    all.ids$times.seen[i] <- nrow(filter(sessions.hyenas, id == id.i & (date < "2018-01-01")))
  }
  all.ids <- filter(all.ids, times.seen > 5)   #break between 4 and 16
  all.ids$times.seen <- NULL
  rm(id.i)
  
  ### Session-level variables ###
  
  #Set session date
  session.date <- filter(sessions.intx, Session == all.ids$session[1])$date
  all.ids$date <- session.date
  
  #Assign session clan
  all.ids$clan <- filter(sessions.intx, Session == all.ids$session[1])$clan
  
  #Assign session context
  all.ids$context <- filter(sessions.intx, Session == all.ids$session[1])$context
  
  #Assign session-mob group size
  all.ids$group_size <- nrow(all.ids)
  
  ### Hyena-level variables ###
  
  #Assign hyena sex
  all.ids$sex <- left_join(all.ids, tblHyenas, by = c("hyena" = "id"))$sex
  
  #Assign hyena status
  all.ids$status <- NA
  for(i in 1:nrow(all.ids)){
    id.i <- all.ids$hyena[i]
    #Find current status in tblClanMembership
    if(nrow(filter(tblClanMembership, id == id.i & (session.date >= start_date) & (session.date <= end_date))) == 1){
      all.ids$status[i] <- 
        filter(tblClanMembership, id == id.i & (session.date >= start_date) & (session.date <= end_date))$status
    }
  }
  rm(id.i)
  
  #Assign hyena tenure for all immigrant males
  all.ids$tenure <- NA
  for(i in 1:nrow(all.ids)){
    id.i <- all.ids$hyena[i]
    if(all.ids$status[i] == "i"){
      if(nrow(filter(tblClanMembership, id == id.i & (session.date >= start_date) & 
                     (session.date <= end_date))) > 0){
        #Assign tenure in years
        all.ids$tenure[i] <- as.numeric(session.date - filter(tblClanMembership, id == id.i & 
                                                                (session.date >= start_date) & 
                                                                (session.date <= end_date))$start_date)/365
      }
    }
  }
  rm(id.i)
  
  #Assign hyena age in years
  all.ids$age <-  as.numeric(session.date -
                               left_join(all.ids, tblHyenas, by = c("hyena" = "id"))$birthdate)/365
  
  # Social ranks #
  
  #Assign social rank
  all.ids$stan.rank <- left_join(all.ids, filter(tblRanks, year == format(session.date, '%Y')), 
                                 by = c("hyena" = "id"))$stan_rank
  
  #For juveniles, assign social rank of mom
  mom.ranks <- left_join(all.ids, tblHyenas, by = c("hyena" = "id"))[,c('mom', 'hyena')] %>%
    left_join(., filter(tblRanks, year == format(session.date, '%Y')), by = c("mom" = "id"))
  ##Females assigned yearly ranks for each year where they are at least 18 months old at the start of the year
  all.ids[is.na(all.ids$stan.rank) & !is.na(all.ids$age) & (all.ids$age < 2.5),]$stan.rank <-   
    mom.ranks$stan_rank[mom.ranks$hyena %in%      #add mom's rank for anyone under age 3 without a rank yet
                          all.ids[is.na(all.ids$stan.rank) & !is.na(all.ids$age) & (all.ids$age < 2.5),]$hyena]
  ##Males assigned yearly ranks for each year where they are at least 5 years old at the start of the year [or when they are immigrants]
  all.ids[is.na(all.ids$stan.rank) & !is.na(all.ids$age) & (all.ids$age < 6) & all.ids$sex == "m",]$stan.rank <- 
    mom.ranks$stan_rank[mom.ranks$hyena %in%      #add mom's rank for males under age 5 without a rank yet
                          all.ids[is.na(all.ids$stan.rank) & !is.na(all.ids$age) & (all.ids$age < 6) & all.ids$sex == "m",]$hyena]
  rm(mom.ranks)
  
  #If mom's rank missing in current year (mom died/disappeared), assign social rank of mom of previous year(s) to juveniles
  for(year.step in 1:6){
    mom.ranks <- left_join(all.ids, tblHyenas, by = c("hyena" = "id"))[,c('mom', 'hyena')] %>%     #create table of mom ranks
      left_join(., filter(tblRanks, year == as.numeric(format(session.date, '%Y')) - year.step), by = c("mom" = "id"))
    ##Females assigned yearly ranks for each year where they are at least 18 months old at the start of the year
    all.ids[is.na(all.ids$stan.rank) & !is.na(all.ids$age) & (all.ids$age < 2.5),]$stan.rank <-   
      mom.ranks$stan_rank[mom.ranks$hyena %in%      #add mom's rank for anyone under age 3 without a rank yet
                            all.ids[is.na(all.ids$stan.rank) & !is.na(all.ids$age) & (all.ids$age < 2.5),]$hyena]
    ##Males assigned yearly ranks for each year where they are at least 5 years old at the start of the year [or when they are immigrants]
    all.ids[is.na(all.ids$stan.rank) & !is.na(all.ids$age) & (all.ids$age < 6) & all.ids$sex == "m",]$stan.rank <- 
      mom.ranks$stan_rank[mom.ranks$hyena %in%      #add mom's rank for males under age 5 without a rank yet
                            all.ids[is.na(all.ids$stan.rank) & !is.na(all.ids$age) & (all.ids$age < 6) & all.ids$sex == "m",]$hyena]
    rm(mom.ranks)
  }
  rm(year.step)
  
  # Reproductive states #
  
  #Assign reproductive state (pregnant, lactating, other, nulliparous) to adult females
  reprostates <- left_join(all.ids[,1:8], tblReproStates, by = c('hyena' = 'mom')) %>%
    filter(., (session.date >= cycle.start), (session.date <= cycle.stop))
  colnames(reprostates)[11] <- "repro.state"
  all.ids <- left_join(all.ids,reprostates[,c(3,11)], by = "hyena")
  
  #If hyena is a juvenile (< 2 years of age), assign repro.state of 'juvenile'
  if(nrow(all.ids[!is.na(all.ids$age) & (all.ids$age < 2) & is.na(all.ids$repro.state),]) > 0){
    all.ids[!is.na(all.ids$age) & (all.ids$age < 2),]$repro.state <- 'j'
  }
  
  #If hyena is an adult male (either > 2 years of age or an immigrant), assign repro.state of 'male'
  if(nrow(all.ids[(!is.na(all.ids$sex) & all.ids$sex == 'm') & (is.na(all.ids$age) | (all.ids$age >= 2)),]) > 0){
    all.ids[(!is.na(all.ids$sex) & all.ids$sex == 'm') & (is.na(all.ids$age) | (all.ids$age >= 2)),]$repro.state <- 'm'
  }
  
  #Assign parity to adult females
  all.ids <- left_join(all.ids,reprostates[,c(3,13)], by = "hyena")
  all.ids$parity <- match(all.ids$parity, letters)   #convert letters to numbers
  
  #Assign parity = 0 to nulliparous females (females who haven't given birth yet)
  if(nrow(all.ids[all.ids$repro.state == "n" & !is.na(all.ids$repro.state),]) > 0){
    all.ids[all.ids$repro.state == "n" & !is.na(all.ids$repro.state),]$parity <- 0
  }
  rm(reprostates)
  
  ### Behavior variables ###
  
  #Greetings: did the hyena greet in the 5 minutes prior to the mob?
  mob.grts <- filter(tblGreetings, session == all.ids$session[1], 
                     (time >= (all.ids$time[1] - 300)), (time <= all.ids$time[1]))   #300 seconds = 5 minutes
  all.ids$grt_occ <- NA
  all.ids$grt_occ_mobbers <- NA
  for(i in 1:nrow(all.ids)){
    id.i <- all.ids$hyena[i]
    #Did the hyena greet?
    grts.i <- filter(mob.grts, id.i == id1 | id.i == id2)
    if(nrow(grts.i) == 0){
      all.ids$grt_occ[i] <- FALSE
      all.ids$grt_occ_mobbers[i] <- FALSE
    }
    if(nrow(grts.i) > 0){
      all.ids$grt_occ[i] <- TRUE
      #Did the hyena greet with a mobber?
      grts.mob.i <- filter(grts.i, (id1 %in% filter(all.ids, mobber == T)$hyena & id1 != id.i) |
                             (id2 %in% filter(all.ids, mobber == T)$hyena & id2 != id.i))
      if(nrow(grts.mob.i) > 0){
        all.ids$grt_occ_mobbers[i] <- TRUE
      }
      if(nrow(grts.mob.i) == 0){
        all.ids$grt_occ_mobbers[i] <- FALSE
      }
      rm(grts.mob.i)
    }
    rm(grts.i)
  }
  rm(id.i)
  rm(mob.grts)
  
  #Aggression: was the hyena involved in an aggression in the 5 minutes prior to the mob?
  mob.aggs <- filter(tblAggression, session == all.ids$session[1], 
                     (time >= (all.ids$time[1] - 300)), (time <= all.ids$time[1]))   #300 seconds = 5 minutes
  all.ids$agg_occ <- NA
  all.ids$agg_occ_mobbers <- NA
  for(i in 1:nrow(all.ids)){
    id.i <- all.ids$hyena[i]
    #Was the hyena involved in an aggression?
    aggs.i <- filter(mob.aggs, id.i == aggressor | id.i == recip)
    if(nrow(aggs.i) == 0){
      all.ids$agg_occ[i] <- FALSE
      all.ids$agg_occ_mobbers[i] <- FALSE
    }
    if(nrow(aggs.i) > 0){
      all.ids$agg_occ[i] <- TRUE
      #Was the hyena involved in an aggression with a mobber?
      aggs.mob.i <- filter(aggs.i, (aggressor %in% filter(all.ids, mobber == T)$hyena & aggressor != id.i) | 
                             (recip %in% filter(all.ids, mobber == T)$hyena & recip != id.i))
      if(nrow(aggs.mob.i) > 0){
        all.ids$agg_occ_mobbers[i] <- TRUE
      }
      if(nrow(aggs.mob.i) == 0){
        all.ids$agg_occ_mobbers[i] <- FALSE
      }
      rm(aggs.mob.i)
    }
    rm(aggs.i)
  }
  rm(id.i)
  rm(mob.aggs)
  
  #Assign body condition
  all.ids$body.cond <- left_join(all.ids, lh.indvinfo, by = c("session" = "Session", "hyena" = "Hyena"))$BodyCondition
  
  ### Group-level variables ###
  
  # Relatedness #
  
  #Create maternal kinship matrix - moms & children get a 1, siblings get a 1
  ids <- all.ids$hyena
  kinmat <- matrix(dimnames = list(ids,ids), 0, ncol = length(ids), nrow = length(ids))
  for(row in 1:length(ids)){
    #Assign moms & kids a 1
    kinmat[row,] <- as.numeric(!is.na(tblHyenas[tblHyenas$id == ids[row],'mom']) &
                                 tblHyenas[tblHyenas$id == ids[row],'mom'] == ids)
    #Assign maternal siblings a 1
    kinmat[row,] <- kinmat[row,] + as.numeric(!is.na(tblHyenas[tblHyenas$id == ids[row],'mom']) &  
                                                !is.na(tblHyenas[match(ids, tblHyenas$id),'mom']) &
                                                tblHyenas[match(ids, tblHyenas$id),'mom'] == tblHyenas[tblHyenas$id == ids[row],'mom'])
  }
  rm(ids)
  rm(row)
  diag(kinmat) <- 0
  kinmat <- kinmat + t(kinmat)
  kinmat[] <- as.numeric(kinmat > 0)  
  
  #Assign proportion of hyenas to whom the focal hyena is related
  all.ids$prop.mob.related <- NA
  all.ids$prop.present.related <- NA
  for(i in 1:nrow(all.ids)){
    id.i <- all.ids$hyena[i]
    #Proportion of mobbers to whom the focal hyena is related
    all.ids[all.ids$hyena == id.i,]$prop.mob.related <- 
      mean(kinmat[id.i, all.ids[all.ids$hyena != id.i & all.ids$mobber == TRUE,]$hyena], na.rm = TRUE)
    #Proportion of hyenas present to whom the focal hyena is related
    all.ids[all.ids$hyena == id.i,]$prop.present.related <- 
      mean(kinmat[id.i, all.ids[all.ids$hyena != id.i,]$hyena], na.rm = TRUE)
  }
  rm(id.i)
  rm(kinmat)
  #note that ones which return as NA are mobs with a single mobber (and unIDs)  
  #note that ones which return as Nan / -Inf are mobs composed of only immigrant males
  
  # Association index #
  
  #Filter to all sessions with all.ids$hyena present in the last year
  ids <- all.ids$hyena
  sessions.AI <- filter(sessions.hyenas, id %in% ids,
                        date <= session.date, date >= (session.date - 365))
  
  #Create association network
  sessions.gbi <- get_group_by_individual(data.frame(id = sessions.AI$id,
                                                     group = sessions.AI$session,
                                                     date = sessions.AI$date),
                                          identities = ids,
                                          data_format = 'individuals')
  association.network <- get_network(sessions.gbi, 'GBI', 'SRI')
  rm(sessions.AI)
  rm(ids)
  rm(sessions.gbi)
  
  #Assign average association indicies
  all.ids$mob.ai.avg <- NA
  all.ids$present.ai.avg <- NA
  for(i in 1:nrow(all.ids)){
    id.i <- all.ids$hyena[i]
    #Focal hyena's association index with mobbers over past year
    all.ids[all.ids$hyena == id.i,]$mob.ai.avg <- 
      mean(association.network[id.i, all.ids[all.ids$hyena != id.i & all.ids$mobber == TRUE,]$hyena])
    #Focal hyena's association index with hyenas present over past year
    all.ids[all.ids$hyena == id.i,]$present.ai.avg <- 
      mean(association.network[id.i, all.ids[all.ids$hyena != id.i,]$hyena])
  }
  rm(id.i)
  rm(association.network)
  #note that ones which return as Nan / -Inf are ones which are single mobbers (aka they mobbed with unIDs)
  
  #Assign relative rank
  all.ids$mob.prop.rank.higher <- NA
  all.ids$present.prop.rank.higher <- NA
  for(i in 1:nrow(all.ids)){
    id.i <- all.ids$hyena[i]
    #Proportion of mobbers ranked higher than focal hyena
    all.ids[all.ids$hyena == id.i,]$mob.prop.rank.higher <- 
      length(which(all.ids[all.ids$hyena != id.i & all.ids$mobber == TRUE & !is.na(all.ids$stan.rank),]$stan.rank > 
                     all.ids[all.ids$hyena == id.i & !is.na(all.ids$stan.rank),]$stan.rank)) /
      nrow(all.ids[all.ids$hyena != id.i & all.ids$mobber == TRUE & !is.na(all.ids$stan.rank),])
    #Proportion of hyenas present ranked higher than focal hyena
    all.ids[all.ids$hyena == id.i,]$present.prop.rank.higher <- 
      length(which(all.ids[all.ids$hyena != id.i & !is.na(all.ids$stan.rank),]$stan.rank > 
                     all.ids[all.ids$hyena == id.i & !is.na(all.ids$stan.rank),]$stan.rank)) /
      nrow(all.ids[all.ids$hyena != id.i & !is.na(all.ids$stan.rank),])
  }
  rm(id.i)
  
  #save in list
  id.by.mob.list[[counter]] <- all.ids
  counter <- counter+1
  rm(session.date)
  rm(all.ids)
}

id.by.mob <- do.call(rbind, id.by.mob.list)
# write.csv(id.by.mob, "id.by.mob.csv")

id.by.mob.save <- id.by.mob
nm.missing     #0

rm(id.by.mob.list)
rm(counter)
rm(nm.missing)


########## 11.5 Clean combined mobbing data ##########

id.by.mob <- id.by.mob.save

#Format columns
#All dataframes
id.by.mob$session <- as.character(gsub(" ", "", id.by.mob$session))
id.by.mob$mobID <- as.character(gsub(" ", "", id.by.mob$mobID))
id.by.mob$hyena <- as.character(gsub(" ", "", id.by.mob$hyena))
id.by.mob$time <- as.POSIXct(id.by.mob$time, format="%Y-%m-%d %H:%M:%S")
id.by.mob$order <- as.numeric(id.by.mob$order)
id.by.mob$mobber <- as.logical(id.by.mob$mobber)
id.by.mob$date <- as.Date(id.by.mob$date, format = "%Y-%m-%d")
id.by.mob$clan <- as.factor(as.character(id.by.mob$clan))
id.by.mob$context <- as.factor(as.character(id.by.mob$context))
id.by.mob$group_size <- as.numeric(id.by.mob$group_size)

#Hyena-level variables
id.by.mob[id.by.mob$sex == "u" & !is.na(id.by.mob$sex), ]$sex <- NA
id.by.mob$sex <- as.factor(as.character(id.by.mob$sex))
id.by.mob$status <- as.factor(as.character(id.by.mob$status))
id.by.mob$tenure <- as.numeric(id.by.mob$tenure)
id.by.mob$age <- as.numeric(id.by.mob$age)
id.by.mob$stan.rank <- as.numeric(id.by.mob$stan.rank)
id.by.mob$repro.state <- as.factor(id.by.mob$repro.state)
id.by.mob$parity <- as.numeric(id.by.mob$parity)
id.by.mob$body.cond <- as.factor(id.by.mob$body.cond)

#Add age.class
id.by.mob$age.class <- as.factor(ifelse(id.by.mob$age >= 2 | id.by.mob$status == "i", "adult", "juvenile"))
unique(filter(id.by.mob, is.na(age.class))$hyena)
id.by.mob[is.na(id.by.mob$age.class) &
            (id.by.mob$hyena == "ao" | id.by.mob$hyena == "sktl" | id.by.mob$hyena == "apl" | 
               id.by.mob$hyena == "bor" | id.by.mob$hyena == "badg" | id.by.mob$hyena == "grim" | 
               id.by.mob$hyena == "lcs" | id.by.mob$hyena == "angi" | id.by.mob$hyena == "ele" | 
               id.by.mob$hyena == "ojy" | id.by.mob$hyena == "pro" | id.by.mob$hyena == "saw" | 
               id.by.mob$hyena == "silk" | id.by.mob$hyena == "slin" | id.by.mob$hyena == "ink" | 
               id.by.mob$hyena == "sau"),]$age.class <- "adult"

#Add social rank as categorical
id.by.mob$rank.cat <- NA
id.by.mob[id.by.mob$stan.rank >= -1 & id.by.mob$stan.rank < -0.3333333 & 
            !is.na(id.by.mob$stan.rank),]$rank.cat <- "low"
id.by.mob[id.by.mob$stan.rank >= -0.3333333 & id.by.mob$stan.rank < 0.3333333 & 
            !is.na(id.by.mob$stan.rank),]$rank.cat <- "medium"
id.by.mob[id.by.mob$stan.rank >= 0.3333333 & id.by.mob$stan.rank <= 1 & 
            !is.na(id.by.mob$stan.rank),]$rank.cat <- "high"
id.by.mob$rank.cat <- as.factor(id.by.mob$rank.cat)

#Assign number of adult females present
id.by.mob$num_fem_present <- NA
id.by.mob$prop_fem_present <- NA
for(i in 1:nrow(id.by.mob)){
  mobid <- id.by.mob$mobID[i]
  af.hyenas.i <- filter(id.by.mob, mobID == mobid & sex == "f" & age.class == "adult")
  if(nrow(af.hyenas.i) > 0){
    id.by.mob$num_fem_present[i] <- nrow(af.hyenas.i)
    id.by.mob$prop_fem_present[i] <- nrow(af.hyenas.i)/id.by.mob$group_size[i]
  }
  else{
    id.by.mob$num_fem_present[i] <- 0
    id.by.mob$prop_fem_present[i] <- 0
  }
}
rm(mobid)
rm(af.hyenas.i)

id.by.mob$fem_present <- NA
for(i in 1:nrow(id.by.mob)){
  if(id.by.mob$num_fem_present[i] == 0 | is.na(id.by.mob$num_fem_present[i])){
    id.by.mob$fem_present[i] <- FALSE
  }
  if(id.by.mob$num_fem_present[i] != 0 & !is.na(id.by.mob$num_fem_present[i])){
    id.by.mob$fem_present[i] <- TRUE
  }
}

#Behavior variables
id.by.mob$grt_occ <- as.logical(id.by.mob$grt_occ)
id.by.mob$grt_occ_mobbers <- as.logical(id.by.mob$grt_occ_mobbers)
id.by.mob$agg_occ <- as.logical(id.by.mob$agg_occ)
id.by.mob$agg_occ_mobbers <- as.logical(id.by.mob$agg_occ_mobbers)

#Group-level variables - convert & check
id.by.mob$prop.mob.related <- as.numeric(id.by.mob$prop.mob.related)
filter(id.by.mob, mobID %in% id.by.mob[is.nan(id.by.mob$prop.mob.related),]$mobID & mobber == T)
#all is.nan are mobs with a single known mobber (n=12)
id.by.mob[is.nan(id.by.mob$prop.mob.related),]$prop.mob.related <- NA

id.by.mob$prop.present.related <- as.numeric(id.by.mob$prop.present.related)
filter(id.by.mob, mobID %in% id.by.mob[is.nan(id.by.mob$prop.present.related),]$mobID)
#all is.nan are mobs with a single known hyena present (n=1)
id.by.mob[is.nan(id.by.mob$prop.present.related),]$prop.present.related <- NA

id.by.mob$mob.ai.avg <- as.numeric(id.by.mob$mob.ai.avg)
filter(id.by.mob, mobID %in% id.by.mob[is.nan(id.by.mob$mob.ai.avg),]$mobID & mobber == T)
#all is.nan are mobs with a single known mobber (n=12)
id.by.mob[is.nan(id.by.mob$mob.ai.avg),]$mob.ai.avg <- NA

id.by.mob$present.ai.avg <- as.numeric(id.by.mob$present.ai.avg)
filter(id.by.mob, mobID %in% id.by.mob[is.nan(id.by.mob$present.ai.avg),]$mobID)
#all is.nan are mobs with a single known hyena present (n=1)
id.by.mob[is.nan(id.by.mob$present.ai.avg),]$present.ai.avg <- NA

id.by.mob$mob.prop.rank.higher <- as.numeric(id.by.mob$mob.prop.rank.higher)
filter(id.by.mob, mobID %in% id.by.mob[is.nan(id.by.mob$mob.prop.rank.higher),]$mobID & mobber == T & !is.na(mob.ai.avg))
id.by.mob[is.nan(id.by.mob$mob.prop.rank.higher),]$mob.prop.rank.higher <- NA
#all is.nan are mobs with a single known mobber or where all other mobbers are of unknown rank  (n=12+22)

id.by.mob$present.prop.rank.higher <- as.numeric(id.by.mob$present.prop.rank.higher)
View(filter(id.by.mob, mobID %in% id.by.mob[is.nan(id.by.mob$present.prop.rank.higher),]$mobID))
id.by.mob[is.nan(id.by.mob$present.prop.rank.higher),]$present.prop.rank.higher <- NA
#all is.nan are talek east hyenas (n=11)

#Change hyenas without a stan.rank to NA not 0
id.by.mob[is.na(id.by.mob$stan.rank),]$mob.prop.rank.higher <- NA
id.by.mob[is.na(id.by.mob$stan.rank),]$present.prop.rank.higher <- NA

id.by.mob <- unique(id.by.mob)
summary(id.by.mob)


########## 11.6 Check NAs - TMM checked/approved all ##########

#Unknown sex - 22 rows, 6 total individuals
unknown.sex <- filter(id.by.mob, is.na(sex))
unknown.sex <- unknown.sex[,c("hyena", "clan", "sex")]
unknown.sex <- unique(unknown.sex)
unknown.sex <- left_join(unknown.sex, tblHyenas, by = c("hyena" = "id"))      #all listed as "u" in tblHyenas

#Unknown tenure - 0 rows
unknown.tenure <- filter(id.by.mob, is.na(tenure) & status == "i")    #0

#Unknown age - 88 rows, 17 total individuals
unknown.age <- filter(id.by.mob, is.na(age) & status != "i")    #don't know immigrant ages generally
unknown.age <- unknown.age[,c("hyena", "clan", "sex", "status", "age")]
unknown.age <- unique(unknown.age)   #all original Serena females except for mth (Talek cub) - okay

#Unknown rank - 82 rows - 11 individual-years not in talek.e
unknown.rank <- filter(id.by.mob, is.na(stan.rank))
unknown.rank$year <- format.Date(unknown.rank$date, format = "%Y")
unknown.rank <- unknown.rank[,c("hyena", "clan", "date", "year", "sex", "age", "status", "stan.rank")]
unknown.rank$clan2 <- NA
for(i in 1:nrow(unknown.rank)){
  unknown.rank$clan2[i] <- filter(tblClanMembership, id == unknown.rank$hyena[i] & 
                                    (unknown.rank$date[i] >= start_date) & 
                                    (unknown.rank$date[i] <= end_date))$clan
}
unknown.rank <- filter(unknown.rank, clan2 != "talek.e")     #don't have talek.e ranks
unknown.rank <- unique(unknown.rank)

#Unknown reproductive state - 7 rows - 2 individuals
unknown.rs <- filter(id.by.mob, is.na(repro.state))     #2 individuals
unknown.rs <- unknown.rs[,c("hyena", "clan", "date", "repro.state")]
unknown.rs <- unique(unknown.rs)

#Save file
save(file = "12.id_by_mob.Rdata", list = c("id.by.mob"))




