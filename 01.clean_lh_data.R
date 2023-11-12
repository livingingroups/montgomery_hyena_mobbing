################################################################################
##### 1.0 Loading and cleaning lion-hyena data #####
################################################################################

########## 1.1 Set working directory & download packages ##########

rm(list = ls())
options(stringsAsFactors = FALSE)
setwd("~/Documents/R/LionHyena")
library(tidyverse)
hyenadata::update_tables("1.2.88")
library(hyenadata)


########## 1.2 Load and clean core demography tables ##########

#tblHyenas
data("tblHyenas")
tblHyenas$mom <- gsub(" ", "", tblHyenas$mom)
tblHyenas$dad <- gsub(" ", "", tblHyenas$dad)
tblHyenas$kaycode <- as.numeric(tblHyenas$kaycode)
tblHyenas$sex <- as.factor(tblHyenas$sex)
tblHyenas$status <- as.factor(tblHyenas$status)
tblHyenas$number_littermates <- as.numeric(tblHyenas$number_littermates)
tblHyenas$litrank <- as.numeric(tblHyenas$litrank)
tblHyenas <- tblHyenas[,c(1,4,7:13,15)].    #drop unnecessary columns
tblHyenas <- filter(tblHyenas, !is.na(id))  
tblHyenas <- unique(tblHyenas)

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
tblSessions$tracked <- as.logical(as.numeric(tblSessions$tracked))
tblSessions$seen <- as.logical(as.numeric(tblSessions$seen))
tblSessions$pickup <- as.logical(as.numeric(tblSessions$pickup))
tblSessions <- tblSessions[,1:12]
tblSessions <- filter(tblSessions, !is.na(session)) 
tblSessions <- unique(tblSessions)

#tblHyenasPerSession
data("tblHyenasPerSession")
tblHyenasPerSession <- tblHyenasPerSession[,1:2]
tblHyenasPerSession <- filter(tblHyenasPerSession, !is.na(session) & !is.na(id))
tblHyenasPerSession <- unique(tblHyenasPerSession)

#tblPredatorsPerSession
data("tblPredatorsPerSession")
tblPredatorsPerSession$predator <- tolower(gsub(" ", "", tblPredatorsPerSession$predator))
tblPredatorsPerSession$predator <- as.factor(tblPredatorsPerSession$predator)
tblPredatorsPerSession$sex <- tolower(gsub(" ", "", tblPredatorsPerSession$sex))
tblPredatorsPerSession$sex <- as.factor(tblPredatorsPerSession$sex)
tblPredatorsPerSession$age <- tolower(gsub(" ", "", tblPredatorsPerSession$age))
tblPredatorsPerSession$age <- as.factor(tblPredatorsPerSession$age)
tblPredatorsPerSession$num_individuals <- as.numeric(tblPredatorsPerSession$num_individuals)
tblPredatorsPerSession <- tblPredatorsPerSession[,1:6]
tblPredatorsPerSession <- filter(tblPredatorsPerSession, !is.na(session) & !is.na(predator))     #remove 6
tblPredatorsPerSession <- unique(tblPredatorsPerSession)     #remove 10


########## 1.3 Add estimated birthdates to tblHyenas ##########

#Add in estimated birthdates (Van Horn et al. 2004) for hyenas without birthdates in tblHyenas
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


########## 1.4 Lion-hyena sessions from backend ##########

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

#Make dataset of lion-hyena sessions based on tblSessions
sessions.lh.be <- filter(tblSessions, clan == "talek" | clan == "kcm" | clan == "pond" |       #filter to study clans
                           clan == "talek.w" | clan == "serena.n" | clan == "serena.s" | 
                           clan == "happy.zebra" | is.na(clan))
sessions.lh.be <- filter(sessions.lh.be, session %in% tblHyenasPerSession$session |     #filter to sessions with study hyenas
                           !is.na(sessions.lh.be$hyenas))
sessions.lh.be <- filter(sessions.lh.be, seen == TRUE)     #filter to visible (not tracked) hyenas
sessions.lh.be <- left_join(sessions.lh.be, tblPredatorsPerSession, by = "session")
sessions.lh.be <- filter(sessions.lh.be, predator == "lion")     #filter to sessions with lions
sessions.lh.be <- filter(sessions.lh.be, date < "2017-01-01")    #filter to sessions prior to end of study (2016)
sessions.lh.be <- unique(sessions.lh.be[,c(1:13,18)])     #1009


########## 1.5 tblLionHyenaSessions ##########

#Read in data from KDSL & TMM data extraction
lh.sessions <- read.csv("00.raw_data/tblLionHyenaSessions.csv", na.strings= c("", " "), 
                        colClasses = 'character')
colnames(lh.sessions)[1] <- "Session"
lh.sessions$Session <- as.character(tolower(gsub(" ", "", lh.sessions$Session)))

#Cleaning data
lh.sessions$hyena_count <- as.numeric(lh.sessions$hyena_count)
lh.sessions$lion_count <- as.numeric(lh.sessions$lion_count)
lh.sessions$note_quality <- as.numeric(lh.sessions$note_quality)
lh.sessions$food_present <- as.logical(lh.sessions$food_present)
lh.sessions$notes <- as.character(lh.sessions$notes)
lh.sessions$entry_complete <- as.logical(lh.sessions$entry_complete)
lh.sessions$session_error_notes <- as.character(lh.sessions$session_error_notes)
lh.sessions$who_checked <- NULL
lh.sessions$date_checked <- NULL
lh.sessions$entered_by <- NULL
lh.sessions$entry_date <- NULL
lh.sessions$who_starts_with_food <- as.factor(lh.sessions$who_starts_with_food)
lh.sessions$who_ends_with_food <- as.factor(lh.sessions$who_ends_with_food)
lh.sessions$do_hyenas_eat <- as.factor(lh.sessions$do_hyenas_eat)
lh.sessions$no_intx_yes_no <- as.logical(lh.sessions$no_intx_yes_no)
summary(lh.sessions)
str(lh.sessions)

#Find errors in session number: sessions in lh.sessions but not in tblSessions
lh.sessions <- left_join(lh.sessions, tblSessions[,1:2], by = c("Session" = "session"))
errors.sessions <- anti_join(lh.sessions, tblSessions, by = c("Session" = "session"))   
# 2 sessions with incorrect session numbers, would be dropped anyways (no vis hyena)
rm(errors.sessions)

#Fix one session with incorrect clan
lh.sessions[lh.sessions$Session == "t60953" & !is.na(lh.sessions$Session),]$clan <- "fig.tree"

#Remove sessions from non-study clans (extracted data from all clans for Lehmann et al. 2017)
lh.sessions <- filter(lh.sessions, clan == "talek" | clan == "kcm" | clan == "pond" | 
                        clan == "talek.w" |  clan == "serena.n" | clan == "serena.s" | 
                        clan == "happy.zebra" | is.na(clan))

#Compare datasets: sessions in sessions.lh.be but not in lh.sessions
missing.sessions1 <- anti_join(sessions.lh.be, lh.sessions, by = c("session" = "Session"))   #1 row - fig.tree session, would be dropped anyways
rm(missing.sessions1)

#Figure out which sessions to drop from this analysis (based on data quality)
lh.sessionstodrop <- read.csv("00.raw_data/lh.sessionstodrop_TMM.csv", na.strings= c("", " "), 
                              colClasses = 'character')
lh.sessionstodrop$Session <- as.character(tolower(gsub(" ", "", lh.sessionstodrop$Session)))
lh.sessionstodrop$DropFromAnalyis <- as.factor(lh.sessionstodrop$DropFromAnalyis)
lh.sessionstodrop <- filter(lh.sessionstodrop, DropFromAnalyis == "YES")
lh.sessions <- filter(lh.sessions, !(Session %in% lh.sessionstodrop$Session))     #1000 left
# TMM checked/approved all on 27-Mar-19, updated 17-Feb-20
rm(lh.sessionstodrop)

#Compare datasets: sessions in lh.sessions but not in sessions.lh.be
missing.sessions2 <- anti_join(lh.sessions, sessions.lh.be, by = c("Session" = "session"))   
missing.sessions2 <- left_join(missing.sessions2, tblSessions, by = c("Session" = "session"))   #0 rows
rm(missing.sessions2)

#Find errors: Problems that must be fixed for all sessions
problems.tblLionHyenaSessions0 <- filter(lh.sessions, is.na(note_quality) | entry_complete == FALSE)    #0 rows
rm(problems.tblLionHyenaSessions0)

#Find errors: Problems that must be fixed for sessions with interactions and note_quality > 1
lh.sessions.good <- filter(lh.sessions, no_intx_yes_no == FALSE & note_quality > 1)   #343 sessions
lh.sessions.good <- left_join(lh.sessions.good, unique(tblPredatorsPerSession[,c(1,7)]), 
                              by = c("Session" = "session"))
problems.tblLionHyenaSessions1 <- filter(lh.sessions.good, is.na(hyena_count) | hyena_count < 1)   #no hyenas  #0 sessions
problems.tblLionHyenaSessions1$TMMnotes <- "hyena_count is NA"
problems.tblLionHyenaSessions2 <- filter(lh.sessions.good, is.na(lion_count) | lion_count < 1)   #no lions  #1 session - okay
problems.tblLionHyenaSessions2$TMMnotes <- "lion_count is NA"
problems.tblLionHyenaSessions <- left_join(lh.sessions.good, tblSessions[,2:6], 
                                           by = c("Session" = "session"))
problems.tblLionHyenaSessions3 <- filter(problems.tblLionHyenaSessions, food_present == TRUE,   #food present is inconsistent with other fields
                                         location != "c", location != "k", 
                                         location != "d", location != "n")
problems.tblLionHyenaSessions3 <- problems.tblLionHyenaSessions3[,1:14]    #0 sessions
problems.tblLionHyenaSessions3$TMMnotes <- "check food_present"
problems.tblLionHyenaSessions4 <- filter(problems.tblLionHyenaSessions, food_present == FALSE, 
                                         location == "c", location == "k")
problems.tblLionHyenaSessions4 <- problems.tblLionHyenaSessions4[,1:14]   #0 sessions
problems.tblLionHyenaSessions4$TMMnotes <- "check food_present"
problems.tblLionHyenaSessions5 <- filter(lh.sessions.good, food_present == FALSE, 
                                         (!is.na(who_starts_with_food) | !is.na(who_ends_with_food) | 
                                            !is.na(do_hyenas_eat)))   #0 sessions
problems.tblLionHyenaSessions5$TMMnotes <- "check food_present and who starts/ends with food"
problems.tblLionHyenaSessions6 <- filter(lh.sessions.good, food_present == TRUE, is.na(do_hyenas_eat))   #0 sessions
problems.tblLionHyenaSessions6$TMMnotes <- "check food present and do hyenas eat"
problems.tblLionHyenaSessions7 <- filter(lh.sessions.good, lion_count != total_lions | is.na(total_lions))     #lion count is inconsistent   #0 sessions
problems.tblLionHyenaSessions7$TMMnotes <- "check lion_count"

problems.tblLionHyenaSessions <- rbind(problems.tblLionHyenaSessions1, problems.tblLionHyenaSessions2, 
                                       problems.tblLionHyenaSessions3, problems.tblLionHyenaSessions4, 
                                       problems.tblLionHyenaSessions5, problems.tblLionHyenaSessions6, 
                                       problems.tblLionHyenaSessions7)
# write.csv(problems.tblLionHyenaSessions, "problems.tblLionHyenaSessions.csv", na = "", row.names = FALSE)   #DONE

# TMM checked/approved all on 17-Apr-19
rm(problems.tblLionHyenaSessions1)
rm(problems.tblLionHyenaSessions2)
rm(problems.tblLionHyenaSessions3)
rm(problems.tblLionHyenaSessions4)
rm(problems.tblLionHyenaSessions5)
rm(problems.tblLionHyenaSessions6)
rm(problems.tblLionHyenaSessions7)
rm(problems.tblLionHyenaSessions)


########## 1.6 tblLionHyenaInteractions ##########

#Read in data
lh.intx <- read.csv("00.raw_data/tblLionHyenaInteractions.csv", na.strings= c("", " "), 
                    colClasses = 'character')
lh.intx$session <- as.character(tolower(gsub(" ", "", lh.intx$session)))

#Cleaning data
lh.intx$id <- as.numeric(lh.intx$id)
lh.intx$time <- as.POSIXct(lh.intx$time, format="%H:%M") 
lh.intx$num_hyenas <- as.numeric(lh.intx$num_hyenas)
lh.intx$num_female_ad_lions <- as.numeric(lh.intx$num_female_ad_lions)
lh.intx$num_male_ad_lions <- as.numeric(lh.intx$num_male_ad_lions)
lh.intx$num_other_lions <- as.numeric(lh.intx$num_other_lions)
lh.intx$num_unknown_lions <- as.numeric(lh.intx$num_unknown_lions)
lh.intx$distance_raw <- lh.intx$distance
lh.intx$distance <- as.numeric(lh.intx$distance)
lh.intx$mobbing <- as.logical(lh.intx$mobbing)
lh.intx$description <- as.character(lh.intx$description)
lh.intx$notes <- as.character(lh.intx$notes)
lh.intx$hyena_list <- as.character(tolower(gsub(" ", "", lh.intx$hyena_list)))
lh.intx$Incomplete_CIs <- as.character(lh.intx$Incomplete_CIs)
lh.intx$num_lions <- rowSums(lh.intx[,5:8], na.rm=TRUE)
str(lh.intx)
summary(lh.intx)

#Drop lines that are blank
lh.intx$keep.row <- apply(lh.intx[,c(3:9, 11:14)],1,function(x)any(!is.na(x)))
lh.intx <- filter(lh.intx, keep.row == TRUE)    #delete 21 rows

#Find errors in session number: Sessions in lh.intx but not in tblSessions
errors.intx <- anti_join(lh.intx, tblSessions, by = "session")  
# 29 sessions - all messed up amboseli sessions, ok to drop
rm(errors.intx)

#Join with tblSessions
lh.intx <- left_join(lh.intx, tblSessions[,1:2], by = "session")

#Only keep interactions from sessions in lh.sessions - check which sessions we're removing
lh.intx.remove.check <- filter(lh.intx, !(session %in% lh.sessions$Session))    #1074 sessions
lh.intx.remove.check <- filter(lh.intx.remove.check, clan == "talek" | clan == "kcm" |     #drop sessions in non-study study clans
                                 clan == "pond" | clan == "talek.w" | clan == "serena.n" | 
                                 clan == "serena.s" | clan == "happy.zebra" | is.na(clan))
lh.intx.remove.check <- filter(lh.intx.remove.check, !grepl("amb", lh.intx.remove.check$session))    #drop amboseli sessions
lh.intx.remove.check <- left_join(lh.intx.remove.check, tblSessions, by = "session")
lh.intx.remove.check <- unique(lh.intx.remove.check[,c(2,19:29)])     #9 sessions
# all sessions with experiments or all unIDed/dead hyenas - ok to drop
rm(lh.intx.remove.check)

#Only keep interactions from sessions in lh.sessions
lh.intx <- filter(lh.intx, session %in% lh.sessions$Session)     #3993 left

#Check no_intx column
lh.nointx.check <- inner_join(filter(lh.sessions, no_intx_yes_no == TRUE), lh.intx,     #sessions with no_intx box checked but which appear in the intx dataset
                              by = c("Session" = "session"))
lh.nointx.check$TMMnotes <- "check if no_intx"
lh.nointx.check <- filter(lh.nointx.check, grepl("lion", lh.nointx.check$description))    #only re-check ones where intx was with lions not other hyenas
lh.nointx.check <- rbind(lh.nointx.check, filter(left_join(lh.sessions, lh.intx,       #add sessions with no_intx checked and with mobbing - 0 rows added
                                                           by = c("Session" = "session")), 
                                                 no_intx_yes_no == TRUE & mobbing == TRUE))
lh.nointx.check <- lh.nointx.check[,c(1,31)]
lh.nointx.check <- unique(lh.nointx.check)   #101 sessions
# write.csv(lh.nointx.check, "lh.nointx.check.csv", na = "", row.names = FALSE)    #DONE
# TMM checked/approved all on 19-Apr-19
rm(lh.nointx.check)

#Check note_quality column
lh.quality.check <- inner_join(filter(lh.intx, mobbing == TRUE & !is.na(hyena_list)),     #check low-quality sessions where mobbing occurred and known hyenas were present
                               filter(lh.sessions, note_quality == 1), by = c("session" = "Session"))
lh.quality.check <- left_join(lh.quality.check, tblSessions, by = "session")
lh.quality.check$TMMnotes <- "check if note_quality == 1"
length(unique(lh.quality.check$session))   #10 sessions
# write.csv(lh.quality.check, "lh.quality.check.csv", na = "", row.names = FALSE)    #DONE
# TMM checked/approved all on 5-Jul-19
rm(lh.quality.check)

#Checking for mobbing - compare descriptions with mobbing check box - look for missed mobs
lh.intx$mob.notes <- NA
lh.intx$mob.notes <- (grepl(pattern = "mob", x = lh.intx$description) | 
                        grepl(pattern = "mob", x = lh.intx$notes))
problems1.tblLionHyenaInteractions <- filter(lh.intx, mob.notes == TRUE & mobbing == FALSE )
lh.intx$mob.notes <- (grepl(pattern = "app", x = lh.intx$description) | 
                        grepl(pattern = "app", x = lh.intx$notes))
problems2.tblLionHyenaInteractions <- filter(lh.intx, mob.notes == TRUE & mobbing == FALSE & 
                                               num_hyenas > 1 & num_lions > 0)
lh.intx$mob.notes <- (grepl(pattern = "rush", x = lh.intx$description) | 
                        grepl(pattern = "rush", x = lh.intx$notes))
problems3.tblLionHyenaInteractions <- filter(lh.intx, mob.notes == TRUE & mobbing == FALSE & 
                                               num_hyenas > 1 & num_lions > 0)
lh.intx$mob.notes <- (grepl(pattern = "circle", x = lh.intx$description) | 
                        grepl(pattern = "circle", x = lh.intx$notes))
problems4.tblLionHyenaInteractions <- filter(lh.intx, mob.notes == TRUE & mobbing == FALSE & 
                                               num_hyenas > 1 & num_lions > 0)
lh.intx$mob.notes <- (grepl(pattern = "surround", x = lh.intx$description) | 
                        grepl(pattern = "surround", x = lh.intx$notes))
problems5.tblLionHyenaInteractions <- filter(lh.intx, mob.notes == TRUE & mobbing == FALSE & 
                                               num_hyenas > 1 & num_lions > 0)
lh.intx$mob.notes <- (grepl(pattern = "coal", x = lh.intx$description) | 
                        grepl(pattern = "coal", x = lh.intx$notes))
problems6.tblLionHyenaInteractions <- filter(lh.intx, mob.notes == TRUE & mobbing == FALSE & 
                                               num_hyenas > 1 & num_lions > 0)
mobbingcheck.tblLionHyenaInteractions <- rbind(problems1.tblLionHyenaInteractions,
                                               problems2.tblLionHyenaInteractions, 
                                               problems3.tblLionHyenaInteractions,
                                               problems4.tblLionHyenaInteractions,
                                               problems5.tblLionHyenaInteractions, 
                                               problems6.tblLionHyenaInteractions)
mobbingcheck.tblLionHyenaInteractions <- rbind(mobbingcheck.tblLionHyenaInteractions,    #add intx entries bw lions & multiple hyenas at close quarters
                                               filter(lh.intx, num_lions > 0 & num_hyenas > 1 & 
                                                        distance < 3 & mobbing == FALSE))
mobbingcheck.tblLionHyenaInteractions <- unique(mobbingcheck.tblLionHyenaInteractions)   #168 rows
mobbingcheck.tblLionHyenaInteractions <- left_join(mobbingcheck.tblLionHyenaInteractions, 
                                                   tblSessions, by = "session")
# write.csv(mobbingcheck.tblLionHyenaInteractions, "mobbingcheck.tblLionHyenaInteractions.csv", na = "", row.names = FALSE)    #DONE

#TMM checked/approved all on 2-Jul-19
rm(problems1.tblLionHyenaInteractions)
rm(problems2.tblLionHyenaInteractions)
rm(problems3.tblLionHyenaInteractions)
rm(problems4.tblLionHyenaInteractions)
rm(problems5.tblLionHyenaInteractions)
rm(problems6.tblLionHyenaInteractions)
rm(mobbingcheck.tblLionHyenaInteractions)

#Checking for NOT mobbing - compare descriptions with mobbing check box - look for things incorrectly marked as mobs
non.mobbingcheck.tblLionHyenaInteractions <- filter(lh.intx, mob.notes == FALSE & mobbing == TRUE)
non.mobbingcheck.tblLionHyenaInteractions$mob.notes <- 
  ((grepl(pattern = "app", x = non.mobbingcheck.tblLionHyenaInteractions$description) | 
      grepl(pattern = "app", x = non.mobbingcheck.tblLionHyenaInteractions$notes)) & 
     (!grepl(pattern = "carc", x = non.mobbingcheck.tblLionHyenaInteractions$description) & 
        !grepl(pattern = "carc", x = non.mobbingcheck.tblLionHyenaInteractions$notes)) & 
     (!grepl(pattern = "kill", x = non.mobbingcheck.tblLionHyenaInteractions$description) & 
        !grepl(pattern = "kill", x = non.mobbingcheck.tblLionHyenaInteractions$notes)))
non.mobbingcheck.tblLionHyenaInteractions <- filter(non.mobbingcheck.tblLionHyenaInteractions, 
                                                    mob.notes == FALSE & mobbing == TRUE)
non.mobbingcheck.tblLionHyenaInteractions <- rbind(non.mobbingcheck.tblLionHyenaInteractions,     #add mobbing entries at large distances
                                                   filter(lh.intx, mobbing == TRUE & 
                                                            !is.na(distance) & distance > 30))
non.mobbingcheck.tblLionHyenaInteractions <- unique(non.mobbingcheck.tblLionHyenaInteractions)   #206 rows
non.mobbingcheck.tblLionHyenaInteractions <- left_join(non.mobbingcheck.tblLionHyenaInteractions, 
                                                       tblSessions, by = "session")
# write.csv(non.mobbingcheck.tblLionHyenaInteractions, "non.mobbingcheck.tblLionHyenaInteractions.csv", na = "", row.names = FALSE)    #DONE

#TMM checked/approved all on 2-Jul-19
rm(non.mobbingcheck.tblLionHyenaInteractions)

#Problems that must be fixed - only check sessions with interactions and note_quality > 1
lh.intx.good <- filter(lh.intx, session %in% lh.sessions.good$Session)     #3339 left
problems1.tblLionHyenaInteractions <- filter(lh.intx.good, mobbing == TRUE, 
                                             (is.na(num_hyenas) & !is.na(hyena_list)) | num_hyenas < 2)    #13 rows
problems1.tblLionHyenaInteractions$TMMnotes <- "num_hyenas < 2 or is NA" 
problems2.tblLionHyenaInteractions <- filter(lh.intx.good, mobbing == TRUE, 
                                             (is.na(num_lions) | num_lions < 1))   #1 row
problems2.tblLionHyenaInteractions$TMMnotes <- "num_lions < 1 or is NA"
problems3.tblLionHyenaInteractions <- filter(lh.intx.good, mobbing == TRUE, is.na(distance))   #72 rows
problems3.tblLionHyenaInteractions$TMMnotes <- "mobbing but distance is NA"
problems4.tblLionHyenaInteractions <- filter(lh.intx, is.na(time) | (is.na(distance) & 
                                                                       !is.na(distance_raw)))  #0  rows
problems4.tblLionHyenaInteractions$TMMnotes <- "time or distance issues"

#Check for typos in hyena names in mobbing sessions
lh1.mobbing <- lh.intx.good[lh.intx.good$mobbing == TRUE,]     
lh1.mobbing <- separate_rows(lh1.mobbing, hyena_list, sep = ',')
lh1.mobbing <- filter(lh1.mobbing, hyena_list != '')
lh1.mobbing <- filter(lh1.mobbing, !is.na(hyena_list))
problems5.tblLionHyenaInteractions <- anti_join(lh1.mobbing, tblHyenas, by = c("hyena_list" = "id"))
problems5.tblLionHyenaInteractions <- filter(lh.intx.good, id %in% problems4.tblLionHyenaInteractions$id)   #0 rows
problems5.tblLionHyenaInteractions$TMMnotes <- "spelling/typo problems in hyena_list"
problems.tblLionHyenaInteractions <- rbind(problems1.tblLionHyenaInteractions, 
                                           problems2.tblLionHyenaInteractions, 
                                           problems3.tblLionHyenaInteractions, 
                                           problems4.tblLionHyenaInteractions, 
                                           problems5.tblLionHyenaInteractions)    #86 rows
# write.csv(problems.tblLionHyenaInteractions, "problems.tblLionHyenaInteractions.csv", na = "", row.names = FALSE)    #DONE

#TMM checked/approved all on 17-Apr-19, updated on 24-Jun-19
rm(problems1.tblLionHyenaInteractions)
rm(problems2.tblLionHyenaInteractions)
rm(problems3.tblLionHyenaInteractions)
rm(problems4.tblLionHyenaInteractions)
rm(problems5.tblLionHyenaInteractions)
rm(lh1.mobbing)
rm(problems.tblLionHyenaInteractions)


##########1.7 tblLionHyenaIndivSessionNotes##########

#Read in data
lh.indvsessions <- read.csv("00.raw_data/tblLionHyenaIntxbyIndivBehaviorSessions.csv", 
                            na.strings= c("", " "), colClasses = 'character') 
lh.indvsessions$Session <- as.character(tolower(gsub(" ", "", lh.indvsessions$Session)))

#Cleaning data
lh.indvsessions$Date.Entered <- NULL
lh.indvsessions$Who.Entered <- NULL
lh.indvsessions$Date.Checked <- NULL
lh.indvsessions$Who.Checked <- NULL
lh.indvsessions$Complete. <- as.logical(lh.indvsessions$Complete.)
lh.indvsessions$Lion.Interest <- as.factor(as.character(lh.indvsessions$Lion.Interest))
lh.indvsessions$Drop.From.Analysis <- as.logical(lh.indvsessions$Drop.From.Analysis)
lh.indvsessions$Distances.only <- as.logical(lh.indvsessions$Distances.only)
lh.indvsessions$Notes <- as.character(lh.indvsessions$Notes)
summary(lh.indvsessions)
str(lh.indvsessions)

#Sessions in lh.sessions but not in lh.indvsessions
additions.tblLionHyenaIntxbyIndivBehaviorSessions <- anti_join(lh.sessions.good, 
                                                               lh.indvsessions, by = "Session")   #0 sessions
additions.tblLionHyenaIntxbyIndivBehaviorSessions$TMMnotes <- "needs to be added to tblLionHyenaIntxbyIndivBehaviorSessions"
additions.tblLionHyenaIntxbyIndivBehaviorSessions <- additions.tblLionHyenaIntxbyIndivBehaviorSessions[,c(1,15)]
# write.csv(additions.tblLionHyenaIntxbyIndivBehaviorSessions, "additions.tblLionHyenaIntxbyIndivBehaviorSessions.csv", na = "", row.names = FALSE)
rm(additions.tblLionHyenaIntxbyIndivBehaviorSessions)

#Only include "good" sessions - sessions with interactions and note_quality > 1
lh.indvsessions.good <- filter(lh.indvsessions, Session %in% lh.sessions.good$Session)     #343 left

#Check Drop.From.Analysis
lh.indvsessionstodrop <- filter(lh.indvsessions.good, Drop.From.Analysis == TRUE)    #23 sessions
# write.csv(lh.indvsessionstodrop, "check.lh.indvsessionstodrop.csv", na = "", row.names = FALSE)
# KDSL checked/approved all on 20-Feb-20

#Filter out drop from analysis sessions
lh.indvsessions.good <- filter(lh.indvsessions.good, !(Session %in% lh.indvsessionstodrop$Session))     #320 left
summary(lh.indvsessions.good)

#Problems to fix
problems1.tblLionHyenaIntxbyIndivBehaviorSessions <- filter(lh.indvsessions.good, 
                                                            Complete. == FALSE)    #0 rows
problems1.tblLionHyenaIntxbyIndivBehaviorSessions$TMMnotes <- "Session is not Complete"
problems2.tblLionHyenaIntxbyIndivBehaviorSessions <- filter(lh.indvsessions.good, 
                                                            Distances.only == TRUE)    #0 rows
problems2.tblLionHyenaIntxbyIndivBehaviorSessions$TMMnotes <- "Distances.only = TRUE (should be FALSE)"
problems3.tblLionHyenaIntxbyIndivBehaviorSessions <- filter(lh.indvsessions.good, 
                                                            Lion.Interest == "Over200m" | 
                                                              is.na(Lion.Interest))    #0 rows
problems3.tblLionHyenaIntxbyIndivBehaviorSessions$TMMnotes <- "Check Lion.Interest"
problems.tblLionHyenaIntxbyIndivBehaviorSessions <- rbind(problems1.tblLionHyenaIntxbyIndivBehaviorSessions,   #0 rows
                                                          problems2.tblLionHyenaIntxbyIndivBehaviorSessions, 
                                                          problems3.tblLionHyenaIntxbyIndivBehaviorSessions)
# write.csv(problems.tblLionHyenaIntxbyIndivBehaviorSessions, "problems.tblLionHyenaIntxbyIndivBehaviorSessions.csv", na = "", row.names = FALSE)     #DONE
rm(problems1.tblLionHyenaIntxbyIndivBehaviorSessions)
rm(problems2.tblLionHyenaIntxbyIndivBehaviorSessions)
rm(problems3.tblLionHyenaIntxbyIndivBehaviorSessions)
rm(problems.tblLionHyenaIntxbyIndivBehaviorSessions)


##########1.8 tblLionHyenaIndivInfo##########

#Read in data
lh.indvinfo <- read.csv("00.raw_data/tblLionHyenaIndvInfonew.csv", na.strings= c("", " "), 
                        colClasses = 'character')
lh.indvinfo$Session <- as.character(tolower(gsub(" ", "", lh.indvinfo$Session)))

#Cleaning data
lh.indvinfo$Hyena <- as.character(tolower(gsub(" ", "", lh.indvinfo$Hyena)))
lh.indvinfo$BodyCondition <- as.factor(as.character(gsub(" ", "", lh.indvinfo$BodyCondition)))
lh.indvinfo$TimeFirstSeen <- as.POSIXct(lh.indvinfo$TimeFirstSeen, format="%H:%M") 
lh.indvinfo$LastSeenOld.Confirmed <- as.POSIXct(lh.indvinfo$LastSeenOld.Confirmed, format="%H:%M") 
lh.indvinfo$TimeLastMention <- as.POSIXct(lh.indvinfo$TimeLastMention, format="%H:%M") 
lh.indvinfo$TimeNextScan <- as.POSIXct(lh.indvinfo$TimeNextScan, format="%H:%M") 
lh.indvinfo$Bloody. <- as.logical(lh.indvinfo$Bloody.)
lh.indvinfo$AddedHyena <- as.character(tolower(gsub(" ", "", lh.indvinfo$AddedHyena)))
summary(lh.indvinfo)
str(lh.indvinfo)

#Combine AddedHyena and Hyena columns (two columns created after issues with data entry form)
for(i in 1:nrow(lh.indvinfo)){
  hyena.i <- lh.indvinfo$Hyena[i]
  add.i <- lh.indvinfo$AddedHyena[i]
  if(is.na(hyena.i) & !is.na(add.i)){
    lh.indvinfo$Hyena[i] <- lh.indvinfo$AddedHyena[i]
  }
}

#Drop lines that are blank
lh.indvinfo$keep.row <- apply(lh.indvinfo[,c(3:7)],1,function(x)any(!is.na(x)))
lh.indvinfo <- filter(lh.indvinfo, keep.row == TRUE)     #remove 88 rows

#Only include "good" sessions - sessions with interactions and note_quality > 1
lh.indvinfo.good <- filter(lh.indvinfo, Session %in% lh.indvsessions.good$Session)     #4507 left
summary(lh.indvinfo.good)

#Compare tblHyenasPerSession and lh.indvinfo
tblHyenasPerSession.good <- filter(tblHyenasPerSession, session %in% lh.sessions.good$Session)
tblHyenasPerSession.good <- filter(tblHyenasPerSession.good, !(session %in% lh.indvsessionstodrop$Session))
additions.tblLionHyenaIndvInfo <- anti_join(tblHyenasPerSession.good, lh.indvinfo.good,      #hyenas in tblHyenasPerSession but not lh.indvinfo
                                            by = c("session" = "Session", "id" = "Hyena"))   #0 rows
colnames(additions.tblLionHyenaIndvInfo)[1] <- "Session"
colnames(additions.tblLionHyenaIndvInfo)[2] <- "Hyena"
additions.tblLionHyenaIndvInfo$TMMnotes <- "hyena in tblHyenasPerSession but not in tblLionHyenaIndvInfo"
subtractions.tblLionHyenaIndvInfo <- anti_join(lh.indvinfo.good, tblHyenasPerSession.good,      #hyenas in lh.indvinfo but not tblHyenasPerSession
                                               by = c("Session" = "session", "Hyena" = "id"))   #0 rows
subtractions.tblLionHyenaIndvInfo <- subtractions.tblLionHyenaIndvInfo[,c(1:2)]
subtractions.tblLionHyenaIndvInfo$TMMnotes <- "hyena in tblLionHyenaIndvInfo but not in tblHyenasPerSession"
check.hyenas.tblLionHyenaIndvInfo <- rbind(additions.tblLionHyenaIndvInfo, subtractions.tblLionHyenaIndvInfo)
check.hyenas.tblLionHyenaIndvInfo <- filter(check.hyenas.tblLionHyenaIndvInfo, 
                                            !is.na(Session) & !is.na(Hyena))   #0 rows
# write.csv(check.hyenas.tblLionHyenaIndvInfo, "check.hyenas.tblLionHyenaIndvInfo.csv", na = "", row.names = FALSE)
rm(additions.tblLionHyenaIndvInfo)
rm(subtractions.tblLionHyenaIndvInfo)
rm(check.hyenas.tblLionHyenaIndvInfo)

#Problems to fix
problems1.tblLionHyenaIndvInfonew <- filter(lh.indvinfo.good, is.na(BodyCondition))     #0 rows
problems1.tblLionHyenaIndvInfonew$TMMnotes <- "BodyCondition is NA"
problems2.tblLionHyenaIndvInfonew <- filter(lh.indvinfo.good, is.na(TimeFirstSeen))     #0 rows
problems2.tblLionHyenaIndvInfonew$TMMnotes <- "TimeFirstSeen is NA"
problems3.tblLionHyenaIndvInfonew <- filter(lh.indvinfo.good, is.na(LastSeenOld.Confirmed) & 
                                              is.na(TimeLastMention) & is.na(TimeNextScan))     #0 rows
problems3.tblLionHyenaIndvInfonew$TMMnotes <- "There is no TimeLastSeen"
problems4.tblLionHyenaIndvInfonew <- filter(lh.indvinfo.good, !is.na(TimeLastMention) & 
                                              is.na(TimeNextScan))     #0 rows
problems4.tblLionHyenaIndvInfonew$TMMnotes <- "There is no TimeNextScan"
problems5.tblLionHyenaIndvInfonew <- filter(lh.indvinfo.good, is.na(TimeLastMention) & 
                                              !is.na(TimeNextScan))     #0 rows
problems5.tblLionHyenaIndvInfonew$TMMnotes <- "There is no TimeLastMention"
problems6.tblLionHyenaIndvInfonew <- filter(lh.indvinfo.good, TimeFirstSeen == "1899-12-30 00:00:00" & 
                                              LastSeenOld.Confirmed != "1899-12-30 00:00:00")     #0 rows
problems6.tblLionHyenaIndvInfonew$TMMnotes <- "TimeFirstSeen is 00:00 but LastSeenOld.Confirmed is not"
problems7.tblLionHyenaIndvInfonew <- filter(lh.indvinfo.good, TimeFirstSeen != "1899-12-30 00:00:00" & 
                                              LastSeenOld.Confirmed == "1899-12-30 00:00:00")     #0 rows
problems7.tblLionHyenaIndvInfonew$TMMnotes <- "LastSeenOld.Confirmed is 00:00 but TimeFirstSeen is not"

problems.tblLionHyenaIndvInfo <- rbind(problems1.tblLionHyenaIndvInfonew, 
                                       problems2.tblLionHyenaIndvInfonew, problems3.tblLionHyenaIndvInfonew, 
                                       problems4.tblLionHyenaIndvInfonew, problems5.tblLionHyenaIndvInfonew, 
                                       problems6.tblLionHyenaIndvInfonew, problems7.tblLionHyenaIndvInfonew)
# for(s in unique(problems.tblLionHyenaIndvInfo$Session)){
#   for(i in unique(filter(problems.tblLionHyenaIndvInfo, Session ==s)$Hyena)){
#     problems.tblLionHyenaIndvInfo[problems.tblLionHyenaIndvInfo$Session == s & 
#                                     problems.tblLionHyenaIndvInfo$Hyena == i,]$TMMnotes <- 
#       filter(problems.tblLionHyenaIndvInfo, Session == s & Hyena == i) %>% 
#       pull(TMMnotes) %>%
#       paste(collapse = '; ')
#   }
# }
problems.tblLionHyenaIndvInfo <- unique(problems.tblLionHyenaIndvInfo)   #0 rows
# write.csv(problems.tblLionHyenaIndvInfo, "problems.tblLionHyenaIndvInfo.csv", na = "", row.names = FALSE)
rm(problems1.tblLionHyenaIndvInfonew)
rm(problems2.tblLionHyenaIndvInfonew)
rm(problems3.tblLionHyenaIndvInfonew)
rm(problems4.tblLionHyenaIndvInfonew)
rm(problems5.tblLionHyenaIndvInfonew)
rm(problems6.tblLionHyenaIndvInfonew)
rm(problems7.tblLionHyenaIndvInfonew)
rm(problems.tblLionHyenaIndvInfo)

#Combine TimeLastMention & TimeNextScan into LastSeen
for(i in 1:nrow(lh.indvinfo.good)){
  if(!is.na(lh.indvinfo.good$TimeLastMention[i]) & !is.na(lh.indvinfo.good$TimeNextScan[i])){
    lh.indvinfo.good$LastSeenOld.Confirmed[i] <- mean(c(lh.indvinfo.good$TimeLastMention[i], 
                                                        lh.indvinfo.good$TimeNextScan[i]))
  }
}
summary(lh.indvinfo.good)   #no NAs in LastSeenOld.Confirmed


##########1.9 tblLionHyenaIndivBehavior##########

#Read in data
lh.indvbehav <- read.csv("00.raw_data/tblLionHyenaIndividualBehaviors.csv", na.strings= c("", " "), 
                         colClasses = 'character')
lh.indvbehav$Session <- as.character(tolower(gsub(" ", "", lh.indvbehav$Session)))

#Cleaning data
lh.indvbehav$ID <- as.numeric(lh.indvbehav$ID)
lh.indvbehav$Hyena <- as.character(tolower(gsub(" ", "", lh.indvbehav$Hyena)))
lh.indvbehav$Time <- as.POSIXct(lh.indvbehav$Time, format="%H:%M") 
lh.indvbehav$Order <- as.numeric(lh.indvbehav$Order)
lh.indvbehav$Behavior <- as.character(lh.indvbehav$Behavior)
lh.indvbehav$Target <- as.character(lh.indvbehav$Target)
lh.indvbehav$distance_raw <- as.character(lh.indvbehav$Distance)
lh.indvbehav$Distance <- gsub("<", "", lh.indvbehav$Distance)
lh.indvbehav$Distance <- gsub(">", "", lh.indvbehav$Distance)
lh.indvbehav$Distance <- gsub("\\+", "", lh.indvbehav$Distance)
lh.indvbehav$Distance <- gsub("\\?", "", lh.indvbehav$Distance)
lh.indvbehav$Distance <- strsplit(lh.indvbehav$Distance, "-") %>% 
  lapply(FUN = as.numeric) %>%
  sapply(FUN = mean)
lh.indvbehav$Distance <- as.numeric(lh.indvbehav$Distance)
lh.indvbehav$Leader.Y.N. <- as.logical(lh.indvbehav$Leader.Y.N.)
lh.indvbehav$Food <- as.character(lh.indvbehav$Food)
lh.indvbehav$Notes <- as.character(lh.indvbehav$Notes)
lh.indvbehav$IDold <- NULL
summary(lh.indvbehav)
str(lh.indvbehav)

#Drop lines that are blank
lh.indvbehav$keep.row <- apply(lh.indvbehav[,c(3:8,10:12)],1,function(x)any(!is.na(x)))
lh.indvbehav <- filter(lh.indvbehav, keep.row == TRUE)  #remove 51 rows

#Only include "good" sessions - sessions with interactions and note_quality > 1
lh.indvbehav.good <- filter(lh.indvbehav, Session %in% lh.indvsessions.good$Session)     
summary(lh.indvbehav.good)

#Check that hyenas are in tblHyenasPerSession
lh.indvbehav.good$hyena.check <- NA
for(i in 1:nrow(lh.indvbehav.good)){
  session.i <- lh.indvbehav.good$Session[i]
  hyena.i <- lh.indvbehav.good$Hyena[i]
  tblHyenasPerSession.i <- filter(tblHyenasPerSession.good, session == session.i)
  if(hyena.i %in% tblHyenasPerSession.i$id){
    lh.indvbehav.good$hyena.check[i] <- TRUE
  }
  if(!(hyena.i %in% tblHyenasPerSession.i$id)){
    lh.indvbehav.good$hyena.check[i] <- FALSE
  }
}
rm(tblHyenasPerSession.i)
nrow(filter(lh.indvbehav.good, hyena.check == FALSE & !grepl("unid", lh.indvbehav.good$Hyena) &    #Filter out unIDs and lions
              !grepl("lion", lh.indvbehav.good$Hyena)))    #0 rows - all hyenas in tblHyenasPerSession

#Check that time is within session times from tblSessions (that all behaviors occur within the session times)
lh.indvbehav.good$time.check <- NA
for(i in 1:nrow(lh.indvbehav.good)){
  session.i <- lh.indvbehav.good$Session[i]
  time.i <- lh.indvbehav.good$Time[i]
  start.i <- filter(tblSessions, session == session.i)$start
  stop.i <- filter(tblSessions, session == session.i)$stop
  if(!is.na(time.i) & !is.na(start.i) & !is.na(stop.i)){
    if((time.i >= start.i) & (time.i <= stop.i)){
      lh.indvbehav.good$time.check[i] <- TRUE
    }
    if(!((time.i >= start.i) & (time.i <= stop.i))){
      lh.indvbehav.good$time.check[i] <- FALSE
    }
  }
}

#Check that time is within times from indvinfo (that all hyenas are behaving during the time they're present at the session)
lh.indvbehav.good$time.check2 <- NA
for(i in 1:nrow(lh.indvbehav.good)){
  session.i <- lh.indvbehav.good$Session[i]
  id.i <- lh.indvbehav.good$Hyena[i]
  if(id.i %in% tblHyenas$id & !grepl("unid", id.i)){
    if(id.i %in% filter(lh.indvinfo.good, Session == session.i)$Hyena){
      time.i <- lh.indvbehav.good$Time[i]
      start.i <- filter(lh.indvinfo.good, Session == session.i & Hyena == id.i)$TimeFirstSeen
      stop.i <- filter(lh.indvinfo.good, Session == session.i & Hyena == id.i)$LastSeenOld.Confirmed
      if(!is.na(time.i) & !is.na(start.i) & !is.na(stop.i)){
        if((time.i >= start.i) & (time.i <= stop.i)){
          lh.indvbehav.good$time.check2[i] <- TRUE
        }
        if(!((time.i >= start.i) & (time.i <= stop.i))){
          lh.indvbehav.good$time.check2[i] <- FALSE
        }
      }
    }
  }
}
summary(lh.indvbehav.good)

#Check blanks in time.check2
nrow(filter(lh.indvbehav.good, is.na(time.check2) & !grepl("unid", lh.indvbehav.good$Hyena) &    #Filter out unIDs and lions
              !grepl("lion", lh.indvbehav.good$Hyena)))      #0 rows - all hyenas within their time present at the session

#Problems to fix
problems1.tblLionHyenaIndvBehav <- filter(lh.indvbehav.good, is.na(Session))    #0 rows
problems1.tblLionHyenaIndvBehav$TMMnotes <- "Session is NA"
problems2.tblLionHyenaIndvBehav <- filter(lh.indvbehav.good, is.na(Hyena))     #0 rows
problems2.tblLionHyenaIndvBehav$TMMnotes <- "Hyena is NA"
problems3.tblLionHyenaIndvBehav <- filter(lh.indvbehav.good, hyena.check == FALSE)   #0 rows
problems3.tblLionHyenaIndvBehav <- filter(problems3.tblLionHyenaIndvBehav, 
                                          !grepl("unid", problems3.tblLionHyenaIndvBehav$Hyena))   
problems3.tblLionHyenaIndvBehav <- filter(problems3.tblLionHyenaIndvBehav, 
                                          !grepl("lion", problems3.tblLionHyenaIndvBehav$Hyena))   #0 rows
problems3.tblLionHyenaIndvBehav$TMMnotes <- "Hyena not in tblHyenasPerSession"
problems4.tblLionHyenaIndvBehav <- filter(lh.indvbehav.good, is.na(Time))     #0 rows
problems4.tblLionHyenaIndvBehav$TMMnotes <- "Time is NA"
problems5.tblLionHyenaIndvBehav <- filter(lh.indvbehav.good, time.check == FALSE)   #0 rows
problems5.tblLionHyenaIndvBehav$TMMnotes <- "Time is not within session start/stop times"
problems6.tblLionHyenaIndvBehav <- filter(lh.indvbehav.good, time.check2 == FALSE)   #0 rows
problems6.tblLionHyenaIndvBehav$TMMnotes <- "Time is not within hyena first/last seen times"
problems7.tblLionHyenaIndvBehav <- filter(lh.indvbehav.good, is.na(Order))     #0 rows
problems7.tblLionHyenaIndvBehav$TMMnotes <- "Order is NA"
problems8.tblLionHyenaIndvBehav <- filter(lh.indvbehav.good, is.na(Behavior))    #0 rows
problems8.tblLionHyenaIndvBehav$TMMnotes <- "Behavior is NA"
problems9.tblLionHyenaIndvBehav <- filter(lh.indvbehav.good, is.na(Distance) & !is.na(distance_raw))     #0 rows
problems9.tblLionHyenaIndvBehav$TMMnotes <- "Distance is messed up"
problems.tblLionHyenaIndvBehav <- rbind(problems1.tblLionHyenaIndvBehav, problems2.tblLionHyenaIndvBehav, 
                                        problems3.tblLionHyenaIndvBehav, problems4.tblLionHyenaIndvBehav, 
                                        problems5.tblLionHyenaIndvBehav, problems6.tblLionHyenaIndvBehav, 
                                        problems7.tblLionHyenaIndvBehav, problems8.tblLionHyenaIndvBehav, 
                                        problems9.tblLionHyenaIndvBehav)
problems.tblLionHyenaIndvBehav <- unique(problems.tblLionHyenaIndvBehav)     #0
# write.csv(problems.tblLionHyenaIndvBehav, "problems.tblLionHyenaIndvBehav.csv", na = "", row.names = FALSE)
rm(problems1.tblLionHyenaIndvBehav)
rm(problems2.tblLionHyenaIndvBehav)
rm(problems3.tblLionHyenaIndvBehav)
rm(problems4.tblLionHyenaIndvBehav)
rm(problems5.tblLionHyenaIndvBehav)
rm(problems6.tblLionHyenaIndvBehav)
rm(problems7.tblLionHyenaIndvBehav)
rm(problems8.tblLionHyenaIndvBehav)
rm(problems9.tblLionHyenaIndvBehav)
rm(problems.tblLionHyenaIndvBehav)


##########1.10 Lion-Hyena Datasets - final##########

#Remove sessions in lh.sessions and lh.intx that aren't in lh.indvsessions
lh.sessions.checked <- lh.sessions.good
lh.intx.checked <- lh.intx.good
lh.sessions.good <- filter(lh.sessions.good, Session %in% lh.indvsessions.good$Session)    #remove 23 lines
lh.intx.good <- filter(lh.intx.good, session %in% lh.indvsessions.good$Session)     #remove 84 lines

#Check that all sessions are in all databases
anti_join(lh.sessions.good, lh.intx.good, by = c("Session" = "session"))    #t9162 - okay, nothing in lh.intx for this session
anti_join(lh.sessions.good, lh.indvsessions.good, by = "Session")     
anti_join(lh.sessions.good, lh.indvinfo.good, by = "Session")     
anti_join(lh.sessions.good, lh.indvbehav.good, by = "Session")
anti_join(lh.intx.good, lh.sessions.good, by = c("session" = "Session"))
anti_join(lh.indvsessions.good, lh.sessions.good, by = "Session")    
anti_join(lh.indvinfo.good, lh.sessions.good, by = "Session")  
anti_join(lh.indvbehav.good, lh.sessions.good, by = "Session")    

#Clean workspace
lh.indvbehav <- lh.indvbehav.good[,1:11]
lh.indvinfo <- lh.indvinfo.good[,1:8]
lh.indvsessions <- lh.indvsessions.good
lh.intx.all <- lh.intx[,c(1:14,16)]
lh.intx.checked <- lh.intx.checked[,c(1:14,16)]
lh.intx <- lh.intx.good[,c(1:14,16)]
lh.sessions.all <- lh.sessions[,1:13]
lh.sessions.checked <- lh.sessions.checked[,1:13]
lh.sessions <- lh.sessions.good[,1:13]

save(file = "02.cleaned_lh_data.Rdata", list = c("lh.sessions", "lh.sessions.checked","lh.sessions.all", 
                                                 "lh.intx", "lh.intx.checked","lh.intx.all", 
                                                 "lh.indvsessions", "lh.indvinfo", "lh.indvbehav"))


