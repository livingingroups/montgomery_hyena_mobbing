################################################################################
##### 3.0 Combining mobbing from session and individual data extractions #####
################################################################################

##### Start code by Kenna D.S. Lehmann #####

# This code organizes and prepares LH lh.intx Interaction and lh.indvbehav Individual Behavior datasets.
# It then creates a unique mobID code for each mob that matches between the two datasets.
# This mobID prevents miscounting of mobs by connecting the two datasets.
# This code is for checking and coding purposes.

#### Contents ####
# 1.0 - Pull Data 
#   1.1 - Set working directory 
#   1.2 - Load data tables 
# 2.0 - Prepare data for creating mobID 
#   2.1 - Deal with approaches in lh.indvbehav dataset.
#   2.2 - Prepare data for creating mobID
# 3.0 - Cycle through lh.intx and find lh.indvbehav matches - assign mobID code #
#   3.1 - Assign mobID to lv.1
#   3.2 - Assign mobID to lh.indvbehav

#### 1.0 - Pull Data ####
#### Environment prep
rm(list = ls())
options(stringsAsFactors = FALSE)
set.seed(4386)

## load libraries
library(tidyverse)
hyenadata::update_tables("1.2.88")
library(hyenadata)

#     1.1 Set working directory
setwd("~/Documents/R/LionHyena")
#     1.2 Load data tables
load("02.cleaned_lh_data.Rdata")
data("tblSessions")
rm(lh.indvinfo)
rm(lh.indvsessions)
rm(lh.intx.all)
rm(lh.intx.checked)
rm(lh.sessions.all)
rm(lh.sessions.checked)

#### 2.0 - Clean and simplify datasets ####
# 2.1 - Narrow down to mobs. 
# For lh.intx this is narrowed down to mobs within note quality 2 and 3 sessions and anytime mobbing is checked as true
lh.intx$session <- as.character(lh.intx$session)
lh.intx <- left_join(lh.intx, lh.sessions, by=c("session" = "Session"))
lh.intx <- subset(lh.intx, mobbing==TRUE)
# For lh.indvbehav this is any line that has mob or app anywhere in the behavior column.
lh.indvbehav <- subset(lh.indvbehav, grepl("mob|app", lh.indvbehav$Behavior))

# First, check notes column in lh.intx for bonus hyenas (when hyena_list was too long for the data entry form)
length(unique(lh.intx$session))
# write.csv(left_join(lh.intx, tblSessions, by = "session"), "lh.intx.mob.csv", na = "", row.names = FALSE)
# Notes columns checked by hand by TMM and KDSL
lh.intx[lh.intx$id == 1106, ]$hyena_list <- paste(lh.intx[lh.intx$id == 1106, ]$hyena_list, "nod", sep = ",")
lh.intx[lh.intx$id == 2132, ]$hyena_list <- paste(lh.intx[lh.intx$id == 2132, ]$hyena_list, "sy,bag,agp,bsh,fn,ger", sep = ",")
lh.intx[lh.intx$id == 3321, ]$hyena_list <- paste(lh.intx[lh.intx$id == 3321, ]$hyena_list, "pant,alfe,tavi,styr,khtm", sep = ",")
lh.intx[lh.intx$id == 1002203, ]$hyena_list <- paste(lh.intx[lh.intx$id == 1002203, ]$hyena_list, "arro,strm,hucn", sep = ",")
lh.intx[lh.intx$id == 4000135, ]$hyena_list <- paste(lh.intx[lh.intx$id == 4000135, ]$hyena_list, "pan,amaz,juno,mgta,cen,dion,suk", sep = ",")
lh.intx[lh.intx$id == 4000151, ]$hyena_list <- paste(lh.intx[lh.intx$id == 4000151, ]$hyena_list, "suk", sep = ",")
lh.intx[lh.intx$id == 4000152, ]$hyena_list <- paste(lh.intx[lh.intx$id == 4000152, ]$hyena_list, "midd", sep = ",")

# This can be edited to keep in any columns that you need/want. 
lh.intx <- dplyr::select(lh.intx, id, session, time, hyena_list, num_hyenas, distance, description, notes.x)
lh.indvbehav <- dplyr::select(lh.indvbehav, ID, Session, Hyena, Time, Order, Behavior, Leader.Y.N., Target, Distance)


####  2.1 - Deal with approaches in lh.indvbehav dataset. ####
# Get rid of approaches of only one hyena
# Approaches of 2+ hyenas with the same time and order get renamed as mobs
lh.indvbehav$mob <- NA
for (i in 1:length(lh.indvbehav$ID)) {
  if (grepl("mob", lh.indvbehav$Behavior[i])==TRUE & 
      !grepl("Fig Tree hyenas", lh.indvbehav$Target[i]) & 
      !grepl("east hyenas", lh.indvbehav$Target[i])) {
    lh.indvbehav$mob[i]<- "mob"
  } else if (lh.indvbehav$Hyena[i]=="UNIDs") {
    lh.indvbehav$mob[i] <- "mob"
  } else
    for (j in 1:length(lh.indvbehav$ID)) {
      if (i==j) {
        next
      } else if (lh.indvbehav$Session[i]==lh.indvbehav$Session[j] & 
                 lh.indvbehav$Time[i]==lh.indvbehav$Time[j] &
                 lh.indvbehav$Order[i]==lh.indvbehav$Order[j] & 
                 !grepl("Fig Tree hyenas", lh.indvbehav$Target[i]) & 
                 !grepl("east hyenas", lh.indvbehav$Target[i])) 
      {
        lh.indvbehav$mob[i] <- "mob"
      } else 
        next
    }
}

# Get rid of non-mobs
lh.indvbehav <- subset(lh.indvbehav, grepl("mob", lh.indvbehav$mob))


####  2.2 - Prepare data for creating mobID ####
# Add mobID column to each datset
lh.indvbehav$mobID <- NA
lh.intx$mobID <- NA
lh.indvbehav$counted <- NA

# Make sure the time columns are the same structure
lh.intx$time <- as.character(lh.intx$time)
lh.indvbehav$Time <- as.character(lh.indvbehav$Time)

# Fix hyena list in lh.intx so that the commas are correct and standardized.
lh.intx$hyena_list <- as.character(lh.intx$hyena_list)

for (i in 1:(length(lh.intx$session))) {
  if (is.na(lh.intx$hyena_list[i])==TRUE) {
    next
  } else {
    if (str_sub(lh.intx$hyena_list[i], 1,1)!=',') {
      lh.intx$hyena_list[i] <- paste0(',',lh.intx$hyena_list[i])
    }
    if (str_sub(lh.intx$hyena_list[i], -1,-1)!=',') {
      lh.intx$hyena_list[i] <- paste0(lh.intx$hyena_list[i],',')
    }  
  }
}


#### 3.0 - Cycle through lh.intx and find lh.indvbehav matches - assign mobID code ####

#     3.1 - Assign mobID to lv.1
# This assigns a mobID to the lh.intx notes that have a mob in the same session at the same time
# as the lh.indvbehav dataset. If session and time do not match between the datasets, mobID is left blank
# in the lh.intx dataset.
for (i in 1:(length(lh.intx$id))){
  for (j in 1:(length(lh.indvbehav$ID))) {
    if (lh.intx$session[i]==lh.indvbehav$Session[j] & lh.intx$time[i]==lh.indvbehav$Time[j]) {
      lh.intx$mobID[i] <- i
      next
    }
  }
}

# If there is no mob at a matching time or session in indvbehv, say so
lh.intx$mobID[is.na(lh.intx$mobID)==TRUE] <- "There are no mobs at this session and time in indvbehav"

countsessions <- subset(lh.intx, mobID=="There are no mobs at this session and time in indvbehav")


#     3.2 - Assign mobID to lh.indvbehav
# Create standardized order number from the intx dataset's Notes column.
lh.intx$order <- NA

for (i in 1:length(lh.intx$id)) {
  if (grepl('1st', lh.intx$notes.x[i], ignore.case=TRUE)==TRUE) {
    lh.intx$order[i] <- 1
  } else if (grepl('2nd', lh.intx$notes.x[i], ignore.case=TRUE)==TRUE) {
    lh.intx$order[i] <- 2
  } else if (grepl('3rd', lh.intx$notes.x[i], ignore.case=TRUE)==TRUE) {
    lh.intx$order[i] <- 3
  } else if (grepl('4th', lh.intx$notes.x[i], ignore.case=TRUE)==TRUE) {
    lh.intx$order[i] <- 4
  } else if (grepl('5th', lh.intx$notes.x[i], ignore.case=TRUE)==TRUE) {
    lh.intx$order[i] <- 5
  } else if (grepl('6th', lh.intx$notes.x[i], ignore.case=TRUE)==TRUE) {
    lh.intx$order[i] <- 6
  } else if (grepl('7th', lh.intx$notes.x[i], ignore.case=TRUE)==TRUE) {
    lh.intx$order[i] <- 7
  } else if (grepl('8th', lh.intx$notes.x[i], ignore.case=TRUE)==TRUE) {
    lh.intx$order[i] <- 8
  } else if (grepl('9th', lh.intx$notes.x[i], ignore.case=TRUE)==TRUE) {
    lh.intx$order[i] <- 9
  } else if (grepl('1 \\(order\\)', lh.intx$notes.x[i], ignore.case=TRUE)==TRUE) {
    lh.intx$order[i] <- 1
  } else if (grepl('2 \\(order\\)', lh.intx$notes.x[i], ignore.case=TRUE)==TRUE) {
    lh.intx$order[i] <- 2
  } else if (grepl('3 \\(order\\)', lh.intx$notes.x[i], ignore.case=TRUE)==TRUE) {
    lh.intx$order[i] <- 3
  } else if (grepl('4 \\(order\\)', lh.intx$notes.x[i], ignore.case=TRUE)==TRUE) {
    lh.intx$order[i] <- 4
  }
}

# create meaningful columns
lh.indvbehav$mobID <- NA
lh.intx$used <- NA
lh.indvbehav$counted <- NA
lh.intx$chckmin <- NA

# order the dataset
lh.intx <- lh.intx[with(lh.intx, order(session, time, order)), ]
lh.indvbehav <- lh.indvbehav[with(lh.indvbehav, order(Session, Time, Order)),]

# loop through each mob of intx dataset
for (i in 1:length(lh.intx$session)) {
  # reduce lh.intx list to the same session and time that hasn't been used yet.
  lh.intx.tmplist <- subset(lh.intx, session==lh.intx$session[i] & time==lh.intx$time[i] & is.na(used)==TRUE)
  # if there is more than one mob in this session, at this time then...
  if (length(lh.intx.tmplist$session)>1) {
    # ...find the earliest order and save it as a variable
    lh.intx.Order <- min(lh.intx.tmplist[,"order"])
    # find the indvbehav lines that match session, time, and hasn't been used yet
    lh.indvbehav.tmplist <- subset(lh.indvbehav, Session==lh.intx$session[i] & Time==lh.intx$time[i] & is.na(counted)==TRUE)
    # find the earliest order and save it as a variable
    lh.indvbehav.Order <- min(lh.indvbehav.tmplist[,"Order"])
    # if there are no indvbehav entries left...
    if (lh.indvbehav.Order==Inf) {
      # ...make a note to check it
      lh.intx$chckmin[i] <- "fucucked, missing from indvbehav"
    }
    # if the intx order is empty, skip to next loop of i
    if (is.na(lh.intx.Order)) {
      next
    }
    # loop through each line of indvbehav dataset
    for (j in 1:length(lh.indvbehav$Session)) {
      # if session and time are the same, AND the hyena in indvbehav matches a hyena in intx's hyena_list
      # AND it hasn't been counted yet, and it is the lowest order in these loops ...
      if ( lh.indvbehav$Session[j]==lh.intx$session[i] &
           lh.indvbehav$Time[j]==lh.intx$time[i] &
           is.na(lh.indvbehav$counted[j]) &
           lh.intx$order[i]==lh.intx.Order &
           lh.indvbehav$Order[j]==lh.indvbehav.Order &
           ( (grepl(lh.indvbehav$Hyena[j], lh.intx$hyena_list[i], ignore.case=TRUE)) | 
             (grepl(lh.indvbehav$Hyena[j], lh.intx$notes.x[i], ignore.case=TRUE)) |
             ( (grepl("unids|unid", lh.indvbehav$Hyena[j], ignore.case=TRUE)) & 
               ( is.na(lh.intx$hyena_list[i]) |
                 is.na(lh.intx$num_hyenas[i]) |
                 (str_count(lh.intx$hyena_list[i], ',')-1)<lh.intx$num_hyenas[i]) ) ) 
      ) {
        # ...assign the intx mobID to the indvbehav mobID
        lh.indvbehav$mobID[j] <- lh.intx$mobID[i]
        # show that mob was counted in the indvbehav dataset
        lh.indvbehav$counted[j] <- 'X'
        # and that it was used in the intx dataset
        lh.intx$used[i] <- 'X'
      }
    }
    # If there is only one mob at this session, at this time, then...
  } else {
    # loop through indvbehav
    for (j in 1:length(lh.indvbehav$Session)) {
      # If the session and time match, and the hyena in indvbehav is found in intx hyena_list AND it hasn't been counted yet
      if (lh.indvbehav$Session[j]==lh.intx$session[i] &
          lh.indvbehav$Time[j]==lh.intx$time[i] &
          is.na(lh.indvbehav$counted[j]==TRUE) &
          ((grepl(lh.indvbehav$Hyena[j], lh.intx$notes.x[i], ignore.case=TRUE)) |
           (grepl(lh.indvbehav$Hyena[j], lh.intx$hyena_list[i], ignore.case=TRUE))) ) {
        # assign the mob ID from intx
        lh.indvbehav$mobID[j] <- lh.intx$mobID[i] 
        # show that it's been counted in indvbehav
        lh.indvbehav$counted[j] <- 'X'
        # and used in intx
        lh.intx$used[i] <- 'X'
        # If session,time, hyena dont' match because the hyena is "unid" or "unids" in indvbehav dataset, 
        # with some extra checks like hyena list is unknown, number of hyenas in unknown, 
        # or the hyena list is smaller than the number of hyenas...
      } else if  (lh.indvbehav$Session[j]==lh.intx$session[i] &
                  lh.indvbehav$Time[j]==lh.intx$time[i] &
                  grepl("unids|unid", lh.indvbehav$Hyena[j], ignore.case=TRUE)==TRUE & 
                  (is.na(lh.intx$hyena_list[i]) |
                   is.na(lh.intx$num_hyenas[i]) |
                   (str_count(lh.intx$hyena_list[i], ',')-1)<lh.intx$num_hyenas[i]) ) {
        # ...assign the mob ID from intx
        lh.indvbehav$mobID[j] <- lh.intx$mobID[i]
        # show thtat it's been counted in indvbehav
        lh.indvbehav$counted[j] <- 'X'
        # and used in intx
        lh.intx$used[i] <- 'X'
      }  
    } 
  }
}

# create loop for when the mob exists in indvbehav but not in intx.

for (i in 1:length(lh.indvbehav$Session)){
  if (is.na(lh.indvbehav$mobID[i])){
    lh.indvbehav$mobID[i] <- "no mobs matching this session and time in intx dataset"
    for (j in 1:length(lh.intx$session)){
      if (lh.intx$session[j]==lh.indvbehav$Session[i] &
          lh.intx$time[j]==lh.indvbehav$Time[i]) {
        lh.indvbehav$mobID[i] <- NA
        break
      } else {
        next
      }
      
    }
  }
}

for (i in 1:length(lh.indvbehav$Session)) {
  if (is.na(lh.indvbehav$mobID[i])) {
    for (j in 1:length(lh.indvbehav$Session)) {
      if (lh.indvbehav$Session[i]==lh.indvbehav$Session[j] &
          lh.indvbehav$Time[i]==lh.indvbehav$Time[j] &
          lh.indvbehav$Order[i]==lh.indvbehav$Order[j] & 
          grepl("al\\d|unid|af\\d|am\\d", lh.indvbehav$Hyena[i], ignore.case = TRUE ) &
          i!=j) {
        lh.indvbehav$mobID[i] <- lh.indvbehav$mobID[j]
        lh.indvbehav$counted[i] <- "X"
        break
      }
    }
  }
}

# Secondary check for unided issues

for (i in 1:length(lh.indvbehav$Session)) {
  if (is.na(lh.indvbehav$mobID[i])) {
    lh.indvbehav.tmplist <- subset(lh.indvbehav, Session==lh.indvbehav$Session[i] & Time==lh.indvbehav$Time[i])
    if (nrow(lh.indvbehav.tmplist)==1 & grepl("unid|unids", lh.indvbehav$Hyena[i], ignore.case = TRUE)) {
      lh.intx.tmplist <- subset(lh.intx, session==lh.indvbehav$Session[i] & time==lh.indvbehav$Time[i])
      if (nrow(lh.intx.tmplist==1)) {
        lh.indvbehav$mobID[i] <- lh.intx.tmplist$mobID
        lh.indvbehav$counted[i] <- "X"
        lh.intx[lh.intx$session==lh.indvbehav$Session[i],]$used <- "X"
      }
    }
  }
}

## find entries that need the order added

for (i in 1:length(lh.intx$session)) {
  if (is.na(lh.intx$order[i])) {
    lh.intx.tmplist <- subset(lh.intx, session==lh.intx$session[i] & time==lh.intx$time[i])
    if (nrow(lh.intx.tmplist)>1) {
      lh.intx$order[i] <- "fix order"
    }
  }
}

sum(is.na(lh.indvbehav$mobID))
rm(lh.intx.tmplist)
rm(lh.indvbehav.tmplist)

missing.lvl1 <- filter(lh.intx, is.na(mobID) | is.na(used) | mobID=="There are no mobs at this session and time in indvbehav")     #0
missing.lvl2 <- filter(lh.indvbehav, is.na(mobID) | is.na(counted) | mobID == "no mobs matching this session and time in intx dataset")     #0
# write.csv(missing.lvl1, file='lh.intx.MobToCheck.csv')
# write.csv(missing.lvl2, file='lh.indvbehav.MobToCheck.csv')
rm(missing.lvl1)
rm(missing.lvl2)

########## End code by Kenna D.S. Lehmann ##########


########## 3.1 Check and clean output of KDSL code ##########

#Final datasets
lh.intx.tmp <- lh.intx
lh.intx.tmp <- filter(lh.intx.tmp, !is.na(mobID))
lh.indvbehav.tmp <- lh.indvbehav
lh.indvbehav.tmp <- filter(lh.indvbehav.tmp, !is.na(mobID))

#Attach to original datasets
load("02.cleaned_lh_data.Rdata")
lh.intx[lh.intx$id == 1106, ]$hyena_list <- paste(lh.intx[lh.intx$id == 1106, ]$hyena_list, "nod", sep = ",")
lh.intx[lh.intx$id == 2132, ]$hyena_list <- paste(lh.intx[lh.intx$id == 2132, ]$hyena_list, "sy,bag,agp,bsh,fn,ger", sep = ",")
lh.intx[lh.intx$id == 3321, ]$hyena_list <- paste(lh.intx[lh.intx$id == 3321, ]$hyena_list, "pant,alfe,tavi,styr,khtm", sep = ",")
lh.intx[lh.intx$id == 1002203, ]$hyena_list <- paste(lh.intx[lh.intx$id == 1002203, ]$hyena_list, "arro,strm,hucn", sep = ",")
lh.intx[lh.intx$id == 4000135, ]$hyena_list <- paste(lh.intx[lh.intx$id == 4000135, ]$hyena_list, "pan,amaz,juno,mgta,cen,dion,suk", sep = ",")
lh.intx[lh.intx$id == 4000151, ]$hyena_list <- paste(lh.intx[lh.intx$id == 4000151, ]$hyena_list, "suk", sep = ",")
lh.intx[lh.intx$id == 4000152, ]$hyena_list <- paste(lh.intx[lh.intx$id == 4000152, ]$hyena_list, "midd", sep = ",")
lh.intx.mob <- left_join(lh.intx, lh.intx.tmp[,c(1,9)], by = "id")
lh.intx.mob <- filter(lh.intx.mob, !is.na(mobID))
lh.indvbehav.mob <- left_join(lh.indvbehav, lh.indvbehav.tmp[,c(1,11)], by = "ID")
lh.indvbehav.mob <- filter(lh.indvbehav.mob, !is.na(mobID))

#Clean lh.intx.mob
colnames(lh.intx.mob)[1] <- "event"
lh.intx.mob$mobID <- as.numeric(lh.intx.mob$mobID)

#Clean lh.indvbehav.mob
colnames(lh.indvbehav.mob)[1] <- "event"
colnames(lh.indvbehav.mob)[2] <- "session"
colnames(lh.indvbehav.mob)[3] <- "hyena"
colnames(lh.indvbehav.mob)[4] <- "time"
colnames(lh.indvbehav.mob)[5] <- "order"
colnames(lh.indvbehav.mob)[6] <- "behavior"
colnames(lh.indvbehav.mob)[7] <- "target"
colnames(lh.indvbehav.mob)[8] <- "distance"
colnames(lh.indvbehav.mob)[9] <- "leader"
colnames(lh.indvbehav.mob)[10] <- "food"
colnames(lh.indvbehav.mob)[11] <- "notes"
lh.indvbehav.mob$mobID <- as.numeric(lh.indvbehav.mob$mobID)


########## 3.2 Fix mobs with only one hyena ##########

count <- lh.indvbehav.mob %>% group_by(mobID) %>% summarize(count = length(unique(hyena)))
count <- filter(count, count == 1)  #59 mobs have only 1 IDed hyena
count <- filter(lh.indvbehav.mob, mobID %in% count$mobID)
filter(count, !(grepl('unid', count$hyena, ignore.case = TRUE)))  #0 mobs 
# all mobs with 1 hyena are unIDs - okay


########## 3.3 Fix mobs with the same hyena in it twice ##########

lh.indvbehav.mob[lh.indvbehav.mob$event == "1679",]$mobID <- "62"
lh.indvbehav.mob[lh.indvbehav.mob$event == "4206",]$mobID <- "174"
lh.indvbehav.mob[lh.indvbehav.mob$event == "15661",]$mobID <- "214"

count <- lh.indvbehav.mob %>% group_by(mobID) %>% summarize(contains.duplicates = any(duplicated(hyena)))
count <- filter(count, count$contains.duplicates == TRUE)
count <- filter(lh.indvbehav.mob, mobID %in% count$mobID)
count <- filter(count, !is.na(mobID))
count <- arrange(count, mobID, order)
unique(count$mobID)   #0
unique(count$session)   #0


########## 3.4 Fix mobs with multiple session, time, order numbers ##########

lh.indvbehav.mob[lh.indvbehav.mob$event == "5264",]$mobID <- "272"

lh.indvbehav.mobid <-lh.indvbehav.mob[,c(2,4,5,12)]
lh.indvbehav.mobid <- unique(lh.indvbehav.mobid)

#Session - mobIDs with multiple sessionIDs
count <- lh.indvbehav.mobid %>% group_by(mobID) %>% summarize(count = length(unique(time)))
count <- filter(count, count > 1)
unique(count$mobID)   #0

#Time - mobIDs with multiple times
count <- lh.indvbehav.mobid %>% group_by(mobID) %>% summarize(count = length(unique(time)))
count <- filter(count, count > 1)   #0
unique(count$mobID)   #0

#Order - mobIDs with multiple order numbers
count <- lh.indvbehav.mobid %>% group_by(mobID) %>% summarize(count = length(unique(order)))
count <- filter(count, count > 1)   
unique(count$mobID)
length(unique(count$mobID))    #1
# "91" - "t21136" - OK (multiple order numbers = joiners)


########## 3.5 Fix unique session, time, order with multiple mobID numbers ##########

lh.indvbehav.mobid <-lh.indvbehav.mob[,c(2,4,5,12)]
lh.indvbehav.mobid <- unique(lh.indvbehav.mobid)
count <- lh.indvbehav.mobid %>% group_by(session, time, order) %>% summarize(count = length(unique(mobID)))
count <- filter(count, count > 1)
count     #0


########## 3.6 Check for hyenas in lh.intx but missing from lh.indvbehav ##########

id.by.mob.list = list()
counter = 1
all.ids <- data.frame(hyena=character(), mobID=character())

#Separate lh.intx$hyena_list into different rows
for(i in 1:nrow(lh.intx.mob)){
  mobid <- lh.intx.mob$mobID[i]
  if(!is.na(mobid)){
    # `substring<-`("," != ",", paste (","))
    hyenas <- filter(lh.intx.mob, mobID == mobid)$hyena_list %>% 
      strsplit(split = ',') %>% .[[1]]
    if(length(hyenas)){
      all.ids <- data.frame(hyena = hyenas, mobID = rep(mobid, length(hyenas))) 
    }
    #save in list
    id.by.mob.list[[counter]] <- all.ids
    counter <- counter+1
  }
}
lh.intx.mob.sep <- do.call(rbind, id.by.mob.list)
lh.intx.mob.sep$hyena <- gsub(" ", "", lh.intx.mob.sep$hyena)
lh.intx.mob.sep[lh.intx.mob.sep$hyena=="" & !is.na(lh.intx.mob.sep$hyena),] <- NA
lh.intx.mob.sep$mobID <- as.numeric(lh.intx.mob.sep$mobID)
lh.intx.mob.sep <- filter(lh.intx.mob.sep, !is.na(hyena) & !is.na(mobID))

lh.indvbehav.mob$mobID <- as.numeric(lh.indvbehav.mob$mobID)

#Check for hyenas in lh.intx but not in lh.indvbehav
lh.intx.missing <- anti_join(lh.intx.mob.sep, lh.indvbehav.mob, by = c("hyena", "mobID"))
lh.intx.missing <- unique(lh.intx.missing)    #0 hyenas missing
length(unique(lh.intx.missing$mobID))    #0 mobs

#Check for hyenas in lh.indvbehav but not in lh.intx
lh.indvbehav.missing <- anti_join(lh.indvbehav.mob, lh.intx.mob.sep, by = c("hyena", "mobID"))
lh.indvbehav.missing <- unique(lh.indvbehav.missing)     #193 rows
lh.indvbehav.missing <- filter(lh.indvbehav.missing, !grepl("unid", hyena))    #all unIDs
length(unique(lh.indvbehav.missing$mobID))    #0 mobs


########## 3.7 Check that all mobIDs are in both datasets ##########

missing.lh.intx.mob <- filter(lh.intx.mob, !(lh.intx.mob$mobID %in% lh.indvbehav.mob$mobID))   #0
missing.lh.indvbehav.mob <- filter(lh.indvbehav.mob, !(lh.indvbehav.mob$mobID %in% lh.intx.mob$mobID))    #0


########## 3.8 Check joins ##########

#Find minutes in which there are multiple mob
check.joins <- lh.intx.mob
check.joins$multiple.mobs <- NA
for(i in 1:nrow(check.joins)){
  session.i <- check.joins$session[i]
  time.i <- check.joins$time[i]
  mobs <- filter(check.joins, session == session.i & time == time.i)
  if(nrow(mobs) > 0){
    check.joins$multiple.mobs[i] <- TRUE
  }
  if(nrow(mobs) == 0){
    check.joins$multiple.mobs[i] <- FALSE
  }
}
rm(mobs)

#Check that the mobs are actually separate mobs and not the same mob (with joining hyenas)
check.joins <- filter(check.joins, multiple.mobs == TRUE)
check.joins <- filter(check.joins, grepl("join", check.joins$description) | 
                        grepl("join", check.joins$notes))
# write.csv(check.joins, "check.joins.csv", na = "", row.names = FALSE)    #51
#Checked and approved by KDSL & TMM on 03-Dec-19


########## 3.9 Number of unIDs ##########

unids <- filter(lh.indvbehav.mob, grepl("unid", lh.indvbehav.mob$hyena))     #193
nrow(unids)/nrow(lh.indvbehav.mob)    #0.10861 -> 10% of mobbing hyenas were unIDed
length(unique(unids$session))     #59
length(unique(unids$mobID))    #105


########## 3.10 Final datasets ##########

#Final lh.indvbehav.mob dataset
lh.intx.mob <- filter(lh.intx.mob, !is.na(mobID))

#Final lh.indvbehav.mob dataset
lh.indvbehav.mob <- filter(lh.indvbehav.mob, !is.na(mobID))

save(file = "04.lh_data_mob.Rdata", list = c("lh.intx.mob", "lh.indvbehav.mob"))




