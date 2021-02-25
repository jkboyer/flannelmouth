#convert data into capture history format
#authors: Jan Boyer, Charles Yackulic
#inputs: all_PIT_tagged_flannelmouth.csv in project data folder
#        all PIT tagged flannies caught since 2004 with boat efishing,
#        baited and unbaited hoop nets, and temporary submersible antennas
#outputs: ./data/FMS_capture_history_reach.csv (capture history, reach)
#         ./data/FMS_capture_history_length.csv (capture history, total length)

#load packages and data ######
fms <- read.csv("./data/all_PIT_tagged_flannelmouth.csv",
                stringsAsFactors = FALSE)
require(dplyr)

#gears: EL = boat electrofishing
#       HB = baited hoop net
#       HU = unbaited hoop net
#       AN = temp antenna

#subset to only needed cols to simplify and help loop run faster ##########
fms <- fms %>%
  select(trip.gear, #ID field for columns
         PITTAG, #ID field for rows
         reach_no, TL, #these will be data to fill each matrix
         TRIP_ID, season, year, gear_code_simplified, disposition) #other data

# remove missing data that would mess up model #################
# records with NA entries should have been removed in load_FMS_data.r file,
# but this is just an additional check to make sure data is good for model!

#1. remove record if reach_no is missing
#2. remove record if TL is missing (except for antenna data)
#3. remove record if we do not have effort data for that trip.gear
#      (will need to also load effort dataframe to check)
#    ok I already did that in the load_FMS_data.r file

fms <- fms %>% #remove records missing reach
  filter(!is.na(reach_no))

fms <- fms %>% #remove records missing length (except for antenna data)
  filter(gear_code_simplified == "AN" | #keep all antenna data, it does not have length
           !is.na(TL)) #for other gears, only keep if it has total length


# test with smaller dataset (subset of FMS) #############
#can ignore this if you are running code for real
#leaving it here because it's helpful for troubleshooting if errors occur

#Make a tiny dataframe so I can practice running the loop quickly
#what PIT tags were seen the most? test with those
tagcount <- fms %>%
  group_by(PITTAG) %>%
  summarise(n = n()) %>%
  arrange(-n)

tags.to.test <- c(tagcount$PITTAG[1:20], #some fish with lots of records
                  tagcount$PITTAG[1765:1775], #fish with 3 or 4 records
                  tagcount$PITTAG[51537:51547],#fish with only 1 record
                  tagcount$PITTAG[20:40]) #more fish w/many records

#small dataframe to test loop with
fms.test <- fms %>%
  filter(PITTAG %in% tags.to.test)

#make 2 matrices: 1 with reach
#                 1 with total length
#ncols = each trip + gear combination
#nrows = each unique PIT tag

#create variables for n trip/gear combos, and n unique PIT tags
#these are used to determine size of matrices
n.PIT = length(unique(fms.test$PITTAG)) # n rows = n unique fish
n.trips.gears = length(unique(fms.test$trip.gear)) #n cols = n trips + gears

# reach matrix
fms.reach <- matrix(0, #fill with zeros to start with
                 ncol = n.trips.gears + 1, #n trips/gear codes + 1 = n columns
                 nrow = n.PIT) #n unique tags = n rows

#give matrix row and column names - needed so that the loop knows where
#each tag and trip/gear record goes in matrix!
rownames(fms.reach) <- sort(unique(fms.test$PITTAG))
colnames(fms.reach) <- c(unique(fms.test$trip.gear), "disposition")

#loop through tags and grab applicable data for each fish/PITTAG
#to put in matrix
#j: PITTAG (individual fish)
#k: each record for the fish

#first loop works, error is in second

#for each PITTAG in the unique PITTAGs in fms dataframe
for (j in 1:length(unique(fms.test$PITTAG))){
  #create a subset df (temp) with only records for that PITTAG
  temp <- subset(fms.test, fms.test$PITTAG == sort(unique(fms.test$PITTAG))[j])
  #For each fish, loop through records for that fish
  for (k in 1:dim(temp)[1]){ ##### ? error?
    #and fill the fms.TL matrix with the reach for that record
    fms.reach[j, temp$trip.gear[k]] <- temp$reach_no[k]
    #get the disposition info for the fish's last record
    tm <- min(ifelse(temp$disposition == "alive", 1, -1))
    #put disposition into the last column for that fish's row
    fms.reach[j, n.trips.gears + 1] <- tm
    }
}

#total length matrix
fms.TL <- matrix(0, #fill with zeros to start with
                 ncol = n.trips.gears + 1, #n trips/gear codes + 1 columns
                 nrow = n.PIT) #n unique tags

#give matrix row and column names
#sort() is needed so things are ordereed correctly
rownames(fms.TL) <- sort(unique(fms.test$PITTAG))
colnames(fms.TL) <- c(unique(fms.test$trip.gear), "disposition")

#loop to fill empty fms.TL matrix with fish lengths
#for each PITTAG in the unique PITTAGs in fms dataframe
for (j in 1:length(unique(fms.test$PITTAG))){
  #create a subset df (temp) with only records for that PITTAG
  temp <- subset(fms.test, fms.test$PITTAG == sort(unique(fms.test$PITTAG))[j])
  #For each fish, loop through records for that fish
  for (k in 1:dim(temp)[1]){
    #and fill the fms.TL matrix with the total length at that record
    fms.TL[j, temp$trip.gear[k]] <- temp$TL[k]
    #get the disposition info for the fish's last record
  tm <- min(ifelse(temp$disposition == "alive", 1, -1))
  #put disposition into the last column for that fish's row
  fms.TL[j, n.trips.gears + 1] <- tm
  }
}

#remove all test dataframes before running loops with all data
rm(fms.test, fms.TL, fms.reach, temp, tagcount, j, k, n.PIT, n.trips.gears,
   tags.to.test, tm)

#create capture histories with all FMS data #########
#make 2 matrices: 1 with reach
#                 1 with total length
#ncols = each trip + gear combination
#nrows = each unique PIT tag

#create variables for n trip/gear combos, and n unique PIT tags
#these are used to determine size of matrices
n.PIT = length(unique(fms$PITTAG)) # n rows = n unique fish
n.trips.gears = length(unique(fms$trip.gear)) #n cols = n trips + gears

# reach matrix #######
#NOTE: on Jan's computer, loops about take 7-8 hours to run
#      try to run overnight, or when not at computer
#      and save workspace (.rdata) afterwards

fms.reach <- matrix(0, #fill with zeros to start with
                    ncol = n.trips.gears + 1, #n trips/gear codes + 1 = n columns
                    nrow = n.PIT) #n unique tags = n rows

#give matrix row and column names - needed so that the loop knows where
#each tag and trip/gear record goes in matrix!
rownames(fms.reach) <- sort(unique(fms$PITTAG))
colnames(fms.reach) <- c(unique(fms$trip.gear), "disposition")

#loop through tags and grab applicable data for each fish/PITTAG
#to put in matrix
#j: PITTAG (individual fish)
#k: each record for the fish

#first loop works, error is in second

#for each PITTAG in the unique PITTAGs in fms dataframe
for (j in 1:length(unique(fms$PITTAG))){
  #create a subset df (temp) with only records for that PITTAG
  temp <- subset(fms, fms$PITTAG == sort(unique(fms$PITTAG))[j])
  #For each fish, loop through records for that fish
  for (k in 1:dim(temp)[1]){ ##### ? error?
    #and fill the fms.TL matrix with the reach for that record
    fms.reach[j, temp$trip.gear[k]] <- temp$reach_no[k]
    #get the disposition info for the fish's last record
    tm <- min(ifelse(temp$disposition == "alive", 1, -1))
    #put disposition into the last column for that fish's row
    fms.reach[j, n.trips.gears + 1] <- tm
  }
}


#total length matrix #########
fms.TL <- matrix(0, #fill with zeros to start with
                 ncol = n.trips.gears + 1, #n trips/gear codes + 1 columns
                 nrow = n.PIT) #n unique tags

#give matrix row and column names
rownames(fms.TL) <- sort(unique(fms$PITTAG))
colnames(fms.TL) <- c(unique(fms$trip.gear), "disposition")

#loop to fill empty fms.TL matrix with fish lengths
#for each PITTAG in the unique PITTAGs in fms dataframe
for (j in 1:length(unique(fms$PITTAG))){
  #create a subset df (temp) with only records for that PITTAG
  temp <- subset(fms, fms$PITTAG == sort(unique(fms$PITTAG))[j])
  #For each fish, loop through records for that fish
  for (k in 1:dim(temp)[1]){
    #and fill the fms.TL matrix with the total length at that record
    fms.TL[j, temp$trip.gear[k]] <- temp$TL[k]
    #get the disposition info for the fish's last record
    tm <- min(ifelse(temp$disposition == "alive", 1, -1))
    #put disposition into the last column for that fish's row
    fms.TL[j, n.trips.gears + 1] <- tm
  }
}

#save capture histories
write.csv(fms.reach, "./data/FMS_capture_history_reach.csv")
write.csv(fms.TL, "./data/FMS_capture_history_length.csv")
