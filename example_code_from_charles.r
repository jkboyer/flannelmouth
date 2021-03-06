bCH <- matrix(0, ncol = 32, #n trips/gear codes + 1 col for disposition
              nrow = length(unique(BNT$TAG)))#n unique tags

#BNT trip.num = FMS trip.num.gear

#loop through tags and grab applicable data for each fish/PITTAG
#to put in matrix
for (j in 1:length(unique(BNT$TAG))){
  #temp is subset of data for each tag
  temp <- subset(BNT, BNT$TAG==sort(unique(BNT$TAG))[j])
  #loop through each temp file to get records for each fish
  #j = individual fish/PITTAG
  #k = each record for the fish
  for (k in 1:dim(temp)[1]){
    #this is where I grab either reach or total length
    # <- temp$reach[k] or <- temp$TOTAL_LENGTH[k] (no equation)
    #repeat line twice (BCH1, BCH2 to get each variable)
    bCH[j, temp$tripnum[k]] <- temp$strata[k] + 3*(temp$loc[k] - 1)}
  tm <- min(ifelse(temp$DISP == "RA", 1, -1)) #DISP  for last column
  bCH[j, 32] <- tm}

#this makes one, I need to make 2 (TL and reach)

#simpler, more applicable version of same code


