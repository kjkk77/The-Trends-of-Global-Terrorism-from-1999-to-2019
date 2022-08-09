########################### Data Cleaning Start################################

# iday
gt<-gt[which(gt$iday != 0),]
## Deleted "iday" entries with 0's.

gt[c(1, 3,	4,	5,	6,	7,	13,	16,	
     17,	18,	19,	20,	21,	22,	24,	25,	
     26,	47,	48,	55,	56,	59,	60,	61,	
     62,	63,	64,	65,	66,	67,	68,	69,	
     70,	71,	72,	73,	74,	75,	76,	77,	
     78,	79,	80,	81,	98,	105, 106,	
     107,	108,	109,	110,	111,	112, 
     113,	114,	115,	116,	117,	118, 
     119,	120,	121,	122,	123,	124,	
     125,	126,	127,	128,	129,	130,	
     131,	132,	133,	134,	135)] <- NULL 
# Deleted Unwanted Variables

gt <- gt[which(gt$iyear >= 1999),] #Now, we have data from 1999 to 2019.
gt <- gt[which(gt$doubtterr == 0),] # Filtered out doubtful cases.

######################### Kim's Data Cleaning ##################################

#success
gt$success <- as.logical(gt$success)
#Logical values assigned for "1" and "0".


#suicide
gt$suicide <- as.logical(gt$suicide)
#^^ ^^Logical values assigned for "1" and "0".


#attacktype1, attacktype2, attacktype3

gt$attacktype1[gt$attacktype1 == ""] <- NA
gt$attacktype2[gt$attacktype2 == ""] <- NA
gt$attacktype3[gt$attacktype3 == ""] <- NA
# Replace blanks with NA's.

which(gt$attacktype1 == 1 & gt$attacktype2 == 1)
# No repeating "1" for attacktype1 & attacktype2.
which(gt$attacktype1 == 1 & gt$attacktype3 == 1)
# No repeating "1" for attacktype1 & attacktype3.
which(gt$attacktype1 == 1 & gt$attacktype2 == 1 & 
        gt$attacktype3 == 1)
# No repeating "1" for attacktype1 & attacktype2 & attacktype3

which(gt$attacktype1 == 2 & gt$attacktype2 == 2)
gt$attacktype2[which(gt$attacktype1 == 2 & 
                       gt$attacktype2 == 2)] <- NA
which(gt$attacktype1 == 2 & gt$attacktype2 == 2)
# No repeating "2" for attacktype1 & attacktype2.

which(gt$attacktype1 == 2 & gt$attacktype3 == 2)
gt$attacktype3[which(gt$attacktype1 == 2 & 
                       gt$attacktype3 == 2)] <- NA
which(gt$attacktype1 == 2 & gt$attacktype3 == 2)
# No repeating "2" for attacktype1 & attacktype3.

which(gt$attacktype1 == 2 & gt$attacktype2 == 2 & 
        gt$attacktype3 == 2)
# No repeating "2" for attacktype1 & attacktype2 & attacktype3

which(gt$attacktype1 == 3 & gt$attacktype2 == 3)
gt$attacktype2[which(gt$attacktype1 == 3 & 
                       gt$attacktype2 == 3)] <- NA
which(gt$attacktype1 == 3 & gt$attacktype2 == 3)
# No repeating "3" for attacktype1 & attacktype2.

which(gt$attacktype1 == 3 & gt$attacktype3 == 3)
gt$attacktype3[which(gt$attacktype1 == 3 & 
                       gt$attacktype3 == 3)] <- NA
which(gt$attacktype1 == 3 & gt$attacktype3 == 3)
# No repeating "3" for attacktype1 & attacktype3.

which(gt$attacktype1 == 3 & gt$attacktype2 == 3 & 
        gt$attacktype3 == 3)
# No repeating "3" for attacktype1 & attacktype2 & attacktype3

which(gt$attacktype1 == 4 & gt$attacktype2 == 4)
# No repeating "4" for attacktype1 & attacktype2.

which(gt$attacktype1 == 4 & gt$attacktype3 == 4)
# No repeating "4" for attacktype1 & attacktype3.

which(gt$attacktype1 == 4 & gt$attacktype2 == 4 & 
        gt$attacktype3 == 4)
# No repeating "4" for attacktype1 & attacktype2 & attacktype3

which(gt$attacktype1 == 5 & gt$attacktype2 == 5)
# No repeating "5" for attacktype1 & attacktype2.

which(gt$attacktype1 == 5 & gt$attacktype3 == 5)
# No repeating "5" for attacktype1 & attacktype3.

which(gt$attacktype1 == 5 & gt$attacktype2 == 5 & 
        gt$attacktype3 == 5)
# No repeating "5" for attacktype1 & attacktype2 & attacktype3

which(gt$attacktype1 == 6 & gt$attacktype2 == 6)
gt$attacktype2[which(gt$attacktype1 == 6 & 
                       gt$attacktype2 == 6)] <- NA
which(gt$attacktype1 == 6 & gt$attacktype2 == 6)
# No repeating "6" for attacktype1 & attacktype2.

which(gt$attacktype1 == 6 & gt$attacktype3 == 6)
gt$attacktype3[which(gt$attacktype1 == 6 & 
                       gt$attacktype3 == 6)] <- NA
which(gt$attacktype1 == 6 & gt$attacktype3 == 6)
# No repeating "6" for attacktype1 & attacktype3.

which(gt$attacktype1 == 6 & gt$attacktype2 == 6 & 
        gt$attacktype3 == 6)
# No repeating "6" for attacktype1 & attacktype2 & attacktype3

which(gt$attacktype1 == 7 & gt$attacktype2 == 7)
# No repeating "7" for attacktype1 & attacktype2.
which(gt$attacktype1 == 7 & gt$attacktype3 == 7)
# No repeating "7" for attacktype1 & attacktype3.
which(gt$attacktype1 == 7 & gt$attacktype2 == 7 & 
        gt$attacktype3 == 7)
# No repeating "7" for attacktype1 & attacktype2 & attacktype3

which(gt$attacktype1 == 8 & gt$attacktype2 == 8)
# No repeating "8" for attacktype1 & attacktype2
# Note: attacktype3 only goes up to 7.

which(gt$attacktype1 == 9 & gt$attacktype2 == 9)
# No repeating "9" for attacktype1 & attacktype2

which(gt$attacktype2 == 9)
gt$attacktype2[which(gt$attacktype2 == 9)]
gt$attacktype1[which(gt$attacktype2 == 9)]
gt$attacktype2[which(gt$attacktype2 == 9)] <- NA
# Deleted "9" (Unknown from non-"9" attacktype1 observations)

which(gt$attacktype1 == 9 & gt$attacktype2 != 9)
which(gt$attacktype1 == 9 & gt$attacktype3 != 9)
# No "unknown" attacktype1 with known attacktype2 or attacktype3.



# weaptype1-4/weapsubtype1-4
gt$weaptype1[gt$weaptype1 == ""] <- NA
gt$weaptype2[gt$weaptype2 == ""] <- NA
gt$weaptype3[gt$weaptype3 == ""] <- NA
gt$weaptype4[gt$weaptype4 == ""] <- NA
gt$weapsubtype1[gt$weapsubtype1 == ""] <- NA
gt$weapsubtype2[gt$weapsubtype2 == ""] <- NA
gt$weapsubtype3[gt$weapsubtype3 == ""] <- NA
gt$weapsubtype4[gt$weapsubtype4 == ""] <- NA
# Replace any blanks with NA's.

## weaptype 5: Firearms
which(gt$weapsubtype1 == 2 & gt$weapsubtype2 ==2)
gt$weaptype2[which(gt$weapsubtype1 == 2 & 
                     gt$weapsubtype2 ==2)] <- NA
gt$weapsubtype2[which(gt$weapsubtype1 == 2 & 
                        gt$weapsubtype2 ==2)] <- NA
which(gt$weapsubtype1 == 2 & gt$weapsubtype2 ==2)
# No repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 2 & gt$weapsubtype3 ==2)
gt$weaptype3[which(gt$weapsubtype1 == 2 & 
                     gt$weapsubtype3 ==2)] <- NA
gt$weapsubtype3[which(gt$weapsubtype1 == 2 & 
                        gt$weapsubtype3 ==2)] <- NA
which(gt$weapsubtype1 == 2 & gt$weapsubtype3 ==2)
# No repeating weaptype3/weapsubtype3.
which(gt$weapsubtype1 == 2 & gt$weapsubtype4 ==2)
# No repeating weaptype4/weapsubtype4.

which(gt$weapsubtype1 == 3 & gt$weapsubtype2 ==3)
# No repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 3 & gt$weapsubtype3 ==3)
# No repeating weaptype3/weapsubtype3.
which(gt$weapsubtype1 == 3 & gt$weapsubtype4 ==3)
# No repeating weaptype4/weapsubtype4.


which(gt$weapsubtype1 == 4 & gt$weapsubtype2 ==4)
# repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 4 & gt$weapsubtype3 ==4)
# No repeating weaptype3/weapsubtype3.
which(gt$weapsubtype1 == 4 & gt$weapsubtype4 ==4)
# No repeating weaptype4/weapsubtype4.

which(gt$weapsubtype1 == 5 & gt$weapsubtype2 ==5)
gt$weaptype2[which(gt$weapsubtype1 == 5 & 
                     gt$weapsubtype2 ==5)] <- NA
gt$weapsubtype2[which(gt$weapsubtype1 == 5 & 
                        gt$weapsubtype2 == 5)] <- NA
which(gt$weapsubtype1 == 5 & gt$weapsubtype2 ==5)
# repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 5 & gt$weapsubtype3 ==5)
gt$weaptype3[which(gt$weapsubtype1 == 5 & 
                     gt$weapsubtype3 ==5)] <- NA
gt$weapsubtype3[which(gt$weapsubtype1 == 5 & 
                        gt$weapsubtype3 == 5)] <- NA
which(gt$weapsubtype1 == 5 & gt$weapsubtype3 ==5)
# No repeating weaptype3/weapsubtype3.
which(gt$weapsubtype1 == 5 & gt$weapsubtype4 ==5)
# No repeating weaptype4/weapsubtype4.

which(gt$weapsubtype1 == 6 & gt$weapsubtype2 == 6)
# repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 6 & gt$weapsubtype3 == 6)
# No repeating weaptype3/weapsubtype3.
which(gt$weapsubtype1 == 6 & gt$weapsubtype4 == 6)
# No repeating weaptype4/weapsubtype4

## weaptype 6: Explosives

which(gt$weapsubtype1 == 7 & gt$weapsubtype2 == 7)
gt$weaptype2[which(gt$weapsubtype1 == 7 & 
                     gt$weapsubtype2 == 7)] <- NA
gt$weapsubtype2[which(gt$weapsubtype1 == 7 & 
                        gt$weapsubtype2 == 7)] <- NA
which(gt$weapsubtype1 == 7 & gt$weapsubtype2 == 7)
# repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 7 & gt$weapsubtype3 == 7)
gt$weaptype3[which(gt$weapsubtype1 == 7 & 
                     gt$weapsubtype3 ==7)] <- NA
gt$weapsubtype3[which(gt$weapsubtype1 == 7 & 
                        gt$weapsubtype3 == 7)] <- NA
which(gt$weapsubtype1 == 7 & gt$weapsubtype3 == 7)
# No repeating weaptype3/weapsubtype3.
which(gt$weapsubtype1 == 7 & gt$weapsubtype4 == 7)
# No repeating weaptype4/weapsubtype4

which(gt$weapsubtype1 == 8 & gt$weapsubtype2 == 8)
# repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 8 & gt$weapsubtype3 == 8)
# No repeating weaptype3/weapsubtype3.
which(gt$weapsubtype1 == 8 & gt$weapsubtype4 == 8)
# No repeating weaptype4/weapsubtype4

which(gt$weapsubtype1 == 9 & gt$weapsubtype2 == 9)
# repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 9 & gt$weapsubtype3 == 9)
# No repeating weaptype3/weapsubtype3.
which(gt$weapsubtype1 == 9 & gt$weapsubtype4 == 9)
# No repeating weaptype4/weapsubtype4

which(gt$weapsubtype1 == 10 & gt$weapsubtype2 == 10)
# No repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 10 & gt$weapsubtype3 == 10)
# No repeating weaptype3/weapsubtype3.
which(gt$weapsubtype1 == 10 & gt$weapsubtype4 == 10)
# No repeating weaptype4/weapsubtype4

which(gt$weapsubtype1 == 11 & gt$weapsubtype2 == 11)
gt$weaptype2[which(gt$weapsubtype1 == 11 & 
                     gt$weapsubtype2 == 11)] <- NA
gt$weapsubtype2[which(gt$weapsubtype1 == 11 & 
                        gt$weapsubtype2 == 11)] <- NA
which(gt$weapsubtype1 == 11 & gt$weapsubtype2 == 11)
# No repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 11 & gt$weapsubtype3 == 11)
# No repeating weaptype3/weapsubtype3.
which(gt$weapsubtype1 == 11 & gt$weapsubtype4 == 11)
# No repeating weaptype4/weapsubtype4

which(gt$weapsubtype1 == 12 & gt$weapsubtype2 == 12)
gt$weaptype2[which(gt$weapsubtype1 == 12 & 
                     gt$weapsubtype2 == 12)] <- NA
gt$weapsubtype2[which(gt$weapsubtype1 == 12 & 
                        gt$weapsubtype2 == 12)] <- NA
which(gt$weapsubtype1 == 12 & gt$weapsubtype2 == 12)
# No repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 12 & gt$weapsubtype3 == 12)
# No repeating weaptype3/weapsubtype3
which(gt$weapsubtype1 == 12 & gt$weapsubtype4 == 12)
# No repeating weaptype4/weapsubtype4

which(gt$weapsubtype1 == 13 & gt$weapsubtype2 == 13)
# No repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 13 & gt$weapsubtype3 == 13)
# No repeating weaptype3/weapsubtype3
which(gt$weapsubtype1 == 13 & gt$weapsubtype4 == 13)
# No repeating weaptype4/weapsubtype4

which(gt$weapsubtype1 == 14 & gt$weapsubtype2 == 14)
# No repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 14 & gt$weapsubtype3 == 14)
# No repeating weaptype3/weapsubtype3
which(gt$weapsubtype1 == 14 & gt$weapsubtype4 == 14)
# No repeating weaptype4/weapsubtype4

which(gt$weapsubtype1 == 15 & gt$weapsubtype2 == 15)
# No repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 15 & gt$weapsubtype3 == 15)
# No repeating weaptype3/weapsubtype3
which(gt$weapsubtype1 == 15 & gt$weapsubtype4 == 15)
# No repeating weaptype4/weapsubtype4

which(gt$weapsubtype1 == 16 & gt$weapsubtype2 == 16)
gt$weaptype2[which(gt$weapsubtype1 == 16 & 
                     gt$weapsubtype2 == 16)] <- NA
gt$weapsubtype2[which(gt$weapsubtype1 == 16 & 
                        gt$weapsubtype2 == 16)] <- NA
which(gt$weapsubtype1 == 16 & gt$weapsubtype2 == 16)
# No repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 16 & gt$weapsubtype3 == 16)
gt$weaptype3[which(gt$weapsubtype1 == 16 & 
                     gt$weapsubtype3 == 16)] <- NA
gt$weapsubtype3[which(gt$weapsubtype1 == 16 & 
                        gt$weapsubtype3 == 16)] <- NA
which(gt$weapsubtype1 == 16 & gt$weapsubtype3 == 16)
# No repeating weaptype3/weapsubtype3
which(gt$weapsubtype1 == 16 & gt$weapsubtype4 == 16)
# No repeating weaptype4/weapsubtype4

which(gt$weapsubtype1 == 17 & gt$weapsubtype2 == 17)
gt$weaptype2[which(gt$weapsubtype1 == 17 & 
                     gt$weapsubtype2 == 17)] <- NA
gt$weapsubtype2[which(gt$weapsubtype1 == 17 & 
                        gt$weapsubtype2 == 17)] <- NA
which(gt$weapsubtype1 == 17 & gt$weapsubtype2 == 17)
# No repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 17 & gt$weapsubtype3 == 17)
gt$weaptype3[which(gt$weapsubtype1 == 17 & 
                     gt$weapsubtype3 == 17)] <- NA
gt$weapsubtype3[which(gt$weapsubtype1 == 17 & 
                        gt$weapsubtype3 == 17)] <- NA
which(gt$weapsubtype1 == 17 & gt$weapsubtype3 == 17)
# No repeating weaptype3/weapsubtype3
which(gt$weapsubtype1 == 17 & gt$weapsubtype4 == 17)
# No repeating weaptype4/weapsubtype4

which(gt$weapsubtype1 == 28 & gt$weapsubtype2 == 28)
# No repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 28 & gt$weapsubtype3 == 28)
# No repeating weaptype3/weapsubtype3
which(gt$weapsubtype1 == 28 & gt$weapsubtype4 == 28)
# No repeating weaptype4/weapsubtype4

which(gt$weapsubtype1 == 29 & gt$weapsubtype2 == 29)
# No repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 29 & gt$weapsubtype3 == 29)
# No repeating weaptype3/weapsubtype3
which(gt$weapsubtype1 == 29 & gt$weapsubtype4 == 29)
# No repeating weaptype4/weapsubtype4

which(gt$weapsubtype1 == 31 & gt$weapsubtype2 == 31)
# No repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 31 & gt$weapsubtype3 == 31)
# No repeating weaptype3/weapsubtype3
which(gt$weapsubtype1 == 31 & gt$weapsubtype4 == 31)
# No repeating weaptype4/weapsubtype4

## weaptype 8: Incendiary
which(gt$weapsubtype1 == 18 & gt$weapsubtype2 == 18)
gt$weaptype2[which(gt$weapsubtype1 == 18 & 
                     gt$weapsubtype2 == 18)] <- NA
gt$weapsubtype2[which(gt$weapsubtype1 == 18 & 
                        gt$weapsubtype2 == 18)] <- NA
which(gt$weapsubtype1 == 18 & gt$weapsubtype2 == 18)
# No repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 18 & gt$weapsubtype3 == 18)
# No repeating weaptype3/weapsubtype3
which(gt$weapsubtype1 == 18 & gt$weapsubtype4 == 18)
# No repeating weaptype4/weapsubtype4


which(gt$weapsubtype1 == 19 & gt$weapsubtype2 == 19)
# No repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 19 & gt$weapsubtype3 == 19)
# No repeating weaptype3/weapsubtype3
which(gt$weapsubtype1 == 19 & gt$weapsubtype4 == 19)
# No repeating weaptype4/weapsubtype4

which(gt$weapsubtype1 == 20 & gt$weapsubtype2 == 20)
# No repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 20 & gt$weapsubtype3 == 20)
# No repeating weaptype3/weapsubtype3
which(gt$weapsubtype1 == 20 & gt$weapsubtype4 == 20)
# No repeating weaptype4/weapsubtype4

## weaptype 9: Melee (subtypes: 21-24,26,27)
which(gt$weapsubtype1 == 21 & gt$weapsubtype2 == 21)
# No repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 21 & gt$weapsubtype3 == 21)
# No repeating weaptype3/weapsubtype3
which(gt$weapsubtype1 == 21 & gt$weapsubtype4 == 21)
# No repeating weaptype4/weapsubtype4

which(gt$weapsubtype1 == 22 & gt$weapsubtype2 == 22)
# No repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 22 & gt$weapsubtype3 == 22)
# No repeating weaptype3/weapsubtype3
which(gt$weapsubtype1 == 22 & gt$weapsubtype4 == 22)
# No repeating weaptype4/weapsubtype4

which(gt$weapsubtype1 == 23 & gt$weapsubtype2 == 23)
gt$weaptype2[which(gt$weapsubtype1 == 23 & 
                     gt$weapsubtype2 == 23)] <- NA
gt$weapsubtype2[which(gt$weapsubtype1 == 23 & 
                        gt$weapsubtype2 == 23)] <- NA
which(gt$weapsubtype1 == 23 & gt$weapsubtype2 == 23)
# No repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 23 & gt$weapsubtype3 == 23)
gt$weaptype3[which(gt$weapsubtype1 == 23 & 
                     gt$weapsubtype3 == 23)] <- NA
gt$weapsubtype3[which(gt$weapsubtype1 == 23 & 
                        gt$weapsubtype3 == 23)] <- NA
which(gt$weapsubtype1 == 23 & gt$weapsubtype3 == 23)
# No repeating weaptype3/weapsubtype3
which(gt$weapsubtype1 == 23 & gt$weapsubtype4 == 23)
# No repeating weaptype4/weapsubtype4

which(gt$weapsubtype1 == 24 & gt$weapsubtype2 == 24)
gt$weaptype2[which(gt$weapsubtype1 == 24 & 
                     gt$weapsubtype2 == 24)] <- NA
gt$weapsubtype2[which(gt$weapsubtype1 == 24 & 
                        gt$weapsubtype2 == 24)] <- NA
which(gt$weapsubtype1 == 24 & gt$weapsubtype2 == 24)
# No repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 24 & gt$weapsubtype3 == 24)
# No repeating weaptype3/weapsubtype3
which(gt$weapsubtype1 == 24 & gt$weapsubtype4 == 24)
# No repeating weaptype4/weapsubtype4

which(gt$weapsubtype1 == 26 & gt$weapsubtype2 == 26)
# No repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 26 & gt$weapsubtype3 == 26)
# No repeating weaptype3/weapsubtype3
which(gt$weapsubtype1 == 26 & gt$weapsubtype4 == 26)
# No repeating weaptype4/weapsubtype4

which(gt$weapsubtype1 == 27 & gt$weapsubtype2 == 27)
# No repeating weaptype2/weapsubtype2.
which(gt$weapsubtype1 == 27 & gt$weapsubtype3 == 27)
# No repeating weaptype3/weapsubtype3
which(gt$weapsubtype1 == 27 & gt$weapsubtype4 == 27)
# No repeating weaptype4/weapsubtype4


# nkill, nkillus
gt$nkill[gt$nkill == ""] <- NA
# Replace blanks with NA's
which(gt$nkill == 0 & gt$nkillus > 0)
# Vector indicating where nkill is 0 and nkillus is not 0.
gt$nkill[which(gt$nkill == 0 & gt$nkillus > 0)] <- gt$nkillus[
  which(gt$nkill == 0 & gt$nkillus > 0)]
# Let nkill == non-0 entry of the nkillus, if nkill == 0


which(is.na(gt$nkill) == TRUE & gt$nkillus > 0)
# Vector indicating where nkill is NA and nkillus is not 0.
gt$nkill[which(is.na(gt$nkill) == TRUE & gt$nkillus > 0)] <- gt$nkillus[
  which(is.na(gt$nkill) == TRUE & gt$nkillus > 0)]
# Let nkill == non-0 entry of the nkillus, if nkill == NA
which(gt$nkill == 0 & gt$nkillter > 0)
# Vector indicating where nkill is blank and nkillter is not 0; NA

# nwound, nwoundus
gt$nwound[gt$nwound == ""] <- NA
# Replace blanks with NA's
which(gt$nwound == 0 & gt$nwoundus > 0)
# Vector indicating where nwound is 0 and nwoundus is not 0.
gt$nwound[which(gt$nwound == 0 & gt$nwoundus > 0)] <- gt$nkillus[
  which(gt$nwound == 0 & gt$nwoundus > 0)]
# Let nwound == the non-0 entry of the nwoundus, if nwound == 0.

which(is.na(gt$nwound) == TRUE & gt$nwoundus > 0)
# Vector indicating where nwound is NA and nwound is not 0.
gt$nwound[which(is.na(gt$nwound) == TRUE & gt$nwoundus > 0)] <- gt$nkillus[
  which(is.na(gt$nwound) == TRUE & gt$nwoundus > 0)]
# Let nwound == the non-0 entry of the nwoundus, if nwound == NA
which(gt$nwound == 0 & gt$nwoundter > 0)
# Vector indicating where nwound is blank and nwoundter is not 0

########################## Tran's Data Cleaning ################################

gt$region_txt[gt$region_txt == ""] <- NA 
gt$longitude[gt$plongitude == ""] <- NA 
gt$latitude[gt$latitude == ""] <- NA
gt$provstate[gt$provstate == ""] <- NA 
# Filter out empty observations by filling in NA.

index_states <- gt$provstate[which(gt$country_txt == "United States")]
# Create a vector with US states with terrorism incidents.

gt$targtype1_txt <- factor(gt$targtype1_txt)

gt$targsubtype1_txt[gt$targsubtype1_txt == ""] <- NA
# Filter out empty observations by filling in NA.

sort(table(gt$targsubtype1_txt), decreasing = TRUE)
gt$targsubtype1_txt <- 
  factor(gt$targsubtype1_txt, levels = names(sort(table(gt$targsubtype1_txt),
                                                  decreasing = TRUE)))

sort(table(gt$targsubtype1), decreasing = TRUE)
gt$targsubtype1 <- 
  factor(gt$targsubtype1, 
         levels = names(sort(table(gt$targsubtype1), decreasing = TRUE)))

sort(table(gt$targtype1), decreasing = TRUE)
gt$targtype1 <- 
  factor(gt$targtype1, levels = names(sort(table(gt$targtype1), 
                                           decreasing = TRUE)))

gt$country_txt <- factor(gt$country_txt, 
                         levels = names(sort(table(gt$country_txt),
                                             decreasing = TRUE)))
sort(table(gt$country_txt), decreasing = TRUE)

# Factorize "txt" variables.
# Assign levels for each.
# Sort them in decreasing order.
# Chose the top 5 or 10 subcategories and do the analysis on 
# ... them instead of all subcategory.
# Repeat the process if the variables have the same context.

############################ Data Cleaning End #################################

############################ Data Analysis Start ###############################

############################ Tran's Data Analysis ##############################
library(ggplot2)
gt$region_txt <- factor(gt$region_txt,
                        levels = names(sort(table(gt$region_txt),
                                            decreasing = FALSE)))
ggplot(gt,aes(x = region_txt, fill = region_txt)) +
  geom_histogram(stat = "count") + theme_light() +
  theme(axis.text.x = element_text(size = 7, angle = 90)) +
  labs(x = "", y = "Frequency",
       title = "Number of Attacks Per Regions from 1999 to 2019") +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

#world map
library(maps)
library(ggplot2)
world <- map_data("world")
gt$region_txt[gt$region_txt == ""] <- NA
gt$longitude[gt$plongitude == ""] <- NA
gt$latitude[gt$latitude == ""] <- NA



ggplot(data = world) +
  geom_polygon(aes(x = long, y = lat, group = group), 
               color = "black", fill = "#4F4440") +
  geom_point(data = gt,
             mapping = aes(x = longitude, y = latitude, color = region_txt), 
             size = 0.1) +
  labs(x = "Longitude", y = "Latitude", color = "Regions: ",
       title = "Terrorism Attacks In The World from 1999 to 2019") +
  coord_quickmap() + theme_light() + 
  theme(plot.title = element_text(size = 12)) 

#US
index_states <- gt$provstate[which(gt$country_txt == "United States")]
usa <- map_data("state")

ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group, alpha = 1),
               fill = "#C5ab9f", color = "dark grey") +
  geom_point(data = subset(gt, gt$country_txt == "United States"),
             mapping = aes(x = longitude, y = latitude),
             color = "red", size = 1) +
  coord_quickmap() + theme_light() +
  labs(x = "Longitude", y = "Latitude",
       title = "Terrorism Attacks on United States (1999-2019)") +
  theme(legend.position = "none") + xlim(-135, -60) + ylim(20, 55)

#Each year 1999-2001
ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), 
               fill = "#4F4440") +
  geom_point(data = subset(gt, gt$country_txt == "United States"),
             mapping = aes(x = longitude, y = latitude),
             color = "red", size = 0.2) +
  coord_quickmap() + theme_light() +
  labs(x = "Longitude", y = "Latitude",
       title = "Terrorism Attack on United States Through Each Year") +
  theme(legend.position = "none") + xlim(-135, -60) + ylim(20, 55) +
  facet_wrap(~iyear)


index_city <- gt$city[which(gt$country_txt == "United States")]
sort(table(index_city), decreasing = TRUE)

index_states <- gt$provstate[which(gt$country_txt == "United States")]
sort(table(index_states), decreasing = TRUE) 

gt$targtyp1_txt <- factor(gt$targtype1_txt)

#target government and establishment
ggplot(data = subset(gt, targtype1 == c(2,3,4,7,22,17)),
       mapping = aes(x = reorder(targtype1_txt, iyear, FUN = median),
                     y = iyear, fill = targtyp1_txt)) +
  geom_jitter(size = 0.1, alpha = 0.1) +
  geom_boxplot() +
  labs(title = "Box Plot Shows Target Relates to Government and Establishment",
       x = "Type of target", y = "Years") +
  theme_minimal() + coord_flip() + theme(legend.position = "none")
#target public
ggplot(data = subset(gt, targtype1 == c(1,6,9,21,11,14,16,18,19)),
       mapping = aes(x = reorder(targtype1_txt, iyear, FUN = median),
                     y = iyear, fill = targtyp1_txt)) +
  geom_jitter(size = 0.1, alpha = 0.1) +
  geom_boxplot() +
  labs(title = "Box Plot Shows Target Relates to Public",
       x = "Type of target", y = "Years") +
  theme_minimal() + coord_flip() + theme(legend.position = "none")
#target to ideology
ggplot(data = subset(gt, targtype1 == c(5,8,10,12,15)),
       mapping = aes(x = reorder(targtype1_txt, iyear, FUN = median),
                     y = iyear, fill = targtyp1_txt)) +
  geom_jitter(size = 0.1, alpha = 0.1) +
  geom_boxplot() +
  labs(title = "Box Plot Shows Target Relate to Ideologies",
       x = "Type of target", y = "Years") +
  theme_minimal() + coord_flip() + theme(legend.position = "none")
#target unknown
ggplot(data = subset(gt, targtype1 == c(13,20)),
       mapping = aes(x = reorder(targtype1_txt, iyear, FUN = median),
                     y = iyear, fill = targtyp1_txt)) +
  geom_jitter(size = 0.1, alpha = 0.1) +
  geom_boxplot() +
  labs(title = "Box Plot Shows Target with Uncertain Reasons",
       x = "Type of target", y = "Years") +
  theme_minimal() + coord_flip() + theme(legend.position = "none")

library(RColorBrewer)
mycolor = c(brewer.pal(name = "PiYG", n = 11), brewer.pal(name = "PuOr", n = 11))
ggplot(data = gt, mapping = aes(x = reorder(targtype1_txt, iyear, FUN = median), 
                                y = iyear, fill = targtype1_txt)) +
  geom_jitter(size = 0.1, alpha = 0.1) +
  geom_boxplot() +
  labs(title = "Box Plot Shows Distribution of Target Types Through Years",
       x = "Type of target", y = "Years") +
  theme_minimal() + coord_flip() + theme(legend.position = "none") +
  scale_fill_manual(values = mycolor) + theme(title = element_text(size = 9))

gt$targsubtype1_txt[gt$targsubtype1_txt == ""] <- NA
sort(table(gt$targsubtype1_txt), decreasing = TRUE)
gt$targsubtype1_txt <- 
  factor(gt$targsubtype1_txt, levels = names(sort(table(gt$targsubtype1_txt),
                                                  decreasing = TRUE)))
sort(table(gt$targsubtype1), decreasing = TRUE)
gt$targsubtype1 <- 
  factor(gt$targsubtype1, 
         levels = names(sort(table(gt$targsubtype1), decreasing = TRUE)))

sort(table(gt$targtype1), decreasing = TRUE)
sort(table(gt$targtype1_txt), decreasing = TRUE)

gt$targtype1 <- 
  factor(gt$targtype1, levels = names(sort(table(gt$targtype1), 
                                           decreasing = TRUE)))

library(forcats)
library(ggplot2)
ggplot(data = subset(gt, targtype1 == c(14,2,3,4,1))) + 
  geom_bar(aes(x = fct_infreq(targtype1_txt),
               fill = region_txt), position = "stack") +
  labs(title = "Distribution of Top 5 Most Common Target Types Through Regions", 
       x = "", y = "Frequency", fill = "Regions: ") + theme_minimal() + 
  theme(axis.text.x = element_text(size = 5), title = element_text(size = 10))
############################ Kim's Data Analysis ###############################
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(scales)

# Data Frame: gt_1
gt_1 <- select(gt, iyear, region_txt, attacktype1, attacktype2, attacktype3, 
               nkill, nwound, success)

newattack <- rep(0,length(gt_1$attacktype1))

for (i in 1:length(gt_1$attacktype1)){
  if (is.na(gt_1$attacktype1[i]) == FALSE & 
      is.na(gt_1$attacktype2[i]) == FALSE) {
    newattack[i] <- 10
  }
  else {
    newattack[i] <- gt_1$attacktype1[i]
  }
}
gt_1$newattack <- newattack

# Plot: Terrorism Tactics By Region
ggplot(data = gt_1, aes(x= region_txt, fill = as.factor(newattack))) + 
  geom_bar(position = "fill") + theme_light()+
  labs(x = "Region",y = "Rel. Frequency", 
       title = "Terrorism Tactics By Region") + 
  theme(axis.text.x= element_text(angle=45, size=7.5, vjust = 1,
                                  hjust = 1),
        legend.text = element_text(size = 6.5)) + 
  scale_fill_brewer(palette = "Set3", name = "Tactic Type", 
                    labels = c("Assassination","Armed Assault","Bombing/Explosion",
                               "Hijacking", "Barricade Incident", "Kidnapping",
                               "Facility/Infrastructure Attack","Unarmed Assault",
                               "Unknown", "Combination")) + 
  guides(fill = guide_legend(title = NULL))


# Data Frame: gt_3
for (i in 1999:2019) {
  if (i == 1999){
    x2 <- c(i,unname(table(factor(gt_1$newattack[which(gt_1$iyear == i)], levels = 1:10))))
  }
  else if (i < 2019){
    x2 <- rbind(x2,c(i,unname(table(
      factor(gt_1$newattack[which(gt_1$iyear == i)], levels = 1:10
      )))))
  }
  else{
    x2 <- rbind(x2,c(i,unname(table(
      factor(gt_1$newattack[which(gt_1$iyear == i)], levels = 1:10
      )))))
    gt_3 <- as.data.frame(x2)}
}

colnames(gt_3) <- c("Year","Assassination","Armed Assault","Bombing/Explosion",
                    "Hijacking", "Barricade Incident", "Kidnapping",
                    "Facility/Infrastructure Attack","Unarmed Assault","Unknown",
                    "Combination")

gt_3 <- melt(gt_3,id.vars = c("Year"))

# Plot: Frequency of Terrorism Tactics 1999-2019 
ggplot(data = gt_3, aes(x = as.factor(Year), y = value, color = variable,
                        group = variable)) +
  theme_light()+ geom_line(size = 0.8, alpha = 0.75) + geom_point()+
  labs(title = "Frequency of Terrorism Tactics 1999-2019", x = "Years", 
       y = "Frequency") +
  theme(axis.text.x= element_text(angle=90, size=8),
        panel.background = element_rect(color = "black")) +
  scale_color_brewer(palette = "Paired", name = "Tactic Type", 
                     labels = c("Assassination","Armed Assault","Bomb./Expl.",
                                "Hijack", "Barricade", "Kidnap",
                                "Facil./Infrastr.","Unarmed Assault","Unknown", 
                                "Combination"))

# Plot: Death Count from Terrorism 1999-2019
ggplot(data = gt_1, aes(x = as.factor(iyear), y = nkill, 
                        fill = as.factor(newattack))) + 
  geom_col() +
  scale_fill_brewer(palette = "Paired", name = "Tactic Type", 
                    labels = c("Assassination","Armed Assault","Bomb./Expl.",
                               "Hijack", "Barricade", "Kidnap",
                               "Facil./Infrastr.","Unarmed Assault","Unknown", 
                               "Combination")) + 
  labs(title = "Death Count from Terrorism 1999-2019",
       x = "Year", y = "Death") + theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, size = 7))

#Data Frame: gt_cum
gt_test <- select(gt,iyear, nkillter)
for (i in 1999:2019) {
  if ( i == 1999) {
    gt_cum <- c(i,
                sum(gt_test$nkillter[which(gt_test$iyear == i)], na.rm = TRUE))
  }
  else {
    gt_cum <- rbind(gt_cum, c(i,sum(gt_test$nkillter[which(gt_test$iyear == i)], 
                                    na.rm = TRUE)))
  }
} 
gt_cum <- as.data.frame(gt_cum)
colnames(gt_cum) <- c("Year", "Perp.Death")

# Plot: Perpetrator Fatality
ggplot(data = gt_cum, aes(x = as.factor(Year), 
                          y = Perp.Death)) + theme_light() +
  geom_col() +
  labs(x = "Year", title = "Perpetrator Fatality") + theme(axis.text.x = element_text(angle = 90))

# Data Frame: gt_8
x6 <- c(0)
for (i in 1:10){
  a <- sum(gt_1$nkill[which(gt_1$newattack == i)], na.rm = TRUE)/length(which(gt_1$newattack == i))
  x6[i] <- a
}
gt_8 <- as.data.frame(x6)
colnames(gt_8) <- c("Deaths-Per-Event")
rownames(gt_8) <- c("Assassination","Armed Assault","Bomb./Expl.",
                    "Hijack", "Barricade", "Kidnap",
                    "Facil./Infrastr.","Unarmed Assault","Unknown","Combination")

# Data Frame: gt_7
for (i in 1:10){
  if (i == 1){
    x6 <- c(i,sum(gt_6$nkill[which(gt_6$newattack == i)]),
            sum(gt_6$nwound[which(gt_6$newattack == i)]))
  }
  else {
    x6 <- rbind(x6, c(i,sum(gt_6$nkill[which(gt_6$newattack == i)]),
                      sum(gt_6$nwound[which(gt_6$newattack == i)])))
  }
}
gt_7 <- as.data.frame(x6)
colnames(gt_7) <- c("Tactic", "Dead", "Wounded")

gt_7$Tactic <- factor(gt_7$Tactic, levels = c(1:10),
                      labels = c("Assassination","Armed Assault","Bombing/Explosion",
                                 "Hijacking", "Barricade", "Kidnapping",
                                 "Fac./Infr.","Unarmed","Unknown", "Combo"))
gt_7 <- melt(gt_7, id.vars = c("Tactic"))
colnames(gt_7) <- c("Tactic", "Type", "Frequency")

# Plot: Death-to-Wounded Ratio
ggplot(data = subset(gt_7), 
       aes(x = Tactic, y= Frequency, fill = Type)) + theme_light() +
  geom_col(position = "dodge") + theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.75, size = 7.5), axis.text.y =  element_text(size = 10)) +
  scale_y_continuous(labels = comma) + labs(title = "Death-to-Wounded Ratio") + geom_text(aes(label = Frequency), position = position_dodge(1), size = 2.25, vjust = -0.4)


# Plot: Success Rate Per Tactic
ggplot(data = gt_1) + 
  geom_bar(mapping = aes(x= as.factor(newattack), fill = success),position = "fill") + theme_light() + labs(title = "Success Rates of Tactics", y= "Rel. Freq", x = "Tactic") +
  scale_x_discrete(labels = c("Assassination","Armed Assault","Bombing/Explosion",
                              "Hijacking", "Barricade Incident", "Kidnapping",
                              "Facility/Infrastructure Attack","Unarmed Assault","Unknown",
                              "Combination")) +
  theme(axis.text.x = element_text(angle = 45, size = 8,
                                   hjust = 1))
############################ Data Analysis End #################################

