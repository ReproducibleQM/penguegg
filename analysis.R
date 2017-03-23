##################################################################
##                      Penguegg group                          ##
##      Changes rockhopper penguin in egg size over time        ##
##                        RQM Course                            ##
##################################################################

library(dplyr)
library(ggplot2)
library(geosphere)
library(scales)
library(RColorBrewer)
library(AICcmodavg)

#########################import data##############################
rawEggSize <- read.csv("NRPE_eggSize_raw.csv")
names(rawEggSize) <- c('Source', 'SpecimenID', 'Location', 'Day',
                       'Month', 'Year', 'EggOrder', 'Length', 'Breadth', 'Volume', 'Reference')
##################################################################



###################Define discriminant function####################

##'Defines discriminant function with arbitrary cutoff
##'to determine egg-laying order of eggs listed as
##'unknown in dataset. Discriminant function comes from
##'Bond et al. 2016: 
##'D = 0.58*Length + 0.39*Breadth - 57.48
##'D = 0.73*Length + 0.50*Breadth - 72.39

assignOrder <- function(length, breadth, cutoff = 0.66){
  ##discriminant function
  ####From results
  #d <- length*0.73 + breadth*0.50 - 72.39
  
  ####From results, with signs switched
  #d <- -length*0.73 - breadth*0.50 + 72.39
  
  ####From the abstract
  d <- length*0.58 + breadth*0.39 - 57.48
  probB <- 1/(1+exp(-d))
  
  ###Discriminate
  if(probB > cutoff){
    return('B')
  }else if(probB < (1-cutoff)){
    return('A')
  }else{return('U')}
}
##################################################################

###########Define function for classifying oddballs###############
classifyOddballs <- function(eggdat, sdcutoff = 4){
  ###Identify means and sd's of breadth and length
  sdlength <- sd(eggdat$Length)
  sdbreadth <- sd(eggdat$Breadth)
  meanlength <- mean(eggdat$Length)
  meanbreadth <- mean(eggdat$Breadth)
  
  ##Classify abnormal eggs based on length and breadth
  ##based on supplied cutoff
  lengthAbnormal <- abs(eggdat$Length - meanlength)/sdlength > sdcutoff
  breadthAbnormal <- abs(eggdat$Breadth - meanbreadth)/sdbreadth > sdcutoff
  
  ##Change EggOrder to ODDBALL for eggs that are EITHER abnormal in length
  ##or abnormal in breadth
  eggdat$EggOrder <- as.character(eggdat$EggOrder)
  eggdat[lengthAbnormal | breadthAbnormal,'EggOrder'] <- 'ODDBALL'
  eggdat$EggOrder <- as.factor(eggdat$EggOrder)
  
  return(eggdat)
}
##################################################################



######################Apply discriminant function#################
eggSizeAssigned <- rawEggSize

##CHANGE CUTOFF HERE
AccuracyCutoff = 0.66
##

eggSizeAssigned[eggSizeAssigned$EggOrder == 'U',]$EggOrder <- mapply(FUN = assignOrder, 
                                                                     filter(eggSizeAssigned, EggOrder == 'U')$Length, 
                                                                     filter(eggSizeAssigned, EggOrder == 'U')$Breadth, 
                                                                     AccuracyCutoff)
##################################################################

########################Classify Oddballs#########################
##CHANGE CUTOFF HERE
oddballCutoff <- 3.5
##

eggSizeCleaned <- classifyOddballs(eggSizeAssigned, oddballCutoff)
##################################################################


###############Compare to Bond et al. assignments#################
##'The discriminant function above should have reproduced
##'the assignment procedure in Bond et al. The plots below
##'suggest that it has done so, with the exception of 
##'ODDBALLS, which we haven't been classified yet in this analysis
eggSizePub <- read.csv('NRPE_eggSize.csv')
par(mfrow = c(2,1))
boxplot(eggSizePub$length ~ eggSizePub$predEggOrder, main = 'Bond et al.')
boxplot(eggSizeCleaned$Length ~ eggSizeCleaned$EggOrder, main = 'Us')
plot(eggSizePub$predEggOrder, main = 'Bond et al.')
plot(eggSizeCleaned$EggOrder, main = 'Us')

summary(eggSizePub$predEggOrder)
summary(eggSizeCleaned$EggOrder)
##################################################################


##################Some basic descriptive plots####################
boxplot(data = eggSizeCleaned[eggSizeCleaned$EggOrder == 'B',], Volume ~ Year, main = 'B Eggs')
boxplot(data = eggSizeCleaned[eggSizeCleaned$EggOrder == 'A',], Volume ~ Year, main = 'A Eggs')

boxplot(data = eggSizeCleaned[eggSizeCleaned$EggOrder == 'B',], Volume ~ Month, main = 'B Eggs')
boxplot(data = eggSizeCleaned[eggSizeCleaned$EggOrder == 'A',], Volume ~ Month, main = 'A Eggs')
# 
#########################Volume ~ Month############################
# eggSizeNoOdd <- eggSizeNoOdd[complete.cases(eggSizeNoOdd),]
# 
# summary(lm(Volume ~ Month, data = eggSizeNoOdd))
# m1 <- lm(Volume ~ Month, data = eggSizeNoOdd)
# ##without high leverage data point
# summary(lm(Volume ~ Month, data = eggSizeNoOdd[-936,]))
# 
# ##split by Egg Order
# summary(lm(Volume ~ Month, data = filter(eggSizeNoOdd, EggOrder == 'B'))) ## p = .00105
# summary(lm(Volume ~ Month, data = filter(eggSizeNoOdd, EggOrder == 'A'))) ##n.s
###################################################################
# 
# 
#########################Volume ~ Year#############################
# m2 <- lm(Volume ~ Year, data = eggSizeNoOdd)
# boxplot(data = eggSizeNoOdd, Volume ~ Year)
# 
# ##Split by egg order
# summary(lm(Volume ~ Year, data = filter(eggSizeNoOdd, EggOrder == 'B'))) ##n.s
# summary(lm(Volume ~ Year, data = filter(eggSizeNoOdd, EggOrder == 'A'))) ##p = .00699
###################################################################
# 
# 
#######################Volume ~ Location###########################
# m3 <- lm(Volume ~ Location, data = eggSizeNoOdd)
# boxplot(Volume ~ Location, eggSizeNoOdd[eggSizeNoOdd$EggOrder == 'A',], main = 'A')
# boxplot(Volume ~ Location, eggSizeNoOdd[eggSizeNoOdd$EggOrder == 'B',], main = 'B')
# summary(lm(Volume ~  Location, data = filter(eggSizeNoOdd, EggOrder == 'B')))
# summary(lm(Volume ~  Location, data = filter(eggSizeNoOdd, EggOrder == 'A')))
###################################################################
##################################################################
#eggSizeNoOdd <- filter(eggSizeCleaned, EggOrder != 'ODDBALL')
eggSizeNoOdd <- filter(eggSizeCleaned, EggOrder %in% c('A', 'B'))

#################Model selection for basic variables###############
##'SST at different ranges
##'SSP at different ranges
##'Volume as response variable
##'Location
##'Month
##'Egg Order seperately
##'Year

#eggDat.A <- filter(eggSizeNoOdd, EggOrder == 'A')
#eggDat.B <- filter(eggSizeNoOdd, EggOrder == 'B')
eggs <- eggSizeNoOdd

###remove missing data
eggs <- eggs[complete.cases(eggs),]
eggs$ConvergentZone <- ifelse(eggs$Location == 'Gough', 1, 0)

###Model selection for basic variables - A eggs
eggsA <- filter(eggs, EggOrder == 'A')
mb1a <- lm(data = eggsA, Volume ~ 1)
mb2a <- lm(data = eggsA, Volume ~ Location)
mb3a <- lm(data = eggsA, Volume ~ Year)
mb4a <- lm(data = eggsA, Volume ~ Month)
mb5a <- lm(data = eggsA, Volume ~ ConvergentZone)
mb6a <- lm(data = eggsA, Volume ~ Month + Year)
mb7a <- lm(data = eggsA, Volume ~ Month + Location)
mb8a <- lm(data = eggsA, Volume ~ Month + ConvergentZone)
mb9a <- lm(data = eggsA, Volume ~ Year + Location)
mb10a <- lm(data = eggsA, Volume ~ Year + ConvergentZone)
mb11a <- lm(data = eggsA, Volume ~ Year + ConvergentZone + Month)
mb12a <- lm(data = eggsA, Volume ~ Year + Location + Month)

aic.a <- aictab(list(mb1a, mb2a, mb3a, mb4a, mb5a, mb6a, mb7a, mb8a, mb9a, mb10a, mb11a, mb12a))
aic.a



###Model selection for basic variables - B eggs
eggsB <- filter(eggs, EggOrder == 'B')
mb1b <- lm(data = eggsB, Volume ~ 1)
mb2b <- lm(data = eggsB, Volume ~ Location)
mb3b <- lm(data = eggsB, Volume ~ Year)
mb4b <- lm(data = eggsB, Volume ~ Month)
mb5b <- lm(data = eggsB, Volume ~ ConvergentZone)
mb6b <- lm(data = eggsB, Volume ~ Month + Year)
mb7b <- lm(data = eggsB, Volume ~ Month + Location)
mb8b <- lm(data = eggsB, Volume ~ Month + ConvergentZone)
mb9b <- lm(data = eggsB, Volume ~ Year + Location)
mb10b <- lm(data = eggsB, Volume ~ Year + ConvergentZone)
mb11b <- lm(data = eggsB, Volume ~ Year + ConvergentZone + Month)
mb12b <- lm(data = eggsB, Volume ~ Year + Location + Month)

aic.b <- aictab(list(mb1b, mb2b, mb3b, mb4b, mb5b, mb6b, mb7b, mb8b, mb9b, mb10b, mb11b, mb12b))
aic.b



########################Read in environmental data################
edatS <- read.fwf('Environmental Data1800/MSG2.S.enh.180001.201412_1', widths = c(6,4,3,7,8,5,8,7,8,8,8,8,8,9,8,8), skip = 2, head = F)
names(edatS) <- c('Year', 'Mon', 'BSZ', 'BLO', 'BLA', 'PID2', 'S1', 'S3', 'S5', 'M','N','S','D','HT', 'X', 'Y')
edatS[edatS$HT == -9999,'HT'] <- NA 

edatP <- read.fwf('Environmental Data1800/MSG2.P.enh.180001.201412_1', widths = c(6,4,3,7,8,5,8,7,8,8,8,8,8,9,8,8), skip = 2, head = F)
names(edatP) <- c('Year', 'Mon', 'BSZ', 'BLO', 'BLA', 'PID2', 'S1', 'S3', 'S5', 'M','N','S','D','HT', 'X', 'Y')
edatP[edatP$HT == -9999,'HT'] <- NA 
##################################################################


####################Calculate distances to islands################
###Coordinates of the center of the Tristan da Cunha island cluster
tristanLat <- -37.26
tristanLon <- -12.47

goughLat <- -40.31
goughLon <- -9.93

edatP$Lon <- edatP$BLO + edatP$X
edatP$Lat <- edatP$BLA + edatP$Y
edatP[edatP$Lon > 180,'Lon'] <- edatP[edatP$Lon > 180,'Lon'] - 360

###Define bins for binning distances
bins <- c(0, 250, 500, 1000, 2100, 100000)

##Dist in kilometers
edatP$distTristan <- distGeo(c(tristanLon, tristanLat), mapply(c, edatP[,c('Lon', 'Lat')]))/1000
edatP$distTristanBin <- cut(edatP$distTristan, breaks = bins, right = T)
edatP$distGough <- distGeo(c(goughLon, goughLat), mapply(c, edatP[,c('Lon', 'Lat')]))/1000
edatP$distGoughBin <- cut(edatP$distGough, breaks = bins, right = T)




edatS$Lon <- edatS$BLO + edatS$X
edatS$Lat <- edatS$BLA + edatS$Y
edatS[edatS$Lon > 180,'Lon'] <- edatS[edatS$Lon > 180,'Lon'] - 360

###distance in kilometers
edatS$distTristan <- distGeo(c(tristanLon, tristanLat), mapply(c, edatS[,c('Lon', 'Lat')]))/1000
edatS$distTristanBin <- cut(edatS$distTristan, breaks = bins, right = T)
edatS$distGough <- distGeo(c(goughLon, goughLat), mapply(c, edatS[,c('Lon', 'Lat')]))/1000
edatS$distGoughBin <- cut(edatS$distGough, breaks = bins, right = T, labels = c('bin1, bin2 '))

colors <- brewer.pal(5, 'Mono')
colors2 <- brewer.pal(5, 'Blues')
#plot(mapply(c, edatP[,c('Lon', 'Lat')]), col = alpha(colors[edatP$distTristanBin], 0.3))
#points(mapply(c, edatP[,c('Lon', 'Lat')]), col = alpha(colors[edatP$distGoughBin], 0.1))
#text(x = c(tristanLon, goughLon), y = c(tristanLat, goughLat), labels = c('T', 'G'), col = 'white')

ggplot(aes(x = Lon, y = Lat), data = filter(edatP) +
  geom_raster(alpha = 0.5, fill = colors2[edatP$distTristanBin], interpolate = T)+
  geom_raster(aes(x = Lon, y = Lat), fill = colors2[edatP$distGoughBin], alpha =0.5, data = edatP)

##'Potential bin categories
##'0-250
##'0-500
##'0-1000
##'0-2000
##################################################################


################Aggregate means of environmental data#############

##Timing of different lifehistory periods - Cuthbert 2013 (penguins natural history and conservation)
IncubationMonths <- c(7,8,9,10)
ForagingMonthsLiberal <- c(4,5,6,7)
ForaginMonthsCore <- c(5,6)
BroodingMonths <- c(10,11,12,1)



###Foraging ranges from different lifehistory periods - from IUCN 2016 
IncubationDistT <- 800
IncubationDistG <- 670
ForagingDistT <- 2100
ForagingDistG <- 2100
BroodingDistT <- 35
BroodingDistG <- 24


for(row in 1:length(eggSizeNoOdd[,1])){
  year <- eggSizeNoOdd[row,'Year']
  location <- ifelse(eggSizeNoOdd[row,'Location'] == 'Gough', 'G', 'T')
  if(location == 'T'){
    #eggSizeNoOdd[row,'IncubationMeanSST'] <- mean(filter(edatS, Mon %in% IncubationMonths, Year == year, distTristan <= IncubationDistT)$M)
    #eggSizeNoOdd[row,'IncubationMeanSSP'] <- mean(filter(edatP, Mon %in% IncubationMonths, Year == year, distTristan <= IncubationDistT)$M)
    eggSizeNoOdd[row,'ForagingMeanSST_lib'] <- mean(filter(edatS, Mon %in% ForagingMonthsLiberal, Year == year, distTristan <= ForagingDistT)$M)
    eggSizeNoOdd[row,'ForagingMeanSSP_lib'] <- mean(filter(edatP, Mon %in% ForagingMonthsLiberal, Year == year, distTristan <= ForagingDistT)$M)
    eggSizeNoOdd[row,'ForagingMeanSST_cons'] <- mean(filter(edatS, Mon %in% ForagingMonthsLiberal, Year == year, distTristan <= ForagingDistT)$M)
    eggSizeNoOdd[row,'ForagingMeanSSP_cons'] <- mean(filter(edatP, Mon %in% ForagingMonthsLiberal, Year == year, distTristan <= ForagingDistT)$M)
  }else{
    #eggSizeNoOdd[row,'IncubationMeanSST'] <- mean(filter(edatS, Mon %in% IncubationMonths, Year == year, distGough <= IncubationDistG)$M)
    #eggSizeNoOdd[row,'IncubationMeanSSP'] <- mean(filter(edatP, Mon %in% IncubationMonths, Year == year, distGough <= IncubationDistG)$M)
    eggSizeNoOdd[row,'ForagingMeanSST'] <- mean(filter(edatS, Mon %in% ForagingMonths, Year == year, distGough <= ForagingDistG)$M)
    eggSizeNoOdd[row,'ForagingMeanSSP'] <- mean(filter(edatP, Mon %in% ForagingMonths, Year == year, distGough <= ForagingDistG)$M)
  }
}


##################################################################






