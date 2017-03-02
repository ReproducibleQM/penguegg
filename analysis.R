##################################################################
##                      Penguegg group                          ##
##      Changes rockhopper penguin in egg size over time        ##
##                        RQM Course                            ##
##################################################################

library(dplyr)
library(ggplot2)
library(geosphere)

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

########################CLassify Oddballs#########################
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
##################################################################
#eggSizeNoOdd <- filter(eggSizeCleaned, EggOrder != 'ODDBALL')
eggSizeNoOdd <- filter(eggSizeCleaned, EggOrder %in% c('A', 'B'))
eggSizeNoOdd <- eggSizeNoOdd[complete.cases(eggSizeNoOdd),]
m0 <- lm(Volume ~ EggOrder, data = eggSizeNoOdd)


########################Volume ~ Month############################
summary(lm(Volume ~ Month, data = eggSizeNoOdd))
m1 <- lm(Volume ~ Month, data = eggSizeNoOdd)
##without high leverage data point
summary(lm(Volume ~ Month, data = eggSizeNoOdd[-936,]))

##split by Egg Order
summary(lm(Volume ~ Month, data = filter(eggSizeNoOdd, EggOrder == 'B'))) ## p = .00105
summary(lm(Volume ~ Month, data = filter(eggSizeNoOdd, EggOrder == 'A'))) ##n.s
##################################################################


########################Volume ~ Year#############################
m2 <- lm(Volume ~ Year, data = eggSizeNoOdd)
boxplot(data = eggSizeNoOdd, Volume ~ Year)

##Split by egg order
summary(lm(Volume ~ Year, data = filter(eggSizeNoOdd, EggOrder == 'B'))) ##n.s
summary(lm(Volume ~ Year, data = filter(eggSizeNoOdd, EggOrder == 'A'))) ##p = .00699
##################################################################


######################Volume ~ Location###########################
m3 <- lm(Volume ~ Location, data = eggSizeNoOdd)
boxplot(Volume ~ Location, eggSizeNoOdd[eggSizeNoOdd$EggOrder == 'A',], main = 'A')
boxplot(Volume ~ Location, eggSizeNoOdd[eggSizeNoOdd$EggOrder == 'B',], main = 'B')
summary(lm(Volume ~  Location, data = filter(eggSizeNoOdd, EggOrder == 'B')))
summary(lm(Volume ~  Location, data = filter(eggSizeNoOdd, EggOrder == 'A')))

ggplot()
##################################################################

######################Multiple Regression#########################
m4 <- lm(Volume ~ EggOrder + Month, data = eggSizeNoOdd)
m5 <- lm(Volume ~ EggOrder * Month, data = eggSizeNoOdd)
m6 <- lm(Volume ~ EggOrder + Year, data = eggSizeNoOdd)
m7 <- lm(Volume ~ EggOrder * Year, data = eggSizeNoOdd)
m8 <- lm(Volume ~ EggOrder + Location, data = eggSizeNoOdd)
m9 <- lm(Volume ~ EggOrder * Location, data = eggSizeNoOdd)
m10 <- lm(Volume ~ EggOrder * Month + Location, data = eggSizeNoOdd)
m11 <- lm(Volume ~ EggOrder * Year + Location, data = eggSizeNoOdd)
m12 <- lm(Volume ~ EggOrder + Location + Year, data = eggSizeNoOdd)
m13 <- lm(Volume ~ EggOrder + Location + Year + Month, data = eggSizeNoOdd)
m14 <- lm(Volume ~ EggOrder*Month + Location + Year*EggOrder, data = eggSizeNoOdd)
##################################################################

########################Model comparison##########################
aic <- AIC(m0, m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14)
aic[order(aic$AIC),]
##################################################################


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


edatP$distTristan <- distGeo(c(tristanLon, tristanLat), mapply(c, edatP[,c('Lon', 'Lat')]))
edatP$distGough <- distGeo(c(goughLon, goughLat), mapply(c, edatP[,c('Lon', 'Lat')]))



edatS$Lon <- edatS$BLO + edatS$X
edatS$Lat <- edatS$BLA + edatS$Y
edatS[edatS$Lon > 180,'Lon'] <- edatS[edatS$Lon > 180,'Lon'] - 360


edatS$distTristan <- distGeo(c(tristanLon, tristanLat), mapply(c, edatS[,c('Lon', 'Lat')]))
edatS$distGough <- distGeo(c(goughLon, goughLat), mapply(c, edatS[,c('Lon', 'Lat')]))

plot(mapply(c, edatP[,c('Lon', 'Lat')]))
text(x = c(tristanLon, goughLon), y = c(tristanLat, goughLat), labels = c('T', 'G'), col = 'red')

##'Potential bin categories
##'0-250
##'0-500
##'0-1000
##'0-2000
##################################################################




