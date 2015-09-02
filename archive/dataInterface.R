library(xlsx)

#Population structure
ageGroupHeader <- seq(1,22,1)
urbanRuralGroups <- seq(1,2,1)

years <- rep(1989:2050, each = 1)
ageGroup <- rep(1:length(ageGroupHeader), each = length(years))
maleUrbanPopulation <- rep(1:length(ageGroupHeader), each = length(years))
maleRuralPopulation <- rep(1:length(ageGroupHeader), each = length(years))
femaleUrbanPopulation <- rep(1:length(ageGroupHeader), each = length(years))
femaleRuralPopulation <- rep(1:length(ageGroupHeader), each = length(years))

populationsRF_DF<-data.frame(years, ageGroup, maleUrbanPopulation, maleRuralPopulation, femaleUrbanPopulation, femaleRuralPopulation)

#Filling data from file by years, urban/rural, male/female
populationRF_r_urban <- read.xlsx(paste(getwd(),"/RFData/1.5.xls", sep = ""), sheetIndex = 1, startRow = 36, endRow = 57, header = F)
populationsRF_DF[populationsRF_DF$years==1989,]$maleUrbanPopulation <- populationRF_r_urban[["X3"]]
populationsRF_DF[populationsRF_DF$years==1989,]$femaleUrbanPopulation <- populationRF_r_urban[["X4"]]

populationsRF_DF[populationsRF_DF$years==2002,]$maleUrbanPopulation <- populationRF_r_urban[["X6"]]
populationsRF_DF[populationsRF_DF$years==2002,]$femaleUrbanPopulation <- populationRF_r_urban[["X7"]]

populationsRF_DF[populationsRF_DF$years==2010,]$maleUrbanPopulation <- populationRF_r_urban[["X9"]]
populationsRF_DF[populationsRF_DF$years==2010,]$femaleUrbanPopulation <- populationRF_r_urban[["X10"]]

populationsRF_DF[populationsRF_DF$years==2014,]$maleUrbanPopulation <- populationRF_r_urban[["X12"]]
populationsRF_DF[populationsRF_DF$years==2014,]$femaleUrbanPopulation <- populationRF_r_urban[["X13"]]
# populationsRF_DF[populationsRF_DF$years==2010, ]

populationRF_r_rural <- read.xlsx(paste(getwd(),"/RFData/1.5.xls", sep = ""), sheetIndex = 1, startRow = 64, endRow = 85, header = F)
populationsRF_DF[populationsRF_DF$years==1989,]$maleRuralPopulation <- populationRF_r_rural[["X3"]]
populationsRF_DF[populationsRF_DF$years==1989,]$femaleRuralPopulation <- populationRF_r_rural[["X4"]]

populationsRF_DF[populationsRF_DF$years==2002,]$maleRuralPopulation <- populationRF_r_rural[["X6"]]
populationsRF_DF[populationsRF_DF$years==2002,]$femaleRuralPopulation <- populationRF_r_rural[["X7"]]

populationsRF_DF[populationsRF_DF$years==2010,]$maleRuralPopulation <- populationRF_r_rural[["X9"]]
populationsRF_DF[populationsRF_DF$years==2010,]$femaleRuralPopulation <- populationRF_r_rural[["X10"]]

populationsRF_DF[populationsRF_DF$years==2014,]$maleRuralPopulation <- populationRF_r_rural[["X12"]]
populationsRF_DF[populationsRF_DF$years==2014,]$femaleRuralPopulation <- populationRF_r_rural[["X13"]]

#Koefs structure
#Birth
birthAgeGroupHeader <- seq(1,8,1)
birthAgeGroup <- rep(1:length(birthAgeGroupHeader), each = length(years))
femaleUrbanBirth <- rep(1:length(birthAgeGroupHeader), each = length(years))
femaleRuralBirth <- rep(1:length(birthAgeGroupHeader), each = length(years))

birthKoefs_DF <- data.frame(years, birthAgeGroup, femaleUrbanBirth, femaleRuralBirth)

birthKoefs_r_urban <- read.xlsx(paste(getwd(),"/RFData/4.3.xls", sep = ""), sheetIndex = 1, startRow = 42, endRow = 61, header = F)
birthKoefs_r_urban <- as.data.frame(t(birthKoefs_r_urban))
colnames(birthKoefs_r_urban) <- birthKoefs_r_urban[1, ]
birthKoefs_r_urban <- birthKoefs_r_urban[-1, ]

for(year in names(birthKoefs_r_urban)){
  birthKoefs_DF[birthKoefs_DF$years==year,]$femaleUrbanBirth <- birthKoefs_r_urban[[paste("",year,sep = "")]]
  birthKoefs_DF[birthKoefs_DF$years==year,]$femaleUrbanBirth[length(birthAgeGroupHeader)] <- 0
}
# birthKoefs_DF[birthKoefs_DF$years==2013,]$femaleUrbanBirth

birthKoefs_r_rural <- read.xlsx(paste(getwd(),"/RFData/4.3.xls", sep = ""), sheetIndex = 1, startRow = 70, endRow = 89, header = F)
birthKoefs_r_rural <- as.data.frame(t(birthKoefs_r_rural))
colnames(birthKoefs_r_rural) <- birthKoefs_r_rural[1, ]
birthKoefs_r_rural <- birthKoefs_r_rural[-1, ]

for(year in names(birthKoefs_r_rural)){
  birthKoefs_DF[birthKoefs_DF$years==year,]$femaleRuralBirth <- birthKoefs_r_rural[[paste("",year,sep = "")]]
  birthKoefs_DF[birthKoefs_DF$years==year,]$femaleRuralBirth[length(birthAgeGroupHeader)] <- 0
}
# birthKoefs_DF[birthKoefs_DF$years==2013,]$femaleRuralBirth

#Death
deathAgeGroupHeader <- seq(1,19,1)
deathAgeGroup <- rep(1:length(deathAgeGroupHeader), each = length(years))

maleUrbanDeath <- rep(1:length(deathAgeGroupHeader), each = length(years))
maleRuralDeath <- rep(1:length(deathAgeGroupHeader), each = length(years))
femaleUrbanDeath <- rep(1:length(deathAgeGroupHeader), each = length(years))
femaleRuralDeath <- rep(1:length(deathAgeGroupHeader), each = length(years))

deathKoefs_DF <- data.frame(years, deathAgeGroup, maleUrbanDeath, maleRuralDeath, femaleUrbanDeath, femaleRuralDeath)

deathKoefs_r_urban <- read.xlsx(paste(getwd(),"/RFData/5.2.xls", sep = ""), sheetIndex = 1, startRow = 33, endRow = 51, header = F)
deathKoefs_DF[deathKoefs_DF$years==2011,]$maleUrbanDeath <- deathKoefs_r_urban[["X5"]]
deathKoefs_DF[deathKoefs_DF$years==2012,]$maleUrbanDeath <- deathKoefs_r_urban[["X6"]]
deathKoefs_DF[deathKoefs_DF$years==2013,]$maleUrbanDeath <- deathKoefs_r_urban[["X7"]]

deathKoefs_DF[deathKoefs_DF$years==2011,]$femaleUrbanDeath <- deathKoefs_r_urban[["X8"]]
deathKoefs_DF[deathKoefs_DF$years==2012,]$femaleUrbanDeath <- deathKoefs_r_urban[["X9"]]
deathKoefs_DF[deathKoefs_DF$years==2013,]$femaleUrbanDeath <- deathKoefs_r_urban[["X10"]]

deathKoefs_r_rural <- read.xlsx(paste(getwd(),"/RFData/5.2.xls", sep = ""), sheetIndex = 1, startRow = 55, endRow = 73, header = F)
deathKoefs_DF[deathKoefs_DF$years==2011,]$maleRuralDeath <- deathKoefs_r_rural[["X5"]]
deathKoefs_DF[deathKoefs_DF$years==2012,]$maleRuralDeath <- deathKoefs_r_rural[["X6"]]
deathKoefs_DF[deathKoefs_DF$years==2013,]$maleRuralDeath <- deathKoefs_r_rural[["X7"]]

deathKoefs_DF[deathKoefs_DF$years==2011,]$femaleRuralDeath <- deathKoefs_r_rural[["X8"]]
deathKoefs_DF[deathKoefs_DF$years==2012,]$femaleRuralDeath <- deathKoefs_r_rural[["X9"]]
deathKoefs_DF[deathKoefs_DF$years==2013,]$femaleRuralDeath <- deathKoefs_r_rural[["X10"]]

#remove all unused vars
rm(list=ls()[! ls() %in% c("deathKoefs_DF","birthKoefs_DF", "populationsRF_DF")])

RunSimulation <- function(populationsRF_DF, birthKoefs_DF, deathKoefs_DF){
  for(currentFutureYear in seq(2015,2050,1)){
    #Birth
    newbieUrban <- 0
    newbieRural <- 0
    currentPopulation <- populationsRF_DF[populationsRF_DF$years == currentFutureYear-1,]
    futurePopulation <- populationsRF_DF[populationsRF_DF$years == currentFutureYear,]
    
    for(birthGroup in seq(1,7,1)){
      newbieUrban <- newbieUrban + currentPopulation$femaleUrbanPopulation[birthGroup+7]/1000*birthKoefs_DF[birthKoefs_DF$years==currentFutureYear-1,]$femaleUrbanBirth[birthGroup]
    }
    for(birthGroup in seq(1,7,1)){
      newbieRural <- newbieRural + currentPopulation$femaleRuralPopulation[birthGroup+7]/1000*birthKoefs_DF[birthKoefs_DF$years==currentFutureYear-1,]$femaleRuralBirth[birthGroup]
    }
    
    populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == 1),]$maleUrbanPopulation <- round(newbieUrban/2,0)
    populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == 1),]$femaleUrbanPopulation <- round(newbieUrban/2,0)
    populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == 1),]$maleRuralPopulation <- round(newbieRural/2,0)
    populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == 1),]$femaleRuralPopulation <- round(newbieRural/2,0)
    
    #Death
    for(populGroup in seq(1,22,1)){
      if(populGroup == 1){
        populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$maleUrbanPopulation <- currentPopulation[currentPopulation$ageGroup == populGroup,]$maleUrbanPopulation - round(currentPopulation[currentPopulation$ageGroup == populGroup,]$maleUrbanPopulation/1000 * deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$maleUrbanDeath[1],0)
        populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$femaleUrbanPopulation <- currentPopulation[currentPopulation$ageGroup == populGroup,]$femaleUrbanPopulation - round(currentPopulation[currentPopulation$ageGroup == populGroup,]$femaleUrbanPopulation/1000 * deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$femaleUrbanDeath[1],0)
        populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$maleRuralPopulation <- currentPopulation[currentPopulation$ageGroup == populGroup,]$maleRuralPopulation - round(currentPopulation[currentPopulation$ageGroup == populGroup,]$maleRuralPopulation/1000 * deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$maleRuralDeath[1],0)
        populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$femaleRuralPopulation <- currentPopulation[currentPopulation$ageGroup == populGroup,]$femaleRuralPopulation - round(currentPopulation[currentPopulation$ageGroup == populGroup,]$femaleRuralPopulation/1000 * deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$femaleRuralDeath[1],0)
      }
      if(populGroup < 6 & populGroup > 1){
        populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$maleUrbanPopulation <- currentPopulation[currentPopulation$ageGroup == populGroup,]$maleUrbanPopulation - round(currentPopulation[currentPopulation$ageGroup == populGroup,]$maleUrbanPopulation/1000 * deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$maleUrbanDeath[2]/4,0)
        populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$femaleUrbanPopulation <- currentPopulation[currentPopulation$ageGroup == populGroup,]$femaleUrbanPopulation - round(currentPopulation[currentPopulation$ageGroup == populGroup,]$femaleUrbanPopulation/1000 * deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$femaleUrbanDeath[2]/4,0)
        populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$maleRuralPopulation <- currentPopulation[currentPopulation$ageGroup == populGroup,]$maleRuralPopulation - round(currentPopulation[currentPopulation$ageGroup == populGroup,]$maleRuralPopulation/1000 * deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$maleRuralDeath[2]/4,0)
        populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$femaleRuralPopulation <- currentPopulation[currentPopulation$ageGroup == populGroup,]$femaleRuralPopulation - round(currentPopulation[currentPopulation$ageGroup == populGroup,]$femaleRuralPopulation/1000 * deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$femaleRuralDeath[2]/4,0)
      }
      if(populGroup >= 6){
        populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$maleUrbanPopulation <- currentPopulation[currentPopulation$ageGroup == populGroup,]$maleUrbanPopulation - round(currentPopulation[currentPopulation$ageGroup == populGroup,]$maleUrbanPopulation/1000 * deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$maleUrbanDeath[populGroup-3],0)
        populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$femaleUrbanPopulation <- currentPopulation[currentPopulation$ageGroup == populGroup,]$femaleUrbanPopulation - round(currentPopulation[currentPopulation$ageGroup == populGroup,]$femaleUrbanPopulation/1000 * deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$femaleUrbanDeath[populGroup-3],0)
        populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$maleRuralPopulation <- currentPopulation[currentPopulation$ageGroup == populGroup,]$maleRuralPopulation - round(currentPopulation[currentPopulation$ageGroup == populGroup,]$maleRuralPopulation/1000 * deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$maleRuralDeath[populGroup-3],0)
        populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$femaleRuralPopulation <- currentPopulation[currentPopulation$ageGroup == populGroup,]$femaleRuralPopulation - round(currentPopulation[currentPopulation$ageGroup == populGroup,]$femaleRuralPopulation/1000 * deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$femaleRuralDeath[populGroup-3],0)
      }
    }
    
  }
}

RunFillDynamics <- function(birthKoefs_DF, deathKoefs_DF){
  for(currentFutureYear in seq(2014,2050,1)){
    birthKoefs_DF[birthKoefs_DF$years==currentFutureYear,]$femaleUrbanBirth <- birthKoefs_DF[birthKoefs_DF$years==currentFutureYear-1,]$femaleUrbanBirth
    birthKoefs_DF[birthKoefs_DF$years==currentFutureYear,]$femaleRuralBirth <- birthKoefs_DF[birthKoefs_DF$years==currentFutureYear-1,]$femaleRuralBirth
  }
  
  for(currentFutureYear in seq(2014,2050,1)){
    deathKoefs_DF[deathKoefs_DF$years==currentFutureYear,]$maleUrbanDeath <- deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$maleUrbanDeath
    deathKoefs_DF[deathKoefs_DF$years==currentFutureYear,]$femaleUrbanDeath <- deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$femaleUrbanDeath
    deathKoefs_DF[deathKoefs_DF$years==currentFutureYear,]$maleRuralDeath <- deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$maleRuralDeath
    deathKoefs_DF[deathKoefs_DF$years==currentFutureYear,]$femaleRuralDeath <- deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$femaleRuralDeath
  }
}

RunFillDynamics(birthKoefs_DF, deathKoefs_DF)
RunSimulation(populationsRF_DF, birthKoefs_DF, deathKoefs_DF)



