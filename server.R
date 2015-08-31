library(shiny)
library(rCharts)
library(googleVis)
library(ggplot2)
library(gdata)
library(plyr)
library(xts)
options(shiny.error=traceback)


shinyServer(function(input, output, session) {
  
#   years <- rep(1989:2050, each = 1)
#   ageGroupHeader <- seq(1,22,1)
#   urbanRuralGroups <- seq(1,2,1)
#   birthAgeGroupHeader <- seq(1,8,1)
#   deathAgeGroupHeader <- seq(1,19,1)
#   
#   populationsRF_DF<-data.frame(years,
#                                ageGroup = rep(1:length(ageGroupHeader), each = length(years)),
#                                maleUrbanPopulation = rep(1:length(ageGroupHeader), each = length(years)), 
#                                maleRuralPopulation = rep(1:length(ageGroupHeader), each = length(years)), 
#                                femaleUrbanPopulation = rep(1:length(ageGroupHeader), each = length(years)), 
#                                femaleRuralPopulation = rep(1:length(ageGroupHeader), each = length(years))
#   )
#   
#   birthKoefs_DF <- data.frame(years, 
#                               birthAgeGroup = rep(1:length(birthAgeGroupHeader), each = length(years)),
#                               femaleUrbanBirth = rep(1:length(birthAgeGroupHeader), each = length(years)), 
#                               femaleRuralBirth = rep(1:length(birthAgeGroupHeader), each = length(years))
#   )
#   
#   deathKoefs_DF <- data.frame(years, 
#                               deathAgeGroup = rep(1:length(deathAgeGroupHeader), each = length(years)), 
#                               maleUrbanDeath = rep(1:length(deathAgeGroupHeader), each = length(years)),
#                               maleRuralDeath = rep(1:length(deathAgeGroupHeader), each = length(years)),
#                               femaleUrbanDeath = rep(1:length(deathAgeGroupHeader), each = length(years)),
#                               femaleRuralDeath = rep(1:length(deathAgeGroupHeader), each = length(years))
#   )
#   
#   list <- structure(NA,class="result")
#   "[<-.result" <- function(x,...,value) {
#     args <- as.list(match.call())
#     args <- args[-c(1:2,length(args))]
#     length(value) <- length(args)
#     for(i in seq(along=args)) {
#       a <- args[[i]]
#       if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
#     }
#     x
#   }
#   
#   
#   readData <- function(populationsRF_DF){
#     library(xlsx)
#     #Filling data from file by years, urban/rural, male/female
#     populationRF_r_urban <- read.xlsx(paste(getwd(),"/RFData/1.5.xls", sep = ""), sheetIndex = 1, startRow = 36, endRow = 57, header = F)
#     populationsRF_DF[populationsRF_DF$years==1989,]$maleUrbanPopulation <- populationRF_r_urban[["X3"]]
#     populationsRF_DF[populationsRF_DF$years==1989,]$femaleUrbanPopulation <- populationRF_r_urban[["X4"]]
#     
#     populationsRF_DF[populationsRF_DF$years==2002,]$maleUrbanPopulation <- populationRF_r_urban[["X6"]]
#     populationsRF_DF[populationsRF_DF$years==2002,]$femaleUrbanPopulation <- populationRF_r_urban[["X7"]]
#     
#     populationsRF_DF[populationsRF_DF$years==2010,]$maleUrbanPopulation <- populationRF_r_urban[["X9"]]
#     populationsRF_DF[populationsRF_DF$years==2010,]$femaleUrbanPopulation <- populationRF_r_urban[["X10"]]
#     
#     populationsRF_DF[populationsRF_DF$years==2014,]$maleUrbanPopulation <- populationRF_r_urban[["X12"]]
#     populationsRF_DF[populationsRF_DF$years==2014,]$femaleUrbanPopulation <- populationRF_r_urban[["X13"]]
#     # populationsRF_DF[populationsRF_DF$years==2010, ]
#     
#     populationRF_r_rural <- read.xlsx(paste(getwd(),"/RFData/1.5.xls", sep = ""), sheetIndex = 1, startRow = 64, endRow = 85, header = F)
#     populationsRF_DF[populationsRF_DF$years==1989,]$maleRuralPopulation <- populationRF_r_rural[["X3"]]
#     populationsRF_DF[populationsRF_DF$years==1989,]$femaleRuralPopulation <- populationRF_r_rural[["X4"]]
#     
#     populationsRF_DF[populationsRF_DF$years==2002,]$maleRuralPopulation <- populationRF_r_rural[["X6"]]
#     populationsRF_DF[populationsRF_DF$years==2002,]$femaleRuralPopulation <- populationRF_r_rural[["X7"]]
#     
#     populationsRF_DF[populationsRF_DF$years==2010,]$maleRuralPopulation <- populationRF_r_rural[["X9"]]
#     populationsRF_DF[populationsRF_DF$years==2010,]$femaleRuralPopulation <- populationRF_r_rural[["X10"]]
#     
#     populationsRF_DF[populationsRF_DF$years==2014,]$maleRuralPopulation <- populationRF_r_rural[["X12"]]
#     populationsRF_DF[populationsRF_DF$years==2014,]$femaleRuralPopulation <- populationRF_r_rural[["X13"]] 
#     return(populationsRF_DF)
#   }
#   
#   readKoefs <- function(birthKoefs_DF, deathKoefs_DF){
#     #Koefs structure
#     #Birth
#     birthKoefs_r_urban <- read.xlsx(paste(getwd(),"/RFData/4.3.xls", sep = ""), sheetIndex = 1, startRow = 42, endRow = 61, header = F)
#     birthKoefs_r_urban <- as.data.frame(t(birthKoefs_r_urban))
#     colnames(birthKoefs_r_urban) <- birthKoefs_r_urban[1, ]
#     birthKoefs_r_urban <- birthKoefs_r_urban[-1, ]
#     
#     for(year in names(birthKoefs_r_urban)){
#       birthKoefs_DF[birthKoefs_DF$years==year,]$femaleUrbanBirth <- birthKoefs_r_urban[[paste("",year,sep = "")]]
#       birthKoefs_DF[birthKoefs_DF$years==year,]$femaleUrbanBirth[length(birthAgeGroupHeader)] <- 0
#     }
#     # birthKoefs_DF[birthKoefs_DF$years==2013,]$femaleUrbanBirth
#     
#     birthKoefs_r_rural <- read.xlsx(paste(getwd(),"/RFData/4.3.xls", sep = ""), sheetIndex = 1, startRow = 70, endRow = 89, header = F)
#     birthKoefs_r_rural <- as.data.frame(t(birthKoefs_r_rural))
#     colnames(birthKoefs_r_rural) <- birthKoefs_r_rural[1, ]
#     birthKoefs_r_rural <- birthKoefs_r_rural[-1, ]
#     
#     for(year in names(birthKoefs_r_rural)){
#       birthKoefs_DF[birthKoefs_DF$years==year,]$femaleRuralBirth <- birthKoefs_r_rural[[paste("",year,sep = "")]]
#       birthKoefs_DF[birthKoefs_DF$years==year,]$femaleRuralBirth[length(birthAgeGroupHeader)] <- 0
#     }
#     # birthKoefs_DF[birthKoefs_DF$years==2013,]$femaleRuralBirth
#     
#     #Death
#     deathKoefs_r_urban <- read.xlsx(paste(getwd(),"/RFData/5.2.xls", sep = ""), sheetIndex = 1, startRow = 33, endRow = 51, header = F)
#     deathKoefs_DF[deathKoefs_DF$years==2011,]$maleUrbanDeath <- deathKoefs_r_urban[["X5"]]
#     deathKoefs_DF[deathKoefs_DF$years==2012,]$maleUrbanDeath <- deathKoefs_r_urban[["X6"]]
#     deathKoefs_DF[deathKoefs_DF$years==2013,]$maleUrbanDeath <- deathKoefs_r_urban[["X7"]]
#     
#     deathKoefs_DF[deathKoefs_DF$years==2011,]$femaleUrbanDeath <- deathKoefs_r_urban[["X8"]]
#     deathKoefs_DF[deathKoefs_DF$years==2012,]$femaleUrbanDeath <- deathKoefs_r_urban[["X9"]]
#     deathKoefs_DF[deathKoefs_DF$years==2013,]$femaleUrbanDeath <- deathKoefs_r_urban[["X10"]]
#     
#     deathKoefs_r_rural <- read.xlsx(paste(getwd(),"/RFData/5.2.xls", sep = ""), sheetIndex = 1, startRow = 55, endRow = 73, header = F)
#     deathKoefs_DF[deathKoefs_DF$years==2011,]$maleRuralDeath <- deathKoefs_r_rural[["X5"]]
#     deathKoefs_DF[deathKoefs_DF$years==2012,]$maleRuralDeath <- deathKoefs_r_rural[["X6"]]
#     deathKoefs_DF[deathKoefs_DF$years==2013,]$maleRuralDeath <- deathKoefs_r_rural[["X7"]]
#     
#     deathKoefs_DF[deathKoefs_DF$years==2011,]$femaleRuralDeath <- deathKoefs_r_rural[["X8"]]
#     deathKoefs_DF[deathKoefs_DF$years==2012,]$femaleRuralDeath <- deathKoefs_r_rural[["X9"]]
#     deathKoefs_DF[deathKoefs_DF$years==2013,]$femaleRuralDeath <- deathKoefs_r_rural[["X10"]]
#     
#     return(list(birthKoefs_DF, deathKoefs_DF))
#     #remove all unused vars
#     #   rm(list=ls()[! ls() %in% c("deathKoefs_DF","birthKoefs_DF", "populationsRF_DF")])
#   }
#   
#   koefsCorrect <- function(birthKoefs_DF, deathKoefs_DF, yearBegins, yearEnds){
#     #Fill koefs
#     for(currentFutureYear in seq(yearBegins,yearEnds,1)){
#       birthKoefs_DF[birthKoefs_DF$years==currentFutureYear,]$femaleUrbanBirth <- birthKoefs_DF[birthKoefs_DF$years==currentFutureYear-1,]$femaleUrbanBirth
#       birthKoefs_DF[birthKoefs_DF$years==currentFutureYear,]$femaleRuralBirth <- birthKoefs_DF[birthKoefs_DF$years==currentFutureYear-1,]$femaleRuralBirth
#     }
#     
#     for(currentFutureYear in seq(yearBegins,yearEnds,1)){
#       deathKoefs_DF[deathKoefs_DF$years==currentFutureYear,]$maleUrbanDeath <- deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$maleUrbanDeath
#       deathKoefs_DF[deathKoefs_DF$years==currentFutureYear,]$femaleUrbanDeath <- deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$femaleUrbanDeath
#       deathKoefs_DF[deathKoefs_DF$years==currentFutureYear,]$maleRuralDeath <- deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$maleRuralDeath
#       deathKoefs_DF[deathKoefs_DF$years==currentFutureYear,]$femaleRuralDeath <- deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$femaleRuralDeath
#     }
#     return(list(birthKoefs_DF, deathKoefs_DF))
#   }
#   
#   simulateRF <- function(populationsRF_DF, yearBegins, yearEnds){
#     #simulate demography dynamics
#     for(currentFutureYear in seq(yearBegins,yearEnds,1)){
#       #Birth
#       newbieUrban <- 0
#       newbieRural <- 0
#       currentPopulation <- populationsRF_DF[populationsRF_DF$years == currentFutureYear-1,]
#       futurePopulation <- populationsRF_DF[populationsRF_DF$years == currentFutureYear,]
#       
#       for(birthGroup in seq(1,7,1)){
#         newbieUrban <- newbieUrban + currentPopulation$femaleUrbanPopulation[birthGroup+7]/1000*birthKoefs_DF[birthKoefs_DF$years==currentFutureYear-1,]$femaleUrbanBirth[birthGroup]
#       }
#       for(birthGroup in seq(1,7,1)){
#         newbieRural <- newbieRural + currentPopulation$femaleRuralPopulation[birthGroup+7]/1000*birthKoefs_DF[birthKoefs_DF$years==currentFutureYear-1,]$femaleRuralBirth[birthGroup]
#       }
#       
#       populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == 1),]$maleUrbanPopulation <- round(newbieUrban/2,0)
#       populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == 1),]$femaleUrbanPopulation <- round(newbieUrban/2,0)
#       populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == 1),]$maleRuralPopulation <- round(newbieRural/2,0)
#       populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == 1),]$femaleRuralPopulation <- round(newbieRural/2,0)
#       
#       #Death
#       for(populGroup in seq(1,22,1)){
#         if(populGroup == 1){
#           populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$maleUrbanPopulation <- currentPopulation[currentPopulation$ageGroup == populGroup,]$maleUrbanPopulation - round(currentPopulation[currentPopulation$ageGroup == populGroup,]$maleUrbanPopulation/1000 * deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$maleUrbanDeath[1],0)
#           populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$femaleUrbanPopulation <- currentPopulation[currentPopulation$ageGroup == populGroup,]$femaleUrbanPopulation - round(currentPopulation[currentPopulation$ageGroup == populGroup,]$femaleUrbanPopulation/1000 * deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$femaleUrbanDeath[1],0)
#           populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$maleRuralPopulation <- currentPopulation[currentPopulation$ageGroup == populGroup,]$maleRuralPopulation - round(currentPopulation[currentPopulation$ageGroup == populGroup,]$maleRuralPopulation/1000 * deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$maleRuralDeath[1],0)
#           populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$femaleRuralPopulation <- currentPopulation[currentPopulation$ageGroup == populGroup,]$femaleRuralPopulation - round(currentPopulation[currentPopulation$ageGroup == populGroup,]$femaleRuralPopulation/1000 * deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$femaleRuralDeath[1],0)
#         }
#         if(populGroup < 6 & populGroup > 1){
#           populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$maleUrbanPopulation <- currentPopulation[currentPopulation$ageGroup == populGroup,]$maleUrbanPopulation - round(currentPopulation[currentPopulation$ageGroup == populGroup,]$maleUrbanPopulation/1000 * deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$maleUrbanDeath[2]/4,0)
#           populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$femaleUrbanPopulation <- currentPopulation[currentPopulation$ageGroup == populGroup,]$femaleUrbanPopulation - round(currentPopulation[currentPopulation$ageGroup == populGroup,]$femaleUrbanPopulation/1000 * deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$femaleUrbanDeath[2]/4,0)
#           populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$maleRuralPopulation <- currentPopulation[currentPopulation$ageGroup == populGroup,]$maleRuralPopulation - round(currentPopulation[currentPopulation$ageGroup == populGroup,]$maleRuralPopulation/1000 * deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$maleRuralDeath[2]/4,0)
#           populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$femaleRuralPopulation <- currentPopulation[currentPopulation$ageGroup == populGroup,]$femaleRuralPopulation - round(currentPopulation[currentPopulation$ageGroup == populGroup,]$femaleRuralPopulation/1000 * deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$femaleRuralDeath[2]/4,0)
#         }
#         if(populGroup >= 6){
#           populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$maleUrbanPopulation <- currentPopulation[currentPopulation$ageGroup == populGroup,]$maleUrbanPopulation - round(currentPopulation[currentPopulation$ageGroup == populGroup,]$maleUrbanPopulation/1000 * deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$maleUrbanDeath[populGroup-3],0)
#           populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$femaleUrbanPopulation <- currentPopulation[currentPopulation$ageGroup == populGroup,]$femaleUrbanPopulation - round(currentPopulation[currentPopulation$ageGroup == populGroup,]$femaleUrbanPopulation/1000 * deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$femaleUrbanDeath[populGroup-3],0)
#           populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$maleRuralPopulation <- currentPopulation[currentPopulation$ageGroup == populGroup,]$maleRuralPopulation - round(currentPopulation[currentPopulation$ageGroup == populGroup,]$maleRuralPopulation/1000 * deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$maleRuralDeath[populGroup-3],0)
#           populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$femaleRuralPopulation <- currentPopulation[currentPopulation$ageGroup == populGroup,]$femaleRuralPopulation - round(currentPopulation[currentPopulation$ageGroup == populGroup,]$femaleRuralPopulation/1000 * deathKoefs_DF[deathKoefs_DF$years==currentFutureYear-1,]$femaleRuralDeath[populGroup-3],0)
#         }
#       }
#       
#       #Growth
#       for(populGroup in seq(2,5,1)){
#         container <- populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$maleUrbanPopulation
#         populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$maleUrbanPopulation <- populationsRF_DF[which(populationsRF_DF$years == currentFutureYear-1 & populationsRF_DF$ageGroup == populGroup-1),]$maleUrbanPopulation
#         populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$femaleUrbanPopulation <- populationsRF_DF[which(populationsRF_DF$years == currentFutureYear-1 & populationsRF_DF$ageGroup == populGroup-1),]$femaleUrbanPopulation
#         populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$maleRuralPopulation <- populationsRF_DF[which(populationsRF_DF$years == currentFutureYear-1 & populationsRF_DF$ageGroup == populGroup-1),]$maleRuralPopulation
#         populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$femaleRuralPopulation <- populationsRF_DF[which(populationsRF_DF$years == currentFutureYear-1 & populationsRF_DF$ageGroup == populGroup-1),]$femaleRuralPopulation
#       }
#       
#       populGroup <- 6
#       a <- populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$maleUrbanPopulation
#       b <- populationsRF_DF[which(populationsRF_DF$years == currentFutureYear-1 & populationsRF_DF$ageGroup == populGroup-1),]$maleUrbanPopulation
#       populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$maleUrbanPopulation <- a-a/5+b
#       a <- populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$femaleUrbanPopulation
#       b <- populationsRF_DF[which(populationsRF_DF$years == currentFutureYear-1 & populationsRF_DF$ageGroup == populGroup-1),]$femaleUrbanPopulation
#       populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$femaleUrbanPopulation <- a-a/5+b
#       a <- populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$maleRuralPopulation
#       b <- populationsRF_DF[which(populationsRF_DF$years == currentFutureYear-1 & populationsRF_DF$ageGroup == populGroup-1),]$maleRuralPopulation
#       populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$maleRuralPopulation <- a-a/5+b
#       a <- populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$femaleRuralPopulation
#       b <- populationsRF_DF[which(populationsRF_DF$years == currentFutureYear-1 & populationsRF_DF$ageGroup == populGroup-1),]$femaleRuralPopulation
#       populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$femaleRuralPopulation <- a-a/5+b
#       
#       for(populGroup in seq(7,22,1)){
#         container <- populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$maleUrbanPopulation
#         a <- populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$maleUrbanPopulation
#         b <- populationsRF_DF[which(populationsRF_DF$years == currentFutureYear-1 & populationsRF_DF$ageGroup == populGroup-1),]$maleUrbanPopulation
#         populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$maleUrbanPopulation <- a-a/5+b/5
#         
#         a <- populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$femaleUrbanPopulation
#         b <- populationsRF_DF[which(populationsRF_DF$years == currentFutureYear-1 & populationsRF_DF$ageGroup == populGroup-1),]$femaleUrbanPopulation
#         populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$femaleUrbanPopulation <- a-a/5+b/5
#         
#         a <- populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$maleRuralPopulation
#         b <- populationsRF_DF[which(populationsRF_DF$years == currentFutureYear-1 & populationsRF_DF$ageGroup == populGroup-1),]$maleRuralPopulation
#         populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$maleRuralPopulation <- a-a/5+b/5
#         
#         a <- populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$femaleRuralPopulation
#         b <- populationsRF_DF[which(populationsRF_DF$years == currentFutureYear-1 & populationsRF_DF$ageGroup == populGroup-1),]$femaleRuralPopulation
#         populationsRF_DF[which(populationsRF_DF$years == currentFutureYear & populationsRF_DF$ageGroup == populGroup),]$femaleRuralPopulation <- a-a/5+b/5
#       }
#       
#     }
#     return(populationsRF_DF)
#   }
#   
#   populationsRF_DF <- readData(populationsRF_DF)
#   list[birthKoefs_DF, deathKoefs_DF] <- readKoefs(birthKoefs_DF, deathKoefs_DF)
#   list[birthKoefs_DF, deathKoefs_DF] <- koefsCorrect(birthKoefs_DF, deathKoefs_DF, 1991,1994)
#   list[birthKoefs_DF, deathKoefs_DF] <- koefsCorrect(birthKoefs_DF, deathKoefs_DF, 2014,2050)
#  
#   #Рисуем, что получилось
#   bK_plot <- birthKoefs_DF %>% mutate(birthAgeGroup = as.factor(birthAgeGroup)) %>% 
#     gather(urbanRuralGroup, value, -c(years, birthAgeGroup))
#   ggplot(bK_plot, aes(years, value, color=birthAgeGroup))+geom_line()+facet_grid(urbanRuralGroup ~ .)
#   dK_plot <- deathKoefs_DF %>% mutate(deathAgeGroup = as.factor(deathAgeGroup)) %>%
#     gather(group, value, -c(years, deathAgeGroup)) %>% mutate(sex = substr(group, 1, 1), urban = grepl("Urban",group)) 
#   ggplot(dK_plot, aes(years, value, color=deathAgeGroup))+geom_line()+facet_grid(sex~urban)
#   popul_plot <- populationsRF_DF %>% gather(group, value, -c(years, ageGroup)) %>% mutate(ageGroup=as.factor(ageGroup), sex = substr(group, 1, 1), urban = grepl("Urban",group)) 
#   ggplot(popul_plot, aes(years, value, bg=ageGroup))+geom_bar(stat = "identity")+facet_grid(sex~urban)
#   
#   populationsRF_DF1 <- simulateRF(populationsRF_DF, 1990,2001)
#   populationsRF_DF1 <- simulateRF(populationsRF_DF1, 2003,2009)
#   populationsRF_DF1 <- simulateRF(populationsRF_DF1, 2011,2013)
#   populationsRF_DF1 <- simulateRF(populationsRF_DF1, 2015,2050)
#   
#   popul_plot2 <- populationsRF_DF1 %>% gather(group, value, -c(years, ageGroup)) %>% mutate(ageGroup=as.factor(ageGroup), sex = substr(group, 1, 1), urban = grepl("Urban",group))
#   ggplot(popul_plot2, aes(years, value, bg=ageGroup))+geom_bar(stat = "identity")+facet_grid(sex~urban)
#   
# 
# #   dataDemographyRF_transformed <- ddply(populationsRF_DF, c("years", "ageGroup"), summarise,
# #                                         population = sum(maleUrbanPopulation, femaleUrbanPopulation, maleRuralPopulation, femaleRuralPopulation))
# #   dataDemographyRF <- data.frame(time = rep(1989:2050, each = 1),
# #                                  var = rep(1:22, each = 1*(2050-1989+1)),
# #                                  val = dataDemographyRF_transformed$population
# #   )
# #   
# #   groups <- aggregate(. ~ years, data = populationsRF_DF, FUN=sum)
# #   dataDemographyRF <- data.frame(time = rep(1989:2050, each = 1),
# #                                  var = rep(1:4, each = 1*(2050-1989+1)),
# #                                  val = c(groups$maleUrbanPopulation, groups$femaleUrbanPopulation, groups$maleRuralPopulation, groups$femaleRuralPopulation)
# #   )
#   
#   output$graphDecompo <- renderChart({
#     n <- ggplot(popul_plot2, aes(years, value, bg=ageGroup))+geom_bar(stat = "identity")+facet_grid(sex~urban)
#       # nPlot(value ~ time, data = popul_plot2, type = "stackedAreaChart", group="ageGroup")
# #     n$chart(duration = 0, useInteractiveGuideline = T)
#     n$set(dom = 'graphDecompo')
#     return(n)
#   })
#   for (i in 1:3) {
#     i
#   }
  
  output$birthControlGraph <- renderChart({
    birthKoefs_df <- data.frame(group = rep(1:7, each = 1),
                                var = rep(1, each = 7),
                                values = rep(1, each = 7)
    )
    birthControlGraph <- nPlot(values ~ group, data = birthKoefs_df, type = "multiBarHorizontalChart", group="var")
    birthControlGraph$chart(showControls = F, showValues = F, transitionDuration = 0)
    birthControlGraph$set(dom = 'birthControlGraph')
    return(birthControlGraph)
  })

  output$deathControlGraph <- renderChart({
    deathKoefs_df <- data.frame(group = rep(1:7, each = 1),
                                var = rep(1, each = 7),
                                values = c(input$c_d_1,input$c_d_2,input$c_d_3,input$c_d_4,input$c_d_5,input$c_d_6,input$c_d_7)
    )
    deathControlGraph <- nPlot(values ~ group, data = deathKoefs_df, type = "multiBarHorizontalChart", group="var")
    deathControlGraph$chart(showControls = F, showValues = F, transitionDuration = 0)
    deathControlGraph$set(dom = 'deathControlGraph')
    return(deathControlGraph)
  })
  
  


  df <- data.frame(country=c("US", "GB", "BR"), val1=c(10,13,14), val2=c(23,12,32))
  output$googleVizChart <-  renderGvis({
    library(RCurl)
    url <- "https://raw.githubusercontent.com/mages/diesunddas/master/Data/US%20Presidential%20Elections.csv"
    dat <- getURL(url, ssl.verifypeer=0L, followlocation=1L)
    dat <- read.csv(text=dat)
    myData <- subset(dat, year == 1932)
    myData$state <- as.character(myData$state)
    # https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2#RU
    # https://en.wikipedia.org/wiki/ISO_3166-2:RU
    myData[1,1]<-"RU-AD"
    myData[2,1]<-"RU-AL"
    myData[3,1]<-"RU-TA"
    myData[4,1]<-"RU-TY"
    myData[5,1]<-"RU-SA"
    myData$demVote <- myData$demVote * input$i_mapMultiplier
    myData$state <- as.factor(myData$state)
    gvisGeoChart(myData,
                 locationvar="state", colorvar="demVote",
                 options=list(region="RU", displayMode="regions", 
                              resolution="provinces",
                              enableRegionInteractivity = TRUE,
                              width="100%", height=600,
                              colorAxis="{colors:['#FFFFFF', '#0000FF']}"
                 ))     
    })
})