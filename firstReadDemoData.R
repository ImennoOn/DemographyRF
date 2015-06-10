#For final data
library(data.table)
library(dplyr)
library(tidyr)
library(reshape2)
years_f <- seq(1989, 2050, 1)
age_f <- seq(5, 100, 5)
urbanF_f <- factor(c(1, 0),labels = c("urban", "rural"))
maleF_f <- factor(c(1, 0), labels = c("male", "female"))


#Main Functions

#New version of DF for demography simulation
readRegionsLabels <- function(){
  library(xlsx)
  library(dplyr)
  regionNamesOrigin <- read.xlsx(paste(getwd(),"/RFData/Pril4_2014.xls", sep = ""), sheetIndex = 2, startRow = 12, endRow = 290, colIndex = 1, header = F)
  regionLabels <- filter(regionNamesOrigin, (X1 != "2013" & X1 != "2012"))
  regionLabels <- filter(regionLabels, !grepl(x = X1, pattern = "округ.*$"))
  regionLabels <- lapply(regionLabels$X1, str_trim)
  return(regionLabels)
  #c <- readRegionsLabels()
}

regions <- as.character(readRegionsLabels())
populationDT <- data.table(year = rep(years_f, each = length(age_f)*length(regions)*2*2),
                           age = rep(age_f, each = length(regions)*2*2),
                           region = rep(regions, each = 2*2),
                           urban = rep(0:1, each = 2),
                           sex = rep(0:1, each = 1),
                           population = 0,
                           deathCoef = 0,
                           birthCoef = 0
)

readBirthCoef <- function(populationDT){
  library(xlsx)
  library(dplyr)
  #Prepare data
  headerBirthC <- 6
  urbanLines <- c(296, 574)
  ruralLines <- c(580, 858)
  header <- read.xlsx(paste(getwd(),"/RFData/Pril4_2014.xls", sep = ""), sheetIndex = 2, startRow = headerBirthC, endRow = headerBirthC, colIndex = seq(1,9,1), header = F)
  header[] <- lapply(header, as.character)
  header[10] <- "Urban"
  header[1] <- "RegionYearColumn"
  
  birthCUrbanOrigin <- read.xlsx(paste(getwd(),"/RFData/Pril4_2014.xls", sep = ""), sheetIndex = 2, startRow = urbanLines[1], endRow = urbanLines[2], header = F)
  birthCUrbanOrigin$urban <- rep(1, length(birthCUrbanOrigin))
  names(birthCUrbanOrigin) <- header
  birthCUrbanOrigin[,2:9] <- sapply(birthCUrbanOrigin[,2:9], as.character)
  birthCUrbanOrigin[,2:9] <- sapply(birthCUrbanOrigin[,2:9], as.numeric)
  
  birthCRuralOrigin <- read.xlsx(paste(getwd(),"/RFData/Pril4_2014.xls", sep = ""), sheetIndex = 2, startRow = ruralLines[1], endRow = ruralLines[2], header = F)
  birthCRuralOrigin$urban <- rep(0, length(birthCRuralOrigin))
  birthCRuralOrigin[,2:9] <- sapply(birthCRuralOrigin[,2:9], as.character)
  birthCRuralOrigin[,2:9] <- sapply(birthCRuralOrigin[,2:9], as.numeric)
  names(birthCRuralOrigin) <- header
  
  birthCoefDF <- rbind(birthCRuralOrigin, birthCUrbanOrigin)
  birthCoefDT <- data.table(birthCoefDF)
  
  #Parsing
  #Find proper row
  for (regionCounter in readRegionsLabels()) {
    #Find proper area
    sequencingRegionRow <- which(grepl(regionCounter, birthCoefDT$RegionYearColumn))
    for (urbanCounter in 0:1) {
      for (yearCounter in c(2012, 2013)) {
        for (ageGCounter in 2:8) {
          rowCurr <- sequencingRegionRow[urbanCounter+1]+yearCounter-2011
          ageC <- ageGCounter*5+10
          birthCoef <- as.numeric(as.vector(birthCoefDT[rowCurr,])[[ageGCounter]])
          if (is.na(birthCoef)) {
            msg <- paste("year:", yearCounter,"; age:", ageC, "; ageGroup:", ageGCounter, "; region:", regionCounter, "; urban:", urbanCounter)
            print(msg)
          }
          populationDT[with(populationDT, 
                            which(year == yearCounter &
                                    age == ageC & 
                                    region == regionCounter & 
                                    urban == urbanCounter & 
                                    sex == 0)),]$birthCoef <- birthCoef
        }
      }
    }
  }
  return(populationDT)
}
#TESTS BirthCoef
# system.time(populationDT <- readBirthCoef(populationDT))
# save(populationDT, file="demographyBlankData.Rda")
# object.size(populationDT)
load("demographyBlankData.Rda")
#---

readPopulation <- function(populationDT){
  library(xlsx)
  library(dplyr)
  library(stringr)

  populationWB <- loadWorkbook(paste(getwd(),"/RFData/2ph_reg_2013_chislennost.xls", sep = ""))
  sheets <- names(getSheets(populationWB))
  
  for (sheetName in sheets) {
    if (nchar(sheetName) > 4) {
      selRegion <- str_trim(as.character(read.xlsx(paste(getwd(),"/RFData/2ph_reg_2013_chislennost.xls", sep = ""), sheetName = sheetName, rowIndex = 2, header = F)[[1]]),side = "both")
      data<- read.xlsx(paste(getwd(),"/RFData/2ph_reg_2013_chislennost.xls", sep = ""), sheetName = sheetName, rowIndex = seq(14,128,6), colIndex = c(1,7,8,10,11), header = F)
      names(data)<-c("ageG", "UrbanMale", "UrbanFemale", "RuralMale", "RuralFemale")

      for (ageCounter in 1:20) {
        for (column in 2:5) {
          sexCounter <- ifelse((column==2|column==4),1,0)
          urbanCounter <- ifelse(column<4,1,0)
          groupPopulation <- data[ageCounter, column]
          
          if (is.na(groupPopulation)) {
            msg <- "missing population"
            groupPopulation <- 0
            print(msg)
          }
          
          populationDT[with(populationDT, 
                            which(year == 2014 &
                                    age == ageCounter*5 & 
                                    region == selRegion & 
                                    urban == urbanCounter & 
                                    sex == sexCounter)),]$population <- groupPopulation
        }
        print(ageCounter)
        }
    }
    print(paste(match(sheetName, sheets)[1], "of", length(sheets), Sys.time()))
  }
  return(populationDT)
}
#TESTS Population
# system.time(populationDT <- readPopulation(populationDT))
# save(populationDT, file="demographyBlankData.Rda")
sum(filter(populationDT, year == 2014 & region == "Белгородская область")$population)
sum(filter(populationDT, year == 2014 & region == "Брянская область")$population)
sum(filter(populationDT, year == 2014 & region == "Владимирская область")$population)
sum(filter(populationDT, year == 2014 & region == "Воронежская область")$population)
df <- aggregate(.~region, data = filter(populationDT, year == 2014), FUN = sum)[,c(1,6)]
sum(df$population)
barplot(as.numeric(df[1:20,]$population), names.arg = substr(df[1:20,]$region,1,3) )

readDeathCoef <- function(populationDT){
  library(dplyr)
  library(stringr)
  library(readxl)
  library(xlsx)

  timerStart <- Sys.time()
  filePath <- paste(getwd(),"/RFData/Smertnost_2013.xlsx", sep = "")
  filePathv2 <- paste(getwd(),"/RFData/3tc_reg_2013nck_tabl_smertnosti.xls", sep = "")
  wb <- loadWorkbook(filePathv2)
  sheets <- names(getSheets(wb))
  lenShWB <- length(sheets)
  regionsInDT <- unique(populationDT$region)
  i <- 0
  for (sheetCounter in sheets) {
    # sheetCounter <- "38000000000_2_2"
    parseFlags <- NA
    parseFlags <- ifelse(length(grep("_8_2$", sheetCounter))!= 0, list(c(1,0)), parseFlags)
    parseFlags <- ifelse(length(grep("_8_3$", sheetCounter))!= 0, list(c(1,1)), parseFlags)
    parseFlags <- ifelse(length(grep("_2_3$", sheetCounter))!= 0, list(c(0,1)), parseFlags)
    parseFlags <- ifelse(length(grep("_2_2$", sheetCounter))!= 0, list(c(0,0)), parseFlags)
    
    urbanFlag <- parseFlags[[1]][1]
    sexFlag <- parseFlags[[1]][2]
    
    if((nchar(sheetCounter) > 9 && !is.na(urbanFlag) && !is.na(sexFlag))){
      deathData <- read_excel(filePath, sheet = sheetCounter, na = "")
      selectedRegion <- str_trim(as.character(deathData[1,1]), side = "both")
      regionFlag <- !is.na(match(selectedRegion, regionsInDT))
      if (regionFlag) {
        populationDT[with(populationDT, 
                          which(year == 2013 &
                                  region == selectedRegion & 
                                  urban == urbanFlag & 
                                  sex == sexFlag)),]$deathCoef <- as.numeric(deathData[seq(9,123,6),9][[1]])
      }
      }
    i <- i+1
    print(paste(i, "of", lenShWB, Sys.time()-timerStart, "---SheetName:", sheetCounter))
  }
  return(populationDT)
}
#TEST DeathCoef
system.time(populationDT <- readDeathCoef(populationDT))
save(populationDT, file="demographyBlankData.Rda")

#VIEW Section

#Trans death and birth coefs
load("demographyBlankData.Rda")
regions <- unique(populationDT$region)
for (yearCounter in seq(2014,2050,1)) {
    populationDT[with(populationDT, 
                      which(year == yearCounter)),]$deathCoef <- populationDT[with(populationDT, 
                                                                                   which(year == yearCounter-1)),]$deathCoef
    populationDT[with(populationDT, 
                      which(year == yearCounter)),]$birthCoef <- populationDT[with(populationDT, 
                                                                                   which(year == yearCounter-1)),]$birthCoef
}

#Population simulation
simulatePopulation <- function(populationDT, regions, startYear, endYear){
  for (yearCounter in seq(startYear,endYear,1)) {
    for (regionCounter in regions) {
          lastYearPopulation <- populationDT[with(populationDT, 
                                                  which(year == yearCounter-1 &
                                                          region == regionCounter)),]$population
          deathCoefs <- populationDT[with(populationDT, 
                                                  which(year == yearCounter-1 &
                                                          region == regionCounter)),]$deathCoef
          survivors <- lastYearPopulation * deathCoefs
          #Happy B-Day
          for (i in length(survivors):5) {
            survivors[i] <- survivors[i]-survivors[i]/5+survivors[i-4]/5
          }
          
          #Newcommers
          births <- populationDT[with(populationDT, 
                                      which(year == yearCounter-1 &
                                              region == regionCounter)),]$birthCoef
          newcommers <- (births * lastYearPopulation /1000) #couse we use sexCounter
          newcommersR <- sum(newcommers[seq(13,39,4)])
          newcommersU <- sum(newcommers[seq(15,39,4)])
          survivors[4:3] <- survivors[4:3] + newcommersU/2
          survivors[2:1] <- survivors[2:1] + newcommersR/2
          
          #Update values
          populationDT[with(populationDT, 
                            which(year == yearCounter &
                                    region == regionCounter)),]$population <- survivors
          print(paste(regionCounter, "in", yearCounter, "completed"))
    }
  }
  return(populationDT)
}
system.time(populationDT <- simulatePopulation(populationDT, regions, 2015, 2050))
save(populationDT, file="demographyBlankData.Rda")

#VIEW Section start
regionSelection <- regions[10]
#1
populationDT[with(populationDT, 
                  which(year == 2013 &
                          region == regionSelection & 
                          urban == 0 & 
                          sex == 0)),]
#2
view <- data.frame(populationDT[with(populationDT,
                          which(region == regionSelection &
                                  year %in% seq(2015,2050,1))),])
ageAsFactor <- as.factor(view$age)
view$age <- ageAsFactor
view <- reshape(view, idvar=c("year","population"), timevar="age", direction="wide")

#3
df <- aggregate(.~region, data = filter(populationDT, year == 2020), FUN = sum)[,c(1,6)]
sum(df$population)

#VIEW Section end

