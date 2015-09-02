library(xlsx)
library(dplyr)
library(zoo)

years_f <- seq(1989, 2050, 1)
age_f <- seq(5, 100, 5)
urbanF_f <- factor(c(1, 0),labels = c("urban", "rural"))
maleF_f <- factor(c(1, 0), labels = c("male", "female"))
regions <- as.character(readRegionsLabels())
populationDF <- data.frame(year = rep(years_f, each = length(age_f)*length(regions)*2*2),
                           age = rep(age_f, each = length(regions)*2*2),
                           region = rep(regions, each = 2*2),
                           urban = rep(0:1, each = 2),
                           sex = rep(0:1, each = 1),
                           population = NA,
                           deathCoef = NA,
                           birthCoef = NA,
                           stringsAsFactors=F)

# Используемые константы
## Данные по роджаемости
c_births_file <- paste(getwd(),"/Data/Pril4_2014.xls", sep = "")


# Помощники
## returns string w/o leading whitespace
trim.leading <- function (x)  sub("^\\s+", "", x)
## returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)
## returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Методы
## Достать список регионов
readRegionsLabels <- function(){
  regionNamesOrigin <- read.xlsx(c_births_file, sheetIndex = 2, startRow = 12, endRow = 290, colIndex = 1, header = F)
  regionLabels <- filter(regionNamesOrigin, (X1 != "2013" & X1 != "2012"))
  remove(regionNamesOrigin)
  regionLabels <- filter(regionLabels, !grepl(x = X1, pattern = "округ.*$"))
  regionLabels <- as.list(as.character(regionLabels$X1))
  regionLabels <- lapply(regionLabels, trim)
  regionLabels <- lapply(regionLabels, function(x) {if(grepl("^г. ", x)) substr(x, 4, nchar(x)) else x})
  regionLabels <- lapply(regionLabels, function(x) {if(grepl("^Республика ", x)) substr(x, 12, nchar(x)) else x})
  regionLabels <- lapply(regionLabels, function(x) {if(grepl("край$", x)) substr(x, 1, nchar(x)-5) else x})
  regionLabels <- sapply(regionLabels, function(x) {if(grepl("область$", x)) substr(x, 1, nchar(x)-8) else x})
  return(regionLabels)
}

## Читаем коэф. рождаемости
readBirthCoefs <- function(populationDF){
  #Prepare data
  headerBirthC <- 6
  urbanLines <- c(296, 574)
  ruralLines <- c(580, 858)
  header <- read.xlsx(c_births_file, sheetIndex = 2, startRow = headerBirthC, endRow = headerBirthC, colIndex = seq(1,9,1), header = F)
  header[] <- lapply(header, as.character)
  header[10] <- "Urban"
  header[1] <- "RegionYearColumn"
  
  birthCUrbanOrigin <- read.xlsx(c_births_file, sheetIndex = 2, startRow = urbanLines[1], endRow = urbanLines[2], header = F)
  birthCRuralOrigin <- read.xlsx(c_births_file, sheetIndex = 2, startRow = ruralLines[1], endRow = ruralLines[2], header = F)
  birthCUrbanOrigin$urban <- rep(1, length(birthCUrbanOrigin))
  birthCRuralOrigin$urban <- rep(0, length(birthCRuralOrigin))
  names(birthCUrbanOrigin) <- header
  names(birthCRuralOrigin) <- header
  birthCoefDF <- rbind(birthCRuralOrigin, birthCUrbanOrigin)
  
  #Parsing and finding proper row
  for (regionCounter in regions) {
    #Find proper area
    sequencingRegionRow <- which(grepl(regionCounter, birthCoefDF$RegionYearColumn))
    for (urbanCounter in c(0,1)) {
      for (yearCounter in c(2012, 2013)) {
        for (ageGCounter in 2:8) {
          rowCurr <- sequencingRegionRow[urbanCounter+1]+yearCounter-2011
          colCurr <- ageGCounter
          age <- ageGCounter*5+10
          birthCoef <- as.numeric(birthCoefDF[rowCurr,colCurr])
          populationDF[with(populationDF, 
                            which(year == yearCounter &
                                    age == age & 
                                    region == regionCounter & 
                                    urban == urbanCounter & 
                                    sex == 0)),]$birthCoef <- birthCoef
        }
      }
    }
  }
  return(populationDF)
}

## Читаем коэф. смерности
readDeathCoefs <- function(populationDF){
  #Prepare data
  headerDeathsC <- 6
  urbanLines <- c(296, 574)
  ruralLines <- c(580, 858)
  header <- read.xlsx(c_deaths_file, sheetIndex = 2, startRow = headerBirthC, endRow = headerBirthC, colIndex = seq(1,9,1), header = F)
  header[] <- lapply(header, as.character)
  header[10] <- "Urban"
  header[1] <- "RegionYearColumn"
  
  birthCUrbanOrigin <- read.xlsx(c_births_file, sheetIndex = 2, startRow = urbanLines[1], endRow = urbanLines[2], header = F)
  birthCRuralOrigin <- read.xlsx(c_births_file, sheetIndex = 2, startRow = ruralLines[1], endRow = ruralLines[2], header = F)
  birthCUrbanOrigin$urban <- rep(1, length(birthCUrbanOrigin))
  birthCRuralOrigin$urban <- rep(0, length(birthCRuralOrigin))
  names(birthCUrbanOrigin) <- header
  names(birthCRuralOrigin) <- header
  birthCoefDF <- rbind(birthCRuralOrigin, birthCUrbanOrigin)
  
  #Parsing
  #Find proper row
  for (regionCounter in regions) {
    #Find proper area
    sequencingRegionRow <- which(grepl(regionCounter, birthCoefDF$RegionYearColumn))
    for (urbanCounter in c(0,1)) {
      for (yearCounter in c(2012, 2013)) {
        for (ageGCounter in 2:8) {
          rowCurr <- sequencingRegionRow[urbanCounter+1]+yearCounter-2011
          colCurr <- ageGCounter
          age <- ageGCounter*5+10
          birthCoef <- as.numeric(as.character(birthCoefDF[rowCurr,colCurr]))
          populationDF[with(populationDF, 
                            which(year == yearCounter &
                                    age == age & 
                                    region == regionCounter & 
                                    urban == urbanCounter & 
                                    sex == 0)),]$birthCoef <- birthCoef
        }
      }
    }
  }
  return(populationDF)
}

system.time(populationDF <- readBirthCoef(populationDF))
save(populationDF, file="demographyBlankData_v2.Rda")

