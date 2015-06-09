#Demography helper file
years <- seq(1989, 2050, 1)
ageGroupHeader <- seq(1,22,1)
urbanRuralGroups <- seq(1,2,1)
birthAgeGroupHeader <- seq(1,8,1)
deathAgeGroupHeader <- seq(1,19,1)

#For final data
years_f <- seq(1989, 2050, 1)
age_f <- seq(1, 100, 1)
urbanF_f <- factor(c(TRUE, FALSE),labels = c("urban", "rural"))
maleF_f <- factor(c(TRUE, FALSE), labels = c("male", "female"))


#Main Functions

#New version of DF for demography simulation
readRegionsLabels <- function(){
  library(xlsx)
  library(dplyr)
  regionNamesOrigin <- read.xlsx(paste(getwd(),"/RFData/Pril4_2014.xls", sep = ""), sheetIndex = 2, startRow = 12, endRow = 290, colIndex = 1, header = F)
  regionLabels <- filter(regionNamesOrigin, (X1 != "2013" & X1 != "2012"))
  regionLabels <- filter(regionLabels, !grepl(x = X1, pattern = "округ.*$"))
  return(as.vector(regionLabels$X1, mode = "any"))
  #c <- readRegionsLabels()
}


dfLength <- rep(1, length(years_f)*length(age_f)*length(urbanF_f)*length(maleF_f)*length(readRegionsLabels()))
populationDF <- data.frame(year = years_f, 
                           age = age_f,
                           region = readRegionsLabels(),
                           urban = c("urban", "rural"),
                           sex = c("male", "female"),
                           population = dfLength,
                           deathCoef = dfLength,
                           birthCoef = dfLength
)


#Read Data from its source
#Read population data - all available sources
#Demography 2014 gks.ru
readBirthCoef <- function(populationDF){
  library(xlsx)
  library(dplyr)
  #Prepare data
  headerBirthC <- 6
  urbanLines <- c(296, 574)
  ruralLines <- c(580, 858)
  header <- read.xlsx(paste(getwd(),"/RFData/Pril4_2014.xls", sep = ""), sheetIndex = 2, startRow = headerPopul, endRow = headerPopul, colIndex = seq(1,9,1), header = F)
  header[] <- lapply(header, as.character)
  header[10] <- "Urban"
  header[1] <- "RegionYearColumn"
  
  birthCUrbanOrigin <- read.xlsx(paste(getwd(),"/RFData/Pril4_2014.xls", sep = ""), sheetIndex = 2, startRow = urbanLines[1], endRow = urbanLines[2], header = F)
  birthCRuralOrigin <- read.xlsx(paste(getwd(),"/RFData/Pril4_2014.xls", sep = ""), sheetIndex = 2, startRow = ruralLines[1], endRow = ruralLines[2], header = F)
  birthCUrbanOrigin$urban <- rep(1, length(birthCUrbanOrigin))
  birthCRuralOrigin$urban <- rep(0, length(birthCRuralOrigin))
  names(birthCUrbanOrigin) <- header
  names(birthCRuralOrigin) <- header
  birthCoefDF <- rbind(birthCRuralOrigin, birthCUrbanOrigin)
  
  #Parsing
  #Find proper row
  #Write in proper year
  result <- data.frame(year, region, age, urban, sex,  birthcoef)
  for (yearCounter in c(2012,2013)) {
    for (regionCounter in readRegionsLabels()) {
      for (ageCounter in seq(15,49,1)) {
        for (urbanCounter in c(1,0)) {
          ageGroup <- switch (ageCounter,
            '>15' = 1,
            '20-24' = 2
          )
          birthCoef <- select(birthCoefDF, )
          result <- (yearCounter,regionCounter,ageCounter,urbanCounter,0,birthCoef)
        }
      }
    }
    }
}
#Read death coef data - all AS

#Read births per 1000 women - all AS

