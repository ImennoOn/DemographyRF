#For final data
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
  return(as.vector(regionLabels$X1, mode = "any"))
  #c <- readRegionsLabels()
}

# fillPopulationDF <- function(populationDF){
#   regions <- as.character(readRegionsLabels())
#   for (year in years_f) {
#     for (age in age_f) {
#       for (region in regions) {
#         for (urban in 0:1) {
#           for (sex in 0:1) {
#             populationDF[nrow(populationDF)+1,] <- c(year, age, region, urban, sex, 1, 1, 1)
#           }
#         }
#       }
#     }
#   }
#   return(populationDF)
# }

regions <- as.character(readRegionsLabels())
populationDF <- data.frame(year = rep(years_f, each = length(age_f)*length(regions)*2*2),
                           age = rep(age_f, each = length(regions)*2*2),
                           region = rep(regions, each = 2*2),
                           urban = rep(0:1, each = 2),
                           sex = rep(0:1, each = 1),
                           population = 1,
                           deathCoef = 1,
                           birthCoef = 1,
                           stringsAsFactors=F
)

save(populationDF, file="demographyBlankData.Rda")
load("demographyBlankData.Rda")

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
  header <- read.xlsx(paste(getwd(),"/RFData/Pril4_2014.xls", sep = ""), sheetIndex = 2, startRow = headerBirthC, endRow = headerBirthC, colIndex = seq(1,9,1), header = F)
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
  for (regionCounter in readRegionsLabels()) {
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
save(populationDF, file="demographyBlankData.Rda")

