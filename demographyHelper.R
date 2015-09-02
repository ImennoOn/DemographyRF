library(xlsx)
library(dplyr)
library(zoo)
library(XLConnect)
library(tidyr)

# Используемые константы
## Данные по роджаемости
c_births_file <- paste(getwd(),"/Data/Pril4_2014.xls", sep = "")
c_deaths_file <- paste(getwd(),"/Data/2013_Tab_Smertnosti.xls", sep = "")
c_population_file <- paste(getwd(),"/Data/2013_chislennost.xls", sep = "")

# Помощники
## returns string w/o leading whitespace
trim.leading <- function (x)  sub("^\\s+", "", x)
## returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)
## returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Методы
## Единый способ приведения наименования регионов
brushUpRegions <- function(regionLabels) {
  regionLabels <- lapply(regionLabels, trim)
  regionLabels <- lapply(regionLabels, function(x) {gsub("г\\.", "", x)})
  regionLabels <- lapply(regionLabels, function(x) {gsub("Республика", "", x)})
  regionLabels <- lapply(regionLabels, function(x) {gsub("республика", "", x)})
  # regionLabels <- lapply(regionLabels, function(x) {if(grepl("^Республика ", x)) substr(x, 12, nchar(x)) else x})
  regionLabels <- lapply(regionLabels, function(x) {if(grepl("край$", x)) substr(x, 1, nchar(x)-5) else x})
  regionLabels <- lapply(regionLabels, function(x) {if(grepl("область$", x)) substr(x, 1, nchar(x)-8) else x})
  regionLabels <- lapply(regionLabels, trim)
  return(regionLabels)
}

## Достать список регионов
readRegionsLabels <- function(){
  regionNamesOrigin <- read.xlsx(c_births_file, 
                                 sheetIndex = 2, 
                                 startRow = 12,
                                 endRow = 290, 
                                 colIndex = 1,
                                 header = F)
  regionLabels <- filter(regionNamesOrigin, (X1 != "2013" & X1 != "2012"))
  remove(regionNamesOrigin)
  # regionLabels <- filter(regionLabels, !grepl(x = X1, pattern = "округ.*$"))
  regionLabels <- as.list(as.character(regionLabels$X1))
  return(brushUpRegions(regionLabels))
}

## Читаем коэф. рождаемости
readBirthCoefs <- function(populationDF){
  #Prepare data
  headerBirthC <- 6
  urbanLines <- c(296, 574)
  ruralLines <- c(580, 858)
  header <- read.xlsx(c_births_file, sheetIndex = 2, 
                      startRow = headerBirthC, endRow = headerBirthC, 
                      colIndex = seq(1,9,1), header = F)
  header[] <- lapply(header, as.character)
  header[10] <- "Urban"
  header[1] <- "RegionYearColumn"
  
  
  birthCUrbanOrigin <- read.xlsx(c_births_file, sheetIndex = 2,
                                 startRow = urbanLines[1], endRow = urbanLines[2],
                                 header = F)
  birthCUrbanOrigin$urban <- rep(1, length(birthCUrbanOrigin))
  names(birthCUrbanOrigin) <- header
  birthCUrbanOrigin$RegionYearColumn <- as.character(birthCUrbanOrigin$RegionYearColumn)
  birthCUrbanOrigin <- birthCUrbanOrigin %>% mutate(year = as.numeric(RegionYearColumn))
  listTMP <- birthCUrbanOrigin$RegionYearColumn
  regionsTMP <- listTMP[!(listTMP=="2012" | listTMP=="2013")]
  listTMP[(listTMP=="2012")] <- regionsTMP
  listTMP[(listTMP=="2013")] <- regionsTMP
  birthCUrbanOrigin$RegionYearColumn <- listTMP
  birthCUrbanOrigin <- birthCUrbanOrigin[!is.na(birthCUrbanOrigin$year),]
  
  birthCRuralOrigin <- read.xlsx(c_births_file, sheetIndex = 2, 
                                 startRow = ruralLines[1], endRow = ruralLines[2],
                                 header = F)
  birthCRuralOrigin$urban <- rep(0, length(birthCRuralOrigin))
  names(birthCRuralOrigin) <- header
  birthCRuralOrigin$RegionYearColumn <- as.character(birthCRuralOrigin$RegionYearColumn)
  birthCRuralOrigin <- birthCRuralOrigin %>% mutate(year = as.numeric(RegionYearColumn))
  birthCRuralOrigin$RegionYearColumn <- listTMP
  birthCRuralOrigin <- birthCRuralOrigin[!is.na(birthCRuralOrigin$year),]
  
  birthCoefDF <- rbind(birthCRuralOrigin, birthCUrbanOrigin)
  
  names(birthCoefDF) <- c("region", paste0("X",seq(20, 50, 5)), "overall", "urban", "year")
  birthCoefDF <- filter(birthCoefDF, !grepl(region, pattern = "округ.*$"))
  birthCoefDF$region <- unlist(brushUpRegions(birthCoefDF$region))
  birthCoefDF <- transform(birthCoefDF, X20 = as.numeric(as.character(X20)),
                           X25 = as.numeric(as.character(X25)),
                           X30 = as.numeric(as.character(X30)),
                           X35 = as.numeric(as.character(X35)),
                           X40 = as.numeric(as.character(X40)),
                           X45 = as.numeric(as.character(X45)),
                           X50 = as.numeric(as.character(X50)),
                           overall = as.numeric(as.character(overall)))
  names(birthCoefDF) <- c("region", seq(20, 50, 5), "overall", "urban", "year")
  # sum(birthCoefDF[,as.character(seq(20,50,5))], na.rm = T)
  birthCoefDF <- birthCoefDF %>% select(-overall) %>% gather(age, birthCoef, -c(region, year, urban))
  # sum(birthCoefDF$birthCoef, na.rm = T)
  birthCoefDF$birthCoef <- c(na.approx(birthCoefDF$birthCoef), 0.1)
  birthCoefDF$sex <- 0
  
  populationDF$region <- as.factor(populationDF$region)
  birthCoefDF$region <- as.factor(birthCoefDF$region)
  populationDF <- merge(populationDF, birthCoefDF,
                        by=intersect(names(populationDF), names(birthCoefDF))[1:5], all.x = T) %>%
    mutate(birthCoef = birthCoef.y) %>%
    select(-c(birthCoef.x,birthCoef.y))
  # sum(populationDF$birthCoef, na.rm = T)
  remove(birthCRuralOrigin)
  remove(birthCUrbanOrigin)
  remove(header)
  
  return(populationDF)
}

## Читаем коэф. смерности
readDeathCoefs <- function(populationDF){
  #Prepare data
  headerDeathsC <- 3
  # Колонка для коэф дожития
  stillAliveCoefColumns <- c(1,9)
  stillAliveCoefRows <- c(2,125)
  
  coef_regions <- list()
  stillAliveCoefs <- list()
  
  wb <- XLConnect::loadWorkbook(c_deaths_file)  
  sheets <- getSheets(wb)
  # Оставляем лишь необходимые листы. Выделяем через булевые листы необходимые, склеиваем, запрашиваем
  isUrban <- function(x) {preLastChar = substr(x, nchar(x)-2,
                                               nchar(x)-2); if(preLastChar == "8") TRUE else FALSE}
  isMale <- function(x) {lastChar = substr(x, nchar(x), 
                                           nchar(x)); if(lastChar == "3") TRUE else FALSE}
  isReq <- function(x) {lastChar = substr(x, nchar(x), 
                                           nchar(x)); preLastChar = substr(x, nchar(x)-2,
                                                                           nchar(x)-2); 
                                           if(nchar(x) > 8 &
                                                         (lastChar == "2" | lastChar == "3") &
                                                         (preLastChar == "8" | preLastChar == "2")) TRUE else FALSE}
  req <- sapply(sheets, isReq)
  
  # Тут очень грузная часть, стоит задуматься о прерываниях, на работу сборщика мусора Java
  # Вроде как исправлено
  for (excelList in sheets[req]) {
    stillAliveCoefsXLSX <- readWorksheet(wb, 
                                 sheet = excelList,
                                 startRow = stillAliveCoefRows[1],
                                 endRow = stillAliveCoefRows[2],
                                 startCol = stillAliveCoefColumns[1],
                                 endCol = stillAliveCoefColumns[2],
                                 header = F)
    # Определяем регион
    coef_regions[[excelList]] <- as.character(stillAliveCoefsXLSX[1,1])
    # Оставляем лишь коэф дожития для каждых 5 лет
    stillAliveCoefs[[excelList]] <- as.double(as.character(stillAliveCoefsXLSX[seq(9,125,6),9]))
  }
  
  coef_regions <- brushUpRegions(coef_regions)
  
  # TODO переписать через merge, как в birthCoefs
  for (index in names(stillAliveCoefs)) {
    curr_urban <- as.numeric(isUrban(index))
    curr_sex <- as.numeric(isMale(index))
    curr_region <- coef_regions[[index]]
    # print(paste(curr_region, curr_sex, curr_urban, sep = " : "))
    if(curr_region %in% regions){
      populationDF[with(populationDF, 
                         which(year == 2013 &
                                 region == curr_region & 
                                 urban == curr_urban & 
                                 sex == curr_sex)),]$deathCoef <- stillAliveCoefs[[index]]
    }
  }
  
  return(populationDF)
}

## Читаем количество жителей
readPopulation <- function(populationDF){
  headerPopulC <- 2
  populColumns <- c(1,11)
  populRows <- c(2,130)
  
  regions_L <- list()
  urbanMaleP_L <- list()
  urbanFemaleP_L <- list()
  ruralMaleP_L <- list()
  ruralFemaleP_L <- list()
  
  wb <- XLConnect::loadWorkbook(c_population_file)  
  sheets <- getSheets(wb)
  # Оставляем лишь необходимые листы. Выделяем через булевые листы необходимые, склеиваем, запрашиваем
  isUrban <- function(x) {preLastChar = substr(x, nchar(x)-2,
                                               nchar(x)-2); if(preLastChar == "8") TRUE else FALSE}
  isMale <- function(x) {lastChar = substr(x, nchar(x), 
                                           nchar(x)); if(lastChar == "3") TRUE else FALSE}
  isReq <- function(x) {if(nchar(x) > 6) TRUE else FALSE}
  
  # Тут очень грузная часть, стоит задуматься о прерываниях, на работу сборщика мусора Java
  # Вроде как исправлено
  for (excelList in sheets[sapply(sheets,isReq)]) {
    populationXLSX <- readWorksheet(wb,
                                    sheet = excelList,
                                    startRow = populRows[1],
                                    endRow = populRows[2],
                                    startCol = populColumns[1],
                                    endCol = populColumns[2],
                                    header = F)
    # Определяем регион
    regions_L[[excelList]] <- as.character(populationXLSX[1,1])
    # Раскидываем население по листам. Колонки 7,8,10,11
    urbanMaleP_L[[excelList]] <- as.character(populationXLSX[seq(13,130,6),7])
    urbanFemaleP_L[[excelList]] <- as.character(populationXLSX[seq(13,130,6),8])
    ruralMaleP_L[[excelList]] <- as.character(populationXLSX[seq(13,130,6),10])
    ruralFemaleP_L[[excelList]] <- as.character(populationXLSX[seq(13,130,6),11])
  }
  # Приводим в нормальный вид список регионов
  regions_L <- brushUpRegions(regions_L)
  # Помощник, дальше используется для сбора в один DF
  getDTfromPopulList <- function(populList, isex, iurban, index) {
    data.frame(year = rep(2013, 20),
               age = seq(5,100,5),
               region = rep(regions_L[[index]], 20),
               urban = rep(iurban, 20),
               sex = rep(isex,20), 
               population = populList[[index]]
               )
  }
  # Склеиваем все полученные результаты в DF
  DF <- data.frame()
  for (index in names(regions_L)) {
    DF <- rbind(DF,
                getDTfromPopulList(urbanMaleP_L, 1, 1, index), 
                getDTfromPopulList(urbanFemaleP_L, 0, 1, index), 
                getDTfromPopulList(ruralMaleP_L, 1, 0, index),
                getDTfromPopulList(ruralFemaleP_L, 0, 0, index))
  }
  # Merge mo'fo
  populationDF <- merge(populationDF, 
                        DF, 
                        by=intersect(names(populationDF), names(DF))[1:5], all.x = T) %>% 
    # UNTESTED FEATURE as.as.
    mutate(population = as.numeric(as.character(population.y))) %>% 
    select(-c(population.x,population.y))
  
  return(populationDF)
}

simulateRF <- function(DF, startYear, endYear){
  #simulate demography dynamics
  # DF$population <- as.numeric(as.character(DF$population))
  for(curr_forecastedYear in seq(startYear,endYear-1,1)){
    # Меня можно за это покорать, но это временная затычка
    # MOCK
    if(curr_forecastedYear > 2013){
      DF[DF$year==curr_forecastedYear,]$deathCoef <- DF[DF$year==curr_forecastedYear-1,]$deathCoef
      DF[DF$year==curr_forecastedYear,]$birthCoef <- DF[DF$year==curr_forecastedYear-1,]$birthCoef
    }
    # ENDMOCK
    #Birth
    currentPopulation <- DF[DF$year == curr_forecastedYear,]
    
    currentPopulation$newbies <- (currentPopulation$population*currentPopulation$birthCoef/1000)/2
    cPNewbies <- currentPopulation %>% 
      group_by(region, urban, sex) %>%
      summarise(population = sum(newbies, na.rm=TRUE))
    cPNewbies[seq(2,nrow(cPNewbies),2),]$population <- cPNewbies[seq(1,nrow(cPNewbies),2),]$population
    
    #Death
    currentPopulation$deaths <- -(currentPopulation$population*(1-currentPopulation$deathCoef))
    
    #Growth & everything
    currentPopulation$nextYearPopulation <- 0
    currentPopulation[currentPopulation$age == 5,]$nextYearPopulation <-
      cPNewbies$population +
      currentPopulation[currentPopulation$age == 5,]$population*(1-1/5) +
      currentPopulation[currentPopulation$age == 5,]$deaths
    for (curr_age in seq(10, 100, 5)) {
      currentPopulation[currentPopulation$age == curr_age,]$nextYearPopulation <-
        currentPopulation[currentPopulation$age == curr_age-5,]$population*(1/5) + 
        currentPopulation[currentPopulation$age == curr_age,]$population*(1-1/5) +
        currentPopulation[currentPopulation$age == curr_age,]$deaths
    }
    DF[DF$year==curr_forecastedYear+1,]$population <- currentPopulation$nextYearPopulation
  }
  return(DF)
}

# Рабочая зона
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

system.time(populationDF <- readBirthCoefs(populationDF))
system.time(populationDF <- readDeathCoefs(populationDF))
system.time(populationDF <- readPopulation(populationDF))
# system.time(populationDF <- readMigration(populationDF))
# saveRDS(populationDF, file="./Data/demographyBlankData_v3.RDS")

