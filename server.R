library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(googleVis)
library(ggplot2)
library(rCharts)
library(data.table)
library(dplyr)
library(XML)
library(RCurl)
# options(shiny.error = traceback)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)

# Помощники
# source("rcharts_pyramids.R")
## returns string w/o leading or trailing whitespace
trim <- function (x)
  gsub("^\\s+|\\s+$", "", x)

brushUpRegions <- function(regionLabels) {
  regionLabels <-
    lapply(regionLabels, function(x) {
      gsub("автономный округ", "", x)
    })
  regionLabels <- lapply(regionLabels, trim)
  regionLabels <-
    lapply(regionLabels, function(x) {
      gsub("г\\.", "", x)
    })
  regionLabels <-
    lapply(regionLabels, function(x) {
      gsub("Республика", "", x)
    })
  regionLabels <-
    lapply(regionLabels, function(x) {
      gsub("республика", "", x)
    })
  regionLabels <-
    lapply(regionLabels, function(x) {
      gsub("федеральный округ", "", x)
    })
  regionLabels <-
    lapply(regionLabels, function(x) {
      gsub("автономный округ", "", x)
    })
  regionLabels <-
    lapply(regionLabels, function(x) {
      if (grepl("край$", x))
        substr(x, 1, nchar(x) - 5)
      else
        x
    })
  regionLabels <-
    lapply(regionLabels, function(x) {
      if (grepl("область$", x))
        substr(x, 1, nchar(x) - 8)
      else
        x
    })
  regionLabels <- lapply(regionLabels, trim)
  return(regionLabels)
}

simulateRF <- function(DF, startYear, endYear) {
  #simulate demography dynamics
  # DF$population <- as.numeric(as.character(DF$population))
  for (curr_forecastedYear in seq(startYear,endYear - 1,1)) {
    # Меня можно за это покорать, но это временная затычка
    # MOCK
    if (curr_forecastedYear > 2013) {
      DF[DF$year == curr_forecastedYear,]$deathCoef <-
        DF[DF$year == curr_forecastedYear - 1,]$deathCoef
      DF[DF$year == curr_forecastedYear,]$birthCoef <-
        DF[DF$year == curr_forecastedYear - 1,]$birthCoef
    }
    # ENDMOCK
    #Birth
    currentPopulation <- DF[DF$year == curr_forecastedYear,]
    
    currentPopulation$newbies <-
      (currentPopulation$population * currentPopulation$birthCoef / 1000) / 2
    cPNewbies <- currentPopulation %>%
      group_by(region, urban, sex) %>%
      dplyr::summarise(population = sum(newbies, na.rm = TRUE))
    cPNewbies[seq(2,nrow(cPNewbies),2),]$population <-
      cPNewbies[seq(1,nrow(cPNewbies),2),]$population
    
    #Death
    currentPopulation$deaths <-
      -(currentPopulation$population * (1 - currentPopulation$deathCoef))
    
    #Growth & everything
    currentPopulation$nextYearPopulation <- 0
    currentPopulation[currentPopulation$age == 5,]$nextYearPopulation <-
      cPNewbies$population +
      currentPopulation[currentPopulation$age == 5,]$population * (1 - 1 /
                                                                     5) +
      currentPopulation[currentPopulation$age == 5,]$deaths
    for (curr_age in seq(10, 100, 5)) {
      currentPopulation[currentPopulation$age == curr_age,]$nextYearPopulation <-
        currentPopulation[currentPopulation$age == curr_age - 5,]$population *
        (1 / 5) +
        currentPopulation[currentPopulation$age == curr_age,]$population *
        (1 - 1 / 5) +
        currentPopulation[currentPopulation$age == curr_age,]$deaths
    }
    DF[DF$year == curr_forecastedYear + 1,]$population <-
      currentPopulation$nextYearPopulation
  }
  return(DF)
}

dPyramid <- function(dataPopulDF, iyear, iregionCode, icolors = NULL) {
  #Загружаем данные
  demTreeData <- dataPopulDF %>%
    select(regionCode, age, sex, population, year) %>%
    filter(year == iyear, regionCode == iregionCode) %>%
    group_by(age, sex) %>%
    dplyr::summarise(popul = sum(population))
  demTreeData[demTreeData$sex == 1,]$popul <-
    -demTreeData[demTreeData$sex == 1,]$popul
  demTreeData$sex <- as.factor(unlist(lapply(demTreeData$sex, function(x) {
    if (x == 1)
      "M"
    else
      "Ж"
  })))
  demTreeData$abs <- abs(demTreeData$popul)
  demTreeData <- demTreeData[order(rev(demTreeData$age)),][order(demTreeData$sex),]
  demTreeData$age <- as.factor(demTreeData$age)
    
    d1 <- dPlot(
      x = "popul",
      y = "age",
      groups = "sex",
      data = demTreeData,
      type = 'bar'
    )
    
    # Мелкие настройки отображения
    d1$yAxis(type = "addCategoryAxis", orderRule = "ord")
    d1$xAxis(type = "addMeasureAxis")
    d1$legend(
      x = 60, y = 10, width = 150, height = 20, horizontalAlign = "right"
    )
    
    if (!is.null(colors)) {
      d1$colorAxis(type = "addColorAxis",
                   colorSeries = "gencode",
                   palette = colors)
    }
    if (length(year) > 1) {
      d1$set(storyboard = "Year")
      max_x <-
        round_any(max(demTreeData$popul), 10000, f = ceiling)
      min_x <-
        round_any(min(demTreeData$popul), 10000, f = floor)
      d1$xAxis(overrideMax = max_x, overrideMin = min_x)
    }
    
    if (max(demTreeData$popul   >= 1000000)) {
      d1$setTemplate(
        afterScript =
          "
        <script>
        x._getFormat = function () {
        return function(d) {
        return d3.format(',.1f')(Math.abs(d) / 1000000) + 'm';
        };
        };
        myChart.draw()
        </script>
        "
      )
    } else {
      d1$setTemplate(
        afterScript =
          "
        <script>
        x._getFormat = function () {
        return function(d) {
        return d3.format(',.0f')(Math.abs(d) / 1000) + 'k';
        };
        };
        myChart.draw()
        </script>
        "
      )
  }
    d1
    }

nPyramid <- function(dataPopulDF, iyear, icolors = NULL) {
    #Загружаем данные
    demTreeData <- dataPopulDF %>%
      select(age, sex, population, year) %>%
      filter(year == iyear) 
    demTreeData[demTreeData$sex == 1,]$population <- -demTreeData[demTreeData$sex == 1,]$population
    demTreeData <- transform(demTreeData,
                             sex = as.factor(unlist(lapply(demTreeData$sex, function(x) {if (x == 1) "M" else "Ж"}))),
                             age = as.factor(age),
                             population = as.numeric(as.character(population)))
    demTreeData <- demTreeData %>% group_by(age, sex) %>% summarize(popul = sum(population, na.rm = T))
    demTreeData <- demTreeData[order(rev(demTreeData$age)),][order(demTreeData$sex),]
    demTreeData$abs <- abs(demTreeData$popul)
    # names(demTreeData) <- c("Возраст", "Пол", "Численность", "abs")
    
    n1 <- nPlot(
      x = "age",
      y = "popul",
      group = "sex",
      type = 'multiBarHorizontalChart',
      data = demTreeData
    )
    
    n1$chart(stacked = TRUE)
    n1$chart(
      tooltipContent = "#! function(key, x, y, e){
      var format = d3.format('0,000');
      return '<h3>' + key + ', age ' + x + '</h3>' +
      '<p>' + 'Population: ' + format(e.point.abs) + '</p>'
  } !#"
    )
    if (max(demTreeData$popul >= 1000000)) {
      n1$yAxis(
        axisLabel = "Численность",
        tickFormat = "#! function(d) {
        return d3.format(',.1f')(Math.abs(d) / 1000000) + 'M'
    } !#"
      )
    } else {
      n1$yAxis(
        axisLabel = "Численность",
        tickFormat = "#! function(d) {
        return d3.format(',.0f')(Math.abs(d) / 1000) + 'K'
    } !#"
      )
      
    }
    if (!is.null(colors)) {
      n1$chart(color = icolors)
    }
    n1
}

shinyServer(function(input, output, session) {
  # Секция первого запуска приложения, инициализация и предподготовка
  geocodes_file <- "./Data/RussiaGeocodesForGoogleVis.csv"
  geocodes <- read.csv(geocodes_file)
  geocodes$Субъект.Федерации   <- brushUpRegions(geocodes$Субъект.Федерации)
  
  populationDF_file <- "./Data/demographyBlankData_v3.RDS"
  populationDF <- readRDS(populationDF_file, refhook = NULL)
  
  # Выбираем данные для демонстрации
  myData_v <-
    populationDF %>% select(population, regionCode, year) %>%
    group_by(year, regionCode) %>%
    summarize(population = sum(population, na.rm = TRUE))
  
  #   populationDF$population <- as.numeric(as.character(populationDF$population))
  #   populationDF <- simulateRF(populationDF, 2013, 2050)
  #   library(parallel)
  #   cl <- makeCluster(8)
  #   regionCode <- lapply(populationDF$region,
  #                           function(x) {
  #                             as.character(geocodes$Геокод[geocodes$Субъект.Федерации==as.character(x)])
  #                             })
  #   regionCode[lapply(regionCode, length)==0] <- "-"
  #   stopCluster(cl)
  #   populationDF$regionCode <- as.factor(unlist(regionCode))
  #   saveRDS(populationDF, populationDF_file)
  
  # Секция изменения модели
  mapOutputData <- reactive({
    selectedYear <- as.numeric(input$yearSlider)
    mapOutputData <- myData_v[myData_v$year == selectedYear,]
    mapOutputData
  })
  
  # Секция изменения UI
  output$googleVizMap <- renderGvis({
    # https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2#RU
    # https://en.wikipedia.org/wiki/ISO_3166-2:RU
    df <- mapOutputData()
    isolate({
      gvisGeoChart(
        df,
        locationvar = "regionCode", colorvar = "population",
        options = list(
          region = "RU", displayMode = "regions",
          resolution = "provinces",
          enableRegionInteractivity = TRUE,
          width = "100%", height = "100%",
          colorAxis = "{colors:['purple', 'red', 'orange', 'green']}",
          backgroundColor = "lightblue"
        )
      )
    })
  })
  
  output$demTree <- renderChart({
    selectedYear <- as.numeric(input$yearSlider)
    isolate({
      d1 <- nPyramid(populationDF, selectedYear, icolors = c("orange", "lightblue"))
      d1$params$width <- 410
      d1$set(dom = "demTree")
      d1
    })
  })
  
  output$population_RC <- renderChart({
    isolate({
      dataGR <- populationDF %>%
        select(year, age, population)
      dataGR <- transform(dataGR,
                          age = as.factor(age),
                          year = as.factor(year),
                          population = as.numeric(as.character(population)))
      dataGR <- dataGR %>% group_by(year, age) %>%
        summarize(popul = sum(population, na.rm = TRUE)) 
      
      population_RC <- nPlot(popul ~ year, group =  'age', data = dataGR, type = 'stackedAreaChart', id = "population_RC")
      population_RC$chart(useInteractiveGuideline=TRUE)
      if (max(dataGR$popul,na.rm = T) >= 1000000) {
        population_RC$yAxis(
          axisLabel = "Численность",
          tickFormat = "#! function(d) {
          return d3.format(',.2f')(Math.abs(d) / 1000000) + 'M' } !#"
        )
      } else {
        population_RC$yAxis(
          axisLabel = "Численность",
          tickFormat = "#! function(d) {
          return d3.format(',.0f')(Math.abs(d) / 1000) + 'K' } !#"
        )}
      population_RC$set(dom = "population_RC")
      population_RC
    })
  })
  
  output$birthCoefs_RC <- renderChart({
    selectedRegionCode <- input$region_SI
    isolate({
      dataGR <- populationDF %>%
        select(year, age, sex, birthCoef, regionCode) %>%
        filter(regionCode == selectedRegionCode, age >=20, age <= 50, sex == 0) %>%
        select(age, year, birthCoef)
      dataGR <- transform(dataGR,
                          age = as.factor(age),
                          year = as.factor(year),
                          birthCoef = as.numeric(as.character(birthCoef)))
      
      birthCoefs_RC <- nPlot(birthCoef ~ year, group = 'age', data = dataGR, type = 'lineChart', id = "birthCoefs_RC")
      birthCoefs_RC$yAxis(
          axisLabel = "Рождений на тысяцу женщин",
          tickFormat = "#! function(d) {
          return d3.format(',.2f')(Math.abs(d)) + ' ' } !#")
      birthCoefs_RC$set(dom = "birthCoefs_RC")
      birthCoefs_RC
    })
  })
  
  output$deathCoefs_RC <- renderChart({
    selectedRegionCode <- input$region_SI
    isolate({
      dataGR <- populationDF %>%
        select(year, age, deathCoef, regionCode) %>%
        filter(regionCode == selectedRegionCode) %>%
        select(age, year, deathCoef)
      dataGR <- transform(dataGR,
                          age = as.factor(age),
                          year = as.factor(year),
                          deathCoef = 1-as.numeric(as.character(deathCoef)))
      
      deathCoefs_RC <- nPlot(deathCoef ~ year, group = 'age', data = dataGR, type = 'lineChart', id = "deathCoefs_RC")
      deathCoefs_RC$chart(useInteractiveGuideline=TRUE)
      deathCoefs_RC$yAxis(
        axisLabel = "Вероятность умереть",
        tickFormat = "#! function(d) {
        return d3.format(',.0%')(Math.abs(d)) + ' ' } !#")
      deathCoefs_RC$set(dom = "deathCoefs_RC")
      deathCoefs_RC
      })
    })
  
})
