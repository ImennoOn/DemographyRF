library(shiny)
library(shinydashboard)
library(rCharts)

# Choices for drop-downs
mapOutput_list <- c(
  "Численность населения" = "dd_population",
  "Индекс обеспеченности" = "dd_indexOb",
  "Прирост/убыть населения" = "dd_pripost",
  "Смертность" = "dd_deaths",
  "Рождаемость" = "dd_births"
)
scenario_list <- c(
  "Позитивный" = "scenarioH",
  "Позитивно-умеренный" = "scenarioHM",
  "Умеренный" = "scenarioM",
  "Негативный" = "scenarioL"
)
regions_list <- c(
  "Москва" = "RU-MOS",
  "Московская область" = "RU-MOW"
)

shinyUI(navbarPage("Демографическая карта РФ", id="nav",

  tabPanel("Интерактивная карта",
    div(class="outer",
        tags$head(
        # Include our custom CSS
        includeCSS("styles.css")
        ),
        htmlOutput("googleVizMap"),
        absolutePanel(id = "yearContlor", class = "panel panel-default", fixed = TRUE,
                      draggable = TRUE, top = 80, left = 80, right = "auto", bottom = "auto",
                      width = 630, height = "auto", 
                      align = "center",
                      sliderInput("yearSlider", "", 1989, 2050, 2015, width = "95%")),
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                      draggable = TRUE, top = 80, left = "auto", right = 40, bottom = "auto",
                      width = 450, height = "auto",
                      h2("Карта"),
                      selectInput("mapOutputOption", "Цветовая шкала:", mapOutput_list),
                      selectInput("scenario", "Сценарий:", scenario_list, selected = "adultpop"),
                      conditionalPanel("input.mapOutputOption == 'dd_population'",
                                       div("Тут по понятным причинам нет нормального текста, но есть функционал для тыц-тыц по выпадающим спискам...")
                                       ),
                      conditionalPanel("input.scenario != 'scenarioH'",
                                       div(".. вот и сценарий меняться должен, но не меняется")),
                      showOutput("demTree", "nvd3") #dimple nvd3
                      ),
        tags$div(id="cite",
                 'Под лицензией ', tags$em('cc by-nc'), '. 2015'
                 )
        )
    ),
  
  tabPanel("Данные",
           tags$head(tags$style('.col-sm-10 {padding-left: 150px; padding-right: 20px ;}')),
           fluidRow(box(width = 10,
                        showOutput("population_RC", "nvd3")),
                    box(width = 10,
                        div(id="sDiv", " "),
                        selectInput("region_SI", "Регион:", regions_list),
                        h5("Рождений на 1000 женщин"),
                        showOutput("birthCoefs_RC", "nvd3"),
                        h5("Вероятность умереть"),
                        showOutput("deathCoefs_RC", "nvd3"))
                    )
           ),
  
  tabPanel("Выводы",
           tableOutput("debugInfo_geo"),
           tableOutput("debugInfo_popul")),
  
  tabPanel("Методика"),
  
  conditionalPanel("false", icon("crosshair"))
))
