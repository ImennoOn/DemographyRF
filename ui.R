# ui.R
library(shiny)
library(rCharts)

Sys.setlocale(category = "LC_ALL", locale = "ru_RU.UTF-8")
txt <- "Демография"

htmlTagsAppend <- function(el, styleTags) {
  htmltools::tagAppendAttributes(el,
                                 style = styleTags
  )
}

textInputRow<-function (inputId, label, value = "", ...) 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value, class="input-mini"))
}

shinyUI(fluidPage(theme = "bootstrap.css",
  titlePanel(txt),
  
  sidebarLayout(
    sidebarPanel(
                  conditionalPanel('input.CurrentView === "Основной график"',
                                   h4("Demography forecast"),
                                   p("Kratkoe opisanie modeli"),
                                   selectInput('scenarioIn','Scenario:', choices = c("Optimistic", "Neutral", "Pessimistic")),
                                   checkboxInput('migration', 'Migration', value = T),
                                   br(),
                                   p("Opisanie modeli polnoe i voobwe tyt razmestit bolwe teksta pro podhod...")
                                   ),
                  conditionalPanel('input.CurrentView === "Рождаемость"',
                                   h4("Rogdaemost"),
                                   p("Opisanie podhoda k koef rogdaemosti")
                                   ),
                  conditionalPanel('input.CurrentView === "Смертность"',
                                   h4("Smertnost"),
                                   p("Opisanie podhoda k koef smertnosti")
                                   )
                  ),
    
    mainPanel(    
                  tabsetPanel( id = "CurrentView",
                               tabPanel(title = "Основной график",
                                        showOutput("graphDecompo","nvd3"),
                                        br()
#                                         showOutput("birthControlGraph","nvd3"),
#                                         showOutput("deathControlGraph","nvd3")
                                        ),
                               tabPanel(title = "Рождаемость",
                                        fluidRow(column(2,
                                                        br(),
                                                        br(),
                                                        br(),
                                                        numericInput(inputId = "c_b_1", label = NULL, value = 0),
                                                        br(),
                                                        numericInput(inputId = "c_b_2", label = NULL, value = 30),
                                                        br(),
                                                        numericInput(inputId = "c_b_3", label = NULL, value = 50),
                                                        br(),
                                                        numericInput(inputId = "c_b_4", label = NULL, value = 45),
                                                        br(),
                                                        numericInput(inputId = "c_b_5", label = NULL, value = 32),
                                                        br(),
                                                        numericInput(inputId = "c_b_6", label = NULL, value = 10),
                                                        br(),
                                                        numericInput(inputId = "c_b_7", label = NULL, value = 1)
                                                        ),
                                                 column(4,
                                                        showOutput("birthControlGraph","nvd3"))
                                                 )
                                        ),
                               tabPanel(title = "Смертность",
                                        fluidRow(column(2,
                                                        br(),
                                                        br(),
                                                        br(),
                                                        numericInput(inputId = "c_d_1", label = NULL, value = 0),
                                                        br(),
                                                        numericInput(inputId = "c_d_2", label = NULL, value = 30),
                                                        br(),
                                                        numericInput(inputId = "c_d_3", label = NULL, value = 50),
                                                        br(),
                                                        numericInput(inputId = "c_d_4", label = NULL, value = 45),
                                                        br(),
                                                        numericInput(inputId = "c_d_5", label = NULL, value = 32),
                                                        br(),
                                                        numericInput(inputId = "c_d_6", label = NULL, value = 10),
                                                        br(),
                                                        numericInput(inputId = "c_d_7", label = NULL, value = 1)
                                                        ),
                                                 column(4,
                                                        showOutput("deathControlGraph","nvd3")
                                                        )
                                                 )
                                        )
                               )
              )
              )
))
