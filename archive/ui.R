# ui.R
library(shiny)
library(shinydashboard)
library(rCharts)

Sys.setlocale(category = "LC_ALL", locale = "ru_RU.UTF-8")
txt <- "Demography.rf"

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


shinyUI(fluidPage(includeHTML(paste(getwd(),"/www/head.html",sep="")),
                  includeHTML(paste(getwd(),"/www/navbar.html",sep="")),
                  includeHTML(paste(getwd(),"/www/carousel.html",sep="")),
                  HTML('    <!-- Marketing messaging and featurettes
    ================================================== -->
    <!-- Wrap the rest of the page in another container to center all the content. -->

    <div class="container marketing">'),
                  includeHTML(paste(getwd(),"/www/threeCol.html",sep="")),
                  fluidRow(column(width = 12, height = 700,
                                  sliderInput("i_mapMultiplier", "Multiply", 1, 15, 1),
                                  htmlOutput("googleVizChart"))),
                  includeHTML(paste(getwd(),"/www/navbar.html",sep="")),
                  includeHTML(paste(getwd(),"/www/carousel.html",sep="")),
#                   HTML('<hr class="featurette-divider">'),
#                   showOutput("graphDecompo","nvd3"),
#                   HTML('<hr class="featurette-divider">'),
#                   HTML('
#     </div>
#     <!-- /.container -->'),
                  includeHTML(paste(getwd(),"/www/modalforms.html",sep=""))
                  ))