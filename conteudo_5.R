library(shiny)
library(tidyverse)
library(shinyWidgets)
library(highcharter)
library(reactable)



source(file = 'R/scatter_chart_function.R')
source(file = 'R/bar_chart_function.R')
source(file = 'R/table_function.R')

source(file = 'R/modules/module_league_level.R')


ui <- fluidPage(
  div(
    class = 'main_title_div',
    h1(class = 'main_title',
       'Premiere League 2020 - Dashboard')
  ),
  
  includeCSS(path = 'www/css/style.css'),
  
  module_league_UI('league_level')
  
  
)

server <- function(input, output, session) {
  module_league_Server('league_level')
}

shinyApp(ui, server)
