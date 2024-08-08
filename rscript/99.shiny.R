install.packages("shiny")
# install.packages("devtools")
devtools::install_github("hadley/ggstat")
library(shiny)
library(plotly)
library(ggstat)

plotly_example("shiny", "event_data_persist")
plotly_example("shiny", "crossfilter")
#!!
plotly_example("shiny", "drag_brush")
