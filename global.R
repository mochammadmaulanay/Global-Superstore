library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(scales)
library(echarts4r)
library(highcharter)
library(htmlwidgets)

df <- read_rds("Global_Superstore.rds")


source("ui.R")
source("server.R")
shinyApp(ui,server)