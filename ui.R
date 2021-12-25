header <- 
  dashboardHeader(
    title = "Sales Dashboard"
  )

sidebar <- 
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Overview", tabName = "overview", icon = icon("home")),
      menuItem(text = "Map", tabName = "map", icon = icon("globe")),
      menuItem(text = "Country", tabName = "country", icon = icon("map-marker-alt")),
      menuItem(text = "About", tabName = "about", icon = icon("user"))
    )
  )


body <- 
  dashboardBody(
    # Tag CSS - Start ----------------------------------------------------------
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      # Tag CSS - End ----------------------------------------------------------
    ),
    
    
    # Tab Content - Start ------------------------------------------------------
    tabItems(
      # Overview Tab - Start ---------------------------------------------------
      tabItem(
        tabName = "overview",
        fluidPage(
          fluidRow(
            valueBoxOutput(width = 4, outputId = "overviewSalesGrowth"),
            valueBoxOutput(width = 4, outputId = "overviewProfitGrowth"),
            valueBoxOutput(width = 4, outputId = "overviewSalesSeason")
          ),
          
          fluidRow(
            box(
              width = 6,
              echarts4rOutput(outputId = "overviewProfitByMarket")
            ),
            box(
              width = 6,
              echarts4rOutput(outputId = "overviewProfitBySegment")
            )
          ),
          
          fluidRow(
            box(
              width = 6,
              echarts4rOutput(outputId = "overviewProfitByCategory")
            ),
            box(
              width = 6,
              echarts4rOutput(outputId = "overviewProfitMissed")
            )
          )
        )
        # Overview Tab - End ---------------------------------------------------
      ),
      
      
      # Map Tab - Start --------------------------------------------------------
      tabItem(
        tabName = "map",
        fluidPage(
          fluidRow(
            box(
              background = "black",
              width = 4,
              height = 80,
              selectInput(
                inputId = "valueSelector",
                label = "Select value:", 
                choices = c("Number of transactions", "Profit", "Sales"),
                selected = "Profit"
              )
            ),
            box(
              background = "black",
              width = 4,
              height = 80,
              selectInput(
                inputId = "categorySelector",
                label = "Select category :",
                choices = sort(unique(df$Category)),
                selected = "Furniture", 
              )
            ),
            box(
              background = "black",
              width = 4,
              height = 80,
              selectInput(
                inputId = "yearSelector",
                label = "Select year :",
                choices = sort(unique(df$Year)),
                selected = "2014"
              )
            )
          ),
          
          fluidRow(
            box(
              width = 12,
              height = 480,
              echarts4rOutput(outputId = "salesMap")
            )
          )
        )
        # Map Tab - End --------------------------------------------------------
      ),
      
      
      # Country Tab - Start ----------------------------------------------------
      tabItem(
        tabName = "country",
        fluidPage(
          fluidRow(
            box(
              width = 4,
              height = 80,
              background = "black",
              selectInput(inputId = "countrySelector", 
                          label = "Select country:", 
                          choices = sort(unique(df$Country)), 
                          selected = "Indonesia")
            ),
            box(
              width = 4,
              height = 80,
              background = "black",
              dateRangeInput(inputId = "dateSelector", 
                             label = "Select range:", 
                             start = "2011-01-01", 
                             end = "2014-12-01")
            ),
            box(
              width = 4,
              height = 80,
              background = "black",
              selectInput(inputId = "categoryCheckSelector", 
                          label = "Select category:", 
                          choices = sort(unique(df$Category)), 
                          selected = "Furniture",
                          multiple = TRUE)
            )
          ),
          
          fluidRow(
            box(
              width = 12, 
              echarts4rOutput(outputId = "countrySales")
            )
          ),
          
          fluidRow(
            box(
              width = 12, 
              echarts4rOutput(outputId = "countryProfit")
            )
          ),
          
          fluidRow(
            box(
              width = 6,
              echarts4rOutput(outputId = "topSubcategory")
            ),
            box(
              width = 6,
              echarts4rOutput(outputId = "shippingStats")
            )
          )
        )
        # Country Tab - End ----------------------------------------------------
      ),
      # About Tab- Start ------------------------------------------------------
      tabItem(
        tabName = "about",
        fluidPage(
          h1("Global Superstore Visualization"),
          h5("By ", a("Mochammad Maulana Yusuf", href = "https://www.linkedin.com/in/mochammadmaulana/")),
          h2("Dataset"),
          p("This visualization was made using Global Superstore dataset which
            provided by Chandra Shekhar, published on ", 
            a("Kaggle.", href = "https://www.kaggle.com/shekpaul/global-superstore")),
          h2("Library"),
          p("Several R library that used to create this visualization are:"),
          p("-  Tidyverse"),
          p("-  Lubridadate"),
          p("-  Scales"),
          p("-  Shiny"),
          p("-  Shinydashboard"),
          p("-  Echarts4r"),
          p("-  Highcharter"),
          p("-  htmlwidgets"),
          h2("Code"),
          p("Find out my code in ", a("GitHub", href = "https://github.com/kularudi/Capstone_Algoritma_Academy/tree/main/Data_Visualization"))
        )
        # About Tab - End ------------------------------------------------------
      )
    )
  )

# Arrange Page -----------------------------------------------------------------
ui <- dashboardPage(header, sidebar, body)