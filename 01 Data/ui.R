#ui.R 

library(shiny)

# Define UI for application that plots random distributions 
shinyUI(fluidpage(title = "Major League Baseball Statisitics",
  tabsetPanel(
    tabPanel(title = "Home Run Hitters Scatterplot", plotOutput("scatplot"))
    tabPanel(title = "Power Hitting Barchart", plotOutput("barplot"))
    tabPanel(title = "Crosstab Avg. On Base Percentage for Texas Rangers", plotOutput("crossplot") )
  )))

  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("KPI1", 
                "KPI_Low_Max_value:", 
                min = 1,
                max = 4750, 
                value = 4750),
    sliderInput("KPI2", 
                "KPI_Medium_Max_value:", 
                min = 4750,
                max = 5000, 
                value = 5000)
