library(shiny)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Index per country"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
      selectInput("var", "Choose a category:", choices = c("Index"= 'log_indexprice', "Unemployment Rate"= 'unemploymentrate')),
      # Input: Checkbox for whether outliers should be included ----
      #checkboxInput("outliers", "Show outliers", TRUE)
      
      selectInput("cont", "Choose a country:", choices = c("India" = 'India', "Japan" = 'Japan',
                                                           "China" = 'China', "Germany"= 'Germany',
                                                           "Hong Kong" = 'Hong Kong', "France" = 'France', "United Kingdom" = 'United Kingdom',
                                                           "United States of America" = 'United States of America'))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      "The dataset used for this project contains data for 9 countries (China, France, Germany, Hong Kong, India, Japan, Spain, United Kingdom and United States of America)
      from 1980 through 2020.
      It includes major economic factors like inflation, unemployment, GDP, exchange rate (base USD) and per capita income.
      Apart from that it has the stock prices of the respective country's major stock index which can help in analysing the data set to identify
      the impact of major macroeconomic variables on the movement of stock index prices and vice versa. We emphsaize that the visualized data here is only for the years 1991-2020.
      That is mainly for cleanness of that data and availability from about 1990 to 2020.",
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against mpg ----
      plotOutput("indexpricePlot"),
      
      plotOutput("allindexpricePlot")
      
    )
  ),
  
  
  tabPanel("Correlation Metrics",
           headerPanel("Correlations Between Different indicators for different countries"),
           mainPanel(
             "A graph that illustarates the correlation between the chosen catagories",
             selectInput('var0', "Select a country:", choices = c("India" = 'India', "Japan" = 'Japan', "Hong Kong" = 'Hong Kong',
                                                                  "China" = 'China', "Germany"= 'Germany', "France" = 'France', "United Kingdom" = 'United Kingdom',
                                                                  "United States of America" = 'United States of America')),
             
             selectInput('var1', "First Variable", choices=c("Index" = "log_indexprice", "Unemployment Rate"= 'unemploymentrate', "Per capita" = "percapitaincome_norm")),
             selectInput('var2', "Second Variable", choices=c("Index" = "log_indexprice", "Unemployment Rate"= 'unemploymentrate', "Per capita" = "percapitaincome_norm")),
             plotOutput('correlation')
             
           )
           
  )
  
  
  
)


