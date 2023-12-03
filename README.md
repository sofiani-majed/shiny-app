# shiny-app

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






server <- function(input, output) {

  
  
  
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(magrittr)
  library(tidyverse)
  
  data <- read.csv("Economic Data - 9 Countries (1980-2020).csv")
  
  
  data_clean<-filter(data, year >1990, !is.na(unemploymentrate), !is.na(percapitaincome), !is.na(log_indexprice))
  
  data_clean_norm <- data_clean %>% mutate(percapitaincome_norm=percapitaincome/mean(percapitaincome, na.rm=TRUE)) 
  
  
  
  data_clean_india<-filter(data_clean_norm, country == "India")
  data_clean_china<-filter(data_clean_norm, country == "China")
  data_clean_germany<-filter(data_clean_norm, country == "Germany")
  data_clean_japan<-filter(data_clean_norm, country == "Japan")
  data_clean_hongkong<-filter(data_clean_norm, country == "Hong Kong")
  data_clean_france<-filter(data_clean_norm, country == "France")
  data_clean_kingdom<-filter(data_clean_norm, country == "United Kingdom")
  data_clean_usa<-filter(data_clean_norm, country == "United States of America")
  data_clean_spain<-filter(data_clean_norm, country == "Spain")
  
  
  
  
  
    
  
  
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste("Year ~", input$var)
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  
  
  
  
  output$indexpricePlot <- renderPlot({DD<-filter(data_clean_norm, country == input$cont)
  ggplot(DD, aes(year, DD[,input$var], size=3), col = "#75AADB") +
    xlab("Year") + ylab(input$var) +
    geom_point()
  
  
  }) 
  
  ##ALL COUNTRIES TOGETHER
  output$allindexpricePlot <- renderPlot({
    ggplot(data_clean, aes(year, data_clean_norm[,input$var], color=factor(country), size=3), col = "#75AADB") +
      xlab("Year") + ylab(input$var) +
      geom_point()    
    
    
    #hist(data_clean[,input$var],
    #outline = input$outliers,
    #col = "#75AADB", pch = 19)
  })
  #as.numeric(formulaText()) 
  
  output$correlation<- renderPlot({DD<-filter(data_clean_norm,
                                              year >1990,
                                              !is.na(unemploymentrate),
                                              !is.na(percapitaincome), !is.na(log_indexprice), country == input$var0)
  req(input$var1)
  req(input$var2)
  #x <-DD[,input$var1]
  #y <- DD[input$var2]
  fit1 <- lm(DD[,input$var1]~DD[,input$var2])
  plot(DD[,input$var1]~DD[,input$var2] , xlab=input$var1, ylab=input$var2, type='p', col = "red")
  abline(fit1)
  })
}

# Create Shiny app ----
#shinyApp(ui, server)
