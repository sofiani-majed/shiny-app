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
