library(shiny)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(lubridate)
library(xts)
library(tidyr)
bcl <- read.csv("data/Crime_Data.csv", stringsAsFactors = FALSE)
filtered <- bcl %>% filter(bcl$Crime.Subcategory == "HOMICIDE")
filtered <- filtered[-c(1:37), ]

report_number <- unlist(filtered[1])
occurred_date <- unlist(filtered[2])
occured_time <- unlist(filtered[3])
reported_date <- unlist(filtered[4])
reported_time <- unlist(filtered[5])
crime_subcategory <- unlist(filtered[6])
offence <- unlist(filtered[7])
precinct <- unlist(filtered[8])
sector <- unlist(filtered[9])
beat <- unlist(filtered[10])
neighborhood <- unlist(filtered[11])

filtered_df2 <- data.frame(report_number, occurred_date, occured_time, reported_date, reported_time, 
                           crime_subcategory, offence, precinct, sector, beat,
                           neighborhood)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    subset_df <- subset(filtered_df, filtered_df$occurred_date >= input$daterange[1] & filtered_df$occurred_date <= input$daterange[2])
    subset_df <- subset(subset_df, subset_df$sector == input$sectorInput)
    View(subset_df)
    ggplot(subset_df, aes(x = occured_time, fill = occured_time)) + geom_histogram(stat = 'count') +
      xlab("Hour of Day") + ylab("Amount of Homicides") + ggtitle("Graph of Homicides Commited at Corresponding Hours") 
    
  })
  
  output$amounttt <- renderText( {
    subset_df <- subset(filtered_df, filtered_df$occurred_date >= input$daterange[1] & filtered_df$occurred_date <= input$daterange[2])
    paste("There have been", nrow(subset_df) ,"homicides in Seattle within your time frame")
  })
  
  output$neig_freq <- renderPlot({
    newdata <- select(subset_df, sector, neighborhood)
    newdata1 <- filter(subset_df, sector == input$sectorInput)
    newdata2 <- count(newdata1$neighborhood)
    colnames(newdata2) <- c("neighborhood", "freq")
    newdata3 <- newdata2[order(newdata2$freq), ]
    ggplot(newdata3, aes(x = neighborhood, y = freq))+
      geom_bar(stat = "identity", width = .5, fill="tomato2")+
      labs(title = "Neighborhood v.s. Frequency",
           subtitle = "in each selected section")

  })

}
shinyServer(server)