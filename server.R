#this is my special server.R
library(shiny)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(lubridate)
library(xts)
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

filtered_df <- data.frame(report_number, occurred_date, occured_time, reported_date, reported_time, 
                          crime_subcategory, offence, precinct, sector, beat,
                          neighborhood)
holder <- (filtered_df$occurred_date)
holder2 <- mdy(holder)

filtered_df$occurred_date <- holder2

filtered_df$reported_date <- mdy(filtered_df$reported_date)

temp <- filtered_df$occured_time 
temp <- signif(temp, digits = 2)
temp2 <- mapply(function(x, y) paste0(rep(x, y), collapse = ""), 0, 4 - nchar(temp))
temp <- paste0(temp2, temp)
temp <- substr(temp, 1, 2)

filtered_df$occured_time <- temp

server <- function(input, output) {
  # creates bar graph of selected variable vs number of homicides
  output$barGraph <- renderPlot({
    subset_df <- subset(filtered_df, filtered_df$occurred_date >= input$daterange[1] & filtered_df$occurred_date <= input$daterange[2])
    subset_df <- subset(subset_df, subset_df$sector == input$sectorInput)
    
    # types_selected <- filter(subset_df, offence %in% input$type)
    # types_selected$offence <- gsub("HOMICIDE-", "", types_selected$offence)
    ggplot(subset_df, aes(x = offence, fill = neighborhood)) +
      geom_bar(stat = "count") + 
      ylab("Amount of Homicides") + xlab("") + ggtitle("Types of Homicides vs. Amount of Homicides") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    
  })
  
  output$distPlot <- renderPlot({
    subset_df <- subset(filtered_df, filtered_df$occurred_date >= input$daterange[1] & filtered_df$occurred_date <= input$daterange[2])
    subset_df <- subset(subset_df, subset_df$sector == input$sectorInput)
    ggplot(subset_df, aes(x = occured_time, fill = neighborhood)) + geom_histogram(stat = 'count') +
      xlab("Hour of Day") + ylab("Amount of Homicides") + ggtitle("Graph of Homicides Commited at Corresponding Hours") 
    
  })
  
  output$amounttt <- renderText( {
    sector_choice <- input$sectorInput
    subset_df <- subset(filtered_df, filtered_df$occurred_date >= input$daterange[1] & filtered_df$occurred_date <= input$daterange[2])
    subset_df <- subset(subset_df, subset_df$sector == input$sectorInput)
    paste("There have been", nrow(subset_df) ,"homicides in sector", sector_choice, "within your time frame.")
  })
  
  output$neig_freq <- renderPlot({
    subset_df <- subset(filtered_df, filtered_df$occurred_date >= input$daterange[1] & filtered_df$occurred_date <= input$daterange[2])
    subset_df <- subset(subset_df, subset_df$sector == input$sectorInput)
    ggplot(subset_df, aes(x = neighborhood, fill = neighborhood)) +
      geom_bar(stat = "count")+
      labs(title = "Neighborhood v.s. Frequency",
           subtitle = "in each selected section")
    
  })
  
  output$credit <- renderText ({
    paste("Data set is provided by the City of Seattle, and can be accessed by visiting https://data.seattle.gov/Public-Safety/Crime-Data/4fs7-3vj5")
  })
}
shinyServer(server)