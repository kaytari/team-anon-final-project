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

# Does the same stuff as ui.R page
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
  # Filters data off inputs from user
  re_subset <- reactive({
    subset_df <- subset(filtered_df, filtered_df$occurred_date >= input$daterange[1] & filtered_df$occurred_date <= input$daterange[2])
    subset_df <- subset(subset_df, subset_df$sector == input$sectorInput) %>%
      return()
  })
  # creates bar graph of selected variable vs number of homicides
  output$barGraph <- renderPlot({
    re_subset()
    
    # types_selected <- filter(subset_df, offence %in% input$type)
    # types_selected$offence <- gsub("HOMICIDE-", "", types_selected$offence)
    ggplot(re_subset(), aes(x = offence, fill = neighborhood)) +
      geom_bar(stat = "count") + 
      ylab("Amount of Homicides") + xlab("") + ggtitle("Types of Homicides vs. Amount of Homicides") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    
  })
  
  # Creates a histogram of the counts of homicides and hour of day
  output$distPlot <- renderPlot({
    re_subset()
    ggplot(re_subset(), aes(x = occured_time, fill = neighborhood)) + geom_histogram(stat = 'count') +
      xlab("Hour of Day") + ylab("Amount of Homicides") + ggtitle("Graph of Homicides Commited at Corresponding Hours") 
    
  })
  
  # Prints the amount of homicides based off selected section and date range
  output$amounttt <- renderText( {
    sector_choice <- input$sectorInput
    re_subset()
    paste("There have been", nrow(re_subset()) ,"homicides in sector", sector_choice, "within your time frame.")
  })
  
  # Creates bar plot of count of homicides and neighborhoods
  output$neig_freq <- renderPlot({
    re_subset()
    ggplot(re_subset(), aes(x = neighborhood, fill = neighborhood)) +
      geom_bar(stat = "count")+
      labs(title = "Neighborhood v.s. Frequency",
           subtitle = "in each selected section")
    
  })
  
  # Creates a pie chart of the previous visualization
  output$pieGraph <- renderPlot({
    re_subset()
    neighborhoods <- group_by(re_subset(), neighborhood) %>%
      summarize(count = n())
    total_homicides <- sum(neighborhoods$count)
    print(total_homicides)
    lbls <- neighborhoods$neighborhood
    lbls <- paste(lbls, round(neighborhoods$count/total_homicides * 100, digits = 2), "%")
    pie(neighborhoods$count, labels = lbls, 
        main = "Piechart of Neighborhoods vs. Amount of Homicides")

  })
  
  # Provides credit for where we got the data
  output$credit <- renderText ({
    paste("Data set is provided by the City of Seattle, and can be accessed by visiting https://data.seattle.gov/Public-Safety/Crime-Data/4fs7-3vj5")
  })
  
  # Provides information for first plot
  output$how <- renderText ({
    paste("Through this graph we are able to provide users an easy way to visually see the different types of homicides that are commited.
          Additionally, since the graph is subsetted based off the user's desired section and time frame, users are able to see information from specific sectors.
          In organizing the visualization with the color of each stacked bar corresponding to a different neighborhood, user's can see which areas are contain each type of homicide.")
  })
  
  # Provides information for second plot
  output$when <- renderText ({
    paste("This histogram provides the user with a visual representation of the actual time homicides are occuring. Initially, each bar was filled to correspond with each hour, 
          however since we are able to subset the data by a specific date frame and sector, we were able to provide users occured times for homicides based on hour of the day and neighborhood the event occured.
          By providing the user with this data, they can identify which neighborhoods have high rates of homicides at specific hours. For example, the First Hill neighborhood in the E sector has more homicides 
          commited at 11:00am rather than Capital Hill that has no homicides commited in the daytime. ")
  })
  
  # Provides information for third and forth plot
  output$where <- renderText ({
    paste("This bar chart presents the homicides commited through the specific lens of location. While the other visualizations provide data that pertains to neighborhoods in the area, 
          this visualization by itself provides the user with an organized view of the amount of homicides committed in each area. A potential use of this visualization is for potential 
          homeowners in the area that want a brief overview of murders in an area they might want to move into.")
  })
}
shinyServer(server)