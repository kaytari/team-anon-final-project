library(shiny)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(lubridate)
library(xts)
#Loading in data and filtering out all cells that don't corerespond to homicides
#Also removed events that occured before 1/1/09
bcl <- read.csv("data/Crime_Data.csv", stringsAsFactors = FALSE)
filtered <- bcl %>% filter(bcl$Crime.Subcategory == "HOMICIDE")
filtered <- filtered[-c(1:37), ]

##Flattiing out data set from list format into data frame
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
## Fixes how dates are stored in data frame
holder <- (filtered_df$occurred_date)
holder2 <- mdy(holder)

filtered_df$occurred_date <- holder2

filtered_df$reported_date <- mdy(filtered_df$reported_date)

# Rounds time to nearest hour and only takes first 2 numbers
temp <- filtered_df$occured_time 
temp <- signif(temp, digits = 2)
temp2 <- mapply(function(x, y) paste0(rep(x, y), collapse = ""), 0, 4 - nchar(temp))
temp <- paste0(temp2, temp)
temp <- substr(temp, 1, 2)

filtered_df$occured_time <- temp

ui <- fluidPage(
  titlePanel(title = h1("Homicide Data in Seattle")),
  
  sidebarLayout(
    sidebarPanel(
      ## Allows users to select which variable to examine homicide data over 
      ## Precinct, Location Sector, or Neighborhood
      selectInput("variable",
                  "Choose a variable:",
                  choices = c("Type of Homicide" = "offence",
                            "Precinct" = "precinct",
                            "Sector" = "sector",
                            "Neighborhood" = "neighborhood")
      ),
      
      ## Allows users to select a range of dates to examine how homicides occur over span of day
      dateRangeInput(
        inputId = "daterange",
        label = "Select the occured date range",
        start = min(filtered_df$occurred_date),
        end = max(filtered_df$occurred_date),
        min = min(filtered_df$occurred_date),
        max = max(filtered_df$occurred_date),
        format = "yyyy/mm/dd",
        separator = "-"
      )
   ),
   
    ## Displays Bar Graph, then histogram, the last graph
    mainPanel(
      plotOutput("barGraph"),
      plotOutput("distPlot"),
      textOutput("amounttt")
    )
  )
)

shinyUI(ui)