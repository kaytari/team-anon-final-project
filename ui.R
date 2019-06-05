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
offence<- unlist(filtered[7])
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

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h1("View Data Based off Sector and Time Frame"),
      ## Allows users to select which variable to examine homicide data over 
      ## Precinct, Location Sector, or Neighborhood
      selectInput("sectorInput",
                  "Choose a Sector:",
                  choices = c("B", "C", "D", "E","F", "G", "J", "K", "L", "M", "N", "O", "Q", "R", "S", "U", "W")
      ),
      
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
    mainPanel(
      h2("What's the number homicides occured in this sector and time frame?"),
      textOutput("amounttt"),
      h2("How are homicides commited?"),
      plotOutput("barGraph"),
      textOutput("how"),
      h2("When are they most frequent?"),
      plotOutput("distPlot"),
      textOutput("when"),
      h2("Where are they happening?"),
      plotOutput("neig_freq"),
      textOutput("where"),
      br(),
      br(),
      plotOutput("pieGraph"),
      textOutput("credit")
    )
  )
)
shinyUI(ui)