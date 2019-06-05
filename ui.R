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
      h1("Hello :)"),
      ## Allows users to select which variable to examine homicide data over 
      ## Precinct, Location Sector, or Neighborhood
      checkboxGroupInput("type",
                         "Select Type of Homicide(s):",
                         choices = c("Premeditated-BODYFORCE" = "HOMICIDE-PREMEDITATED-BODYFORCE",
                                     "Premidated-WEAPON" = "HOMICIDE-PREMEDITATED-WEAPON",
                                     "Premidated-GUN" = "HOMICIDE-PREMEDITATED-GUN",
                                     "Neg-Mans-BODYFORCE" = "HOMICIDE-NEG-MANS-BODYFORCE",
                                     "Neg-Mans-VEHICLE" = "HOMICIDE-NEG-MANS-VEHICLE",
                                     "Neg-Mans-WEAPON" = "HOMICIDE-NEG-MANS-WEAPON"),
      ),
      
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
      plotOutput("barGraph"),
      plotOutput("distPlot"),
      plotOutput("neig_freq"),
      textOutput("amounttt")
    )
  )
)
shinyUI(ui)