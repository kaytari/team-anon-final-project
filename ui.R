library(shiny)
library(ggplot2)
library(dplyr)
library(ggthemes)
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
my_ui <- fluidPage(
  titlePanel(title = h1("Homicide Data in Seattle")),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("variable",
                  "Choose a variable:",
                  choices = c("Type of Homicide" = "offence",
                            "Precinct" = "precinct",
                            "Sector" = "sector",
                            "Neighborhood" = "neighborhood")
      )
   ),
    mainPanel(
      plotOutput("barGraph")
    )
  )
)

shinyUI(my_ui)