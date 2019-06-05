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

my_server <- function(input, output){
  # creates table of data to use when plotting
  get_count <- reactive({
    df_summary <- filtered_df %>%
      select(input$variable) %>%
      table() 
  })
  # color palette of light blue to blue
  pal <- reactive({ 
    pal1 <- colorRampPalette(colors = c("lightblue", "blue"))(nrow(get_count()))
  })
  # creates bar graph of selected variable vs number of homicides
  output$barGraph <- renderPlot({
    par(mar = c(8, 4, 2, 2) + 1)
    
    end_point = 0.5 + nrow(get_count()) + nrow(get_count())-1
    
    barplot(get_count(), ylab = "Number of Homicides", 
            col = pal(),
            main = paste("homicides vs.", input$variable), xaxt = "n", space = 1)
    
    text(seq(1.5,end_point,by=2), par("usr")[3]-0.25, 
         srt = 50, adj= 1, xpd = TRUE,
         labels = paste(rownames(get_count())), cex=0.65)
  })
}

shinyServer(my_server)