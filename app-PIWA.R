#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(baseballr)
library(tidyverse)
library(dplyr)
# Define UI for application that draws a histogram
ui <- fluidPage(textInput(inputId = "ID",
                          label = "Enter Player MLBAM ID",
                          value = 545361),
                selectInput(inputId = "HitSide",
                            label = "Hitting Side",
                            choices = c("R", "L")),
                selectInput(inputId = "PitchSide",
                            label = "Pitcher Side",
                            choices = c("R", "L")),
                textOutput("PlayerTitle"),
                plotOutput("spraychart"),
                plotOutput("hitzone"),
                plotOutput("chase"),
                tableOutput("EV")
                
               
)

                
    # Sidebar with a slider input for number of bins 
   
        # Show a plot of the generated distribution

# Define server logic required to draw a histogram
server <- function(input, output) {
defined <- reactive(strsplit(input$player, " "))
# Player ID work around (PIWA) data <- reactive(playerid_lookup(last_name = defined()[[1]][2], first_name = defined()[[1]][1]) %>% arrange(desc(birth_year))
playerData <- reactive(scrape_statcast_savant_batter(start_date = "2022-01-01", end_date = Sys.Date(), batterid = input$ID) %>%
                         filter(stand == input$HitSide, p_throws == input$PitchSide))
BIP <- reactive(subset(playerData(), playerData()$type == "X"))
chaseData <- reactive(subset(playerData(), description %in% c("hit_into_play", "swinging_strike", "foul", "swinging_strike_blocked")) %>%
  filter(plate_x < -.71 | plate_x > .71 | plate_z < 1.5 | plate_z > 3.6))
PlayerTitle <- reactive(mlb_people(input$ID))

output$PlayerTitle <- renderText(paste("Offensive Summary for ", PlayerTitle()$full_name))
output$spraychart <- renderPlot(ggspraychart(playerData()))
output$hitzone <- renderPlot(ggplot(BIP(), aes(x = plate_x, y = plate_z, fill = events, color = events)) + geom_point() + geom_segment(x= -.71, xend = .71, y = 1.5, yend = 1.5, color = "black") + geom_segment(x= .71, xend = .71, y = 1.5, yend = 3.6, color = "black") + geom_segment(x= -.71, xend = .71, y = 3.6, yend = 3.6, color = "black") + geom_segment(x= -.71, xend = -.71, y = 3.6, yend = 1.5, color = "black") + coord_fixed(xlim = c(-2,2), ylim = c(0,5)))
output$chase <- renderPlot(ggplot(chaseData(), aes(plate_x, plate_z, color = events)) + geom_point() + geom_segment(x= -.71, xend = .71, y = 1.5, yend = 1.5, color = "black") + geom_segment(x= .71, xend = .71, y = 1.5, yend = 3.6, color = "black") + geom_segment(x= -.71, xend = .71, y = 3.6, yend = 3.6, color = "black") + geom_segment(x= -.71, xend = -.71, y = 3.6, yend = 1.5, color = "black") + coord_fixed(xlim = c(-2,2), ylim = c(0,5)))
output$EV <- renderTable(tibble(BIP() %>% group_by(pitch_name) %>% summarize(AvgEV = mean(launch_speed, na.rm = TRUE), AvgLA = mean(launch_angle, na.rm = TRUE), count = n())))

}

# Run the application 
shinyApp(ui = ui, server = server)
