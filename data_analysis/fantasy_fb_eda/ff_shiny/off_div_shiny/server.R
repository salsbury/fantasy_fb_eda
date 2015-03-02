library(shiny)
library(ggplot2)
library(dplyr)
load("../ff_df/off_pl_opp.RData")

shinyServer(function(input, output){
    output$plot <- renderPlot({
                    team <- off_pl_opp %>% filter(Team == input$team)
                      gg <- ggplot(team, aes(Oppo_div, FFPts, color = Position)) + 
                        geom_boxplot() + 
                          xlab("Divisions") + 
                          ggtitle(paste("Positional Fantasy Points for", input$team))
                    print(gg)
                      })
              })