library(shiny)
library(ggplot2)
library(dplyr)

load("../ff_df/off_pl_opp.RData")

shinyServer(function(input, output){

  output$plot <- 
    renderPlot({
      if(input$off_def == "Defense"){
          team <- off_pl_opp %>% filter(Oppo == input$team)
            gg <- ggplot(team, aes(Week, FFPts, fill = Position)) + 
                    geom_bar(stat = "Identity") + 
                      scale_x_discrete(breaks = unique(as.numeric(team$Week)), 
                          labels = as.character(unlist(unique(team[,c("Team", "Week")])[1]))) + 
                        xlab("Opponents") + 
                          ggtitle(paste("Total Number of Fantasy Points Given up by", input$team))
              print(gg)
                  }
      else {
          team <- off_pl_opp %>% filter(Team == input$team)
            gg <- ggplot(team, aes(Week, FFPts, fill = Position)) + 
                    geom_bar(stat = "Identity") + 
                      scale_x_discrete(breaks = unique(as.numeric(team$Week)), 
                          labels = as.character(unlist(unique(team[,c("Oppo", "Week")])[1]))) + 
                        xlab("Opponents") + 
                          ggtitle(paste("Total Number of Fantasy Points Scored by", input$team))
              print(gg)
              }    
          })
              })