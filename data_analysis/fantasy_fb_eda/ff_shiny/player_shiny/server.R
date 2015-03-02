library(shiny)
library(ggplot2)
library(dplyr)

shinyServer(function(input, output){
  output$plot <- renderPlot({
    player <- off_pl_opp %>% filter(Player == input$player)
    gg <- ggplot(player, aes(Oppo_div, FFPts)) + geom_boxplot()
    print(gg)
  })
})