library(shiny)
library(ggplot2)
library(dplyr)


shinyServer(function(input, output){

  output$choose_pos <- renderUI({
      selectInput("position", "Choose the offensive position:", 
                choices = c("QB", "RB", "WR", "TE", "K"),
                selected = "QB")
            })

# creating football statistic selection list for selected position and removing unneccesary variables
# removing variables "Player", "Team", "G" as well as "Week", "Year", "Position", "Oppo"
#
output$choose_var <- renderUI({
  
        dat <- switch(input$position,
                  "QB" = qb_10_14,
                  "RB" = rb_10_14,
                  "WR" = wr_10_14,
                  "TE" = te_10_14,
                  "K" = k_10_14
                        )
        coln <- names(dat)[-c(1:3)]
        coln <- head(coln, -4)
    selectInput("var", "Choose the football statistic:",
                choices = coln,
                selected = coln[1])
              })
  

  output$plot <- renderPlot({
    dat <- switch(input$position,
                  "QB" = qb_10_14,
                  "RB" = rb_10_14,
                  "WR" = wr_10_14,
                  "TE" = te_10_14,
                  "K" = k_10_14)
  
# code to print out error if 
    validate(
            need(c(input$player1) %in% dat$Player, "Please select a player (Player 1) for the position."),
            need(c(input$player2) %in% dat$Player, "Please select a player (Player 2) for the position.")
    )

# creating a data frame for the selected position and fb statistic for 2014    
    dat_2014 <- filter(dat, Year == 2014)
    sel_dat1 <- dat_2014[, c("Player", "Team", "Week", "Oppo", input$var)]
    colnames(sel_dat1) <- c("Player", "Team", "Week", "Oppo", "var1")

# attaching to the previous dataframe rows for the bye weeks of the players    
    pl_team<-unique(select(sel_dat1, Player, Team))
    pl_bye<-inner_join(pl_team, byes)
    pl_bye$var1 <- NA
    sel_dat1 <- rbind(sel_dat1, pl_bye)

# choosing only the rows that apply to each respective player chosen
    pl_dat1 <- sel_dat1 %>% filter(Player %in% c(input$player1))
    pl_dat2 <- sel_dat1 %>% filter(Player %in% c(input$player2))
    
# code to account for players who either did not play for that week or was
# not in the top 50 scoring players for that position
# places a "DNP" for the Opponent so opponents can be shown on the x-axis
#

    pl1_week <- sort(unique(as.numeric(as.character(pl_dat1$Week))))
    pl2_week <- sort(unique(as.numeric(as.character(pl_dat2$Week))))
    
    if(length(pl1_week) != 17){
      one_17 <- 1:17
      not_in <- one_17[!(one_17 %in% pl1_week)]
      dnp_df <-cbind.data.frame(Player= input$player1, 
                                Team = unique(pl_dat1$Team), 
                                Week = not_in, 
                                Oppo = "DNP", var1 = NA)
        pl_dat1 <- rbind(pl_dat1, dnp_df)
                                }
    if(length(pl2_week) != 17){
      one_17 <- 1:17
      not_in <- one_17[!(one_17 %in% pl2_week)]
      dnp_df <-cbind.data.frame(Player= input$player2, 
                                Team = unique(pl_dat2$Team), 
                                Week = not_in, 
                                Oppo = "DNP", var1 = NA)
      pl_dat2 <- rbind(pl_dat2, dnp_df)
                          }
# code to combine the two player data frames into one    
    both_pl <- rbind(pl_dat1, pl_dat2)
    both_pl_s <- both_pl %>% arrange(Week, Player)
    
# code to create labels for the opponents that can be stacked on top of each other
# for each week (i.e. opponents for one player are all on one line; opponents for other
# player all on the other line)
    opp <- as.character(unlist(both_pl_s[,c("Oppo", "Week")][1]))
    opp1 <- opp[seq(1,length(opp),2)]
    opp2 <- opp[seq(2,length(opp),2)]
    lab <- paste(opp1, opp2, sep="\n")
    brk <- unique(as.numeric(both_pl_s$Week))

# removing data points that are NA for selected football statistic
    clean_dat <- both_pl_s[!(is.na(both_pl_s$var1)),]
print(clean_dat)
# printing line plot for selected players and the selected statistic
    gg <- ggplot(clean_dat, aes(Week, var1, color = Player, group = Player)) + 
                geom_line() + 
                geom_point(size = 3) + 
                geom_text(aes(label = var1), size = 3, vjust = -1.5) +
                ylab(input$var) + 
                xlab("Opponent") + 
                ggtitle(paste(input$var, "for", input$player1, "and", input$player2, "for Each Week", sep = " ")) +
                scale_x_discrete(breaks = brk , labels = lab)
      print(gg)
              })
    })