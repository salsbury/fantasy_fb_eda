library(shiny)
library(ggplot2)
library(dplyr)


shinyServer(function(input, output){
  output$choose_pos <- renderUI({
      selectInput("position", "Choose the offensive position:", 
                choices = c("QB", "RB", "WR", "TE", "K"),
                selected = "QB")
            })
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
    
    dat_2014 <- filter(dat, Year == 2014)
    sel_dat1 <- dat_2014[, c("Player", "Team", "Week", "Oppo", input$var)]
    colnames(sel_dat1) <- c("Player", "Team", "Week", "Oppo", "var1")
    
    pl_team<-unique(select(sel_dat1, Player, Team))
    pl_bye<-inner_join(pl_team, byes)
    pl_bye$var1 <- NA
    sel_dat1 <- rbind(sel_dat1, pl_bye)
    
    pl_dat1 <- sel_dat1 %>% filter(Player %in% c(input$player1))
    pl_dat2 <- sel_dat1 %>% filter(Player %in% c(input$player2))
    
    pl1_week <- sort(unique(as.numeric(pl_dat1$Week)))
    pl2_week <- sort(unique(as.numeric(pl_dat2$Week)))
    
    if(length(pl1_week) != 17){
      one_17 <- 1:17
      not_in <- one_17[!(one_17 %in% pl1_week)]
      dnp_df <-cbind.data.frame(Player= input$player1, 
                                Team = unique(pl_dat1$Team), Week = not_in, Oppo = "DNP", var1 = NA)
      pl_dat1 <- rbind(pl_dat1, dnp_df)
    }
    if(length(pl2_week) != 17){
      one_17 <- 1:17
      not_in <- one_17[!(one_17 %in% pl2_week)]
      dnp_df <-cbind.data.frame(Player= input$player2, 
                                Team = unique(pl_dat2$Team), Week = not_in, Oppo = "DNP", var1 = NA)
      pl_dat2 <- rbind(pl_dat2, dnp_df)
    }
    
    both_pl <- rbind(pl_dat1, pl_dat2)
    
    both_pl_s <- both_pl %>% arrange(Week, Player)
    
    opp <- as.character(unlist(both_pl_s[,c("Oppo", "Week")][1]))
    opp1 <- opp[seq(1,length(opp),2)]
    opp2 <- opp[seq(2,length(opp),2)]
    lab <- paste(opp1, opp2, sep="\n")
    brk <- unique(as.numeric(both_pl_s$Week))
    clean_dat <- both_pl_s[!(is.na(both_pl_s$var1)),]
    
    gg <- ggplot(clean_dat, aes(Week, var1, color = Player, group = Player)) + 
            geom_line() + geom_point(size = 3) + 
              geom_text(aes(label = var1), size = 3, vjust = -1.5) +
                ylab(input$var) + xlab("Opponent") + 
                ggtitle(paste(input$var, "for", input$player1, "and", input$player2, "for Each Week", sep = " ")) +
                scale_x_discrete(breaks = brk , labels = lab) + 
                theme(axis.text.x = element_text(colour = c(rep("red",17), rep("blue",17))))
      print(gg)
        })
      })