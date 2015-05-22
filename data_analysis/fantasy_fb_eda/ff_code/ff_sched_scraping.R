library(rvest)
library(tidyr)
library(dplyr)

# code written to scrap the 2014 NFL schedule from fftoday.com

sos<-html("http://www.fftoday.com/stats/fantasystats.php?o=3&PosID=10&LeagueID=1")
teams <- sos %>% html_nodes(".tablehdrsmall strong") %>% html_text()
sched <-sos %>% html_nodes(".smallestbody") %>% html_text()
clean_sch <- gsub("(@?[A-Z]*)([^A-Z]*)", "\\1", sched)
sched_2014<- cbind.data.frame(teams, matrix(clean_sch, nrow=32, byrow = T), stringsAsFactors = F)
colnames(sched_2014) <- c("Team", paste("W", 1:17, sep = ""))
save(sched_2014, file = "sched_2014.RData")
