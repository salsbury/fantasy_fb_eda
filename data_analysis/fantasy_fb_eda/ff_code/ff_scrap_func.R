library(rvest)
library(tidyr)
library(dplyr)

fb_scrap <- function(position, year, week){
# scraps fantasy football data from website www.fftoday.com
# Args:
#       position: Football position you want data for
#                 (Can be "QB", "RB", "WR", "K", "DEF", "DL", "LB", "DB")
#       year:     Season you want the data for (Seasons 2000:2014)
#       week:     The week you want the data for (Weeks 1:17)
#
# Returns:
#       A list with values:
#       text: a character vector that contains the table values you want to tidy/munge
#       links: a character vector of links to each player
  pos_num <- switch(position,
                    "QB" = 10, "RB" = 20, "WR" = 30, "TE" = 40, "K" = 80, "DEF" = 99,
                    "DL" = 50, "LB" = 60, "DB" = 70)
  html_add <- paste('http://www.fftoday.com/stats/playerstats.php?Season=', year, "&GameWeek=", week, "&PosID=",
                    pos_num, "&LeagueID=1", sep="")
  fb_html <- html(html_add)
  text <- fb_html %>% html_nodes(".sort1") %>% html_text()
  #to extract out the player links
  child <- fb_html %>% html_nodes(".sort1") %>% html_children() %>% unlist()
  a_href <- unlist(lapply(1:length(child), function(i) child[i]$a))
  link <- sapply(a_href, function(i) html_attr(i, "href"))
  final_link <- paste("http://www.fftoday.com", link, sep="")
  list("text" = text, "links" = final_link)
}

qb_df <- function(qb_char){
  # function to obtain a data frame for Qbs
  # Args:
  #     character vector that contains table values for qb's
  #     obtained from the first value of fb_scrap function
  #
  # Returns:
  #     a data frame with the correct types for the columns; missing year and week values
  #
  tr<-lapply(1:13, function (i) qb_char[seq(i, length(qb_char), 13)])
  names(tr) <- c("Player", "Team", "G", "Comp", "P_Att", "P_Yard", "P_TD", "INT",
                 "R_Att", "R_Yard", "R_TD", "FFPts", "FFPts_G")
  qbdf<-data.frame(do.call(cbind, tr), stringsAsFactors=F)
  qbdf$Player <- sapply(strsplit(qbdf$Player, "\\. "), function(i) i[2])
  n <- c("G", "Comp", "P_Att", "P_Yard", "P_TD", "INT", "R_Att", "R_Yard", "R_TD",
         "FFPts", "FFPts_G")
  qbdf[n] <- lapply(qbdf[n], as.numeric)
  qbdf$Team <- factor(qbdf$Team)
  qbdf
}

rb_df <- function(rb_char){
  # function to obtain a data frame for Rbs
  # Args:
  #     character vector that contains table values for Rb's
  #     obtained from the first value of fb_scrap function
  #
  # Returns:
  #     a data frame with the correct types for the columns; missing year and week values
  #
  tr<-lapply(1:12, function (i) rb_char[seq(i, length(rb_char), 12)])
  names(tr) <- c("Player", "Team", "G", "Rush_Att", "Rush_Yard", "Rush_TD",
                 "Rec_Tar", "Rec", "Rec_Yard", "Rec_TD", "FFPts", "FFPts_G")
  rbdf<-data.frame(do.call(cbind, tr), stringsAsFactors=F)
  rbdf$Player <- sapply(strsplit(rbdf$Player, "\\. "), function(i) i[2])
  n <- c("G", "Rush_Att", "Rush_Yard", "Rush_TD",
         "Rec_Tar", "Rec", "Rec_Yard", "Rec_TD", "FFPts", "FFPts_G")
  rbdf[n] <- lapply(rbdf[n], as.numeric)
  rbdf$Team <- factor(rbdf$Team)
  rbdf
}

wr_df <- function(wr_char){
  # function to obtain a data frame for wrs
  # Args:
  #     character vector that contains table values for wr's
  #     obtained from the first value of fb_scrap function
  #
  # Returns:
  #     a data frame with the correct types for the columns; missing year and week values
  #
  tr<-lapply(1:12, function (i) wr_char[seq(i, length(wr_char), 12)])
  names(tr) <- c("Player", "Team", "G", "Rec_Tar", "Rec", "Rec_Yard", "Rec_TD", "Rush_Att",
                 "Rush_Yard", "Rush_TD", "FFPts", "FFPts_G")
  wrdf<-data.frame(do.call(cbind, tr), stringsAsFactors=F)
  wrdf$Player <- sapply(strsplit(wrdf$Player, "\\. "), function(i) i[2])
  n <- c("G", "Rec_Tar", "Rec", "Rec_Yard", "Rec_TD", "Rush_Att",
  "Rush_Yard", "Rush_TD", "FFPts", "FFPts_G")
  wrdf[n] <- lapply(wrdf[n], as.numeric)
  wrdf$Team <- factor(wrdf$Team)
  wrdf
}

te_df <- function(te_char){
  # function to obtain a data frame for tes
  # Args:
  #     character vector that contains table values for te's
  #     obtained from the first value of fb_scrap function
  #
  # Returns:
  #     a data frame with the correct types for the columns; missing year and week values
  #
  tr<-lapply(1:9, function (i) te_char[seq(i, length(te_char), 9)])
  names(tr) <- c("Player", "Team", "G", "Rec_Tar", "Rec", "Rec_Yard", "Rec_TD", "FFPts", "FFPts_G")
  tedf<-data.frame(do.call(cbind, tr), stringsAsFactors=F)
  tedf$Player <- sapply(strsplit(tedf$Player, "\\. "), function(i) i[2])
  n <- c("G", "Rec_Tar", "Rec", "Rec_Yard", "Rec_TD", "FFPts", "FFPts_G")
  tedf[n] <- lapply(tedf[n], as.numeric)
  tedf$Team <- factor(tedf$Team)
  tedf
}

k_df <- function(k_char){
  # function to obtain a data frame for ks
  # Args:
  #     character vector that contains table values for k's
  #     obtained from the first value of fb_scrap function
  #
  # Returns:
  #     a data frame with the correct types for the columns; missing year and week values
  #
  tr<-lapply(1:10, function (i) k_char[seq(i, length(k_char), 10)])
  names(tr) <- c("Player", "Team", "G", "FGM", "FGA", "FG_Per", "EPM", "EPA", "FFPts", "FFPts_G")
  kdf<-data.frame(do.call(cbind, tr), stringsAsFactors=F)
  kdf$Player <- sapply(strsplit(kdf$Player, "\\. "), function(i) i[2])
  kdf$FG_Per <- as.numeric(strsplit(kdf$FG_Per, "%")[[1]])
  n <- c("G", "FGM", "FGA", "EPM", "EPA", "FFPts", "FFPts_G")
  kdf[n] <- lapply(kdf[n], as.numeric)
  kdf$Team <- factor(kdf$Team)
  kdf
}

def_df <- function(def_char){
  # function to obtain a data frame for defs
  # Args:
  #     character vector that contains table values for def's
  #     obtained from the first value of fb_scrap function
  #
  # Returns:
  #     a data frame with the correct types for the columns; missing year and week values
  #
  tr<-lapply(1:13, function (i) def_char[seq(i, length(def_char), 13)])
  names(tr) <- c("Team", "G", "Sack", "FR", "INT", "DefTD", "PA", "PaYd_G", "RuYd_G", "Safety", "KickTd", "FFPts", "FFPts_G")
  defdf<-data.frame(do.call(cbind, tr), stringsAsFactors=F)
  defdf$Team <- sapply(strsplit(defdf$Team, "\\. "), function(i) i[2])
  n <- c("G", "Sack", "FR", "INT", "DefTD", "PA", "PaYd_G", "RuYd_G", "Safety", "KickTd", "FFPts", "FFPts_G")
  defdf[n] <- lapply(defdf[n], as.numeric)
  defdf$Team <- factor(defdf$Team)
  defdf
}

df_weeks <- function(position, year, weeks){
# Creates a data frame for the stats of a given position for a given year (can choose the weeks)
# Uses fb_scrap function above as well as corresponding position_df function
# 
# Args:
#     Position: position you want the stats for (ex: "QB", "RB", "WR", "TE", "K", "DEF")
#     Year: The year you want it for
#     Weeks: The weeks you want it for (should be a numeric vector)
#
# Returns:
#         a data frame for the position, year, and weeks wanted
#
  year_df <- do.call(rbind, lapply(weeks, function(i){
    players <- fb_scrap(position, year, i)
    pldf <- switch(position,
                 "QB" = qb_df(players[[1]]),
                 "RB" = rb_df(players[[1]]),
                 "WR" = wr_df(players[[1]]),
                 "TE" = te_df(players[[1]]),
                 "K" = k_df(players[[1]]),
                 "DEF" = def_df(players[[1]]))
                 
    pldf$Week = factor(i)
    pldf$Year = year
    pldf
        }))
    year_df
        }


df_years_weeks <- function(position, years, weeks){
# Wrapper function to expand df_weeks to be able to include multiple years
# uses df_weeks function and just lapplies it so multiple years are accepted
#
# Args:
#     Position: position you want the stats for (ex: "QB", "RB", "WR", "TE", "K", "DEF")
#     Year: The years you want it for
#     Weeks: The weeks you want it for (should be a numeric vector)
#
# Returns:
#         a data frame for the position, years, and weeks wanted
#
  do.call(rbind, lapply(years, function(j){
    df_weeks(position, j, weeks)
  }))
}

#Things to fix
# def_df gives out "St" instead of "St. Louis Rams"



qb_10_14 <- df_years_weeks("QB", 2010:2014, 1:17)
rb_10_14 <- df_years_weeks("RB", 2010:2014, 1:17)
wr_10_14 <- df_years_weeks("WR", 2010:2014, 1:17)
te_10_14 <- df_years_weeks("TE", 2010:2014, 1:17)
k_10_14 <- df_years_weeks("K", 2010:2014, 1:17)
def_10_14 <- df_years_weeks("DEF", 2010:2014, 1:17)





