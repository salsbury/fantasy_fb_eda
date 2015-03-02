library(dplyr)
library(tidyr)

load("ff_df/qb_10_14.RData")
load("ff_df/rb_10_14.RData")
load("ff_df/wr_10_14.RData")
load("ff_df/te_10_14.RData")
load("ff_df/k_10_14.RData")
load("ff_df/def_10_14.RData")
load("ff_df/sched_2014.RData")
load("ff_df/off_pl_opp.RData")


# creating a data frame with all offensive players 
# and their fantasy points for a given week/year

off_pl <- rbind(qb_10_14[,c("Player", "Team", "Position", "Year", "Week", "FFPts")],
      rb_10_14[,c("Player", "Team","Position", "Year", "Week", "FFPts")],
      wr_10_14[,c("Player", "Team","Position", "Year", "Week", "FFPts")],
      te_10_14[,c("Player", "Team","Position", "Year", "Week", "FFPts")],
      k_10_14[,c("Player", "Team","Position", "Year", "Week", "FFPts")])

#
# code to wrangle schedule into opponents of QB's for 2014
#
off_pl_14 <- filter(off_pl, Year== 2014)
long_sch <- sched_2014 %>% gather(Week, Oppo, -Team)

# removing any character that is not an letter from the beginning (targeting the "@" character)
long_sch$Oppo <- gsub("([^A-Z]?)([A-Z]*)", "\\2", long_sch$Oppo)
long_sch$Week <- factor(extract_numeric(long_sch$Week))

# finding out the bye weeks for each QB
byes <-filter(long_sch, Oppo == 'B')
byes$Oppo = "BYE"
qb_team<-unique(select(filter(qb_10_14, Year==2014), Player, Team))
qb_bye<-inner_join(qb_team, byes)

# joining off_pl_14 and long_sch to create a data frame of offensive players and their opponents
off_pl_opp <- inner_join(off_pl_14, long_sch, by = c("Team", "Week"))

# code to specify divisions for each NFL team
divisions <- c(rep("NFC_W", 4), rep("NFC_E", 4), rep("NFC_N", 4), rep("NFC_S", 4),
               rep("AFC_W", 4), rep("AFC_E", 4), rep("AFC_N", 4), rep("AFC_S", 4))
names(divisions) <- c("ARI", "STL", "SF", "SEA", "PHI", "DAL", "WAS", "NYG",
                      "GB", "DET", "CHI", "MIN", "CAR", "NO", "ATL", "TB",
                      "OAK", "DEN", "SD", "KC", "NE", "MIA", "BUF", "NYJ",
                      "PIT", "BAL", "CLE", "CIN", "HOU", "TEN", "IND", "JAC")
off_pl_opp$Oppo_div = divisions[off_pl_opp$Oppo]
