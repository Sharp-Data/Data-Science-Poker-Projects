library(tidyr)
library(dplyr)
library(ggplot2)

poker_df <- read.csv(file = "C:/Users/GamingFoSho/Documents/Data-Science-Poker-Projects/Report.csv")
nl25 <- read.csv(file = "C:/Users/GamingFoSho/Documents/Data-Science-Poker-Projects/nl25vsPlayer1.csv")
nl25_df <- data.frame(nl25)
nl50_df <- read.csv(file = "C:/Users/GamingFoSho/Documents/Data-Science-Poker-Projects/Report50.csv")
head(poker_df)
str(nl25_df)

nl25_df <- mutate(nl25_df, bb_won = Net.Won / 0.25)
nl25_df <- mutate(nl25_df, bb_won_per100h = bb_won / (Hands / 100))
nl25_df <- mutate(nl25_df, pfr_rate = PFR / VP.IP)

nl25_df$bad_player <- ifelse(nl25_df$bb_won_per100h < -1, 1, 0)
nl25_df$agg_player <- ifelse(nl25_df$X3Bet >= 0.035, 1, 0)

for(i in 1:nrow(nl25_df)) {
  if (nl25_df[i, "bad_player"] == 1 & nl25_df[i, "agg_player"] == 0) {
    nl25_df[i, "Fish"] <- 1
  } else {
    nl25_df[i, "Fish"] <- 0
  }
}

str(nl25_df)
nl25_df$Fish
length(nl25_df)
nl25_df$Site <- as.character(nl25_df$Site)
nl25_df$player <- paste(nl25_df$Player.Name, nl25_df$Site)
nl25_df5k <- filter(nl25_df, Hands >= 5000)
write.csv(nl25_df, file = "nl25_vs_Player.csv")




