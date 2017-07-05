getwd()

library(tidyr)
library(dplyr)
library(ggplot2)

poker_df <- read.csv(file = "C:/Users/GamingFoSho/Documents/Data-Science-Poker-Projects/Report.csv")
nl25 <- read.csv(file = "C:/Users/GamingFoSho/Documents/Data-Science-Poker-Projects/Report25.csv")
nl25_df <- data.frame(nl25)
nl50_df <- read.csv(file = "C:/Users/GamingFoSho/Documents/Data-Science-Poker-Projects/Report50.csv")
head(poker_df)
str(nl25_df)

nl25_df <- mutate(nl25_df, bb_won = Net.Won / 0.25)
nl25_df <- mutate(nl25_df, bb_won_per100h = bb_won / (Hands / 100))
nl25_df <- mutate(nl25_df, vpip_hands = Hands * VP.IP)
nl25_df <- mutate(nl25_df, pfr_hands = Hands * PFR)
nl25_df <- mutate(nl25_df, bet3_hands = Hands * X3Bet)
nl25_df <- mutate(nl25_df, wtsd_hands = Hands * WTSD.)
nl25_df <- mutate(nl25_df, pfr_rate = PFR / VP.IP)

nl25_df$bad_player <- ifelse(nl25_df$bb_won_per100h < -1, 1, 0)
nl25_df$agg_player <- ifelse(nl25_df$X3Bet >= 0.06, 1, 0)

for(i in 1:length(nl25_df)) {
  if (nl25_df[i, "bad_player"] == 1 & nl25_df[i, "agg_player"] == 0) {
    nl25_df[i, "Fish"] <- 1
  } else {
    nl25_df[i, "Fish"] <- 0
  }
}

nl25_df$Site <- as.character(nl25_df$Site)
nl25_df$player <- paste(nl25_df$Player.Name, nl25_df$Site)
nl25_df5k <- filter(nl25_df, Hands >= 5000)
str(nl25_df5k)
str(nl25_df)

write.csv(nl25_df, file = "nl25.csv")
write.csv(nl25_df5k, file = "nl25_5k.csv")
Fish <- filter(nl25_df, Fish == 1)
Fish <- select(Fish, Hands, Net.Won, bb_won, vpip_hands, pfr_hands, bet3_hands, wtsd_hands)  
Fish_mean <- summarise_each(Fish, funs(mean))
Fish_mean <- mutate(Fish_mean, vpip = vpip_hands / Hands)
Fish_mean <- mutate(Fish_mean, pfr = pfr_hands / Hands)  
Fish_mean <- mutate(Fish_mean, bet3 = bet3_hands / Hands)
Fish_mean <- mutate(Fish_mean, wtsd = wtsd_hands / Hands)
Fish_mean

nl25_df$good_player <- ifelse(nl25_df$bb_won_per100h > 2, 1, 0)
good_players <- filter(nl25_df, good_player == 1)
good_players <- select(good_players, Hands, Net.Won, bb_won, vpip_hands, pfr_hands, bet3_hands, wtsd_hands)  
good_mean <- summarise_each(good_players, funs(mean))
good_mean <- mutate(good_mean, vpip = vpip_hands / Hands)
good_mean <- mutate(good_mean, pfr = pfr_hands / Hands)  
good_mean <- mutate(good_mean, bet3 = bet3_hands / Hands)
good_mean <- mutate(good_mean, wtsd = wtsd_hands / Hands)
good_mean

nl25_df$bad_player
ggplot(nl25_df, aes(bb_won_per100h)) +
  geom_histogram(binwidth = .5)
