# IST707 
# Final Project
# Qingkun Zhu
# 2020/10/02

# Tasks:
#1.	Association rules of win
#   The main object is the match_df dataset. Most variables of the match_df dataset, such as tower_status_dire, are factors to show the status of the structure in Dota2. In this task, the win should be divided into two groups, Rad win and Dire win, first. Then the two kinds of win are the right-side item of the target association rules.
#2.	Difference between the players in the different ranks
#   Find the differences between the players in the different ranks(clusters) by player_ratings_df, and players_df. This task involves clustering and classification. First get the different player clusters from player_ratings_df. Then given the cluster, to classify the players_df by the decision trees.
#3.	Ability prediction
#   The main object is the ability_upgrades_df dataset, hero_names_df and players_df. To predict which ability will be upgraded next for a certain hero. 
#4.	Purchase prediction
#   The main object is the purchase_log_df dataset and players_df dataset. To predict which item will be purchased next, and to find the association rules about the purchase.


# library
library(dplyr)
library(lubridate)
library(tidyr)
library(data.table)
library(Matrix)
library(arules)
library(cluster)
library(ggplot2)
library(factoextra)
library(ggpubr)

# function
# the function number2binary, for convert a decimal number to a binary number
# which includes an argument noBits to control how many bits are returned. 
# Standard is to return 32 bits.
number2binary = function(number, noBits) {  
  binary_vector = rev(as.numeric(intToBits(number)))
  if(missing(noBits)) {
    return(binary_vector)
  } else {
    binary_vector[-(1:(length(binary_vector) - noBits))]
  }
}
#example
#x <- 3
#y <- number2binary(x, 4)
#y



# A.Exploration

# A.1 input the raw data sets
# create a vector for the file names
file_name <- c("ability_ids.csv", "ability_upgrades.csv", "chat.csv", "cluster_regions.csv", "hero_names.csv",
               "item_ids.csv", "match.csv", "match_outcomes.csv", "objectives.csv", "patch_dates.csv",
               "player_ratings.csv", "player_time.csv", "players.csv", "purchase_log.csv", "teamfights.csv",
               "teamfights_players.csv", "test_labels.csv", "test_player.csv")
# set the path of the input files
file_path <- "D:/Study/IST 707 Data Analytics/IST707 Project/raw_data/"
setwd(file_path) # set the working space
# ability_ids_df, 
# 688 observations with 2 variables: 
# ability_id, and ability_name which including the common abilities, like default_attack, and the exclusive abilities, like the sven_storm_bolt.
ability_ids_df <- read.csv(file = file_name[1])
ability_ids_df$ability_id <- as.character(ability_ids_df$ability_id)# convert the ability_id to character
summary(ability_ids_df)
# ability_upgrades_df,
# 8939599 observations with 5 variables:
# ability, it is the ability_id
# level, it is the level when the ability was upgraded
# time, it is the time point when the ability was upgraded
# player_slot, its values 0,1,2,3,4 are for The Radiant, and its values 128,129,130, 131, 132 are for The Dire
# match_id
ability_upgrades_df <- read.csv(file = file_name[2])
table(ability_upgrades_df$player_slot)
#  0      1      2      3      4      128    129    130    131    132 
# 901492 893169 894381 893323 890618 899684 892341 892241 893182 889168 
ability_upgrades_df <- ability_upgrades_df %>% # convert the data type
  mutate(ability = as.character(ability),
         level = as.integer(level),
         player_slot = as.factor(player_slot),
         match_id = as.character(match_id)) 
summary(ability_upgrades_df)
# chat_df,
# 610146 observations with 5 variables:
# match_id
# key, it is the content of the chat
# slot, its values 0,1,2,3,4 are for The Radiant, and its values 128,129,130,131,132 are for The Dire
# time, it is the time point when the chat happened
# unit, I do not know what it is
chat_df <- read.csv(file = file_name[3])
summary(chat_df)
# cluster_regions_df,
# 53 observations with 2 variables:
# cluster, cluster can link matches to geographic region
# region, such as US WEST
cluster_regions_df <- read.csv(file = file_name[4])
cluster_regions_df <- cluster_regions_df %>%
  mutate(cluster = as.factor(cluster))
summary(cluster_regions_df)
# hero_names_df
# 112 observations with 3 variables:
# name, hero_id, and localized_name
hero_names_df <- read.csv(file = file_name[5])
hero_names_df <- hero_names_df %>%
  mutate(hero_id = as.character(hero_id))
summary(hero_names_df)
# item_ids_df,
# 189 observations with 2 variables:
# item_id, and item_name
item_ids_df <- read.csv(file = file_name[6])
item_ids_df$item_id <- as.character(item_ids_df$item_id)
summary(item_ids_df)
# match_df,
# 50000 observations with 13 variables:
# match_id, 
# start_time, 2015-11-05 14:01:52 ~ 2015-11-18 01:46:55
# duration, tower_status_radiant, tower_status_dire, 
# barracks_status_dire, barracks_status_radiant, first_blood_time, game_mode, radiant_win,
# negative_votes, positive_votes, cluster
# detailed information, https://wiki.teamfortress.com/wiki/WebAPI/GetMatchDetails#Tower_Status%22tower_status_dire%22:%202047
match_df <- read.csv(file = file_name[7])
match_df <- match_df %>%
  mutate(match_id = as.character(match_id),
         cluster = as.character(cluster),
         start_time = as.POSIXct(start_time, origin="1970-01-01")
         )
summary(match_df)
# match_outcomes_df,
# 1828588 observations with 10 variables:
# match_id,
# account_id_0~4, the account id of the five players who were in the same team (rad/dire)
# start_time, 2015-07-15 22:43:05 ~ 2015-11-12 01:59:04
# parser_version,
# win, 0/1
# rad, 0/1, to tell which team it was
match_outcomes_df <- read.csv(file = file_name[8])
match_outcomes_df <- match_outcomes_df %>%
  mutate(match_id = as.character(match_id),
         account_id_0 = as.character(account_id_0),
         account_id_1 = as.character(account_id_1),
         account_id_2 = as.character(account_id_2),
         account_id_3 = as.character(account_id_3),
         account_id_4 = as.character(account_id_4),
         start_time = as.POSIXct(start_time, origin="1970-01-01"))
summary(match_outcomes_df)
table(match_outcomes_df$parser_version)# 12:621876, 13:1069984, 14:136728
#plot(match_outcomes_df$start_time,match_outcomes_df$parser_version)# overlap
# count(match_outcomes_df[match_outcomes_df$match_id %in% match_df$match_id ,])# no match matches
# objectives_df,
# 1173396 observations with 9 variables:
# I do not know the detailed information about those variables
objectives_df <- read.csv(file = file_name[9])
summary(objectives_df)
# patch_dates_df,
# 19 observations with 2 variables:
# patch_date, and name which is the name of the version
# release dates for various patches, use start_time from match.csv to determine which patch a match was played in
patch_dates_df <- read.csv(file = file_name[10])
patch_dates_df <- patch_dates_df %>%
  mutate(patch_date = as.Date(patch_date),
         name = as.factor(name))
summary(patch_dates_df)
# player_ratings_df
# 834226 observations with 5 variables:
# account_id, total_wins, total_matches, 
# trueskill_mu, trueskill_sigma, 
# trueskill ratings have two components, mu, which can be interpreted as the skill, with higher value being better, and sigma which is the uncertainty of the rating.
player_ratings_df <- read.csv(file = file_name[11])
player_ratings_df <- player_ratings_df %>%
  mutate(account_id = as.character(account_id))
summary(player_ratings_df)
# player_time_df
# 2209778 observations with 32 variables:
# match_id,
# times, 1 min to collect each data point, including gold, last hit, experience
# gold_t_#, lh_t_#, xp_h_#, #: rad 0,1,2,3,4 ; dire 128,129,130,131,132
player_time_df <- read.csv(file = file_name[12])
player_time_df$match_id <- as.character(player_time_df$match_id)
summary(player_time_df)
# players_df,
# 500000 observations with 73 variables: each observation is for each player in each match
# match_id, and account_id
# hero_id, and player_slot
# gold, gold_spent, gold_per_min, xp_per_min      
# kills, deaths, assists
# denies·´²¹, last_hitsÕý²¹, 
# stuns, hero_damage, hero_healing, tower_damage
# item_#, #:0~5 every player has 6 boxes for 6 items, but not always full
# level, in these matches, the maximum level is 25
# leaver_status, 0 - NONE - finished match, no abandon. 1 - DISCONNECTED - player DC, no abandon. 2 - DISCONNECTED_TOO_LONG - player DC > 5min, abandoned. 3 - ABANDONED - player DC, clicked leave, abandoned. 4 - AFK - player AFK, abandoned. 5 - NEVER_CONNECTED - player never connected, no abandon. 6 - NEVER_CONNECTED_TOO_LONG - player took too long to connect, no abandon.
# xp_hero, xp_creep, xp_roshan, xp_other, gold_other, gold_death, gold_buyback, gold_abandon, gold_sell, 
# gold_destroying_structure, gold_killing_heros, gold_killing_creeps, gold_killing_roshan, gold_killing_couriers,
# unit_order_none, unit_order_move_to_position, unit_order_move_to_target, unit_order_attack_move,
# unit_order_attack_target, unit_order_cast_position, unit_order_cast_target, unit_order_cast_target_tree,
# unit_order_cast_no_target, unit_order_cast_toggle, unit_order_hold_position......
# !! in this project, I do not analyze those unit orders
players_df <- read.csv(file = file_name[13])
players_df <- players_df %>%
  mutate(match_id = as.character(match_id),
         account_id = as.character(account_id),
         hero_id = as.character(hero_id),
         player_slot = as.factor(player_slot))
summary(players_df)
count(players_df[players_df$account_id %in% player_ratings_df$account_id ,])# to check the referrence between playes_df and player_ratings_df
# purchase_log_df
# 18193745 observations with 4 variables:
# item_id,
# time, the negative values might indicate purchasing the items before the fighting
# player_slot,
# match_id
purchase_log_df <- read.csv(file = file_name[14])
purchase_log_df <- purchase_log_df %>%
  mutate(item_id = as.character(item_id),
         player_slot = as.factor(player_slot),
         match_id = as.character(match_id))
summary(purchase_log_df)
# teamfights_df
# 539047 observations with 5 variables:
# match_id,
# start, the time point of the team fight starting
# end, the time point of the team fight end
# last_death, the time point of the last death
# deaths, the number of the deaths in the team fight
# this data set could be linked with the players_df to get more details
# end = last_death + 15(s)
teamfights_df <- read.csv(file = file_name[15])
teamfights_df$match_id <- as.character(teamfights_df$match_id)
#table(teamfights_df$end-teamfights_df$last_death) # end = last_death + 15(s)
summary(teamfights_df)
# teamfights_players_df
# 5390470 observations with 8 variables:
# match_id, player_slot, buybacks, damage, deaths, gold_delta, xp_end, xp_start
# this data set could be linked with the players_df and the teamfights_df to get more details
teamfights_players_df <- read.csv(file = file_name[16])
teamfights_players_df <- teamfights_players_df %>%
  mutate(match_id = as.character(match_id),
         player_slot = as.factor(player_slot))
summary(teamfights_players_df)
# test_labels_df
# 100000 observations with 2 variables:
# match_id and radiant_win
test_labels_df <- read.csv(file = file_name[17])
test_labels_df$match_id <- as.character(test_labels_df$match_id)
summary(test_labels_df)
# test_player_df
# 100000 observations with 4 variables:
# match_id, account_id, hero_id, player_slot
test_player_df <- read.csv(file =  file_name[18])
test_player_df <- test_player_df %>%
  mutate(match_id = as.character(match_id),
         account_id = as.character(account_id),
         hero_id = as.character(hero_id),
         player_slot = as.factor(player_slot))
summary(test_player_df)
count(test_player_df[which(test_player_df$account_id == 0),])/count(test_player_df) # 0.36, about 1/3 players are anonymous

# Task1:	Association rules of win
# Data set: match_df,player_time_df,teamfights_df
str(match_df)
summary(match_df)
summary(player_time_df)
summary(teamfights_df)
# because at the following steps I will generate many new variables which is related to time,
# I want to make sure is there any difference of the outcomes between the matches with different durations.
# Task1.1: just use match_df to find association rules of win
# find and remove the duplicate data
sum(duplicated(match_df$match_id))# no duplicate data
# missing data
sum(is.na.data.frame(match_df))# no missing data
# wrong data
boxplot(match_df$duration)# in seconds since the match began
quantile(match_df$duration, probs = c(0.01,0.99))#drop less than 1200, or more than 4200
match_df_clean <- match_df[(match_df$duration>=1200 & match_df$duration<=4200),]
table(match_df$tower_status_radiant)
boxplot(match_df_clean$first_blood_time)# in seconds since the match began
match_df_clean <- match_df_clean[match_df_clean$first_blood_time>5,]
# convert the status variables and game_mode into factors
match_df_clean <- match_df_clean %>%
  mutate(tower_status_radiant = as.factor(tower_status_radiant),
         tower_status_dire = as.factor(tower_status_dire),
         barracks_status_dire = as.factor(barracks_status_dire),
         barracks_status_radiant = as.factor(barracks_status_radiant),
         game_mode = as.factor(game_mode))
# remove match_id, start_time, negative_votes, positive_votes, cluster
match_df_clean_Noid <- match_df_clean[-c(1,2,11:13)]
# just select game_mode == 22: All Random Deathmatch
match_df_clean_Noid <- match_df_clean_Noid[match_df_clean_Noid$game_mode == 22,]
match_df_clean_Noid <- match_df_clean_Noid[-c(7)]
# convert the radiant_win into numeric type for correlation matrix
match_df_clean_Noid$match_df_clean_Noid <- apply(match_df_clean_Noid, 1, FUN = function(x)
  if(x['radiant_win'] == 'False') 0
  else 1)
summary(match_df_clean_Noid)
# correlation matrix
cor(match_df_clean_Noid[c(1,6,7)])
# discretize
match_df_clean_Noid$duration <- apply(match_df_clean_Noid, 1, FUN = function(x) # discretize duration into 4 ranges
  if(x['duration'] < 2035) 'Short time' 
  else if((x['duration']>=2035) & (x['duration']<2415)) 'Common time S'
  else if((x['duration']>=2415) & (x['duration']<2863)) 'Common time L'
  else 'Long time')
table(match_df_clean_Noid$duration)
# summary(match_df_clean_Noid$first_blood_time)
# match_df_clean_Noid$first_blood_time <- apply(match_df_clean_Noid, 1, FUN = function(x) # discretize first_blood_time into 4 ranges
#   if(x['first_blood_time'] < 47) 'Early_FB' 
#   else if((x['first_blood_time']>=47) & (x['first_blood_time']<100)) 'Common_FB_E'
#   else if((x['first_blood_time']>=100) & (x['first_blood_time']<161)) 'Common_FB_L'
#   else 'Late_FB')
# table(match_df_clean_Noid$first_blood_time)
# because of the error of first_blood_time, delete the column
match_df_clean_Noid <- match_df_clean_Noid[-c(6)]
# get transaction dataset
match_df_clean_Noid <- match_df_clean_Noid %>%
  mutate(duration = as.factor(duration),
         #first_blood_time = as.factor(first_blood_time),
         radiant_win = as.factor(radiant_win))
# convert to transactions
match_df_trans <- as(match_df_clean_Noid, "transactions") 
#inspect(match_df_trans)
# explore the association rules
RadWin_rules_1 <- apriori(match_df_trans, parameter = list(supp=0.25, conf = 0.9, minlen = 2), appearance = list(rhs = "radiant_win=True"))
inspect(RadWin_rules_1)
#     lhs                             rhs               support confidence  coverage     lift count
# [1] {barracks_status_dire=0}     => {radiant_win=1} 0.3022720  0.9946956 0.3038839 1.908714 11814
# [2] {tower_status_dire=0}        => {radiant_win=1} 0.3305189  0.9935395 0.3326681 1.906495 12918
# [3] {barracks_status_radiant=63} => {radiant_win=1} 0.4571436  0.9987702 0.4577065 1.916533 17867
# [4] {tower_status_dire=0,                                                                        
# barracks_status_dire=0}     => {radiant_win=1} 0.3022720  0.9976355 0.3029884 1.914355 11814
# [5] {barracks_status_dire=0,                                                                     
# barracks_status_radiant=63} => {radiant_win=1} 0.2716713  0.9995293 0.2717992 1.917989 10618
# [6] {tower_status_dire=0,                                                                        
# barracks_status_radiant=63} => {radiant_win=1} 0.2970269  0.9996556 0.2971293 1.918231 11609
# [7] {tower_status_dire=0,                                                                        
# barracks_status_dire=0,                                                                     
# barracks_status_radiant=63} => {radiant_win=1} 0.2716713  0.9997175 0.2717480 1.918350 10618
number2binary(63,8)#barracks_status_radiant=63: all radiant tower status are survived
RadWin_rules_2 <- apriori(match_df_trans, parameter = list(supp=0.08, conf = 0.8, minlen = 2), appearance = list(rhs = "radiant_win=True"))
inspect(RadWin_rules_2)
number2binary(1974,16)#{tower_status_radiant=1974}  => {radiant_win=1} 0.08049330  1.0000000 0.08049330 1.918892  3146
# tower_status_radiant=1974£º all radiant first tower are not survived but all other radiant towers are survived.
RadWin_rules_3 <- apriori(match_df_trans, parameter = list(supp=0.05, conf = 0.5, minlen = 2), appearance = list(rhs = "radiant_win=True"))
RadWin_rules_3_mid<- subset(RadWin_rules_3, lhs %ain% c("barracks_status_dire=3"))
inspect(RadWin_rules_3_mid)
number2binary(3,8)



# Task1.2: Combined with player_time_df and teamfights_df, use the match_df_clean to predict the outcome of the match
# use player_time_df to get the differences of 
#   the sum of gold, the sum of last hits, and the sum of experience between rad and dire per min.
summary(player_time_df[player_time_df$times == 0,])# to find is there any wrong data when time is 0
player_time_df_clean <- player_time_df[!((player_time_df$times == 0)&
                                                 ((player_time_df$gold_t_0 != 0)|
                                                    (player_time_df$gold_t_1 != 0)|
                                                    (player_time_df$gold_t_2 != 0)|
                                                    (player_time_df$gold_t_3 != 0)|
                                                    (player_time_df$gold_t_4 != 0)|
                                                    (player_time_df$gold_t_128 != 0)|
                                                    (player_time_df$gold_t_129 != 0)|
                                                    (player_time_df$gold_t_130 != 0)|
                                                    (player_time_df$gold_t_131 != 0)|
                                                    (player_time_df$gold_t_132 != 0)|
                                                    (player_time_df$lh_t_0 != 0)|
                                                    (player_time_df$lh_t_1 != 0)|
                                                    (player_time_df$lh_t_2 != 0)|
                                                    (player_time_df$lh_t_3 != 0)|
                                                    (player_time_df$lh_t_4 != 0)|
                                                    (player_time_df$lh_t_128 != 0)|
                                                    (player_time_df$lh_t_129 != 0)|
                                                    (player_time_df$lh_t_130 != 0)|
                                                    (player_time_df$lh_t_131 != 0)|
                                                    (player_time_df$lh_t_132 != 0)|
                                                    (player_time_df$xp_t_0 != 0)|
                                                    (player_time_df$xp_t_1 != 0)|
                                                    (player_time_df$xp_t_2 != 0)|
                                                    (player_time_df$xp_t_3 != 0)|
                                                    (player_time_df$xp_t_4 != 0)|
                                                    (player_time_df$xp_t_128 != 0)|
                                                    (player_time_df$xp_t_129 != 0)|
                                                    (player_time_df$xp_t_130 != 0)|
                                                    (player_time_df$xp_t_131 != 0)|
                                                    (player_time_df$xp_t_132 != 0))
                                               ),]
summary(player_time_df_clean[player_time_df_clean$times == 0,])
player_time_dif <- player_time_df_clean %>%
  mutate(match_id = match_id,
         times = times,
         gold_dif = (gold_t_0+gold_t_1+gold_t_2+gold_t_3+gold_t_4-gold_t_128-gold_t_129-gold_t_130-gold_t_131-gold_t_132)/
           (gold_t_0+gold_t_1+gold_t_2+gold_t_3+gold_t_4+gold_t_128+gold_t_129+gold_t_130+gold_t_131+gold_t_132),
         lh_dif = (lh_t_0+lh_t_1+lh_t_2+lh_t_3+lh_t_4-lh_t_128-lh_t_129-lh_t_130-lh_t_131-lh_t_132)/
           (lh_t_0+lh_t_1+lh_t_2+lh_t_3+lh_t_4+lh_t_128+lh_t_129+lh_t_130+lh_t_131+lh_t_132),
         xp_dif = (xp_t_0+xp_t_1+xp_t_2+xp_t_3+xp_t_4-xp_t_128-xp_t_129-xp_t_130-xp_t_131-xp_t_132)/
           (xp_t_0+xp_t_1+xp_t_2+xp_t_3+xp_t_4+xp_t_128+xp_t_129+xp_t_130+xp_t_131+xp_t_132))%>%
  select(match_id, times, gold_dif, lh_dif, xp_dif)
#
plot(gold_dif~times,data = player_time_dif)
plot(lh_dif~times,data = player_time_dif)
plot(xp_dif~times,data = player_time_dif)
# divide match_df_clean into match_df_clean_RadWin and match_df_clean_DireWin
match_df_clean_RadWin <- match_df_clean[match_df_clean$radiant_win == 'True',]
match_df_clean_DireWin <- match_df_clean[match_df_clean$radiant_win == 'False',]
#
player_time_dif_RadWin <- player_time_dif[player_time_dif$match_id %in% match_df_clean_RadWin$match_id,]
plot(gold_dif~times,data = player_time_dif_RadWin)
abline(h=0, col="orange")
plot(lh_dif~times,data = player_time_dif_RadWin)
abline(h=0, col="orange")
plot(xp_dif~times,data = player_time_dif_RadWin)
abline(h=0, col="orange")
# use the values of times of player_time_dif to create new columns
# long-to-wide reshaping
match_dif <- dcast(setDT(player_time_dif), match_id~times, value.var=c('gold_dif', 'lh_dif', 'xp_dif'))
summary(match_dif)
match_dif <- match_dif
# then I also want to get the number of team fights in each match
match_teamfight <- teamfights_df %>%
  group_by(match_id)
test <- match_df %>%  inner_join(player_time_df, by = "match_id")


# Task2:	Difference between the players in the different ranks
# Data set: players_df, player_ratings_df, 
summary(players_df)
summary(player_ratings_df)
count(players_df[players_df$account_id %in% player_ratings_df$account_id ,])# to check the referrence between playes_df and player_ratings_df
players_df_selected <- players_df[-c(1,3,18:23,40:73)]
players_df_selected <- players_df_selected[players_df_selected$account_id %in% player_ratings_df$account_id ,]
summary(players_df_selected)
players_df_selected %>%
  group_by(account_id,hero_id) %>%
  summarise_each(funs(mean=mean(., na.rm = TRUE)))

# k-mean player_ratings
player_ratings_df_selected <- player_ratings_df[player_ratings_df$account_id %in% players_df$account_id, ]
# boxplot(player_ratings_df$total_wins)
# boxplot(player_ratings_df$total_matches)
# boxplot(player_ratings_df$trueskill_mu)
# boxplot(player_ratings_df$trueskill_sigma)
player_ratings_clean <- player_ratings_df[(player_ratings_df$total_wins<20)&(player_ratings_df$total_matches<40),]
# find and remove the duplicate data
sum(duplicated(player_ratings_clean$account_id))# no duplicate data
str(player_ratings_clean)
set.seed(1121)
player_ratings <- player_ratings_clean[sample(nrow(player_ratings_clean),10000,replace=FALSE),]
# Normalization
normalize <- function(x){
  (x-min(x))/(max(x)-min(x))
}
summary(player_ratings_df_selected)
summary(player_ratings)
player_ratings_normalized <- player_ratings %>%
  mutate(total_wins = normalize(total_wins),
         total_matches = normalize(total_matches),
         trueskill_mu = normalize(trueskill_mu),
         trueskill_sigma = normalize(trueskill_sigma))
summary(player_ratings_normalized)
# Construct an elbow diagram
elbow_plot <- player_ratings_normalized %>%
  select(2,3,4,5) %>% # without account_id
  fviz_nbclust(kmeans, method = "wss")
elbow_plot # choose the number of clusters 4
# Cluster
clusters <- player_ratings_normalized %>%
  select(2,3,4,5) %>%
  kmeans(centers = 4,
         nstart = 30)
table(clusters$cluster)
clusters_test1 <- player_ratings_normalized %>%# test 1
  select(2,3,4,5) %>%
  kmeans(centers = 4,
         nstart = 10)
table(clusters_test1$cluster)
clusters_test2 <- player_ratings_normalized %>%# test2
  select(2,3,4,5) %>%
  kmeans(centers = 4,
         nstart = 80)
table(clusters_test2$cluster)
# Visualize the clusters
fviz_cluster(clusters, player_ratings_normalized[2:5], ellipse.type = "convex")
clusters$centers[1,2]
player_ratings_clean_normalized <- player_ratings_clean %>%
  mutate(total_wins = normalize(total_wins),
         total_matches = normalize(total_matches),
         trueskill_mu = normalize(trueskill_mu),
         trueskill_sigma = normalize(trueskill_sigma))
# euclidean distance
euli_dist <- function(x,y){
  (x-min(x))/(max(x)-min(x))
}
player_ratings_clean_normalized <- player_ratings_clean_normalized %>%
  mutate(cluster = case_when((total_wins+total_matches+trueskill_mu+trueskill_sigma - clusters$centers)))
c1 <- clusters$centers[1,1:4]
c2 <- clusters$centers[2,1:4]
dist(c1,c2)

# Task3: Best pair of heroes:
# The main objects are the match_df dataset, players_df, and hero_names_df. To find the pair of heros with the highest rate of win.
# select match_id and radiant_win from match_df_clean
match_df_win <- select(match_df_clean,match_id,radiant_win)
# get match_players from players_df, the match_players consists of match_id, hero_id(list), player_slot (True is rad, False is dire).
players_df_selected_2 <- players_df[,c(1,3,4)]
match_players <- players_df_selected_2 %>% 
  group_by(match_id) %>%
  summarise(hero_id = list(unique(hero_id)),
            player_slot = (as.numeric(player_slot) < 5)) %>%
  distinct() %>%
  #str(match_players)
  #str(match_players$hero_id[1]) 
  mutate(hero_id = case_when(player_slot~list(unlist(hero_id, recursive = FALSE)[1:5]),
                             !player_slot~list(unlist(hero_id, recursive = FALSE)[6:10])))
# get match_winners from match_players give the radiant_win of match_df_win
#!!! the match_id are not same
match_winners <- merge(x=match_players, y=match_df_win,by="match_id",all.x=TRUE)

sample(hero_names_df$hero_id,size=2,replace=TRUE)
pair_hero <- replicate(477, toString(sample(hero_names_df$hero_id,size=2,replace=TRUE)))
win_rate <- rnorm(477, mean = 0.59, sd=0.08)
hist(win_rate)
win_test_df <- data.frame(
  pair_heroes = pair_hero,
  win_rate = win_rate
)
str(win_test_df)
plot(win_test_df$win_rate)
abline(h=0.41,col = "green")
abline(h=0.77,col = "green")
