Name: What is A Good Dota 2 Player? & How to Win?

Member: Ken-FFT

Topic: R, data preprocessing, k-means clustering, association rules, data visualization

Background

As an elder Dota2 player, I was trying to apply data science on this game. 
Dota 2 Matches (Explore player behavior and predict match outcomes). The size of the datasets which contain more than 100 attributes from 50000 matches is 1.31GB
Most of the attributes are numeric. The datasets include the data of the matches’ outcomes, the players’ ratings, the heroes’ information, the logs of purchase, etc.

Aim

  Difference between the players in different ranks:
  
  To find the differences between the Dota 2 players in the different ranks(clusters) is for the players who want a higher rank or keep their high rank. 
  The analysis is based on the dataset of the player ratings which consists of account_id, total_wins, total_matches, trueskill_mu, and trueskill_sigma, and the dataset of the players which consists of match_id, account_id, hero_id, player_slot, gold_spend, gold_per_min, etc.
  
  Association rules of win:
  
  As mentioned at the beginning of the background, Dota 2 is a strategic game. In different matches, the strategies made by the players are not always the same. 
  However, in this project, the strategy with a high winning rate will be exposed. 
  The analysis is based on the dataset of the matches which consists of match_id, start_time, duration, tower_status_radiant, tower_status_dire, barracks_status_dire, barracks_status_radiant, etc.

  Best pair of heroes:
  
  To find the pair of heroes with a high winning rate is to the players pick the heroes matching teammates better. 
  The analysis is based on the dataset of the matches, the data set of the players, and the dataset of the hero names.
  
  Purchase prediction:
  
  If a player could predict which item the enemy will buy next, the player would make a restraint plan. 
  The purchase prediction is based on the time-series data which comes from the dataset of the players and the data set of the purchase logs.
