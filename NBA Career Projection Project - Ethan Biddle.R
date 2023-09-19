library(tidyverse)
library(dplyr)
awards <- read_csv("awards_data.csv")
player_data <- read_csv("player_stats.csv")

player_data$player <- iconv(player_data$player, from = 'UTF-8', to = 'ASCII//TRANSLIT')
player_data$player <- str_replace_all(player_data$player, "'", "")
player_data$player <- str_replace_all(player_data$player, "B.J.", "BJ")
player_data$player <- str_replace_all(player_data$player, "R.J.", "RJ")

#Making awards and player data sets smaller so only take values I need
new_awards <- subset(awards, select = c(nbapersonid, season, `All NBA First Team`, `All NBA Second Team`, 
                                         `All NBA Third Team`, `Defensive Player Of The Year_rk`, `Most Valuable Player_rk`
                                         , all_star_game))
#take out duplicates via mid-season trades
new_awards <- unique(new_awards)

player_info <- subset(player_data, select = c(player, nbapersonid, draftyear, season, games, 
                                          games_start, mins))

player_stats <- subset(player_data, select = c(nbapersonid, season, fgm, fga, tot_reb, 
                                               ast, steals, blocks, tov, points, WS, VORP))


#take out duplicates via mid-season trades
player_info <- player_info %>% group_by(player, nbapersonid, season, draftyear) %>%
  summarise(across(c(games_start, games, mins), sum))

player_stats <- player_stats %>% group_by(nbapersonid, season) %>%
  summarise(across(c(fgm, fga, tot_reb, ast, steals, blocks, tov, points, WS, VORP), sum))

#merging two datasets
data <- left_join(player_info, new_awards)
data <- data %>% left_join(player_stats, by = c('nbapersonid', 'season'))


#creating years played variable
data$years_played <- data$season-data$draftyear
data$outcome <- "OUTCOME"

train_data <- filter(data, years_played>3 & draftyear<2017)
test_data <- filter(data, draftyear>2017)

#elites
elite_players <- train_data %>% filter(`All NBA First Team`==1 | `All NBA Second Team`==1| 
                              `All NBA Third Team`==1 | `Defensive Player Of The Year_rk` == 1 |
                            `Most Valuable Player_rk` == 1) %>% 
  group_by(nbapersonid) %>% 
  count() %>%
  filter(n>1)

data <- data |> dplyr::mutate(outcome = ifelse(nbapersonid %in% elite_players$nbapersonid, "ELITE", outcome))

#allstars
all_star_players <- train_data %>% filter(all_star_game==T) %>% 
  group_by(nbapersonid) %>% 
  count() %>%
  filter(n>1)

#cant be in allstar group if in elite group
`%notin%` <- Negate(`%in%`)
all_star_players <- filter(all_star_players, nbapersonid %notin% elite_players$nbapersonid)

data <- data |> dplyr::mutate(outcome = ifelse(nbapersonid %in% all_star_players$nbapersonid, "ALL-STAR", outcome))

#adjusting for shortened seasons
train_data <- train_data |> dplyr::mutate(games_start = ifelse(season==2011, games_start * (82/66), games_start))
train_data <- train_data |> dplyr::mutate(games_start = ifelse(season == 2019 | season == 2020, games_start * (82/72), games_start))
train_data <- train_data |> dplyr::mutate(mins = ifelse(season==2011, mins * (82/66), mins))
train_data <- train_data |> dplyr::mutate(mins = ifelse(season== 2019 | season == 2020, mins * (82/72), mins))

#starters
starter_players <- train_data %>% filter(games_start>=41 | mins >= 2000) %>%
  group_by(nbapersonid) %>% 
  count() %>%
  filter(n>1)

#cant be in starters if in all star or elite group
starter_players <- filter(starter_players, nbapersonid %notin% all_star_players$nbapersonid)
starter_players <- filter(starter_players, nbapersonid %notin% elite_players$nbapersonid)

data <- data |> dplyr::mutate(outcome = ifelse(nbapersonid %in% starter_players$nbapersonid, "STARTER", outcome))

#rotationals
rotation_players <- train_data %>% filter(mins>=1000) %>%
  group_by(nbapersonid) %>% 
  count() %>%
  filter(n>1)

#cant be in rotation group if in any previous groups
rotation_players <- filter(rotation_players, nbapersonid %notin% all_star_players$nbapersonid)
rotation_players <- filter(rotation_players, nbapersonid %notin% elite_players$nbapersonid)
rotation_players <- filter(rotation_players, nbapersonid %notin% starter_players$nbapersonid)

data <- data |> dplyr::mutate(outcome = ifelse(nbapersonid %in% rotation_players$nbapersonid, "ROTATIONAL", outcome))

#rostered
rostered_players <- train_data %>% filter(games>=1) %>%
  group_by(nbapersonid) %>% 
  count() %>%
  filter(n>1)

#cant be in roster group if in any previous groups
rostered_players <- filter(rostered_players, nbapersonid %notin% all_star_players$nbapersonid)
rostered_players <- filter(rostered_players, nbapersonid %notin% elite_players$nbapersonid)
rostered_players <- filter(rostered_players, nbapersonid %notin% starter_players$nbapersonid)
rostered_players <- filter(rostered_players, nbapersonid %notin% rotation_players$nbapersonid)

data <- data |> dplyr::mutate(outcome = ifelse(nbapersonid %in% rostered_players$nbapersonid, "ROSTERED", outcome))

train_data2 <- filter(data, years_played<4 & draftyear<2017)

train_data2 <- train_data2 |> dplyr::mutate(outcome = ifelse(outcome == "OUTCOME", "OUT OF LEAGUE", outcome))

train_data2 <- subset(train_data2, select = c(player, nbapersonid, season, draftyear, games, games_start, outcome, fgm,
                                            fga, tot_reb, ast, steals, blocks, tov, points, WS, VORP))
test_data <- subset(test_data, select = c(player, nbapersonid, season, draftyear, games, games_start, outcome, fgm,
                                            fga, tot_reb, ast, steals, blocks, tov, points, WS, VORP))

#summarizing so players w/ less than four years aren't outweighed
train_data2 <- train_data2 %>% group_by(player, nbapersonid, outcome, draftyear) %>%
  summarise(across(c(games, games_start, fgm, fga, tot_reb, ast, 
                     steals, blocks, tov, points, WS, VORP), sum))


test_data <- test_data %>% group_by(player, nbapersonid, outcome, draftyear) %>%
  summarise(across(c(games, games_start, fgm, fga, tot_reb, ast, 
                     steals, blocks, tov, points, WS, VORP), sum))


#making each value per game to even playing field
train_data2$points <- train_data2$points/train_data2$games
colnames(train_data2)[which(names(train_data2)=="points")] <- "ppg"
train_data2$tot_reb <- train_data2$tot_reb/train_data2$games
colnames(train_data2)[which(names(train_data2)=="tot_reb")] <- "rbpg"
train_data2$ast <- train_data2$ast/train_data2$games
colnames(train_data2)[which(names(train_data2)=="ast")] <- "apg"
train_data2$steals <- train_data2$steals/train_data2$games
colnames(train_data2)[which(names(train_data2)=="steals")] <- "spg"
train_data2$tov <- train_data2$tov/train_data2$games
colnames(train_data2)[which(names(train_data2)=="tov")] <- "topg"
train_data2$blocks <- train_data2$blocks/train_data2$games
colnames(train_data2)[which(names(train_data2)=="blocks")] <- "bpg"
train_data2$fgp <- train_data2$fgm/train_data2$fga

train_data2 <- train_data2 |> dplyr::mutate(WS = ifelse(draftyear==2016, WS/2, WS))
train_data2 <- train_data2 |> dplyr::mutate(VORP = ifelse(draftyear==2016, VORP/2, VORP))
train_data2 <- train_data2 |> dplyr::mutate(WS = ifelse(draftyear==2015, WS/3, WS))
train_data2 <- train_data2 |> dplyr::mutate(VORP = ifelse(draftyear==2015, VORP/3, VORP))
train_data2 <- train_data2 |> dplyr::mutate(WS = ifelse(draftyear<2015, WS/4, WS))
train_data2 <- train_data2 |> dplyr::mutate(VORP = ifelse(draftyear<2015, VORP/4, VORP))

test_data$points <- test_data$points/test_data$games
colnames(test_data)[which(names(test_data)=="points")] <- "ppg"
test_data$tot_reb <- test_data$tot_reb/test_data$games
colnames(test_data)[which(names(test_data)=="tot_reb")] <- "rbpg"
test_data$ast <- test_data$ast/test_data$games
colnames(test_data)[which(names(test_data)=="ast")] <- "apg"
test_data$steals <- test_data$steals/test_data$games
colnames(test_data)[which(names(test_data)=="steals")] <- "spg"
test_data$tov <- test_data$tov/test_data$games
colnames(test_data)[which(names(test_data)=="tov")] <- "topg"
test_data$blocks <- test_data$blocks/test_data$games
colnames(test_data)[which(names(test_data)=="blocks")] <- "bpg"
test_data$fgp <- test_data$fgm/test_data$fga

test_data <- test_data |> dplyr::mutate(WS = ifelse(draftyear==2020, WS/2, WS))
test_data <- test_data |> dplyr::mutate(VORP = ifelse(draftyear==2020, VORP/2, VORP))
test_data <- test_data |> dplyr::mutate(WS = ifelse(draftyear==2019, WS/3, WS))
test_data <- test_data |> dplyr::mutate(VORP = ifelse(draftyear==2019, VORP/3, VORP))
test_data <- test_data |> dplyr::mutate(WS = ifelse(draftyear==2018, WS/4, WS))
test_data <- test_data |> dplyr::mutate(VORP = ifelse(draftyear==2018, VORP/4, VORP))

#changing to factor so I can scale
train_data2 <- train_data2 %>% mutate_if(is.character, as.factor)

#random forest experiment
library(randomForest)
rf1 <- randomForest(as.factor(outcome) ~ games_start + games + ppg + rbpg + apg + spg + bpg + topg + WS + VORP,
                      data = train_data2, ntree = 1000, mtry = 5)


#running prediction
predictionRF <- predict(rf1, newdata = test_data, type = "prob")
rownames(predictionRF) <- test_data$player

#reordering columns so it goes Elite -> Out of League
predictionRF <- predictionRF[, c(2,1,6,5,4,3)]

subset(predictionRF, rownames(predictionRF) %in% ('FIRST LAST'))

#Variable Importance Plot
randomForest::varImpPlot(rf1,
                         sort=T,
                         main="Variable Importance Plot")

