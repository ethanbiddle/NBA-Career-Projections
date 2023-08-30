library(tidyverse)
library(dplyr)
awards <- read_csv("awards_data.csv")
player_data <- read_csv("player_stats.csv")
team_data <- read_csv("team_stats.csv")
rebounding_data <- read_csv("team_rebounding_data_22.csv")


#Q1

#Making awards and players dataset smaller with only the values I need
new_awards1 <- subset(awards, select = c(nbapersonid, `All NBA First Team`, `All NBA Second Team`, 
                                        `All NBA Third Team`, all_star_game, season))
new_awards1 <- new_awards1 %>% filter(`All NBA First Team`==1 | `All NBA Second Team`==1| 
                                        `All NBA Third Team`==1 | all_star_game ==T)
#Summarising player data to account for midseason trades
new_pd1 <- subset(player_data, select = c(nbapersonid, games, points, season))
new_pd1 <- new_pd1 %>% group_by(nbapersonid, season) %>%
  summarise(across(c(points, games), sum))

#Merge two datasets via playerID
data_Q1 <- merge(new_awards1, new_pd1, by = "nbapersonid", all.x = T, all.y = T)
data_Q1 <- data_Q1 %>% filter(season.x==season.y)

#Add Points per Game column
data_Q1$ppg <- data_Q1$points/data_Q1$games

#Calculate each Team
first_team <- data_Q1 %>% filter(data_Q1$`All NBA First Team`==1)
summary(first_team) #25.9

second_team <- data_Q1 %>% filter(data_Q1$`All NBA Second Team`==1)
summary(second_team) #23.1

third_team <- data_Q1 %>% filter(data_Q1$`All NBA Third Team`==1)
summary(third_team) #20.6

all_stars <- data_Q1 %>% filter(data_Q1$`all_star_game`==T)
summary(all_stars) #21.6

#Q2

#Making awards and players dataset smaller with only the values I need, code runs faster
new_awards2 <- subset(awards, select = c(nbapersonid, season, `All NBA First Team`, `All NBA Second Team`, 
                                        `All NBA Third Team`))
new_awards2 <- new_awards2 %>% filter(`All NBA First Team`==1 | `All NBA Second Team`==1| `All NBA Third Team`==1)

new_pd2 <- subset(player_data, select = c(nbapersonid, draftyear, season))
new_pd2 <- unique(new_pd2)

#Merging the two datasets via PlayerID
data_Q2 <- merge(x=new_awards2,y=new_pd2, 
                 by="nbapersonid")
data_Q2 <- data_Q2 %>% filter(season.x==season.y)
data_Q2 <- data_Q2 %>% filter(draftyear>2006)

#Creating variable for years played
data_Q2$years_played <- data_Q2$season.x-data_Q2$draftyear

#arranging data so that we keep the lowest years played value
data_Q2 <- arrange(data_Q2, years_played)

#get rid of repeat values after first year as All NBA
data_Q2_minimized <- data_Q2 %>% distinct(nbapersonid, .keep_all = T)

summary(data_Q2_minimized)


#Q3
#Making awards and player data sets smaller so only take values I need
new_awards3 <- subset(awards, select = c(nbapersonid, season, `All NBA First Team`, `All NBA Second Team`, 
                                         `All NBA Third Team`, `Defensive Player Of The Year_rk`, `Most Valuable Player_rk`
                                         , all_star_game))
#take out duplicates via mid-season trades
new_awards3 <- unique(new_awards3)

new_pd3 <- subset(player_data, select = c(nbapersonid, draftyear, season, games, 
                                          games_start, mins))
#take out duplicates via mid-season trades
new_pd3 <- new_pd3 %>% group_by(nbapersonid, season, draftyear) %>%
  summarise(across(c(games_start, games, mins), sum))

#merging two datasets
data_Q3 <- left_join(new_pd3, new_awards3)

#creating years played variable
data_Q3$years_played <- data_Q3$season-data_Q3$draftyear

#filtering for first four years of career
data_Q3 <- data_Q3 %>% filter(years_played>4)

#elites
elite_seasons <- data_Q3 %>% filter(`All NBA First Team`==1 | `All NBA Second Team`==1| 
                              `All NBA Third Team`==1 | `Defensive Player Of The Year_rk` == 1 |
                            `Most Valuable Player_rk` == 1)
#only want players if they did it twice
elite_players <- elite_seasons[duplicated(elite_seasons$nbapersonid),]
number_of_elites <- length(unique(elite_players$nbapersonid))

#allstars
all_star_seasons <- data_Q3 %>% filter(all_star_game==T)

#cant be in allstar group if in elite group
`%notin%` <- Negate(`%in%`)
all_star_seasons <- filter(all_star_seasons, nbapersonid %notin% elite_players$nbapersonid)

#only want players if they did it twice
all_star_players <- all_star_seasons[duplicated(all_star_seasons$nbapersonid),]
number_of_all_stars <- length(unique(all_star_players$nbapersonid))

#adjusting for shortened seasons
data_Q3 <- data_Q3 |> dplyr::mutate(games_start = 
                                              ifelse(season==2011, games_start * (82/66), games_start))
data_Q3 <- data_Q3 |> dplyr::mutate(games_start = 
                                      ifelse(season== 2019 | season == 2020, games_start * (82/72), games_start))
data_Q3 <- data_Q3 |> dplyr::mutate(mins = 
                                      ifelse(season==2011, mins * (82/66), mins))
data_Q3 <- data_Q3 |> dplyr::mutate(mins = 
                                      ifelse(season== 2019 | season == 2020, mins * (82/72), mins))

#starters
starter_seasons <- data_Q3 %>% filter(games_start>40 | mins > 1999)

#cant be in starters if in all star or elite group
starter_seasons <- filter(starter_seasons, nbapersonid %notin% all_star_players$nbapersonid)
starter_seasons <- filter(starter_seasons, nbapersonid %notin% elite_players$nbapersonid)

#only want if they did it twice
starter_players <- starter_seasons[duplicated(starter_seasons$nbapersonid),]
number_of_starters <- length(unique(starter_players$nbapersonid))

#rotationals
rotation_seasons <- data_Q3 %>% filter(mins>999)

#cant be in rotation group if in any previous groups
rotation_seasons <- filter(rotation_seasons, nbapersonid %notin% all_star_players$nbapersonid)
rotation_seasons <- filter(rotation_seasons, nbapersonid %notin% elite_players$nbapersonid)
rotation_seasons <- filter(rotation_seasons, nbapersonid %notin% starter_players$nbapersonid)

#only want if they did it twice
rotation_players <- rotation_seasons[duplicated(rotation_seasons$nbapersonid),]
number_of_rotationals <- length(unique(rotation_players$nbapersonid))

#rostered
roster_seasons <- data_Q3 %>% filter(mins>0)

#cant be in roster group if in any previous groups
roster_seasons <- filter(roster_seasons, nbapersonid %notin% all_star_players$nbapersonid)
roster_seasons <- filter(roster_seasons, nbapersonid %notin% elite_players$nbapersonid)
roster_seasons <- filter(roster_seasons, nbapersonid %notin% starter_players$nbapersonid)
roster_seasons <- filter(roster_seasons, nbapersonid %notin% rotation_players$nbapersonid)

#only want if they did it twice
roster_players <- roster_seasons[duplicated(roster_seasons$nbapersonid),]
number_of_rostered <- length(unique(roster_players$nbapersonid))

#out of league if they didnt fit any other category
out_of_league <- filter(new_pd3, nbapersonid %notin% elite_players$nbapersonid)
out_of_league <- filter(out_of_league, nbapersonid %notin% all_star_players$nbapersonid)
out_of_league <- filter(out_of_league, nbapersonid %notin% starter_players$nbapersonid)
out_of_league <- filter(out_of_league, nbapersonid %notin% rotation_players$nbapersonid)
out_of_league <- filter(out_of_league, nbapersonid %notin% roster_players$nbapersonid)
number_of_ool <- length(unique(out_of_league$nbapersonid))

#2010 Players

#elite
elite_2010 <- filter(elite_players, draftyear == 2010)
#dont want to double count
elite_2010 <- elite_2010[!duplicated(elite_2010$nbapersonid), ]
count(elite_2010) #1

#all-stars
all_star_2010 <- filter(all_star_players, draftyear == 2010)
#dont want to double count
all_star_2010 <- all_star_2010[!duplicated(all_star_2010$nbapersonid), ]
count(all_star_2010) #2

#starters
starters_2010 <- filter(starter_players, draftyear == 2010)
#dont want to double count
starters_2010 <- starters_2010[!duplicated(starters_2010$nbapersonid), ]
count(starters_2010) #9

#rotationals
rotation_2010 <- filter(rotation_players, draftyear == 2010)
#dont want to double count
rotation_2010 <- rotation_2010[!duplicated(rotation_2010$nbapersonid), ]
count(rotation_2010) #8

#rostered
roster_2010 <- filter(roster_players, draftyear == 2010)
#dont want to double count
roster_2010 <- roster_2010[!duplicated(roster_2010$nbapersonid), ]
count(roster_2010) #11

#out of league
ool_2010 <- filter(out_of_league, draftyear == 2010)
#dont want to double count
ool_2010 <- ool_2010[!duplicated(ool_2010$nbapersonid), ]
count(ool_2010) #42


#Q4

#subset data with values I need
new_pd4 <- subset(player_data, select = c(player, nbapersonid, draftyear, season, games, 
                                          games_start, mins, fgm, fga, tot_reb, ast, 
                                          steals, blocks, tov, points, usg, WS, VORP))

#training data only players drafted 2015 and earlier
train_data_Q4 <- filter(new_pd4, draftyear <2016)

#creating years played variable
train_data_Q4$years_played <- train_data_Q4$season - train_data_Q4$draftyear

#creating outcome variable to store value
train_data_Q4$outcome <- "OUTCOME"

#using datasets from previous cleaning to assign values
train_data_Q4 <- train_data_Q4 |> dplyr::mutate(outcome = 
                                      ifelse(nbapersonid %in% elite_players$nbapersonid, "ELITE", outcome))
train_data_Q4 <- train_data_Q4 |> dplyr::mutate(outcome = 
                                      ifelse(nbapersonid %in% all_star_players$nbapersonid, "ALL-STAR", outcome))
train_data_Q4 <- train_data_Q4 |> dplyr::mutate(outcome = 
                                      ifelse(nbapersonid %in% starter_players$nbapersonid, "STARTER", outcome))
train_data_Q4 <- train_data_Q4 |> dplyr::mutate(outcome = 
                                      ifelse(nbapersonid %in% rotation_players$nbapersonid, "ROTATIONAL", outcome))
train_data_Q4 <- train_data_Q4 |> dplyr::mutate(outcome = 
                                      ifelse(nbapersonid %in% roster_players$nbapersonid, "ROSTERED", outcome))
train_data_Q4 <- train_data_Q4 |> dplyr::mutate(outcome = 
                                      ifelse(nbapersonid %in% out_of_league$nbapersonid, "OUT OF LEAGUE", outcome))

#only want first four years of each players career
train_data_Q4 <- filter(train_data_Q4, years_played<4)

#summarizing so players w/ less than four years arent outweighed (i.e. LeBron only has one season in this dataset)
train_data_Q4 <- train_data_Q4 %>% group_by(player, nbapersonid, outcome) %>%
  summarise(across(c(games_start, games, mins, fgm, fga, tot_reb, ast, 
                     steals, blocks, tov, points, usg, WS, VORP), mean))

#making each value per game to even playing field
train_data_Q4$points <- train_data_Q4$points/train_data_Q4$games
colnames(train_data_Q4)[which(names(train_data_Q4)=="points")] <- "ppg"
train_data_Q4$tot_reb <- train_data_Q4$tot_reb/train_data_Q4$games
colnames(train_data_Q4)[which(names(train_data_Q4)=="tot_reb")] <- "rbpg"
train_data_Q4$ast <- train_data_Q4$ast/train_data_Q4$games
colnames(train_data_Q4)[which(names(train_data_Q4)=="ast")] <- "apg"
train_data_Q4$steals <- train_data_Q4$steals/train_data_Q4$games
colnames(train_data_Q4)[which(names(train_data_Q4)=="steals")] <- "spg"
train_data_Q4$tov <- train_data_Q4$tov/train_data_Q4$games
colnames(train_data_Q4)[which(names(train_data_Q4)=="tov")] <- "topg"
train_data_Q4$mins <- train_data_Q4$mins/train_data_Q4$games
colnames(train_data_Q4)[which(names(train_data_Q4)=="mins")] <- "minpg"
train_data_Q4$blocks <- train_data_Q4$blocks/train_data_Q4$games
colnames(train_data_Q4)[which(names(train_data_Q4)=="blocks")] <- "bpg"

#changing to factor so I can scale
train_data_Q4 <- train_data_Q4 %>% mutate_if(is.character, as.factor)

#random forest experiment
library(randomForest)
rf1 <- randomForest(outcome ~ games_start + games + ppg + rbpg + apg + spg + bpg + topg + minpg + WS + VORP + usg,
                      data = train_data_Q4, ntree = 1000, mtry = 5)

#creating test data

#only want players drafted after 2018
test_data_Q4 <- filter(new_pd4, draftyear > 2017)

#fix name changes with accents and shortened first names
test_data_Q4$player <- iconv(test_data_Q4$player, from = 'UTF-8', to = 'ASCII//TRANSLIT')
test_data_Q4$player <- str_replace_all(test_data_Q4$player, "'", "")
test_data_Q4$player <- str_replace_all(test_data_Q4$player, "B.J.", "BJ")
test_data_Q4$player <- str_replace_all(test_data_Q4$player, "R.J.", "RJ")

#creating years played variable
test_data_Q4$years_played <- test_data_Q4$season - test_data_Q4$draftyear

#only want first four years of career
test_data_Q4 <- filter(test_data_Q4, years_played<4)

#creating variable for response
test_data_Q4$outcome <- "OUTCOME"

#summarizing so we get players averages like we did with train data
test_data_Q4 <- test_data_Q4 %>% group_by(player, nbapersonid, outcome) %>%
  summarise(across(c(games_start, games, mins, fgm, fga, tot_reb, ast, 
                     steals, blocks, tov, points, usg, WS, VORP), mean))

test_data_Q4$points <- test_data_Q4$points/test_data_Q4$games
colnames(test_data_Q4)[which(names(test_data_Q4)=="points")] <- "ppg"
test_data_Q4$tot_reb <- test_data_Q4$tot_reb/test_data_Q4$games
colnames(test_data_Q4)[which(names(test_data_Q4)=="tot_reb")] <- "rbpg"
test_data_Q4$ast <- test_data_Q4$ast/test_data_Q4$games
colnames(test_data_Q4)[which(names(test_data_Q4)=="ast")] <- "apg"
test_data_Q4$steals <- test_data_Q4$steals/test_data_Q4$games
colnames(test_data_Q4)[which(names(test_data_Q4)=="steals")] <- "spg"
test_data_Q4$tov <- test_data_Q4$tov/test_data_Q4$games
colnames(test_data_Q4)[which(names(test_data_Q4)=="tov")] <- "topg"
test_data_Q4$mins <- test_data_Q4$mins/test_data_Q4$games
colnames(test_data_Q4)[which(names(test_data_Q4)=="mins")] <- "minpg"
test_data_Q4$blocks <- test_data_Q4$blocks/test_data_Q4$games
colnames(test_data_Q4)[which(names(test_data_Q4)=="blocks")] <- "bpg"

#running prediction
predictionRF <- predict(rf1, newdata = test_data_Q4, type = "prob")
rownames(predictionRF) <- test_data_Q4$player

#reordering columns so it goes Elite -> Out of League
predictionRF <- predictionRF[, c(6,5,3,2,4,1)]

#Variable Importance Plot
randomForest::varImpPlot(rf1,
                         sort=T,
                         main="Variable Importance Plot")

