Test_PbP_Data <- Test_PbP_Data[Test_PbP_Data$game_period < 5,]
Test_PbP_Data <- is_home(Test_PbP_Data)
Test_PbP_Data <- is_goal(Test_PbP_Data)
Test_PbP_Data <- Test_PbP_Data %>% group_by(game_id) %>%
arrange(event_index, .by_group = TRUE) %>%
    mutate(time_diff = game_seconds - lag(game_seconds))

#marking shots as rebound or rush
Test_PbP_Data$time_diff[is.na(Test_PbP_Data$time_diff)] <- 0
Test_PbP_Data$is_home[is.na(Test_PbP_Data$is_home)] <- 0

Test_PbP_Data$is_rebound <- ifelse(Test_PbP_Data$time_diff < 3 & 
                                        Test_PbP_Data$event_type %in% fenwick_events &
                                        Test_PbP_Data$event_team == 
                                        lag(Test_PbP_Data$event_team),
                                    1, 0)

Test_PbP_Data$is_rush <- ifelse(Test_PbP_Data$time_diff < 4 &
                                     lag(abs(Test_PbP_Data$coords_x)) < 25 &
                                     Test_PbP_Data$event_type %in% fenwick_events,
                                 1, 0)

Test_PbP_Data$is_rebound[is.na(Test_PbP_Data$time_diff)] <- 0
Test_PbP_Data$is_rush[is.na(Test_PbP_Data$is_rush)] <- 0
Test_Fenwick_Data<- filter(Test_PbP_Data, event_type %in% c("SHOT", "MISS", "GOAL"))
Test_Fenwick_Data <- filter(Test_Fenwick_Data, !is.na(event_detail))
Test_Fenwick_Data$event_detail<- as.factor(Test_Fenwick_Data$event_detail)
Test_PbP_Data <- filter(Test_PbP_Data, coords_x != 'NA' & coords_y != 'NA')
#shot angle
Test_Fenwick_Data$coords_y <- ifelse(Test_Fenwick_Data$coords_x < 0,
                                      -1 * Test_Fenwick_Data$coords_y, Test_Fenwick_Data$coords_y)

Test_Fenwick_Data$coords_x <- abs(Test_Fenwick_Data$coords_x)


Test_Fenwick_Data$shot_angle <- (asin(abs(Test_Fenwick_Data$coords_y)/sqrt((87.95 
                - Test_Fenwick_Data$coords_x)^2 + Test_Fenwick_Data$coords_y^2))*180)/ 3.14

Test_Fenwick_Data$shot_angle <- ifelse(Test_Fenwick_Data$coords_x > 88, 90 + 
                                            (90 - Test_Fenwick_Data$shot_angle), 
                                        Test_Fenwick_Data$shot_angle)
#shooter strength
Test_Fenwick_Data$distance <- sqrt(Test_Fenwick_Data$coords_x^2 + Test_Fenwick_Data$coords_y^2)

Test_Fenwick_Data$shooter_strength <- ifelse(Test_Fenwick_Data$game_strength_state %in% even_strength, 
                                              'EV', NA)
Test_Fenwick_Data$shooter_strength <- ifelse(Test_Fenwick_Data$game_strength_state %in% home_advantage
                                              & Test_Fenwick_Data$is_home == 1, 'PP', Test_Fenwick_Data$shooter_strength)
Test_Fenwick_Data$shooter_strength <- ifelse(Test_Fenwick_Data$game_strength_state %in% away_advantage
                                              & Test_Fenwick_Data$is_home == 1, 'SH', Test_Fenwick_Data$shooter_strength)
Test_Fenwick_Data$shooter_strength <- ifelse(Test_Fenwick_Data$game_strength_state %in% home_advantage
                                              & Test_Fenwick_Data$is_home == 0, 'SH', Test_Fenwick_Data$shooter_strength)
Test_Fenwick_Data$shooter_strength <- ifelse(Test_Fenwick_Data$game_strength_state %in% away_advantage
                                              & Test_Fenwick_Data$is_home == 0, 'PP', Test_Fenwick_Data$shooter_strength)
Test_Fenwick_Data$shooter_strength <- ifelse(Test_Fenwick_Data$game_strength_state %in% home_empty_net
                                              & Test_Fenwick_Data$is_home == 1, 'PP', Test_Fenwick_Data$shooter_strength)
Test_Fenwick_Data$shooter_strength <- ifelse(Test_Fenwick_Data$game_strength_state %in% home_empty_net
                                              & Test_Fenwick_Data$is_home == 0, 'EN', Test_Fenwick_Data$shooter_strength)
Test_Fenwick_Data$shooter_strength <- ifelse(Test_Fenwick_Data$game_strength_state %in% away_empty_net
                                              & Test_Fenwick_Data$is_home == 1, 'EN', Test_Fenwick_Data$shooter_strength)
Test_Fenwick_Data$shooter_strength <- ifelse(Test_Fenwick_Data$game_strength_state %in% away_empty_net
                                              & Test_Fenwick_Data$is_home == 0, 'PP', Test_Fenwick_Data$shooter_strength)
Test_Fenwick_Data$shooter_strength <- ifelse(Test_Fenwick_Data$game_strength_state %in% home_empty_net
                                              & Test_Fenwick_Data$is_home == 1, 'PP', Test_Fenwick_Data$shooter_strength)
Test_Fenwick_Data$shooter_strength <- ifelse(Test_Fenwick_Data$game_strength_state %in% c('Ev0', '0vE'), 
                                              'PS', Test_Fenwick_Data$shooter_strength)

shot_vars <- c('game_id','event_index','coords_x', "coords_y", 'shot_angle', 
               'shooter_strength', 'is_goal', 'event_detail', 'distance', 'is_home',
               'is_rebound', 'is_rush')

test_model <- Test_Fenwick_Data[,shot_vars]
#predictin xG by data
test_model$xG <- predict(xGmodel, test_model, type = "response")
test_model$Goal_Predictions <- predict(xGmodel, test_model)


xGvsGF <- group_by(Test_Fenwick_Data, game_id) %>% 
    summarise('HomeGoals' = sum(Home_Goal), 'AwayGoals' = sum(Away_Goal), 
              'GFPercent' = sum(Home_Goal)/(sum(Away_Goal) + sum(Home_Goal)),
              'Home_xG' = sum(xGHome), 'Away_xG' = sum(xGAway), 
              'xGFPercent' = sum(xGHome)/(sum(xGAway)+
                                                                                                                                                sum(xGHome)))

head(xGvsGF)