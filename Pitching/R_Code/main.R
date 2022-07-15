setwd('C:/Users/rusla/OneDrive/MLBAnalyticsJobs/MLB Player Projections/Pitching/R_Code')
library('dplyr')
library('knitr')
library('corrplot')
library('Lahman')
library('ggplot2')
library('sqldf')
library('GGally')
library('openxlsx')
library('caret')
library('stringr')
library('rhandsontable')
library('knitr')
library('baseballr')
library('rvest')
library('xml2')
library('kableExtra')
source('functions.R')
options(scipen = 999)
current_season <- as.numeric(substr(Sys.Date(), 1, 4)) - 1
teamnames <- baseballr::teams_lu_table %>% 
  filter(sport.name == "Major League Baseball") %>% 
  select(teamName, abbreviation, league.name) %>% distinct() %>% 
  mutate(teamName = case_when(teamName == "D-backs" ~ "Dbacks",
                              TRUE ~ teamName)) %>% 
  rename("Team" = "abbreviation") %>% 
  add_row(teamName = "Expos", Team = "MON") %>% 
  mutate(League = case_when(league.name == "American League" ~ "AL",
                            league.name == "National League" ~ "AL",
                            TRUE ~ "NL")) %>% select(-league.name) %>% 
  add_row(Team = "FLA", League = "NL") %>% 
  add_row(Team = "WSN", League = "NL") %>% 
  add_row(Team = "SFG", League = "NL") %>%
  add_row(Team = "CHW", League = "AL") %>% 
  add_row(Team = "KCR", League = "AL") %>% 
  add_row(Team = "SDP", League = "NL") %>%
  add_row(Team = "TBR", League = "AL") %>%
  add_row(Team = "ANA", League = "AL") %>%
  add_row(Team = "TBD", League = "AL") 

park_factors <- get_park_factors(2002, current_season) %>% 
  select(Season, Team, home_team, hr)

pitching <- scrape_fangraphs_range(2002, current_season, "pitching")
pitching_clean <- clean_pitching_data(pitching) %>% 
  arrange(Name, Season) %>%
  group_by(playerid) %>%
  mutate(Team = na_if(Team, "- - -")) %>%
  tidyr::fill(Team, .direction = 'updown') %>% ungroup()

pitching2 <- add_projection_years(pitching_clean, current_season + 5)
pitching3 <- add_projected_prior_seasons(pitching2)

pitching_stats <- c('G','GS','G_pct','GS_pct','IP','ERA','tERA','FIP','xFIP','WAR','AVG',
                    'WHIP','BABIP','SO','K_9','K_pct','BB','BB_9','BB_pct','K_BB','H',
                    'H_9','HR','HR_9','LD_pct','GB_pct','FB_pct','HR_FB','Strike_pct',
                    'FBv','FBall_pct','O_Swing_pct','Z_Swing_pct','Swing_pct',
                    'O_Contact_pct','Z_Contact_pct','Contact_pct','Zone_pct',
                    'F_Strike_pct','SwStr_pct','IP_G','StartIP_GS', 'BIP_IP','H_IP')
pitching_full_data <- merge_pitching_stats(pitching3, pitching_stats)

historical_future <- historical_future_split(pitching_full_data, current_season)
historical <- historical_future[[1]] %>% 
  mutate(Num_Pitches_Harmonic = 2 / ((1 / Pitches_Current) + (1 / Pitches_Projected)))
future <- historical_future[[2]]

#####################################################################################
# G
x_vars <- c('G_pct_Current','G_pct_Prior','G_pct_Prior_2',"Age_Projected",
            'MLB_Service_Projected','Pos_Group_Current','Num_Pitches_Harmonic')
future_preds_g <- train_models(historical, 'G_pct_Projected', x_vars, "rqlasso", 3, 1:5)
future_preds <- future_preds_g[[1]] %>% select(Name, Team, Age_Projected, Pos_Group_Current,
                                               Season_Projected, G_pct_Projected) %>% 
  mutate(G_Projected = round(G_pct_Projected * 162)) %>% 
  select(-G_pct_Projected)
####################################################################################
# GS
x_vars <- c('GS_pct_Current','GS_pct_Prior','GS_pct_Prior_2',"Age_Projected",
            'MLB_Service_Projected','Pos_Group_Current','Num_Pitches_Harmonic')
future_preds_gs <- train_models(historical, 'GS_pct_Projected', x_vars, "rqlasso", 3, 1:5)
future_preds <- future_preds %>%
  left_join(future_preds_gs[[1]] %>% select(Name, Team, Season_Projected, 
                                            GS_pct_Projected), 
            by = c("Name","Team","Season_Projected")) %>% 
  mutate(GS_Projected = round(GS_pct_Projected * 162)) %>% 
  select(-GS_pct_Projected) %>% 
  mutate(GS_Projected = case_when((Pos_Group_Current == "RP") & (GS_Projected < 10) ~ 0,
                                  TRUE ~ GS_Projected),
         GS_Projected = case_when(GS_Projected > G_Projected ~ G_Projected,
                                  TRUE ~ GS_Projected),
         GS_Projected = case_when((Pos_Group_Current == "SP") & (G_Projected >= 22) & (Name != "Dinelson Lamet") ~ G_Projected, 
                                  TRUE ~ GS_Projected),
         GS_Projected = case_when((Pos_Group_Current == "SP") & (GS_Projected > 33) ~ 33,
                                  TRUE ~ GS_Projected),
         G_Projected = case_when((Pos_Group_Current == "SP") & (G_Projected > 33) ~ 33,
                                  TRUE ~ G_Projected))
#####################################################################################  
# IP
x_vars <- c('IP_G_Current','IP_G_Prior','IP_G_Prior_2',"Age_Projected",
            'MLB_Service_Projected','Pos_Group_Current','Num_Pitches_Harmonic')
future_preds_ip <- train_models(historical, 'IP_G_Projected', x_vars, "rqlasso", 3, 1:5)
future_preds <- future_preds %>%
  left_join(future_preds_ip[[1]] %>% select(Name, Team, Season_Projected, IP_G_Projected_Lower,
                                            IP_G_Projected, IP_G_Projected_Upper), 
            by = c("Name","Team","Season_Projected")) %>% 
  mutate(IP_Projected_Lower = round(IP_G_Projected_Lower * G_Projected, 1),
         IP_Projected = round(IP_G_Projected * G_Projected, 1),
         IP_Projected_Upper = round(IP_G_Projected_Upper * G_Projected, 1),
         IP_Projected_Upper = case_when((Pos_Group_Current == "RP") & (IP_Projected_Upper > 110) ~ 110,
                                        TRUE ~ IP_Projected_Upper),
         IP_Projected_Upper = case_when((Pos_Group_Current == "SP") & (IP_Projected_Upper > 250) ~ 250,
                                        TRUE ~ IP_Projected_Upper)) %>% 
  select(-IP_G_Projected_Lower, -IP_G_Projected, -IP_G_Projected_Upper)
#####################################################################################
# H/9
x_vars <- c('H_9_Current','H_9_Prior','H_9_Prior_2','LD_pct_Current','GB_pct_Current',
            'FB_pct_Current','Strike_pct_Current','FBv_Current','FBall_pct_Current',
            'O_Swing_pct_Current','Z_Swing_pct_Current','Swing_pct_Current',
            'O_Contact_pct_Current','Z_Contact_pct_Current','Contact_pct_Current',
            'Zone_pct_Current','F_Strike_pct_Current','SwStr_pct_Current',"Age_Projected",
            'MLB_Service_Projected','Pos_Group_Current','Num_Pitches_Harmonic')
future_preds_h <- train_models(historical, 'H_9_Projected', x_vars, "rqlasso", 3, 1:5)
future_preds <- future_preds %>%
  left_join(future_preds_h[[1]] %>% select(Name, Team, Season_Projected, H_9_Projected_Lower,
                                            H_9_Projected, H_9_Projected_Upper), 
            by = c("Name","Team","Season_Projected")) %>% 
  mutate(H_9_Projected_Lower = round(H_9_Projected_Lower, 2),
         H_9_Projected = round(H_9_Projected, 2),
         H_9_Projected_Upper = round(H_9_Projected_Upper, 2),
         H_Projected_Lower = round((H_9_Projected_Lower / 9) * IP_Projected_Lower),
         H_Projected = round((H_9_Projected / 9) * IP_Projected),
         H_Projected_Upper = round((H_9_Projected_Upper / 9) * IP_Projected_Upper))

future_preds <- na.omit(future_preds)
setwd("C:/Users/rusla/OneDrive/MLBAnalyticsJobs/MLB Player Projections/Pitching/Data")
write.csv(future_preds, "pitching_projections_data.csv", row.names = F)

######################################################################################




pitching <- pitching %>% 
  mutate(TrueFB. = FB. - (FB. * IFFB.),
         TrueIFFB. = (FB. * IFFB.),
         TBF = IP * 2.9 + H + BB + HBP,
         BIP = TBF - HR - SO - BB - HBP,
         BIP_PerIP = BIP / IP,
         H_PerIP = H / IP,
         BB_PerIP = BB / IP,
         HBP_PerIP = HBP / IP) %>% 
  group_by(Season, League) %>%
  mutate(League_ERA = mean(ERA, na.rm = T),
         League_FIP = mean(FIP, na.rm = T),
         League_xFIP = mean(xFIP, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(ERA_minus = (ERA + (ERA - (ERA * ESPN_PF))) / (League_ERA) * 100,
         FIP_minus = (FIP + (FIP - (FIP * ESPN_PF))) / (League_FIP) * 100,
         xFIP_minus = (xFIP + (xFIP - (xFIP * ESPN_PF))) / (League_xFIP) * 100)
###################################################################################
# Projecting Hits Per IP, Hits
pitchers <- add_projection_years(pitchers, 11)
pitchers2 <- add_projected_prior_seasons(pitchers)
pitchers2 <- merge_pitching_stats(pitchers2, c('IP','Pos_Group','H_PerIP','K.','BB.',
                                               'LD.','GB.','FB.','Soft.',
                                               'Med.','Hard.','Oppo.','Cent.',
                                               'Pull.','Contact.','Zone.'))
history_future <- historical_future_split(pitchers2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(IP_Harmonic = 2 / ((1 / IP_Current) + (1 / IP_Projected)))

y_var <- c('H_PerIP_Projected')
x_vars <- c('H_PerIP_Current','H_PerIP_Prior','H_PerIP_Prior_2','poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','LD._Current',
            'LD._Prior','LD._Prior_2','GB._Current','GB._Prior','GB._Prior_2',
            'FB._Current','FB._Prior','FB._Prior_2','Soft._Current','Soft._Prior',
            'Soft._Prior_2','Med._Current','Med._Prior','Med._Prior_2','Hard._Current',
            'Hard._Prior','Hard._Prior_2','Oppo._Current','Oppo._Prior','Oppo._Prior_2',
            'Cent._Current','Cent._Prior','Cent._Prior_2','Pull._Current',
            'Pull._Prior','Pull._Prior_2','Contact._Current','Contact._Prior',
            'Contact._Prior_2','Zone._Current','Zone._Prior','Zone._Prior_2',
            'IP_Harmonic')
x_vars2 <- c('H_PerIP_Current','H_PerIP_Prior','H_PerIP_Prior_2','Age_Projected',
            'Pos_Group_Current','MLB_Service_Projected','LD._Current',
            'LD._Prior','LD._Prior_2','GB._Current','GB._Prior','GB._Prior_2',
            'FB._Current','FB._Prior','FB._Prior_2','Soft._Current','Soft._Prior',
            'Soft._Prior_2','Med._Current','Med._Prior','Med._Prior_2','Hard._Current',
            'Hard._Prior','Hard._Prior_2','Oppo._Current','Oppo._Prior','Oppo._Prior_2',
            'Cent._Current','Cent._Prior','Cent._Prior_2','Pull._Current',
            'Pull._Prior','Pull._Prior_2','Contact._Current','Contact._Prior',
            'Contact._Prior_2','Zone._Current','Zone._Prior','Zone._Prior_2',
            'IP_Harmonic')

list_of_errors <- list()
list_of_mapes <- list()
for (i in 1:5)
{
  print(i)
  print('---------------------------------------------------------')
  x <- train_models(historical, y_var, x_vars, x_vars2, model_type = 'gbm', tuneLength = 5, 
                    years_out = i)
  mapes <- do.call(rbind, x[[1]])
  resid <- unlist(x[[4]])
  list_of_errors[[i]] <- resid
  list_of_mapes[[i]] <- mapes
}
# Run final model through current_season and predict future seasons
errors <- list_of_errors
z <- predict_future_years(historical, future, y_var, x_vars, x_vars2, 
                          model_type = 'gbm', tuneLength = 5, years_out = 5, errors, 1)
model <- z[[2]]
#coef(model$finalModel, model$bestTune$lambda)

H_PerIP <- z[[1]] %>% 
  select(Season_Projected, Playerid, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(H_PerIP = 'Stat_Projected', H_PerIP_Upper = 'Stat_Projected_Upper', 
         H_PerIP_Lower = 'Stat_Projected_Lower')

future_preds <- future_preds %>% 
  inner_join(H_PerIP, by = c('Playerid','Season_Projected')) %>%
  mutate(H = H_PerIP * IP, 
         H_Upper = H_PerIP_Upper * IP_Upper,
         H_Lower = H_PerIP_Lower * IP_Lower)
####################################################################################
# Projecting BB using BB_PerIP
pitchers <- add_projection_years(pitchers, 11)
pitchers2 <- add_projected_prior_seasons(pitchers)
pitchers2 <- merge_pitching_stats(pitchers2, c('IP','Pos_Group','BB_PerIP',
                                               'SwStr.','Swing.','Contact.',
                                               'Zone.','F.Strike.','O.Contact.','Z.Contact.',
                                               'Z.Swing.','O.Swing.'))
history_future <- historical_future_split(pitchers2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(IP_Harmonic = 2 / ((1 / IP_Current) + (1 / IP_Projected)))

y_var <- c('BB_PerIP_Projected')
x_vars <- c('BB_PerIP_Current','BB_PerIP_Prior','BB_PerIP_Prior_2','poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','SwStr._Current','SwStr._Prior',
            'SwStr._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
            'Contact._Current','Contact._Prior','Contact._Prior_2','Zone._Current',
            'Zone._Prior','Zone._Prior_2','F.Strike._Current','F.Strike._Prior',
            'F.Strike._Prior_2','IP_Harmonic')
x_vars2 <- c('BB_PerIP_Current','BB_PerIP_Prior','BB_PerIP_Prior_2','Age_Projected',
             'Pos_Group_Current','MLB_Service_Projected','SwStr._Current','SwStr._Prior',
             'SwStr._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
             'Contact._Current','Contact._Prior','Contact._Prior_2','Zone._Current',
             'Zone._Prior','Zone._Prior_2','F.Strike._Current','F.Strike._Prior',
             'F.Strike._Prior_2','IP_Harmonic')

list_of_errors <- list()
list_of_mapes <- list()
for (i in 1:5)
{
  print(i)
  print('---------------------------------------------------------')
  x <- train_models(historical, y_var, x_vars, x_vars2, model_type = 'gbm', tuneLength = 5, 
                    years_out = i)
  mapes <- do.call(rbind, x[[1]])
  resid <- unlist(x[[4]])
  list_of_errors[[i]] <- resid
  list_of_mapes[[i]] <- mapes
}
# Run final model through current_season and predict future seasons
errors <- list_of_errors
z <- predict_future_years(historical, future, y_var, x_vars, x_vars2, 
                          model_type = 'gbm', tuneLength = 5, years_out = 5, errors, 1)
model <- z[[2]]
#coef(model$finalModel, model$bestTune$lambda)

BB_PerIP <- z[[1]] %>% 
  select(Season_Projected, Playerid, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(BB_PerIP = 'Stat_Projected', BB_PerIP_Upper = 'Stat_Projected_Upper', 
         BB_PerIP_Lower = 'Stat_Projected_Lower')

future_preds <- future_preds %>% 
  inner_join(BB_PerIP, by = c('Playerid','Season_Projected')) %>%
  mutate(BB = BB_PerIP * IP, 
         BB_Upper = BB_PerIP_Upper * IP_Upper,
         BB_Lower = BB_PerIP_Lower * IP_Lower)
####################################################################################
# Projecting HBP Per IP using to find HBP total
pitchers <- add_projection_years(pitchers, 11)
pitchers2 <- add_projected_prior_seasons(pitchers)
pitchers2 <- merge_pitching_stats(pitchers2, c('IP','Pos_Group','HBP_PerIP'))
history_future <- historical_future_split(pitchers2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(IP_Harmonic = 2 / ((1 / IP_Current) + (1 / IP_Projected)))

y_var <- c('HBP_PerIP_Projected')
x_vars <- c('HBP_PerIP_Current','HBP_PerIP_Prior','HBP_PerIP_Prior_2','poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','IP_Harmonic')
x_vars2 <- c('HBP_PerIP_Current','HBP_PerIP_Prior','HBP_PerIP_Prior_2','Age_Projected',
            'Pos_Group_Current','MLB_Service_Projected','IP_Harmonic')

list_of_errors <- list()
list_of_mapes <- list()
for (i in 1:5)
{
  print(i)
  print('---------------------------------------------------------')
  x <- train_models(historical, y_var, x_vars, x_vars2, model_type = 'gbm', tuneLength = 5, 
                    years_out = i)
  mapes <- do.call(rbind, x[[1]])
  resid <- unlist(x[[4]])
  list_of_errors[[i]] <- resid
  list_of_mapes[[i]] <- mapes
}
# Run final model through current_season and predict future seasons
errors <- list_of_errors
z <- predict_future_years(historical, future, y_var, x_vars, x_vars2, 
                          model_type = 'gbm', tuneLength = 5, years_out = 5, errors, 1)
model <- z[[2]]
#coef(model$finalModel, model$bestTune$lambda)

HBP_PerIP <- z[[1]] %>% 
  select(Season_Projected, Playerid, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(HBP_PerIP = 'Stat_Projected', HBP_PerIP_Upper = 'Stat_Projected_Upper', 
         HBP_PerIP_Lower = 'Stat_Projected_Lower')

future_preds <- future_preds %>% 
  inner_join(HBP_PerIP, by = c('Playerid','Season_Projected')) %>%
  mutate(HBP = HBP_PerIP * IP, 
         HBP_Upper = HBP_PerIP_Upper * IP_Upper,
         HBP_Lower = HBP_PerIP_Lower * IP_Lower) %>%
  mutate(TBF = IP*2.9 + H + BB + HBP,
         TBF_Lower = IP_Lower*2.9 + H_Lower + BB_Lower + HBP_Lower,
         TBF_Upper = IP_Upper*2.9 + H_Upper + BB_Upper + HBP_Upper)
#######################################################################
# Projecting BABIP
pitchers <- add_projection_years(pitchers, 11)
pitchers2 <- add_projected_prior_seasons(pitchers)
pitchers2 <- merge_pitching_stats(pitchers2, c('IP','Pos_Group','BABIP',
                                               'xBABIP','Pull.','Oppo.',
                                               'Cent.','Soft.','Med.','Hard.',
                                               'GB.','LD.','FB.'))
history_future <- historical_future_split(pitchers2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(IP_Harmonic = 2 / ((1 / IP_Current) + (1 / IP_Projected)))

y_var <- c('BABIP_Projected')
x_vars <- c('BABIP_Current','BABIP_Prior','BABIP_Prior_2','poly(Age_Projected, 2)',
            'xBABIP_Current','xBABIP_Prior','xBABIP_Prior_2',
            'Pos_Group_Current','MLB_Service_Projected','LD._Current',
            'LD._Prior','LD._Prior_2','GB._Current','GB._Prior','GB._Prior_2',
            'FB._Current','FB._Prior','FB._Prior_2','Soft._Current','Soft._Prior',
            'Soft._Prior_2','Med._Current','Med._Prior','Med._Prior_2','Hard._Current',
            'Hard._Prior','Hard._Prior_2','Oppo._Current','Oppo._Prior','Oppo._Prior_2',
            'Cent._Current','Cent._Prior','Cent._Prior_2','Pull._Current',
            'Pull._Prior','Pull._Prior_2',
            'IP_Harmonic')
x_vars2 <- c('BABIP_Current','BABIP_Prior','BABIP_Prior_2','Age_Projected',
             'Pos_Group_Current','MLB_Service_Projected','LD._Current',
             'xBABIP_Current','xBABIP_Prior','xBABIP_Prior_2',
             'LD._Prior','LD._Prior_2','GB._Current','GB._Prior','GB._Prior_2',
             'FB._Current','FB._Prior','FB._Prior_2','Soft._Current','Soft._Prior',
             'Soft._Prior_2','Med._Current','Med._Prior','Med._Prior_2','Hard._Current',
             'Hard._Prior','Hard._Prior_2','Oppo._Current','Oppo._Prior','Oppo._Prior_2',
             'Cent._Current','Cent._Prior','Cent._Prior_2','Pull._Current',
             'Pull._Prior','Pull._Prior_2',
             'IP_Harmonic')
list_of_errors <- list()
list_of_mapes <- list()
for (i in 1:5)
{
  print(i)
  print('---------------------------------------------------------')
  x <- train_models(historical, y_var, x_vars, x_vars2, model_type = 'gbm', tuneLength = 5, 
                    years_out = i)
  mapes <- do.call(rbind, x[[1]])
  resid <- unlist(x[[4]])
  list_of_errors[[i]] <- resid
  list_of_mapes[[i]] <- mapes
}
# Run final model through current_season and predict future seasons
errors <- list_of_errors
z <- predict_future_years(historical, future, y_var, x_vars, x_vars2, 
                          model_type = 'gbm', tuneLength = 5, years_out = 5, errors, 1)
model <- z[[2]]
#coef(model$finalModel, model$bestTune$lambda)

BABIP <- z[[1]] %>% 
  select(Season_Projected, Playerid, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(BABIP = 'Stat_Projected', BABIP_Upper = 'Stat_Projected_Upper', 
         BABIP_Lower = 'Stat_Projected_Lower')

future_preds <- future_preds %>% 
  inner_join(BABIP, by = c('Playerid','Season_Projected'))
####################################################################
# Projecting BIP Per IP
pitchers <- add_projection_years(pitchers, 11)
pitchers2 <- add_projected_prior_seasons(pitchers)
pitchers2 <- merge_pitching_stats(pitchers2, c('IP','Pos_Group','BIP_PerIP',
                                               'SwStr.','Swing.','Contact.',
                                               'Zone.','F.Strike.','O.Contact.','Z.Contact.',
                                               'Z.Swing.','O.Swing.','FB.','LD.','GB.','Pull.',
                                               'Oppo.','Cent.','Hard.','Med.','Soft.','K.','BB.'))
history_future <- historical_future_split(pitchers2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(IP_Harmonic = 2 / ((1 / IP_Current) + (1 / IP_Projected)))

y_var <- c('BIP_PerIP_Projected')
x_vars <- c('FB._Current','FB._Prior','FB._Prior_2','GB._Current','GB._Prior',
            'GB._Prior_2','LD._Current','LD._Prior','LD._Prior_2','Hard._Current',
            'Hard._Prior','Hard._Prior_2','O.Contact._Current','O.Contact._Prior',
            'O.Contact._Prior_2','Z.Contact._Current','Z.Contact._Prior',
            'Z.Contact._Prior_2','poly(Age_Projected, 2)','Pos_Group_Current',
            'MLB_Service_Projected','SwStr._Current','SwStr._Prior','SwStr._Prior_2',
            'Swing._Current','Swing._Prior','Swing._Prior_2','Contact._Current','Contact._Prior',
            'Contact._Prior_2','Zone._Current','Zone._Prior','Zone._Prior_2',
            'F.Strike._Current','F.Strike._Prior','F.Strike._Prior_2','K._Current',
            'K._Prior','K._Prior_2','BB._Current','BB._Prior','BB._Prior_2','IP_Harmonic')
x_vars2 <- c('FB._Current','FB._Prior','FB._Prior_2','GB._Current','GB._Prior',
             'GB._Prior_2','LD._Current','LD._Prior','LD._Prior_2','Hard._Current',
             'Hard._Prior','Hard._Prior_2','O.Contact._Current','O.Contact._Prior',
             'O.Contact._Prior_2','Z.Contact._Current','Z.Contact._Prior',
             'Z.Contact._Prior_2','Age_Projected','Pos_Group_Current',
             'MLB_Service_Projected','SwStr._Current','SwStr._Prior','SwStr._Prior_2',
             'Swing._Current','Swing._Prior','Swing._Prior_2','Contact._Current','Contact._Prior',
             'Contact._Prior_2','Zone._Current','Zone._Prior','Zone._Prior_2',
             'F.Strike._Current','F.Strike._Prior','F.Strike._Prior_2','K._Current',
             'K._Prior','K._Prior_2','BB._Current','BB._Prior','BB._Prior_2','IP_Harmonic')

list_of_errors <- list()
list_of_mapes <- list()
for (i in 1:5)
{
  print(i)
  print('---------------------------------------------------------')
  x <- train_models(historical, y_var, x_vars, x_vars2, model_type = 'gbm', tuneLength = 5, 
                    years_out = i)
  mapes <- do.call(rbind, x[[1]])
  resid <- unlist(x[[4]])
  list_of_errors[[i]] <- resid
  list_of_mapes[[i]] <- mapes
}
# Run final model through current_season and predict future seasons
errors <- list_of_errors
z <- predict_future_years(historical, future, y_var, x_vars, x_vars2, 
                          model_type = 'gbm', tuneLength = 5, years_out = 5, errors, 1)
model <- z[[2]]
#coef(model$finalModel, model$bestTune$lambda)

BIP_PerIP <- z[[1]] %>% 
  select(Season_Projected, Playerid, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(BIP_PerIP = 'Stat_Projected', BIP_PerIP_Upper = 'Stat_Projected_Upper', 
         BIP_PerIP_Lower = 'Stat_Projected_Lower')

future_preds <- future_preds %>% 
  inner_join(BIP_PerIP, by = c('Playerid','Season_Projected')) %>%
  mutate(BIP = BIP_PerIP * IP,
         BIP_Lower = BIP_PerIP_Lower * IP_Lower,
         BIP_Upper = BIP_PerIP_Upper * IP_Upper)
####################################################################################
# Projecting K%
pitchers <- add_projection_years(pitchers, 11)
pitchers2 <- add_projected_prior_seasons(pitchers)
pitchers2 <- merge_pitching_stats(pitchers2, c('IP','Pos_Group','K.','GB.',
                                               'FB.','LD.','FBv','FB_Thrown.','HR.FB.1',
                                               'O.Swing.','Z.Swing.','Swing.','Zone.',
                                               'O.Contact.','Z.Contact.','Contact.',
                                               'F.Strike.','SwStr.'))
history_future <- historical_future_split(pitchers2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(IP_Harmonic = 2 / ((1 / IP_Current) + (1 / IP_Projected)))

y_var <- c('K._Projected')
x_vars <- c('K._Current','K._Prior','K._Prior_2','poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','FBv_Current',
            'FBv_Prior','FBv_Prior_2','O.Swing._Current','O.Swing._Prior',
            'O.Swing._Prior_2','Z.Swing._Current','Z.Swing._Prior',
            'Z.Swing._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
            'O.Contact._Current','O.Contact._Prior','O.Contact._Prior_2',
            'Z.Contact._Current','Z.Contact._Prior','Z.Contact._Prior_2',
            'Contact._Current','Contact._Prior','Contact._Prior_2',
            'Zone._Current','Zone._Prior','Zone._Prior_2','F.Strike._Current',
            'F.Strike._Prior','F.Strike._Prior_2','SwStr._Current','SwStr._Prior',
            'SwStr._Prior_2','IP_Harmonic')
x_vars2 <- c('K._Current','K._Prior','K._Prior_2','Age_Projected',
            'Pos_Group_Current','MLB_Service_Projected','FBv_Current',
            'FBv_Prior','FBv_Prior_2','O.Swing._Current','O.Swing._Prior',
            'O.Swing._Prior_2','Z.Swing._Current','Z.Swing._Prior',
            'Z.Swing._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
            'O.Contact._Current','O.Contact._Prior','O.Contact._Prior_2',
            'Z.Contact._Current','Z.Contact._Prior','Z.Contact._Prior_2',
            'Contact._Current','Contact._Prior','Contact._Prior_2',
            'Zone._Current','Zone._Prior','Zone._Prior_2','F.Strike._Current',
            'F.Strike._Prior','F.Strike._Prior_2','SwStr._Current','SwStr._Prior',
            'SwStr._Prior_2','IP_Harmonic')

list_of_errors <- list()
list_of_mapes <- list()
for (i in 1:5)
{
  print(i)
  print('---------------------------------------------------------')
  x <- train_models(historical, y_var, x_vars, x_vars2, model_type = 'gbm', tuneLength = 5, 
                    years_out = i)
  mapes <- do.call(rbind, x[[1]])
  resid <- unlist(x[[4]])
  list_of_errors[[i]] <- resid
  list_of_mapes[[i]] <- mapes
}
# Run final model through current_season and predict future seasons
errors <- list_of_errors
z <- predict_future_years(historical, future, y_var, x_vars, x_vars2, 
                          model_type = 'gbm', tuneLength = 5, years_out = 5, errors, 1)
model <- z[[2]]
#coef(model$finalModel, model$bestTune$lambda)

K. <- z[[1]] %>% 
  select(Season_Projected, Playerid, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(K. = 'Stat_Projected', K._Upper = 'Stat_Projected_Upper', 
         K._Lower = 'Stat_Projected_Lower')

future_preds <- future_preds %>% 
  inner_join(K., by = c('Playerid','Season_Projected')) %>%
  mutate(SO = K. * TBF,
         SO_Lower = K._Lower * TBF_Lower,
         SO_Upper = K._Upper * TBF_Upper,
         K_Per9 = (SO / IP) * 9,
         K_Per9_Lower = (SO_Lower / IP_Lower) * 9,
         K_Per9_Upper = (SO_Upper / IP_Upper) * 9)
#########################################################################
# Projecting BB%
pitchers <- add_projection_years(pitchers, 11)
pitchers2 <- add_projected_prior_seasons(pitchers)
pitchers2 <- merge_pitching_stats(pitchers2, c('IP','Pos_Group','BB.','GB.',
                                               'FB.','LD.','FBv','FB_Thrown.','HR.FB.1',
                                               'O.Swing.','Z.Swing.','Swing.','Zone.',
                                               'O.Contact.','Z.Contact.','Contact.',
                                               'F.Strike.','SwStr.'))
history_future <- historical_future_split(pitchers2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(IP_Harmonic = 2 / ((1 / IP_Current) + (1 / IP_Projected)))

y_var <- c('BB._Projected')
x_vars <- c('BB._Current','BB._Prior','BB._Prior_2','poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','FBv_Current',
            'FBv_Prior','FBv_Prior_2','O.Swing._Current','O.Swing._Prior',
            'O.Swing._Prior_2','Z.Swing._Current','Z.Swing._Prior',
            'Z.Swing._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
            'O.Contact._Current','O.Contact._Prior','O.Contact._Prior_2',
            'Z.Contact._Current','Z.Contact._Prior','Z.Contact._Prior_2',
            'Contact._Current','Contact._Prior','Contact._Prior_2',
            'Zone._Current','Zone._Prior','Zone._Prior_2','F.Strike._Current',
            'F.Strike._Prior','F.Strike._Prior_2','SwStr._Current','SwStr._Prior',
            'SwStr._Prior_2','IP_Harmonic')
x_vars2 <- c('BB._Current','BB._Prior','BB._Prior_2','Age_Projected',
             'Pos_Group_Current','MLB_Service_Projected','FBv_Current',
             'FBv_Prior','FBv_Prior_2','O.Swing._Current','O.Swing._Prior',
             'O.Swing._Prior_2','Z.Swing._Current','Z.Swing._Prior',
             'Z.Swing._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
             'O.Contact._Current','O.Contact._Prior','O.Contact._Prior_2',
             'Z.Contact._Current','Z.Contact._Prior','Z.Contact._Prior_2',
             'Contact._Current','Contact._Prior','Contact._Prior_2',
             'Zone._Current','Zone._Prior','Zone._Prior_2','F.Strike._Current',
             'F.Strike._Prior','F.Strike._Prior_2','SwStr._Current','SwStr._Prior',
             'SwStr._Prior_2','IP_Harmonic')

list_of_errors <- list()
list_of_mapes <- list()
for (i in 1:5)
{
  print(i)
  print('---------------------------------------------------------')
  x <- train_models(historical, y_var, x_vars, x_vars2, model_type = 'gbm', tuneLength = 5, 
                    years_out = i)
  mapes <- do.call(rbind, x[[1]])
  resid <- unlist(x[[4]])
  list_of_errors[[i]] <- resid
  list_of_mapes[[i]] <- mapes
}
# Run final model through current_season and predict future seasons
errors <- list_of_errors
z <- predict_future_years(historical, future, y_var, x_vars, x_vars2, 
                          model_type = 'gbm', tuneLength = 5, years_out = 5, errors, 1)
model <- z[[2]]
#coef(model$finalModel, model$bestTune$lambda)

BB. <- z[[1]] %>% 
  select(Season_Projected, Playerid, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(BB. = 'Stat_Projected', BB._Upper = 'Stat_Projected_Upper', 
         BB._Lower = 'Stat_Projected_Lower')

future_preds <- future_preds %>% 
  inner_join(BB., by = c('Playerid','Season_Projected')) %>%
  mutate(BB = BB. * TBF,
         BB_Lower = BB._Lower * TBF_Lower,
         BB_Upper = BB._Upper * TBF_Upper,
         BB_Per9 = (BB / IP) * 9,
         BB_Per9_Lower = (BB_Lower / IP_Lower) * 9,
         BB_Per9_Upper = (BB_Upper / IP_Upper) * 9)
#########################################################################
# Projecting ERA
pitchers <- add_projection_years(pitchers, 11)
pitchers2 <- add_projected_prior_seasons(pitchers)
pitchers2 <- merge_pitching_stats(pitchers2, c('IP','Pos_Group','ERA','GB.',
                                               'FB.','LD.','FBv','FB_Thrown.','HR.FB.1',
                                               'O.Swing.','Z.Swing.','Swing.','Zone.',
                                               'O.Contact.','Z.Contact.','Contact.',
                                               'F.Strike.','SwStr.','Oppo.','Pull.',
                                               'Soft.','Med.','Hard.','Cent.'))
history_future <- historical_future_split(pitchers2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(IP_Harmonic = 2 / ((1 / IP_Current) + (1 / IP_Projected)))

y_var <- c('ERA_Projected')
x_vars <- c('ERA_Current','ERA_Prior','ERA_Prior_2','poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','FBv_Current',
            'FBv_Prior','FBv_Prior_2','O.Swing._Current','O.Swing._Prior',
            'O.Swing._Prior_2','Z.Swing._Current','Z.Swing._Prior',
            'Z.Swing._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
            'O.Contact._Current','O.Contact._Prior','O.Contact._Prior_2',
            'Z.Contact._Current','Z.Contact._Prior','Z.Contact._Prior_2',
            'Contact._Current','Contact._Prior','Contact._Prior_2',
            'Zone._Current','Zone._Prior','Zone._Prior_2','F.Strike._Current',
            'F.Strike._Prior','F.Strike._Prior_2','SwStr._Current','SwStr._Prior',
            'SwStr._Prior_2','Oppo._Current','Oppo._Prior','Oppo._Prior_2',
            'Pull._Current','Pull._Prior','Pull._Prior_2','Cent._Current',
            'Cent._Prior','Cent._Prior_2','Soft._Current','Soft._Prior',
            'Soft._Prior_2','Med._Current','Med._Prior','Med._Prior_2',
            'Hard._Current','Hard._Prior','Hard._Prior_2',
            'IP_Harmonic')
x_vars2 <- c('ERA_Current','ERA_Prior','ERA_Prior_2','Age_Projected',
             'Pos_Group_Current','MLB_Service_Projected','FBv_Current',
             'FBv_Prior','FBv_Prior_2','O.Swing._Current','O.Swing._Prior',
             'O.Swing._Prior_2','Z.Swing._Current','Z.Swing._Prior',
             'Z.Swing._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
             'O.Contact._Current','O.Contact._Prior','O.Contact._Prior_2',
             'Z.Contact._Current','Z.Contact._Prior','Z.Contact._Prior_2',
             'Contact._Current','Contact._Prior','Contact._Prior_2',
             'Zone._Current','Zone._Prior','Zone._Prior_2','F.Strike._Current',
             'F.Strike._Prior','F.Strike._Prior_2','SwStr._Current','SwStr._Prior',
             'SwStr._Prior_2','Oppo._Current','Oppo._Prior','Oppo._Prior_2',
             'Pull._Current','Pull._Prior','Pull._Prior_2','Cent._Current',
             'Cent._Prior','Cent._Prior_2','Soft._Current','Soft._Prior',
             'Soft._Prior_2','Med._Current','Med._Prior','Med._Prior_2',
             'Hard._Current','Hard._Prior','Hard._Prior_2',
             'IP_Harmonic')

list_of_errors <- list()
list_of_mapes <- list()
for (i in 1:5)
{
  print(i)
  print('---------------------------------------------------------')
  x <- train_models(historical, y_var, x_vars, x_vars2, model_type = 'gbm', tuneLength = 5, 
                    years_out = i)
  mapes <- do.call(rbind, x[[1]])
  resid <- unlist(x[[4]])
  list_of_errors[[i]] <- resid
  list_of_mapes[[i]] <- mapes
}
# Run final model through current_season and predict future seasons
errors <- list_of_errors
z <- predict_future_years(historical, future, y_var, x_vars, x_vars2, 
                          model_type = 'gbm', tuneLength = 5, years_out = 5, errors, 1)
model <- z[[2]]
#coef(model$finalModel, model$bestTune$lambda)

ERA <- z[[1]] %>% 
  select(Season_Projected, Playerid, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(ERA = 'Stat_Projected', ERA_Upper = 'Stat_Projected_Upper', 
         ERA_Lower = 'Stat_Projected_Lower')

future_preds <- future_preds %>% 
  inner_join(ERA, by = c('Playerid','Season_Projected'))
#########################################################################
# Projecting FIP
pitchers <- add_projection_years(pitchers, 11)
pitchers2 <- add_projected_prior_seasons(pitchers)
pitchers2 <- merge_pitching_stats(pitchers2, c('IP','Pos_Group','FIP','GB.',
                                               'FB.','LD.','FBv','FB_Thrown.','HR.FB.1',
                                               'O.Swing.','Z.Swing.','Swing.','Zone.',
                                               'O.Contact.','Z.Contact.','Contact.',
                                               'F.Strike.','SwStr.','Oppo.','Pull.',
                                               'Soft.','Med.','Hard.','Cent.'))
history_future <- historical_future_split(pitchers2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(IP_Harmonic = 2 / ((1 / IP_Current) + (1 / IP_Projected)))

y_var <- c('FIP_Projected')
x_vars <- c('FIP_Current','FIP_Prior','FIP_Prior_2','poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','FBv_Current',
            'FBv_Prior','FBv_Prior_2','O.Swing._Current','O.Swing._Prior',
            'O.Swing._Prior_2','Z.Swing._Current','Z.Swing._Prior',
            'Z.Swing._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
            'O.Contact._Current','O.Contact._Prior','O.Contact._Prior_2',
            'Z.Contact._Current','Z.Contact._Prior','Z.Contact._Prior_2',
            'Contact._Current','Contact._Prior','Contact._Prior_2',
            'Zone._Current','Zone._Prior','Zone._Prior_2','F.Strike._Current',
            'F.Strike._Prior','F.Strike._Prior_2','SwStr._Current','SwStr._Prior',
            'SwStr._Prior_2','Oppo._Current','Oppo._Prior','Oppo._Prior_2',
            'Pull._Current','Pull._Prior','Pull._Prior_2','Cent._Current',
            'Cent._Prior','Cent._Prior_2','Soft._Current','Soft._Prior',
            'Soft._Prior_2','Med._Current','Med._Prior','Med._Prior_2',
            'Hard._Current','Hard._Prior','Hard._Prior_2',
            'IP_Harmonic')
x_vars2 <- c('FIP_Current','FIP_Prior','FIP_Prior_2','Age_Projected',
             'Pos_Group_Current','MLB_Service_Projected','FBv_Current',
             'FBv_Prior','FBv_Prior_2','O.Swing._Current','O.Swing._Prior',
             'O.Swing._Prior_2','Z.Swing._Current','Z.Swing._Prior',
             'Z.Swing._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
             'O.Contact._Current','O.Contact._Prior','O.Contact._Prior_2',
             'Z.Contact._Current','Z.Contact._Prior','Z.Contact._Prior_2',
             'Contact._Current','Contact._Prior','Contact._Prior_2',
             'Zone._Current','Zone._Prior','Zone._Prior_2','F.Strike._Current',
             'F.Strike._Prior','F.Strike._Prior_2','SwStr._Current','SwStr._Prior',
             'SwStr._Prior_2','Oppo._Current','Oppo._Prior','Oppo._Prior_2',
             'Pull._Current','Pull._Prior','Pull._Prior_2','Cent._Current',
             'Cent._Prior','Cent._Prior_2','Soft._Current','Soft._Prior',
             'Soft._Prior_2','Med._Current','Med._Prior','Med._Prior_2',
             'Hard._Current','Hard._Prior','Hard._Prior_2',
             'IP_Harmonic')

list_of_errors <- list()
list_of_mapes <- list()
for (i in 1:5)
{
  print(i)
  print('---------------------------------------------------------')
  x <- train_models(historical, y_var, x_vars, x_vars2, model_type = 'gbm', tuneLength = 5, 
                    years_out = i)
  mapes <- do.call(rbind, x[[1]])
  resid <- unlist(x[[4]])
  list_of_errors[[i]] <- resid
  list_of_mapes[[i]] <- mapes
}
# Run final model through current_season and predict future seasons
errors <- list_of_errors
z <- predict_future_years(historical, future, y_var, x_vars, x_vars2, 
                          model_type = 'gbm', tuneLength = 5, years_out = 5, errors, 1)
model <- z[[2]]
#coef(model$finalModel, model$bestTune$lambda)

FIP <- z[[1]] %>% 
  select(Season_Projected, Playerid, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(FIP = 'Stat_Projected', FIP_Upper = 'Stat_Projected_Upper', 
         FIP_Lower = 'Stat_Projected_Lower')

future_preds <- future_preds %>% 
  inner_join(FIP, by = c('Playerid','Season_Projected'))
#########################################################################
# Projecting xFIP
pitchers <- add_projection_years(pitchers, 11)
pitchers2 <- add_projected_prior_seasons(pitchers)
pitchers2 <- merge_pitching_stats(pitchers2, c('IP','Pos_Group','xFIP','GB.',
                                               'FB.','LD.','FBv','FB_Thrown.','HR.FB.1',
                                               'O.Swing.','Z.Swing.','Swing.','Zone.',
                                               'O.Contact.','Z.Contact.','Contact.',
                                               'F.Strike.','SwStr.','Oppo.','Pull.',
                                               'Soft.','Med.','Hard.','Cent.'))
history_future <- historical_future_split(pitchers2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(IP_Harmonic = 2 / ((1 / IP_Current) + (1 / IP_Projected)))

y_var <- c('xFIP_Projected')
x_vars <- c('xFIP_Current','xFIP_Prior','xFIP_Prior_2','poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','FBv_Current',
            'FBv_Prior','FBv_Prior_2','O.Swing._Current','O.Swing._Prior',
            'O.Swing._Prior_2','Z.Swing._Current','Z.Swing._Prior',
            'Z.Swing._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
            'O.Contact._Current','O.Contact._Prior','O.Contact._Prior_2',
            'Z.Contact._Current','Z.Contact._Prior','Z.Contact._Prior_2',
            'Contact._Current','Contact._Prior','Contact._Prior_2',
            'Zone._Current','Zone._Prior','Zone._Prior_2','F.Strike._Current',
            'F.Strike._Prior','F.Strike._Prior_2','SwStr._Current','SwStr._Prior',
            'SwStr._Prior_2','Oppo._Current','Oppo._Prior','Oppo._Prior_2',
            'Pull._Current','Pull._Prior','Pull._Prior_2','Cent._Current',
            'Cent._Prior','Cent._Prior_2','Soft._Current','Soft._Prior',
            'Soft._Prior_2','Med._Current','Med._Prior','Med._Prior_2',
            'Hard._Current','Hard._Prior','Hard._Prior_2',
            'IP_Harmonic')
x_vars2 <- c('xFIP_Current','xFIP_Prior','xFIP_Prior_2','Age_Projected',
             'Pos_Group_Current','MLB_Service_Projected','FBv_Current',
             'FBv_Prior','FBv_Prior_2','O.Swing._Current','O.Swing._Prior',
             'O.Swing._Prior_2','Z.Swing._Current','Z.Swing._Prior',
             'Z.Swing._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
             'O.Contact._Current','O.Contact._Prior','O.Contact._Prior_2',
             'Z.Contact._Current','Z.Contact._Prior','Z.Contact._Prior_2',
             'Contact._Current','Contact._Prior','Contact._Prior_2',
             'Zone._Current','Zone._Prior','Zone._Prior_2','F.Strike._Current',
             'F.Strike._Prior','F.Strike._Prior_2','SwStr._Current','SwStr._Prior',
             'SwStr._Prior_2','Oppo._Current','Oppo._Prior','Oppo._Prior_2',
             'Pull._Current','Pull._Prior','Pull._Prior_2','Cent._Current',
             'Cent._Prior','Cent._Prior_2','Soft._Current','Soft._Prior',
             'Soft._Prior_2','Med._Current','Med._Prior','Med._Prior_2',
             'Hard._Current','Hard._Prior','Hard._Prior_2',
             'IP_Harmonic')

list_of_errors <- list()
list_of_mapes <- list()
for (i in 1:5)
{
  print(i)
  print('---------------------------------------------------------')
  x <- train_models(historical, y_var, x_vars, x_vars2, model_type = 'gbm', tuneLength = 5, 
                    years_out = i)
  mapes <- do.call(rbind, x[[1]])
  resid <- unlist(x[[4]])
  list_of_errors[[i]] <- resid
  list_of_mapes[[i]] <- mapes
}
# Run final model through current_season and predict future seasons
errors <- list_of_errors
z <- predict_future_years(historical, future, y_var, x_vars, x_vars2, 
                          model_type = 'gbm', tuneLength = 5, years_out = 5, errors, 1)
model <- z[[2]]
#coef(model$finalModel, model$bestTune$lambda)

xFIP <- z[[1]] %>% 
  select(Season_Projected, Playerid, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(xFIP = 'Stat_Projected', xFIP_Upper = 'Stat_Projected_Upper', 
         xFIP_Lower = 'Stat_Projected_Lower')

future_preds <- future_preds %>% 
  inner_join(xFIP, by = c('Playerid','Season_Projected')) %>%
  mutate(WHIP = (BB + H) / IP,
         WHIP_Lower = (BB_Lower + H_Lower) / IP_Lower,
         WHIP_Upper = (BB_Upper + H_Upper) / IP_Upper)

future_preds <- future_preds %>% 
  inner_join(park_factor, by = 'Team') %>%
  mutate(League = as.character(League)) %>%
  group_by(Season_Projected, League) %>%
  mutate(League_ERA = mean(ERA, na.rm = T),
         League_FIP = mean(FIP, na.rm = T),
         League_xFIP = mean(xFIP, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(ERA_minus = (ERA + (ERA - (ERA * ESPN_PF))) / (League_ERA) * 100,
         ERA_minus_Lower = (ERA_Lower + (ERA_Lower - (ERA_Lower * ESPN_PF))) / (League_ERA) * 100,
         ERA_minus_Upper = (ERA_Upper + (ERA_Upper - (ERA_Upper * ESPN_PF))) / (League_ERA) * 100,
         FIP_minus = (FIP + (FIP - (FIP * ESPN_PF))) / (League_FIP) * 100,
         FIP_minus_Lower = (FIP_Lower + (FIP_Lower - (FIP_Lower * ESPN_PF))) / (League_FIP) * 100,
         FIP_minus_Upper = (FIP_Upper + (FIP_Upper - (FIP_Upper * ESPN_PF))) / (League_FIP) * 100,
         xFIP_minus = (xFIP + (xFIP - (xFIP * ESPN_PF))) / (League_xFIP) * 100,
         xFIP_minus_Lower = (xFIP_Lower + (xFIP_Lower - (xFIP_Lower * ESPN_PF))) / (League_xFIP) * 100,
         xFIP_minus_Upper = (xFIP_Upper + (xFIP_Upper - (xFIP_Upper * ESPN_PF))) / (League_xFIP) * 100
         )

