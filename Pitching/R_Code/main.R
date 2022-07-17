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
                            league.name == "National League" ~ "NL",
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
                    'FBv','FBall_pct','O_Swing_pct','Z_Swing_pct','Swing_pct','WAR_200IP',
                    'O_Contact_pct','Z_Contact_pct','Contact_pct','Zone_pct','TBF_G',
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
# TBF
x_vars <- c('TBF_G_Current','TBF_G_Prior','TBF_G_Prior_2',"Age_Projected",
            'MLB_Service_Projected','Pos_Group_Current','Num_Pitches_Harmonic')
future_preds_tbf <- train_models(historical, 'TBF_G_Projected', x_vars, "rqlasso", 3, 1:5)
future_preds <- future_preds %>%
  left_join(future_preds_tbf[[1]] %>% select(Name, Team, Season_Projected, TBF_G_Projected_Lower,
                                           TBF_G_Projected, TBF_G_Projected_Upper), 
            by = c("Name","Team","Season_Projected")) %>% 
  mutate(TBF_Projected_Lower = round(TBF_G_Projected_Lower * G_Projected),
         TBF_Projected = round(TBF_G_Projected * G_Projected),
         TBF_Projected_Upper = round(TBF_G_Projected_Upper * G_Projected)) %>% 
  select(-TBF_G_Projected_Lower, -TBF_G_Projected, -TBF_G_Projected_Upper)

#######################################################################################
# K%
x_vars <- c('K_pct_Current','K_pct_Prior','K_pct_Prior_2','SwStr_pct_Current',
            'Swing_pct_Current','Contact_pct_Current','Zone_pct_Current',
            'O_Contact_pct_Current','Z_Contact_pct_Current','Z_Swing_pct_Current',
            'O_Swing_pct_Current',"Age_Projected",'MLB_Service_Projected',
            'Pos_Group_Current','Num_Pitches_Harmonic')
future_preds_k <- train_models(historical, 'K_pct_Projected', x_vars, "rqlasso", 5, 1:5)
future_preds <- future_preds %>%
  left_join(future_preds_k[[1]] %>% select(Name, Team, Season_Projected, K_pct_Projected_Lower,
                                           K_pct_Projected, K_pct_Projected_Upper), 
            by = c("Name","Team","Season_Projected")) %>% 
  mutate(K_pct_Projected_Lower = round(K_pct_Projected_Lower, 1),
         K_pct_Projected = round(K_pct_Projected, 1),
         K_pct_Projected_Upper = round(K_pct_Projected_Upper, 1),
         SO_Projected_Lower = round(TBF_Projected * (K_pct_Projected_Lower / 100)),
         SO_Projected = round(TBF_Projected * (K_pct_Projected / 100)),
         SO_Projected_Upper = round(TBF_Projected * (K_pct_Projected_Upper / 100)),
         K_9_Projected_Lower = round((SO_Projected_Lower / IP_Projected) * 9, 1),
         K_9_Projected = round((SO_Projected / IP_Projected) * 9, 1),
         K_9_Projected_Upper = round((SO_Projected_Upper / IP_Projected) * 9, 1))
##################################################################################
# BB%
x_vars <- c('BB_pct_Current','BB_pct_Prior','BB_pct_Prior_2','SwStr_pct_Current',
            'Swing_pct_Current','Contact_pct_Current','Zone_pct_Current',
            'O_Contact_pct_Current','Z_Contact_pct_Current','Z_Swing_pct_Current',
            'O_Swing_pct_Current',"Age_Projected",'MLB_Service_Projected',
            'Pos_Group_Current','Num_Pitches_Harmonic')
future_preds_bb <- train_models(historical, 'BB_pct_Projected', x_vars, "rqlasso", 5, 1:5)
future_preds <- future_preds %>%
  left_join(future_preds_bb[[1]] %>% select(Name, Team, Season_Projected, BB_pct_Projected_Lower,
                                            BB_pct_Projected, BB_pct_Projected_Upper), 
            by = c("Name","Team","Season_Projected")) %>% 
  mutate(BB_pct_Projected_Lower = round(BB_pct_Projected_Lower, 1),
         BB_pct_Projected = round(BB_pct_Projected, 1),
         BB_pct_Projected_Upper = round(BB_pct_Projected_Upper, 1),
         BB_Projected_Lower = round(TBF_Projected * (BB_pct_Projected_Lower / 100)),
         BB_Projected = round(TBF_Projected * (BB_pct_Projected / 100)),
         BB_Projected_Upper = round(TBF_Projected * (BB_pct_Projected_Upper / 100)),
         BB_9_Projected_Lower = round((BB_Projected_Lower / IP_Projected) * 9, 1),
         BB_9_Projected = round((BB_Projected / IP_Projected) * 9, 1),
         BB_9_Projected_Upper = round((BB_Projected_Upper / IP_Projected) * 9, 1),
         K_BB_Projected_Lower = round(SO_Projected_Lower / BB_Projected_Lower, 2),
         K_BB_Projected = round(SO_Projected / BB_Projected, 2),
         K_BB_Projected_Upper = round(SO_Projected_Upper / BB_Projected_Upper, 2))
####################################################################################
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
######################################################################################
# BABIP
x_vars <- c('BABIP_Current','BABIP_Prior','BABIP_Prior_2','LD_pct_Current','GB_pct_Current',
            'FB_pct_Current','Strike_pct_Current','FBv_Current','FBall_pct_Current',
            'O_Swing_pct_Current','Z_Swing_pct_Current','Swing_pct_Current',
            'O_Contact_pct_Current','Z_Contact_pct_Current','Contact_pct_Current',
            'Zone_pct_Current','F_Strike_pct_Current','SwStr_pct_Current',"Age_Projected",
            'MLB_Service_Projected','Pos_Group_Current','Num_Pitches_Harmonic')
future_preds_babip <- train_models(historical, 'BABIP_Projected', x_vars, "rqlasso", 3, 1:5)
future_preds <- future_preds %>%
  left_join(future_preds_babip[[1]] %>% select(Name, Team, Season_Projected, BABIP_Projected_Lower,
                                               BABIP_Projected, BABIP_Projected_Upper), 
            by = c("Name","Team","Season_Projected")) %>% 
  mutate(BABIP_Projected_Lower = round(BABIP_Projected_Lower, 3),
         BABIP_Projected = round(BABIP_Projected, 3),
         BABIP_Projected_Upper = round(BABIP_Projected_Upper, 3))
#####################################################################################
# AVG
x_vars <- c('AVG_Current','AVG_Prior','AVG_Prior_2','LD_pct_Current','GB_pct_Current',
            'FB_pct_Current','Strike_pct_Current','FBv_Current','FBall_pct_Current',
            'O_Swing_pct_Current','Z_Swing_pct_Current','Swing_pct_Current',
            'O_Contact_pct_Current','Z_Contact_pct_Current','Contact_pct_Current',
            'Zone_pct_Current','F_Strike_pct_Current','SwStr_pct_Current','HR_FB_Current',
            "Age_Projected",'MLB_Service_Projected','Pos_Group_Current','Num_Pitches_Harmonic')
future_preds_avg <- train_models(historical, 'AVG_Projected', x_vars, "rqlasso", 3, 1:5)
future_preds <- future_preds %>%
  left_join(future_preds_avg[[1]] %>% select(Name, Team, Season_Projected, AVG_Projected_Lower,
                                              AVG_Projected, AVG_Projected_Upper), 
            by = c("Name","Team","Season_Projected")) %>% 
  mutate(AVG_Projected_Lower = round(AVG_Projected_Lower, 3), 
         AVG_Projected = round(AVG_Projected, 3), 
         AVG_Projected_Upper = round(AVG_Projected_Upper, 3), 
         WHIP_Projected_Lower = round((H_Projected_Lower + BB_Projected_Lower) / IP_Projected_Lower, 2),
         WHIP_Projected = round((H_Projected + BB_Projected) / IP_Projected, 2),
         WHIP_Projected_Upper = round((H_Projected_Upper + BB_Projected_Upper) / IP_Projected_Upper, 2))
#####################################################################################
# ERA
x_vars <- c('ERA_Current','ERA_Prior','ERA_Prior_2','LD_pct_Current','GB_pct_Current',
            'FB_pct_Current','Strike_pct_Current','FBv_Current','FBall_pct_Current',
            'O_Swing_pct_Current','Z_Swing_pct_Current','Swing_pct_Current',
            'O_Contact_pct_Current','Z_Contact_pct_Current','Contact_pct_Current',
            'Zone_pct_Current','F_Strike_pct_Current','SwStr_pct_Current','HR_FB_Current',
            "Age_Projected",'MLB_Service_Projected','Pos_Group_Current','Num_Pitches_Harmonic')
future_preds_era <- train_models(historical, 'ERA_Projected', x_vars, "rqlasso", 3, 1:5)
future_preds <- future_preds %>%
  left_join(future_preds_era[[1]] %>% select(Name, Team, Season_Projected, ERA_Projected_Lower,
                                               ERA_Projected, ERA_Projected_Upper), 
            by = c("Name","Team","Season_Projected")) %>% 
  mutate(ERA_Projected_Lower = round(ERA_Projected_Lower, 2),
         ERA_Projected = round(ERA_Projected, 2),
         ERA_Projected_Upper = round(ERA_Projected_Upper, 2))
######################################################################################
# FIP
x_vars <- c('FIP_Current','FIP_Prior','FIP_Prior_2','LD_pct_Current','GB_pct_Current',
            'FB_pct_Current','Strike_pct_Current','FBv_Current','FBall_pct_Current',
            'O_Swing_pct_Current','Z_Swing_pct_Current','Swing_pct_Current',
            'O_Contact_pct_Current','Z_Contact_pct_Current','Contact_pct_Current',
            'Zone_pct_Current','F_Strike_pct_Current','SwStr_pct_Current','HR_FB_Current',
            "Age_Projected",'MLB_Service_Projected','Pos_Group_Current','Num_Pitches_Harmonic')
future_preds_FIP <- train_models(historical, 'FIP_Projected', x_vars, "rqlasso", 3, 1:5)
future_preds <- future_preds %>%
  left_join(future_preds_FIP[[1]] %>% select(Name, Team, Season_Projected, FIP_Projected_Lower,
                                             FIP_Projected, FIP_Projected_Upper), 
            by = c("Name","Team","Season_Projected")) %>% 
  mutate(FIP_Projected_Lower = round(FIP_Projected_Lower, 2),
         FIP_Projected = round(FIP_Projected, 2),
         FIP_Projected_Upper = round(FIP_Projected_Upper, 2))
######################################################################################
# xFIP
x_vars <- c('xFIP_Current','xFIP_Prior','xFIP_Prior_2','LD_pct_Current','GB_pct_Current',
            'FB_pct_Current','Strike_pct_Current','FBv_Current','FBall_pct_Current',
            'O_Swing_pct_Current','Z_Swing_pct_Current','Swing_pct_Current',
            'O_Contact_pct_Current','Z_Contact_pct_Current','Contact_pct_Current',
            'Zone_pct_Current','F_Strike_pct_Current','SwStr_pct_Current','HR_FB_Current',
            "Age_Projected",'MLB_Service_Projected','Pos_Group_Current','Num_Pitches_Harmonic')
future_preds_xFIP <- train_models(historical, 'xFIP_Projected', x_vars, "rqlasso", 3, 1:5)
future_preds <- future_preds %>%
  left_join(future_preds_xFIP[[1]] %>% select(Name, Team, Season_Projected, xFIP_Projected_Lower,
                                             xFIP_Projected, xFIP_Projected_Upper), 
            by = c("Name","Team","Season_Projected")) %>% 
  mutate(xFIP_Projected_Lower = round(xFIP_Projected_Lower, 2),
         xFIP_Projected = round(xFIP_Projected, 2),
         xFIP_Projected_Upper = round(xFIP_Projected_Upper, 2))
#################################################################################
# WAR Per 200 IP
x_vars <- c('WAR_200IP_Current','WAR_200IP_Prior','WAR_200IP_Prior_2','LD_pct_Current','GB_pct_Current',
            'FB_pct_Current','Strike_pct_Current','FBv_Current','FBall_pct_Current',
            'tERA_Current','O_Swing_pct_Current','Z_Swing_pct_Current','Swing_pct_Current',
            'O_Contact_pct_Current','Z_Contact_pct_Current','Contact_pct_Current',
            'Zone_pct_Current','F_Strike_pct_Current','SwStr_pct_Current','HR_FB_Current',
            'K_pct_Current', 'BB_pct_Current','FIP_Current','HR_FB_Current','HR_9_Current',
            "Age_Projected",'MLB_Service_Projected','Pos_Group_Current','Num_Pitches_Harmonic')
future_preds_war <- train_models(historical, 'WAR_200IP_Projected', x_vars, "rqlasso", 3, 1:5)
future_preds <- future_preds %>%
  left_join(future_preds_war[[1]] %>% select(Name, Team, Season_Projected, WAR_200IP_Projected_Lower,
                                             WAR_200IP_Projected, WAR_200IP_Projected_Upper), 
            by = c("Name","Team","Season_Projected")) %>% 
  mutate(WAR_200IP_Projected_Lower = round(WAR_200IP_Projected_Lower, 1),
         WAR_200IP_Projected = round(WAR_200IP_Projected, 1),
         WAR_200IP_Projected_Upper = round(WAR_200IP_Projected_Upper, 1))
#################################################################################
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

