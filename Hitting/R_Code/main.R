setwd("C:/Users/rusla/OneDrive/MLBAnalyticsJobs/Projections/Hitting/R_Code")
#https://www.usatoday.com/sports/mlb/salaries/2008/player/all/
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
  select(teamName, abbreviation) %>% distinct() %>% 
  mutate(teamName = case_when(teamName == "D-backs" ~ "Dbacks",
                              TRUE ~ teamName)) %>% 
  rename("Team" = "abbreviation") %>% 
  add_row(teamName = "Expos", Team = "MON")

#park_factors <- get_park_factors(2002, current_season) %>% 
  #select(Season, Team, home_team, hr)
hitting <- scrape_fangraphs_range(2002, current_season, "hitting")
positions <- scrape_fangraphs_defense(2002, current_season) %>%
  mutate(Inn = as.numeric(Inn)) %>% 
  group_by(Name, Season) %>%
  mutate(Total_Inn = sum(Inn)) %>% 
  arrange(Name, Season, -Inn) %>% 
  filter(row_number()==1) %>% 
  arrange(Name, Season, Pos, Pos_Group) %>% 
  select(Name, Season, Team, Pos, Pos_Group, Total_Inn) %>% 
  rename("Inn" = "Total_Inn") %>% distinct()

hitting_clean <- clean_hitting_data(hitting) %>% 
  inner_join(positions, by = c("Name","Season","Team")) %>%
  filter(Pos_Group != "P") %>% 
  mutate(Pos_Group = as.factor(Pos_Group)) %>% 
  arrange(Name, Season) %>%
  group_by(playerid) %>%
  mutate(Team = na_if(Team, "- - -")) %>%
  tidyr::fill(Team, .direction = 'updown') %>% ungroup()

hitting2 <- add_projection_years(hitting_clean, current_season + 5)
hitting3 <- add_projected_prior_seasons(hitting2)

offensive_stats <- c('G_pct','PA',"PA_G","AB_G",'K_pct','SwStr_pct','Swing_pct','Contact_pct','BB_pct',
                     'PA_HBP','Zone_pct','O_Contact_pct','Z_Contact_pct','Z_Swing_pct','BB_K',
                     'O_Swing_pct','PA_SF','BIP_PA','FB_pct','LD_pct','GB_pct','BABIP','HR_FB',
                     'ISO','AVG','wRC_plus','wOBA','OBP','SLG','OPS','RBI_BIP','R_TOB',
                     'Hard_pct','WAR','WAR_162_G','AB_HR','PA_HR')
hitting_full_data <- merge_hitting_stats(hitting3, offensive_stats)

historical_future <- historical_future_split(hitting_full_data, current_season)
historical <- historical_future[[1]] %>% 
  mutate(PA_Harmonic = 2 / ((1 / PA_Current) + (1 / PA_Projected)))
future <- historical_future[[2]]
######################################################################################
# GP
x_vars <- c('G_pct_Current','G_pct_Prior','G_pct_Prior_2',"Age_Projected",
            'MLB_Service_Projected','Pos_Group_Current','PA_Harmonic')
future_preds_g <- train_models(historical, 'G_pct_Projected', x_vars, "rqlasso", 5, 1:5)
future_preds <- future_preds_g[[1]] %>% select(Name, Team, Age_Projected, Pos_Group_Current,
                                               Season_Projected, G_pct_Projected) %>% 
  mutate(G_Projected = 162 * (G_pct_Projected / 100),
         G_Projected = case_when(G_Projected >= 155 ~ 155, TRUE ~ round(G_Projected))) %>% 
  select(-G_pct_Projected)

######################################################################################
# PA
x_vars <- c('PA_G_Current','PA_G_Prior','PA_G_Prior_2',"Age_Projected",
            'MLB_Service_Projected','Pos_Group_Current','PA_Harmonic')
future_preds_pa <- train_models(historical, 'PA_G_Projected', x_vars, "rqlasso", 5, 1:5)
future_preds <- future_preds %>%
  left_join(future_preds_pa[[1]] %>% select(Name, Team, Season_Projected, 
                                            PA_G_Projected_Lower, PA_G_Projected,
                                            PA_G_Projected_Upper), 
            by = c("Name","Team","Season_Projected")) %>% 
  mutate(PA_Projected_Lower = round(G_Projected * PA_G_Projected_Lower),
         PA_Projected = round(G_Projected * PA_G_Projected),
         PA_Projected_Upper = round(G_Projected * PA_G_Projected_Upper)) %>% 
  select(-PA_G_Projected_Lower, -PA_G_Projected, -PA_G_Projected_Upper)
#####################################################################################
# AB
x_vars <- c('AB_G_Current','AB_G_Prior','AB_G_Prior_2',"Age_Projected",
            'MLB_Service_Projected','Pos_Group_Current','PA_Harmonic')
future_preds_ab <- train_models(historical, 'AB_G_Projected', x_vars, "rqlasso", 5, 1:5)
future_preds <- future_preds %>%
  left_join(future_preds_ab[[1]] %>% select(Name, Team, Season_Projected, 
                                            AB_G_Projected_Lower, AB_G_Projected,
                                            AB_G_Projected_Upper), 
            by = c("Name","Team","Season_Projected")) %>% 
  mutate(AB_Projected_Lower = round(G_Projected * AB_G_Projected_Lower),
         AB_Projected = round(G_Projected * AB_G_Projected),
         AB_Projected_Upper = round(G_Projected * AB_G_Projected_Upper)) %>% 
  select(-AB_G_Projected_Lower, -AB_G_Projected, -AB_G_Projected_Upper)

#####################################################################################
#K%
x_vars <- c('K_pct_Current','K_pct_Prior','K_pct_Prior_2','SwStr_pct_Current',
            'SwStr_pct_Prior','SwStr_pct_Prior_2','Swing_pct_Current','Swing_pct_Prior',
            'Swing_pct_Prior_2','Contact_pct_Current','Contact_pct_Prior',
            'Contact_pct_Prior_2','Zone_pct_Current','Zone_pct_Prior','Zone_pct_Prior_2',
            'O_Contact_pct_Current','Z_Contact_pct_Current','Z_Swing_pct_Current',
            'O_Swing_pct_Current','O_Contact_pct_Prior','Z_Contact_pct_Prior',
            'Z_Swing_pct_Prior','O_Swing_pct_Prior','O_Contact_pct_Prior_2',
            'Z_Contact_pct_Prior_2','Z_Swing_pct_Prior_2','O_Swing_pct_Prior_2',
            "Age_Projected",'MLB_Service_Projected','Pos_Group_Current','PA_Harmonic')
future_preds_k <- train_models(historical, 'K_pct_Projected', x_vars, "rqlasso", 5, 1:5)
future_preds <- future_preds %>%
  left_join(future_preds_k[[1]] %>% select(Name, Team, Season_Projected, K_pct_Projected_Lower,
                                           K_pct_Projected, K_pct_Projected_Upper), 
            by = c("Name","Team","Season_Projected")) %>% 
  mutate(K_pct_Projected_Lower = round(K_pct_Projected_Lower, 1),
         K_pct_Projected = round(K_pct_Projected, 1),
         K_pct_Projected_Upper = round(K_pct_Projected_Upper, 1))
#####################################################################################
#BB%
x_vars <- c('BB_pct_Current','BB_pct_Prior','BB_pct_Prior_2','SwStr_pct_Current',
            'SwStr_pct_Prior','SwStr_pct_Prior_2','Swing_pct_Current','Swing_pct_Prior',
            'Swing_pct_Prior_2','Contact_pct_Current','Contact_pct_Prior',
            'Contact_pct_Prior_2','Zone_pct_Current','Zone_pct_Prior','Zone_pct_Prior_2',
            'O_Contact_pct_Current','Z_Contact_pct_Current','Z_Swing_pct_Current',
            'O_Swing_pct_Current','O_Contact_pct_Prior','Z_Contact_pct_Prior',
            'Z_Swing_pct_Prior','O_Swing_pct_Prior','O_Contact_pct_Prior_2',
            'Z_Contact_pct_Prior_2','Z_Swing_pct_Prior_2','O_Swing_pct_Prior_2',
            "Age_Projected",'MLB_Service_Projected','Pos_Group_Current','PA_Harmonic')
future_preds_bb <- train_models(historical, 'BB_pct_Projected', x_vars, "rqlasso", 5, 1:5)
future_preds <- future_preds %>%
  left_join(future_preds_bb[[1]] %>% select(Name, Team, Season_Projected, BB_pct_Projected_Lower,
                                            BB_pct_Projected, BB_pct_Projected_Upper), 
            by = c("Name","Team","Season_Projected")) %>% 
  mutate(BB_pct_Projected_Lower = round(BB_pct_Projected_Lower, 1),
         BB_pct_Projected = round(BB_pct_Projected, 1),
         BB_pct_Projected_Upper = round(BB_pct_Projected_Upper, 1))
###################################################################################
# AVG 
x_vars <- c('AVG_Current','AVG_Prior','AVG_Prior_2','FB_pct_Current',
            'FB_pct_Prior','FB_pct_Prior_2','GB_pct_Current','GB_pct_Prior',
            'GB_pct_Prior_2','LD_pct_Current','LD_pct_Prior','LD_pct_Prior_2',
            'Hard_pct_Current','Hard_pct_Prior','Hard_pct_Prior_2','SwStr_pct_Current',
            'SwStr_pct_Prior','SwStr_pct_Prior_2','Swing_pct_Current','Swing_pct_Prior',
            'Swing_pct_Prior_2','Contact_pct_Current','Contact_pct_Prior',
            'Contact_pct_Prior_2','Zone_pct_Current','Zone_pct_Prior','Zone_pct_Prior_2',
            'O_Contact_pct_Current','Z_Contact_pct_Current','Z_Swing_pct_Current',
            'O_Swing_pct_Current','O_Contact_pct_Prior','Z_Contact_pct_Prior',
            'Z_Swing_pct_Prior','O_Swing_pct_Prior','O_Contact_pct_Prior_2',
            'Z_Contact_pct_Prior_2','Z_Swing_pct_Prior_2','O_Swing_pct_Prior_2',
            "Age_Projected",'MLB_Service_Projected','Pos_Group_Current','PA_Harmonic')
future_preds_avg <- train_models(historical, 'AVG_Projected', x_vars, "rqlasso", 5, 1:5)
future_preds <- future_preds %>%
  left_join(future_preds_avg[[1]] %>% select(Name, Team, Season_Projected, AVG_Projected_Lower,
                                             AVG_Projected, AVG_Projected_Upper), 
            by = c("Name","Team","Season_Projected")) %>% 
  mutate(AVG_Projected_Lower = round(AVG_Projected_Lower, 3),
         AVG_Projected = round(AVG_Projected, 3),
         AVG_Projected_Upper = round(AVG_Projected_Upper, 3))
###################################################################################
# OBP
x_vars <- c('OBP_Current','OBP_Prior','OBP_Prior_2','FB_pct_Current',
            'FB_pct_Prior','FB_pct_Prior_2','GB_pct_Current','GB_pct_Prior',
            'GB_pct_Prior_2','LD_pct_Current','LD_pct_Prior','LD_pct_Prior_2',
            'Hard_pct_Current','Hard_pct_Prior','Hard_pct_Prior_2','SwStr_pct_Current',
            'SwStr_pct_Prior','SwStr_pct_Prior_2','Swing_pct_Current','Swing_pct_Prior',
            'Swing_pct_Prior_2','Contact_pct_Current','Contact_pct_Prior',
            'Contact_pct_Prior_2','Zone_pct_Current','Zone_pct_Prior','Zone_pct_Prior_2',
            'O_Contact_pct_Current','Z_Contact_pct_Current','Z_Swing_pct_Current',
            'O_Swing_pct_Current','O_Contact_pct_Prior','Z_Contact_pct_Prior',
            'Z_Swing_pct_Prior','O_Swing_pct_Prior','O_Contact_pct_Prior_2',
            'Z_Contact_pct_Prior_2','Z_Swing_pct_Prior_2','O_Swing_pct_Prior_2',
            "Age_Projected",'MLB_Service_Projected','Pos_Group_Current','PA_Harmonic')
future_preds_obp <- train_models(historical, 'OBP_Projected', x_vars, "rqlasso", 5, 1:5)
future_preds <- future_preds %>%
  left_join(future_preds_obp[[1]] %>% select(Name, Team, Season_Projected, OBP_Projected_Lower,
                                             OBP_Projected, OBP_Projected_Upper), 
            by = c("Name","Team","Season_Projected")) %>% 
  mutate(OBP_Projected_Lower = round(OBP_Projected_Lower, 3),
         OBP_Projected = round(OBP_Projected, 3),
         OBP_Projected_Upper = round(OBP_Projected_Upper, 3))
###################################################################################
# SLG
x_vars <- c('SLG_Current','SLG_Prior','SLG_Prior_2','FB_pct_Current',
            'FB_pct_Prior','FB_pct_Prior_2','GB_pct_Current','GB_pct_Prior',
            'GB_pct_Prior_2','LD_pct_Current','LD_pct_Prior','LD_pct_Prior_2',
            'Hard_pct_Current','Hard_pct_Prior','Hard_pct_Prior_2','SwStr_pct_Current',
            'SwStr_pct_Prior','SwStr_pct_Prior_2','Swing_pct_Current','Swing_pct_Prior',
            'Swing_pct_Prior_2','Contact_pct_Current','Contact_pct_Prior',
            'Contact_pct_Prior_2','Zone_pct_Current','Zone_pct_Prior','Zone_pct_Prior_2',
            'O_Contact_pct_Current','Z_Contact_pct_Current','Z_Swing_pct_Current',
            'O_Swing_pct_Current','O_Contact_pct_Prior','Z_Contact_pct_Prior',
            'Z_Swing_pct_Prior','O_Swing_pct_Prior','O_Contact_pct_Prior_2',
            'Z_Contact_pct_Prior_2','Z_Swing_pct_Prior_2','O_Swing_pct_Prior_2',
            "Age_Projected",'MLB_Service_Projected','Pos_Group_Current','PA_Harmonic')
future_preds_slg <- train_models(historical, 'SLG_Projected', x_vars, "rqlasso", 5, 1:5)
future_preds <- future_preds %>%
  left_join(future_preds_slg[[1]] %>% select(Name, Team, Season_Projected, SLG_Projected_Lower,
                                             SLG_Projected, SLG_Projected_Upper), 
            by = c("Name","Team","Season_Projected")) %>% 
  mutate(SLG_Projected_Lower = round(SLG_Projected_Lower, 3),
         SLG_Projected = round(SLG_Projected, 3),
         SLG_Projected_Upper = round(SLG_Projected_Upper, 3))
###################################################################################
# wOBA
x_vars <- c('wOBA_Current','wOBA_Prior','wOBA_Prior_2','FB_pct_Current',
            'FB_pct_Prior','FB_pct_Prior_2','GB_pct_Current','GB_pct_Prior',
            'GB_pct_Prior_2','LD_pct_Current','LD_pct_Prior','LD_pct_Prior_2',
            'Hard_pct_Current','Hard_pct_Prior','Hard_pct_Prior_2','SwStr_pct_Current',
            'SwStr_pct_Prior','SwStr_pct_Prior_2','Swing_pct_Current','Swing_pct_Prior',
            'Swing_pct_Prior_2','Contact_pct_Current','Contact_pct_Prior',
            'Contact_pct_Prior_2','Zone_pct_Current','Zone_pct_Prior','Zone_pct_Prior_2',
            'O_Contact_pct_Current','Z_Contact_pct_Current','Z_Swing_pct_Current',
            'O_Swing_pct_Current','O_Contact_pct_Prior','Z_Contact_pct_Prior',
            'Z_Swing_pct_Prior','O_Swing_pct_Prior','O_Contact_pct_Prior_2',
            'Z_Contact_pct_Prior_2','Z_Swing_pct_Prior_2','O_Swing_pct_Prior_2',
            "Age_Projected",'MLB_Service_Projected','Pos_Group_Current','PA_Harmonic')
future_preds_woba <- train_models(historical, 'wOBA_Projected', x_vars, "rqlasso", 5, 1:5)
future_preds <- future_preds %>%
  left_join(future_preds_woba[[1]] %>% select(Name, Team, Season_Projected, wOBA_Projected_Lower,
                                              wOBA_Projected, wOBA_Projected_Upper), 
            by = c("Name","Team","Season_Projected")) %>% 
  mutate(wOBA_Projected_Lower = round(wOBA_Projected_Lower, 3),
         wOBA_Projected = round(wOBA_Projected, 3),
         wOBA_Projected_Upper = round(wOBA_Projected_Upper, 3))
###################################################################################
# BABIP
x_vars <- c('BABIP_Current','BABIP_Prior','BABIP_Prior_2','FB_pct_Current',
            'FB_pct_Prior','FB_pct_Prior_2','GB_pct_Current','GB_pct_Prior',
            'GB_pct_Prior_2','LD_pct_Current','LD_pct_Prior','LD_pct_Prior_2',
            'Hard_pct_Current','Hard_pct_Prior','Hard_pct_Prior_2','SwStr_pct_Current',
            'SwStr_pct_Prior','SwStr_pct_Prior_2','Swing_pct_Current','Swing_pct_Prior',
            'Swing_pct_Prior_2','Contact_pct_Current','Contact_pct_Prior',
            'Contact_pct_Prior_2','Zone_pct_Current','Zone_pct_Prior','Zone_pct_Prior_2',
            'O_Contact_pct_Current','Z_Contact_pct_Current','Z_Swing_pct_Current',
            'O_Swing_pct_Current','O_Contact_pct_Prior','Z_Contact_pct_Prior',
            'Z_Swing_pct_Prior','O_Swing_pct_Prior','O_Contact_pct_Prior_2',
            'Z_Contact_pct_Prior_2','Z_Swing_pct_Prior_2','O_Swing_pct_Prior_2',
            "Age_Projected",'MLB_Service_Projected','Pos_Group_Current','PA_Harmonic')
future_preds_babip <- train_models(historical, 'BABIP_Projected', x_vars, "rqlasso", 5, 1:5)
future_preds <- future_preds %>%
  left_join(future_preds_babip[[1]] %>% select(Name, Team, Season_Projected, BABIP_Projected_Lower,
                                               BABIP_Projected, BABIP_Projected_Upper), 
            by = c("Name","Team","Season_Projected")) %>% 
  mutate(BABIP_Projected_Lower = round(BABIP_Projected_Lower, 3),
         BABIP_Projected = round(BABIP_Projected, 3),
         BABIP_Projected_Upper = round(BABIP_Projected_Upper, 3))
###########################################################################################
# wRC+
x_vars <- c('wRC_plus_Current','wRC_plus_Prior','wRC_plus_Prior_2','FB_pct_Current',
            'FB_pct_Prior','FB_pct_Prior_2','GB_pct_Current','GB_pct_Prior',
            'GB_pct_Prior_2','LD_pct_Current','LD_pct_Prior','LD_pct_Prior_2',
            'Hard_pct_Current','Hard_pct_Prior','Hard_pct_Prior_2','SwStr_pct_Current',
            'SwStr_pct_Prior','SwStr_pct_Prior_2','Swing_pct_Current','Swing_pct_Prior',
            'Swing_pct_Prior_2','Contact_pct_Current','Contact_pct_Prior',
            'Contact_pct_Prior_2','Zone_pct_Current','Zone_pct_Prior','Zone_pct_Prior_2',
            'O_Contact_pct_Current','Z_Contact_pct_Current','Z_Swing_pct_Current',
            'O_Swing_pct_Current','O_Contact_pct_Prior','Z_Contact_pct_Prior',
            'Z_Swing_pct_Prior','O_Swing_pct_Prior','O_Contact_pct_Prior_2',
            'Z_Contact_pct_Prior_2','Z_Swing_pct_Prior_2','O_Swing_pct_Prior_2',
            "Age_Projected",'MLB_Service_Projected', 'Pos_Group_Current','PA_Harmonic')
future_preds_wrc <- train_models(historical, 'wRC_plus_Projected', x_vars, "rqlasso", 5, 1:5)
future_preds <- future_preds %>%
  left_join(future_preds_wrc[[1]] %>% select(Name, Team, Season_Projected, wRC_plus_Projected_Lower,
                                             wRC_plus_Projected, wRC_plus_Projected_Upper), 
            by = c("Name","Team","Season_Projected")) %>% 
  mutate(wRC_plus_Projected_Lower = round(wRC_plus_Projected_Lower),
         wRC_plus_Projected = round(wRC_plus_Projected),
         wRC_plus_Projected_Upper = round(wRC_plus_Projected_Upper))
###################################################################################
# WAR 
x_vars <- c('WAR_162_G_Current','WAR_162_G_Prior','WAR_162_G_Prior_2',
            "Age_Projected",'MLB_Service_Projected', 'Pos_Group_Current','PA_Harmonic')
future_preds_war <- train_models(historical, 'WAR_162_G_Projected', x_vars, "rqlasso", 5, 1:5)
future_preds <- future_preds %>%
  left_join(future_preds_war[[1]] %>% select(Name, Team, Season_Projected, 
                                             WAR_162_G_Projected_Lower, WAR_162_G_Projected,
                                             WAR_162_G_Projected_Upper), 
            by = c("Name","Team","Season_Projected")) %>% 
  mutate(WAR_162_G_Projected_Lower = round(WAR_162_G_Projected_Lower, 1),
         WAR_162_G_Projected = round(WAR_162_G_Projected, 1),
         WAR_162_G_Projected_Upper = round(WAR_162_G_Projected_Upper, 1))
###################################################################################
future_preds <- future_preds %>% 
  mutate(SO_Projected_Lower = round(PA_Projected_Lower * (K_pct_Projected_Lower / 100)),
         SO_Projected = round(PA_Projected * (K_pct_Projected / 100)),
         SO_Projected_Upper = round(PA_Projected_Upper * (K_pct_Projected_Upper / 100)),
         BB_Projected_Lower = round(PA_Projected_Lower * (BB_pct_Projected_Lower / 100)),
         BB_Projected = round(PA_Projected * (BB_pct_Projected / 100)),
         BB_Projected_Upper = round(PA_Projected_Upper * (BB_pct_Projected_Upper / 100)),
         BB_K_Projected_Lower = round(BB_Projected_Lower / SO_Projected_Lower, 2),
         BB_K_Projected = round(BB_Projected / SO_Projected, 2),
         BB_K_Projected_Upper = round(BB_Projected_Upper / SO_Projected_Upper, 2),
         H_Projected_Lower = round(AB_Projected_Lower * AVG_Projected_Lower),
         H_Projected = round(AB_Projected * AVG_Projected),
         H_Projected_Upper = round(AB_Projected_Upper * AVG_Projected_Upper),
         OPS_Projected_Lower = OBP_Projected_Lower + SLG_Projected_Lower,
         OPS_Projected = OBP_Projected + SLG_Projected,
         OPS_Projected_Upper = OBP_Projected_Upper + SLG_Projected_Upper,
         ISO_Projected_Lower = SLG_Projected_Lower - AVG_Projected_Lower,
         ISO_Projected = SLG_Projected - AVG_Projected,
         ISO_Projected_Upper = SLG_Projected_Upper - AVG_Projected_Upper)


setwd("C:/Users/rusla/OneDrive/MLBAnalyticsJobs/Projections/Hitting/Data")
write.csv(future_preds, "hitting_projections.csv")
hitters <- unique(future_preds$Name)
seasons <- unique(future_preds$Season_Projected)
quantiles <- c("5th","50th","95th")

#######################################################################################
#park_factors <- get_park_factors(2002, current_season) %>% 
#select(Season, Team, home_team, hr)
#pitching <- scrape_fangraphs_range(2002, current_season, "pitching")
#pitching_clean <- clean_pitching_data(pitching) %>% 
#inner_join(positions, by = c("Name","Season","Team")) %>% 
#arrange(Name, Season) %>%
#group_by(playerid) %>%
#mutate(Team = na_if(Team, "- - -")) %>%
#tidyr::fill(Team, .direction = 'updown') %>% ungroup()

#pitching2 <- add_projection_years(pitching_clean, 2026)
#pitching3 <- add_projected_prior_seasons(pitching2)
#######################################################################################

# Projecting HBP
hitters <- add_projection_years(hitters, 11)
hitters2 <- add_projected_prior_seasons(hitters)
hitters2 <- merge_hitting_stats(hitters2, c('PA','PA_PerHBP'))
history_future <- historical_future_split(hitters2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(PA_Harmonic = 2 / ((1 / PA_Current) + (1 / PA_Projected)))

y_var <- c('PA_PerHBP_Projected')
x_vars <- c('PA_PerHBP_Current','PA_PerHBP_Prior','PA_PerHBP_Prior_2',
            'MLB_Service_Projected','Pos_Group_Current','poly(Age_Projected,2)','PA_Projected')
x_vars2 <- c('PA_PerHBP_Current','PA_PerHBP_Prior','PA_PerHBP_Prior_2',
             'MLB_Service_Projected','Pos_Group_Current','Age_Projected','PA_Projected')

list_of_errors <- list()
list_of_mapes <- list()
for (i in 1:6)
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
                          model_type = 'gbm', tuneLength = 5, years_out = 6, errors, 1)
model <- z[[2]]
#coef(model$finalModel, model$bestTune$lambda)

HBP <- z[[1]] %>% 
  select(Season_Projected, Playerid, Stat_Projected) %>%
  rename(PA_PerHBP = 'Stat_Projected')

future_preds <- future_preds %>% 
  inner_join(HBP, by = c('Playerid','Season_Projected')) %>%
  mutate(HBP = PA / PA_PerHBP)
#######################################################################################
# Projecting SF
hitters <- add_projection_years(hitters, 11)
hitters2 <- add_projected_prior_seasons(hitters)
hitters2 <- merge_hitting_stats(hitters2, c('PA','PA_PerSF'))
history_future <- historical_future_split(hitters2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(PA_Harmonic = 2 / ((1 / PA_Current) + (1 / PA_Projected)))

y_var <- c('PA_PerSF_Projected')
x_vars <- c('PA_PerSF_Current','PA_PerSF_Prior','PA_PerSF_Prior_2',
            'MLB_Service_Projected','Pos_Group_Current','poly(Age_Projected,2)','PA_Projected')
x_vars2 <- c('PA_PerSF_Current','PA_PerSF_Prior','PA_PerSF_Prior_2',
             'MLB_Service_Projected','Pos_Group_Current','Age_Projected','PA_Projected')

list_of_errors <- list()
list_of_mapes <- list()
for (i in 1:6)
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
                          model_type = 'gbm', tuneLength = 5, years_out = 6, errors, 1)
model <- z[[2]]
#coef(model$finalModel, model$bestTune$lambda)

SF <- z[[1]] %>% 
  select(Season_Projected, Playerid, Stat_Projected) %>%
  rename(PA_PerSF = 'Stat_Projected')

future_preds <- future_preds %>% 
  inner_join(SF, by = c('Playerid','Season_Projected')) %>%
  mutate(SF = PA / PA_PerSF)
future_preds <- future_preds %>% 
  mutate(AB = PA - BB - HBP - SF,
         AB_Lower = PA_Lower - BB_Lower - HBP - SF,
         AB_Upper = PA_Upper - BB_Upper - HBP - SF)
#######################################################################################
# Projecting BIP AVG
hitters <- add_projection_years(hitters, 11)
hitters2 <- add_projected_prior_seasons(hitters)
hitters2 <- merge_hitting_stats(hitters2, c('PA','BIP_AVG','SwStr.','Swing.','Contact.',
                                            'Zone.','F.Strike.','O.Contact.','Z.Contact.',
                                            'Z.Swing.','O.Swing.','FB.','LD.','GB.','Pull.',
                                            'Oppo.','Cent.','Hard.','Med.','Soft.','K.','BB.'))
history_future <- historical_future_split(hitters2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(PA_Harmonic = 2 / ((1 / PA_Current) + (1 / PA_Projected)))

y_var <- c('BIP_AVG_Projected')
x_vars <- c('FB._Current','FB._Prior','FB._Prior_2','GB._Current','GB._Prior',
            'GB._Prior_2','LD._Current','LD._Prior','LD._Prior_2','Hard._Current',
            'Hard._Prior','Hard._Prior_2','O.Contact._Current','O.Contact._Prior',
            'O.Contact._Prior_2','Z.Contact._Current','Z.Contact._Prior',
            'Z.Contact._Prior_2','poly(Age_Projected, 2)','Pos_Group_Current',
            'MLB_Service_Projected','SwStr._Current','SwStr._Prior','SwStr._Prior_2',
            'Swing._Current','Swing._Prior','Swing._Prior_2','Contact._Current','Contact._Prior',
            'Contact._Prior_2','Zone._Current','Zone._Prior','Zone._Prior_2',
            'F.Strike._Current','F.Strike._Prior','F.Strike._Prior_2','K._Current',
            'K._Prior','K._Prior_2','BB._Current','BB._Prior','BB._Prior_2','PA_Projected')
x_vars2 <- c('FB._Current','FB._Prior','FB._Prior_2','GB._Current','GB._Prior',
            'GB._Prior_2','LD._Current','LD._Prior','LD._Prior_2','Hard._Current',
            'Hard._Prior','Hard._Prior_2','O.Contact._Current','O.Contact._Prior',
            'O.Contact._Prior_2','Z.Contact._Current','Z.Contact._Prior',
            'Z.Contact._Prior_2','Age_Projected','Pos_Group_Current',
            'MLB_Service_Projected','SwStr._Current','SwStr._Prior','SwStr._Prior_2',
            'Swing._Current','Swing._Prior','Swing._Prior_2','Contact._Current','Contact._Prior',
            'Contact._Prior_2','Zone._Current','Zone._Prior','Zone._Prior_2',
            'F.Strike._Current','F.Strike._Prior','F.Strike._Prior_2','K._Current',
            'K._Prior','K._Prior_2','BB._Current','BB._Prior','BB._Prior_2','PA_Projected')

list_of_errors <- list()
list_of_mapes <- list()
for (i in 1:6)
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
                          model_type = 'gbm', tuneLength = 5, years_out = 6, errors, 1)
model <- z[[2]]
#coef(model$finalModel, model$bestTune$lambda)

BIP_AVG <- z[[1]] %>% 
  select(Season_Projected, Playerid, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(BIP_AVG = 'Stat_Projected', BIP_AVG_Upper = 'Stat_Projected_Upper', 
         BIP_AVG_Lower = 'Stat_Projected_Lower')

future_preds <- future_preds %>% 
  inner_join(BIP_AVG, by = c('Playerid','Season_Projected')) %>%
  mutate(BIP = PA * BIP_AVG, 
         BIP_Upper = PA_Upper * BIP_AVG_Upper,
         BIP_Lower = PA_Lower * BIP_AVG_Lower)
#######################################################################################
# Projecting FB%
hitters <- add_projection_years(hitters, 11)
hitters2 <- add_projected_prior_seasons(hitters)
hitters2 <- merge_hitting_stats(hitters2, c('PA','SwStr.','Swing.','Contact.',
                                            'Zone.','F.Strike.','O.Contact.','Z.Contact.',
                                            'Z.Swing.','O.Swing.','FB.','LD.','GB.','Pull.',
                                            'Oppo.','Cent.','Hard.','Med.','Soft.','Spd'))
history_future <- historical_future_split(hitters2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(PA_Harmonic = 2 / ((1 / PA_Current) + (1 / PA_Projected)))

y_var <- c('FB._Projected')
x_vars <- c('FB._Current','FB._Prior','FB._Prior_2',
            'GB._Current','GB._Prior','GB._Prior_2','LD._Current','LD._Prior','LD._Prior_2',
            'Hard._Current','Hard._Prior','Hard._Prior_2','Spd_Current','Spd_Prior','Spd_Prior_2',
            'poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','SwStr._Current','SwStr._Prior',
            'SwStr._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
            'Contact._Current','Contact._Prior','Contact._Prior_2','Zone._Current',
            'Zone._Prior','Zone._Prior_2','F.Strike._Current','F.Strike._Prior',
            'F.Strike._Prior_2','PA_Projected')
x_vars2 <- c('FB._Current','FB._Prior','FB._Prior_2',
             'GB._Current','GB._Prior','GB._Prior_2','LD._Current','LD._Prior','LD._Prior_2',
             'Hard._Current','Hard._Prior','Hard._Prior_2','Spd_Current','Spd_Prior','Spd_Prior_2',
             'Age_Projected',
             'Pos_Group_Current','MLB_Service_Projected','SwStr._Current','SwStr._Prior',
             'SwStr._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
             'Contact._Current','Contact._Prior','Contact._Prior_2','Zone._Current',
             'Zone._Prior','Zone._Prior_2','F.Strike._Current','F.Strike._Prior',
             'F.Strike._Prior_2','PA_Projected')

list_of_errors <- list()
list_of_mapes <- list()
for (i in 1:6)
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
                          model_type = 'gbm', tuneLength = 5, years_out = 6, errors, 1)
model <- z[[2]]
#coef(model$finalModel, model$bestTune$lambda)

FB. <- z[[1]] %>% 
  select(Season_Projected, Playerid, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(FB. = 'Stat_Projected', FB._Upper = 'Stat_Projected_Upper', 
         FB._Lower = 'Stat_Projected_Lower')

future_preds <- future_preds %>% 
  inner_join(FB., by = c('Playerid','Season_Projected')) %>%
  mutate(Num_FB = FB. * BIP,
         Num_FB_Upper = FB._Upper * BIP_Upper,
         Num_FB_Lower = FB._Lower * BIP_Lower)
#######################################################################################
# Projecting HR/FB
hitters <- add_projection_years(hitters, 11)
hitters2 <- add_projected_prior_seasons(hitters)
hitters2 <- merge_hitting_stats(hitters2, c('PA','HR.FB','Hard.','ISO','Pull.',
                                            'Cent.','Oppo.','LD.','FB.'))
history_future <- historical_future_split(hitters2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(PA_Harmonic = 2 / ((1 / PA_Current) + (1 / PA_Projected)))

y_var <- c('HR.FB_Projected')
x_vars <- c('HR.FB_Current','HR.FB_Prior','HR.FB_Prior_2','poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','ISO_Current',
            'ISO_Prior','ISO_Prior_2','Pull._Current','Pull._Prior','Pull._Prior_2',
            'LD._Current','LD._Prior','LD._Prior_2','FB._Current','FB._Prior','FB._Prior_2',
            'PA_Projected')
x_vars2 <- c('HR.FB_Current','HR.FB_Prior','HR.FB_Prior_2','Age_Projected',
            'Pos_Group_Current','MLB_Service_Projected','ISO_Current',
            'ISO_Prior','ISO_Prior_2','Pull._Current','Pull._Prior','Pull._Prior_2',
            'LD._Current','LD._Prior','LD._Prior_2','FB._Current','FB._Prior','FB._Prior_2',
            'PA_Projected')


list_of_errors <- list()
list_of_mapes <- list()
for (i in 1:6)
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
                          model_type = 'gbm', tuneLength = 5, years_out = 6, errors, 1)
model <- z[[2]]
#coef(model$finalModel, model$bestTune$lambda)

HR.FB <- z[[1]] %>% 
  select(Season_Projected, Playerid, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(HR.FB = 'Stat_Projected', HR.FB_Upper = 'Stat_Projected_Upper', 
         HR.FB_Lower = 'Stat_Projected_Lower')

future_preds <- future_preds %>% 
  inner_join(HR.FB, by = c('Playerid','Season_Projected')) %>%
  mutate(HR = Num_FB * HR.FB,
         HR_Upper = Num_FB_Upper * HR.FB_Upper,
         HR_Lower = Num_FB_Lower * HR.FB_Lower)
#######################################################################################
# Projecting RBI Per BIP
hitters <- add_projection_years(hitters, 11)
hitters2 <- add_projected_prior_seasons(hitters)
hitters2 <- merge_hitting_stats(hitters2, c('PA','RBI_Per_BIP','SwStr.','Swing.','Contact.',
                                            'Zone.','F.Strike.','O.Contact.','Z.Contact.',
                                            'Z.Swing.','O.Swing.','FB.','LD.','GB.','Pull.',
                                            'Oppo.','Cent.','Hard.','Med.','Soft.','Spd'))
history_future <- historical_future_split(hitters2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(PA_Harmonic = 2 / ((1 / PA_Current) + (1 / PA_Projected)))

y_var <- c('RBI_Per_BIP_Projected')
x_vars <- c('RBI_Per_BIP_Current','RBI_Per_BIP_Prior','RBI_Per_BIP_Prior_2',
            'FB._Current','FB._Prior','FB._Prior_2',
            'GB._Current','GB._Prior','GB._Prior_2','LD._Current','LD._Prior','LD._Prior_2',
            'Hard._Current','Hard._Prior','Hard._Prior_2','Spd_Current','Spd_Prior','Spd_Prior_2',
            'poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','SwStr._Current','SwStr._Prior',
            'SwStr._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
            'Contact._Current','Contact._Prior','Contact._Prior_2','Zone._Current',
            'Zone._Prior','Zone._Prior_2','F.Strike._Current','F.Strike._Prior',
            'F.Strike._Prior_2','PA_Projected')
x_vars2 <- c('RBI_Per_BIP_Current','RBI_Per_BIP_Prior','RBI_Per_BIP_Prior_2',
             'FB._Current','FB._Prior','FB._Prior_2',
             'GB._Current','GB._Prior','GB._Prior_2','LD._Current',
             'LD._Prior','LD._Prior_2','Hard._Current','Hard._Prior',
             'Hard._Prior_2','Spd_Current','Spd_Prior','Spd_Prior_2','Age_Projected',
             'Pos_Group_Current','MLB_Service_Projected','SwStr._Current','SwStr._Prior',
             'SwStr._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
             'Contact._Current','Contact._Prior','Contact._Prior_2','Zone._Current',
             'Zone._Prior','Zone._Prior_2','F.Strike._Current','F.Strike._Prior',
             'F.Strike._Prior_2','PA_Projected')
list_of_errors <- list()
list_of_mapes <- list()
for (i in 1:6)
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
                          model_type = 'gbm', tuneLength = 5, years_out = 6, errors, 1)
model <- z[[2]]
#coef(model$finalModel, model$bestTune$lambda)

RBI_Per_BIP <- z[[1]] %>% 
  select(Season_Projected, Playerid, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(RBI_Per_BIP  = 'Stat_Projected', RBI_Per_BIP_Upper = 'Stat_Projected_Upper', 
         RBI_Per_BIP_Lower = 'Stat_Projected_Lower')

future_preds <- future_preds %>% 
  inner_join(RBI_Per_BIP, by = c('Playerid','Season_Projected')) %>%
  mutate(RBI = RBI_Per_BIP*(BIP - SF) + (1.565*HR) + SF,
         RBI_Lower = RBI_Per_BIP_Lower*(BIP_Lower - SF) + (1.565*HR_Lower) + SF,
         RBI_Upper = RBI_Per_BIP_Upper*(BIP_Upper - SF) + (1.565*HR_Upper) + SF)
#######################################################################################
# Projecting Runs Per TOB
hitters <- add_projection_years(hitters, 11)
hitters2 <- add_projected_prior_seasons(hitters)
hitters2 <- merge_hitting_stats(hitters2, c('PA','Runs_Per_TOB','SwStr.','Swing.','Contact.',
                                            'Zone.','F.Strike.','O.Contact.','Z.Contact.',
                                            'Z.Swing.','O.Swing.','FB.','LD.','GB.','Pull.',
                                            'Oppo.','Cent.','Hard.','Med.','Soft.','Spd'))
history_future <- historical_future_split(hitters2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(PA_Harmonic = 2 / ((1 / PA_Current) + (1 / PA_Projected)))

y_var <- c('Runs_Per_TOB_Projected')
x_vars <- c('Runs_Per_TOB_Current','Runs_Per_TOB_Prior','Runs_Per_TOB_Prior_2',
            'FB._Current','FB._Prior','FB._Prior_2',
            'GB._Current','GB._Prior','GB._Prior_2','LD._Current','LD._Prior','LD._Prior_2',
            'Hard._Current','Hard._Prior','Hard._Prior_2','Spd_Current','Spd_Prior','Spd_Prior_2',
            'poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','SwStr._Current','SwStr._Prior',
            'SwStr._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
            'Contact._Current','Contact._Prior','Contact._Prior_2','Zone._Current',
            'Zone._Prior','Zone._Prior_2','F.Strike._Current','F.Strike._Prior',
            'F.Strike._Prior_2','PA_Projected')
x_vars2 <- c('Runs_Per_TOB_Current','Runs_Per_TOB_Prior','Runs_Per_TOB_Prior_2',
             'FB._Current','FB._Prior','FB._Prior_2',
             'GB._Current','GB._Prior','GB._Prior_2','LD._Current',
             'LD._Prior','LD._Prior_2','Hard._Current','Hard._Prior',
             'Hard._Prior_2','Spd_Current','Spd_Prior','Spd_Prior_2','Age_Projected',
             'Pos_Group_Current','MLB_Service_Projected','SwStr._Current','SwStr._Prior',
             'SwStr._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
             'Contact._Current','Contact._Prior','Contact._Prior_2','Zone._Current',
             'Zone._Prior','Zone._Prior_2','F.Strike._Current','F.Strike._Prior',
             'F.Strike._Prior_2','PA_Projected')
list_of_errors <- list()
list_of_mapes <- list()
for (i in 1:6)
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
                          model_type = 'gbm', tuneLength = 5, years_out = 6, errors, 1)
model <- z[[2]]
#coef(model$finalModel, model$bestTune$lambda)

Runs_Per_TOB<- z[[1]] %>% 
  select(Season_Projected, Playerid, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(Runs_Per_TOB  = 'Stat_Projected', Runs_Per_TOB_Upper = 'Stat_Projected_Upper', 
         Runs_Per_TOB_Lower = 'Stat_Projected_Lower')

future_preds <- future_preds %>% 
  inner_join(Runs_Per_TOB, by = c('Playerid','Season_Projected')) %>%
  mutate(R = Runs_Per_TOB*(Hits + BB + HBP - HR) + HR,
         R_Lower = Runs_Per_TOB_Lower*(Hits_Lower + BB_Lower + HBP - HR_Lower) + HR_Lower,
         R_Upper = Runs_Per_TOB_Upper*(Hits_Upper + BB_Upper + HBP - HR_Upper) + HR_Upper)
##############################################################################
# View forecasts
setwd('C:/Users/rusla/OneDrive/MLBAnalyticsJobs/Projections/Hitting/Player_Comparisons/Player_Metrics_Forecasts')
pred_year <- 2020
preds <- future_preds %>% 
  filter(Season_Projected == pred_year) %>%
  mutate(K. = SO / PA,
         BB. = BB / PA) %>%
  select(Name,Season_Projected,Pos_Group_Current,PA,AB,Hits,
         HR_Lower,HR,HR_Upper,BB,SO,K., BB.,AVG,OBP,SLG,OPS,ISO,BABIP,wOBA,
         wRC.,wRC._Lower,wRC._Upper,RBI_Lower,RBI,RBI_Upper,R_Lower,R,R_Upper) %>% 
  arrange(Season_Projected, Name) %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(PA = as.integer(PA), AB = as.integer(AB), 
         Hits = as.integer(Hits), HR = as.integer(HR),
         BB = as.integer(BB), SO = as.integer(SO), 
         wRC. = as.integer(wRC.), RBI = as.integer(RBI),
         R = as.integer(R), Season_Projected = as.integer(Season_Projected)) %>%
  rename(Pos_Group = 'Pos_Group_Current', Season = 'Season_Projected')

kable(preds, row.names = F) %>%
kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                          full_width = F, position = "left", fixed_thead = T) %>%
  kableExtra::footnote(symbol = paste0(pred_year," Player Metric Forecasts")) %>%
  kableExtra::scroll_box(width = "200%", height = "100%") %>%
  kableExtra::save_kable(file = paste0(pred_year,"_Player_Metric_Forecasts.html"))
  
#rhandsontable(preds, width = 2550, height = 2000) %>%
#hot_cols(columnSorting = TRUE) %>%
#hot_cols("float", format = "0.000") %>%
#hot_cols("int", format = "0") %>%
#hot_cols(colWidths = 135) %>%
#hot_rows(rowHeights = 5)
########################################################################
# ranks
setwd('C:/Users/rusla/OneDrive/MLBAnalyticsJobs/Projections/Hitting/Player_Comparisons/Player_Rankings_Forecasts')
rank_year <- 2020
ranks <- ranking_projected_players(future_preds, rank_year)
kable(ranks, row.names = F) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                            full_width = F, position = "left", fixed_thead = T) %>%
  kableExtra::footnote(symbol = paste0(rank_year," Player Ranking Forecasts")) %>%
  kableExtra::scroll_box(width = "200%", height = "100%") %>%
  kableExtra::save_kable(file = paste0(rank_year,"_Player_Ranking_Forecasts.html"))
#rhandsontable(ranks, width = 2560, height = 500) %>%
  #hot_cols(columnSorting = TRUE) %>%
  #hot_cols("float", format = "0.000") %>%
  #hot_cols("int", format = "0") %>%
  #hot_cols(colWidths = 135) %>%
  #hot_rows(rowHeights = 5)
#############################################################################
