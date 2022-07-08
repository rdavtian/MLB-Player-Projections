setwd('C:/Users/rusla/OneDrive/MLBAnalyticsJobs/Projections/Pitching/')
source('functions.R')
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

# set working directory
setwd('C:/Users/rusla/OneDrive/MLBAnalyticsJobs/Projections/Pitching/Data')
# remove scientific notation 
options(scipen = 999)

#####################################################################################
# Data Reading, Cleaning, Manipulating
pitching <- read.csv('fangraphs_pitching.csv', header = T, fileEncoding="UTF-8-BOM")
pitching <- pitching %>% 
  arrange(Name, Season) %>%
  group_by(playerid) %>%
  mutate(Team = na_if(Team, "- - -")) %>%
  tidyr::fill(Team, .direction = 'updown') %>% ungroup()

pitchers <- read.csv('player_age_seasons.csv', header = T, fileEncoding="UTF-8-BOM")[, c('Season','Name','Team','Age','playerid')]
pitchers <- pitchers %>% 
  arrange(Name, Season) %>%
  group_by(playerid) %>%
  mutate(Team = na_if(Team, "- - -")) %>%
  tidyr::fill(Team, .direction = 'updown') %>% ungroup()

salaries <- loadWorkbook('salaries.xlsx')
cpi <- read.csv('CPI.csv', header = T, fileEncoding="UTF-8-BOM")
park_factor <- read.csv('park_factors.csv', header = T, fileEncoding="UTF-8-BOM")
pitching$Name <- as.character(pitching$Name)
pitchers$Name <- as.character(pitchers$Name)
pitching$Team <- as.character(pitching$Team)
pitchers$Team <- as.character(pitchers$Team)
park_factor$Team <- as.character(park_factor$Team)

pitching <- pitching %>%
  left_join(park_factor, by = 'Team') %>%
  mutate(League = as.character(League)) %>%
  mutate(Start.IP = case_when(IP - Relief.IP == 0 ~ 0,
                              TRUE ~ Start.IP),
         Relief.IP = case_when(IP - Start.IP == 0 ~ 0,
                               TRUE ~ Relief.IP))
pitching$Start.IP <- ifelse((is.na(pitching$Start.IP)) & (pitching$IP - pitching$Relief.IP != 0), pitching$IP - pitching$Relief.IP, pitching$Start.IP)
pitching$Relief.IP <- ifelse((is.na(pitching$Relief.IP)) & (pitching$IP - pitching$Start.IP != 0), pitching$IP - pitching$Start.IP, pitching$Relief.IP)
pitching <- pitching %>% 
  mutate(Pos_Group = case_when(Start.IP > Relief.IP ~ 'SP',
                               Start.IP <= Relief.IP ~ 'RP'),
         IP_PerG = IP / G,
         TrueFB. = FB. - (FB. * IFFB.),
         TrueIFFB. = (FB. * IFFB.),
         xBABIP = (.128*FB.) + (.234*GB.) + (0.700*LD.),
         Start.IP_PerGS = case_when(GS > 0 ~ Start.IP / GS, 
                              TRUE ~ 0),
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

sheetNames <- sheets(salaries)
for(i in 1:length(sheetNames))
{
  assign(sheetNames[i],readWorkbook(salaries,sheet = i))
}
salaries <- rbind(salary08,salary09,salary10,salary11,salary12,salary13,salary14,
                  salary15,salary16,salary17,salary18,salary19)
salaries <- salaries %>%
  mutate(Pos_Group_Current = case_when(Pos %in% c('LF','CF','RF') ~ 'OF',
                               Pos %in% c('1B','3B') ~ 'CornerIF',
                               Pos %in% c('2B','SS') ~ 'MiddleIF',
                               Pos %in% c('C') ~ 'C',
                               Pos %in% c('OF') ~ 'OF',
                               Pos %in% c('DH') ~ 'DH',
                               Pos %in% c('SP','P') ~ 'SP',
                               Pos %in% c('RP') ~ 'RP')) %>% 
  select(-Pos) %>% filter(Pos_Group_Current %in% c('SP','RP'))
salaries <- salaries %>% 
  inner_join(pitching[,c('Name','Season','Age')],
            by = c('Name','Season')) %>%
  rename(Age_Current = 'Age', Season_Current = 'Season') %>%
  mutate(Avg_Annual = as.numeric(Avg_Annual),
         Length = as.numeric(Length)) %>%
  inner_join(cpi, by = c('Season_Current' = 'Season')) %>%
  mutate(CPI_2020 = 250.466,
         adjusted_AAV = ((Avg_Annual * CPI_2020) / (CPI)) * 0.000001)

pitchers <- add_projection_years(pitchers, 11)
pitchers2 <- add_projected_prior_seasons(pitchers)
pitchers2 <- merge_pitching_stats(pitchers2, c('IP','Pos_Group','ERA','FIP',
                                               'xFIP','BABIP','K_Per9','xBABIP',
                                               'WAR','IP_PerG'))
history_future <- historical_future_split(pitchers2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(IP_Harmonic = 2 / ((1 / IP_Current) + (1 / IP_Projected)))

#################################################################################
# Visualizations
vars <- c('BABIP_Current','xBABIP_Prior','xBABIP_Prior_2')
scatterplot_by_group(historical, 'IP_Current','IP_Prior')
scatterplot_matrix(historical, c('BABIP_Current','BABIP_Prior'))
correlation_matrix(historical, c('wRC._Current','Hard._Current','wRC._Prior','Soft._Current',
                                 'wRC._Prior_2','GB._Current','BABIP_Prior','LD._Current'))

age_curves(historical, 'Age_Current', 'xBABIP_Current')
age_curves(salaries, 'Age_Current', 'adjusted_AAV')
age_curves_delta_method(pitching, 'BABIP','BABIP.x','BABIP.y')

year_to_year_correlation(pitching, 'K.')

values <- as.data.frame(historical[, vars])
usdm::vif(values)
###################################################################################
# TESTING!!!!!!
# Training models on historical years (cross validation)
parametersGrid <- expand.grid(alpha = seq(0.15, 1, 0.05),
                              lambda = seq(0.005, 1, length = 25))
control <- trainControl(method = "cv", number = 3)
parametersGrid <- expand.grid(sparcity = 0.3)
y_var <- c('IP_Projected')
x_vars <- c('IP_Current','IP_Prior','IP_Prior_2',
            'poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','IP_Harmonic')
x_vars2 <- c('IP_Current','IP_Prior','IP_Prior_2',
            'Age_Projected',
            'Pos_Group_Current','MLB_Service_Projected','IP_Harmonic')

x <- train_models(historical, y_var, x_vars, x_vars2, model_type = 'gbm', tuneLength = 3, 
                  years_out = 1)
mapes <- do.call(rbind, x[[1]])
coefs <- do.call(rbind, x[[2]])
rmses <- x[[3]]
resid <- unlist(x[[4]])
tests <- do.call(rbind, x[[5]])
model <- x[[6]]
coef(model$finalModel)
#coef(model$finalModel, model$bestTune$lambda)
tests2 <- tests %>% 
  mutate(Stat_Projected_Preds = Stat_Projected_Preds) %>%
  rename('IP_Projected_Preds' = Stat_Projected_Preds) %>%
  mutate(abs_diff = abs(IP_Projected - IP_Projected_Preds))
summary(tests2$abs_diff)
mean(rmses)
mean(mapes$Mape)
summary(mapes$Mape)
hist(mapes$Mape, breaks = 100, col = 'cyan', xlim = c(0, 70))
hist(resid, breaks = 50, col = 'green')
####################################################################################
# Projecting G
pitchers <- add_projection_years(pitchers, 11)
pitchers2 <- add_projected_prior_seasons(pitchers)
pitchers2 <- merge_pitching_stats(pitchers2, c('IP','Pos_Group','G'))
history_future <- historical_future_split(pitchers2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(IP_Harmonic = 2 / ((1 / IP_Current) + (1 / IP_Projected)))

y_var <- c('G_Projected')
x_vars <- c('G_Current','G_Prior','G_Prior_2','poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','IP_Harmonic')
x_vars2 <- c('G_Current','G_Prior','G_Prior_2','Age_Projected',
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

future_preds <- z[[1]] %>% 
  select(Name, Team, Season_Projected, Age_Projected, Playerid, Pos_Group_Current, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(G = 'Stat_Projected', G_Upper = 'Stat_Projected_Upper', 
         G_Lower = 'Stat_Projected_Lower')
####################################################################################
# Projecting GS
pitchers <- add_projection_years(pitchers, 11)
pitchers2 <- add_projected_prior_seasons(pitchers)
pitchers2 <- merge_pitching_stats(pitchers2, c('IP','Pos_Group','GS'))
history_future <- historical_future_split(pitchers2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(IP_Harmonic = 2 / ((1 / IP_Current) + (1 / IP_Projected)))

y_var <- c('GS_Projected')
x_vars <- c('GS_Current','GS_Prior','GS_Prior_2','poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','IP_Harmonic')
x_vars2 <- c('GS_Current','GS_Prior','GS_Prior_2','Age_Projected',
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

GS <- z[[1]] %>% 
  select(Season_Projected, Playerid, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(GS = 'Stat_Projected', GS_Upper = 'Stat_Projected_Upper', 
         GS_Lower = 'Stat_Projected_Lower')

future_preds <- future_preds %>% 
  inner_join(GS, by = c('Playerid','Season_Projected')) %>%
  mutate(GS = case_when(GS < 0 ~ 0, 
                        TRUE ~ GS))
####################################################################################
# Projecting IP_PerG to get to IP 
pitchers <- add_projection_years(pitchers, 11)
pitchers2 <- add_projected_prior_seasons(pitchers)
pitchers2 <- merge_pitching_stats(pitchers2, c('IP','Pos_Group','IP_PerG'))
history_future <- historical_future_split(pitchers2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(IP_Harmonic = 2 / ((1 / IP_Current) + (1 / IP_Projected)))

y_var <- c('IP_PerG_Projected')
x_vars <- c('IP_PerG_Current','IP_PerG_Prior','IP_PerG_Prior_2','poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','IP_Harmonic')
x_vars2 <- c('IP_PerG_Current','IP_PerG_Prior','IP_PerG_Prior_2','Age_Projected',
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

IP_PerG <- z[[1]] %>% 
  select(Season_Projected, Playerid, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(IP_PerG = 'Stat_Projected', IP_PerG_Upper = 'Stat_Projected_Upper', 
         IP_PerG_Lower = 'Stat_Projected_Lower')

future_preds <- future_preds %>% 
  inner_join(IP_PerG, by = c('Playerid','Season_Projected')) %>%
  mutate(IP = IP_PerG * G, 
         IP_Upper = IP_PerG_Upper * G_Upper,
         IP_Lower = IP_PerG_Lower * G_Lower)
##########################################################################
# Projecting Start.IP Per GS and multiply by GS to get Start IP
pitchers <- add_projection_years(pitchers, 11)
pitchers2 <- add_projected_prior_seasons(pitchers)
pitchers2 <- merge_pitching_stats(pitchers2, c('IP','Pos_Group','Start.IP_PerGS'))
history_future <- historical_future_split(pitchers2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>%
  mutate(IP_Harmonic = 2 / ((1 / IP_Current) + (1 / IP_Projected)))

y_var <- c('Start.IP_PerGS_Projected')
x_vars <- c('Start.IP_PerGS_Current','Start.IP_PerGS_Prior','Start.IP_PerGS_Prior_2','poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','IP_Harmonic')
x_vars2 <- c('Start.IP_PerGS_Current','Start.IP_PerGS_Prior','Start.IP_PerGS_Prior_2','Age_Projected',
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

Start.IP_PerGS <- z[[1]] %>%
  select(Season_Projected, Playerid, Stat_Projected,
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(Start.IP_PerGS = 'Stat_Projected', Start.IP_PerGS_Upper = 'Stat_Projected_Upper',
         Start.IP_PerGS_Lower = 'Stat_Projected_Lower') %>%
  mutate(Start.IP_PerGS = case_when(Start.IP_PerGS < 0 ~ 0, 
                                    TRUE ~ Start.IP_PerGS),
         Start.IP_PerGS_Upper = case_when(Start.IP_PerGS_Upper < 0 ~ 0, 
                                    TRUE ~ Start.IP_PerGS_Upper))

future_preds <- future_preds %>%
  inner_join(Start.IP_PerGS, by = c('Playerid','Season_Projected')) %>%
  mutate(Start.IP = Start.IP_PerGS * GS,
         Start.IP_Upper = Start.IP_PerGS_Upper * GS_Upper,
         Start.IP_Lower = Start.IP_PerGS_Lower * GS_Lower)
####################################################################################
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

##############################################################################
# View forecasts
setwd('C:/Users/rusla/OneDrive/MLBAnalyticsJobs/Projections/Pitching/Player_Comparisons/Player_Metrics_Forecasts')
pred_year <- 2020
preds <- future_preds %>% 
  filter(Season_Projected == pred_year) %>%
  select(Name,Season_Projected,Pos_Group_Current,G,GS,IP,ERA,FIP,xFIP,
         BABIP,H,BB,SO,BB.,K.,BB_Per9,K_Per9,WHIP,ERA_minus,FIP_minus,
         xFIP_minus) %>% 
  arrange(Season_Projected, Name) %>%
  mutate_if(is.numeric, round, 3) %>%
  #mutate(PA = as.integer(PA), AB = as.integer(AB), 
         #Hits = as.integer(Hits), HR = as.integer(HR),
         #BB = as.integer(BB), SO = as.integer(SO), 
         #wRC. = as.integer(wRC.), RBI = as.integer(RBI),
         #R = as.integer(R), Season_Projected = as.integer(Season_Projected)) %>%
  rename(Pos = 'Pos_Group_Current', Season = 'Season_Projected')

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
setwd('C:/Users/rusla/OneDrive/MLBAnalyticsJobs/Projections/Pitching/Player_Comparisons/Player_Rankings_Forecasts')
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

#######################################################################
# Visualizations
setwd('C:/Users/rusla/OneDrive/MLBAnalyticsJobs/Projections/Pitching/Player_Comparisons/Plots')
projection_plot(future_preds, 'Madison Bumgarner', 'BB','BB_Upper','BB_Lower')
plot_past_future_comp(pitching, future_preds, 'Madison Bumgarner','Clayton Kershaw',
                      'H','H_Lower','H_Upper')
projection_comp_plot1(future_preds,'Madison Bumgarner','Clayton Kershaw','BABIP','BABIP_Upper','BABIP_Lower')
projection_comp_plot2(future_preds,'Madison Bumgarner','Clayton Kershaw','BABIP','BABIP_Upper','BABIP_Lower')

past_player_performance(pitching, 'Madison Bumgarner', 'H')
past_player_performance_comp1(pitching, 'Madison Bumgarner','Clayton Kershaw','FIP')

plot_past_future(pitching, future_preds, 'Clayton Kershaw', 'ERA_minus','ERA_minus_Lower','ERA_minus_Upper')
plot_past_future_comp(pitching, future_preds, 'Kenley Jansen','Aroldis Chapman',
                      'ERA_minus','ERA_minus_Lower','ERA_minus_Upper')
  #############################################################################
# Which players are projected higher, lower for next season
current_season = 2019
all <- plot_past_future_comp(pitching, future_preds, 'Clayton Kershaw', 'Madison Bumgarner',
                             'ERA_minus','ERA_minus_Lower','ERA_minus_Upper')
all <- all %>% filter(Season >= current_season, Season <= current_season + 1)
list_of_names <- unique(all$Name)
list <- list()
i = 1
stat = 'ERA_minus'
for (name in list_of_names)
{
  old_value <- all %>% 
    filter(Name == name, Season == current_season) %>% 
    select(stat) %>% pull() %>% round(., 3)
  new_value <- all %>% 
    filter(Name == name, Season == current_season + 1) %>%
    select(stat) %>% pull() %>% round(., 3)
  pct_change <- round(((new_value - old_value) / (old_value))*100,3)
  list[[i]] <- data.frame(cbind(name, stat, old_value, new_value, pct_change))
  i = i + 1
}
df <- bind_rows(list)
df <- df %>%
  mutate(stat = as.character(stat),
         old_value = as.numeric(old_value),
         new_value = as.numeric(new_value), 
         pct_change = as.numeric(pct_change)) %>%
  arrange(-pct_change) %>% View()

future_preds %>% inner_join(pitching[pitching$Season %in% c(2017,2018,2019),c('Name','Team')],
                     by = c('Name')) %>%
  filter(Team == "Giants") %>% distinct() %>% View()
