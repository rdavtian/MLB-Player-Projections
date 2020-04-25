#https://www.usatoday.com/sports/mlb/salaries/2008/player/all/
setwd('C:/Users/rusla/OneDrive/MLBAnalyticsJobs/Projections/Hitting/')
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
setwd('C:/Users/rusla/OneDrive/MLBAnalyticsJobs/Projections/Hitting/Data')
# remove scientific notation 
options(scipen = 999)

#####################################################################################
# Data Reading, Cleaning, Manipulating
offense <- read.csv('fangraphs_hitting.csv', header = T, fileEncoding="UTF-8-BOM")
offense <- offense %>% 
  arrange(Name, Season) %>%
  group_by(playerid) %>%
  mutate(Team = na_if(Team, "- - -")) %>%
  tidyr::fill(Team, .direction = 'updown') %>% ungroup()

hitters <- read.csv('player_age_seasons.csv', header = T, fileEncoding="UTF-8-BOM")[, c('Season','Name','Team','Age','playerid')]
hitters <- hitters %>% 
  arrange(Name, Season) %>%
  group_by(playerid) %>%
  mutate(Team = na_if(Team, "- - -")) %>%
  tidyr::fill(Team, .direction = 'updown') %>% ungroup()

positions <- read.csv('positions.csv', header = T, fileEncoding="UTF-8-BOM")[, c('Season','Name','Pos','Inn','playerid')]
salaries <- loadWorkbook('salaries.xlsx')
cpi <- read.csv('CPI.csv', header = T, fileEncoding="UTF-8-BOM")
offense$Name <- as.character(offense$Name)
hitters$Name <- as.character(hitters$Name)
positions$Name <- as.character(positions$Name)
offense$Team <- as.character(offense$Team)
hitters$Team <- as.character(hitters$Team)

positions <- sqldf("SELECT Season, Name, playerid, Pos, MAX(Inn) AS Inn
                    FROM positions GROUP BY playerid, Season")
offense <- offense %>%
  mutate(PA_PerG = PA / G,
         PA_PerHBP = case_when(HBP > 0 ~ PA / HBP,
                               HBP == 0 ~ 0),
         PA_PerSF = case_when(SF > 0 ~ PA / SF,
                              SF == 0 ~ 0),
         TrueFB. = FB. - (FB. * IFFB.),
         TrueIFFB. = (FB. * IFFB.),
         AB_PerHR = AB / HR,
         BIP = AB - SO - HR + SF,
         BIP_AVG = BIP / PA,
         OPS = OBP + SLG,
         TB = (X1B + 2*(X2B) + 3*(X3B) + 4*(HR)),
         RBI_Per_BIP = (RBI - (HR*1.565) - SF) / (AB - HR - SO),
         Runs_Per_TOB = (R - HR) / (H + BB + HBP - HR), 
         xBABIP = 0.1911 + (LD. * 0.38) - (TrueFB. * 0.1502) - 
         (TrueIFFB. * 0.4173) + (Hard. * 0.25502) + (Spd*0.0049) + 
           ((GB. * Pull.) * -0.1492))
offense$Name[offense$playerid == 14145] <- 'Daniel  Robertson'
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
                               Pos %in% c('SP','RP','P') ~ 'P')) %>% select(-Pos)
salaries <- salaries %>% 
  left_join(offense[,c('playerid','Season','Age')],
            by = c('Playerid' = 'playerid','Season')) %>%
  rename(Age_Current = 'Age', Season_Current = 'Season') %>%
  mutate(Avg_Annual = as.numeric(Avg_Annual),
         Length = as.numeric(Length)) %>%
  inner_join(cpi, by = c('Season_Current' = 'Season')) %>%
  mutate(CPI_2020 = 250.466,
         adjusted_AAV = ((Avg_Annual * CPI_2020) / (CPI)) * 0.000001)

hitters <- add_projection_years(hitters, 11)
hitters2 <- add_projected_prior_seasons(hitters)
hitters2 <- merge_hitting_stats(hitters2, c('PA','AVG','SwStr.','Swing.','Contact.',
                                            'Zone.','F.Strike.','O.Contact.','Z.Contact.',
                                            'Z.Swing.','O.Swing.','FB.','LD.','GB.','Pull.',
                                            'Oppo.','Cent.','Hard.','Med.','Soft.','Spd',
                                            'ISO','wOBA'))
history_future <- historical_future_split(hitters2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(PA_Harmonic = 2 / ((1 / PA_Current) + (1 / PA_Projected)))
#################################################################################
# Visualizations
vars <- c('BABIP_Current','xBABIP_Prior','xBABIP_Prior_2')
scatterplot_by_group(historical, 'PA_PerG_Current','PA_PerG_Prior')
scatterplot_matrix(historical, c('BABIP_Current','BABIP_Prior'))
correlation_matrix(historical, c('wRC._Current','Hard._Current','wRC._Prior','Soft._Current',
                                 'wRC._Prior_2','GB._Current','BABIP_Prior','LD._Current'))

age_curves(historical, 'Age_Current', 'wOBA_Current')
age_curves(salaries, 'Age_Current', 'adjusted_AAV')
age_curves_delta_method(offense, 'BABIP','BABIP.x','BABIP.y')

year_to_year_correlation(offense, 'Hard.')

values <- as.data.frame(historical[, vars])
usdm::vif(values)
###################################################################################
# TESTING!!!!!!
# Training models on historical years (cross validation)
parametersGrid <- expand.grid(alpha = seq(0.15, 1, 0.05),
                              lambda = seq(0.005, 1, length = 25))
control <- trainControl(method = "cv", number = 3)
parametersGrid <- expand.grid(sparcity = 0.3)
y_var <- c('AVG_Projected')
x_vars <- c('AVG_Current','AVG_Prior','AVG_Prior_2','FB._Current','FB._Prior','FB._Prior_2',
            'GB._Current','GB._Prior','GB._Prior_2','LD._Current','LD._Prior','LD._Prior_2',
            'Hard._Current','Hard._Prior','Hard._Prior_2','Spd_Current','Spd_Prior','Spd_Prior_2',
            'poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','SwStr._Current','SwStr._Prior',
            'SwStr._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
            'Contact._Current','Contact._Prior','Contact._Prior_2','Zone._Current',
            'Zone._Prior','Zone._Prior_2','F.Strike._Current','F.Strike._Prior',
            'F.Strike._Prior_2','PA_Projected')
x_vars2 <- c('AVG_Current','AVG_Prior','AVG_Prior_2','FB._Current','FB._Prior','FB._Prior_2',
             'GB._Current','GB._Prior','GB._Prior_2','LD._Current','LD._Prior','LD._Prior_2',
             'Hard._Current','Hard._Prior','Hard._Prior_2','Spd_Current','Spd_Prior','Spd_Prior_2',
             'Age_Projected',
             'Pos_Group_Current','MLB_Service_Projected','SwStr._Current','SwStr._Prior',
             'SwStr._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
             'Contact._Current','Contact._Prior','Contact._Prior_2','Zone._Current',
             'Zone._Prior','Zone._Prior_2','F.Strike._Current','F.Strike._Prior',
             'F.Strike._Prior_2','PA_Projected')

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
  rename('AVG_Projected_Preds' = Stat_Projected_Preds) %>%
  mutate(abs_diff = abs(AVG_Projected - AVG_Projected_Preds))
summary(tests2$abs_diff)
mean(rmses)
mean(mapes$Mape)
summary(mapes$Mape)
hist(mapes$Mape, breaks = 100, col = 'cyan', xlim = c(0, 70))
hist(resid, breaks = 50, col = 'green')

####################################################################################
# Projecting PA 
hitters2 <- add_projected_prior_seasons(hitters)
hitters2 <- merge_hitting_stats(hitters2, c('PA','PA_PerG'))
history_future <- historical_future_split(hitters2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(PA_Harmonic = 2 / ((1 / PA_Current) + (1 / PA_Projected)))

y_var <- c('PA_PerG_Projected')
x_vars <- c('PA_PerG_Current','PA_PerG_Prior','PA_PerG_Prior_2','poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','PA_Projected')
x_vars2 <- c('PA_PerG_Current','PA_PerG_Prior','PA_PerG_Prior_2','Age_Projected',
             'Pos_Group_Current','MLB_Service_Projected','PA_Projected')

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
                          model_type = 'gbm', tuneLength = 5, years_out = 6, errors, 150)
model <- z[[2]]
#coef(model$finalModel, model$bestTune$lambda)

future_preds <- z[[1]] %>% 
  select(Name, Team, Season_Projected, Age_Projected, Playerid, Pos_Group_Current, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(PA = 'Stat_Projected', PA_Upper = 'Stat_Projected_Upper', 
         PA_Lower = 'Stat_Projected_Lower')
#######################################################################################
# Projecting K%
hitters2 <- add_projected_prior_seasons(hitters)
hitters2 <- merge_hitting_stats(hitters2, c('PA','K.','SwStr.','Swing.','Contact.',
                                            'Zone.','F.Strike.','O.Contact.','Z.Contact.',
                                            'Z.Swing.','O.Swing.'))
history_future <- historical_future_split(hitters2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(PA_Harmonic = 2 / ((1 / PA_Current) + (1 / PA_Projected)))

y_var <- c('K._Projected')
x_vars <- c('K._Current','K._Prior','K._Prior_2','poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','SwStr._Current','SwStr._Prior',
            'SwStr._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
            'Contact._Current','Contact._Prior','Contact._Prior_2','Zone._Current',
            'Zone._Prior','Zone._Prior_2','F.Strike._Current','F.Strike._Prior',
            'F.Strike._Prior_2','PA_Projected')
x_vars2 <- c('K._Current','K._Prior','K._Prior_2','Age_Projected',
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

K. <- z[[1]] %>% 
  select(Season_Projected, Playerid, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(K. = 'Stat_Projected', K._Upper = 'Stat_Projected_Upper', 
         K._Lower = 'Stat_Projected_Lower')

future_preds <- future_preds %>% 
  inner_join(K., by = c('Playerid','Season_Projected')) %>%
  mutate(SO = PA * K., 
         SO_Upper = PA_Upper * K._Upper,
         SO_Lower = PA_Lower * K._Lower)
#######################################################################################
# Projecting BB%
hitters2 <- add_projected_prior_seasons(hitters)
hitters2 <- merge_hitting_stats(hitters2, c('PA','BB.','SwStr.','Swing.','Contact.',
                                            'Zone.','F.Strike.','O.Contact.','Z.Contact.',
                                            'Z.Swing.','O.Swing.'))
history_future <- historical_future_split(hitters2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(PA_Harmonic = 2 / ((1 / PA_Current) + (1 / PA_Projected)))

y_var <- c('BB._Projected')
x_vars <- c('BB._Current','BB._Prior','BB._Prior_2','poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','SwStr._Current','SwStr._Prior',
            'SwStr._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
            'Contact._Current','Contact._Prior','Contact._Prior_2','Zone._Current',
            'Zone._Prior','Zone._Prior_2','F.Strike._Current','F.Strike._Prior',
            'F.Strike._Prior_2','PA_Projected')
x_vars2 <- c('BB._Current','BB._Prior','BB._Prior_2','Age_Projected',
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

BB. <- z[[1]] %>% 
  select(Season_Projected, Playerid, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(BB. = 'Stat_Projected', BB._Upper = 'Stat_Projected_Upper', 
         BB._Lower = 'Stat_Projected_Lower')

future_preds <- future_preds %>% 
  inner_join(BB., by = c('Playerid','Season_Projected')) %>%
  mutate(BB = PA * BB., 
         BB_Upper = PA_Upper * BB._Upper,
         BB_Lower = PA_Lower * BB._Lower)
#######################################################################################
# Projecting HBP
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
# Projecting BABIP
hitters2 <- add_projected_prior_seasons(hitters)
hitters2 <- merge_hitting_stats(hitters2, c('PA','BABIP','xBABIP','SwStr.','Swing.','Contact.',
                                            'Zone.','F.Strike.','O.Contact.','Z.Contact.',
                                            'Z.Swing.','O.Swing.','FB.','LD.','GB.','Pull.',
                                            'Oppo.','Cent.','Hard.','Med.','Soft.','Spd'))
history_future <- historical_future_split(hitters2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(PA_Harmonic = 2 / ((1 / PA_Current) + (1 / PA_Projected)))

y_var <- c('BABIP_Projected')
x_vars <- c('BABIP_Current','BABIP_Prior','BABIP_Prior_2','xBABIP_Current',
            'xBABIP_Prior','xBABIP_Prior_2','FB._Current','FB._Prior','FB._Prior_2',
            'GB._Current','GB._Prior','GB._Prior_2','LD._Current','LD._Prior','LD._Prior_2',
            'Hard._Current','Hard._Prior','Hard._Prior_2','Spd_Current','Spd_Prior','Spd_Prior_2',
            'poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','SwStr._Current','SwStr._Prior',
            'SwStr._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
            'Contact._Current','Contact._Prior','Contact._Prior_2','Zone._Current',
            'Zone._Prior','Zone._Prior_2','F.Strike._Current','F.Strike._Prior',
            'F.Strike._Prior_2','PA_Projected')
x_vars2 <- c('BABIP_Current','BABIP_Prior','BABIP_Prior_2','xBABIP_Current',
            'xBABIP_Prior','xBABIP_Prior_2','FB._Current','FB._Prior','FB._Prior_2',
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

BABIP <- z[[1]] %>% 
  select(Season_Projected, Playerid, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(BABIP = 'Stat_Projected', BABIP_Upper = 'Stat_Projected_Upper', 
         BABIP_Lower = 'Stat_Projected_Lower')

future_preds <- future_preds %>% 
  inner_join(BABIP, by = c('Playerid','Season_Projected'))
#######################################################################################
# Projecting FB%
hitters2 <- add_projected_prior_seasons(hitters)
hitters2 <- merge_hitting_stats(hitters2, c('PA','FB.','SwStr.','Swing.','Contact.',
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
# Projecting AVG
hitters2 <- add_projected_prior_seasons(hitters)
hitters2 <- merge_hitting_stats(hitters2, c('PA','AVG','FB.','SwStr.','Swing.','Contact.',
                                            'Zone.','F.Strike.','O.Contact.','Z.Contact.',
                                            'Z.Swing.','O.Swing.','FB.','LD.','GB.','Pull.',
                                            'Oppo.','Cent.','Hard.','Med.','Soft.','Spd'))
history_future <- historical_future_split(hitters2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(PA_Harmonic = 2 / ((1 / PA_Current) + (1 / PA_Projected)))

y_var <- c('AVG_Projected')
x_vars <- c('AVG_Current','AVG_Prior','AVG_Prior_2','FB._Current','FB._Prior','FB._Prior_2',
            'GB._Current','GB._Prior','GB._Prior_2','LD._Current','LD._Prior','LD._Prior_2',
            'Hard._Current','Hard._Prior','Hard._Prior_2','Spd_Current','Spd_Prior','Spd_Prior_2',
            'poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','SwStr._Current','SwStr._Prior',
            'SwStr._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
            'Contact._Current','Contact._Prior','Contact._Prior_2','Zone._Current',
            'Zone._Prior','Zone._Prior_2','F.Strike._Current','F.Strike._Prior',
            'F.Strike._Prior_2','PA_Projected')
x_vars2 <- c('AVG_Current','AVG_Prior','AVG_Prior_2','FB._Current','FB._Prior','FB._Prior_2',
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

AVG <- z[[1]] %>% 
  select(Season_Projected, Playerid, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(AVG = 'Stat_Projected', AVG_Upper = 'Stat_Projected_Upper', 
         AVG_Lower = 'Stat_Projected_Lower')

future_preds <- future_preds %>% 
  inner_join(AVG, by = c('Playerid','Season_Projected'))
#######################################################################################
# Projecting wRC+
hitters2 <- add_projected_prior_seasons(hitters)
hitters2 <- merge_hitting_stats(hitters2, c('PA','wRC.','FB.','SwStr.','Swing.','Contact.',
                                            'Zone.','F.Strike.','O.Contact.','Z.Contact.',
                                            'Z.Swing.','O.Swing.','FB.','LD.','GB.','Pull.',
                                            'Oppo.','Cent.','Hard.','Med.','Soft.','Spd'))
history_future <- historical_future_split(hitters2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(PA_Harmonic = 2 / ((1 / PA_Current) + (1 / PA_Projected)))

y_var <- c('wRC._Projected')
x_vars <- c('wRC._Current','wRC._Prior','wRC._Prior_2','FB._Current','FB._Prior','FB._Prior_2',
            'GB._Current','GB._Prior','GB._Prior_2','LD._Current','LD._Prior','LD._Prior_2',
            'Hard._Current','Hard._Prior','Hard._Prior_2','Spd_Current','Spd_Prior','Spd_Prior_2',
            'poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','SwStr._Current','SwStr._Prior',
            'SwStr._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
            'Contact._Current','Contact._Prior','Contact._Prior_2','Zone._Current',
            'Zone._Prior','Zone._Prior_2','F.Strike._Current','F.Strike._Prior',
            'F.Strike._Prior_2','PA_Projected')
x_vars2 <- c('wRC._Current','wRC._Prior','wRC._Prior_2','FB._Current','FB._Prior','FB._Prior_2',
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

wRC. <- z[[1]] %>% 
  select(Season_Projected, Playerid, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(wRC. = 'Stat_Projected', wRC._Upper = 'Stat_Projected_Upper', 
         wRC._Lower = 'Stat_Projected_Lower')

future_preds <- future_preds %>% 
  inner_join(wRC., by = c('Playerid','Season_Projected'))
#######################################################################################
# Projecting wOBA
hitters2 <- add_projected_prior_seasons(hitters)
hitters2 <- merge_hitting_stats(hitters2, c('PA','wOBA','FB.','SwStr.','Swing.','Contact.',
                                            'Zone.','F.Strike.','O.Contact.','Z.Contact.',
                                            'Z.Swing.','O.Swing.','FB.','LD.','GB.','Pull.',
                                            'Oppo.','Cent.','Hard.','Med.','Soft.','Spd'))
history_future <- historical_future_split(hitters2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(PA_Harmonic = 2 / ((1 / PA_Current) + (1 / PA_Projected)))

y_var <- c('wOBA_Projected')
x_vars <- c('wOBA_Current','wOBA_Prior','wOBA_Prior_2','FB._Current','FB._Prior','FB._Prior_2',
            'GB._Current','GB._Prior','GB._Prior_2','LD._Current','LD._Prior','LD._Prior_2',
            'Hard._Current','Hard._Prior','Hard._Prior_2','Spd_Current','Spd_Prior','Spd_Prior_2',
            'poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','SwStr._Current','SwStr._Prior',
            'SwStr._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
            'Contact._Current','Contact._Prior','Contact._Prior_2','Zone._Current',
            'Zone._Prior','Zone._Prior_2','F.Strike._Current','F.Strike._Prior',
            'F.Strike._Prior_2','PA_Projected')
x_vars2 <- c('wOBA_Current','wOBA_Prior','wOBA_Prior_2','FB._Current','FB._Prior','FB._Prior_2',
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

wOBA <- z[[1]] %>% 
  select(Season_Projected, Playerid, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(wOBA = 'Stat_Projected', wOBA_Upper = 'Stat_Projected_Upper', 
         wOBA_Lower = 'Stat_Projected_Lower')

future_preds <- future_preds %>% 
  inner_join(wOBA, by = c('Playerid','Season_Projected')) %>%
  mutate(Hits = AB * AVG,
         Hits_Upper = AB_Upper * AVG_Upper,
         Hits_Lower = AB_Lower * AVG_Lower)
#######################################################################################
# Projecting OBP
hitters2 <- add_projected_prior_seasons(hitters)
hitters2 <- merge_hitting_stats(hitters2, c('PA','OBP','FB.','SwStr.','Swing.','Contact.',
                                            'Zone.','F.Strike.','O.Contact.','Z.Contact.',
                                            'Z.Swing.','O.Swing.','FB.','LD.','GB.','Pull.',
                                            'Oppo.','Cent.','Hard.','Med.','Soft.','Spd'))
history_future <- historical_future_split(hitters2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(PA_Harmonic = 2 / ((1 / PA_Current) + (1 / PA_Projected)))

y_var <- c('OBP_Projected')
x_vars <- c('OBP_Current','OBP_Prior','OBP_Prior_2','FB._Current','FB._Prior','FB._Prior_2',
            'GB._Current','GB._Prior','GB._Prior_2','LD._Current','LD._Prior','LD._Prior_2',
            'Hard._Current','Hard._Prior','Hard._Prior_2','Spd_Current','Spd_Prior','Spd_Prior_2',
            'poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','SwStr._Current','SwStr._Prior',
            'SwStr._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
            'Contact._Current','Contact._Prior','Contact._Prior_2','Zone._Current',
            'Zone._Prior','Zone._Prior_2','F.Strike._Current','F.Strike._Prior',
            'F.Strike._Prior_2','PA_Projected')
x_vars2 <- c('OBP_Current','OBP_Prior','OBP_Prior_2','FB._Current','FB._Prior','FB._Prior_2',
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

OBP <- z[[1]] %>% 
  select(Season_Projected, Playerid, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(OBP = 'Stat_Projected', OBP_Upper = 'Stat_Projected_Upper', 
         OBP_Lower = 'Stat_Projected_Lower')

future_preds <- future_preds %>% 
  inner_join(OBP, by = c('Playerid','Season_Projected'))
#######################################################################################
# Projecting SLG
hitters2 <- add_projected_prior_seasons(hitters)
hitters2 <- merge_hitting_stats(hitters2, c('PA','SLG','FB.','SwStr.','Swing.','Contact.',
                                            'Zone.','F.Strike.','O.Contact.','Z.Contact.',
                                            'Z.Swing.','O.Swing.','FB.','LD.','GB.','Pull.',
                                            'Oppo.','Cent.','Hard.','Med.','Soft.','Spd'))
history_future <- historical_future_split(hitters2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(PA_Harmonic = 2 / ((1 / PA_Current) + (1 / PA_Projected)))

y_var <- c('SLG_Projected')
x_vars <- c('SLG_Current','SLG_Prior','SLG_Prior_2','FB._Current','FB._Prior','FB._Prior_2',
            'GB._Current','GB._Prior','GB._Prior_2','LD._Current','LD._Prior','LD._Prior_2',
            'Hard._Current','Hard._Prior','Hard._Prior_2','Spd_Current','Spd_Prior','Spd_Prior_2',
            'poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','SwStr._Current','SwStr._Prior',
            'SwStr._Prior_2','Swing._Current','Swing._Prior','Swing._Prior_2',
            'Contact._Current','Contact._Prior','Contact._Prior_2','Zone._Current',
            'Zone._Prior','Zone._Prior_2','F.Strike._Current','F.Strike._Prior',
            'F.Strike._Prior_2','PA_Projected')
x_vars2 <- c('SLG_Current','SLG_Prior','SLG_Prior_2','FB._Current','FB._Prior','FB._Prior_2',
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

SLG <- z[[1]] %>% 
  select(Season_Projected, Playerid, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(SLG = 'Stat_Projected', SLG_Upper = 'Stat_Projected_Upper', 
         SLG_Lower = 'Stat_Projected_Lower')

future_preds <- future_preds %>% 
  inner_join(SLG, by = c('Playerid','Season_Projected')) %>%
  mutate(ISO = SLG - AVG,
         ISO_Lower = SLG_Lower - AVG_Lower, 
         ISO_Upper = SLG_Upper - AVG_Upper,
         OPS = OBP + SLG,
         OPS_Lower = OBP_Lower + SLG_Lower,
         OPS_Upper = OBP_Upper + SLG_Upper)
#######################################################################################
# Projecting RBI Per BIP
hitters2 <- add_projected_prior_seasons(hitters)
hitters2 <- merge_hitting_stats(hitters2, c('PA','RBI_Per_BIP','FB.','SwStr.','Swing.','Contact.',
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
hitters2 <- add_projected_prior_seasons(hitters)
hitters2 <- merge_hitting_stats(hitters2, c('PA','Runs_Per_TOB','FB.','SwStr.','Swing.','Contact.',
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

#######################################################################
# Visualizations
setwd('C:/Users/rusla/OneDrive/MLBAnalyticsJobs/Projections/Hitting/Player_Comparisons/Plots')
projection_plot(future_preds, 'Carlos Correa', 'PA','PA_Upper','PA_Lower')
plot_past_future_comp(offense, future_preds, 'Manny Machado','Bryce Harper',
                      'wRC.','wRC._Lower','wRC._Upper')
projection_comp_plot1(future_preds,'Ben Gamel','Buster Posey','wRC.','wRC._Upper','wRC._Lower')
projection_comp_plot2(future_preds,'Ben Gamel','Buster Posey','wRC.','wRC._Upper','wRC._Lower')

past_player_performance(offense, 'Ian Kinsler', 'wRC.')
past_player_performance_comp1(offense, 'Christian Yelich','Mike Trout','wRC.')

plot_past_future(offense, future_preds, 'Carlos Correa', 'PA','PA_Lower','PA_Upper')

plot_past_future_comp(offense, future_preds, 'Mookie Betts','Mike Trout',
                      'wRC.','wRC._Lower','wRC._Upper')
#############################################################################
# Which players are projected higher, lower for next season
current_season = 2019
all <- plot_past_future_comp(offense, future_preds, 'Buster Posey', 'Evan Longoria',
                             'wRC.','wRC._Lower','wRC._Upper')
all <- all %>% filter(Season >= current_season, Season <= current_season + 1)
list_of_names <- unique(all$Name)
list <- list()
i = 1
stat = 'HR'
for (name in list_of_names)
{
  old_value <- all %>% 
    filter(Name == name, Season == current_season) %>% 
    select(stat) %>% pull() %>% round(., 1)
  new_value <- all %>% 
    filter(Name == name, Season == current_season + 1) %>%
    select(stat) %>% pull() %>% round(., 1)
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

# Take a look at player predictions from specific teams
preds %>% inner_join(offense[offense$Season %in% c(2017,2018,2019),c('Name','Team')],
                     by = c('Name')) %>%
  filter(Team == "Giants") %>% distinct() %>% View()
