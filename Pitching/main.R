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
pitchers <- read.csv('player_age_seasons.csv', header = T, fileEncoding="UTF-8-BOM")[, c('Season','Name','Age','playerid')]
salaries <- loadWorkbook('salaries.xlsx')
cpi <- read.csv('CPI.csv', header = T, fileEncoding="UTF-8-BOM")
pitching$Name <- as.character(pitching$Name)
pitchers$Name <- as.character(pitchers$Name)

pitching <- pitching %>%
  mutate(Start.IP = case_when(IP - Relief.IP == 0 ~ 0,
                              TRUE ~ Start.IP),
         Relief.IP = case_when(IP - Start.IP == 0 ~ 0,
                               TRUE ~ Relief.IP))
pitching$Start.IP <- ifelse((is.na(pitching$Start.IP)) & (pitching$IP - pitching$Relief.IP != 0), pitching$IP - pitching$Relief.IP, pitching$Start.IP)
pitching$Relief.IP <- ifelse((is.na(pitching$Relief.IP)) & (pitching$IP - pitching$Start.IP != 0), pitching$IP - pitching$Start.IP, pitching$Relief.IP)
pitching <- pitching %>% 
  mutate(Pos_Group = case_when(Start.IP > Relief.IP ~ 'SP',
                               Start.IP <= Relief.IP ~ 'RP'))

sheetNames <- sheets(salaries)
for(i in 1:length(sheetNames))
{
  assign(sheetNames[i],readWorkbook(salaries,sheet = i))
}
salaries <- rbind(salary08,salary09,salary10,salary11,salary12,salary13,salary14,
                  salary15,salary16,salary17,salary18,salary19)
salaries <- salaries %>%
  mutate(Pos_Group = case_when(Pos %in% c('LF','CF','RF') ~ 'OF',
                               Pos %in% c('1B','3B') ~ 'CornerIF',
                               Pos %in% c('2B','SS') ~ 'MiddleIF',
                               Pos %in% c('C') ~ 'C',
                               Pos %in% c('OF') ~ 'OF',
                               Pos %in% c('DH') ~ 'DH',
                               Pos %in% c('SP','RP','P') ~ 'P')) %>% 
  select(-Pos) %>% filter(Pos_Group == 'P')
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
                                               'xFIP','BABIP','K.9'))
history_future <- historical_future_split(pitchers2, current_season)
historical <- history_future[[1]]; future <- history_future[[2]]
historical <- historical %>% 
  mutate(IP_Harmonic = 2 / ((1 / IP_Current) + (1 / IP_Projected)))

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

year_to_year_correlation(pitching, 'BABIP')

values <- as.data.frame(historical[, vars])
usdm::vif(values)
###################################################################################
# TESTING!!!!!!
# Training models on historical years (cross validation)
parametersGrid <- expand.grid(alpha = seq(0.15, 1, 0.05),
                              lambda = seq(0.005, 1, length = 25))
control <- trainControl(method = "cv", number = 3)
parametersGrid <- expand.grid(sparcity = 0.3)
y_var <- c('ERA_Projected')
x_vars <- c('ERA_Current','ERA_Prior','ERA_Prior_2',
            'poly(Age_Projected, 2)',
            'Pos_Group_Current','MLB_Service_Projected','IP_Harmonic')
x_vars2 <- c('ERA_Current','ERA_Prior','ERA_Prior_2',
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
  rename('ERA_Projected_Preds' = Stat_Projected_Preds) %>%
  mutate(abs_diff = abs(ERA_Projected - ERA_Projected_Preds))
summary(tests2$abs_diff)
mean(rmses)
mean(mapes$Mape)
summary(mapes$Mape)
hist(mapes$Mape, breaks = 100, col = 'cyan', xlim = c(0, 70))
hist(resid, breaks = 50, col = 'green')

####################################################################################
# Projecting PA 
pitchers2 <- add_projected_prior_seasons(pitchers)
pitchers2 <- merge_hitting_stats(pitchers2, c('PA','PA_PerG'))
history_future <- historical_future_split(pitchers2, current_season)
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
  select(Name, Season_Projected, Age_Projected, Playerid, Pos_Group_Current, Stat_Projected, 
         Stat_Projected_Upper, Stat_Projected_Lower) %>%
  rename(PA = 'Stat_Projected', PA_Upper = 'Stat_Projected_Upper', 
         PA_Lower = 'Stat_Projected_Lower')
#######################################################################################

