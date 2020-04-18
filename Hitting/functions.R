current_season = 2019
control <- trainControl(method = "cv", number = 3)
# function that labels correlation between 2 variables
corr_eqn <- function(x,y, digits = 2, binary) 
{ 
  if (binary)
  {
    corr_coef <- round(ltm::biserial.cor(x, y, use = 'complete.obs', level = 2), digits = digits)
    paste("italic(r) == ", corr_coef)
  }
  else {
    corr_coef <- round(cor(x, y, use = 'complete.obs'), digits = digits)
    paste("italic(r) == ", corr_coef)
  }
}

# Function that creates a x,y scatterplot with correlation label
correlation_plot <- function(data, x_var, y_var, x_coord, y_coord, binary) 
{
  if (binary)
  {
    labels <- data.frame(x = x_coord, y = y_coord,
                         label = corr_eqn(data[,x_var], data[, y_var], binary = T))
  }
  else {
    labels <- data.frame(x = x_coord, y = y_coord,
                         label = corr_eqn(data[,x_var], data[, y_var], binary = F))
  }
  x_var <- rlang::sym(quo_name(enquo(x_var)))
  y_var <- rlang::sym(quo_name(enquo(y_var)))
  
  
  ggplot2::ggplot(data = data) + ggplot2::aes(!! x_var, !! y_var) + 
    ggplot2::geom_point() + 
    ggplot2::geom_text(data = labels, ggplot2::aes(x, y, label = label), parse = TRUE, size = 7, col = 'darkred') + 
    ggplot2::geom_smooth(method = "lm", col = 'red') + ggplot2::ggtitle(paste(y_var, " Vs ", x_var)) +
    ggplot2::xlab(paste(x_var)) + ggplot2::ylab(paste(y_var))
}

# creates a scatterplot matrix with correlation
scatterplot_matrix <- function(data, vars) {
  GGally::ggpairs(data[, vars], 
                  lower = list(continuous = GGally::wrap("smooth", color = "cyan")))
}

# Creates a correlation matrix
correlation_matrix <- function(data, vars)
{
  GGally::ggcorr(data[, vars], label = TRUE, label_round = 2,
                 label_size = 5, hjust = 0.75, angle = -45, layout.exp = 2)
}

# Create X, Y Scatterplot by different position groups
scatterplot_by_group <- function(data, x_var, y_var) 
{
  x_var <- rlang::sym(quo_name(enquo(x_var)))
  y_var <- rlang::sym(quo_name(enquo(y_var)))
  
  ggplot2::ggplot(data = data) + ggplot2::aes(!! x_var, !! y_var) + 
    ggplot2::geom_point() + ggplot2::facet_wrap(. ~ Pos_Group_Current) +
    ggplot2::geom_smooth(method = "lm", col = 'red') + ggplot2::ggtitle(paste(y_var, " Vs ", x_var)) +
    ggplot2::xlab(paste(x_var)) + ggplot2::ylab(paste(y_var))
}

# Build Age Curve plotting median fanGraphs metrics across Age by Position Group
age_curves <- function(hitters_data, x_var, y_var) 
{
  data <- hitters_data %>%
    select(x_var, y_var, Pos_Group_Current, Playerid, Season_Current) %>% 
    distinct() %>%
    filter(!is.na(Pos_Group_Current)) %>%
    filter(Pos_Group_Current != 'P') %>%
    filter(Pos_Group_Current != 'DH') %>%
    group_by(Age_Current, Pos_Group_Current) %>%
    summarise(median_stat = median(!! sym(y_var), na.rm = T))
  
  x_var <- rlang::sym(quo_name(enquo(x_var)))
  y_var <- rlang::sym(quo_name(enquo(y_var)))
  
  ggplot2::ggplot(data = data) + ggplot2::aes(Age_Current, median_stat, color = Pos_Group_Current) + 
    ggplot2::geom_point() + #ggplot2::ylim(-15,18) +
    ggplot2::ggtitle(paste0(y_var,' Across Age by Position')) + 
    ggplot2::xlab('Age') + ggplot2::ylab(paste0(y_var)) + #ggplot2::xlim(19,40) + 
    ggplot2::stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1.2, se = T)
}

year_to_year_correlation <- function(offense_data, metric)
{
  stat <- offense %>%
    group_by(Name) %>%
    arrange(Name, Season) %>%
    mutate(num_seasons = n(),
           current_num_season = seq(n())) %>%
    filter(num_seasons >= 7) %>%
    filter(current_num_season <= 7) %>%
    dplyr::select(Name, current_num_season, metric) %>%
    tidyr::spread(current_num_season, metric)
  colnames(stat) <- c('Name','Season1','Season2','Season3','Season4','Season5',
                      'Season6','Season7')#,'Season8','Season9')
  
  cor1 <- cor(stat$Season1, stat$Season2, use = 'complete.obs')
  cor2 <- cor(stat$Season2, stat$Season3, use = 'complete.obs')
  cor3 <- cor(stat$Season3, stat$Season4, use = 'complete.obs')
  cor4 <- cor(stat$Season4, stat$Season5, use = 'complete.obs')
  cor5 <- cor(stat$Season5, stat$Season6, use = 'complete.obs')
  cor6 <- cor(stat$Season6, stat$Season7, use = 'complete.obs')
  #cor7 <- cor(stat$Season7, stat$Season8, use = 'complete.obs')
  #cor8 <- cor(stat$Season8, stat$Season9, use = 'complete.obs')
  cor <- data.frame(cbind(cor1, cor2, cor3, cor4, cor5, cor6))#, cor7, cor8))
  colnames(cor) <- c('Year 1-2','Year 2-3','Year 3-4','Year 4-5','Year 5-6',
                     'Year 6-7')#,'Year 7-8','Year 8-9')
  rownames(cor) <- 'r'
  print(kable(round(cor,3), row.names = T))
  
  year_x <- c(stat$Season1, stat$Season2, stat$Season3, stat$Season4, stat$Season5, 
              stat$Season6)#, stat$Season7, stat$Season8)
  year_x_1 <- c(stat$Season2, stat$Season3, stat$Season4, stat$Season5, stat$Season6, 
                stat$Season7)#, stat$Season8, stat$Season9)
  year_to_year <- data.frame(year_x, year_x_1)
  
  labels <- data.frame(x = 0.2, y = 0.35,label = corr_eqn(year_to_year$year_x, year_to_year$year_x_1, binary = F))
  print(ggplot2::ggplot(data = year_to_year) + ggplot2::aes(x = year_x, y = year_x_1) + 
          ggplot2::geom_point() + 
          ggplot2::geom_text(data = labels, ggplot2::aes(x, y, label = label), parse = TRUE, size = 7, col = 'darkred') + 
          ggplot2::geom_smooth(method = "lm", col = 'red') + ggplot2::ggtitle(paste0("Year to Year ", metric)) +
          ggplot2::xlab("Year X") + ggplot2::ylab("Year X + 1"))
}

# add future projection years for players that have played since 2018
add_projection_years <- function(hitters_data, forecast_year) 
{
  for (name in unique(hitters_data$Name)) 
  { 
    season <- max(hitters_data$Season[hitters_data$Name == name])
    age <- max(hitters_data$Age[hitters_data$Name == name])
    playerid <- hitters_data$playerid[hitters_data$Name == name][1]
    if (season >= current_season - 1)
    {
      while (season < current_season + forecast_year)
      {
        season <- season + 1
        age <- age + 1
        hitters_data <- hitters_data %>% 
          add_row(Name = name, Season = season, Age = age, playerid = playerid)
      }
    }
  }
  hitters_data <- hitters_data %>% 
    group_by(playerid) %>% arrange(Name, Season) %>%
    mutate(MLB_Service_Current = seq(n()))
  return(hitters_data)
}

# Function that adds lagged seasons (2)
# Filters out any hitter aged 43 or older
# Adds cumulative mlb service in years
add_projected_prior_seasons <- function(hitters_data)
{
  hitters_data <- hitters_data %>% ungroup() %>%
    filter(Age <= 42) %>% distinct() %>%
    rename(Season_Current = 'Season', Playerid = 'playerid') %>%
    inner_join(hitters_data[,c('Season','playerid')], by = c('Playerid' = 'playerid')) %>%
    rename(Season_Projected = 'Season', Age_Current = 'Age') %>%
    filter(Season_Projected > Season_Current) %>%
    #filter(Name == 'Mike Trout') %>% 
    arrange(Name, Season_Current, Season_Projected) %>%
    group_by(Playerid) %>%
    mutate(MLB_Service_Projected = Season_Projected - min(Season_Current) + 1,
           Age_Projected = Season_Projected - Season_Current + Age_Current) %>%
    ungroup() %>% 
    mutate(Season_Prior = Season_Current - 1,
           Season_Prior_2 = Season_Current - 2)
  return(hitters_data)
}  

# Merge FanGraphs hitting stats to projected, current, prior, 2 prior, and 3 prior seasons
# Add position and create position groups for current season, extropolate position groups
# for all current seasons
merge_hitting_stats <- function(hitters_data, offensive_stats)
{
  hitters_data <- hitters_data %>% 
    left_join(positions[,c('Season','playerid','Pos')], 
              by = c('Season_Current'='Season','Playerid'='playerid')) %>%
    mutate(Pos_Group_Current = case_when(Pos %in% c('LF','CF','RF') ~ 'OF',
                                         Pos %in% c('1B','3B') ~ 'CornerIF',
                                         Pos %in% c('2B','SS') ~ 'MiddleIF',
                                         Pos %in% c('C') ~ 'C',
                                         Pos %in% c('P') ~ 'P')) %>% select(-Pos) %>% 
    tidyr::fill(Pos_Group_Current, .direction = 'downup')
  
  stats <- hitters_data %>% 
    left_join(offense[,c('Season','Name',offensive_stats)], by = c('Season_Current' = 'Season','Name'))
  colnames(stats)[(ncol(stats) - length(offensive_stats) + 1):ncol(stats)] <- 
    paste0(colnames(stats)[(ncol(stats) - length(offensive_stats) + 1):ncol(stats)],"_Current")
  
  stats <- stats %>%
    left_join(offense[,c('Season','Name',offensive_stats)], by = c('Season_Prior' = 'Season','Name'))
  colnames(stats)[(ncol(stats) - length(offensive_stats) + 1):ncol(stats)] <- 
    paste0(colnames(stats)[(ncol(stats) - length(offensive_stats) + 1):ncol(stats)],"_Prior")
  
  stats <- stats %>%
    left_join(offense[,c('Season','Name',offensive_stats)], by = c('Season_Prior_2' = 'Season','Name'))
  colnames(stats)[(ncol(stats) - length(offensive_stats) + 1):ncol(stats)] <- 
    paste0(colnames(stats)[(ncol(stats) - length(offensive_stats) + 1):ncol(stats)],"_Prior_2")
  
  #stats <- stats %>%
  #left_join(offense[,c('Season','Name',offensive_stats)], by = c('Season_Prior_3' = 'Season','Name'))
  #colnames(stats)[(ncol(stats) - length(offensive_stats) + 1):ncol(stats)] <- 
  #paste0(colnames(stats)[(ncol(stats) - length(offensive_stats) + 1):ncol(stats)],"_Prior_3")
  
  stats <- stats %>%
    left_join(offense[,c('Season','Name',offensive_stats)], by = c('Season_Projected' = 'Season','Name'))
  colnames(stats)[(ncol(stats) - length(offensive_stats) + 1):ncol(stats)] <- 
    paste0(colnames(stats)[(ncol(stats) - length(offensive_stats) + 1):ncol(stats)],"_Projected")
  
  return(stats)
}

# Split hitters data in train/test sets by splitting on most recent season completed
historical_future_split <- function(hitters_data, recent)
{
  historical <- hitters_data %>%
    filter(Season_Current < recent) %>%
    filter(MLB_Service_Current >= 3) %>%
    filter(Season_Projected <= recent)
  future <- hitters_data %>%
    filter(Season_Current >= recent)
  return(list(historical, future))
}
  
# Caret Train modeling function
# Given y_var and set of explanatory vars
train_models <- function(historical_data, y_var, x_vars, x_vars2, model_type, tuneLength, years_out)
{
  historical_years_out <- historical_data %>% 
    filter(Season_Projected - Season_Current == years_out) %>%
    select(Name, Season_Current, Season_Projected, y_var, x_vars2)
  historical_years_out <- tidyr::drop_na(historical_years_out)
  
  set.seed(43)
  until = 2010
  project = until + years_out
  i = 1
  mape_list <- list()
  coefs_list <- list()
  test_datasets <- list()
  rmse_vec <- c()
  resid_list <- list()
  
  while (project <= current_season) 
  {
    train <- historical_years_out %>%
      filter(Season_Projected <= until)
    test <- historical_years_out %>%
      filter(Season_Projected == until + years_out)

    model <- caret::train(as.formula(paste0(y_var, " ~ ", paste0(x_vars[-length(x_vars)], collapse = " + "))), 
                          method = model_type, metric = 'MAE', data = train,
                          #tuneGrid = parametersGrid,
                          tuneLength = tuneLength, verbose = F,
                          weights = PA_Projected, trControl = control)
    #plot(model)
    #summary(model)
    model_preds <- predict(model, test, interval = "confidence")
    test$Stat_Projected_Preds <- model_preds
    test_datasets[[i]] <- test
    actual <- test %>% select(y_var) %>% pull()
    mape <- (abs(actual - model_preds) / (actual))*100
    rmse <- sqrt(mean((actual - model_preds)**2))
    resid <- actual - model_preds
    mape_data <- data.frame(Season_Projection = test$Season_Projected, Mape = mape)
    if (model_type == 'glmnet')
    {
      coefs_data <- as.data.frame(as.matrix(coef(model$finalModel,model$bestTune$lambda)))
      names(coefs_data) <- 'coefficients'
    }
    else if (model_type == 'bayesglm')
    {
      coefs_data <- as.data.frame(as.matrix(coef(model$finalModel)))
      names(coefs_data) <- 'coefficients'
    }
    else if (model_type %in% c('xgbTree','xgbLinear'))
    {
      imp2 <- varImp(model)
      print(barchart(sort(rowMeans(imp2$importance), decreasing = T), 
                     main = "Variable Importance", xlab = "Average Level of Importance",
                     ylab = "Variables"))
      coefs_data <- as.data.frame(NA)
    } 
    else if (model_type %in% c('rf'))
    {
      imp2 <- model$finalModel
      print(barchart(sort(rowMeans(imp2$importance), decreasing = T), 
                     main = "RF Variable Importance", xlab = "Average Level of Importance",
                     ylab = "Variables"))
      coefs_data <- as.data.frame(NA) 
    } 
    else if (model_type %in% c('gbm'))
    {
      imp2 <- summary(model)
      print(imp2)
      barplot(sort(imp2$rel.inf, decreasing = T), horiz = T, col = 'cyan',
                     main = "GBM Variable Importance", xlab = "Average Level of Importance",
                     ylab = "Variables")
      coefs_data <- as.data.frame(NA) 
    } else {
      coefs_data <- as.data.frame(NA)
    }
    mape_list[[i]] <- mape_data
    coefs_list[[i]] <- coefs_data
    resid_list[[i]] <- resid
    rmse_vec <- append(rmse_vec, rmse)
    #print(model)
    until = until + 1
    i = i + 1
    project = project + 1
  }
  return(list(mape_list, coefs_list, rmse_vec, resid_list, test_datasets, model))  
}

# Choose arguement of season to train model to and which to predict future years,
# store predictions for next year and use those values to predict the year after
predict_future_years <- function(historical_data, future_data, y_var, x_vars, x_vars2,
                                 model_type, tuneLength, years_out, errors, err_multiplier)
{
  for (i in 1:length(errors))
  {
    keep <- errors[[i]] <= quantile(errors[[1]], c(0.9)) & errors[[i]] >= quantile(errors[[1]], c(0.1))
    errors[[i]] <- errors[[i]][keep]
  
  }
  train <- historical_data %>%
    select(Name, Season_Current, Season_Projected, y_var, x_vars2, Playerid)
  train <- tidyr::drop_na(train)
  
  test <- future_data %>% 
    select(Name, Season_Current, Season_Projected, x_vars2[-length(x_vars2)], Playerid)
  test <- tidyr::drop_na(test)
  
  model <- caret::train(as.formula(paste0(y_var, " ~ ", paste0(x_vars[-length(x_vars)], collapse = " + "))), 
                          method = model_type, metric = 'MAE', data = train,
                          #tuneGrid = parametersGrid,
                          tuneLength = tuneLength, verbose = F,
                          weights = PA_Projected, trControl = control)
  if (model_type == 'glmnet')
  {
    coefs_data <- as.data.frame(as.matrix(coef(model$finalModel,model$bestTune$lambda)))
    names(coefs_data) <- 'coefficients'
  }
  else if (model_type == 'bayesglm')
  {
    coefs_data <- as.data.frame(as.matrix(coef(model$finalModel)))
    names(coefs_data) <- 'coefficients'
  }
  else if (model_type %in% c('xgbTree','xgbLinear'))
  {
    imp2 <- varImp(model)
    print(barchart(sort(rowMeans(imp2$importance), decreasing = T), 
                   main = "Variable Importance", xlab = "Average Level of Importance",
                   ylab = "Variables"))
    coefs_data <- as.data.frame(NA)
  }
  else if (model_type %in% c('rf'))
  {
    imp2 <- model$finalModel
    print(barchart(sort(rowMeans(imp2$importance), decreasing = T), 
                   main = "RF Variable Importance", xlab = "Average Level of Importance",
                   ylab = "Variables"))
    coefs_data <- as.data.frame(NA) 
  }
  else if (model_type %in% c('gbm'))
  {
    imp2 <- summary(model)
    print(imp2)
    barplot(sort(imp2$rel.inf, decreasing = T), horiz = T, col = 'cyan',
            main = "GBM Variable Importance", xlab = "Average Level of Importance",
            ylab = "Variables")
    coefs_data <- as.data.frame(NA) 
  } else {
    coefs_data <- as.data.frame(NA)
  }

  #plot(model)
  #summary(model)
  model_preds <- predict(model, test, interval = "confidence")
  test$Stat_Projected <- model_preds * err_multiplier
  test <- test %>%
    mutate(err = case_when(Season_Projected - Season_Current == 1 ~ mean(sample(errors[[1]], 200, replace = F)),
                           Season_Projected - Season_Current == 2 ~ mean(sample(errors[[2]], 200, replace = F)),
                           Season_Projected - Season_Current == 3 ~ mean(sample(errors[[3]], 200, replace = T)),
                           Season_Projected - Season_Current == 4 ~ mean(sample(errors[[4]], 200, replace = T)),
                           Season_Projected - Season_Current == 5 ~ mean(sample(errors[[5]], 200, replace = T)),
                           Season_Projected - Season_Current == 6 ~ mean(sample(errors[[6]], 200, replace = T)))) %>%
    mutate(Stat_Projected_Upper = Stat_Projected + (2*abs(err*err_multiplier)),
           Stat_Projected_Lower = Stat_Projected - (2*abs(err*err_multiplier)),
           Stat_Projected_Lower = ifelse(Stat_Projected_Lower < 0, 0, Stat_Projected_Lower)
           ) %>%
    select(-err) %>% filter(Season_Projected - Season_Current <= years_out)
  return(list(test, model))
}

projection_plot <- function(projection_data, player_name, stat, upper, lower)
{
  projection_data <- 
    projection_data %>% filter(Name == player_name)
  
  stat <- rlang::sym(quo_name(enquo(stat)))
  upper <- rlang::sym(quo_name(enquo(upper)))
  lower <- rlang::sym(quo_name(enquo(lower)))
  print(ggplot2::ggplot(data = projection_data, aes(x = Season_Projected, y = !! stat)) + 
          geom_line(color = 'green', size = 2.3) + 
          geom_line(aes(y = !! upper), color = "darkred", linetype = "twodash") + 
          geom_line(aes(y = !! lower), color="darkred", linetype="twodash") + 
          ggtitle(paste0(player_name, " Projected ", stat)) + xlab('Season'))
}

projection_comp_plot1 <- function(projection_data, player1, player2, stat, upper, lower)
{
  projection_data <- 
    projection_data %>% filter(Name %in% c(player1, player2))
  
  stat <- rlang::sym(quo_name(enquo(stat)))
  upper <- rlang::sym(quo_name(enquo(upper)))
  lower <- rlang::sym(quo_name(enquo(lower)))
  
  ggplot2::ggplot(data = projection_data, aes(x = Season_Projected, y = !! stat, col = Name)) + 
    geom_line(size = 2.3) + 
    geom_line(aes(y = !! upper, col = Name), linetype = "twodash") + 
    geom_line(aes(y = !! lower, col = Name), linetype="twodash") + 
    ggtitle(paste0(player1," Vs ", player2, " Projected ", stat)) + xlab('Season')
}

projection_comp_plot2 <- function(projection_data, player1, player2, stat, upper, lower)
{
  projection_data <- 
    projection_data %>% filter(Name %in% c(player1, player2))
  
  stat <- rlang::sym(quo_name(enquo(stat)))
  upper <- rlang::sym(quo_name(enquo(upper)))
  lower <- rlang::sym(quo_name(enquo(lower)))
  
  ggplot2::ggplot(data = projection_data, aes(x = Season_Projected, y = !! stat)) + 
    geom_line(size = 2.3) + geom_line(aes(y = !! upper), colour = 'red', linetype = "twodash") + 
    geom_line(aes(y = !! lower), colour = 'red', linetype = "twodash") + 
    facet_grid( ~Name) + 
    ggtitle(paste0(player1," Vs ", player2, " Projected ", stat)) + xlab('Season')
}

past_player_performance <- function(past_offense_data, player_name, stat)
{
  past_offense_data <- 
    past_offense_data %>% filter(Name == player_name)
  
  stat <- rlang::sym(quo_name(enquo(stat)))
  
  ggplot2::ggplot(data = past_offense_data, aes(x = Season, y = !! stat)) + 
    geom_line(col = 'darkgreen', size = 1.3) + geom_point(size = 3) + 
    ggtitle(paste0(player_name, " ", stat)) + xlab('Season')
}

past_player_performance_comp1 <- function(past_offensive_data, player1, player2, stat)
{
  past_offensive_data <- 
    past_offensive_data %>% filter(Name %in% c(player1, player2))
  
  stat <- rlang::sym(quo_name(enquo(stat)))
  
  ggplot2::ggplot(data = past_offensive_data, aes(x = Season, y = !! stat, col = Name)) + 
    geom_line(size = 1.3) + geom_point(size = 3, col = 'black') + 
    ggtitle(paste0(player1," Vs ", player2, " ", stat)) + xlab('Season')
}

past_player_performance_comp2 <- function(past_offensive_data, player1, player2, stat) 
{
  past_offensive_data <- 
    past_offensive_data %>% filter(Name %in% c(player1, player2))
  
  stat <- rlang::sym(quo_name(enquo(stat)))
  
  ggplot2::ggplot(data = past_offensive_data, aes(x = Season, y = !! stat)) + 
    geom_line(size = 1.3, col = 'red') + geom_point(size = 3, col = 'black') + 
    facet_grid( ~Name) + 
    ggtitle(paste0(player1," Vs ", player2, " ", stat)) + xlab('Season')
}

plot_past_future <- function(past_offense, future_offense, player_name, stat, lower, upper)
{
  future_offense2 <- future_offense %>%
    rename(Season = 'Season_Projected', H = 'Hits',
           playerid = 'Playerid') %>%
    select(Name, Season, PA, AB, HR, H, AVG, OBP, SLG, ISO, wOBA, BABIP, 
           wRC., K., BB., OPS, playerid) 
  
  all <- past_offense %>%
    select(Name, Season, PA, AB, HR, H, AVG, OBP, SLG, ISO, wOBA, BABIP, wRC., 
           K., BB., OPS, playerid) %>%
    bind_rows(future_offense2) %>%
    mutate(Time = case_when(Season <= current_season ~ 'past', 
                              Season > current_season ~ 'future')) %>%
    filter(Name == player_name) 
  
  all <- all %>% 
    left_join(future_offense[, c('Season_Projected','Playerid','PA_Lower','PA_Upper',
                                 'AB_Lower','AB_Upper','HR_Lower','HR_Upper',
                                 'Hits_Lower','Hits_Upper','AVG_Lower','AVG_Upper',
                                 'OBP_Lower','OBP_Upper','SLG_Lower','SLG_Upper',
                                 'ISO_Lower','ISO_Upper','wOBA_Lower','wOBA_Upper',
                                 'BABIP_Lower','BABIP_Upper','wRC._Lower',
                                 'wRC._Upper','K._Lower','K._Upper','BB._Lower',
                                 'BB._Upper','OPS_Lower','OPS_Upper')], 
              by = c('playerid' = 'Playerid', 'Season' = 'Season_Projected'))
  
  stat <- rlang::sym(quo_name(enquo(stat)))
  upper <- rlang::sym(quo_name(enquo(upper)))
  lower <- rlang::sym(quo_name(enquo(lower)))
  
  ggplot(all, aes(x = Season, y = !! stat, col = Time)) + geom_line(size = 1.3) + 
    geom_point(size = 3) + xlab('Season') + 
    geom_line(aes(y = !! lower), colour = 'red', linetype = "twodash") + 
    geom_line(aes(y = !! upper), colour = 'red', linetype = "twodash") + 
    ggtitle(paste0(player_name, " Past and Projected ", stat)) + 
    scale_x_continuous(breaks = round(seq(min(all$Season), max(all$Season), by = 2)))
}

plot_past_future_comp <- function(past_offense, future_offense, player1, player2, stat, lower, upper)
{
  #if (grepl('.',stat))
  #{
    #stat2 <- str_replace(stat, ".$", "+")
  #}
  
  future_offense2 <- future_offense %>%
    rename(Season = 'Season_Projected', H = 'Hits',
           playerid = 'Playerid') %>%
    select(Name, Season, PA, AB, HR, H, AVG, RBI, R, OBP, SLG, OPS, ISO, wOBA, BABIP, 
           wRC., K., BB., OPS, playerid) 
  
  all <- past_offense %>%
    select(Name, Season, PA, AB, HR, H, AVG, RBI, R, OBP, SLG, OPS, ISO, wOBA, BABIP, 
           wRC., K., BB., OPS, playerid) %>%
    bind_rows(future_offense2) %>%
    mutate(Time = case_when(Season <= current_season ~ 'past', 
                            Season > current_season ~ 'future')) %>% 
    left_join(future_offense[, c('Season_Projected','Playerid','PA_Lower','PA_Upper',
                                 'AB_Lower','AB_Upper','HR_Lower','HR_Upper',
                                 'Hits_Lower','Hits_Upper','AVG_Lower','AVG_Upper',
                                 'OBP_Lower','OBP_Upper','SLG_Lower','SLG_Upper',
                                 'ISO_Lower','ISO_Upper','wOBA_Lower','wOBA_Upper',
                                 'BABIP_Lower','BABIP_Upper','wRC._Lower',
                                 'wRC._Upper','K._Lower','K._Upper','BB._Lower',
                                 'BB._Upper','OPS_Lower','OPS_Upper','RBI_Upper',
                                 'RBI_Lower','R_Upper','R_Lower')], 
              by = c('playerid' = 'Playerid', 'Season' = 'Season_Projected'))
  subset <- all %>% 
    filter(Name %in%  c(player1, player2)) 
  
  stat <- rlang::sym(quo_name(enquo(stat)))
  upper <- rlang::sym(quo_name(enquo(upper)))
  lower <- rlang::sym(quo_name(enquo(lower)))
  
  print(ggplot(subset, aes(x = Season, y = !! stat, col = Time)) + geom_line(size = 1.3) + 
    geom_point(size = 3) + facet_grid( ~Name) + 
    geom_line(aes(y = !! lower), colour = 'red', linetype = "twodash") + 
    geom_line(aes(y = !! upper), colour = 'red', linetype = "twodash") + 
    ggtitle(paste0(player2," Vs ", player1, " Past & Projected ", stat)) + 
    xlab('Season') + ylab(stat) + 
    scale_x_continuous(breaks = round(seq(min(all$Season), max(all$Season), by = 2))))
  return(all)
}

ranking_projected_players <- function(future_data, season_projected)
{
  ranks_2020 <- future_data %>%
    filter(Season_Projected == season_projected) %>%
    mutate(K. = SO / PA,
           BB. = BB / PA) %>%
    select(Name,Season_Projected,Pos_Group_Current,PA,AB,Hits,
           HR,RBI,R,BB,SO,K., BB.,AVG,OBP,SLG,OPS,ISO,BABIP,wOBA,wRC.) %>% 
    arrange(Season_Projected, Name)
  
  names <- ranks_2020$Name
  ranks_2020 <- ranks_2020[,4:ncol(ranks_2020)]
  ranks <- data.frame(apply(-ranks_2020, 2, rank, ties.method='min'))
  ranks$SO = (nrow(ranks) + 1) - ranks$SO
  ranks$K. = (nrow(ranks) + 1) - ranks$K.
  
  ranks <- ranks %>% 
    select(HR, RBI, R, AVG, OBP, SLG, OPS, ISO, wOBA, wRC., BB., K., BABIP, Hits) %>%
    mutate(Total = rowMeans(.))
  ranks$Name <- names
  ranks <- ranks %>%
    select(Name,Total,AVG,OBP,SLG,OPS,HR,RBI,R,ISO,wOBA,wRC.,BB.,K.,BABIP,Hits)
  return(ranks %>% arrange(Total))
}

age_curves_delta_method <- function(offense_data, stat, col1, col2)
{
  curve <- offense_data %>% 
    select(Season, Name, playerid, Age, PA, stat) %>%
    filter(Age <= 40 & Age >= 20) %>%
    inner_join(offense[, c('Season','playerid','Age','PA',stat)], 
               by = c('playerid')) %>%
    rename(Season_Current = 'Season.x', Season_Next = 'Season.y',
           Age_Current = 'Age.x', Age_Next = 'Age.y',
           PA_Current = 'PA.x', PA_Next = 'PA.y', 
           Stat_Current = col1, Stat_Next = col2) %>%
    filter(Season_Current < Season_Next & Season_Next - Season_Current == 1) %>%
    tidyr::unite(Age_Bucket, c(Age_Current, Age_Next), sep = '-') %>%
    #mutate(Pct_Change = ((Stat_Next - Stat_Current) / (Stat_Current)) * 100) %>%
    mutate(Weight = 2 / ((1 / PA_Current) + (1 / PA_Next)),
    Stat_Current_Adj = (Weight / PA_Current) * Stat_Current,
    Stat_Next_Adj = (Weight / PA_Next) * Stat_Next,
    Change_Adj = round((Stat_Next_Adj - Stat_Current_Adj),4),
    Pct_Change_Adj = round((Stat_Next_Adj - Stat_Current_Adj) / (Stat_Current_Adj),4) * 100) %>%
    inner_join(positions[, c('Season','playerid','Pos')], by = c('playerid','Season_Current' = 'Season')) %>%
    mutate(Pos_Group = case_when(Pos %in% c('LF','CF','RF') ~ 'OF',
                                 Pos %in% c('1B','3B') ~ 'CornerIF',
                                 Pos %in% c('2B','SS') ~ 'MiddleIF',
                                 Pos %in% c('C') ~ 'C',
                                 Pos %in% c('P') ~ 'P')) %>% select(-Pos) %>%
    filter(!is.na(Pos_Group)) %>%
    group_by(Age_Bucket, Pos_Group) %>%
    summarize(Mean_Pct_Change_Adj = mean(Pct_Change_Adj, na.rm = T))
  
  Age = c()
  for (i in 1:nrow(curve))
  {
    Age[i] = as.numeric(unlist(str_split(curve$Age_Bucket[i], '-'))[2])
  }
  curve$Age = Age
  ggplot2::ggplot(curve, aes(x = Age, y = Mean_Pct_Change_Adj)) +
    facet_grid( ~ Pos_Group) + 
    geom_point() + ggtitle(paste0(stat, " Mean Pct Change Age Curves By Position Group")) + 
    ylab('Mean Weighted Pct Change') + 
    ggplot2::stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1.2, se = T)
}