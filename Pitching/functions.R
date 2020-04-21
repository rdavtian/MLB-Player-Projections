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
age_curves <- function(pitchers_data, x_var, y_var) 
{
  data <- pitchers_data %>%
    select(x_var, y_var, Pos_Group_Current, Playerid, Season_Current) %>% 
    distinct() %>%
    filter(!is.na(Pos_Group_Current)) %>%
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

merge_hitting_stats <- function(pitchers_data, pitching_stats)
{
  pitchers_data <- pitchers_data %>% 
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