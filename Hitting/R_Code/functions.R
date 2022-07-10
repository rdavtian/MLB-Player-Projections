#https://www.usatoday.com/sports/mlb/salaries/2008/player/all/
baseball_fangraphs_scraper <- function(area, season_start, season_end, league = "all", qual, ind = 1, pitcher_type = "pit")
{
  if (str_to_lower(area) == "hitting")
  {
    df <- fg_bat_leaders(season_start, season_end, league = "all", qual = qual, ind = ind)
    
  } else {
    df <- fg_pitch_leaders(season_start, season_end, league = "all", qual = qual, ind = ind, pitcher_type = pitcher_type)
  }
  df <- df %>% select(where(~!all(is.na(.x)))) %>% 
    select_if(~n_distinct(.) > 1)
  cols_to_keep <- sort(colSums(is.na(df)) / nrow(df)) < 0.1
  cols_to_keep <- colnames(df)[cols_to_keep]
  df <- df %>% select(all_of(cols_to_keep))
  return(df)
}

scrape_fangraphs_range <- function(season_start, season_end, area)
{
  list_of_stats <- list()
  index <- 1
  for (i in seq(from = season_start, to = season_end, by = 2))
  {
    if (i == season_end)
    {
      if (area == "hitting")
      {
        df <- baseball_fangraphs_scraper(area = "hitting", season_start = season_end, 
                                         season_end = season_end, league = "all", qual = 40, ind = 1)
        df <- df %>% mutate(Season = as.character(season_end))
      } else {
        print(i)
        df <- baseball_fangraphs_scraper(area = "pitching", season_start = season_end, pitcher_type = "pit",
                                         season_end = season_end, league = "all", qual = 20, ind = 1)
        df <- df %>% mutate(Season = as.character(season_end))
      }
    } else 
    {
      if (area == "hitting")
      {
        df <- baseball_fangraphs_scraper(area = "hitting", season_start = i, 
                                         season_end = i + 1, league = "all", qual = 40, ind = 1)
      } else {
        df <- baseball_fangraphs_scraper(area = "pitching", season_start = i, pitcher_type = "pit",
                                         season_end = i + 1, league = "all", qual = 20, ind = 1)
      }
    }
    list_of_stats[[index]] <-  df
    index <- index + 1
  }
  df <- do.call("bind_rows", list_of_stats)
  return(df)
}

scrape_fangraphs_defense <- function(season_start, season_end)
{
  list_of_stats <- list()
  index <- 1
  if ((season_start < 2002) & (season_end < 2002))
  {
    url <- paste0("https://www.fangraphs.com/leaders.aspx?pos=all&stats=fld&lg=all&qual=50&type=0&season=",season_end,"&month=0&season1=",season_start,"&ind=1&team=0&rost=0&age=0&filter=&players=0&startdate=&enddate=&page=1_12000")
    pg <- read_html(url) # get page
    tab.obj <- html_nodes(pg, '#LeaderBoard1_dg1_ctl00')
    main.tab <- html_table(tab.obj, fill = TRUE) 
    df <- main.tab[[1]]
    colnames(df) <- df[2,]
    df <- df[-(1:4), , drop = FALSE]
    #df <- df[ grep("Page size", df[,1], invert = TRUE) , ]
    #colnames(df) <- df[1,]
    #df <- df[-1,]
    df <- df %>% mutate(Pos_Group = case_when(Pos %in% c("LF","RF","CF","OF") ~ "OF",
                                              Pos %in% c("2B","SS") ~ "Middle IF",
                                              Pos %in% c("1B","3B") ~ "Corner IF",
                                              Pos %in% c("P","SP","RP") ~ "P",
                                              Pos == "C" ~ "C",
                                              Pos == "DH" ~ "DH",
                                              TRUE ~ NA_character_),
                        Season = as.integer(Season)) %>%
      select(Season, Name, Team, Pos, Pos_Group, Inn)
    list_of_stats[[index]] <-  df
    index <- index + 1
  } else if ((season_start < 2002) & (season_end >= 2002))  {
    url <- paste0("https://www.fangraphs.com/leaders.aspx?pos=all&stats=fld&lg=all&qual=50&type=0&season=2001&month=0&season1=",season_start,"&ind=1&team=0&rost=0&age=0&filter=&players=0&startdate=&enddate=&page=1_12000")
    pg <- read_html(url)
    tab.obj <- html_nodes(pg, '#LeaderBoard1_dg1_ctl00')
    main.tab <- html_table(tab.obj, fill = TRUE) 
    df <- main.tab[[1]]
    colnames(df) <- df[2,]
    df <- df[-(1:4), , drop = FALSE]
    #df <- df[ grep("Page size", df[,1], invert = TRUE) , ]
    #colnames(df) <- df[1,]
    #df <- df[-1,]
    df <- df %>% mutate(Pos_Group = case_when(Pos %in% c("LF","RF","CF","OF") ~ "OF",
                                              Pos %in% c("2B","SS") ~ "Middle IF",
                                              Pos %in% c("1B","3B") ~ "Corner IF",
                                              Pos %in% c("P","SP","RP") ~ "P",
                                              Pos == "C" ~ "C",
                                              Pos == "DH" ~ "DH",
                                              TRUE ~ NA_character_),
                        Season = as.integer(Season)) %>% 
      select(Season, Name, Team, Pos, Pos_Group, Inn)
    list_of_stats[[index]] <-  df
    index <- index + 1
  }
  
  for (i in season_start:season_end)
  {
    if (season_end < 2002)
    {
      break
    }
    if (i < 2002)
    {
      next
    }
    url <- paste0("https://www.fangraphs.com/leaders.aspx?pos=all&stats=fld&lg=all&qual=50&type=1&season=",i, "&month=0&season1=",i, "&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=&enddate=&page=1_2000")
    pg <- read_html(url) # get page
    tab.obj <- html_nodes(pg, '#LeaderBoard1_dg1_ctl00')
    main.tab <- html_table(tab.obj, fill = TRUE) 
    df <- main.tab[[1]]
    colnames(df) <- df[2,]
    df <- df[-(1:4), , drop = FALSE]
    #df <- df[ grep("Page size", df[,1], invert = TRUE) , ]
    #colnames(df) <- df[1,]
    #df <- df[-1,]
    df <- df %>% mutate(Season = i,
                        Pos_Group = case_when(Pos %in% c("LF","RF","CF","OF") ~ "OF",
                                              Pos %in% c("2B","SS") ~ "Middle IF",
                                              Pos %in% c("1B","3B") ~ "Corner IF",
                                              Pos %in% c("P","SP","RP") ~ "P",
                                              Pos == "C" ~ "C",
                                              Pos == "DH" ~ "DH",
                                              TRUE ~ NA_character_)) %>% 
      select(Season, Name, Team, Pos, Pos_Group, Inn)
    list_of_stats[[index]] <-  df
    index <- index + 1
  }
  df <- do.call("bind_rows", list_of_stats)
  return(df %>% arrange(Name, Season))
}

get_park_factors <- function(season1, season2)
{
  park_factors <- list()
  i <- 1
  for (season in (season1:season2))
  {
    df <- fg_park(season)
    park_factors[[i]] <- df
    i <- i + 1
  }
  df <- do.call("bind_rows", park_factors)
  df <- df %>% 
    mutate(home_team = case_when(home_team == "Devil Rays" ~ "Rays",
                                 home_team == "Cleveland" ~ "Indians",
                                 home_team == "Diamondbacks" ~ "Dbacks",
                                 TRUE ~ home_team)) %>% 
    inner_join(teamnames, by = c("home_team" = "teamName")) %>% 
    rename("Season" = "season")
  return(df)
}

clean_hitting_data <- function(data)
{
  data2 <- data %>% 
    select(playerid, Season, Name, Team, Age, G, AB, PA, H, `1B`, `2B`, 
           `3B`, HR, R, RBI, BB, IBB, SO, HBP, SF, SH, SB, CS, AVG, K_pct, 
           BB_pct, BB_K, OBP, SLG, OPS, ISO, BABIP, wOBA, WAR, wRC_plus, 
           GB, FB, LD, GB_FB, LD_pct, GB_pct, FB_pct, IFFB_pct, HR_FB, 
           `O-Swing_pct`, `Z-Swing_pct`, Swing_pct, `O-Contact_pct`, `Z-Contact_pct`,
           SwStr_pct, Zone_pct, Contact_pct, Hard_pct) %>% 
    mutate(Season = as.integer(Season)) %>% 
    rename("O_Swing_pct" = "O-Swing_pct","Z_Swing_pct" = "Z-Swing_pct","Swing_pct" = "Swing_pct", 
           "O_Contact_pct" = "O-Contact_pct", "Z_Contact_pct" = "Z-Contact_pct") %>%
    mutate(G_pct = case_when(Season != 2020 ~ (G / 162) * 100, TRUE ~ (G / 60) * 100),
           PA_G = PA / G,
           AB_G = AB / G,
           AB_HR = round(AB / HR, 1),
           PA_HR = round(PA / HR, 1),
           WAR_162_G = round((WAR / G) * 162,1),
           PA_HBP = case_when(HBP > 0 ~ PA / HBP, HBP == 0 ~ 0),
           PA_SF = case_when(SF > 0 ~ PA / SF, SF == 0 ~ 0),
           AB_HR = case_when(HR > 0 ~ AB / HR, HR == 0 ~ 0),
           #BIP = PA - SO - BB - HBP,
           BIP = GB + FB + LD,
           `BIP_PA` = BIP / PA,
           TB = (`1B` + 2*(`2B`) + 3*(`3B`) + 4*(HR)),
           RBI_BIP = (RBI - (HR*1.565) - SF) / (AB - HR - SO),
           R_TOB = (R - HR) / (H + BB + HBP - HR))
  return(data2)
}

clean_pitching_data <- function(data)
{
  data2 <- data %>%
    mutate(`Strike%` = round((Strikes / Pitches) * 100,1)) %>% 
    select(-`#`, -CG, -ShO, -WP, -BK, -Strikes, -Balls, -RS, -Pitches, -BU, -BUH, 
           BUH_pct, -Starting, -Relieving, -RAR, -Dollars, -WPA, -WPA_minus, 
           -WPA_plus, -RE24, -REW, -pLI, -inLI, -gmLI, -exLI, -Pulls, -WPA_LI, 
           -Clutch, -XX_pct, -wKN, -wSF, -wCH, -wCB, -wSL, -wFB, -PO_pct, -KNv,
           -KN_pct, -SF_pct, -SFv, -IFFB, -IFFB_pct, -IFH, -IFH_pct, -BUH_pct) %>% 
    mutate(Season = as.integer(Season)) %>% 
    #rename("K/9" = "K_9", "BB/9" = "BB_9", "K/BB" = "K_BB", "H/9" = "H_9", "HR/9" = "HR_9",
    #"LOB%" = "LOB_pct", "GB/FB" = "GB_FB", "LD%" = "LD_pct", "GB%" = "GB_pct",
    #"FB%" = "FB_pct", "HR/FB" = "HR_FB", "FBall%" = "FBall_pct", "SL%" = "SL_pct",
    #"CB%" = "CB_pct", "CH%" = "CH_pct") %>%
    mutate(`K_pct` = round(SO / TBF, 3) * 100,
           `BB_pct` = round(BB / TBF, 3) * 100) %>% 
    tidyr::replace_na(list(Start_IP = 0, Relief_IP = 0, `FBall_pct` = 0, `SL_pct` = 0,
                           `CB_pct` = 0, `CH_pct` = 0)) %>% 
    mutate(Pos_Group = case_when(Start_IP > Relief_IP ~ 'SP',
                                 Start_IP <= Relief_IP ~ 'RP'),
           `IP_G` = IP / G,
           xBABIP = (.128*`FB%`) + (.234*`GB%`) + (0.700*`LD%`),
           `StartIP/GS` = case_when(GS > 0 ~ Start_IP / GS, TRUE ~ 0),
           TBF = IP * 2.9 + H + BB + HBP,
           BIP = GB + FB + LD,
           `BIP_IP` = BIP / IP,
           `H_IP` = H / IP,
           `BB_IP` = BB / IP,
           `HBP_IP` = HBP / IP)
  return(data2)
}

add_projection_years <- function(data, forecast_year) 
{
  for (name in unique(data$Name)) 
  { 
    season <- data %>% filter(Name == name) %>% select(Season) %>% pull() %>% max()
    age <- data %>% filter(Name == name, Season == season) %>% select(Age) %>% pull()
    player_id <- data %>% filter(Name == name, Season == season, Age == age) %>% select(playerid) %>% distinct() %>% pull()
    team <- data %>% filter(playerid == player_id, Season == season) %>% select(Team) %>% distinct() %>% pull()
    if (season >= current_season - 1)
    {
      while (season < forecast_year)
      {
        season <- season + 1
        age <- age + 1
        data <- data %>% 
          add_row(Name = name, Team = team, Season = season, Age = age, playerid = player_id)
      }
    }
  }
  data <- data %>% 
    group_by(playerid) %>% arrange(Name, Season) %>%
    mutate(MLB_Service_Current = seq(n())) %>%
    ungroup() %>% 
    arrange(Name, Season) %>% 
    group_by(playerid) %>% 
    tidyr::fill(Pos_Group, .direction = 'downup') %>% select(-Pos) %>% 
    filter(Season >= 2002) 
  return(data)
}

# Function that adds lagged seasons (2)
# Filters out any hitter aged 43 or older
# Adds cumulative mlb service in years
add_projected_prior_seasons <- function(data)
{
  data <- data %>% ungroup() %>%
    filter(Age <= 42) %>% distinct() %>%
    rename(Season_Current = 'Season', Playerid = 'playerid') %>%
    inner_join(data[,c('Season','playerid')], by = c('Playerid' = 'playerid')) %>%
    rename(Season_Projected = 'Season', Age_Current = 'Age') %>%
    filter((Season_Projected > Season_Current) & (Season_Current <= current_season) & ((Season_Projected - Season_Current) <= 5)) %>%
    arrange(Name, Season_Current, Season_Projected) %>%
    group_by(Playerid) %>%
    mutate(MLB_Service_Projected = Season_Projected - min(Season_Current) + 1,
           Age_Projected = Season_Projected - Season_Current + Age_Current,
           Age_Sqr_Projected = Age_Projected**2,
           Age_Cube_Projected = Age_Projected**3) %>%
    ungroup() %>% 
    mutate(Season_Prior = Season_Current - 1,
           Season_Prior_2 = Season_Current - 2,
           Season_Prior_3 = Season_Current - 3)
  return(data)
} 

# Merge FanGraphs hitting stats to projected, current, prior, 2 prior, and 3 prior seasons
# Add position and create position groups for current season, extrapolate position groups
# for all current seasons
merge_hitting_stats <- function(data, offensive_stats)
{
  hitter_info <- data %>% select(Name, Team, Season_Current, Age_Current, Playerid, 
                                 MLB_Service_Current, MLB_Service_Projected, Age_Projected, Age_Sqr_Projected, 
                                 Age_Cube_Projected, Season_Projected, Season_Prior, 
                                 Season_Prior_2, Season_Prior_3)
  hitter_stats <- data %>% select(-Age_Current, -MLB_Service_Current, -MLB_Service_Projected, 
                                  -Age_Projected, -Age_Sqr_Projected, -Season_Projected, -Season_Prior, 
                                  -Season_Prior_2, -Season_Prior_3, -Age_Cube_Projected) %>% distinct() %>% 
    rename("Season" = "Season_Current")
  data2 <- hitter_info %>%
    left_join(hitter_stats[, c("Playerid","Season","Inn","Pos_Group",offensive_stats)], 
              by = c("Playerid","Season_Current" = "Season")) %>% 
    rename("Inn_Current" = "Inn", "Pos_Group_Current" = "Pos_Group")
  colnames(data2)[(ncol(data2) - length(offensive_stats) + 1):ncol(data2)] <- paste0(colnames(data2[, offensive_stats]), "_Current")
  
  data2 <- data2 %>%
    left_join(hitter_stats[, c("Playerid","Season",offensive_stats)], 
              by = c("Playerid","Season_Prior" = "Season"))
  colnames(data2)[(ncol(data2) - length(offensive_stats) + 1):ncol(data2)] <- paste0(colnames(data2[, offensive_stats]), "_Prior")
  
  data2 <- data2 %>%
    left_join(hitter_stats[, c("Playerid","Season",offensive_stats)], 
              by = c("Playerid","Season_Prior_2" = "Season"))
  colnames(data2)[(ncol(data2) - length(offensive_stats) + 1):ncol(data2)] <- paste0(colnames(data2[, offensive_stats]), "_Prior_2")
  
  data2 <- data2 %>%
    left_join(hitter_stats[, c("Playerid","Season",offensive_stats)], 
              by = c("Playerid","Season_Prior_3" = "Season"))
  colnames(data2)[(ncol(data2) - length(offensive_stats) + 1):ncol(data2)] <- paste0(colnames(data2[, offensive_stats]), "_Prior_3")
  
  data2 <- data2 %>%
    left_join(hitter_stats[, c("Playerid","Season","Inn","Pos_Group",offensive_stats)], 
              by = c("Playerid","Season_Projected" = "Season")) %>% 
    rename("Inn_Projected" = "Inn", "Pos_Group_Projected" = "Pos_Group")
  colnames(data2)[(ncol(data2) - length(offensive_stats) + 1):ncol(data2)] <- paste0(colnames(data2[, offensive_stats]), "_Projected")
  
  return(data2)
}

# Split hitters data in train/test sets by splitting on most recent season completed
historical_future_split <- function(data, current_season)
{
  historical <- data %>%
    filter(Season_Current < current_season) %>%
    filter(MLB_Service_Current >= 3) %>%
    filter(Season_Projected <= current_season)
  future <- data %>%
    filter(Season_Current >= current_season)
  return(list(historical, future))
}

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
age_curves <- function(data, x_var, y_var) 
{
  data2 <- data %>%
    select(all_of(x_var), y_var, Age, Pos_Group, playerid, Season) %>% 
    distinct() %>%
    filter(!is.na(Pos_Group)) %>%
    filter(Pos_Group != 'P') %>%
    filter(Pos_Group != 'DH') %>%
    group_by(Age, Pos_Group) %>%
    summarise(median_stat = median(!! sym(y_var), na.rm = T), .groups = "drop")
  
  ggplot(data = data2) + aes(Age, median_stat, color = Pos_Group) + 
    geom_point() + #ggplot2::ylim(-15,18) +
    ggtitle(paste0(y_var,' Across Age by Position')) + 
    xlab('Age') + ylab(paste0(y_var)) + #ggplot2::xlim(19,40) + 
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1.2, se = T) + 
    theme(plot.title=element_text(hjust=0.5,vjust=0,size=17, face = 'bold'),
          plot.subtitle=element_text(face="bold", hjust= 0.5, vjust= .0, colour="#3C3C3C", size = 13)) + 
    theme(axis.text.x=element_text(vjust = .5, size=14,colour="#535353",face="bold")) + 
    theme(axis.text.y=element_text(size=15,colour="#535353",face="bold")) + 
    theme(axis.title.y=element_text(size=15,colour="#535353",face="bold",vjust=1.5)) + 
    theme(axis.title.x=element_text(size=15,colour="#535353",face="bold",vjust=0))
}

year_to_year_correlation <- function(data, metric)
{
  stat <- data %>%
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
  
  labels <- data.frame(x = 0.2, y = 0.5, label = corr_eqn(year_to_year$year_x, year_to_year$year_x_1, binary = F))
  print(ggplot(data = year_to_year) + aes(x = year_x, y = year_x_1) + 
          geom_point() + 
          geom_text(data = labels, aes(x, y, label = label), parse = TRUE, size = 10, col = 'darkred') + 
          geom_smooth(method = "lm", col = 'red') + ggtitle(paste0("Year to Year ", metric)) +
          xlab("Year X") + ylab("Year X + 1") + 
          theme(plot.title=element_text(hjust=0.5,vjust=0,size=17, face = 'bold'),
                plot.subtitle=element_text(face="bold", hjust= 0.5, vjust= .0, colour="#3C3C3C", size = 13)) + 
          theme(axis.text.x=element_text(vjust = .5, size=13,colour="#535353",face="bold")) + 
          theme(axis.text.y=element_text(size=13,colour="#535353",face="bold")) + 
          theme(axis.title.y=element_text(size=13,colour="#535353",face="bold",vjust=1.5)) + 
          theme(axis.title.x=element_text(size=13,colour="#535353",face="bold",vjust=0)))
}

train_test_split <- function(data, nrows, train_split)
{
  set.seed(0)
  data <- sample(data)[1:nrows,]
  index <- sort(sample(nrow(data), nrow(data)*train_split))
  train <- data[index,]
  test <- data[-index,]
  return(list(train, test))
}

train_models <- function(historical_data, y_var, x_vars, model_type, tuneLength, years_out, plot = T)
{
  historical_years_out <- historical_data %>% 
    filter((Season_Projected - Season_Current) %in% years_out) %>%
    select(Name, Team, Season_Current, Season_Projected, all_of(y_var), all_of(x_vars)) %>% 
    tidyr::drop_na()
  
  future2 <- future %>% 
    filter((Season_Projected - Season_Current) %in% years_out) %>%
    select(Name, Team, Season_Current, Season_Projected, x_vars[-length(x_vars)]) %>% 
    tidyr::drop_na()
  
  if (sum(grepl("Pos_Group_Current" , colnames(future2))) > 0)
  {
    pos_group_dummies <- data.frame(predict(dummyVars(~ Pos_Group_Current, data = future2), future2))
    future2 <- cbind(future2, pos_group_dummies) %>% 
      rename("Pos_Group_CurrentC" = "Pos_Group_Current.C",
             "Pos_Group_CurrentCorner IF" = "Pos_Group_Current.Corner.IF",
             "Pos_Group_CurrentMiddle IF" = "Pos_Group_Current.Middle.IF",
             "Pos_Group_CurrentOF" = "Pos_Group_Current.OF")
  }
  future2 <- future2 %>% mutate(y_var = NA)
  colnames(future2)[length(future2)] <- y_var
  
  train <- historical_years_out %>% 
    select(Name, Team, Season_Current, Season_Projected, all_of(y_var), all_of(x_vars))
  
  if (y_var == "G_pct_Projected")
  {
    control <- trainControl(method = "cv", number = 5, savePredictions = TRUE, predictionBounds = c(0, 100))
  } else {
    control <- trainControl(method = "cv", number = 5, savePredictions = TRUE)
  }
  
  x_vars2 <- x_vars[-(length(x_vars))]
  x_vars2[which(x_vars2 == "Age_Projected")] <- "poly(Age_Projected, 3)"
  
  if (model_type == "qrf")
  {
    model <- caret::train(as.formula(paste0(y_var, " ~ ", paste0(x_vars2, collapse = " + "))),
                          data = train, method = model_type, tuneLength = tuneLength, verbose = F,
                          weights = PA_Harmonic, trControl = control, metric = 'MAE')
  } else if (model_type %in% c("rqnc","rqlasso")) {
    
    model1 <- caret::train(as.formula(paste0(y_var, " ~ ", paste0(x_vars2, collapse = " + "))),
                           data = train, method = model_type, tuneLength = tuneLength, verbose = F,
                           weights = PA_Harmonic, trControl = control, metric = 'MAE',
                           preProcess = c('scale','center','nzv'), tau = 0.05)
    model2 <- caret::train(as.formula(paste0(y_var, " ~ ", paste0(x_vars2, collapse = " + "))),
                           data = train, method = model_type, tuneLength = tuneLength, verbose = F,
                           weights = PA_Harmonic, trControl = control, metric = 'MAE',
                           preProcess = c('scale','center','nzv'), tau = 0.5)
    model3 <- caret::train(as.formula(paste0(y_var, " ~ ", paste0(x_vars2, collapse = " + "))),
                           data = train, method = model_type, tuneLength = tuneLength, verbose = F,
                           weights = PA_Harmonic, trControl = control, metric = 'MAE',
                           preProcess = c('scale','center','nzv'), tau = 0.95)
  }
  if(plot)
  {
    if (model_type == "qrf")
    {
      var_imp <- (model$finalModel$importance / sum(model$finalModel$importance)) * 100
      labels <- row.names(var_imp)
      df <- data.frame(labels, var_imp)
      imp_plot <- ggplot(df, aes(x = reorder(labels, var_imp), y = var_imp)) +
        geom_bar(stat = "identity", fill = "cyan2") +
        ggtitle(paste0("Quantile Random Forest Variable Importance Chart")) + 
        coord_flip() + scale_y_continuous(name="Variable Importance (0-100)") +
        scale_x_discrete(name="") +
        theme(plot.title=element_text(hjust=0.5,vjust=0,size=17,face = 'bold'),
              plot.subtitle=element_text(face="plain", hjust= -.015, vjust= .09, colour="#3C3C3C", size = 9)) +
        theme(axis.text.x=element_text(vjust = .5, size=13,colour="#535353",face="bold")) +
        theme(axis.text.y=element_text(size=13,colour="#535353",face="bold")) +
        theme(axis.title.y=element_text(size=15,colour="#535353",face="bold",vjust=1.5)) +
        theme(axis.title.x=element_text(size=15,colour="#535353",face="bold",vjust=0)) +
        theme(panel.grid.major.y = element_line(color = "#bad2d4", size = .5)) +
        theme(panel.grid.major.x = element_line(color = "#bdd2d4", size = .5)) +
        theme(panel.background = element_rect(fill = "white"),
              legend.title = element_text(face = "bold", size = 15),
              legend.text = element_text(face = "bold", size = 12),
              legend.key.height= unit(0.7, 'cm'),
              legend.key.width= unit(0.7, 'cm')) +
        theme(strip.text = element_text(face="bold", size=13),
              strip.background = element_rect(fill="lightblue", colour="black",size=1))
    } else if (model_type %in% c("rqnc","rqlasso")) {
      
      tab <- data.frame(`5th Quantile` = round(model1$finalModel$coefficients,3), 
                        `50th Quantile` = round(model2$finalModel$coefficients,3),
                        `95th Quantile` = round(model3$finalModel$coefficients,3))
      colnames(tab) <- c("5th Quantile", "50th Quantile", "95th Quantile")
      for (i in 1:length(rownames(tab)))
      {
        if (rownames(tab)[i] == "poly(Age_Projected, 3)1")
        {
          rownames(tab)[i] <- "Age"
        } else if (rownames(tab)[i] == "poly(Age_Projected, 3)2") {
          rownames(tab)[i] <- "Age^2"
        } else if (rownames(tab)[i] == "poly(Age_Projected, 3)3") {
          rownames(tab)[i] <- "Age^3"
        }
      }
      imp_plot <- kable(tab %>% arrange(-abs(`50th Quantile`))) %>% 
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", 
                                            "responsive"), full_width = F, 
                      position = "center", fixed_thead = T) %>% 
        footnote(symbol = "Estimated Standardized Coefficients from Lasso Quantile Regression")
    }
  }
  
  if (model_type == "qrf")
  {
    preds <- data.frame(predict(model$finalModel, newdata = future2 %>% select(-all_of(y_var)), 
                                what = c(0.05, 0.5, 0.95)))
  } else if (model_type %in% c("rqnc","rqlasso")) {
    
    newdata <- future2 %>% select(-all_of(y_var), -Name, -Team) %>%
      mutate(`poly(Age_Projected, 3)1` = poly(future2$Age_Projected, degree = 3, raw = T)[,1],
             `poly(Age_Projected, 3)2` = poly(future2$Age_Projected, degree = 3, raw = T)[,2],
             `poly(Age_Projected, 3)3` = poly(future2$Age_Projected, degree = 3, raw = T)[,3]) %>%
      select(all_of(model1$finalModel$xNames)) %>% 
      mutate_all(scale, center = TRUE)
    #mutate(across(paste0(str_remove(y_var, "Projected"), "Current"):model1$finalModel$xNames[which(model1$finalModel$xNames == "poly(Age_Projected, 3)1") + 6], scale, center = TRUE))
    #mutate(across(where(is.numeric), scale, center = TRUE))
    #mutate(across(paste0(str_remove(y_var, "Projected"), "Current"):MLB_Service_Projected, scale, center = TRUE))
    preds1 <- predict(model1$finalModel, newx = data.matrix(newdata))
    preds2 <- predict(model2$finalModel, newx = data.matrix(newdata))
    preds3 <- predict(model3$finalModel, newx = data.matrix(newdata))
    preds <- data.frame(quantile..0.05 = preds1, quantile..0.5 = preds2, 
                        quantile..0.95 = preds3)
  }
  future2[, y_var] <- preds$quantile..0.5
  future2[, paste0(y_var, "_Lower")] <- preds$quantile..0.05
  future2[, paste0(y_var, "_Upper")] <- preds$quantile..0.95
  if(plot)
  {
    if (model_type == "qrf")
    {
      return(list(future2, imp_plot, model))
    } else if (model_type %in% c("rqnc","rqlasso")) {
      return(list(future2, imp_plot, model1, model2, model3))
    }
  } else {
    if (model_type == "qrf")
    {
      return(list(future2, model))
    } else if (model_type %in% c("rqnc","rqlasso")) {
      return(list(future2, model1, model2, model3))
    }
  }
}

plot_past_future_performance <- function(player, past_data, future_data, stat, percent = F)
{
  stat2 <- str_replace(stat, "_pct", "%")
  stat2 <- str_replace(stat2, "_plus", "+")
  stat2 <- str_replace(stat2, "_162_G", " Per 162 GP")
  lower <- paste0(stat, "_Projected_Lower")
  upper <- paste0(stat, "_Projected_Upper")
  
  past <- past_data %>% 
    select(Name, Team, Season, all_of(stat)) %>%
    mutate(Time = "Past") %>% 
    filter(Name == player)
  forward <- future_data %>% 
    select(Name, Team, Season_Projected, paste0(stat, "_Projected_Lower"),
           paste0(stat, "_Projected"), paste0(stat, "_Projected_Upper")) %>%
    mutate(Time = "Future") %>% 
    rename("Season" = "Season_Projected") %>% 
    filter(Name == player)
  colnames(forward)[5] <- paste0(stat)
  data <- bind_rows(past, forward)
  
  stat <- rlang::sym(quo_name(enquo(stat)))
  lower <- rlang::sym(quo_name(enquo(lower)))
  upper <- rlang::sym(quo_name(enquo(upper)))
  
  plot <- ggplot(data, aes(x = Season, y = !! stat)) + 
    geom_line(aes(col = Time), size = 1.5) + 
    geom_point(aes(col = Time), size = 3) + xlab('Season') + 
    geom_line(aes(y = !! lower), colour = 'red', linetype = "twodash", size = 1.5) + 
    geom_line(aes(y = !! upper), colour = 'red', linetype = "twodash", size = 1.5) + 
    ggtitle(paste0(unique(data$Name), " Past and Projected ", stat2, 
                   " with 90% Prediction Interval")) + xlab("Season") + ylab(stat2) +
    scale_x_continuous(breaks = round(seq(min(data$Season), max(data$Season), by = 2))) +
    theme(plot.title=element_text(hjust=0.5,vjust=0,size=18,face = 'bold'),
          plot.subtitle=element_text(face="plain", hjust= -.015, vjust= .09, colour="#3C3C3C", size = 9)) +
    theme(axis.text.x=element_text(vjust = .5, size=16,colour="#535353",face="bold")) +
    theme(axis.text.y=element_text(size=16,colour="#535353",face="bold")) +
    theme(axis.title.y=element_text(size=16,colour="#535353",face="bold",vjust=1.5)) +
    theme(axis.title.x=element_text(size=16,colour="#535353",face="bold",vjust=0)) +
    theme(panel.grid.major.y = element_line(color = "#bad2d4", size = .5)) +
    theme(panel.grid.major.x = element_line(color = "#bdd2d4", size = .5)) +
    theme(panel.background = element_rect(fill = "white"),
          legend.title = element_text(face = "bold", size = 16),
          legend.text = element_text(face = "bold", size = 16),
          legend.key.height= unit(0.7, 'cm'),
          legend.key.width= unit(0.7, 'cm')) +
    theme(strip.text = element_text(face="bold", size=16),
          strip.background = element_rect(fill="lightblue", colour="black",size=1))
  
  if(percent)
  {
    plot <- plot + 
      scalesextra::scale_y_pct(breaks = scales::breaks_extended(10)) + 
      geom_label(aes(label = paste0(round(!! stat, 1), "%")), size = 4, alpha = 0.7) +
      geom_label(aes(label = paste0(round(!! stat, 1), "%")), size = 4.1, alpha = 0.7) +
      geom_label(aes(y = !! upper, label = paste0(round(!! upper, 1), "%")), size = 4, alpha = 0.7) + 
      geom_label(aes(y = !! upper, label = paste0(round(!! upper, 1), "%")), size = 4.1, alpha = 0.7) + 
      geom_label(aes(y = !! lower, label = paste0(round(!! lower, 1), "%")), size = 4, alpha = 0.7) + 
      geom_label(aes(y = !! lower, label = paste0(round(!! lower, 1), "%")), size = 4.1, alpha = 0.7)
  } else {
    plot <- plot + scale_y_continuous(breaks = scales::breaks_extended(10)) + 
      geom_label(aes(label = !! stat), size = 4, alpha = 0.7) +
      geom_label(aes(label = !! stat), size = 4.1, alpha = 0.7) +
      geom_label(aes(y = !! upper, label = !! upper), size = 4, alpha = 0.7) + 
      geom_label(aes(y = !! upper, label = !! upper), size = 4.1, alpha = 0.7) + 
      geom_label(aes(y = !! lower, label = !! lower), size = 4, alpha = 0.7) + 
      geom_label(aes(y = !! lower, label = !! lower), size = 4.1, alpha = 0.7)
  }
  return(plot)
}

plot_player_comparison <- function(player1, player2, past_data, future_data, stat, percent = F)
{
  stat2 <- str_replace(stat, "_pct", "%")
  stat2 <- str_replace(stat2, "_plus", "+")
  stat2 <- str_replace(stat2, "_162_G", " Per 162 GP")
  lower <- paste0(stat, "_Projected_Lower")
  upper <- paste0(stat, "_Projected_Upper")
  
  past <- past_data %>% 
    select(Name, Team, Season, all_of(stat)) %>%
    mutate(Time = "Past") %>% 
    filter(Name %in% c(player1, player2))
  forward <- future_data %>% 
    select(Name, Team, Season_Projected, paste0(stat, "_Projected_Lower"),
           paste0(stat, "_Projected"), paste0(stat, "_Projected_Upper")) %>%
    mutate(Time = "Future") %>% 
    rename("Season" = "Season_Projected") %>% 
    filter(Name %in% c(player1, player2))
  colnames(forward)[5] <- paste0(stat)
  data <- bind_rows(past, forward)
  
  stat <- rlang::sym(quo_name(enquo(stat)))
  lower <- rlang::sym(quo_name(enquo(lower)))
  upper <- rlang::sym(quo_name(enquo(upper)))
  
  plot <- ggplot(data, aes(x = Season, y = !! stat)) + 
    geom_line(aes(col = Time), size = 1.5) + 
    geom_point(aes(col = Time), size = 3) + xlab('Season') + 
    geom_line(aes(y = !! lower), colour = 'red', linetype = "twodash", size = 1.5) + 
    geom_line(aes(y = !! upper), colour = 'red', linetype = "twodash", size = 1.5) + 
    ggtitle(paste0("Past and Projected ", stat2, " with 90% Prediction Interval")) + 
    xlab("Season") + ylab(stat2) +
    scale_x_continuous(breaks = round(seq(min(data$Season), max(data$Season), by = 2))) +
    theme(plot.title=element_text(hjust=0.5,vjust=0,size=18,face = 'bold'),
          plot.subtitle=element_text(face="plain", hjust= -.015, vjust= .09, colour="#3C3C3C", size = 9)) +
    theme(axis.text.x=element_text(vjust = .5, size=15,colour="#535353",face="bold")) +
    theme(axis.text.y=element_text(size=15,colour="#535353",face="bold")) +
    theme(axis.title.y=element_text(size=15,colour="#535353",face="bold",vjust=1.5)) +
    theme(axis.title.x=element_text(size=15,colour="#535353",face="bold",vjust=0)) +
    theme(panel.grid.major.y = element_line(color = "#bad2d4", size = .5)) +
    theme(panel.grid.major.x = element_line(color = "#bdd2d4", size = .5)) +
    facet_grid(~ Name) + 
    theme(panel.background = element_rect(fill = "white"),
          legend.title = element_text(face = "bold", size = 16),
          legend.text = element_text(face = "bold", size = 16),
          legend.key.height= unit(0.7, 'cm'),
          legend.key.width= unit(0.7, 'cm')) +
    theme(strip.text = element_text(face="bold", size=16),
          strip.background = element_rect(fill="lightblue", colour="black",size=1))
  
  if(percent)
  {
    plot <- plot + 
      scalesextra::scale_y_pct(breaks = scales::breaks_extended(10)) + 
      geom_label(aes(label = paste0(round(!! stat, 1), "%")), size = 3, alpha = 0.7) +
      geom_label(aes(label = paste0(round(!! stat, 1), "%")), size = 3.2, alpha = 0.7) +
      geom_label(aes(y = !! upper, label = paste0(round(!! upper, 1), "%")), size = 3, alpha = 0.7) + 
      geom_label(aes(y = !! upper, label = paste0(round(!! upper, 1), "%")), size = 3.2, alpha = 0.7) + 
      geom_label(aes(y = !! lower, label = paste0(round(!! lower, 1), "%")), size = 3, alpha = 0.7) + 
      geom_label(aes(y = !! lower, label = paste0(round(!! lower, 1), "%")), size = 3.2, alpha = 0.7)
  } else {
    plot <- plot + scale_y_continuous(breaks = scales::breaks_extended(10)) + 
      geom_label(aes(label = !! stat), size = 3, alpha = 0.7) +
      geom_label(aes(label = !! stat), size = 3.2, alpha = 0.7) +
      geom_label(aes(y = !! upper, label = !! upper), size = 3, alpha = 0.7) + 
      geom_label(aes(y = !! upper, label = !! upper), size = 3.2, alpha = 0.7) + 
      geom_label(aes(y = !! lower, label = !! lower), size = 3, alpha = 0.7) + 
      geom_label(aes(y = !! lower, label = !! lower), size = 3.2, alpha = 0.7)
  }
  return(plot)
}

print_player_projections <- function(player, quantile, past_data, future_data)
{
  
  past <- past_data %>% 
    filter(Name == player) %>% 
    select(Season, Age, G, AB, PA, H, BB, SO, BB_pct, K_pct, BB_K, AVG, OBP, 
           SLG, OPS, ISO, BABIP, wOBA, wRC_plus, WAR_162_G) %>% 
    rename("BB%" = "BB_pct", "K%" = "K_pct", "BB/K" = "BB_K", "wRC+" = "wRC_plus",
           "WAR/162" = "WAR_162_G")
  
  if (quantile == 0.5)
  {
    future <- future_data %>% 
      filter(Name == player) %>%
      select(Season_Projected, Age_Projected, G_Projected, AB_Projected, 
             PA_Projected, H_Projected, BB_Projected, SO_Projected, BB_pct_Projected,
             K_pct_Projected, BB_K_Projected, AVG_Projected, OBP_Projected, 
             SLG_Projected, OPS_Projected, ISO_Projected, BABIP_Projected,
             wOBA_Projected, wRC_plus_Projected, WAR_162_G_Projected) %>% 
      mutate(BB_pct_Projected = paste0(BB_pct_Projected, "%"),
             K_pct_Projected = paste0(K_pct_Projected, "%"))
    colnames(future) <- str_replace(colnames(future), "_Projected", "")
    colnames(future) <- str_replace(colnames(future), "_pct", "%")
    colnames(future) <- str_replace(colnames(future), "_plus", "+")
    future <- future %>% rename("BB/K" = "BB_K", "WAR/162" = "WAR_162_G")
    tab <- rbind(past, future)
    kable(tab, rownames = F) %>% 
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                    full_width = F, position = "center", fixed_thead = T) %>% 
      row_spec(which(tab$Season > current_season), bold = T, color = "black", background = "cyan") %>% 
      footnote(symbol = paste0(player, " 50th Percentile (Median) Projections"))
    
  } else if (quantile == 0.05) {
    future <- future_data %>% 
      filter(Name == player) %>%
      select(Season_Projected, Age_Projected, G_Projected, AB_Projected_Lower, 
             PA_Projected_Lower, H_Projected_Lower, BB_Projected_Lower, 
             SO_Projected_Lower, BB_pct_Projected_Lower, K_pct_Projected_Lower, 
             BB_K_Projected_Lower, AVG_Projected_Lower, OBP_Projected_Lower, 
             SLG_Projected_Lower, OPS_Projected_Lower, ISO_Projected_Lower, 
             BABIP_Projected_Lower, wOBA_Projected_Lower, wRC_plus_Projected_Lower, 
             WAR_162_G_Projected_Lower) %>% 
      mutate(BB_pct_Projected_Lower = paste0(BB_pct_Projected_Lower, "%"),
             K_pct_Projected_Lower = paste0(K_pct_Projected_Lower, "%"))
    colnames(future) <- str_replace(colnames(future), "_Projected_Lower", "")
    colnames(future) <- str_replace(colnames(future), "_Projected", "")
    colnames(future) <- str_replace(colnames(future), "_pct", "%")
    colnames(future) <- str_replace(colnames(future), "_plus", "+")
    future <- future %>% rename("BB/K" = "BB_K", "WAR/162" = "WAR_162_G")
    tab <- rbind(past, future)
    kable(tab, rownames = F) %>% 
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                    full_width = F, position = "center", fixed_thead = T) %>%
      row_spec(which(tab$Season > current_season), bold = T, color = "black", background = "cyan") %>% 
      footnote(symbol = paste0(player, " 5th Percentile Projections"))
  } else if (quantile == 0.95) {
    future <- future_data %>% 
      filter(Name == player) %>%
      select(Season_Projected, Age_Projected, G_Projected, AB_Projected_Upper, 
             PA_Projected_Upper, H_Projected_Upper, BB_Projected_Upper, 
             SO_Projected_Upper, BB_pct_Projected_Upper, K_pct_Projected_Upper, 
             BB_K_Projected_Upper, AVG_Projected_Upper, OBP_Projected_Upper, 
             SLG_Projected_Upper, OPS_Projected_Upper, ISO_Projected_Upper, 
             BABIP_Projected_Upper, wOBA_Projected_Upper, wRC_plus_Projected_Upper, 
             WAR_162_G_Projected_Upper) %>% 
      mutate(BB_pct_Projected_Upper = paste0(BB_pct_Projected_Upper, "%"),
             K_pct_Projected_Upper = paste0(K_pct_Projected_Upper, "%"))
    colnames(future) <- str_replace(colnames(future), "_Projected_Upper", "")
    colnames(future) <- str_replace(colnames(future), "_Projected", "")
    colnames(future) <- str_replace(colnames(future), "_pct", "%")
    colnames(future) <- str_replace(colnames(future), "_plus", "+")
    future <- future %>% rename("BB/K" = "BB_K", "WAR/162" = "WAR_162_G")
    tab <- rbind(past, future)
    kable(tab, rownames = F) %>% 
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                    full_width = F, position = "center", fixed_thead = T) %>% 
      row_spec(which(tab$Season > current_season), bold = T, color = "black", background = "cyan") %>% 
      footnote(symbol = paste0(player, " 95th Percentile Projections"))
  }
}

print_projection_leaderboards <- function(season, future_data, quantile)
{
  if (quantile == 0.5)
  {
    future <- future_data %>% 
      filter(Season_Projected == season) %>%
      select(Name, Season_Projected, Age_Projected, G_Projected, AB_Projected, 
             PA_Projected, H_Projected, BB_Projected, SO_Projected, BB_pct_Projected,
             K_pct_Projected, BB_K_Projected, AVG_Projected, OBP_Projected, 
             SLG_Projected, OPS_Projected, ISO_Projected, BABIP_Projected,
             wOBA_Projected, wRC_plus_Projected, WAR_162_G_Projected) %>% 
      mutate(BB_pct_Projected = BB_pct_Projected / 100,
             K_pct_Projected = K_pct_Projected / 100)
    colnames(future) <- str_replace(colnames(future), "_Projected", "")
    colnames(future) <- str_replace(colnames(future), "_pct", "%")
    colnames(future) <- str_replace(colnames(future), "_plus", "+")
    future <- future %>% rename("BB/K" = "BB_K", "WAR/162" = "WAR_162_G")
    reactable::reactable(future, defaultPageSize = 20,
                         defaultColDef = colDef(
                           footerStyle = list(fontWeight = "bold"),
                           headerStyle = list(background = "#f7f7f8")
                         ),
                         columns = list(Name = colDef(footer = "Quantile Regression"),
                                        Season = colDef(footer = "50th Percentile"),
                                        `BB%` = colDef(format = colFormat(percent = TRUE, digits = 1)),
                                        `K%` = colDef(format = colFormat(percent = TRUE, digits = 1))),
                         bordered = TRUE,
                         highlight = TRUE)
  }  else if (quantile == 0.05) {
    future <- future_data %>% 
      filter(Season_Projected == season) %>%
      select(Name, Season_Projected, Age_Projected, G_Projected, AB_Projected_Lower, 
             PA_Projected_Lower, H_Projected_Lower, BB_Projected_Lower, 
             SO_Projected_Lower, BB_pct_Projected_Lower, K_pct_Projected_Lower, 
             BB_K_Projected_Lower, AVG_Projected_Lower, OBP_Projected_Lower, 
             SLG_Projected_Lower, OPS_Projected_Lower, ISO_Projected_Lower, 
             BABIP_Projected_Lower, wOBA_Projected_Lower, wRC_plus_Projected_Lower, 
             WAR_162_G_Projected_Lower) %>% 
      mutate(BB_pct_Projected_Lower = paste0(BB_pct_Projected_Lower, "%"),
             K_pct_Projected_Lower = paste0(K_pct_Projected_Lower, "%"))
    colnames(future) <- str_replace(colnames(future), "_Projected_Lower", "")
    colnames(future) <- str_replace(colnames(future), "_Projected", "")
    colnames(future) <- str_replace(colnames(future), "_pct", "%")
    colnames(future) <- str_replace(colnames(future), "_plus", "+")
    future <- future %>% rename("BB/K" = "BB_K", "WAR/162" = "WAR_162_G")
    reactable::reactable(future, defaultPageSize = 20,
                         defaultColDef = colDef(
                           footerStyle = list(fontWeight = "bold"),
                           headerStyle = list(background = "#f7f7f8")
                         ),
                         columns = list(Name = colDef(footer = "Quantile Regression"),
                                        Season = colDef(footer = "5th Percentile"),
                                        `BB%` = colDef(format = colFormat(percent = TRUE, digits = 1)),
                                        `K%` = colDef(format = colFormat(percent = TRUE, digits = 1))),
                         bordered = TRUE,
                         highlight = TRUE)
  } else if (quantile == 0.95) {
    future <- future_data %>% 
      filter(Season_Projected == season) %>%
      select(Name, Season_Projected, Age_Projected, G_Projected, AB_Projected_Upper, 
             PA_Projected_Upper, H_Projected_Upper, BB_Projected_Upper, 
             SO_Projected_Upper, BB_pct_Projected_Upper, K_pct_Projected_Upper, 
             BB_K_Projected_Upper, AVG_Projected_Upper, OBP_Projected_Upper, 
             SLG_Projected_Upper, OPS_Projected_Upper, ISO_Projected_Upper, 
             BABIP_Projected_Upper, wOBA_Projected_Upper, wRC_plus_Projected_Upper, 
             WAR_162_G_Projected_Upper) %>% 
      mutate(BB_pct_Projected_Upper = paste0(BB_pct_Projected_Upper, "%"),
             K_pct_Projected_Upper = paste0(K_pct_Projected_Upper, "%"))
    colnames(future) <- str_replace(colnames(future), "_Projected_Upper", "")
    colnames(future) <- str_replace(colnames(future), "_Projected", "")
    colnames(future) <- str_replace(colnames(future), "_pct", "%")
    colnames(future) <- str_replace(colnames(future), "_plus", "+")
    future <- future %>% rename("BB/K" = "BB_K", "WAR/162" = "WAR_162_G")
    reactable::reactable(future, defaultPageSize = 20,
                         defaultColDef = colDef(
                           footerStyle = list(fontWeight = "bold"),
                           headerStyle = list(background = "#f7f7f8")
                         ),
                         columns = list(Name = colDef(footer = "Quantile Regression"),
                                        Season = colDef(footer = "95th Percentile"),
                                        `BB%` = colDef(format = colFormat(percent = TRUE, digits = 1)),
                                        `K%` = colDef(format = colFormat(percent = TRUE, digits = 1))),
                         bordered = TRUE,
                         highlight = TRUE)
  }
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