future_preds_hitters <- read.csv("https://raw.githubusercontent.com/rdavtian/MLB-Player-Projections/master/Hitting/Data/hitting_projections_data.csv", fileEncoding = 'UTF-8-BOM')
future_preds_pitchers <- read.csv("https://raw.githubusercontent.com/rdavtian/MLB-Player-Projections/master/Pitching/Data/pitching_projections_data.csv", fileEncoding = 'UTF-8-BOM')
current_season <- 2022
#current_season <- as.numeric(substr(Sys.Date(), 1, 4))

plot_hitting_past_future_performance <- function(player, past_data, future_data, stat, percent = F)
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

plot_hitting_player_comparison <- function(player1, player2, past_data, future_data, stat, percent = F)
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
    facet_grid(~ Name, scales = "free") + 
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

print_hitting_player_projections <- function(player, quantile, past_data, future_data)
{
  
  past <- past_data %>% 
    filter(Name == player) %>% 
    select(Season, Age, G, AB, PA, H, BB, SO, BB_pct, K_pct, BB_K, AVG, OBP, 
           SLG, OPS, ISO, BABIP, wOBA, wRC_plus, WAR_162_G) %>%
    mutate(BB_pct = paste0(BB_pct, "%"), K_pct = paste0(K_pct, "%")) %>%
    rename("BB%" = "BB_pct", "K%" = "K_pct", "BB/K" = "BB_K", "wRC+" = "wRC_plus",
           "WAR/162" = "WAR_162_G") %>% arrange(Season)
  
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
    kable(tab %>% arrange(Season), rownames = F) %>% 
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
    kable(tab %>% arrange(Season), rownames = F) %>% 
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
    kable(tab %>% arrange(Season), rownames = F) %>% 
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                    full_width = F, position = "center", fixed_thead = T) %>% 
      row_spec(which(tab$Season > current_season), bold = T, color = "black", background = "cyan") %>% 
      footnote(symbol = paste0(player, " 95th Percentile Projections"))
  }
}

print_hitting_projection_leaderboards <- function(season, future_data, quantile)
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
    #reactable::reactable(future, defaultPageSize = 20,
                         #defaultColDef = colDef(
                           #footerStyle = list(fontWeight = "bold"),
                           #headerStyle = list(background = "#f7f7f8")
                         #),
                         #columns = list(Name = colDef(footer = "Quantile Regression"),
                                        #Season = colDef(footer = "50th Percentile"),
                                        #`BB%` = colDef(format = colFormat(percent = TRUE, digits = 1)),
                                        #`K%` = colDef(format = colFormat(percent = TRUE, digits = 1))),
                         #bordered = TRUE,
                         #highlight = TRUE)
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
      mutate(BB_pct_Projected_Lower = BB_pct_Projected_Lower / 100,
             K_pct_Projected_Lower = K_pct_Projected_Lower / 100)
    colnames(future) <- str_replace(colnames(future), "_Projected_Lower", "")
    colnames(future) <- str_replace(colnames(future), "_Projected", "")
    colnames(future) <- str_replace(colnames(future), "_pct", "%")
    colnames(future) <- str_replace(colnames(future), "_plus", "+")
    future <- future %>% rename("BB/K" = "BB_K", "WAR/162" = "WAR_162_G")
    #reactable::reactable(future, defaultPageSize = 20,
                         #defaultColDef = colDef(
                           #footerStyle = list(fontWeight = "bold"),
                           #headerStyle = list(background = "#f7f7f8")
                         #),
                         #columns = list(Name = colDef(footer = "Quantile Regression"),
                                        #Season = colDef(footer = "5th Percentile"),
                                        #`BB%` = colDef(format = colFormat(percent = TRUE, digits = 1)),
                                        #`K%` = colDef(format = colFormat(percent = TRUE, digits = 1))),
                         #bordered = TRUE,
                         #highlight = TRUE)
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
      mutate(BB_pct_Projected_Upper = BB_pct_Projected_Upper / 100,
             K_pct_Projected_Upper = K_pct_Projected_Upper / 100)
    colnames(future) <- str_replace(colnames(future), "_Projected_Upper", "")
    colnames(future) <- str_replace(colnames(future), "_Projected", "")
    colnames(future) <- str_replace(colnames(future), "_pct", "%")
    colnames(future) <- str_replace(colnames(future), "_plus", "+")
    future <- future %>% rename("BB/K" = "BB_K", "WAR/162" = "WAR_162_G")
    #reactable::reactable(future, defaultPageSize = 20,
                         #defaultColDef = colDef(
                           #footerStyle = list(fontWeight = "bold"),
                           #headerStyle = list(background = "#f7f7f8")
                         #),
                         #columns = list(Name = colDef(footer = "Quantile Regression"),
                                        #Season = colDef(footer = "95th Percentile"),
                                        #`BB%` = colDef(format = colFormat(percent = TRUE, digits = 1)),
                                        #`K%` = colDef(format = colFormat(percent = TRUE, digits = 1))),
                         #bordered = TRUE,
                         #highlight = TRUE)
  }
  return(future)
}

plot_pitching_past_future_performance <- function(player, past_data, future_data, stat, percent = F)
{
  stat2 <- str_replace(stat, "_pct", "%")
  stat2 <- str_replace(stat2, "_plus", "+")
  stat2 <- str_replace(stat2, "_162_G", " Per 162 GP")
  stat2 <- str_replace(stat2, "_9", "/9")
  stat2 <- str_replace(stat2, "K_BB", "K/BB")
  stat2 <- str_replace(stat2, "_200IP", " Per 200 IP")
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

plot_pitching_player_comparison <- function(player1, player2, past_data, future_data, stat, percent = F)
{
  stat2 <- str_replace(stat, "_pct", "%")
  stat2 <- str_replace(stat2, "_plus", "+")
  stat2 <- str_replace(stat2, "_162_G", " Per 162 GP")
  stat2 <- str_replace(stat2, "_9", "/9")
  stat2 <- str_replace(stat2, "K_BB", "K/BB")
  stat2 <- str_replace(stat2, "_200IP", " Per 200 IP")
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
    facet_grid(~ Name, scales = "free") + 
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

print_pitching_player_projections <- function(player, quantile, past_data, future_data)
{
  
  past <- past_data %>% 
    filter(Name == player) %>% 
    select(Season, Age, G, GS, IP, H, BB, SO, K_9, BB_9, K_BB, K_pct, BB_pct, 
           AVG, WHIP, BABIP, ERA, FIP, xFIP, WAR_200IP) %>%
    mutate(BB_pct = paste0(BB_pct, "%"), K_pct = paste0(K_pct, "%")) %>%
    rename("K/9" = "K_9", "BB/9" = "BB_9", "K/BB" = "K_BB",
           "BB%" = "BB_pct", "K%" = "K_pct", "WAR/200IP" = "WAR_200IP") %>% arrange(Season)
  
  if (quantile == 0.5)
  {
    future <- future_data %>% 
      filter(Name == player) %>%
      select(Season_Projected, Age_Projected, G_Projected, GS_Projected, 
             IP_Projected, H_Projected, BB_Projected, SO_Projected, K_9_Projected, 
             BB_9_Projected, K_BB_Projected, K_pct_Projected, BB_pct_Projected,
             AVG_Projected, WHIP_Projected, BABIP_Projected, ERA_Projected, FIP_Projected, 
             xFIP_Projected, WAR_200IP_Projected) %>%
    mutate(BB_pct_Projected = paste0(BB_pct_Projected, "%"),
           K_pct_Projected = paste0(K_pct_Projected, "%"))
    colnames(future) <- str_replace(colnames(future), "_Projected", "")
    colnames(future) <- str_replace(colnames(future), "_pct", "%")
    colnames(future) <- str_replace(colnames(future), "_plus", "+")
    colnames(future) <- str_replace(colnames(future), "_9", "/9")
    future <- future %>% rename("K/BB" = "K_BB", "WAR/200IP" = "WAR_200IP")
    tab <- rbind(past, future)
    kable(tab %>% arrange(Season), rownames = F) %>% 
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                    full_width = F, position = "center", fixed_thead = T) %>% 
      row_spec(which(tab$Season > current_season), bold = T, color = "black", background = "cyan") %>% 
      footnote(symbol = paste0(player, " 50th Percentile (Median) Projections"))
    
  } else if (quantile == 0.05) {
    future <- future_data %>% 
      filter(Name == player) %>%
      select(Season_Projected, Age_Projected, G_Projected, GS_Projected, 
             IP_Projected_Lower, H_Projected_Lower, BB_Projected_Lower,
             SO_Projected_Lower, K_9_Projected_Lower, BB_9_Projected_Lower, 
             K_BB_Projected_Lower, K_pct_Projected_Lower, BB_pct_Projected_Lower,
             AVG_Projected_Lower, WHIP_Projected_Lower, BABIP_Projected_Lower, 
             ERA_Projected_Lower, FIP_Projected_Lower, xFIP_Projected_Lower, 
             WAR_200IP_Projected_Lower) %>% 
    mutate(BB_pct_Projected_Lower = paste0(BB_pct_Projected_Lower, "%"),
           K_pct_Projected_Lower = paste0(K_pct_Projected_Lower, "%"))
    colnames(future) <- str_replace(colnames(future), "_Projected_Lower", "")
    colnames(future) <- str_replace(colnames(future), "_Projected", "")
    colnames(future) <- str_replace(colnames(future), "_pct", "%")
    colnames(future) <- str_replace(colnames(future), "_plus", "+")
    colnames(future) <- str_replace(colnames(future), "_9", "/9")
    future <- future %>% rename("K/BB" = "K_BB", "WAR/200IP" = "WAR_200IP")
    tab <- rbind(past, future)
    kable(tab %>% arrange(Season), rownames = F) %>% 
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                    full_width = F, position = "center", fixed_thead = T) %>%
      row_spec(which(tab$Season > current_season), bold = T, color = "black", background = "cyan") %>% 
      footnote(symbol = paste0(player, " 5th Percentile Projections"))
  } else if (quantile == 0.95) {
    future <- future_data %>% 
      filter(Name == player) %>%
      select(Season_Projected, Age_Projected, G_Projected, GS_Projected, 
             IP_Projected_Upper, H_Projected_Upper, BB_Projected_Upper,
             SO_Projected_Upper, K_9_Projected_Upper, BB_9_Projected_Upper, 
             K_BB_Projected_Upper, K_pct_Projected_Upper, BB_pct_Projected_Upper,
             AVG_Projected_Upper, WHIP_Projected_Upper, BABIP_Projected_Upper, 
             ERA_Projected_Upper, FIP_Projected_Upper, xFIP_Projected_Upper,
             WAR_200IP_Projected_Upper) %>% 
    mutate(BB_pct_Projected_Upper = paste0(BB_pct_Projected_Upper, "%"),
           K_pct_Projected_Upper = paste0(K_pct_Projected_Upper, "%"))
    colnames(future) <- str_replace(colnames(future), "_Projected_Upper", "")
    colnames(future) <- str_replace(colnames(future), "_Projected", "")
    colnames(future) <- str_replace(colnames(future), "_pct", "%")
    colnames(future) <- str_replace(colnames(future), "_plus", "+")
    colnames(future) <- str_replace(colnames(future), "_9", "/9")
    future <- future %>% rename("K/BB" = "K_BB", "WAR/200IP" = "WAR_200IP")
    tab <- rbind(past, future)
    kable(tab %>% arrange(Season), rownames = F) %>% 
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                    full_width = F, position = "center", fixed_thead = T) %>% 
      row_spec(which(tab$Season > current_season), bold = T, color = "black", background = "cyan") %>% 
      footnote(symbol = paste0(player, " 95th Percentile Projections"))
  }
}

print_pitching_projection_leaderboards <- function(season, future_data, quantile)
{
  if (quantile == 0.5)
  {
    future <- future_data %>% 
      filter(Season_Projected == season) %>%
      select(Name, Season_Projected, Age_Projected, G_Projected, GS_Projected, 
             IP_Projected, H_Projected, BB_Projected, SO_Projected, K_9_Projected, 
             BB_9_Projected, K_BB_Projected, K_pct_Projected, BB_pct_Projected,
             AVG_Projected, WHIP_Projected, BABIP_Projected, ERA_Projected, 
             FIP_Projected, xFIP_Projected, WAR_200IP_Projected) %>%
      mutate(BB_pct_Projected = BB_pct_Projected / 100,
             K_pct_Projected = K_pct_Projected / 100)
    colnames(future) <- str_replace(colnames(future), "_Projected", "")
    colnames(future) <- str_replace(colnames(future), "_pct", "%")
    colnames(future) <- str_replace(colnames(future), "_plus", "+")
    colnames(future) <- str_replace(colnames(future), "_9", "/9")
    future <- future %>% rename("K/BB" = "K_BB", "WAR/200IP" = "WAR_200IP")
  }  else if (quantile == 0.05) {
    future <- future_data %>% 
      filter(Season_Projected == season) %>%
      select(Name, Season_Projected, Age_Projected, G_Projected, GS_Projected, 
             IP_Projected_Lower, H_Projected_Lower, BB_Projected_Lower,
             SO_Projected_Lower, K_9_Projected_Lower, BB_9_Projected_Lower, 
             K_BB_Projected_Lower, K_pct_Projected_Lower, BB_pct_Projected_Lower,
             AVG_Projected_Lower, WHIP_Projected_Lower, BABIP_Projected_Lower, 
             ERA_Projected_Lower, FIP_Projected_Lower, xFIP_Projected_Lower, 
             WAR_200IP_Projected_Lower) %>% 
      mutate(BB_pct_Projected_Lower = BB_pct_Projected_Lower / 100,
             K_pct_Projected_Lower = K_pct_Projected_Lower / 100)
    colnames(future) <- str_replace(colnames(future), "_Projected_Lower", "")
    colnames(future) <- str_replace(colnames(future), "_Projected", "")
    colnames(future) <- str_replace(colnames(future), "_pct", "%")
    colnames(future) <- str_replace(colnames(future), "_plus", "+")
    colnames(future) <- str_replace(colnames(future), "_9", "/9")
    future <- future %>% rename("K/BB" = "K_BB", "WAR/200IP" = "WAR_200IP")
  } else if (quantile == 0.95) {
    future <- future_data %>% 
      filter(Season_Projected == season) %>%
      select(Name, Season_Projected, Age_Projected, G_Projected, GS_Projected, 
             IP_Projected_Upper, H_Projected_Upper, BB_Projected_Upper,
             SO_Projected_Upper, K_9_Projected_Upper, BB_9_Projected_Upper, 
             K_BB_Projected_Upper, K_pct_Projected_Upper, BB_pct_Projected_Upper,
             AVG_Projected_Upper, WHIP_Projected_Upper, BABIP_Projected_Upper, 
             ERA_Projected_Upper, FIP_Projected_Upper, xFIP_Projected_Upper, 
             WAR_200IP_Projected_Upper) %>% 
      mutate(BB_pct_Projected_Upper = BB_pct_Projected_Upper / 100,
             K_pct_Projected_Upper = K_pct_Projected_Upper / 100)
    colnames(future) <- str_replace(colnames(future), "_Projected_Upper", "")
    colnames(future) <- str_replace(colnames(future), "_Projected", "")
    colnames(future) <- str_replace(colnames(future), "_pct", "%")
    colnames(future) <- str_replace(colnames(future), "_plus", "+")
    colnames(future) <- str_replace(colnames(future), "_9", "/9")
    future <- future %>% rename("K/BB" = "K_BB", "WAR/200IP" = "WAR_200IP")
  }
  return(future)
}
