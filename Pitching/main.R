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
  mutate(Pos_Group_Current = case_when(Start.IP > Relief.IP ~ 'SP',
                                Start.IP <= Relief.IP ~ 'RP'))

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
                                       Pos %in% c('SP','RP','P') ~ 'P')) %>% 
  select(-Pos) %>% filter(Pos_Group_Current == 'P')
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

  

