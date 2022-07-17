set_up_shiny <- function(future_preds_hitters, past_hitting_data, future_preds_pitchers, past_pitching_data)
{
  ui <- fluidPage(
    titlePanel(h1("Ruslan's MLB Player Projections", align = "center", 
                  h3("Method: Quantile Regressions", 
                     align = "center", style = "font-size:16px;"))),
    sidebarLayout(
      sidebarPanel(width = 3, 
                   selectInput("user_output_type", "Output Type", 
                               c("Choose...",
                                 Leaderboards = "user_leaderboards", 
                                 `Player Specific Projections` = "user_player_specific_projections",
                                 `Player Comparison Plot` = "user_player_comparison_plot"
                                )
                              ),
        conditionalPanel(
          condition = "input.user_output_type == 'user_leaderboards'",
          selectInput("user_player_type_leaderboards", "Player Type", choices = c("Choose...","Batter","Pitcher")),
          selectInput("user_season_leaderboards", "Season", choices = c("Choose...",sort(unique(future_preds_hitters$Season_Projected)))),
          selectInput("user_quantiles_leaderboards", "Quantiles", choices = c("Choose...","5th","50th","95th"))
          ),
        conditionalPanel(
          condition = "input.user_output_type == 'user_player_specific_projections'",
          selectInput("user_player_type_player_proj", "Player Type", choices = c("Choose...","Batter","Pitcher")),
          conditionalPanel(condition = "input.user_player_type_player_proj == 'Batter'",
                           selectizeInput("user_name1_input_player_proj","Select Player",  c("Choose...", sort(future_preds_hitters$Name)),
                                          options = list(create = TRUE, createOnBlur = TRUE))
          ),
          conditionalPanel(condition = "input.user_player_type_player_proj == 'Pitcher'",
                           selectizeInput("user_name2_input_player_proj","Select Player",  c("Choose...", sort(future_preds_pitchers$Name)),
                                          options = list(create = TRUE, createOnBlur = TRUE))
          ),
          selectInput("user_display_type_player_proj", "Display Type", choices = c("Choose...","Table","Plot")),
          conditionalPanel(condition = "input.user_display_type_player_proj == 'Table'",
                           selectInput("user_quantiles_player_proj","Quantiles",  choices = c("Choose...","5th","50th","95th"))),
          conditionalPanel(condition = "input.user_display_type_player_proj == 'Plot' & input.user_player_type_player_proj == 'Batter'",
                           selectInput("user_stat1_player_proj","Select Stat",  choices = c("Choose...","PA","AB","H","BB","SO","BB%","K%",
                                                                                           "ISO","BABIP","AVG","OBP","SLG","OPS","wOBA",
                                                                                           "wRC+","WAR/162"))),
          conditionalPanel(condition = "input.user_display_type_player_proj == 'Plot' & input.user_player_type_player_proj == 'Pitcher'",
                           selectInput("user_stat2_player_proj","Select Stat",  choices = c("Choose...","IP","H","BB","SO","K/9","BB/9","K/BB",
                                                                                            "K%","BB%","AVG","WHIP","BABIP","ERA","FIP","xFIP",
                                                                                            "WAR/200IP")))
        ), 
        conditionalPanel(
          condition = "input.user_output_type == 'user_player_comparison_plot'",
          selectInput("user_player_type_player_comp", "Player Type", choices = c("Choose...","Batter","Pitcher")),
          conditionalPanel(condition = "input.user_player_type_player_comp == 'Batter'",
                           selectizeInput("user_name1_input_batter_comp","Select Player 1",  c("Choose...", sort(future_preds_hitters$Name)),
                                          options = list(create = TRUE, createOnBlur = TRUE)),
                           selectizeInput("user_name2_input_batter_comp","Select Player 2",  c("Choose...", sort(future_preds_hitters$Name)),
                                          options = list(create = TRUE, createOnBlur = TRUE)),
                           selectizeInput("user_stat_input_batter_comp","Select Stat",  c("Choose...","PA","AB","H","BB","SO","BB%","K%",
                                                                                          "ISO","BABIP","AVG","OBP","SLG","OPS","wOBA","wRC+","WAR/162"),
                                          options = list(create = TRUE, createOnBlur = TRUE))
          ),
          conditionalPanel(condition = "input.user_player_type_player_comp == 'Pitcher'",
                           selectizeInput("user_name1_input_pitcher_comp","Select Player 1",  c("Choose...", sort(future_preds_pitchers$Name)),
                                          options = list(create = TRUE, createOnBlur = TRUE)),
                           selectizeInput("user_name2_input_pitcher_comp","Select Player 2",  c("Choose...", sort(future_preds_pitchers$Name)),
                                          options = list(create = TRUE, createOnBlur = TRUE)),
                           selectizeInput("user_stat_input_pitcher_comp","Select Stat",  c("Choose...","IP","H","BB","SO","K/9","BB/9","K/BB",
                                                                                           "K%","BB%","AVG","WHIP","BABIP","ERA","FIP","xFIP",
                                                                                           "WAR/200IP"),
                                          options = list(create = TRUE, createOnBlur = TRUE))
                           )
          )
        ), mainPanel(uiOutput('plot'))
    )
  )
  
  
  server <- function(input, output, session) 
  {
    output$plot <- renderUI({
      if (input$user_output_type == "user_leaderboards")
      {
        if (input$user_player_type_leaderboards != "Choose...")
        {
          season <- input$user_season_leaderboards
          quantile <- input$user_quantiles_leaderboards
          if (quantile == "5th")
          {
            quantile <- 0.05
          } else if (quantile == "50th") {
            quantile <- 0.5
          } else if (quantile == "95th") {
            quantile <- 0.95
          }
          if ((input$user_player_type_leaderboards == "Batter") & (season != "Choose...") & (quantile != "Choose..."))
          {
            output$tbl <- renderDT({datatable(print_hitting_projection_leaderboards(season, future_preds_hitters, quantile), rownames= FALSE) %>% DT::formatPercentage(c("BB%","K%"), digits = 1)})
          } else if ((input$user_player_type_leaderboards == "Pitcher") & (season != "Choose...") & (quantile != "Choose...")) {
            output$tbl <- renderDT({datatable(print_pitching_projection_leaderboards(season, future_preds_pitchers, quantile), rownames= FALSE) %>% DT::formatPercentage(c("BB%","K%"), digits = 1)})
          }
        }
      } else if (input$user_output_type == "user_player_specific_projections") {
        
        if (input$user_player_type_player_proj == "Batter")
        {
          if (input$user_name1_input_player_proj != "Choose...")
          {
            batter <- input$user_name1_input_player_proj
            
            if ((input$user_display_type_player_proj == "Table") & (input$user_display_type_player_proj != "Choose...") & (input$user_quantiles_player_proj != "Choose..."))
            {
              quantile <- input$user_quantiles_player_proj
              if (quantile == "5th")
              {
                quantile <- 0.05
              } else if (quantile == "50th") {
                quantile <- 0.5
              } else if (quantile == "95th") {
                quantile <- 0.95
              }
              output$tbl <- renderText({print_hitting_player_projections(batter, quantile, past_hitting_data, future_preds_hitters)})
              htmlOutput("tbl")
              
            } else if ((input$user_display_type_player_proj == "Plot") & (input$user_display_type_player_proj != "Choose...") & (input$user_stat1_player_proj != "Choose...")) {
              stat <- input$user_stat1_player_proj
              if (stat %in% c("K%","BB%"))
              {
                stat <- str_replace(stat, "%", "_pct")
                output$plot1 <- renderPlot({plot_hitting_past_future_performance(batter, past_hitting_data, future_preds_hitters, stat, percent = T)})
                plotOutput("plot1", width = "125%") 
              } else {
                stat <- str_replace(stat, "/162", "_162_G")
                stat <- str_replace(stat, "\\+", "_plus")
                output$plot2 <- renderPlot({plot_hitting_past_future_performance(batter, past_hitting_data, future_preds_hitters, stat, percent = F)})
                plotOutput("plot2", width = "125%", height = "400px") 
              }
            }
          }
        } else if (input$user_player_type_player_proj == "Pitcher") {
          if (input$user_name2_input_player_proj != "Choose...")
          {
            pitcher <- input$user_name2_input_player_proj
            
            if ((input$user_display_type_player_proj == "Table") & (input$user_display_type_player_proj != "Choose...") & (input$user_quantiles_player_proj != "Choose..."))
            {
              quantile <- input$user_quantiles_player_proj
              if (quantile == "5th")
              {
                quantile <- 0.05
              } else if (quantile == "50th") {
                quantile <- 0.5
              } else if (quantile == "95th") {
                quantile <- 0.95
              }
              output$tbl <- renderText({print_pitching_player_projections(pitcher, quantile, past_pitching_data, future_preds_pitchers)})
              htmlOutput("tbl")
              
            } else if ((input$user_display_type_player_proj == "Plot") & (input$user_display_type_player_proj != "Choose...") & (input$user_stat2_player_proj != "Choose...")) {
              stat <- input$user_stat2_player_proj
              if (stat %in% c("K%","BB%"))
              {
                stat <- str_replace(stat, "%", "_pct")
                output$plot1 <- renderPlot({plot_pitching_past_future_performance(pitcher, past_pitching_data, future_preds_pitchers, stat, percent = T)})
                plotOutput("plot1", width = "125%") 
              } else {
                stat <- str_replace(stat, "/162", "_162_G")
                stat <- str_replace(stat, "/9", "_9")
                stat <- str_replace(stat, "\\+", "_plus")
                stat <- str_replace(stat, "K/BB", "K_BB")
                stat <- str_replace(stat, "/200IP", "_200IP")
                output$plot2 <- renderPlot({plot_pitching_past_future_performance(pitcher, past_pitching_data, future_preds_pitchers, stat, percent = F)})
                plotOutput("plot2", width = "125%") 
              }
            }
          }
        }
        
      } else if (input$user_output_type == "user_player_comparison_plot") {
        
        if ((input$user_player_type_player_comp == "Batter"))
        {
          if ((input$user_name1_input_batter_comp != "Choose...") & (input$user_name2_input_batter_comp != "Choose..."))
          {  
            batter1 <- input$user_name1_input_batter_comp
            batter2 <- input$user_name2_input_batter_comp
            if (input$user_stat_input_batter_comp != "Choose...")
            {
              stat <- input$user_stat_input_batter_comp
              if (stat %in% c("K%","BB%"))
              {
                stat <- str_replace(stat, "%", "_pct")
                output$plot1 <- renderPlot({plot_hitting_player_comparison(batter1, batter2, past_hitting_data, future_preds_hitters, stat, percent = T)})
                plotOutput("plot1", width = "125%") 
              } else {
                stat <- str_replace(stat, "/162", "_162_G")
                stat <- str_replace(stat, "\\+", "_plus")
                output$plot2 <- renderPlot({plot_hitting_player_comparison(batter1, batter2, past_hitting_data, future_preds_hitters, stat, percent = F)})
                plotOutput("plot2", width = "125%", height = "400px") 
              }
            }
          }
        } else if ((input$user_player_type_player_comp == "Pitcher"))
        {
          if ((input$user_name1_input_pitcher_comp != "Choose...") & (input$user_name2_input_pitcher_comp != "Choose..."))
          {  
            pitcher1 <- input$user_name1_input_pitcher_comp
            pitcher2 <- input$user_name2_input_pitcher_comp
            if (input$user_stat_input_pitcher_comp != "Choose...")
            {
              stat <- input$user_stat_input_pitcher_comp
              if (stat %in% c("K%","BB%"))
              {
                stat <- str_replace(stat, "%", "_pct")
                output$plot1 <- renderPlot({plot_pitching_player_comparison(pitcher1, pitcher2, past_pitching_data, future_preds_pitchers, stat, percent = T)})
                plotOutput("plot1", width = "125%") 
              } else {
                stat <- str_replace(stat, "/162", "_162_G")
                stat <- str_replace(stat, "/9", "_9")
                stat <- str_replace(stat, "\\+", "_plus")
                stat <- str_replace(stat, "K/BB", "K_BB")
                stat <- str_replace(stat, "/200IP", "_200IP")
                output$plot2 <- renderPlot({plot_pitching_player_comparison(pitcher1, pitcher2, past_pitching_data, future_preds_pitchers, stat, percent = F)})
                plotOutput("plot2", width = "125%", height = "400px") 
              }
            }
          }
        }
      }
    })
  }
  return(list(ui, server))
}