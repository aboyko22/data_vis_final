# load packages ----
library(tidyverse)
library(shiny)
library(bslib)
library(ggpath)
library(gt)
library(glue)
library(ggpubr)

# load data ----
load("data/player_data.rda")
load("data/team_data.rda")
load("data/game_stats.rda")

player_data <- player_data %>% arrange(player)
season_stats <- game_stats %>% select(-c(player, team, conference, home_away, week, color)) %>% colnames()
team_leader_stats <- team_data %>% select(-c(school, conference, mascot, full_name, abbreviation, color,
                                             alt_color, state, latitude, longitude, logo, `Games Played`,
                                             Completions, fourth_downs)) %>% colnames()

percentile_data <- player_data %>%
  mutate(type = case_when(position == "QB" ~ "pass",
                          position %in% c("FB", "RB") ~ "run",
                          position %in% c("WR", "TE") ~ "catch"),
         
         "Passing\nYards" = floor((rank(`Passing Yards`) - 1) / (n() - 1) * 98) + 1,
         "Passing\nTDs" = floor((rank(`Passing TDs`) - 1) / (n() - 1) * 98) + 1,
         "Completion\nPercentage" = floor((rank(`Completion Percentage`) - 1) / (n() - 1) * 98) + 1,
         "Rushing\nYards" = floor((rank(`Rushing Yards`) - 1) / (n() - 1) * 98) + 1,
         "Rushing\nTDs" = floor((rank(`Rushing TDs`) - 1) / (n() - 1) * 98) + 1,
         "Rushing\nYPC" = floor((rank(`Yards Per Carry`) - 1) / (n() - 1) * 98) + 1,
         "Receiving\nYards" = floor((rank(`Receiving Yards`) - 1) / (n() - 1) * 98) + 1,
         "Receiving\nTds" = floor((rank(`Receiving TDs`) - 1) / (n() - 1) * 98) + 1,
         "Receiving\nYPC" = floor((rank(`Yards Per Catch`) - 1) / (n() - 1) * 98) + 1) %>%
  pivot_longer(cols = c("Passing\nYards", "Passing\nTDs", "Completion\nPercentage",
                        "Rushing\nYards", "Rushing\nTDs", "Rushing\nYPC",
                        "Receiving\nYards", "Receiving\nTds", "Receiving\nYPC"), names_to = "percentile") %>%
  mutate(label = case_when(
    value %% 10 == 1 & value %% 100 != 11 ~ str_c(value, "st"),
    value %% 10 == 2 & value %% 100 != 12 ~ str_c(value, "nd"),
    value %% 10 == 3 & value %% 100 != 13 ~ str_c(value, "rd"),
    TRUE ~ str_c(value, "th")))

# ui ----
ui <- page_navbar(
  title = "The Low Down",
  tags$head(tags$link(rel = "shortcut icon", 
                      href = "https://img.icons8.com/?size=96&id=oo2kVzvjMEC4&format=png")),
  tags$style(HTML(".custom-card {--bs-card-spacer-y: 0em; padding-top: 10px;}")),
  theme = bs_theme(bootswatch = "lux"),
  
  # player info ----
  nav_panel(title = "Player Info", icon = icon(name = "football", lib = "font-awesome"),
    
            sidebarLayout(
              sidebarPanel(
                
                selectInput(
                  inputId = "Option1",
                  label = "Choose a player:",
                  choices = unique(player_data$player),
                  selectize = TRUE),
                
                selectInput(
                  inputId = "Option2",
                  label = "Choose a statistic:",
                  choices = NULL,
                  selectize = TRUE)
              ),
              
              mainPanel(
                # Top row with headshot table and donut plot
                fluidRow(
                  column(
                    width = 6,
                    card(class = "custom-card",
                      gt_output(outputId = "headshot"),
                      style = "height: 230px;",
                    )
                  ),
                  column(
                    width = 6,
                    card(class = "custom-card",
                      plotOutput(outputId = "donut"),
                      style = "height: 230px;"
                    )
                  )
                ),
                # Bottom row with the line chart
                fluidRow(
                  column(
                    width = 12,
                    card(
                      plotOutput(outputId = "linechart"),
                      style = "height: 437px;"
                    )
                  )
                )
              )
            )
  ),
                  
                    
  # team info ----
  nav_panel(title = "Team Info", icon = icon(name = "people-group", lib = "font-awesome"),
            sidebarLayout(
              sidebarPanel(
                
                selectInput(
                  inputId = "Option3",
                  label = "Select a team:",
                  choices = unique(team_data$school),
                  selected = "Air Force",
                  selectize = TRUE),

                card(class = "custom-card",
                  tableOutput(outputId = "team_table"),
                  style = "height: 535px;")
                ),
              
              mainPanel(
                card(h4("Team Strengths"),
                plotOutput(outputId = "plot1"),
                style = "height: 683px;")
          )
        )
  ),
  
  # compare tool ----
  nav_panel(title = "Compare", icon = icon(name = "ranking-star", lib = "font-awesome"),
            sidebarLayout(
              sidebarPanel(
                
                selectInput(
                  inputId = "Option5",
                  label = "Select a statistic:",
                  choices = team_leader_stats,
                  selectize = TRUE)
                
                ,
                # 
                # selectInput(
                #   inputId = "Option6",
                #   label = "Select a player:",
                #   choices = c("Player 1", "Player 2", "Player 3"),
                #   selected = "Player 1",
                #   selectize = TRUE)
                # 
                ),
              
              mainPanel(card(plotOutput(outputId = "plot2", height = 650)
            )
          )
        )
  ),
  
  nav_spacer(),
  nav_panel(title = "About", icon = icon(name = "circle-info", lib = "font-awesome"))
)

# server ----
server <- function(input, output, session) {
  
  observeEvent(input$Option1, {
    req(input$Option1) # Ensure a selection is made
    
    # Filter dataset to get the position of the selected player
    selected_position <- player_data %>%
      filter(player == input$Option1) %>%
      pull(position) %>%
      unique()
    
    # Define choices based on the position
    position_choices <- switch(selected_position,
                               "QB" = c("Passing Yards", "Passing Touchdowns", "Completion Percentage", "Quarterback Rating"),
                               "RB" = c("Rushing Yards", "Rushing Touchdowns", "Carries", "Yards per Carry"),
                               "FB" = c("Carries", "Rushing Yards", "Rushing Touchdowns", "Yards per Carry"),
                               "WR" = c("Receptions", "Receiving Yards", "Receiving Touchdowns", "Yards per Catch"),
                               "TE" = c("Receptions", "Receiving Yards", "Receiving Touchdowns", "Yards per Catch"),
                               NULL)
    
    # Update the second selectInput
    updateSelectInput(
      session,
      inputId = "Option2",
      choices = position_choices
    )
  })
  
  output$table <- renderTable({
    team_data %>%
      filter(conference == "Big Ten") %>%
      select(full_name, `Total Yards`)})
  
  output$headshot <- render_gt({
    player_data %>%
      filter(player == input$Option1) %>%
      mutate(info = str_c(position, " •  #", jersey)) %>%
      select(headshot_url, player, info) %>%
      gt() %>%
      text_transform(
        locations = cells_body(columns = headshot_url),
        fn = function(x) {
          glue(
            '<div style="position: relative; width: 220px; height: 160px;">
           <img src="{player_data$logo[which(player_data$headshot_url == x)]}" 
                style="position: absolute; width: 220px; height: 160px; opacity: 0.2;">
           <img src="{player_data$headshot_url[which(player_data$headshot_url == x)]}" 
                style="position: absolute; width: 220px; height: 160px;">
         </div>'
          )}) %>%
      cols_merge(columns = c(player, info), pattern = "{1}<br>{2}") %>%
      cols_label(player = "", headshot_url = "") %>%
      tab_style(style = list(cell_text(size = px(18), weight = "bold", align = "center")),
        locations = cells_body(columns = player)) %>%
      cols_width(headshot_url ~ px(260), player ~ px(150))
  })
  
  output$donut <- renderPlot({
    
    percentile_data %>%
      filter(player == input$Option1,
            if_else(type == "pass", percentile %in% c("Passing\nYards", "Passing\nTDs", "Completion\nPercentage"),
                    if_else(type == "run", percentile %in% c("Rushing\nYards", "Rushing\nTDs", "Rushing\nYPC"),
                            percentile %in% c("Receiving\nYards", "Receiving\nTds", "Receiving\nYPC")))) %>%
      ggplot(aes(x = 2)) +
      geom_bar(aes(y = value, fill = value), stat = "identity", width = 1) +
      geom_label(aes(label = label), x = 0.5, y = 0, fontface = "bold", size = 4) +
      scale_y_continuous(limits = c(0, 99)) +
      scale_fill_gradientn(colors = c("red2", "yellow2", "green2"),
                           values = c(0, 0.6, 1),
                           limits = c(1, 99)) +
      coord_polar(theta = "y") +
      xlim(0.5, 2.5) +
      facet_wrap(~percentile) +
      theme_void() +
      theme(panel.grid = element_blank(),
            panel.background = element_blank(),
            legend.position = "none",
            strip.text = element_text(face = "bold", size = 18, hjust = 0.5, vjust = 1)) +
      labs(caption = "Percentile among qualified FBS players")
  })
  
  output$linechart <- renderPlot({
    
    if (endsWith(input$Option2, "Touchdowns")) {
     game_stats %>%
        filter(player == input$Option1) %>%
        mutate(cumulative_td = cumsum(!!sym(input$Option2))) %>%
        ggplot(aes(x = as_factor(week), y = cumulative_td, group = player, color = color)) +
        geom_col(aes(fill = color), alpha = 0.75) +
        geom_line() +
        geom_label(aes(label = cumulative_td)) +
        scale_fill_identity() +
        scale_color_identity() +
        labs(x = NULL, y = NULL, title = str_c(input$Option1, "'s Cumulative ", input$Option2, " This Season")) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5))
    }
    
    else {
    
    game_stats %>%
      filter(player == input$Option1) %>%
      ggplot(aes(x = as_factor(week), y = !!sym(input$Option2), group = player, color = color)) +
      geom_col(aes(fill = color), alpha = 0.75) +
      geom_line() +
      geom_label(aes(label = !!sym(input$Option2))) +
      scale_fill_identity() +
      scale_color_identity() +
      labs(x = NULL, y = NULL, title = str_c(input$Option1, "'s ", input$Option2, " This Season")) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5))
    }
    
  })
  
  output$team_table <- render_gt({
    
    filtered_data <- team_data %>%
      mutate(`Completion Percentage` = str_c(round(100 * Completions / `Pass Attempts`, digits = 1), "%"),
             `Yards Per Carry` = round(`Rushing Yards` / `Rush Attempts`, digits = 2),
             `3rd Down Conversion` = paste0(`3rd Down Conversion`, "%"),
             `4th Down Conversion` = paste0(`4th Down Conversion`, "%")) %>%
      filter(school == input$Option3)
    
    header_title <- filtered_data$full_name
    header_fill <- filtered_data$color
    header_color <- filtered_data$alt_color
    
    filtered_data %>%
      select(`Passing Yards`, `Passing TDs`, `Completion Percentage`,
             `Rushing Yards`, `Rushing TDs`, `Yards Per Carry`,
             Turnovers, `3rd Down Conversion`, `4th Down Conversion`) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = everything(), names_to = "stat") %>%
      mutate(category = case_when(stat %in% c("Passing Yards", "Passing TDs", "Completion Percentage") ~ "In the Air",
                                  stat %in% c("Rushing Yards", "Rushing TDs", "Yards Per Carry") ~ "On the Ground",
                                  .default = "Miscellaneous")) %>%
      gt(groupname_col = "category") %>%
      tab_header(title = header_title, subtitle = "As of 12/5/2024") %>%
      tab_style(locations = cells_title(), cell_fill(header_fill)) %>%
      tab_style(locations = cells_title(), cell_text(color = header_color, weight = "bold")) %>%
      tab_style(locations = cells_row_groups(), style = list(cell_text(align = "center", weight = "bold"))) %>%
      tab_options(table.width = "350px", column_labels.hidden = TRUE)
  })
  
  output$plot2 <- renderPlot({
    team_data %>%
      slice_max(order_by = !!sym(input$Option5), n = 10) %>%
      ggplot(aes(x = reorder(school, -!!sym(input$Option5)), y = !!sym(input$Option5))) +
      geom_col(aes(fill = color, color = alt_color), alpha = 0.85) +
      geom_from_path(aes(path = logo), height = 0.1) +
      scale_fill_identity() +
      scale_color_identity() +
      labs(x = NULL, y = NULL) +
      theme_minimal()
  })
  
  output$plot1 <- renderPlot({
    
    percent_labels <- tibble(
      x = rep(0.5, 4),
      y = c(12.5, 37.5, 62.5, 87.5),
      label = c("1st-25th", "25th-50th", "50th-75th", "75th-99th"))
    
    team_data %>%
      mutate(pass_yd_percentile = floor((rank(`Passing Yards`) - 1) / (n() - 1) * 98) + 1,
             pass_td_percentile = floor((rank(`Passing TDs`) - 1) / (n() - 1) * 98) + 1,
             pass_ypa_percentile = floor((rank(`Passing Yards` / `Pass Attempts`) - 1) / (n() - 1) * 98) + 1,
             rush_yd_percentile = floor((rank(`Rushing Yards`) - 1) / (n() - 1) * 98) + 1,
             rush_td_percentile = floor((rank(`Rushing TDs`) - 1) / (n() - 1) * 98) + 1,
             rush_ypa_percentile = floor((rank(`Rushing Yards` / `Rush Attempts`) - 1) / (n() - 1) * 98) + 1
      ) %>%
      select(school, color, logo, alt_color, ends_with("percentile")) %>%
      filter(school == input$Option3) %>%
      pivot_longer(cols = ends_with("percentile"), names_to = "category") %>%
      ggplot(aes(x = category, y = value)) +
      geom_col(aes(fill = color, group = school), width = 1, alpha = 0.9) +
      geom_hline(yintercept = c(25, 50, 75, 99), color = "black", lty = 3) +
      geom_segment(x = 0.5:5.5, y = 0, xend = 0.5:5.5, yend = 99, color = "black") +
      annotate(geom = "text",
               label = c("Passing\nYards", "Passing\nTDs", "Passing\nEfficiency",
                         "Rushing\nEfficiency", "Rushing\nTDs", "Rushing\nYards"),
               x = seq(1:6), y = 120, size = 4) +
      geom_label(data = percent_labels, aes(x = x, y = y, label = label)) +
      coord_polar() +
      scale_x_discrete(limits = c("pass_yd_percentile", "pass_td_percentile",
                                  "pass_ypa_percentile", "rush_ypa_percentile", "rush_td_percentile", "rush_yd_percentile"),
                       expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_fill_identity() +
      scale_color_identity() +
      theme_void()
    
  })}

# run the app ----
shinyApp(ui = ui, server = server)
