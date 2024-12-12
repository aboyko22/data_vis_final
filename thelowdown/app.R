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
load("data/percentile_data.rda")

valid_players <- sort(intersect(unique(game_stats$player), intersect(unique(player_data$player), unique(percentile_data$player))))
team_leader_stats <- team_data %>% select(-c(school, mascot, full_name, abbreviation, conference,color, alt_color, state,
                                             latitude, longitude, logo, `Games Played`, Completions, fourth_downs)) %>% colnames()
player_leader_stats <- player_data %>% select(-c(player, position, team, weight, height, jersey, headshot_url, Completions,
                                                 conference, color, alt_color, logo, full_name)) %>% colnames()

# ui ----
ui <- page_navbar(
  title = "The Low Down",
  tags$head(tags$link(rel = "shortcut icon", 
                      href = "https://img.icons8.com/?size=96&id=oo2kVzvjMEC4&format=png")),
  tags$style(HTML(".custom-card {--bs-card-spacer-y: 0em; padding-top: 10px;}
                  .sidebar {height: 97.7%;}")),
  theme = bs_theme(bootswatch = "lux"),
  
  # player info ----
  nav_panel(title = "Player Info", icon = icon(name = "football", lib = "font-awesome"),
    
            sidebarLayout(
              sidebarPanel(class = "sidebar",
                
                card(style = "background-color: transparent;
                              border-color: transparent;
                              height: 85%;",
                  selectInput(
                  inputId = "Option1",
                  label = "Choose a player:",
                  choices = valid_players,
                  selectize = TRUE),
                
                selectInput(
                  inputId = "Option2",
                  label = "Choose a statistic:",
                  choices = NULL,
                  selectize = TRUE)),
                
                card_footer("To qualify, players were required to have either 20 pass attempts, 20 carries, or 10 receptions this season.")
                
                ),
              
              mainPanel(
                # Top row with headshot table and donut plot
                fluidRow(
                  column(
                    width = 6,
                    card(class = "custom-card",
                      gt_output(outputId = "headshot"),
                      style = "height: 230px;"
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
                    card(full_screen = TRUE, h4("Season Stats"),
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
              sidebarPanel(class = "sidebar",
                
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
  
  # season leaders ----
  nav_panel(title = "Leaderboard", icon = icon(name = "ranking-star", lib = "font-awesome"),
            sidebarLayout(
              sidebarPanel(class = "sidebar",
                           
                card(style = "background-color: transparent;
                              border-color: transparent;
                              height: 85%;",
                  selectInput(
                    inputId = "Option6",
                    label = "Team or Player:",
                    choices = c("Team", "Player"),
                    selectize = TRUE),
                                
                    selectInput(
                    inputId = "Option5",
                    label = "Select a statistic:",
                    choices = NULL,
                    selectize = TRUE)),
                           
                    card_footer("To qualify, players were required to have either 20 pass attempts, 20 carries, or 10 receptions this season.")),
                          
              mainPanel(
                card(full_screen = TRUE,
                  plotOutput(outputId = "plot2", height = 649),
                  
            )
          )
        )
  ),
  
  nav_spacer(),
  
  # about page ----
  nav_panel(title = "About", icon = icon(name = "circle-info", lib = "font-awesome"),
            
            fluidRow(
              column(
                width = 4,
                card(style = "height: 683px;",
                     h4("The Low Down"),
                     
                     p("This dashboard was created as a final project for the Stat 302 class at Northwestern University."),
                     p("Its intent is to provide information about a wide range of players and teams in a quick 
                     and understandable manner."),
                     p("You can access information about over 1,000 individual players, from all 138 FBS teams. Do you 
                       want to learn more about your favorite players? The Heisman favorites? 2nd stringers from a team you've
                       never watched? The choice is yours."),
                     p("For more information about the visualizations used in each tab, please consult the dropdown options
                        in the card beside this.")
                )
              ),
              
              # Second column: Two cards stacked on the right
              column(
                width = 8,
                fluidRow(
                  # First stacked card
                  column(width = 12,
                    card(height = "417px",
                         
                         h4("Visualizations"),
                         
                         tags$div(class = "accordion",
                                  tags$div(class = "accordion-item",
                                           tags$h2(class = "accordion-header", id = "headingOne",
                                                   tags$button(class = "accordion-button collapsed", type = "button",
                                                               `data-bs-toggle` = "collapse", `data-bs-target` = "#collapseOne", 
                                                               `aria-expanded` = "false", `aria-controls` = "collapseOne",
                                                               "Player Info")
                                           ),
                                           tags$div(id = "collapseOne", class = "accordion-collapse collapse", 
                                                    `aria-labelledby` = "headingOne", `data-bs-parent` = "#accordionExample",
                                                    tags$div(class = "accordion-body",
                                                             "The Player Info tab showcase a profile of a player's stats through the season. The widget 
                                                             in the top left allows you to search for a desired player. The rings in the top right represent 
                                                             percentile rankings for metrics related to their positon. The bigger the ring, the better they 
                                                             are compared to their peers. Below that is a chart showing how well they've performed in each 
                                                             game this season. Use the second widget in the left to cycle through different statistic, and get
                                                             an overview on their season so far."
                                                    )
                                           )
                                  ),
                                  
                                  tags$div(class = "accordion-item",
                                           tags$h2(class = "accordion-header", id = "headingTwo",
                                                   tags$button(class = "accordion-button collapsed", type = "button",
                                                               `data-bs-toggle` = "collapse", `data-bs-target` = "#collapseTwo", 
                                                               `aria-expanded` = "false", `aria-controls` = "collapseTwo",
                                                               "Team Info")
                                           ),
                                           tags$div(id = "collapseTwo", class = "accordion-collapse collapse", 
                                                    `aria-labelledby` = "headingTwo", `data-bs-parent` = "#accordionExample",
                                                    tags$div(class = "accordion-body",
                                                             "The Team Info tab a quick overview of the offenses for each FBS team. 
                                                             In the top left, there is a widget that allows you choose a team to look at their season stats. Directly 
                                                             below, there is a formatted table to directly display this information. To the right, there is a chart 
                                                             that displays in 6 relevant offensive metrics. The larger a wedge, the better the team is at it. For a quick glance, 
                                                             look down the middle. Everything to the left is rushing and to the right is passing."
                                                    )
                                           )
                                  ),
                                  
                                  tags$div(class = "accordion-item",
                                           tags$h2(class = "accordion-header", id = "headingThree",
                                                   tags$button(class = "accordion-button collapsed", type = "button",
                                                               `data-bs-toggle` = "collapse", `data-bs-target` = "#collapseThree", 
                                                               `aria-expanded` = "false", `aria-controls` = "collapseThree",
                                                               "Leaderboard")
                                           ),
                                           tags$div(id = "collapseThree", class = "accordion-collapse collapse", 
                                                    `aria-labelledby` = "headingThree", `data-bs-parent` = "#accordionExample",
                                                    tags$div(class = "accordion-body",
                                                             "The Leaderboard is a great tool to see who's on top, whether it be for individual 
                                                             players or at the team level. Use the top widget once you've decided which to check 
                                                             out first. Afterwards, the second one let's you pick a new leaderboard to see. Keep an eye on 
                                                             these names and faces throughout the season. College football is a wild game, they may not 
                                                             be here forever."
                                                    )
                                           )
                                  )
                         )
                    )
                    ),
                  
                  # Second stacked card
                  column(width = 12,
                    card(height = "250px",
                         h4("Data"),
                         p("All data used in this dashbord was obtained from the ",
                           a("cfbfastR", href = "https://cfbfastr.sportsdataverse.org/", target = "_blank"), " package.
                           The code used to create this dashboard is open-source and can be found on my ", 
                           a("GitHub", href = "https://github.com/aboyko22/data_vis_final", target = "_blank"), " page.")
                    )
                  )
                )
              )
            )
  )
)

# server ----
server <- function(input, output, session) {
  
  observeEvent(input$Option1, {
    req(input$Option1)

    selected_position <- player_data %>%
      filter(player == input$Option1) %>%
      pull(position) %>%
      unique()

    position_choices <- switch(selected_position,
                               "QB" = c("Passing Yards", "Passing Touchdowns", "Completion Percentage", "Quarterback Rating"),
                               "RB" = c("Rushing Yards", "Rushing Touchdowns", "Carries", "Yards per Carry"),
                               "FB" = c("Carries", "Rushing Yards", "Rushing Touchdowns", "Yards per Carry"),
                               "WR" = c("Receptions", "Receiving Yards", "Receiving Touchdowns", "Yards per Catch"),
                               "TE" = c("Receptions", "Receiving Yards", "Receiving Touchdowns", "Yards per Catch"))

    updateSelectInput(
      session,
      inputId = "Option2",
      choices = position_choices)
  })

  observeEvent(input$Option6, {
  req(input$Option6)

  if (input$Option6 == "Team") {updateSelectInput(session, "Option5", choices = team_leader_stats)}
  else if (input$Option6 == "Player") {updateSelectInput(session, "Option5", choices = player_leader_stats)}
})

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
    
    # Top 3 Regardless
    # percentile_data %>%
    #   filter(player == input$Option1, !is.na(value)) %>%
    #   slice_max(value, n = 3)
    
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
      facet_wrap(~reorder(percentile, -value)) +
      theme_void() +
      theme(panel.grid = element_blank(),
            panel.background = element_blank(),
            legend.position = "none",
            strip.text = element_text(face = "bold", size = 18, hjust = 0.5, vjust = 1)) +
      labs(caption = "Percentile among qualified FBS players")
  })
  
  output$linechart <- renderPlot({
    
    if (endsWith(input$Option2, "Touchdowns")) {
     
      chart_data <- game_stats %>%
        filter(player == input$Option1) %>%
        mutate(cumulative_td = as.integer(cumsum(replace_na(!!sym(input$Option2), 0))))
      
      cumulative_tds <- tail(chart_data$cumulative_td, 1)
        
        if (cumulative_tds == 0) {
          ggplot() +
            geom_text(aes(x = 0, y = 0, label = str_c(input$Option1, " has not scored this season")), size = 10) +
            theme_void()
          }
      
        else {
          
          breaks <- if (cumulative_tds == 1) {2} else {1 + floor(1 + cumulative_tds / 2)}
          
          chart_data %>%
            ggplot(aes(x = reorder(game, game_date), y = cumulative_td, group = player, color = color)) +
            geom_col(aes(fill = color), alpha = 0.75) +
            geom_line() +
            geom_label(aes(label = cumulative_td)) +
            scale_fill_identity() +
            scale_color_identity() +
            scale_y_continuous(name = "Cumulative Touchdowns", n.breaks = breaks, labels = scales::number_format(accuracy = 1),
                               limits = c(0, ceiling(1.05*cumulative_tds))) +
            labs(x = NULL, title = str_c(input$Option1, "'s Cumulative ", input$Option2, " This Season")) +
            theme_minimal() +
            theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5))
        }
      }

    else {
      max_value <- game_stats %>%
        filter(player == input$Option1) %>%
        pull(!!sym(input$Option2)) %>%
        max(na.rm = TRUE)
      
      game_stats %>%
        filter(player == input$Option1) %>%
        ggplot(aes(x = reorder(game, game_date), y = !!sym(input$Option2), group = player, color = color)) +
        geom_col(aes(fill = color), alpha = 0.75) +
        geom_line() +
        geom_label(aes(label = !!sym(input$Option2))) +
        scale_fill_identity() +
        scale_color_identity() +
        scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(NA, ceiling(max_value * 1.05))) +
        labs(x = NULL, title = str_c(input$Option1, "'s ", input$Option2, " This Season")) +
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
      tab_header(title = header_title, subtitle = "As of 12/11/2024") %>%
      tab_style(locations = cells_title(), cell_fill(header_fill)) %>%
      tab_style(locations = cells_title(), cell_text(color = header_color, weight = "bold")) %>%
      tab_style(locations = cells_row_groups(), style = list(cell_text(align = "center", weight = "bold"))) %>%
      tab_options(table.width = "350px", column_labels.hidden = TRUE)
  })
  
  output$plot2 <- renderPlot({
    
    if (input$Option6 == "Team") {
    team_data %>%
      slice_max(order_by = !!sym(input$Option5), n = 10) %>%
      ggplot(aes(x = reorder(school, -!!sym(input$Option5)), y = !!sym(input$Option5))) +
      geom_col(aes(fill = color, color = alt_color), alpha = 0.85) +
      geom_from_path(aes(path = logo), height = 0.1) +
      geom_label(aes(label = !!sym(input$Option5), y = !!sym(input$Option5) * 0.88), color = "black", fill = "white") +
      scale_fill_identity() +
      scale_color_identity() +
      labs(x = NULL, title = str_c("Leaders in ", input$Option5, " This Season"),
           subtitle = "As of 12/11/24") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
            plot.subtitle = element_text(size = 14, hjust = 0.5))
    }
    
    else {
      leaders <- player_data %>%
        {if (input$Option5 %in% c("Completion Percentage", "Yards Per Attempt")) {
          filter(., `Pass Attempts` > 20)}
          
          else if (input$Option5 == "Yards Per Carry") {
            filter(., Carries > 20)}
          
          else if(input$Option5 == "Yards Per Catch")
            filter(., Receptions > 20)
          
          else
            filter(., !is.na(player))}
        
      leaders %>%
          arrange(desc(!!sym(input$Option5)), player) %>%
        slice_head(n = 10) %>%
        ggplot(aes(x = reorder(player, -!!sym(input$Option5)), y = !!sym(input$Option5))) +
        geom_col(aes(fill = color, color = alt_color), alpha = 0.85) +
        geom_from_path(aes(path = logo), height = 0.1) +
        geom_from_path(aes(path = headshot_url), height = 0.1) +
        geom_label(aes(label = !!sym(input$Option5), y = !!sym(input$Option5) * 0.88), color = "black", fill = "white") +
        scale_fill_identity() +
        scale_color_identity() +
        labs(x = NULL, subtitle = "As of 12/11/24", title =
               if_else(input$Option5 %in% c("Longest Run", "Longest Catch"), str_c("Players with the ", input$Option5, " This Season"),
                       if_else(input$Option5 %in% c("Completion Percentage", "Yards Per Attempt", "Yards Per Carry", "Yards Per Catch"),
                               str_c("Players with the Highest ", input$Option5, " This Season"),
                               str_c("Players with the Most ", input$Option5, " This Season")))) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
              plot.subtitle = element_text(size = 14, hjust = 0.5))
      
    }
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
      geom_hline(yintercept = c(25, 50, 75, 99), color = "black", lty = 3) +
      geom_segment(x = 0.5:5.5, y = 0, xend = 0.5:5.5, yend = 99, color = "black") +
      geom_col(aes(fill = color, color = alt_color, group = school), width = 1, alpha = 0.9) +
      annotate(geom = "text",
               label = c("Passing\nYards", "Passing\nTDs", "Yards Per\nAttempt",
                         "Yards Per\nCarry", "Rushing\nTDs", "Rushing\nYards"),
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
