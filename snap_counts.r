library(nflverse)
library(tidyverse)
library(ggplot2)

# Data-----

YEAR <- get_latest_season()
TEAM <- "KC"
CUTOFF <- 10

# Get the snap counts and other info you want. 
snaps1 <- load_snap_counts(seasons = YEAR) |> 
  filter(team == TEAM, !position %in% c("K", "P", "LS", "QB")) |> 
  # Fix players who's official position is dumb and foolish
  mutate(player = clean_player_names(player)) |> 
  mutate(first_name = word(player, 1), .after = "player") |> 
  mutate(last_name = case_when(
    word(player, -2) == "St" ~ paste0("St. ", word(player, -1)),
    TRUE ~ word(player, -1)), .after = "first_name") |> 
  mutate(short_name = paste0(str_sub(first_name, start = 1, end = 1), ".", last_name), .after = "last_name") |> 
  mutate(position = case_when(
    player == "Bryan Cook" ~ "S", 
    player == "Carlos Dunlap" ~ "DE", 
    TRUE ~ position
  )) |> 
  mutate(position_group = case_when(
    position %in% c("T", "G", "C") ~ "OL", 
    position %in% c("RB", "FB") ~ "RB", 
    position %in% c("FS", "SS") ~ "S", 
    position %in% c("CB", "DB") ~ "CB", 
    position %in% c("DT", "NT") ~ "IDL", 
    TRUE ~ position
  )) |> 
  mutate(side = case_when(
    position_group %in% c("OL", "RB", "TE", "WR") ~ "offense", 
    position_group %in% c("CB", "DE", "IDL", "LB", "S") ~ "defense"
  )) |> 
  mutate(snap_pct = case_when(
    side == "offense" ~ (offense_pct * 100), 
    side == "defense" ~ (defense_pct * 100)
  )) |> 
  mutate(snap_counts = case_when(
    side == "offense" ~ offense_snaps, 
    side == "defense" ~ defense_snaps
  )) |> 
  filter(snap_counts != 0) |> 
  group_by(pfr_player_id, short_name) |> 
  mutate(offense_snap_totals = sum(offense_snaps)) |> 
  mutate(defense_snap_totals = sum(defense_snaps)) |> 
  mutate(snap_totals = sum(snap_counts)) |> 
  mutate(short_name_snaps = paste0(short_name, " (", snap_totals, ")"), .after = "short_name") |> 
  mutate(snap_rough_pct = case_when(
    side == "offense" ~ snap_totals / max(offense_snap_totals), 
    side == "defense" ~ snap_totals / max(defense_snap_totals), 
  )) |> 
  ungroup()

# Get list of players with > CUTOFF snaps and filter them in
qualifying_players_df <- snaps1 |> 
  filter(snap_pct >= CUTOFF)

qualifying_players <- unique(qualifying_players_df$short_name_snaps)

# Get the bye week (assuming it's passed already) and add the row to snaps df
bye_week = setdiff(1:max(snaps1$week), snaps1$week)

snaps2 <- snaps1%>%
  add_row(week=bye_week, opponent=TEAM) %>%
  arrange(week) |>
  left_join(select(load_teams(), team_abbr, team_logo_espn), by = c("opponent" = "team_abbr"))

# Make sure every player has a row for every game, regardless of whether 
# or not they played in it. Will need this for dash lines.
snaps3 <- snaps2 |> 
  complete(week, pfr_player_id) |> 
  group_by(pfr_player_id) |> 
  fill(short_name, short_name_snaps, position, position_group, .direction = "downup") |> 
  ungroup()

# List of position groups to run through

position_groups <- c("RB", "TE", "WR", "CB", "DE", "IDL", "LB", "S")

# Loop through the position groups and save images
for(i in position_groups){


# Filter for position groups, and set the levels for the legend
snaps <- snaps3 |> 
  filter(short_name_snaps %in% qualifying_players, position_group == i) |> 
  arrange(-snap_totals, short_name_snaps)

player_list <- unique(snaps$short_name_snaps)

snaps$short_name_snaps <- factor(snaps$short_name_snaps, levels = player_list)

# get middle of x axis for the logo in the center of panel
team_logo_x_pos = mean(snaps$week)

# Get df of just opponent logos
opponents <- snaps2 |> 
  select(opponent, week, team_logo_espn) |> 
  filter(!is.na(opponent)) |> 
  distinct()

# Plot------

#options(repr.plot.width =6, repr.plot.height =4)

(
snap_pct_plot <- 
  ggplot() +
  geom_nfl_logos(mapping = aes(x = mean(snaps$week), y = 50, team_abbr = TEAM, alpha = 0.5, width = 0.4)) + 
  geom_nfl_logos(mapping = aes(x = mean(snaps$week), y = 50, team_abbr = TEAM, alpha = 0.5, width = 0.4), color = "white") + 
  # First geom_line makes the dashed line connecting all dots
  geom_line(data = snaps[!is.na(snaps$snap_pct),], linetype = "dashed", 
            mapping = aes(x = week, y = snap_pct, color = short_name_snaps), alpha = 0.3) + 
  # Second geom_line uses "na.rm = FALSE" to remove lines where there's no week data
  geom_line(snaps, mapping = aes(x = week, y = snap_pct, color = short_name_snaps), na.rm = FALSE) + 
  geom_point(snaps, mapping = aes(x = week, y = snap_pct, color = short_name_snaps)) + 
  # the "clip = 'off'" allows the logos to not disappear when they are outside 
  # the plot area. 
  coord_cartesian(ylim = c(0, 100), clip = "off") +
  geom_nfl_logos(opponents, mapping = aes(x = week, y = -14, team_abbr = opponent), width = 0.05) + 
  # Forces all the weeks to show up on x axis
  scale_y_continuous(labels = c(0, 20, 40, 60, 80, 100), breaks = c(0, 20, 40, 60, 80, 100)) + 
  scale_x_continuous(labels = snaps$week, breaks = snaps$week) +
  #scale_color_viridis_d() +
  labs(
    title = paste0(i, " Snap Percentages ", YEAR),
    subtitle = paste0("Minimum ", CUTOFF, "% of snaps in any game in range"), 
    caption = "Data: nflverse.com   |   Chart: @josephjefe",
    y = "Percentage of Snaps", 
    x = "Week", 
    color = "Player (snaps)"
  ) + 
  theme_light() +
  theme(
    axis.title = element_text(size = 8), 
    axis.text = element_text(size = 7), 
    axis.text.x = element_text(margin=margin(26,0,0,0)), 
    plot.background = element_rect(fill = "white"), 
    plot.title = element_text(hjust = 0.5), 
    plot.subtitle = element_text(hjust = 0.5, size = 8), 
    panel.grid.minor = element_blank(), 
    plot.caption = element_text(size = 5), 
    legend.key.height = unit(0.25, "cm"), 
    legend.key.width = unit(0.35, "cm"), 
    legend.justification = "top", 
    legend.text = element_text(size = 5), 
    legend.title = element_text(size = 7), 
    legend.box.spacing = margin(c(5, 0, 0, 0)), 
    legend.box.background = element_rect(color = "gray80")    
  )
)

ggsave(plot = snap_pct_plot, path = "images/", filename = paste0("Week ", max(snaps1$week), " ", i, " Season Snap Percentages.png"), height = 4, width = 5.5)


(
  snaps_plot <- 
    ggplot() +
    geom_nfl_logos(mapping = aes(x = mean(snaps$week), y = 50, team_abbr = TEAM, alpha = 0.5, width = 0.4)) + 
    geom_nfl_logos(mapping = aes(x = mean(snaps$week), y = 50, team_abbr = TEAM, alpha = 0.5, width = 0.4), color = "white") + 
    # First geom_line makes the dashed line connecting all dots
    geom_line(data = snaps[!is.na(snaps$snap_counts),], linetype = "dashed", 
              mapping = aes(x = week, y = snap_counts, color = short_name_snaps), alpha = 0.3) + 
    # Second geom_line uses "na.rm = FALSE" to remove lines where there's no week data
    geom_line(snaps, mapping = aes(x = week, y = snap_counts, color = short_name_snaps), na.rm = FALSE) + 
    geom_point(snaps, mapping = aes(x = week, y = snap_counts, color = short_name_snaps)) + 
    # the "clip = 'off'" allows the logos to not disappear when they are outside 
    # the plot area. 
    coord_cartesian(ylim = c(0, 85), clip = "off") +
    geom_nfl_logos(opponents, mapping = aes(x = week, y = -14, team_abbr = opponent), width = 0.05) + 
    # Forces all the weeks to show up on x axis
    scale_y_continuous(labels = c(0, 20, 40, 60, 80), breaks = c(0, 20, 40, 60, 80)) + 
    scale_x_continuous(labels = snaps$week, breaks = snaps$week) +
    #scale_color_viridis_d() +
    labs(
      title = paste0(i, " Snap Counts ", YEAR),
      subtitle = paste0("Minimum ", CUTOFF, "% of snaps in any game in range"), 
      caption = "Data: nflverse.com   |   Chart: @josephjefe",
      y = "Number of Snaps", 
      x = "Week", 
      color = "Player (snaps)"
    ) + 
    theme_light() +
    theme(
      axis.title = element_text(size = 8), 
      axis.text = element_text(size = 7), 
      axis.text.x = element_text(margin=margin(26,0,0,0)), 
      plot.background = element_rect(fill = "white"), 
      plot.title = element_text(hjust = 0.5), 
      plot.subtitle = element_text(hjust = 0.5, size = 8), 
      panel.grid.minor = element_blank(), 
      plot.caption = element_text(size = 5), 
      legend.key.height = unit(0.25, "cm"), 
      legend.key.width = unit(0.35, "cm"), 
      legend.justification = "top", 
      legend.text = element_text(size = 5), 
      legend.title = element_text(size = 7), 
      legend.box.spacing = margin(c(5, 0, 0, 0)), 
      legend.box.background = element_rect(color = "gray80")    
    )
)

ggsave(plot = snaps_plot, path = "images/", filename = paste0("Week ", max(snaps1$week), " ", i, " Season Snap Totals.png"), height = 4, width = 5.5)

}

