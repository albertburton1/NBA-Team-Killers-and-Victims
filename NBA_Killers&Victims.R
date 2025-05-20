# Load libraries
library(dplyr)
library(tidyr)
library(scales)
library(gt)
library(gtExtras)
library(nbastatR)
library(future)


# Set environment parameters
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 10)

# Parameters
min_points_threshold <- 10
seasons <- 2020:2024

# Get player game logs
get_player_game_logs <- function(seasons) {
  all_games <- data.frame()
  for (season in seasons) {
    season_games <- game_logs(seasons = season, result_types = "player", season_types = "Regular Season")
    all_games <- rbind(all_games, season_games)
  }
  return(all_games)
}

# Fetch data
all_player_games <- get_player_game_logs(seasons)

# Summarize performance
player_team_stats <- all_player_games %>%
  filter(minutes >= 10) %>%
  group_by(namePlayer) %>%
  filter(mean(pts, na.rm = TRUE) >= min_points_threshold) %>%
  group_by(namePlayer, slugOpponent) %>%
  summarise(
    games_vs_team = n(),
    avg_pts_vs_team = mean(pts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(games_vs_team >= 5) %>%
  group_by(namePlayer) %>%
  mutate(
    total_games = sum(games_vs_team),
    overall_avg_pts = sum(avg_pts_vs_team * games_vs_team) / total_games,
    pts_diff = avg_pts_vs_team - overall_avg_pts,
    pts_diff_pct = (avg_pts_vs_team / overall_avg_pts - 1) * 100
  ) %>%
  ungroup()

# Identify killers and victims
top_1_team_killers <- player_team_stats %>%
  group_by(slugOpponent) %>%
  slice_max(order_by = pts_diff, n = 1, with_ties = FALSE) %>%
  ungroup()

top_1_team_victims <- player_team_stats %>%
  group_by(slugOpponent) %>%
  slice_min(order_by = pts_diff, n = 1, with_ties = FALSE) %>%
  ungroup()

# Load logos and headshots

team_logos <- tibble::tibble(
  Team = c("ATL", "BOS", "BKN", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GSW", "HOU", "IND", "LAC", "LAL", "MEM", 
           "MIA", "MIL", "MIN", "NOP", "NYK", "OKC", "ORL", "PHI", "PHX", "POR", "SAC", "SAS", "TOR", "UTA", "WAS"),
  TeamLogo = c(
    "https://a.espncdn.com/i/teamlogos/nba/500/atl.png",
    "https://a.espncdn.com/i/teamlogos/nba/500/bos.png",
    "https://a.espncdn.com/i/teamlogos/nba/500/bkn.png",
    "https://a.espncdn.com/i/teamlogos/nba/500/cha.png",
    "https://a.espncdn.com/i/teamlogos/nba/500/chi.png",
    "https://a.espncdn.com/i/teamlogos/nba/500/cle.png",
    "https://a.espncdn.com/i/teamlogos/nba/500/dal.png",
    "https://a.espncdn.com/i/teamlogos/nba/500/den.png",
    "https://a.espncdn.com/i/teamlogos/nba/500/det.png",
    "https://a.espncdn.com/i/teamlogos/nba/500/gs.png",
    "https://a.espncdn.com/i/teamlogos/nba/500/hou.png",
    "https://a.espncdn.com/i/teamlogos/nba/500/ind.png",
    "https://a.espncdn.com/i/teamlogos/nba/500/lac.png",
    "https://a.espncdn.com/i/teamlogos/nba/500/lal.png",
    "https://a.espncdn.com/i/teamlogos/nba/500/mem.png",
    "https://a.espncdn.com/i/teamlogos/nba/500/mia.png",
    "https://a.espncdn.com/i/teamlogos/nba/500/mil.png",
    "https://a.espncdn.com/i/teamlogos/nba/500/min.png",
    "https://a.espncdn.com/i/teamlogos/nba/500/no.png",      # Pelicans
    "https://a.espncdn.com/i/teamlogos/nba/500/ny.png",
    "https://a.espncdn.com/i/teamlogos/nba/500/okc.png",
    "https://a.espncdn.com/i/teamlogos/nba/500/orl.png",
    "https://a.espncdn.com/i/teamlogos/nba/500/phi.png",
    "https://a.espncdn.com/i/teamlogos/nba/500/phx.png",
    "https://a.espncdn.com/i/teamlogos/nba/500/por.png",
    "https://a.espncdn.com/i/teamlogos/nba/500/sac.png",
    "https://a.espncdn.com/i/teamlogos/nba/500/sas.png",
    "https://a.espncdn.com/i/teamlogos/nba/500/tor.png",
    "https://a.espncdn.com/i/teamlogos/nba/500/utah.png",    # Jazz
    "https://a.espncdn.com/i/teamlogos/nba/500/wsh.png"
  )
)

player_info <- nba_players() %>%
  select(namePlayer, urlPlayerHeadshot) %>%
  distinct()

# Prepare killer table
killers_table <- top_1_team_killers %>%
  transmute(
    Team = slugOpponent,
    Player = namePlayer,
    `Games vs Team` = games_vs_team,
    `Avg vs Team` = round(avg_pts_vs_team, 1),
    `Overall Avg` = round(overall_avg_pts, 1),
    `Diff` = round(pts_diff, 1),
    `Diff %` = round(pts_diff_pct, 1)
  ) %>%
  left_join(player_info, by = c("Player" = "namePlayer")) %>%
  left_join(team_logos, by = "Team")

# Prepare victims table
victims_table <- top_1_team_victims %>%
  transmute(
    Team = slugOpponent,
    Player = namePlayer,
    `Games vs Team` = games_vs_team,
    `Avg vs Team` = round(avg_pts_vs_team, 1),
    `Overall Avg` = round(overall_avg_pts, 1),
    `Diff` = round(pts_diff, 1),
    `Diff %` = round(pts_diff_pct, 1)
  ) %>%
  left_join(player_info, by = c("Player" = "namePlayer")) %>%
  left_join(team_logos, by = "Team")



# Create GT table for Killers
gt_killers <- killers_table %>%
  select(-Team) %>%
  gt() %>%
  gt_img_rows(columns = urlPlayerHeadshot, height = 50) %>%
  gt_img_rows(columns = TeamLogo, height = 40) %>%
  cols_label(
    TeamLogo = "Team", urlPlayerHeadshot = "Player",
    `Games vs Team` = "Games", `Avg vs Team` = "Avg PTS vs Team",
    `Overall Avg` = "Season Avg PTS", `Diff` = "PTS Diff", `Diff %` = "Diff (%)"
  ) %>%
  cols_move_to_start(columns = TeamLogo) %>%
  cols_move(columns = urlPlayerHeadshot, after = TeamLogo) %>%
  tab_header(
    title = "NBA Killers",
    subtitle = "Top Overperformer vs Each Team (2020–2024)"
  ) %>%
  tab_style(
    style = list(
      cell_text(align = "center", size = "x-large", weight = "bold")
    ),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = list(
      cell_text(align = "center", size = "medium", style = "italic")
    ),
    locations = cells_title(groups = "subtitle")
  ) %>%
  gt_theme_538() %>%
  cols_align("center", everything()) %>%
  gt_hulk_col_numeric(columns = vars(`Diff`, `Diff %`), domain = c(0, max(killers_table$`Diff`))) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = vars(`Games vs Team`, `Avg vs Team`, `Overall Avg`))
  )

gtsave(gt_killers, "nba_killers.png", vwidth = 2800, vheight = 4000)

# Create GT table for Victims
gt_victims <- victims_table %>%
  select(-Team) %>%
  gt() %>%
  gt_img_rows(columns = urlPlayerHeadshot, height = 50) %>%
  gt_img_rows(columns = TeamLogo, height = 40) %>%
  cols_label(
    TeamLogo = "Team", urlPlayerHeadshot = "Player",
    `Games vs Team` = "Games", `Avg vs Team` = "Avg PTS vs Team",
    `Overall Avg` = "Season Avg PTS", `Diff` = "PTS Diff", `Diff %` = "Diff (%)"
  ) %>%
  cols_move_to_start(columns = TeamLogo) %>%
  cols_move(columns = urlPlayerHeadshot, after = TeamLogo) %>%
  tab_header(
    title = "NBA Victims",
    subtitle = "Top Underperformer vs Each Team (2020–2024)"
  ) %>%
  tab_style(
    style = list(
      cell_text(align = "center", size = "x-large", weight = "bold")
    ),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = list(
      cell_text(align = "center", size = "medium", style = "italic")
    ),
    locations = cells_title(groups = "subtitle")
  ) %>%
  gt_theme_538() %>%
  cols_align("center", everything()) %>%
  gt_hulk_col_numeric(columns = vars(`Diff`, `Diff %`), domain = c(min(victims_table$`Diff`), 0)) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = vars(`Games vs Team`, `Avg vs Team`, `Overall Avg`))
  )

gtsave(gt_victims, "nba_victims.png", vwidth = 2800, vheight = 4000)

