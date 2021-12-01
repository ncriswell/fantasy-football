#### Routine Weekly Updates ####================================================
# Code to run weekly to update Tableau tables

#### Setup ####=================================================================

# Source global code that loads packages, pulls standardized reference data, 
#  and creates some utility functions. 
source("R/global.R")

#### Gathering Data ####========================================================

#### ~Reference~ ####===========================================================
# owner reference 
ouid_ref0 <- read_delim("data/reference/owner_uid_ref.txt", col_types = cols(.default = col_character()),
                        delim = "\t") %>% 
  select(owner_name, user_id)

# owner facet grid
ofg0 <- read_csv("data/reference/owner_facet_grid.csv")

# year facet grid
yfg0 <- read_csv("data/reference/year_facet_grid.csv", 
                 col_types = cols(lg_year = col_factor()))

#### Weekly Run ####============================================================
cy_lg_info <- F_get_lg_info(season = 2021)

cy_player_stats <- F_get_weekly_player_stats(week = 1:12, season = 2021, 
                                                .lg_rules = cy_lg_info$lg_rules)

cy_matchups <- F_get_weekly_matchup(season = 2021, 
                                       week = 1:12, 
                                       .lg_id = cy_lg_info$lg_id)

# pull rosters as of now
rost0 <- fromJSON(paste0("https://api.sleeper.app/v1/league/", cy_lg_info$lg_id, 
                         "/rosters")) %>% 
  select(owner_id, players) %>% 
  unnest(players) %>% 
  left_join(ouid_ref0, by = c("owner_id" = "user_id")) %>% 
  select(current_owner = owner_name,
         players)

# The weekly player stats will be combined with matchup, roster, and player info
#  in order to get a comprehensive table

# pull out player info
cy_ps0 <- cy_player_stats$stats_melt1

# join to other attributes
cy_ps1 <- cy_ps0 %>% 
  left_join(cy_matchups$mu_view0) %>% 
  left_join(cy_lg_info$user_vw0) %>% 
  left_join(select(player_view0, player_id, position, player_name) %>% 
              distinct()) %>% 
  mutate(lg_year = 2021)

# matchup
cy_weekly_owner_scores0 <- cy_ps1 %>%
  filter(starter == TRUE, !is.na(matchup_id)) %>% 
  group_by(week, user_id, matchup_id) %>% 
  summarise(tot_points = round(sum(stat_extended, na.rm = TRUE), 2)) %>% 
  ungroup() %>% 
  mutate(week = as.numeric(week))

# Now do a sort of self join kind of deal to stick the points against. 
cy_weekly_owner_scores1 <- cy_weekly_owner_scores0 %>% 
  left_join(cy_weekly_owner_scores0, by = c("week", "matchup_id")) %>% 
  filter(user_id.x != user_id.y) %>% 
  select(week, user_id.x, tot_points.x, tot_points.y, matchup_id) %>% 
  rename(owner = user_id.x, 
         points_for = tot_points.x, 
         points_against = tot_points.y) %>% 
  mutate(win = as.numeric(points_for > points_against),
         loss = as.numeric(!win))

# establish opponent for each week
cy_weekly_opp0 <- cy_weekly_owner_scores0 %>% 
  select(-tot_points) %>% 
  inner_join(cy_weekly_owner_scores0 %>% select(-tot_points),
             by = c("week", "matchup_id")) %>% 
  filter(user_id.x != user_id.y) %>% 
  rename(owner = user_id.x, 
         opp = user_id.y)

# add opponent to weekly scores
cy_weekly_owner_scores2 <- cy_weekly_owner_scores1 %>% 
  left_join(cy_weekly_opp0) %>% 
  mutate(lg_year = 2021)


# Add attributes to data
cy_ps2 <- cy_ps1 %>% 
  left_join(ouid_ref0) %>% 
  mutate(owner_name = case_when(!is.na(owner_name) ~ owner_name, 
                                is.na(owner_name) ~ "FREE AGENT")) %>% 
  mutate(lg_year = as.factor(lg_year), 
         week = as.factor(week)) %>% 
  left_join(yfg0, by = c("lg_year" = "lg_year")) %>% 
  left_join(ofg0, by = c("owner_name" = "owner_name")) %>% 
  left_join(rost0, by = c("player_id" = "players")) %>% 
  mutate(current_owner = case_when(!is.na(current_owner) ~ current_owner, 
                                   TRUE ~ "FREE AGENT"))


cy_weekly_owner_scores3 <- cy_weekly_owner_scores2 %>% 
  left_join(ouid_ref0, 
            by = c("owner"= "user_id")) %>% 
  left_join(ouid_ref0, 
            by = c("opp"= "user_id")) %>% 
  rename(owner_name = owner_name.x, 
         opp_name = owner_name.y) %>% 
  mutate(margin = points_for - points_against) %>% 
  group_by(week, lg_year) %>% 
  mutate(week_med = median(points_for), 
         beat_mean = as.integer(points_for >= mean(points_for)), 
         beat_med = as.integer(points_for >= median(points_for)),
         lg_year = as.factor(lg_year), 
         week = as.factor(week)) %>% 
  left_join(yfg0, by = c("lg_year" = "lg_year")) %>% 
  left_join(ofg0, by = c("owner_name" = "owner_name")) %>% 
  select(-owner, -opp)

# Read and replace the existing Tableau data
player_stats0 <- read_csv("data/tableau/player_stats.csv", 
                          col_types = cols(week = col_factor(), 
                                           lg_year = col_factor(),
                                           player_id = col_character(),
                                           user_id = col_character()))
         
matchups0 <- read_csv("data/tableau/matchup.csv",
                      col_types = cols(week = col_factor(), 
                                       lg_year = col_factor()))

new_player_stats <- cy_ps2 %>% 
  anti_join(player_stats0, by = c("week", "lg_year"))

new_matchups <- cy_weekly_owner_scores3 %>% 
  anti_join(matchups0, by = c("week", "lg_year"))

player_stats1 <- player_stats0 %>% 
  bind_rows(new_player_stats) %>% 
  mutate(status = case_when(owner_name == "FREE AGENT" ~ "FREE AGENT", 
                      starter == TRUE ~ "STARTER", 
                      starter == FALSE ~ "BENCH"))

# Remove positions who are only owned by Free Agent
fa_pos <- player_stats1 %>% 
  group_by(position) %>% 
  summarise(numrec = length(unique(owner_name)),
            has_fa = "FREE AGENT" %in% owner_name) %>% 
  ungroup() %>% 
  filter(numrec == 1, has_fa == TRUE) %>% 
  select(position) %>% pull()

player_stats2 <- player_stats1 %>% 
  filter(!(position %in% fa_pos))

matchups1 <- matchups0 %>% 
  bind_rows(new_matchups) %>% 
  mutate(outcome = case_when(win == 0 ~ "Loss", 
                             TRUE ~ "Win"))


data.table::fwrite(player_stats2, 
                   paste0("data/tableau/player_stats.csv"))

data.table::fwrite(matchups1, 
                   paste0("data/tableau/matchup.csv"))

