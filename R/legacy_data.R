#### Legacy Season Pull ####====================================================

# Code will gather player stats, league info, matchup, and scoring information 
#  for seasons 2017-2020. 

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

# 2020 season
s2020_lg_info <- F_get_lg_info(season = 2020)

s2020_player_stats <- F_get_weekly_player_stats(week = 1:16, season = 2020, 
                          .lg_rules = s2020_lg_info$lg_rules)

s2020_matchups <- F_get_weekly_matchup(season = 2020, 
                       week = 1:16, 
                       .lg_id = s2020_lg_info$lg_id)

# The weekly player stats will be combined with matchup, roster, and player info
#  in order to get a comprehensive table

# pull out player info
s2020_ps0 <- s2020_player_stats$stats_melt1

# join to other attributes
s2020_ps1 <- s2020_ps0 %>% 
  left_join(s2020_matchups$mu_view0) %>% 
  left_join(s2020_lg_info$user_vw0) %>% 
  left_join(select(player_view0, player_id, position, player_name) %>% 
              distinct()) %>% 
  mutate(lg_year = 2020)

# matchup
s2020_weekly_owner_scores0 <- s2020_ps1 %>%
  filter(starter == TRUE, !is.na(matchup_id)) %>% 
  group_by(week, user_id, matchup_id) %>% 
  summarise(tot_points = round(sum(stat_extended, na.rm = TRUE), 2)) %>% 
  ungroup() %>% 
  mutate(week = as.numeric(week))

# Now do a sort of self join kind of deal to stick the points against. 
s2020_weekly_owner_scores1 <- s2020_weekly_owner_scores0 %>% 
  left_join(s2020_weekly_owner_scores0, by = c("week", "matchup_id")) %>% 
  filter(user_id.x != user_id.y) %>% 
  select(week, user_id.x, tot_points.x, tot_points.y, matchup_id) %>% 
  rename(owner = user_id.x, 
         points_for = tot_points.x, 
         points_against = tot_points.y) %>% 
  mutate(win = as.numeric(points_for > points_against),
         loss = as.numeric(!win))

# establish opponent for each week
s2020_weekly_opp0 <- s2020_weekly_owner_scores0 %>% 
  select(-tot_points) %>% 
  inner_join(s2020_weekly_owner_scores0 %>% select(-tot_points),
             by = c("week", "matchup_id")) %>% 
  filter(user_id.x != user_id.y) %>% 
  rename(owner = user_id.x, 
         opp = user_id.y)

# add opponent to weekly scores
s2020_weekly_owner_scores2 <- s2020_weekly_owner_scores1 %>% 
  left_join(s2020_weekly_opp0) %>% 
  mutate(lg_year = 2020)

# 2019 season
s2019_lg_info <- F_get_lg_info(season = 2019)

s2019_player_stats <- F_get_weekly_player_stats(week = 1:16, season = 2019, 
                                                .lg_rules = s2019_lg_info$lg_rules)

s2019_matchups <- F_get_weekly_matchup(season = 2019, 
                                       week = 1:16, 
                                       .lg_id = s2019_lg_info$lg_id)

# The weekly player stats will be combined with matchup, roster, and player info
#  in order to get a comprehensive table

# pull out player info
s2019_ps0 <- s2019_player_stats$stats_melt1

# join to other attributes
s2019_ps1 <- s2019_ps0 %>% 
  left_join(s2019_matchups$mu_view0) %>% 
  left_join(s2019_lg_info$user_vw0) %>% 
  left_join(select(player_view0, player_id, position, player_name) %>% 
              distinct()) %>% 
  mutate(lg_year = 2019)

# matchup
s2019_weekly_owner_scores0 <- s2019_ps1 %>%
  filter(starter == TRUE, !is.na(matchup_id)) %>% 
  group_by(week, user_id, matchup_id) %>% 
  summarise(tot_points = round(sum(stat_extended, na.rm = TRUE), 2)) %>% 
  ungroup() %>% 
  mutate(week = as.numeric(week))

# Now do a sort of self join kind of deal to stick the points against. 
s2019_weekly_owner_scores1 <- s2019_weekly_owner_scores0 %>% 
  left_join(s2019_weekly_owner_scores0, by = c("week", "matchup_id")) %>% 
  filter(user_id.x != user_id.y) %>% 
  select(week, user_id.x, tot_points.x, tot_points.y, matchup_id) %>% 
  rename(owner = user_id.x, 
         points_for = tot_points.x, 
         points_against = tot_points.y) %>% 
  mutate(win = as.numeric(points_for > points_against),
         loss = as.numeric(!win))

# establish opponent for each week
s2019_weekly_opp0 <- s2019_weekly_owner_scores0 %>% 
  select(-tot_points) %>% 
  inner_join(s2019_weekly_owner_scores0 %>% select(-tot_points),
             by = c("week", "matchup_id")) %>% 
  filter(user_id.x != user_id.y) %>% 
  rename(owner = user_id.x, 
         opp = user_id.y)

# add opponent to weekly scores
s2019_weekly_owner_scores2 <- s2019_weekly_owner_scores1 %>% 
  left_join(s2019_weekly_opp0) %>% 
  mutate(lg_year = 2019)

# 2018 season
s2018_lg_info <- F_get_lg_info(season = 2018)

s2018_player_stats <- F_get_weekly_player_stats(week = 1:16, season = 2018, 
                                                .lg_rules = s2018_lg_info$lg_rules)

s2018_matchups <- F_get_weekly_matchup(season = 2018, 
                                       week = 1:16, 
                                       .lg_id = s2018_lg_info$lg_id)

# The weekly player stats will be combined with matchup, roster, and player info
#  in order to get a comprehensive table

# pull out player info
s2018_ps0 <- s2018_player_stats$stats_melt1

# join to other attributes
s2018_ps1 <- s2018_ps0 %>% 
  left_join(s2018_matchups$mu_view0) %>% 
  left_join(s2018_lg_info$user_vw0) %>% 
  left_join(select(player_view0, player_id, position, player_name) %>% 
              distinct()) %>% 
  mutate(lg_year = 2018)

# matchup
s2018_weekly_owner_scores0 <- s2018_ps1 %>%
  filter(starter == TRUE, !is.na(matchup_id)) %>% 
  group_by(week, user_id, matchup_id) %>% 
  summarise(tot_points = round(sum(stat_extended, na.rm = TRUE), 2)) %>% 
  ungroup() %>% 
  mutate(week = as.numeric(week))

# Now do a sort of self join kind of deal to stick the points against. 
s2018_weekly_owner_scores1 <- s2018_weekly_owner_scores0 %>% 
  left_join(s2018_weekly_owner_scores0, by = c("week", "matchup_id")) %>% 
  filter(user_id.x != user_id.y) %>% 
  select(week, user_id.x, tot_points.x, tot_points.y, matchup_id) %>% 
  rename(owner = user_id.x, 
         points_for = tot_points.x, 
         points_against = tot_points.y) %>% 
  mutate(win = as.numeric(points_for > points_against),
         loss = as.numeric(!win))

# establish opponent for each week
s2018_weekly_opp0 <- s2018_weekly_owner_scores0 %>% 
  select(-tot_points) %>% 
  inner_join(s2018_weekly_owner_scores0 %>% select(-tot_points),
             by = c("week", "matchup_id")) %>% 
  filter(user_id.x != user_id.y) %>% 
  rename(owner = user_id.x, 
         opp = user_id.y)

# add opponent to weekly scores
s2018_weekly_owner_scores2 <- s2018_weekly_owner_scores1 %>% 
  left_join(s2018_weekly_opp0) %>% 
  mutate(lg_year = 2018)

# 2017 season
s2017_lg_info <- F_get_lg_info(season = 2017)

s2017_player_stats <- F_get_weekly_player_stats(week = 1:16, season = 2017, 
                                                .lg_rules = s2017_lg_info$lg_rules)

s2017_matchups <- F_get_weekly_matchup(season = 2017, 
                                       week = 1:16, 
                                       .lg_id = s2017_lg_info$lg_id)

# The weekly player stats will be combined with matchup, roster, and player info
#  in order to get a comprehensive table

# pull out player info
s2017_ps0 <- s2017_player_stats$stats_melt1

# join to other attributes
s2017_ps1 <- s2017_ps0 %>% 
  left_join(s2017_matchups$mu_view0) %>% 
  left_join(s2017_lg_info$user_vw0) %>% 
  left_join(select(player_view0, player_id, position, player_name) %>% 
              distinct()) %>% 
  mutate(lg_year = 2017)

# matchup
s2017_weekly_owner_scores0 <- s2017_ps1 %>%
  filter(starter == TRUE, !is.na(matchup_id)) %>% 
  group_by(week, user_id, matchup_id) %>% 
  summarise(tot_points = round(sum(stat_extended, na.rm = TRUE), 2)) %>% 
  ungroup() %>% 
  mutate(week = as.numeric(week))

# Now do a sort of self join kind of deal to stick the points against. 
s2017_weekly_owner_scores1 <- s2017_weekly_owner_scores0 %>% 
  left_join(s2017_weekly_owner_scores0, by = c("week", "matchup_id")) %>% 
  filter(user_id.x != user_id.y) %>% 
  select(week, user_id.x, tot_points.x, tot_points.y, matchup_id) %>% 
  rename(owner = user_id.x, 
         points_for = tot_points.x, 
         points_against = tot_points.y) %>% 
  mutate(win = as.numeric(points_for > points_against),
         loss = as.numeric(!win))

# establish opponent for each week
s2017_weekly_opp0 <- s2017_weekly_owner_scores0 %>% 
  select(-tot_points) %>% 
  inner_join(s2017_weekly_owner_scores0 %>% select(-tot_points),
             by = c("week", "matchup_id")) %>% 
  filter(user_id.x != user_id.y) %>% 
  rename(owner = user_id.x, 
         opp = user_id.y)

# add opponent to weekly scores
s2017_weekly_owner_scores2 <- s2017_weekly_owner_scores1 %>% 
  left_join(s2017_weekly_opp0) %>% 
  mutate(lg_year = 2017)

#### Combine ####===============================================================

#### ~Player Stats~ ####========================================================

comb_ps0 <- s2020_ps1 %>% 
  bind_rows(s2019_ps1) %>% 
  bind_rows(s2018_ps1) %>% 
  bind_rows(s2017_ps1)

comb_ps1 <- comb_ps0 %>% 
  left_join(ouid_ref0) %>% 
  mutate(owner_name = case_when(!is.na(owner_name) ~ owner_name, 
                                is.na(owner_name) ~ "FREE AGENT")) %>% 
  mutate(lg_year = as.factor(lg_year), 
         week = as.factor(week)) %>% 
  left_join(yfg0, by = c("lg_year" = "lg_year")) %>% 
  left_join(ofg0, by = c("owner_name" = "owner_name")) %>% 
  select(-display_name, -roster_id, -user_id)

#### ~Matchups~ ####============================================================

comb_mu0 <- s2020_weekly_owner_scores2 %>% 
  bind_rows(s2019_weekly_owner_scores2) %>% 
  bind_rows(s2018_weekly_owner_scores2) %>% 
  bind_rows(s2017_weekly_owner_scores2) 

# add owner and opponent names onto this
comb_mu1 <- comb_mu0 %>% 
  left_join(ouid_ref0, 
            by = c("owner"= "user_id")) %>% 
  left_join(ouid_ref0, 
            by = c("opp"= "user_id")) %>% 
  rename(owner_name = owner_name.x, 
         opp_name = owner_name.y) %>% 
  mutate(margin = points_for - points_against) %>% 
  group_by(week, lg_year) %>% 
  mutate(beat_mean = as.integer(points_for >= mean(points_for)), 
         beat_med = as.integer(points_for >= median(points_for)),
         lg_year = as.factor(lg_year), 
         week = as.factor(week)) %>% 
  left_join(yfg0, by = c("lg_year" = "lg_year")) %>% 
  left_join(ofg0, by = c("owner_name" = "owner_name")) %>% 
  select(-owner, -opp)

#### ~Write Out~ ####===========================================================

data.table::fwrite(comb_ps1, 
                   paste0("data/tableau/player_stats.csv"))

data.table::fwrite(comb_mu1, 
                   paste0("data/tableau/matchup.csv"))
