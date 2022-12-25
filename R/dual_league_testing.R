sql_id <- "861712947813130240"
pql_id <- "861706911337267200"

lg_ref <- tibble(lg_id = c(sql_id, pql_id), 
                 lg_name = c("Sequel", "Prequel"))

# owner reference 
ouid_ref_legacy0 <- read_delim("data/reference/owner_uid_ref.txt", col_types = cols(.default = col_character()),
                        delim = "\t") %>% 
  select(owner_name, user_id) %>% 
  mutate(owner_name = case_when(owner_name == "SAM" ~ "GOGEL", 
                                TRUE ~ owner_name))

ouid_dual_league <- read_csv("data/reference/2022_season_owner_ref.csv",
                     col_types = cols(.default = col_character())) %>% 
  select(owner_name, user_id, league_2022) %>% 
  distinct()

# combine these so we have legacy information
ouid0 <- ouid_dual_league %>% 
  bind_rows(ouid_ref_legacy0 %>% anti_join(ouid_dual_league))

# owner facet grid
ofg0 <- read_csv("data/reference/owner_facet_grid.csv", 
                 col_types = cols(season = col_factor())) 

# year facet grid
yfg0 <- read_csv("data/reference/year_facet_grid.csv", 
                 col_types = cols(lg_year = col_factor()))

# cy_player_stats <- F_get_weekly_player_stats(week = 1:11, season = 2022, 
#                                              .lg_rules = pql_lg_info$lg_rules)


# Pull information for each league. Will be running all the utility twice - 
  # once for each league and then binding together the good stuff

# league information - lets us map the user IDs to matchup IDs
pql_lg_info <- F_get_lg_info(season = 2022, 
                             .lg_id = pql_id,
                             .method = "lg_id"
)

sql_lg_info <- F_get_lg_info(season = 2022, 
                             .lg_id = sql_id,
                             .method = "lg_id"
)


lg_usr_view <- bind_rows(sql_lg_info$user_vw0,
                         pql_lg_info$user_vw0)

# matchup - lets us know who played who each week
pql_matchups <- F_get_weekly_matchup(season = 2022, 
                                     week = 1:15, 
                                     .lg_id = pql_id)

sql_matchups <- F_get_weekly_matchup(season = 2022, 
                                     week = 1:15, 
                                     .lg_id = sql_id)

mu_view <- bind_rows(pql_matchups$mu_view0,
                     sql_matchups$mu_view0)

# cy_matchups <- F_get_weekly_matchup(season = 2022, 
#                                     week = 1:11, 
#                                     .lg_id = cy_lg_info$lg_id)

# pull player stats
cy_player_stats <- F_get_weekly_player_stats(week = 1:15, season = 2022, 
                                             .lg_rules = pql_lg_info$lg_rules)


# rost0 <- fromJSON(paste0("https://api.sleeper.app/v1/league/", cy_lg_info$lg_id, 
#                          "/rosters")) %>% 
#   select(owner_id, players) %>% 
#   unnest(players) %>% 
#   left_join(ouid_ref0, by = c("owner_id" = "user_id")) %>% 
#   select(current_owner = owner_name,
#          players)

# The weekly player stats will be combined with matchup, roster, and player info
#  in order to get a comprehensive table

# pull out player info
cy_ps0 <- cy_player_stats$stats_melt1

# join to other attributes
cy_ps1 <- cy_ps0 %>% 
  left_join(mu_view) %>% 
  left_join(lg_usr_view) %>% 
  left_join(select(player_view0, player_id, position, player_name) %>% 
              distinct()) %>% 
  mutate(lg_year = 2022)

# matchup
cy_weekly_owner_scores0 <- cy_ps1 %>%
  filter(starter == TRUE, !is.na(matchup_id)) %>% 
  group_by(week, user_id, matchup_id, lg_id) %>% 
  summarise(tot_points = round(sum(stat_extended, na.rm = TRUE), 2)) %>% 
  ungroup() %>% 
  mutate(week = as.numeric(week))

# Now do a sort of self join kind of deal to stick the points against. 
cy_weekly_owner_scores1 <- cy_weekly_owner_scores0 %>% 
  left_join(cy_weekly_owner_scores0, by = c("week", "matchup_id", "lg_id")) %>% 
  filter(user_id.x != user_id.y) %>% 
  select(week, user_id.x, tot_points.x, tot_points.y, matchup_id, lg_id) %>% 
  rename(owner = user_id.x, 
         points_for = tot_points.x, 
         points_against = tot_points.y) %>% 
  mutate(win = as.numeric(points_for > points_against),
         loss = as.numeric(!win))

# establish opponent for each week
cy_weekly_opp0 <- cy_weekly_owner_scores0 %>% 
  select(-tot_points) %>% 
  inner_join(cy_weekly_owner_scores0 %>% select(-tot_points),
             by = c("week", "matchup_id", "lg_id")) %>% 
  filter(user_id.x != user_id.y) %>% 
  rename(owner = user_id.x, 
         opp = user_id.y)

# add opponent to weekly scores
cy_weekly_owner_scores2 <- cy_weekly_owner_scores1 %>% 
  left_join(cy_weekly_opp0, by = c("week", "owner",
                                   "matchup_id", "lg_id")) %>% 
  mutate(lg_year = 2022)


# Add attributes to data
cy_ps2 <- cy_ps1 %>% 
  left_join(ouid0) %>% 
  mutate(owner_name = case_when(!is.na(owner_name) ~ owner_name, 
                                is.na(owner_name) ~ "FREE AGENT")) %>% 
  mutate(lg_year = as.factor(lg_year), 
         week = as.factor(week)) %>% 
  # left_join(yfg0, by = c("lg_year" = "lg_year")) %>% 
  left_join(ofg0, by = c("owner_name" = "owner_name",
                         "lg_year" = "season")) 
# %>% 
#   left_join(rost0, by = c("player_id" = "players")) %>% 
#   mutate(current_owner = case_when(!is.na(current_owner) ~ current_owner, 
#                                    TRUE ~ "FREE AGENT"))


cy_weekly_owner_scores3 <- cy_weekly_owner_scores2 %>% 
  left_join(ouid0, 
            by = c("owner"= "user_id")) %>% 
  left_join(ouid0, 
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
  # left_join(yfg0, by = c("lg_year" = "lg_year")) %>% 
  left_join(ofg0, by = c("owner_name" = "owner_name",
                         "lg_year" = "season")) %>% 
  select(-owner, -opp) 

# Read and replace the existing Tableau data
# start with legacy data from TLST
player_stats_legacy0 <- read_csv("data\\tableau\\2021_archive\\player_stats.csv",
                                 col_types = cols(week = col_factor(), 
                                                  lg_year = col_factor(),
                                                  player_id = col_character(),
                                                  user_id = col_character()))

matchup_legacy0 <-read_csv("data\\tableau\\2021_archive\\matchup.csv",
                           col_types = cols(week = col_factor(), 
                                            lg_year = col_factor()))
# 
# player_stats0 <- read_csv("data/tableau/player_stats.csv", 
#                           col_types = cols(week = col_factor(), 
#                                            lg_year = col_factor(),
#                                            player_id = col_character(),
#                                            user_id = col_character(),
#                                            lg_id = col_character())) %>% 
#   filter(as.numeric(as.character(lg_year)) >= 2022) %>% # D: AHHHHH 
#   select(-season)
# 
# matchups0 <- read_csv("data/tableau/matchup.csv",
#                       col_types = cols(week = col_factor(), 
#                                        lg_year = col_factor(),
#                                        lg_id = col_character())) %>% 
#   filter(as.numeric(as.character(lg_year)) >= 2022) # D: AHHHHH

# new_player_stats <- cy_ps2 %>% 
#   anti_join(player_stats0, by = c("week", "lg_year", "lg_id"))
# 
# new_matchups <- cy_weekly_owner_scores3 %>% 
#   anti_join(matchups0, by = c("week", "lg_year", "lg_id"))

player_stats1 <- cy_ps2 %>% 
  # bind_rows(new_player_stats %>% select(-season)) %>%
  bind_rows(player_stats_legacy0) %>% 
  mutate(status = case_when(owner_name == "FREE AGENT" ~ "FREE AGENT", 
                            starter == TRUE ~ "STARTER", 
                            starter == FALSE ~ "BENCH"))

# ps <- player_stats1 %>% 
#   group_by(lg_id, status, lg_year, owner_name, 
#            week, player_name) %>% summarise(rec = n())

# Remove positions who are only owned by Free Agent
fa_pos <- player_stats1 %>% 
  group_by(position) %>% 
  summarise(numrec = length(unique(owner_name)),
            has_fa = "FREE AGENT" %in% owner_name) %>% 
  ungroup() %>% 
  filter(numrec == 1, has_fa == TRUE) %>% 
  select(position) %>% pull()

player_stats2 <- player_stats1 %>% 
  filter(!(position %in% fa_pos)) %>% 
  left_join(lg_ref) %>% 
  mutate(lg_name = coalesce(lg_name, "No League"))

matchups1 <- cy_weekly_owner_scores3 %>% 
  # bind_rows(matchups0) %>% 
  bind_rows(matchup_legacy0) %>% 
  mutate(outcome = case_when(win == 0 ~ "Loss", 
                             TRUE ~ "Win")) %>% 
  left_join(lg_ref) %>% 
  mutate(lg_name = coalesce(lg_name, "No League"))



data.table::fwrite(player_stats2, 
                   paste0("data/tableau/player_stats.csv"))

data.table::fwrite(matchups1, 
                   paste0("data/tableau/matchup.csv"))
