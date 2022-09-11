#### Draft Gather ####==========================================================
# Code pull draft results for all years and saves results

#### Setup ####=================================================================

# Source global code that loads packages, pulls standardized reference data, 
#  and creates some utility functions. 
source("R/global.R")

#### Utility Functions ####=====================================================

# function to returns picks from a draft when passed a season
F_get_draft <- function(season = 2021){
  
  # need to get league info first
  lg_info <- F_get_lg_info(.user_id = user_id, season = season)
  
  # once league ID is known, can get the draft
  lg_id <- lg_info$lg_id
  draft_info <- fromJSON(paste0("https://api.sleeper.app/v1/league/", 
                                lg_id, 
                                "/drafts")) %>% 
    filter(status == "complete")
  draft_id <- draft_info$draft_id
  
  # now pull draft picks
  draft_results <- fromJSON(paste0("https://api.sleeper.app/v1/draft/", 
                                   draft_id, 
                                   "/picks"))
  draft_results
}


#### Gathering Data ####========================================================

# Will just run the function for each year and then write out results
draft_out0 <- bind_rows(lapply(2017:2021, function(m){
  df <- F_get_draft(m) %>% 
    mutate(lg_year = m)
} ))

# Many of the fields aren't needed
draft_out1 <- draft_out0 %>% 
  select(-metadata, -is_keeper)

# This is a static file which will not change year to year. Can write out. 
data.table::fwrite(draft_out1, 
                   paste0("data/tableau/draft_picks.csv"))
