# Pull draft results to obtain auction values for all players. 

library(tidyverse)
library(jsonlite)
library(glue)

uid0 <- read_csv("data/reference/2022_season_owner_ref.csv",
                 col_types = cols(.default = col_character()))

lgid_preq <- "861706911337267200"
lgid_seq <- "861712947813130240"

# function pulls draft results into data frame
Fget_draft <- function(lgid){
  # construct url to get draft id
  lg_drf_url <- glue("https://api.sleeper.app/v1/league/{lgid}/drafts")
  lg_drf <- fromJSON(lg_drf_url)
  drf_id <- lg_drf$draft_id
  
  # With draft id, now pull draft picks
  drf_url <- glue("https://api.sleeper.app/v1/draft/{drf_id}/picks")
  drf <- fromJSON(drf_url)
  drf_df <- unnest(drf %>% select(-player_id), # player_id also in metadata nest
                   cols = c(metadata)) %>% 
    mutate(lg_id = lgid)
  drf_df
}

# Pull darft results for both leagues
drf0 <- bind_rows(lapply(list(lgid_preq, lgid_seq),
               Fget_draft))

# Join on the owner reference information, add additional column enhancements
drf1 <- drf0 %>% 
  left_join(uid0 %>% select(user_id, owner_name, league_2022) %>% distinct()
            , by = c("picked_by" = "user_id")) %>% 
  mutate(player_name = paste0(last_name, ", ", first_name))

write.csv(drf1, "data/tableau/draft_2022.csv", row.names = FALSE)

#### Scrap Work ####============================================================
id <- "861706911337267200"

lgdrf_url <- "https://api.sleeper.app/v1/league/861706911337267200/drafts"

lgdrf <- fromJSON(lgdrf_url)
did <- lgdrf$draft_id

drf_url <- "https://api.sleeper.app/v1/draft/861706912088039424/picks"

drf <- fromJSON(drf_url)

drf_unnest <- unnest(drf)


id2 <- "861712947813130240"

lgdrf_url2 <- "https://api.sleeper.app/v1/league/861712947813130240/drafts"

lgdrf2 <- fromJSON(lgdrf_url2)
did2 <- lgdrf2$draft_id

drf_url2 <- "https://api.sleeper.app/v1/draft/861712948412948480/picks"

drf2 <- fromJSON(drf_url2)

drf_unnest2 <- unnest(drf2)
