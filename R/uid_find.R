# pull league information to get uid that can be mapped to owner names

library(glue)
library(tidyverse)
library(jsonlite)

lgid_preq <- "861706911337267200"
lgid_seq <- "861712947813130240"

Fget_users <- function(lgid){
  lg_url <- glue("https://api.sleeper.app/v1/league/{lgid}/users")
  user_json0 <- fromJSON(lg_url)
  user0 <- user_json0 %>% 
    select(user_id, league_id, display_name)
}

# combine the two sets of UIDs together
tlst_owners0 <- bind_rows(lapply(list(lgid_preq, lgid_seq), 
                      Fget_users)) 

# Write out the display names. These can change over the course 
#  of the season, but a one time mapping of those names to actual 
#  owner names can be done manually in Excel. Then that will be brought
#  back into analysis and joined on to UID so that the UIDs can be 
#  referenced for display names moving forward. This will also include
#  the league each owner is in. 
write.csv(tlst_owners0 %>%  select(display_name) %>% distinct(), 
          "data/reference/2022_owner_names.csv",
          row.names = FALSE)

owner_names0 <- read_csv("data/reference/2022_owner_names.csv")

# Now join this back onto the original table with the UIDs in it. 
tlst_owners1 <- tlst_owners0 %>% 
  left_join(owner_names0)
  
write.csv(tlst_owners1, "data/reference/2022_season_owner_ref.csv",
          row.names = FALSE)
  
