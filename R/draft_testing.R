library(httr)
library(tidyverse)
library(jsonlite)



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
