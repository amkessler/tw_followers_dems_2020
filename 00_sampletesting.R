# https://www.cultureofinsight.com/blog/2018/01/25/2018-01-25-visualising-twitter-follower-overlap/

library(rtweet)
library(tidyverse)
library(UpSetR)

# get a list of twitter handles you want to compare (I left out Hadley because everyone follows him)
rstaters <- c("PeteButtigieg", "GovernorBullock")

# scrape the user_id of all followers for each handle in the list and bind into 1 dataframe
followers <- map_df(rstaters, ~ get_followers(.x, n = 20000, retryonratelimit = TRUE) %>% mutate(account = .x))

# We should now have a long 2 column dataframe with user_id (followers) and the hande of who 
# they are following in another. Let’s take a peek.

head(followers)

#save as RDS
saveRDS(followers, "processed_data/pete_and_bullock.rds")



# get a de-duplicated list of all followers
aRdent_followers <- unique(followers$user_id)

# for each follower, get a binary indicator of whether they follow each tweeter or not and bind to one dataframe
binaries <- rstaters %>% 
  map_dfc(~ ifelse(aRdent_followers %in% filter(followers, account == .x)$user_id, 1, 0) %>% 
            as.data.frame) # UpSetR doesn't like tibbles

# set column names
names(binaries) <- rstaters

# have a look at the data
head(binaries)


# plot the sets with UpSetR
upset(binaries, nsets = 7, main.bar.color = "SteelBlue", sets.bar.color = "DarkCyan", 
      sets.x.label = "Follower Count", text.scale = c(rep(1.4, 5), 1), order.by = "freq")
