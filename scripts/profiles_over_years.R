## RLionheart
## Shoreline Conservation Areas, Washington State Parks

profile.pattern <- "prof_16|prof_17"
source("scripts/src/assign_profile_parks.R")

profile <- complete.profile %>%
  filter(profile == 16) %>%
  filter(year == 15) 


profile.plot <- ggplot(profile, aes(x = x, y = y, color = year)) +
  geom_line()


