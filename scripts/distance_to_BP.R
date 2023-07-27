## RLionheart
## Shoreline Conservation Areas, Washington State Parks

profile.pattern <- "prof_16|prof_17"
source("scripts/src/assign_profile_parks.R")


### test
t <- complete.profile %>%
  group_by(profile, year) %>%
  mutate(xmean = mean(x),
         ymean = mean(y),
         zmean = mean(z)) %>%
  mutate(feet_to_BP = (sqrt(((BasePoint_X - xmean)^2) +
                              ((BasePoint_Y -  ymean)^2))) *  3.28084) %>% ## Convert m to feet
  select(profile, Park, year, feet_to_BP) %>%
  unique()

tplot <- ggplot(t, aes(year, feet_to_BP, group = Park, color = Park)) +
  geom_line() +
  geom_smooth() +
  xlab("Year") +
  ylab("Distance in feet from BasePoint") + 
  guides(fill=guide_legend(title="")) +
  ggtitle("Distance from Baseline")
tplot
###