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
  unique() %>%
  filter(profile == 17)

ggplot(t, aes(x = feet_to_BP, y = year)) +
  geom_col(position = "identity", color = "black") +
  scale_fill_manual(values = c("#7FBFFF", "#FF7F7F")) +
  scale_y_discrete(limits=rev, position = "right") +
  labs(x = "", y = "Value", title = "South")
