## RLionheart
## Shoreline Conservation Areas, Washington State Parks

profile.pattern <- "prof_16|prof_17"
source("scripts/src/assign_profile_parks.R")


### Midpoint distance to baseline
ecology_stations <- complete.profile %>%
  group_by(profile, year) %>%
  mutate(xmean = mean(x),
         ymean = mean(y),
         zmean = mean(z)) %>%
  mutate(feet_to_BP = (sqrt(((BasePoint_X - xmean)^2) +
                              ((BasePoint_Y -  ymean)^2))) *  3.28084) %>% ## Convert m to feet
  select(profile, Park, year, feet_to_BP) %>%
  unique()

## Check layout
vizdata <- complete.profile %>%
  group_by(profile, year) %>%
  mutate(xmean = mean(x),
         ymean = mean(y),
         zmean = mean(z)) %>%
  mutate(feet_to_BP = (sqrt(((BasePoint_X - xmean)^2) +
                              ((BasePoint_Y -  ymean)^2))) *  3.28084)
visual <- ggplot(data = vizdata %>% 
              filter(year %in% c("06", "21", "22")) %>%
              filter(profile == 17) %>%
              group_by(profile, year)) +
  geom_point(aes(x = x, y = y)) +
  geom_point(aes(x = BasePoint_X, y = BasePoint_Y), color = "red", size = 3) +
  geom_point(aes(x = xmean, y = ymean, color = year), size = 2) +
  geom_line(aes(x = BasePoint_X, y = BasePoint_Y), color = "red", linewidth = 1) +
  ggtitle("Westport South, midpoint and basepoint")
visual

ecology_plot <- ggplot(ecology_stations, aes(year, feet_to_BP, group = Park, color = Park)) +
  geom_line() +
  geom_smooth() +
  xlab("Year") +
  ylab("Distance (feet) from fixed point") + 
  guides(fill=guide_legend(title="")) +
  ggtitle("Westport Shoreline Movement") +
  theme_minimal()
ecology_plot
###

## Rates for 
ecology_rates <- ecology_stations %>%
  filter(year %in% c("06", "21", "22")) %>%
  filter(profile != 17 | year != "21") %>%
  group_by(profile) %>%
  mutate(diff = round(feet_to_BP[year %in% c("21", "22")] - feet_to_BP[year == "06"], digits = 2)) %>%
  mutate(rates = ifelse(profile == 16, round(diff/16, digits = 2), round(diff/17, digits = 2)))


