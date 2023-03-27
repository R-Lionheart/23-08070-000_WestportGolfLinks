## RLionheart
## Westport Golf Links
## March 2023

library(tidyverse)

## Arbitrary base point information
## All info is in meters!

South <- data.frame(year = c("6-1990", "7-2006", "9-2009", "9-2011", "8-2016", "3-2020", "8-2022"),
                 distance_m = c(302.47, 305.00, 308.49, 313.71, 338.23, 344.00, 348.42), 
                 position = "South"
)

North <- data.frame(year = c("6-1990", "7-2006", "9-2009", "9-2011", "8-2016", "3-2020", "8-2022"),
                    distance_m = c(241.23, 322.55, 317.65, 311.69, 309.64, 316.66, 319.76), 
                    position = "North"
)

Dist <- South %>% 
  rbind(North)
Dist$year <- factor(Dist$year , levels=c("6-1990", "7-2006", "9-2009", "9-2011", "8-2016", "3-2020", "8-2022"))


ggplot(Dist, aes(fill=position, y=distance_m, x=year)) + 
  geom_bar(position="dodge", stat="identity")



## Euclidean distances
euclidean <- complete.profile %>%
  mutate(x_midpoint = ((min(x) + max(x))/2)) %>%
  mutate(y_midpoint = ((min(y) + max(y))/2)) %>%
  select(profile, year, X_BasePoint, Y_BasePoint, x_midpoint,  y_midpoint) %>%
  unique() %>%
  arrange(year) %>%
  group_by(profile) %>%
  mutate(euc_dist_to_BP = sqrt(((X_BasePoint - x_midpoint)^2) + ((Y_BasePoint -  y_midpoint)^2))) %>%
  # mutate(test = ifelse(levels(euclidean$year)[1], sqrt(((X_BasePoint - x_midpoint)^2) + ((Y_BasePoint -  y_midpoint)^2)),
  #                                ifelse(year == 21, TRUE, FALSE)))
  mutate(profile_slope = ((Y_BasePoint - y_midpoint) / (X_BasePoint - x_midpoint))) %>%
  #mutate(slope_dir = ifelse(profile_slope > 0, "positive", "negative"))
  mutate(slope_dir = "slope")
euclidean$year <- factor(euclidean$year, levels =  c("97", "98", "99","00", "01", "02", "03",
                                                     "04", "05", "06", "07", "08", "09", "10",
                                                     "11", "12", "13", "14", "15", "16", "17",
                                                     "18", "19", "20", "21", "22"))


## Visualize euclidean distance from average Euclidean distance of each year
midpoint.euc.dist.plot <- ggplot(euclidean %>% drop_na(), 
                                 aes(year, euc_dist_to_BP, fill=slope_dir, group = slope_dir)) +
  facet_wrap(~profile) +
  geom_col(position = position_dodge(width = 0.5)) +
  geom_line(aes(group = slope_dir), position = position_dodge(width = 1),
            size = 1, color = "black") +
  geom_smooth(method = "lm", se = TRUE, color="blue") +
  theme(axis.text = element_blank()) +
  ggtitle(paste("Profile", profile.pattern, ": Midpoint Euclidean Distance from Base Point"))
midpoint.euc.dist.plot

