## RLionheart
## Westport Golf Links
## March 2023

library(tidyverse)

## Arbitrary base point information
## All info is in meters!

South <- data.frame(year = c("7-2006", "9-2009", "9-2011", "8-2016", "3-2020", "8-2022"),
                 distance_m = c(305.00, 308.49, 313.71, 338.23, 344.00, 348.42), 
                 position = "South"
)

North <- data.frame(year = c("7-2006", "9-2009", "9-2011", "8-2016", "3-2020", "8-2022"),
                    distance_m = c(322.55, 317.65, 311.69, 309.64, 316.66, 319.76), 
                    position = "North"
)

Dist <- South %>% 
  rbind(North)
Dist$year <- factor(Dist$year , levels=c("6-1990", "7-2006", "9-2009", "9-2011", "8-2016", "3-2020", "8-2022"))


ggplot(Dist, aes(group=position, y=distance_m, x=year, color=position)) +
  geom_line()

t <- Dist %>% 
  group_by(position) %>% 
  arrange(position, year) %>% 
  mutate(rate = 100 * (distance_m - lag(distance_m))/lag(distance_m)) 

ggplot(data=t, aes(x=year, y=rate, group=position, color = position)) +
  geom_line() +
  geom_point()


