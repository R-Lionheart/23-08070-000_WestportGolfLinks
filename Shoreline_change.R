## RLionheart
## Westport Golf Links
## March 2023

library(tidyverse)

## Arbitrary baseline was drawn approximately 350m from shoreline
## Each historical shoreline from 2006 to present was measured to the baseline.
## All info is in meters.


## Southern benchmark at approximately 46°53'13.57"N, 124° 7'34.81"W
south <- data.frame(year = c("7-2006", "9-2009", "9-2011", "8-2016", "3-2020", "8-2022"),
                 distance_m = c(305.00, 308.49, 313.71, 338.23, 344.00, 348.42), 
                 position = "south")

## Northern benchmark at approximately 46°54'6.84"N, 124° 7'54.29"W
north <- data.frame(year = c("7-2006", "9-2009", "9-2011", "8-2016", "3-2020", "8-2022"),
                    distance_m = c(322.55, 317.65, 311.69, 309.64, 316.66, 319.76), 
                    position = "north")

## Inflection point (where active erosion appears to be mitigated by Corps nourishment),
## approximately 46°53'49.91"N, 124° 7'48.71"W.
inflection <- data.frame(year = c("7-2006", "9-2009", "9-2011", "8-2016", "3-2020", "8-2022"),
                         distance_m = c(328.97, 332.19, 333.04, 331.54, 335.62, 337.26), 
                         position = "inflection")

distance_to_baseline <- south_location %>% 
  rbind(north_location)
distance_to_baseline$year <- factor(distance_to_baseline$year , levels=c("6-1990", "7-2006", "9-2009", "9-2011", "8-2016", "3-2020", "8-2022"))


ggplot(distance_to_baseline, aes(group=position, y=distance_m, x=year, color=position)) +
  geom_line()

t <- Dist %>% 
  group_by(position) %>% 
  arrange(position, year) %>% 
  mutate(rate = 100 * (distance_m - lag(distance_m))/lag(distance_m)) 

ggplot(data=t, aes(x=year, y=rate, group=position, color = position)) +
  geom_line() +
  geom_point()


