## RLionheart
## Westport Golf Links
## March 2023

library(tidyverse)
library(ggpubr)

## Arbitrary baseline was drawn approximately 350m from shoreline
## Each historical shoreline from 2006 to present was measured to the baseline.
## All info is in meters.


## Southern benchmark at approximately 46°53'13.57"N, 124° 7'34.81"W
## Chosen due to proximity to dynamic revetment constructed in 2021
south <- data.frame(year = c("7-2006", "9-2009", "9-2011", "8-2016", "3-2020", "8-2022"),
                 distance_m = c(305.00, 308.49, 313.71, 338.23, 344.00, 348.42), 
                 position = "south") %>%
  mutate(distance_f = distance_m * 3.28084)

## Northern benchmark at approximately 46°54'6.84"N, 124° 7'54.29"W
## Chosen due to proximity to nourishment provided by the Army Corps of Engineers
north <- data.frame(year = c("7-2006", "9-2009", "9-2011", "8-2016", "3-2020", "8-2022"),
                    distance_m = c(322.55, 317.65, 311.69, 309.64, 316.66, 319.76), 
                    position = "north") %>%
  mutate(distance_f = distance_m * 3.28084)

## Inflection point at approximately 46°53'49.91"N, 124° 7'48.71"W.
## Where active erosion appears to be mitigated by Corps nourishment, visual inspection only.
inflection <- data.frame(year = c("7-2006", "9-2009", "9-2011", "8-2016", "3-2020", "8-2022"),
                         distance_m = c(328.97, 332.19, 333.04, 331.54, 335.62, 337.26), 
                         position = "inflection") %>%
  mutate(distance_f = distance_m * 3.28084)

## Combine all locations to one dataframe
distance_to_baseline <- south %>% 
  rbind(north) %>%
  rbind(inflection) %>%
  select(-distance_m) %>%
  mutate(distance_f = distance_f)
distance_to_baseline$year <- factor(distance_to_baseline$year,
                                    levels=c("6-1990", "7-2006", "9-2009", "9-2011", "8-2016", "3-2020", "8-2022"))
distance_to_baseline$position <- factor(distance_to_baseline$position, 
                                        levels = c("north", "inflection", "south"))

## Plot distance by year
distance.plot <- ggplot(distance_to_baseline, 
                        aes(group=position, y=distance_f, x=year, color=position)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("#036C88", "#36A886", "#DBA827"), 
                     name = "Geographic location") +
  xlab("Month-Year") + 
  ylab("Distance from Basepoint (m)") +
  ggtitle("Overall Distance (m)")
distance.plot

## Calculate rates of change (ROC) at each location
ROC <- distance_to_baseline %>% 
  group_by(position) %>% 
  arrange(position, year) %>% 
  mutate(rate = 100 * (distance_f - lag(distance_f))/lag(distance_f))

## Plot rates of change by year
ROC.plot <- ggplot(data=ROC, aes(x=year, y=rate, group=position, color = position)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("#036C88", "#36A886", "#DBA827"), 
                     name = "Geographic location") +
  xlab("Month-Year") + 
  ylab("Rate of Change (m/year)") +
  ggtitle("Rate of Change (m/year)")
ROC.plot

## Combine plots
all.plots <- ggarrange(distance.plot, ROC.plot, common.legend = TRUE, legend="right")
annotate_figure(all.plots, top = text_grob("Shoreline Erosion at Westpoint Light State Park", 
                                      color = "black", face = "bold", size = 14))

## Predict 15-year and 25-year erosion rate
prediction <- distance_to_baseline %>%
  separate(year, into = c("month", "year"), sep = "-") %>%
  group_by(position) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(time = max(year) - min(year),
         distance_change = distance_f[year == 2022] - distance_f[year == 2006],
         f_per_year = round(distance_change / time),
         year_15 = f_per_year * 15,
         year_25 = f_per_year * 25) %>%
  select(position, distance_change, f_per_year, year_15, year_25) %>%
  unique()
