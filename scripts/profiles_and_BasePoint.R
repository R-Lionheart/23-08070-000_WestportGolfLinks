## RLionheart
## 21-0771-001
## January 2023
## Shoreline Conservation Areas, Washington State Parks

profile.pattern <- "prof_16|prof_17"
source("scripts/src/import_profiles2.R")
source("scripts/src/assign_profile_parks.R")


## Combine and add euclidean distance
profile.midpoints <- complete.profile %>%
  select(profile, BasePoint_X, BasePoint_Y, season:z) %>%
  filter(year %in% c("02", "03",
                     "04", "05", "06", "07", "08", "09", "10",
                     "11", "12", "13", "14", "15", "16", "17",
                     "18", "19", "20", "21")) %>%
  group_by(profile, year) %>%
  mutate(x_midpoint = ((min(x) + max(x))/2)) %>%
  mutate(y_midpoint = ((min(y) + max(y))/2)) %>%
  mutate(z_midpoint = ((min(z) + max(z))/2)) %>%
  ###
  filter(profile == 16) %>%
  select(profile, year, z_midpoint) %>%
  unique()
  
  
## Prelim visual of data vs BasePoint
all.basepoint.plot <- ggplot(data = profile.midpoints %>%
                               group_by(profile, year)) +
  #geom_point(aes(x = x, y = y)) +
  #geom_point(aes(x = BasePoint_X, y = BasePoint_Y), color = "red", size = 3) +
  #geom_point(aes(x = x_midpoint, y = y_midpoint, color = year), size = 2) +
  geom_point(aes(x = year, y = z_midpoint)) +
  #geom_line(aes(x = BasePoint_X, y = BasePoint_Y), color = "red", linewidth = 1) +
  ggtitle(paste("Profile:", profile.pattern))
all.basepoint.plot

# Zoom in on a specific profile to see what's happening
# partial.visual <- profile.midpoints %>%
#   filter(profile %in% c(16) & year == "98")
# 
# single.basepoint.plot <- ggplot(data = partial.visual %>% group_by(profile, year)) +
#   geom_point(aes(x = x, y = y), alpha = 0.5) +
#   geom_point(aes(x = x_midpoint, y = y_midpoint), color = "purple", size = 2) +
#   geom_point(aes(x = BasePoint_X, y = BasePoint_Y), color = "red", size = 3) +
#   ggtitle("Profile 16")
# single.basepoint.plot




## Timeseries
prof.data <- profiles.df %>%
  drop_na() %>%
  mutate(year = factor(year, levels =  c("97", "98", "99","00", "01", "02", "03",
                                         "04", "05", "06", "07", "08", "09", "10",
                                         "11", "12", "13", "14", "15", "16", "17",
                                         "18", "19", "20", "21", "22"))) %>%
  filter(season == "f") %>%
  filter(profile == 17) #%>%
  # filter(year %in% c("02", "03",
  #                    "04", "05", "06", "07", "08", "09", "10",
  #                    "11", "12", "13", "14", "15", "16", "17",
  #                    "18", "19", "20", "21"))

## Attempt to redo

profile.timeseries.fig <- plot_ly(prof.data, x = ~x, y = ~as.numeric(year), z = ~z, 
                                  type = "scatter3d", mode = "lines", color=~year) %>%
  layout(
    scene = list(xaxis = list(title = "Position", showticklabels = FALSE),
                 yaxis = list(title = "Year"),
                 zaxis = list(title = "Elevation")),
    #title = list(text = "Westport Light South", y = 0.95),
    legend = levels(year),
    showlegend = FALSE)

profile.timeseries.fig


## mock3d
slant_factor = 1   # How many units x shift per category? Won't work with zero

intercepts = (20 * (0:4)) / slant_factor + 0.8
ggplot(prof.data, 
       aes(x = x - as.numeric(year)*slant_factor, 
           y = year, fill = year)) +
  
  geom_abline(slope = -1 / slant_factor , 
              intercept = intercepts,
              color = "gray90") +
  
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  
  #scale_x_continuous(expand = c(0, 0),
  #                   breaks = 20 * (0:4)) +
  scale_y_discrete(expand = expand_scale(mult = c(0.01, 0.25)), limits=rev) +
  #scale_fill_viridis_c(name = "Temp. [F]", option = "C") +
  labs(
    title = 'profile 17',
    #subtitle = 'Mean temperatures (Fahrenheit) by month for 2016'
  ) +
  theme_ridges(font_size = 13, grid = TRUE) + 
  theme(axis.title.y = element_blank(), panel.grid.major.x = element_blank())
