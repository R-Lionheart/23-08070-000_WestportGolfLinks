## RLionheart

profile.pattern <- "prof_16|prof_17"
source("scripts/src/load_packages.R")
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
  mutate(z_midpoint = ((min(z) + max(z))/2))


## Timeseries
prof.data <- profiles.df %>%
  drop_na() %>%
  mutate(year = factor(year, levels =  c("97", "98", "99","00", "01", "02", "03",
                                         "04", "05", "06", "07", "08", "09", "10",
                                         "11", "12", "13", "14", "15", "16", "17",
                                         "18", "19", "20", "21", "22"))) %>%
  filter(season == "f") %>%
  filter(profile == 17)

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
complete.profile2 <- complete.profile %>%
  filter(year %in% c("02", "03",
                     "04", "05", "06", "07", "08", "09", "10",
                     "11", "12", "13", "14", "15", "16", "17",
                     "18", "19", "20", "21", "22"))



## Good final images
prof16 <- complete.profile2 %>%
  filter(season == "f") %>%
  select(profile, year, x, y, z) %>%
  filter(profile == 16) 


tiff("Northern.tiff", units="in", width=6, height=5, res=300)
ggplot(prof16, aes(x, year, height = z, fill = year)) + 
  geom_ridgeline() +
  scale_y_discrete(limits=rev) +
  scale_fill_viridis(discrete = TRUE, option = "D")+
  xlim(223200, 223500) +
  theme_gdocs() +
  theme(legend.position = "none") +
  labs(
    title = "Northern Location",
    subtitle = 'Westport Profiles: 2002 - 2021'
  ) +
  xlab("Easting Coordinates") +
  ylab("Year") +
  annotate(geom = "text", x = 223210, y = 001, 
           label = "Seaward", color = "#482677FF") +
  annotate(geom = "text", x = 223490, y = 001, 
           label = "Landward", color = "#482677FF")

dev.off()

  

prof17 <- complete.profile2 %>%
  filter(season == "f") %>%
  select(profile, year, x, y, z) %>%
  filter(profile == 17) 

tiff("Southern.tiff", units="in", width=6, height=5, res=300)
ggplot(prof17, aes(x, year, height = z, fill = year)) + 
  geom_ridgeline() +
  scale_y_discrete(limits=rev) +
  scale_fill_viridis(discrete = TRUE, option = "D")+
  xlim(223500, 223850) +
  theme_gdocs() +
  theme(legend.position = "none") +
  labs(
    title = "Southern Location",
    subtitle = 'Westport Profiles: 2002 - 2021'
  ) +
  xlab("Easting Coordinates") +
  ylab("Year")  +
  annotate(geom = "text", x = 223525, y = 001, 
           label = "Seaward", color = "#482677FF") +
  annotate(geom = "text", x = 223825, y = 001, 
           label = "Landward", color = "#482677FF")
dev.off()